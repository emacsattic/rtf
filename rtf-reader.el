;;; rtf-reader.el --- reader for the Rich Text Format (RTF)

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Oliver Scholz <epameinondas@gmx.de>
;; Keywords: wp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package aims to implement a reader for the "Rich Text Format"
;; (RTF), version 1.5.

;; RTF is a standard format for exchanging documents between so-called
;; "word processor" applications, like OpenOffice, Microsoft Word or
;; WordPerfect--and now Emacs.  (Note: it is not entirely clear what
;; the exact scope and purpose of a "word processor" is; see the file
;; "mission.txt".)  One could say that RTF is for this kind of
;; documents what Postscript is for documents which specify their
;; exact appearance on paper. RTF supports storing various character
;; properties on content text (font, size, slant, weight ...),
;; paragraph properties (indentation, space before, space after ...),
;; style sheets for these properties, nested lists with numbers or
;; bullets, foot- and endnotes, tables and much more.

;; In a sense, an RTF file (in short: "the RTF") is like a big, nested
;; s-expression with curly braces instead of parentheses.  For this
;; reason the RTF reader is implemented like a Lisp interpreter.
;; Parsing is done in two steps: reading and evaluating.  The function
;; `rtf-read' reads the RTF from the source buffer into a big lists of
;; s-expression; the function `rtf-eval' interprets this list then and
;; inserts the text, properly formatted and with the proper text
;; properties, into the target buffer.  In the following the term "the
;; reader" refers to that part of this code which reads the RTF file
;; into s-expressions, while "the evaluator" refers to that part of
;; the code which acts on control information and inserts the content
;; text accordingly into a buffer.  I refer to this whole program
;; reading and interpreting RTF as "the parser" or--explicitly
;; mentioning "RTF"--as "the RTF reader" (this is the term used in the
;; RTF specification).

;; The specification for Rich Text Format is by far too large and
;; complicated to be repeated here.  A few cursory notes are added
;; throughout the code, wherever appropriate.  You can obtain the full
;; spec from here: 
;; <URL: http://msdn.microsoft.com/library/specs/rtfspec.htm>.
;; [Note: At the time being (2003-09) the URL above leads to version
;; 1.6 of the RTF spec.  I found a version 1.7 in PDF and Postscript
;; on a different website:
;; <URL: http://www.dubois.ws/software/RTF/>]

;; RTF is a plain text format, mostly (always?) ASCII.  Control
;; information (as opposed to the content text) is designed to be
;; distinguishable for humans.  It is _not_, however designed to be a
;; human-readable format in general.  Most notably it does not provide
;; the possibility for whitespace layout (indentation and the like)
;; which is necessary for a human reader.

;; <rant>I wouldn't say that RTF is designed to be particularly
;; machine-readable, either.</rant>

;; The general layout of an RTF is

;; FILE := '{' HEADER DOCUMENT '}'

;; (See below "The Reader" for further information on the syntax.)

;; A conforming RTF reader is required to ignore everything after the
;; last closing brace (FIXME: conform this in the spec. What about
;; garbage before the first opening brace?).  The header starts with
;; the control word "\rtf" and specifies a lot of document-wide
;; properties, including style sheets.  There is no syntactical
;; construct to mark the end of the header, there is especially no
;; requirement to embrace the header in a group.  Even worse: some
;; information that by definition belongs to the DOCUMENT section may
;; occur within the HEADER section.  An RTF reader is supposed to be
;; able to deal with this.

;; In general an RTF reader is required to be as permissive as
;; possible.  According to the spec, it should simply ignore every
;; control information which it does not know.  For this reason this
;; package tries to be as robust as possible.  My goal is to make it
;; robust enough that a user can feed _any_ file to it without causing
;; an error. (Of course, the rendered "document" would look rather
;; funny, if the file is not valid RTF ...)


;;; Code:

(eval-when-compile (require 'cl)
		   (require 'state-m))

(require 'rtf-controls)

;; --------------------------------------------------------------------
;;;; Data Structure

;;; Temporary Data Structures for Evaluation

;; The structure `rtf-environment' and all structures whose names
;; start with `rtf-env-' are used only while the evaluator does its
;; work.  See "The Evaluator" for further information.

(defstruct rtf-environment
  "The evaluation environment at read time."
  destination     ; current destination
;;  current-style ;; set by \sN
;;  style-override
  document        ; global properties
;;   paragraph       ; text properties (paragraph level) for insertion
;;   characters      ; text properties (character level) for insertion
  secfmt          ; rtf section formatting properties
  parfmt          ; rtf paragraph formatting properties
  charfmt         ; rtf character formatting properties
  )

(defstruct rtf-env-document
  "Document wide data needed at read time."
  encoding         ; ansi, mac, pc, pca
  info             ; info
  orig-font-table  ; fonttbl FIXME
  font-table       ; fonttbl, resolved FIXME
  sec-stylesheets  ; stylesheet
  par-stylesheets  ; stylesheet
  char-stylesheets ; stylesheet
  default-font     ; deff
  )

(defstruct rtf-stylesheet
  "A style sheet.
A style sheet is part of the data gained by parsing the
\\stylesheet destination group."
  type             ; ds, s, cs
  num              ; ds, s, cs
  basedon          ; sbasedon
  plain            ; plain FIXME
  name             ; #PCDATA
  next             ; snext
  autoupd          ; sautoupd
  keycode          ; keycode
  hidden           ; shidden
  formatting
  )
 

(defstruct rtf-env-font
  "Font table.
A font table is part of the data gained by parsing the
\\fonttbl destination group."
  num            ; f
  family         ; fnil, froman ... fbidi
  charset        ; fcharset
  pitch          ; fprq
  panose         ; panose
  non-tagged-name  ; fname
  name           ; #PCDATA
  alt-name        ; falt
;;  emb
;;  type
  codepage)      ; cpg


;;; Persistent Data

;; With the exception of `rtf-document', the evaluator fills the
;; following structures at (RTF-)evalutation time.  But all of them
;; are kept when this RTF reader is done as part of the structure
;; `rtf-document' in a buffer local variable of the same name.  Some
;; of them get further processing after the evaluation is done (read:
;; after all the text is inserted into the buffer!).

(defstruct rtf-paragraph ; (:type vector))
  "Paragraph formatting properties."
  style             ; s
  hyphenation       ; hyphpar
  in-table          ; intbl
  keep              ; keep
  ;; nowdctlpar
  ;; widctlpar
  keep-next         ; keepn
  level             ; level
  no-line-numbering ; noline
  outline-level     ; outlinelevel
  pagebreak-before  ; pagebb
  ;; sbys
  ;; Values for justification are
  ;; `left', `right', `full' or `center'.
  justification     ; ql, qr, qj, qc
  first-line-indent ; fi
  left-indent       ; li
  right-indent      ; ri
  space-before      ; sb
  space-after       ; sa
  line-spacing      ; sl
  charfmt
  ;; slmult
  ;; subdocument
  ;; rtlpar
  ;; ltrpar
  )

(defstruct rtf-env-info ; FIXME: naming convention
  "Document information, such as author, creation time etc."
  title
  author
  operator)

(defstruct rtf-character-props ;; (:type vector))
  "Character formatting properties."
  style       ; cs
  face
  bold        ; b
  italic      ; i
  underlined  ; ul
  foreground  ; cf
  background  ; cb
  subscr-pos  ; dn
  caps        ; caps
  charscalex  ; charscalex
  font        ; f
  fontsize    ; fs
  language)   ; lang


(defvar rtf-document nil
  "Global data of an RTF document.")

(make-variable-buffer-local 'rtf-document)

(defstruct rtf-document
  "Global data of a document in the buffer."
  font-table
  char-styles
  par-styles
  info)



;; --------------------------------------------------------------------
;;;; The Reader

;; The RTF specification distinguishes the following types of
;; information:

;;     * /control words/ start with a backslash ('\').  They consist
;;       of a sequence of letters (from the ASCII repertoire),
;;       optionally followed by on or more digits (optionally preceded
;;       by a minus sign) and end with either a space or any other
;;       character other than a letter or a digit.  The latter
;;       difference is: The reader is required to ignore the space
;;       (especially: to not consider it as part of the content text),
;;       while any other delimiting character may be part of
;;       consecutive control information or of the content text.
;;
;;       Examples: \rtf, \f0, \plain, \ansicpg1252
;;
;;   There is an important semantical differences between control
;;   words that "change the destination" and those that don't, which
;;   affects evaluation.  See the section "The Evaluator" below.

;;     * /control symbols/ start with a backslash followed by a single
;;       non-alphanumeric character.

;; Control words which have no numerical argument and control symbols
;; (in short: "controls") are delivered as interned symbols by the
;; reader.  Control words with numerical argument are delivered as a
;; vector [CONTROL NUMBER], where CONTROL is an interned symbol and
;; NUMBER is--you guessed it--a number.

;;     * /groups/ are text and/or control information enclosed in
;;       curly braces ('{' and '}').

;; The function `rtf-read-token' returns the curly braces as
;; characters.  The function `rtf-read' (calling `rtf-read-token')
;; reads a group recursively and returns it as a list (discarding the
;; curly braces).

;;     * Everything else is text--"unformatted text" in the
;;       lingo of the RTF spec.

;; The reader returns content text as a string.


;;; Example: `rtf-read' called on the following small, but complete
;;; RTF ...

;; {\rtf\ansi\deff0{\fonttbl{\f0\froman Times New Roman;}}{\b
;; The Hunting of the Snark}\par\pard\plain To seek it with thimbles ...}

;; ... returns:

;; ((rtf ansi
;;       [deff 0]
;;       (fonttbl
;;        ([f 0]
;;         froman "Times New Roman;"))
;;       (b "The Hunting of the Snark")
;;       par pard plain "To seek it with thimbles ..."))


(defun rtf-read-token ()
  "Return the next token from RTF data in the current buffer."
  (let (control)
    (run-state-machine ()
      (start (?{ (lambda (o i) (string-to-char i))
		 t exit)
	     (?} (lambda (o i) (string-to-char i))
		 t exit)
	     (?\\ nil t read-control)
	     (?\n nil t start)
	     (t t t read-text))
      
      (read-text 
       (?{ nil nil exit)
       (?} nil nil exit)
       (?\\ nil nil exit)
       (?\n nil t read-text)
       (t t t read-text))
      
      (read-control
       ((?a . ?z) t t read-control-word)
       (t (lambda (o i) (intern (concat o i))) t exit))
      
      (read-control-word
       ((?a . ?z) t t read-control-word)
       ((?- (?0 . ?9)) (lambda (o i) (setq control (intern o)) i)
	               t read-argument)
       ((?\  ?\t ?\n) (lambda (o i) (intern o))
	t exit)
       (t (lambda (o i) (intern o)) nil exit))

;;       (skip-whitespace-after-control
;;        ((?\  ?\t ?\n) nil t skip-whitespace-after-control)
;;        (t nil nil exit))

      (read-argument ((?0 . ?9) t t read-argument)
		     (?\  (lambda (o i)
			    (vector control (string-to-number o))
			    ) t exit)
		     (t (lambda (o i)
			  (vector control (string-to-number o))
			  ) nil exit)))))

(defun rtf-read ()
  "Read RTF expression at point."
  (let ((list nil)
	(reading t)
	token)
    (while reading
      (setq token (rtf-read-token))
      (cond ((eq token ?{)
	     (push (rtf-read) list))
	    ((eq token ?})
	     (setq reading nil))
	    ((null token)
	     (setq reading nil))
;; FIXME
;;	     (error "Invalid rtf: end of file during parsing."))
	    (t (push token list))))
    (nreverse list)))



;; --------------------------------------------------------------------
;;;; The Evaluator

;;; Outline of the Evaluator

;; The evaluator basically works like an unsophisticated Lisp
;; evaluator (mutatis mutandis, of course. The syntax is quite
;; different even in s-expression form).  Controls, groups and content
;; text are the "expressions":

;; EXP        := CONTROL | GROUP | TEXT
;; GROUP      := ( EXP+ )
;; CONTROL    := CONTROL-WORD              ; for example: `ansi'
;;               | CONTROL-SYMBOL          ; `~'
;;               | CONTROL-WORD-WITH-ARG   ; [f 0]

;; The function `rtf-eval' gets called with an expression and the
;; current environment (see below, think of it as a dynamical variable
;; frame for now). It looks at the type of a expression and treats it
;; as it deserves.  For controls it calls `rtf-apply-control', which
;; looks up a function for the control in the hash table
;; `rtf-controls-hash' and passes to it the current environment and
;; the numerical argument, if there is one.  Groups are recursively
;; evaluated by calling `rtf-eval-sequence'.  Text is inserted by the
;; function `rtf-text' into the buffer with the appropriate text
;; properties according to the current environment.

;; Formatting changing controls work by changing the state of the
;; parser.  For example, one way to specify a word as bold in the RTF
;; would be to write:

;; \plain This is default character style, and this is \b bold \plain
;; text.

;; The RTF specification requires that formatting changing controls
;; inside a group do not affect text outside the group.  So an RTF
;; writer could (and probably would) write the above as:

;; \plain this is default character style, and this is {\b bold} text.

;; <rant> 
;; It is much more likely that the typical RTF writer will generate
;; something like:

;; {\plain this is }{\plain default character{\plain  style and {\plain
;; this is \plain}{\b bold}\plain text.
;; </rant>

;; So we pass an "environment" (a structure of the type
;; `rtf-environment') around which stores the current state,
;; i.e. whatever formatting property the evaluator has previously set.
;; For each new group a new environment is created, which copies the
;; current environment.  The formatting changing controls do their job
;; by setting the environment via side effect.


;;; Special Controls

;; An important semantical difference is the difference between
;; "normal" controls and destination changing controls.
;; Syntactically, destination changing controls are always the first
;; item in a group.  I call groups starting with a destination
;; changing control word here "destination groups" or short
;; "destinations".  A destination group contains data that are not
;; meant to form part of the document body immediately, i.e. they
;; contain no body text or commands changing the properties of body
;; test _immediately_.  They either store document wide data, such as
;; style sheets, document information, font information etc.  Or they
;; specify text that is not meant to be inserted into the body at the
;; place where they occur in the RTF source. Footnotes are an example
;; for the latter. AFAICS destination groups do not contain other
;; destination groups.  But they do contain normal groups, especially
;; to ... erm ... group data together.  The fonttbl ("font table")
;; destination group, for example, contains one or more groups, each
;; one specifying a font used in the document.

;; Destination groups are not evaluated by `rtf-eval' immediately.
;; The evaluation is the duty of the destination switching command
;; entirely. See "RTF commands" below.

;; There is another important semantical difference, not mentioned
;; explicitly in the RTF specification.  Some controls do take an
;; argument which, according to the spec, an RTF reader should
;; otherwise tokenise as "unformatted text".  Examples for this would
;; be \u915G or \'e4.  These are (properly!) returned by the reader
;; (see "The Reader" above) as [u 915] followed by "G" and `''
;; followed by "e4", respectively.  But semantically the "G" and the
;; "e4" are arguments to the control word. (This is one of the reasons
;; (but not the only one!)  why I really hate RTF.)  Such controls are
;; named "_special_ controls" for the purpose of the evaluator.

;; We deal with this with a kludge: The function `rtf-read-sequence'
;; checks whether the expression at hand is a special.  If it is, it
;; calls this special, passing the pending stream to it and sets the
;; stream to the return value of named special control.  The code
;; implementing the special pops from the stream whatever it needs and
;; returns the rest.  This is the only case where the return value of
;; a control is significant.


(defsubst rtf-control-p (exp)
  "Return non-nil, if EXP is an RTF control word or symbol."
  (or (symbolp exp)
      (vectorp exp)))

(defsubst rtf-text-p (exp)
  "Return non-nil, if EXP is RTF unformatted text."
  (stringp exp))

(defun rtf-destination-group-p (exp)
  "Return non-nil, if EXP is a group starting a destination group.
A \"destination group\" is an RTF group starting with a
destination changing control word."
  (and (listp exp)
       (symbolp (car exp))
       (get (car exp) 'rtf-destination)))

(defun rtf-group-p (exp)
  "Return non-nil if EXP is an RTF group."
  (and (not (rtf-destination-group-p exp))
       (listp exp)))

;; The RTF standard specifies that some destination switching controls
;; should be preceded by the control symbol \*.  Readers which do not
;; recognise the control may skip the entire group.  (Otherwise a
;; reader is required to simply ignore the unknown command and
;; proceed, which means that it would insert text belonging to
;; the destination into the document body text.)

(defun rtf-ignorable-group-p (exp)
  "Return non-nil, if EXP is a group, which to ignore is save."
  (and (listp exp)
       (eq (car exp) '\*)))

(defun rtf-special-p (exp)
  "Return non-nil, if EXP is a special control word or symbol.
A control is \"special\" if it require a non-numerical argument."
  (and (symbolp exp)
       (get exp 'rtf-special)))

(defun rtf-extend-environment (env)
  "Return a fresh environment copying the environment ENV."
  (if (null env)
      (rtf-make-new-environment)
    (let ((newenv (copy-rtf-environment env)))
      (setf (rtf-environment-charfmt newenv)
	    (copy-rtf-character-props
	     (rtf-environment-charfmt env)))
      (setf (rtf-environment-parfmt newenv)
	    (copy-rtf-paragraph
	     (rtf-environment-parfmt env)))
      newenv)))


(defun rtf-make-new-environment ()
  "Create and return an environment from scratch."
  (make-rtf-environment
   :document (make-rtf-env-document
	      :info (make-rtf-env-info))
   :charfmt (make-rtf-character-props)
   :parfmt (make-rtf-paragraph)))

;;(defvar rtf-test-env-list nil)

(defun rtf-eval (exp env)
  "Evaluate RTF expression EXP in the environment ENV."
;;  (push env rtf-test-env-list)
  (cond ((rtf-destination-group-p exp)
	 (rtf-apply-destination (car exp)
				(cdr exp)
				(rtf-extend-environment env)))
	((rtf-ignorable-group-p exp)
	 (ignore))
;; 	 (let ((control (rtf-control-func (cadr exp))))
;; 	   (when (car control)
;; 	     (rtf-apply-destination control
;; 				    (cddr exp)
;; 				    (rtf-extend-environment env))))
	((rtf-group-p exp)
	 (rtf-eval-sequence exp (rtf-extend-environment env)))
	((rtf-control-p exp) (rtf-apply-control exp env))
	((rtf-text-p exp) (rtf-text exp env))
	(t (error "Unknown expression type: %S" exp)))
;;   (when (and (numberp (rtf-environment-charfmt env))
;; 	     (= (rtf-environment-charfmt env) 2))
;;     (edebug))
  )

(defun rtf-eval-sequence (sequence env)
  "Eval a SEQUENCE of RTF expressions, in environment ENV."
  (let (exp)
    (while sequence
      (setq exp (pop sequence))
      (cond   ((rtf-special-p exp)
	       (setq sequence
		     (rtf-apply-special
		      exp
		      sequence
		      env)))
	      (t (rtf-eval exp env))))))


(defun rtf-apply-control (control env)
  "Execute CONTROL in environment ENV."
  (let ((func (if (vectorp control)
		  (gethash (aref control 0) rtf-controls-hash)
		(gethash control rtf-controls-hash)))
	(arg (and (vectorp control)
		  (aref control 1))))
    (when func
      (funcall func arg env))))

(defun rtf-text (string env)
  "Insert STRING, formatted according to the environment ENV."
  (let ((beg (point)))
    (insert (rtf-reader-propertize
	     string
	     env))
    (fill-region-as-paragraph beg (point))))

(defun rtf-apply-destination (destination group env)
  "Call appropriate destination parsing function.
DESTINATION is the destination changing command.  It is executed
inside the environment ENV and receives the remainder of its
destination group GROUP"
  (let ((func (gethash destination rtf-controls-hash)))
    (when func
      (setf (rtf-environment-destination env)
	    destination)
      (funcall func group env))))

(defun rtf-apply-special (control stream env)
  "Apply special control CONTROL in environment ENV on STREAM.
STREAM is a stream of RTF tokens.  Return, whatever CONTROL
returns."
  (let ((func (if (vectorp control)
		  (gethash (aref control 0) rtf-controls-hash)
		(gethash control rtf-controls-hash)))
	(arg (and (vectorp control)
		  (aref control 1))))
    (if (null func)
	stream
      (funcall func arg stream env))))


(defun rtf-reader-propertize (str env)
  "Return string STR with text properties.
The text properties depend on the environment ENV."
  (propertize str 
	      'rtf-characters (rtf-environment-charfmt env)
	      'rtf-paragraph (rtf-environment-parfmt env)))



;; --------------------------------------------------------------------
;;;; Testing

(defvar rtf-test-var)

(defun rtf-read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (rtf-read-region (point-min) (point-max))))

(defun rtf-read-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (if (<= beg end)
	  (goto-char beg)
	(goto-char end))
      (rtf-read))))

(defun rtf-test-read (file)
  (interactive "f")
  (message "Reading RTF file...")
  (setq rtf-test-var (rtf-read-file file))
  (message "Reading RTF file...done"))


(defun rtf-test-eval ()
  (interactive)
;;  (garbage-collect)
  (let ((buf (generate-new-buffer "*tmp*")))
    (switch-to-buffer buf)
    (let ((env (rtf-extend-environment nil)))
      (rtf-eval (car rtf-test-var) env)
      (rtf-major-mode)
      (setq rtf-document (rtf-setup-document (rtf-environment-document env))))
;;     (rtf-fill-region (point-min) (point-max))
    ))

;; Local Variables:
;; sentence-end-double-space: t
;; End:

(provide 'rtf-reader)
;;; rtf-reader.el ends here

;;; rtf-controls.el --- Code for RTF control words and symbols

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

;; 

;;; Code:

(defvar rtf-controls-hash
  (make-hash-table :test 'eq)
  "Lookup table for RTF control words and control symbols.")

(defmacro define-rtf-destination (name &rest body)
  "Define an RTF destination changing control word.
When called, its destination group and the current environment
are accesible in the variables `group' and `environment'."
  `(progn (put ',name 'rtf-destination t)
	  (puthash ',name
		   (lambda (group environment)
		     ,@body
		     nil) ; Destinations don't return anything.
		   rtf-controls-hash)))
;; 

(defmacro define-rtf-control (name &rest body)
  "Define an RTF control word or control symbol.
When called, its argument and the current environment are
accesible in the variables `argument' and `environment'.  If
there was no argument, `argument' is nil."
  `(puthash ',name (lambda (argument environment)
		     ,@body
		     nil) ; Normal controls do not return anything.
	    rtf-controls-hash))

(defmacro define-rtf-special (name destination &rest body)
  "Define a special RTF control word or control symbol.

A special control is a control that consumes data from the token
stream as argument.  When called, its numerical argument, the
token stream and the current environment are accesible in the
variables `argument', `stream' and environment.  If there was no
argument, ARGUMENT is nil.  STREAM is a list of RTF expressions.

A special control is supposed to pop from the stream whatever
data it needs.  It is _required_ to return all the rest of the
stream which it did not consume."
  `(progn
     ,(when destination
	`(unless (get ',destination 'rtf-destination)
	   (error "Destination %s not defined" ',destination)))
     (put ',name 'rtf-special t)
     (puthash ',name
	      ,(if destination
		   `(lambda (argument stream environment)
		      (when (eq (rtf-environment-destination environment)
				',destination)
			,@body))
		 `(lambda (argument stream environment)
		    ,@body))
	      rtf-controls-hash)))



;;; Utilities

(defun rtf-destination-eval (group env &optional keywords)
  "Return the information in GROUP in an easily accesible form.
GROUP is supposed to be an RTF group and ENV the current
environment.  The return value is an list of the form

\(TEXTUAL-DATA . ALIST)

Where TEXTUAL-DATA is the concatenation of all unformatted text
in group and ALIST is an alist of the controls in GROUP.  The
members are cons cell with the control in the car and its
numerical argument in the cdr.  Controls without an argument have
`t' in the cdr.

The optional third argument KEYWORDS is an alist whose members
have the form

\(CONTROL . (KEY . VALUE))

When `rtf-destination-eval' encounters an control word
CONTROL (presumably without numerical argument) it returns (KEY .
VALUE) as part of ALIST above, instead.  This may be used to
group together mutually exclusive keywords which RTF defines as
flag setting.  For example, for parsing a paragraph style sheet
group, KEYWORDS could be

'((qc . (justification . center))
  (ql . (justification . left)))

So, if `rtf-destination-eval' finds `qc' in GROUP, it
returns `(justification . center)' within ALIST, rather than `(qc
. t)'."
  (let ((text (delq nil (mapcar (lambda (elt)
				     (and (or (stringp elt)
					      (rtf-special-p elt))
					  elt))
				   group)))
	(alist (delq nil
		     (mapcar (lambda (elt)
			       (cond ((rtf-group-p elt)
				      elt)
				     ((rtf-control-p elt)
				      (if (vectorp elt)
					  (cons (aref elt 0) (aref elt 1))
					(or (cdr (assq elt keywords))
					    (cons elt t))))
				     (t nil)))
			     group))))
    (cons (rtf-destination-process-text text env) alist)))

(defun rtf-destination-process-text (text-list env)
  (let (elt result)
    (while text-list
      (setq elt (pop text-list))
      (if (rtf-special-p elt)
	  (setq text-list (rtf-apply-special elt text-list env)) ; FIXME
	(push elt result)))
    (apply 'concat (nreverse result))))
				      
(defun rtf-remove-trailing-semicolon (str)
  "Return string STR without its (potential) trailing semicolon."
  (and (stringp str)
       (string-match
	(rx (and (submatch (+? anything))
		 (optional ?\;)
		 string-end))
	str)
       (match-string 1 str)))

(defun rtf-sort-and-vectorize (list func)
  "Return sort the numbered objects in LIST and return them in a vector.
FUNC is a function which returns the number of an object.  The
returned vector contains the objects at the position according to
the integer returned by FUNC.  Gaps are filled with nil."
  (when list
    (let ((list (sort list (lambda (elt1 elt2)
			     (> (funcall func elt1)
				(funcall func elt2))))))
      (let ((vect (make-vector (1+ (funcall func (car list))) nil)))
	(dolist (elt list)
	  (aset vect (funcall func elt) elt))
	vect))))


;;;; Header

;;; RTF Version

(define-rtf-control rtf
  ;; Maybe we could store the version somewhere?
  ;; OTOH: to what purpose?
  nil)


;;; Character Set

(define-rtf-control ansi
  (require 'code-pages)
  (setf (rtf-env-document-encoding
	 (rtf-environment-document environment))
	'windows-1252))

(define-rtf-control mac
  (setf (rtf-env-document-encoding
	 (rtf-environment-document environment))
	'mac-roman))

;;; Unicode RTF


;;; Font Table

(define-rtf-destination fonttbl
  (let ((fonts (rtf-parse-font-table group environment)))
    (setf (rtf-env-document-font-table
	   (rtf-environment-document environment))
	  (rtf-resolve-fonts fonts))
    ;; We store the original font information for now, just in case
    ;; ... FIXME
    (setf (rtf-env-document-orig-font-table
	   (rtf-environment-document environment))
	  fonts)))

(defvar rtf-font-family-alist
  '((roman . rtf-serif)
    (swiss . rtf-sans-serif)
    (modern . rtf-fixed-width)
    (script . nil)
    (decor . nil)
    (tech . nil)))

(defvar rtf-font-family-translations-alist
  '(("Arial" . swiss)
    ("NULL" . roman)
    ("Times New Roman" . roman)
    ("Courier New" . modern)))

(defun rtf-parse-font-table (table env)
;;   (if (atom (car table))
;;       (rtf-parse-font-table-1 table)
  (let ((flist (delq nil (mapcar (lambda (tbl)
				   (rtf-parse-font-info tbl env))
				   table))))
    (rtf-sort-and-vectorize flist 'rtf-env-font-num)))

(defun rtf-resolve-fonts (vect)
  (let ((face-vect (make-vector (length vect) nil))
	face)
    (dotimes (i (length vect))
      (when (aref vect i)
	(if (setq face (rtf-font-spec-to-face (aref vect i)))
	    (aset face-vect i face)
	  ;; FIXME: Try to find a matching font on the system.
	  nil)))
    face-vect))

(defun rtf-font-spec-to-face (font-spec)
  (or
   ;; First try the family.
   (and (rtf-env-font-family font-spec)
	(cdr (assq (rtf-env-font-family font-spec) 
		   rtf-font-family-alist)))
   ;; Look whether there is a predefined translation for this font.
   (and (rtf-env-font-name font-spec)
	(rtf-font-spec-to-face-1 (rtf-env-font-name font-spec)))
   ;; Then try the alternative name.
   (and (rtf-env-font-alt-name font-spec)
	(rtf-font-spec-to-face-1 (rtf-env-font-alt-name font-spec)))
   ;; Finally try the non-tagged name
   (and (rtf-env-font-non-tagged-name font-spec)
	(rtf-font-spec-to-face-1 (rtf-env-font-non-tagged-name font-spec)))))

(defun rtf-font-spec-to-face-1 (key)
  (let ((family (cdr (assoc key
			    rtf-font-family-translations-alist))))
    (and key
	 (cdr (assq family rtf-font-family-alist)))))

(defconst rtf-font-info-keywords
  '((froman . (family . roman))
    (fswiss . (family . swiss))
    (fmodern . (family . modern))
    (fscript . (family . script))
    (fdecor . (family . decor))
    (ftech . (family . tech))
    (fbidi . (family . bidi)))
"Keywords for `fonttbl' group parsing.
Passed to `rtf-destination-eval' as third argument when parsing
the fonttbl destination group.")

;; (define-skeleton egoge-skeleton
;;   ""
;;   "key: "
;;   "(cdr (assq '" str " alist))")

(defun rtf-parse-font-info (info env)
  (let* ((tblinfo (rtf-destination-eval info env
					rtf-font-info-keywords))
	 (alist (cdr tblinfo)))
    (make-rtf-env-font
     :num (cdr (assq 'f alist))
     :family (cdr (assq 'family alist))
     :charset (cdr (assq 'fcharset alist))
     :pitch (cdr (assq 'fprq alist))
;;     :panose 
;;     :non-tagged-name 
;; FIXME: deal with the `fname' control, which is yet another
;; exepction to the scheme.
     :name (rtf-remove-trailing-semicolon (car tblinfo))
     :alt-name (cadr (assq 'falt alist))
     :codepage (cdr (assq 'cpg alist)))))

;;   (when (listp info)
;;     (let ((f (make-rtf-env-font)))
;;       (while info
;; 	(let ((control (pop info)))
;; 	  (cond
;; 	   ;; Vectors: controls with arg.
;; 	   ((vectorp control)
;; 	    (let ((c (aref control 0))
;; 		  (v (aref control 1)))
;; 	      (case c
;; 		(fprq (setf (rtf-env-font-pitch f) v))
;; 		(fcharset (setf (rtf-env-font-charset f) v))
;; 		(f (setf (rtf-env-font-num f) v))
;; 		;;	      (fbias nil) ; ??
;; 		)))
;; 	   ;; Atoms: controls or fontname.
;; 	   ((atom control)
;; 	    (cond ((memq control
;; 			 '(fnil froman fswiss fmodern
;; 				fscript fdecor ftech fbidi))
;; 		   (setf (rtf-env-font-family f) control))
;; 		  ((stringp control)
;; 		   (setf (rtf-env-font-name f)
;; 			 (rtf-remove-trailing-semicolon control)))))
;; 	   ;; Lists: groups.
;; 	   ((listp control)
;; 	    (when (eq (car control) '*)
;; 	      (setq control (cdr control)))
;; 	    (case (car control)
;; 	      (panose (setf (rtf-env-font-panose f) (cadr control)))
;; 	      (falt (setf (rtf-env-font-altname f) (cadr control)))
;; 	      (fname (setf (rtf-env-font-nontaggedname f)
;; 			   (and (string-match (rx (and (submatch (+? anything))
;; 						       (one-or-more blank)
;; 						       ?\( (+? anything) ?\)
;; 						       (* anything)))
;; 					      (cadr control))
;; 				(match-string 1 (cadr control))))))))))
;;       ;; Return the font struct.
;;       f))

(define-rtf-control deff
  (setf (rtf-env-document-font-table
	 (rtf-environment-document environment))
	argument))


;;; File Tables

(define-rtf-destination filetbl
  nil)


;;; Color Table

(define-rtf-destination colortbl
  nil)

;;; Style Sheet

(define-rtf-destination stylesheet
  (let ((stylesheets (mapcar (lambda (sty)
			       (rtf-parse-stylesheet sty environment))
			       group)))
    (let ((document (rtf-environment-document environment))
	  secstyles parstyles charstyles)
      (dolist (sty stylesheets)
	(case (rtf-stylesheet-type sty)
	  (ds (push sty secstyles))
	  (s (push sty parstyles))
	  (cs (push sty charstyles))))
      (setf (rtf-env-document-sec-stylesheets document)
	    (rtf-sort-and-vectorize secstyles
				    'rtf-stylesheet-num))
      (setf (rtf-env-document-par-stylesheets document)
	    (rtf-sort-and-vectorize parstyles
				    'rtf-stylesheet-num))
      (setf (rtf-env-document-char-stylesheets document)
	    (rtf-sort-and-vectorize charstyles
				    'rtf-stylesheet-num)))))

(defconst rtf-style-parfmt-keywords
  '((ql . (justification . left))
    (qr . (justification . right))
    (qj . (justification . full))
    (qc . (justification . center))))

(defun rtf-parse-stylesheet (stysh env)
  (let* ((info (rtf-destination-eval (if (eq (car stysh) '*)
					 (cdr stysh)
				       stysh)
				     env
				     rtf-style-parfmt-keywords))
	 (alist (cdr info))
	 (type (or (assq 'ds alist)
		   (assq 's alist)
		   (assq 'cs alist)
		   (cons 's 0))))
    (make-rtf-stylesheet
     :type (car type)
     :num (cdr type)
     :basedon (cdr (assq 'sbasedon alist))
     :plain (cdr (assq 'plain alist))
     :name (rtf-remove-trailing-semicolon (car info))
     :next (cdr (assq 'snext alist))
     :autoupd (cdr (assq 'sautoupd alist))
     :keycode (cdr (assq 'keycode alist))
     ;; FIXME:
     :formatting  (if (assq 'cs alist)
		      (rtf-style-make-chrfmt alist type (car info))
		    (rtf-style-make-parfmt alist type (car info)))
     :hidden (cdr (assq 'shidden alist)))))

(defun rtf-style-make-parfmt (alist type name)
  (make-rtf-paragraph
   :style t
;;   :in-table (cdr (assq 'intbl alist))
   :justification (cdr (assq 'justification alist))
   :left-indent (cdr (assq 'li alist))
   :right-indent (cdr (assq 'ri alist))
   :space-before (cdr (assq 'sb alist))
   :space-after (cdr (assq 'sa alist))
   :charfmt (rtf-style-make-chrfmt alist type name)))

(defun rtf-style-make-chrfmt (alist type name)
  (make-rtf-character-props
   :style t
   :face (make-face (make-symbol
		     (format "rtf-%s-%d-%s"
			     (car type) (cdr type)
			     (if (stringp name)
				 (mapconcat 'identity
					    (split-string
					     (rtf-remove-trailing-semicolon
					      name)
					     " ")
					    "-")
			       "unnamed"))))
   :bold (cdr (assq 'b alist))
   :italic (cdr (assq 'i alist))
   :underlined (cdr (assq 'i alist))
   :foreground (cdr (assq 'cf alist))
   :background (cdr (assq 'cb alist))
   :subscr-pos (cdr (assq 'dn alist))
   :caps (cdr (assq 'caps alist))
   :charscalex (cdr (assq 'charscalex alist))
   :font (cdr (assq 'f alist))
   :fontsize (cdr (assq 'fs alist))
   :language (cdr (assq 'lang alist))))

;;; List Table

;;; Track Changes (Revision Marks)


;;;; Document Area

;;; Information Group

(define-rtf-destination info
  nil)

;;; Document Formatting Properties


;;;; Section Text

;;; Section Formatting Properties

;;; Headers and Footers


;;;; Paragraph Text

;;; Paragraph Formatting Properties

(define-rtf-control par
;;  (rtf-new-paragraph environment)
  (setf (rtf-environment-parfmt environment)
	(copy-rtf-paragraph 
	 (rtf-environment-parfmt environment)))
  (rtf-text "\n" environment))

(define-rtf-control pard
  (setf (rtf-environment-parfmt environment)
	(make-rtf-paragraph)))

(define-rtf-control s
  (setf (rtf-paragraph-style
	 (rtf-environment-parfmt environment))
	argument))

;; Alignment

(define-rtf-control ql
  (setf (rtf-paragraph-justification
	 (rtf-environment-parfmt environment))
	'left))
  
(define-rtf-control qr
  (setf (rtf-paragraph-justification
	 (rtf-environment-parfmt environment))
	'right))

(define-rtf-control qj
  (setf (rtf-paragraph-justification
	 (rtf-environment-parfmt environment))
	'full))

(define-rtf-control qc
  (setf (rtf-paragraph-justification
	 (rtf-environment-parfmt environment))
	'center))

;; Indentation

(define-rtf-control fi
  nil)

(define-rtf-control li
  (setf (rtf-paragraph-left-indent
	 (rtf-environment-parfmt environment))
	argument))

(define-rtf-control ri
  (setf (rtf-paragraph-right-indent
	 (rtf-environment-parfmt environment))
	argument))

;; Subdocuments

;; Bidirectional Control

;;; Tabs


;;; Bullets and Numbering


;;; Paragraph Borders


;;; Paragraph Shading


;;; Positioned Objects and Frames


;;; Table Definitions


;;;; Character Text

;;; Font (character) Formatting Properties

(define-rtf-control plain
  (setf (rtf-environment-charfmt environment)
	(make-rtf-character-props
	 :face nil)))

(define-rtf-control b
  (setf (rtf-environment-charfmt environment)
	(copy-rtf-character-props
	 (rtf-environment-charfmt environment)))
  (setf (rtf-character-props-bold
	 (rtf-environment-charfmt environment))
	(if (and argument
		 (numberp argument)
		 (= 0 argument))
	    nil
	  t)))

(define-rtf-control caps
  (setf (rtf-environment-charfmt environment)
	(copy-rtf-character-props
	 (rtf-environment-charfmt environment)))
  (setf (rtf-character-props-caps
	 (rtf-environment-charfmt environment))
	t))

(define-rtf-control charscalex
  (setf (rtf-environment-charfmt environment)
	(copy-rtf-character-props
	 (rtf-environment-charfmt environment)))
  (setf (rtf-character-props-charscalex
	 (copy-rtf-character-props (rtf-environment-charfmt environment)))
	argument))

(define-rtf-control f
  (setf (rtf-environment-charfmt environment)
	(copy-rtf-character-props
	 (rtf-environment-charfmt environment)))
  (setf (rtf-character-props-font
	 (rtf-environment-charfmt environment))
	(aref
	 (rtf-env-document-font-table
	  (rtf-environment-document environment))
	 argument)))

(define-rtf-control fs
  (setf (rtf-environment-charfmt environment)
	(copy-rtf-character-props
	 (rtf-environment-charfmt environment)))
  (setf (rtf-character-props-fontsize
	 (rtf-environment-charfmt environment))
	(* argument 5)))

(define-rtf-control i
  (setf (rtf-environment-charfmt environment)
	(copy-rtf-character-props
	 (rtf-environment-charfmt environment)))
  (setf (rtf-character-props-italic
	 (rtf-environment-charfmt environment))
	(if (and argument
		 (numberp argument)
		 (= 0 argument))
	    nil
	  t)))


(define-rtf-control cs
  (setf (rtf-environment-charfmt environment)
	(copy-rtf-character-props
	 (rtf-environment-charfmt environment)))
  (setf (rtf-character-props-style
	 argument)))

;; TODO: ccs and fcharset


;;; Character Borders and Shading


;;; Associated Character Properties


;;; Highlighting


;;; Special Characters

(define-rtf-control {
  (rtf-text "{" environment))

(define-rtf-control }
  (rtf-text "}" environment))
  

(define-rtf-special \' nil
  (when (stringp (car stream))
    (cons (concat (decode-coding-string
		   (string (rtf-hex-string-to-number
			    (substring (car stream) 0 2)))
		   (rtf-env-document-encoding
		    (rtf-environment-document environment)))
		  (substring (car stream) 2))
	  (cdr stream))))

(defsubst rtf-char-to-number (char)
  (cond ((and (>= char ?0) (<= char ?9))
	 (- char ?0))
	((and (>= char ?a) (<= char ?f))
	 (+ 10 (- char ?a)))
	((and (>= char ?A) (<= char ?F))
	 (+ 10 (- char ?A)))
	(t (error "Not a valid hex char: %S" char))))

(defun rtf-hex-string-to-number (str)
  (let ((count (length str))
	(e 0)
	(number 0))
    (while (> count 0)
      (setq count (1- count))
      (setq number (+ number
		      (* (rtf-char-to-number (aref str count))
			 (expt 16 e))))
      (setq e (1+ e)))
    number))



;; --------------------------------------------------------------------
;;;; Finishing Setup

(defun rtf-setup-document (doc)
  (make-rtf-document
;;    :font-table (rtf-document-setup-font-table
;; 		(rtf-env-document-font-table doc))
   :char-styles (rtf-document-setup-styles
 		 (rtf-env-document-char-stylesheets doc))
   :par-styles (rtf-document-setup-styles
		(rtf-env-document-par-stylesheets doc))
   :info (rtf-env-document-info doc)))

(defun rtf-document-setup-char-styles (styles)
  (let ((vect (make-vector (length styles) nil)))
    (dotimes (i (length styles))
      (when (aref styles i)
	(let ((face (rtf-character-props-face
		     (rtf-stylesheet-formatting (aref styles i)))))
	  (set-face-attribute face nil
			      :inherit (rtf-resolve-char-prop-face-inheritance
					(aref styles i) styles)))))
    vect))

(defun rtf-resolve-char-prop-face-inheritance (style styles-vect)
  (let ((list nil ;; (list rtf-default-face)
	      )
	(basedon (rtf-stylesheet-basedon style))
	(charprops (rtf-stylesheet-formatting style)))
    (when (and basedon
	       (aref styles-vect basedon))
      (push (rtf-character-props-face
	     (aref styles-vect basedon))
	    list))
    (when (rtf-character-props-bold charprops)
      (push 'bold list))
    (when (rtf-character-props-italic charprops)
      (push 'italic list))
    list))
		
(defun rtf-document-setup-styles (styles)
  (let ((vect (make-vector (length styles) nil)))
    (dotimes (i (length styles))
      (when (aref styles i)
	(aset vect i
	      (rtf-resolve-style i styles))))
    vect))

(defun rtf-resolve-style (n styles-vect)
  (let* ((this-style (aref styles-vect n))
	 (style (make-rtf-stylesheet))
	 (type (rtf-stylesheet-type this-style))
	 (base-styles (list this-style))
	 (face (if (eq type 'cs)
		   ;; character style sheet
		    (rtf-character-props-face
		     (rtf-stylesheet-formatting this-style))
		 ;; paragraph style sheet
		 (rtf-character-props-face
		  (rtf-paragraph-charfmt
		   (rtf-stylesheet-formatting this-style)))))
	 (face-inherit nil)
	 (current (and (rtf-stylesheet-basedon this-style)
		       (aref styles-vect
			     (rtf-stylesheet-basedon this-style)))))
    (while current
      (push current base-styles)
      (if (integerp (rtf-stylesheet-basedon current))
	  (setq current (aref styles-vect 
			      (rtf-stylesheet-basedon current)))
	(setq current nil)))
    ;; We go through the list of ancestors, "oldest" one first.
    (dolist (bstyle base-styles)
      (setq style (rtf-merge-struct style bstyle))
      (if (eq type 'cs)
	  (push (rtf-character-props-face
		 (rtf-stylesheet-formatting bstyle))
		face-inherit)
	(push (rtf-character-props-face
	       (rtf-paragraph-charfmt
		(rtf-stylesheet-formatting bstyle)))
	      face-inherit)))
    (set-face-attribute face nil :inherit face-inherit)
    style))

(defun rtf-merge-struct (s1 s2)
  (if (and s1 s2
	   (/= (length s1) (length s2)))
      (error "Trying to merge different structs.")
    (cond ((null s1)
	   (copy-sequence s2))
	  ((null s2)
	   (copy-sequence s1))
	  (t (let ((vect (make-vector (length (or s1 s2)) nil)))
	       (dotimes (i (length vect))
		 (if (and (vectorp (aref s1 i))
			  (vectorp (aref s2 i)))
		     (aset vect i
			   (rtf-merge-struct (aref s1 i)
					     (aref s2 i)))
		   (aset vect i
			 (or (aref s2 i)
			     (aref s1 i)))))
		 vect)))))
  


			
;; Local Variables:
;; sentence-end-double-space: t
;; eval: (font-lock-add-keywords nil '(("(\\(define-rtf-\\(?:control\\|special\\|destination\\)\\)" (1 font-lock-keyword-face) ("\\s-+\\(.*?\\)\\>" nil nil (1 font-lock-function-name-face)))))
;; End:

(provide 'rtf-controls)
;;; rtf-controls.el ends here

;;; state-m.el --- Macro for defining a state machine

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Oliver Scholz <alkibiades@gmx.de>
;; Keywords: lisp

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

;; This package provides a macro `run-state-machine' which is supposed
;; to make the writing of simple and fast reader functions easy.

;; A state machine is defined as a number of states, each of which
;; defines one or more transitions to another state.  The syntax for
;; state definitions is:

;; ( NAME TRANSITION {TRANSITION} )

;; Name is a symbol that serves as the referer to that state in
;; transitions.  The definition of a transition looks like this:

;; ( MATCHER ADD-TO-RESULT ADVANCE NEXT-STATE )

;; MATCHER may be a character, a string, specifying a regexp, a cons
;; cell of two characters specifying a character range, a list of
;; characters or character ranges specifying alternative matches, or
;; the symbol t indicating a default transition.

;; ADD-TO-RESULT may be nil, t or a function.

;; ADVANCE may be nil, t or a function.

;; NEXT-STATE is a symbol: the name of a state.

;;; Code:

;; If a transition has a cons cell or a list as MATCHER, it is
;; expanded into as many transitions with a single character as
;; MATCHER as necessary to cover the range/alternative thus described.
;; So (... ((?a . ?c) nil t next-state) ...) becomes
;; (... (?a nil t next-state) (?b nil t next-state) (?c nil t next-state) ...)

(defun state-m-expand-char-range (range)
  "Expand char-range into a list of characters.
The argument RANGE is of the form (STARTING-CHAR . END-CHAR); the
return value is a list of all characters between STARTING-CHAR and
END-CHAR (both included).

This functions is supposed to be used only internally by the
macro `run-state-machine' during macroexpansion time."
  (and (or (and (consp range)
		(numberp (car range))
		(numberp (cdr range)))
	   (error "Not a valid char-range: %s" range))
       (or (< (car range) (cdr range))
	   (error "A char-range must start with the lower char: %s" range))
       (let ((list nil))
	 (dotimes (i (1+ (- (cdr range) (car range))) (nreverse list))
	   (push (+ (car range) i) list)))))

(defun state-m-expand-transition (trans)
  "Expand a transition with char-alternatives into a list of transitions.

The argument TRANS is a transition (a list).  The car of a transition
is called MATCHER.  If MATCHER is a regexp, a single character or the
symbol t, this function returns the transition unmodified. 

If MATCHER is a character-alternative, the transition is
\"expanded\".  A character-alternative may be a cons cell specifying a
range of characters or a list of cons cells or single characters.  The
cdr of each transition in the returned list is the cdr of TRANS and
their car is a single character within the range specified by the car
of TRANS.

This functions is supposed to be used only internally by the
macro `run-state-machine' during macroexpansion time."
  ;; The car of trans may be either an atom, a char-range (a cons
  ;; cell) or a list of character alternatives.
  (cond ((atom (car trans)) trans)
	((nlistp (cdar trans))
	 ;; The car of trans is a cons cell.
	 (mapcar (lambda (elt) (cons elt (cdr trans)))
		 (state-m-expand-char-range (car trans))))
	(t
      ;; It is a list. 
	 (mapcar (lambda (char)
		   (cons char (cdr trans)))
		 (apply 'nconc
			;; We use `nconc' to splice the return value of
			;; `state-ma-expand-char-range' into the list of
			;; characters.
			(mapcar (lambda (elt)
				  (cond ((numberp elt) (list elt))
					;; Expand character ranges of the
					;; form (STARTING-CHAR . END-CHAR),
					;; so that `char-list' is a list of
					;; characters only.
					((consp elt)
					 (state-m-expand-char-range elt))
					(t (error "Invalid MATCHER: %s"
						  (car trans)))))
				(car trans)))))))

(defun state-m-deal-with-lambda-maybe (expr)
  "Transform an anonymous function, if necessary.

Unless EXPR is a lambda expression, return it unmodified.  Lambda
expressions and functions quoted with `function' are returned as a
list of a `,' and the expression itself, so that `(lambda ...)'
becomes `(\, (lambda ...))'.

This is done in order to force byte compilation of anonymous functions
specified as ADD-TO-RESULT or ADVANCE in a transition by wrapping the
final states vectors into a backquote.

This functions is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  ;; Example: in the expansion of `run-state-machine' the state
  ;; vectors will read:
  ;; (backquote [ ... (( ... (\, (lambda ...)) ... )) ...])
  ;; instead of simply:
  ;; [ ... (( ... (lambda ...) ... )) ...]
  (if (and (listp expr) 
	   (or (eq (car expr) 'lambda)
	       (eq (car expr) 'function)))
      (list '\, expr)
    expr))

(defun state-m-make-transitions (state-definitions)
  "Transform STATE-DEFINITIONS into a list of transitions.

STATE-DEFINITIONS is a list whose elements have the form 
\(STATE-NAME TRANSITION-1 ... TRANSITION-N).  Each transition has the
name of the next state as its last element.

This function gets rid of the state-names and replaces them in the
transitions with an integer, because the expanded macro
`run-state-machine' refers to states not by their names, but by their
position in an array.  This function returns a list of lists of
transitions, whereby the N-th element in the returned list is the list
of transitions that make up the definition of state N.

This functions is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
    ;; First associate the name of the states with consecutively
    ;; running integers, that will make up their position in the
    ;; vector.
    (let* ((count 0)
	   (pos-alist (mapcar
		       (lambda (elt)
			 (cons elt (prog1 count
				     (setq count (1+ count)))))
		       (mapcar 'car state-definitions))))
      ;; Replace in the transitions the names of the next states by
      ;; their position in the array.
      (mapcar (lambda (elt)
		(let ((trans (cdr elt))
		      (list nil))
		  (dolist (i trans (nreverse list))
		    (if (listp (car i))
			;; Expand transitions which specify a list of
			;; alternative chars or a character range as
			;; MATCHER.
			(dolist (elt (state-m-expand-transition i))
			  (push (list (car elt)
				      ;; Transform anonymous functions
				      ;; as ADD-TO-RESULT or ADVANCE,
				      ;; if necessary.
				      (state-m-deal-with-lambda-maybe 
				       (nth 1 elt))
				      (state-m-deal-with-lambda-maybe
				       (nth 2 elt))
				      ;; Get the position of
				      ;; the next state.
				      (cdr (assq (nth 3 elt) pos-alist)))
				list))
		      ;; Since its car is not a list, transition `i'
		      ;; needs no expansion.
		      (push (list (car i)
				  (state-m-deal-with-lambda-maybe
				   (nth 1 i))
				  (state-m-deal-with-lambda-maybe
				   (nth 2 i))
				  (cdr (assq (nth 3 i) pos-alist)))
			    list)))))
	      state-definitions)))

;; A state is defined by a set of possible transitions.  We maintain
;; the definition of the state machine in up to three different
;; vectors, according to the type of the MATCHER of a transition.  The
;; element N of each vector is a part of the definition of state N.  It
;; may be a list of transitions of the same type or nil, if the state
;; definition contains no transitions of that type.  At macroexpansion
;; time `run-state-machine' replaces the name of a state as NEXT-STATE
;; in each transition with an integer referring to the position of
;; that state in the vectors.

;; The three vectors are: 
;; `regexp-vect' -- transitions whose MATCHER is a regexp
;; `char-vect' -- transitions whose MATCHER is a character
;; `defaults-vect' -- transitions whose MATCHER is the symbol t

;; For example a state definition like

;; ...
;; (example-state (?a t t another-state-1)
;; 	       (?b t t another-state-2)
;; 	       ("some \\(regexp\\)" 1 t another-state-3)
;; 	       (t t t another-state-4))
;; ...

;; could be transformed into something like this: (Of course, the name
;; of the next state -- `another-state-*' -- is replaced by an integer
;; indicating the position of that next state in the vectors.)

;; regexp-vector:
;; [ ... (("some \\(regexp\\)" 1 t 5)) ... ]

;; char-vector:
;; [ ... ((?a t t 3) (?b t t 4)) ... ]

;; defaults-vector:
;; [ ... ((t t t 6)) ... ]

;; The resulting state machine will check for a matching transition in
;; those vectors in that order, regular expressions first, defaults
;; last.

;; If a state machine contains no transitions of a certain type, the
;; according vector and the checking is omitted.  That is, if a
;; state machine contains -- for example -- only transitions with a
;; character as MATCHER, there is no need to maintain a vector for
;; regexps and to check for a matching regexp at run time.

(defun state-m-list-transitions-by-type (type trans-list)
  "Return a list of transitions of type TYPE.

TYPE may be one of the symbols `regexp', `char' or
`default'.  TRANS-LIST is a list of lists of transitions as returned by
`state-m-make-transitions'.  The return value is a list, whose elements
are lists of transitions whose MATCHER is of the type TYPE or
nil.  This is used to sort the transitions.

This function is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  (let ((func (cond ((eq type 'regexp) 'stringp)
		    ((eq type 'char) 'numberp)
		    ((eq type 'default) (lambda (x) (eq x t)))
		    (t (error "Unknown type: %s" type)))))
    (mapcar (lambda (state)
	      (delq nil (mapcar (lambda (trans)
				  (if (funcall func (car trans))
				      trans
				    nil))
				state)))
	    trans-list)))

(defun state-m-empty-vector-p (vect)
  "Return t if vector VECT contains only nil.

This function is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  (let ((predicate t))
    (dotimes (i (length vect) predicate)
      (when (aref vect i)
	(setq predicate nil)))))

(defun state-m-parse-states (states-list)
  "Parse state definitions.

Generate proper transitions from the state definitions, thereby expand
transitions whose MATCHER is a character alternative.  Sort the
transitions into different vectors according to the type of their
MATCHER.

The return value is a list of three vectors.  The first one contains
transitions with a regular expression as MATCHER.  The second one
contains transitions with a character as MATCHER.  And the third one
contains the default transitions.

This function is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  (let* ((transitions (state-m-make-transitions states-list))
	 ;; Make the states vectors by sorting the transitions
	 ;; according to the type of MATCHER.  
	 (regexp-vect (apply 'vector
			     (state-m-list-transitions-by-type
				    'regexp transitions)))
	 (char-vect (apply 'vector
			   (state-m-list-transitions-by-type
			    'char transitions)))
	 (default-vect (apply 'vector
			      (state-m-list-transitions-by-type
			       'default transitions))))
    ;; Return the vectors in a list.
    (mapcar (lambda (vect-name)
	      (if (state-m-empty-vector-p (symbol-value vect-name))
		  ;; If all elements of the vector are nil, return nil
		  ;; instead of the vector.
		  nil
		;; Put the vector inside a `(backquote ...)' in order
		;; to force byte compilation of anonymous functions as
		;; ADD-TO-RESULT or ADVANCE (inside of
		;; `state-m-make-transitions' we made sure that lambda
		;; expressions are surrounded by the nessecary syntax
		;; to accomplish this).
		(list 'backquote (symbol-value vect-name))))
	    '(regexp-vect char-vect default-vect))))

;; The following is a bit hairy.  We need to construct the
;; state machine, but each of the three possible vectors needs a
;; slightly different treatment.  For example: to find the matching
;; transition the state machine has to check the MATCHER of each
;; transition in the regexp-vector with `looking-at', while a matching
;; transition in the char-vector is the first one whose MATCHER is
;; equal to the character after point and while the first transition
;; in the defaults-vector matches unconditionally.  Something similar
;; is true for adding something to the return value of the
;; state machine and for advancing point in the buffer as specified by
;; ADD-TO-RESULT and ADVANCE.

;; We deal with this by modularizing the lisp-expressions for this
;; tasks and putting them together in the function
;; `state-m-make-main-expr'.

;; The benefit of this approach is, that if a state machine has no
;; transitions of one or two of the three types, then the expansion of
;; `run-state-machine' contains not a single trait of the according
;; vector.

(defun state-m-make-find-expr (type vect state-var)
  "Return the expression to find the right transition.

The first argument TYPE specifies the type of the transitions-vector,
it may be one of the symbols `regexp', `char' or `default'.

The second argument VECT is the vector that holds the states.  The
third argument STATE-VAR is the name of the variable that is used to
hold the current state in the expanded code.

This function is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  (cond ((eq type 'regexp)
	 ;; regexp: go through each possible
	 ;; transition until MATCHER in one of them
	 ;; matches the text after point.
	 `(catch 'state-found
	    (dolist (trans (aref ,vect ,state-var) nil)
	      (when (looking-at (car trans))
		(throw 'state-found trans)))))
	((eq type 'char)
	 ;; character: get the transition whose
	 ;; MATCHER is eq to the char after point.
	 `(assq (char-after)(aref ,vect ,state-var)))
	((eq type 'default)
	 ;; default: get the next best transition
	 ;; unconditionally.
	 `(car (aref ,vect ,state-var)))))

(defun state-m-make-add-expr (type aux-var output-var)
  "Return the expression to add to the return value of the state machine.

The argument TYPE specifies the type of the transitions-vector, it may
be one of the symbols `regexp', `char' or `default'.

The second argument AUX-VAR is the name of an auxiliary variable that
is used to store temporary input in the expanded code.  In this case
it holds the second element of a transition \(ADD-TO-RESULT).

The third argument OUTPUT-VAR is the name of the variable that is used
hold the future return value in the expanded code.

This function is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  ;; In all cases ADD-TO-RESULT may be a function.  But if it is not,
  ;; regexp-transitions differ from char-transitions and the defaults
  ;; in the respect that the former may specify a subexpression (an
  ;; integer) to add to the result, while it is only t or nil for the
  ;; latter.
  (if (eq type 'regexp)
      ;; Regexps: deal with subexpressions.
      `(if (numberp (if (eq ,aux-var t)
			;; We interpret t as 0.
			(setq ,aux-var 0)
		      ,aux-var))
	   (setq ,output-var
		 (concat ,output-var
			 (match-string ,aux-var)))
	 (setq ,output-var (funcall ,aux-var ,output-var (match-string 0))))
    ;; Chars/defaults: add the char after point.
    `(if (eq ,aux-var t)
	 (setq ,output-var (concat ,output-var
				   (char-to-string (char-after))))
       (setq ,output-var
	     (funcall ,aux-var ,output-var (char-to-string (char-after)))))))
  
(defun state-m-make-advance-expr (type aux-var)
  "Return the expression to advance point in the current buffer.

The argument TYPE specifies the type of the transitions-vector, it may
be one of the symbols 'regexp, 'char or 'default.

The second argument AUX-VAR is the name of a variable that is used to
store temporary values in the expanded code.  In this case it is used
to hold the third element of a transition (ADVANCE).

This function is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  ;; As with ADD-TO-RESULT, ADVANCE may be a function in all
  ;; cases.
  (if (eq type 'regexp)
      ;; Regexp: deal with subexpressions.
      `(if (numberp (if (eq ,aux-var t)
			;; Again we interpret t as 0.
			(setq ,aux-var 0)
		      ,aux-var))
	   (goto-char (match-end ,aux-var))
	 (funcall ,aux-var))
    ;; Chars/defaults: move forward by one char.
    `(if (eq ,aux-var t)
	 (forward-char 1)
       (funcall ,aux-var))))

(defun state-m-make-main-expr-1 (find-expr add-expr advance-expr
					   state-var trans-var
					   output-var aux-var)
  "Return an expression for the construction of a state machine.

The returned expression finds the right transition in a transitions
vector and deals with ADD-TO-RESULT and ADVANCE appropriately. (All
this is determined by FIND-EXPR, ADD-EXPR and ADVANCE-EXPR.)

The other arguments are the names of variables that are used in the
expanded code: STATE-VAR -- name of the variable that holds the number
of the current state, TRANS-VAR -- name of the variable that holds the
matching transition, OUTPUT-VAR -- name of the variable that holds the
result value, AUX-VAR -- name of a variable that is used to store
temporary values.

At run time the expression exits immediately and returns nil if it can
not find any matching transition.

This function is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  `(and (setq ,trans-var ,find-expr)
	(progn
	  ;; Maybe add to the return value.
	  (setq ,aux-var (nth 1 ,trans-var))
	  (when ,aux-var
	    ,add-expr)
	  (setq ,aux-var (nth 2 ,trans-var))
	  (when ,aux-var
	    ,advance-expr)
	  ;; Return the (position of the) next state.
	  (nth 3 ,trans-var))))

(defun state-m-make-main-expr (states statevar outputvar transvar auxvar)
  "Return the main expression to construct a state machine.

The first argument STATES is a list of definition of states; each
element is a list whose car is the name of the state and whose cdr is
a list of transitions.

The following arguments are the names of variables that are used in
the expanded code: STATEVAR -- variable that holds the number of the
current state, OUTPUTVAR -- variable that holds the result value,
TRANSVAR -- variable that holds the matching transition, AUXVAR --
variable used to store temporary values.

This function is supposed to be used only internally by the macro
`run-state-machine' during macroexpansion time."
  (let ((tr-vectors (state-m-parse-states states))
	(expression (list `(error "State machine error at state %d"
				  ,statevar))))
    ;; The resulting expression shall check for the right transition
    ;; to the next state in that order:
    ;; look-up regexp-vector --> look-up char-vector --> 
    ;; look-up defaults-vector --> error
    (dolist (type '((default . 2) (char . 1) (regexp . 0)))
      (when (nth (cdr type) tr-vectors)
	(push (state-m-make-main-expr-1
	       (state-m-make-find-expr (car type)
				       (nth (cdr type) tr-vectors)
				       statevar)
	       (state-m-make-add-expr (car type) auxvar outputvar)
	       (state-m-make-advance-expr (car type) auxvar)
	       statevar transvar outputvar auxvar)
	      expression)))
    (if (> (length expression) 1)
	(cons 'or expression)
      (car expression))))

;;;###autoload
(defmacro run-state-machine (spec &rest states)
  "Define a state machine and run it.

The first argument SPEC is a list of the form 
\(RESULT-VARIABLE END-OF-BUFFER-EXPRESSION).

RESULT-VARIABLE should be a symbol or nil.  In the former case the
symbol is used as the variable-name to store the return value of the
state machine at run time.  This makes it possible to get hold of the
return value in the states, for example to change its type.  By
default the return value is a string.

When the state machine reaches the end of the buffer at run time, it
terminates.  END-OF-BUFFER-EXPRESSION should be a valid Lisp
expression, it is called when the state machine reaches the end of the
current buffer.  If it is omitted or nil, the default behaviour is to
return nil.

The following arguments are the definitions of the state.  Each state
defines one or more transitions to following states.  The syntax is:

STATE         ::= ( NAME TRANSITION {TRANSITION} )
TRANSITION    ::= ( MATCHER ADD-TO-RESULT ADVANCE NEXT-STATE )
MATCHER       ::= CHARACTER | CHAR-ALTERN | REGEXP | t
CHAR-ALTERN   ::= CHAR-LIST | CHAR-RANGE
CHAR-LIST     ::= ( {CHARACTER} | {CHAR-RANGE} )
CHAR-RANGE    ::= ( CHARACTER . CHARACTER )
ADD-TO-RESULT ::= t | nil | FUNCTION | INTEGER
ADVANCE       ::= t | nil | FUNCTION | INTEGER

In each state the state machine goes through the transitions to find
the following state.  As soon as it finds a \"matching\" transition,
it adds to the return value depending on ADD-TO-RESULT, moves point in
the buffer according to ADVANCE and switches to the state indicated by
NEXT-STATE.

If MATCHER is a regular expression, the transition matches if the
regexp matches the text after point with `looking-at'.  If MATCHER is
a character, the transition matches, when the character after point is
`eq' to MATCHER.  If MATCHER is t, the transition matches
unconditionally.

Note that, while the order of transitions in a state-definition is
somewhat important, regular expressions as MATCHER have always
precedence, regardless where they were defined.  Likewise, the state
machine checks the default transition (a transition whose MATCHER is
`t') always last.

If ADD-TO-RESULT is t, the state machine appends the character after
point (if MATCHER is a char or t) or the matching text (if MATCHER is
a regexp) to the return value.  If MATCHER is a regular expression,
ADD-TO-RESULT may be an integer, specifying which subexpression to add
to the return value.  ADD-TO-RESULT may be a function that takes two
arguments: the current return value and the pending input.  Both are
strings.  The return value of the state machine is set to the returned
value of this function.

If ADVANCE is t, the state machine moves point forward by one
character (if MATCHER is a character or t) or to the end of the
matching text (if MATCHER is a regexp).  If MATCHER is a regular
expression, ADVANCE may be an integer, specifying the subexpression to
whose end point should move.  ADVANCE may be a function that takes no
arguments.  In that case this function takes the responsibility to
move point.

The starting state is always the first state in the row.  Specifying
the state name `exit' as the following state in a transition means to
terminate processing; you can't define a state named `exit'."
  ;; Add an exit state at position 0.
  (push (list 'exit) states)
  (let ((state (make-symbol "state"))
	(eob-function (or (cadr spec) 'ignore))
	(retval (or (car spec) (make-symbol "return-value")))
	(state (make-symbol "state"))
	(trans (make-symbol "transition"))
	(auxvar (make-symbol "auxiliary-variable")))
    `(let ((,state 1)
	   (,retval nil)
	   ,trans ,auxvar)
       (while (> ,state 0) ; 0 is the "exit" state.
	 (if (eobp)
	     (progn ,(if (cdr spec)
			 `(setq ,retval ,(cadr spec))
		       `(setq ,retval nil))
		    (setq ,state 0)) ; exit
	   (setq ,state ,(state-m-make-main-expr 
			  states state retval trans auxvar))))
       ,retval)))

(provide 'state-m)

;;Local Variables:
;;no-byte-compile: t
;;End:

;;; state-m.el ends here

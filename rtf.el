;;; rtf.el --- Mode for editing RTF documents.

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Oliver Scholz <epameinondas@gmx.de>
;; Keywords: 

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
(eval-when-compile (require 'cl))

(add-to-list 'load-path (expand-file-name "."))
(require 'rtf-reader)
(require 'rtf-par)

(defconst rtf-font-lock-keywords
  '((rtf-find-rtf-property-region 
     (0 (rtf-resolve-faces (get-text-property (match-beginning 0) 'rtf-paragraph)
			   (get-text-property (match-beginning 0) 'rtf-characters))))))

(defun rtf-resolve-faces (par chars)
  (let ((styles-list nil)
	(par-style (and par
			(rtf-paragraph-style par))))
    (when par-style
      (let ((parent
	     (rtf-stylesheet-formatting
	      (aref (rtf-document-par-styles rtf-document)
		    par-style))))
	(push (rtf-character-props-face
	       (rtf-paragraph-charfmt parent))
	      styles-list)))
    (when (and chars
	       (rtf-character-props-face chars))
      (push (rtf-character-props-face chars) styles-list))
    (when (rtf-character-props-font chars)
      (push (rtf-character-props-font chars)
	    styles-list))
    (when (rtf-character-props-bold chars)
      (push 'bold styles-list))
    (when (rtf-character-props-italic chars)
      (push 'italic styles-list))
    (when (rtf-character-props-fontsize chars)
      (push (list :height (rtf-character-props-fontsize chars))
	    styles-list))
    styles-list))
    
(defvar rtf-left-margin-width 5)

(define-derived-mode rtf-major-mode text-mode "RTF Major"
  "FIXME"
  (setq font-lock-defaults
	(list rtf-font-lock-keywords))
  (set (make-local-variable 'fill-paragraph-function)
       'rtf-fill-paragraph)
  (setq left-margin-width rtf-left-margin-width)
  (set-window-buffer (selected-window) (current-buffer))
  )
		 

(define-minor-mode rtf-mode
  "FIXME"
  nil " RTF" nil
  (cond ((not rtf-mode)
	 ;; Turn mode off.
	 (font-lock-remove-keywords nil rtf-font-lock-keywords))
	(t ;; Turn mode on.
	 (font-lock-add-keywords nil rtf-font-lock-keywords))
	 ))

;; (defstruct rtf-paragraph
;;   left-margin
;;   right-margin
;;   test
;;   face)

;; (defun rtf-paragraph-to-text-properties (par)
;;   (when (rtf-paragraph-p par)
;;     (list 'face (rtf-paragraph-face par))))

;; Two text properties are relevant: `rtf-paragraph' and `rtf-extra'.

(defun rtf-find-rtf-property-region (limit)
  (let ((char-start (text-property-not-all (point)
					   limit 'rtf-characters nil))
	(par-start (text-property-not-all (point)
					  limit 'rtf-paragraph nil)))
    (if (and (not par-start)
	     (not char-start))
	nil
      (let ((start (if (and par-start char-start)
				(min par-start char-start)
			      (or par-start char-start))))
	(goto-char (min (next-single-property-change start
						     'rtf-characters nil limit)
			(next-single-property-change start
						     'rtf-characters nil limit)))
	(set-match-data (list start (point)))
;;     (put-text-property start (point)
;; 		       'face
;; 		       (rtf-characters-to-text-properties
;; 			(get-text-property (1- (point)) 'rtf-characters)))
;;     (goto-char (or (next-single-property-change start
;; 						'rtf-paragraph nil limit)
;; 		   limit))
;;     (add-text-properties start (point)
;; 			 (rtf-paragraph-to-text-properties
;; 			  (get-text-property (1- (point)) 'rtf-paragraph)))
	t))))

;; (defun rtf-characters-to-text-properties (charprops)
;;   (list :family (rtf-character-props-font charprops)
;; 	:weight (when (rtf-character-props-bold charprops)
;; 		  'bold)
;; 	:height (let ((height (rtf-character-props-height charprops)))
;; 		  (when (numberp height)
;; 		    (ceiling (* 1.2 height))))
;; 	:slant (when (rtf-character-props-italic charprops)
;; 		 'italic)))
	

(defface rtf-serif
  '((((type x))
     (:family "adobe-times" :height 1.1))
    (((type w32 mac))
     (:family "Times New Roman" :height 1.1)))
  "FIXME"
  :group 'rtf)

(defvar rtf-default-face 'rtf-default)

(defface rtf-default
  '((t (:inherit rtf-serif)))
  "FIXME")

(defface rtf-sans-serif
  '((((type x))
     (:family "adobe-helvetica" :height 1.1))
    (((type w32 mac))
     (:family "Arial" :height 1.1)))
  "FIXME"
  :group 'rtf)

(defface rtf-fixed-width
  '((t (:inherit fixed-pitch)))
  "FIXME")


;;;; Experimental

(defface rtf-internal-height-1
  '((t (:height 1.2)))
  "FIXME")

(defface rtf-internal-height-2
  '((t (:height 1.5)))
  "FIXME")


(defun rtf-internal-insert-text ()
  (insert-file-contents (expand-file-name "COPYING" data-directory)
			nil 322 1850))

(defun rtf-experimental ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*tmp*"))
  (rtf-internal-insert-text)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (unless (or (eobp) (looking-at "\n"))
	(backward-char 1)
	(delete-char 1)
	(ucs-insert "AD"))))
  (setq buffer-display-table
	(let ((table (make-display-table))
	      (offset (* (face-id 'rtf-internal-height-1)
			 (expt 2 19))))
	  (aset table (decode-char 'ucs #xAD) ; private use area
		[?\n])
	  (aset table ?\n (vconcat (list ?\n (+ ?\n offset))))
;; 	  (aset table ?a (vconcat (list (+ ?a offset) (+ ?b offset))))
	  table))
  )

(defconst rtf-experimental-font-lock-keywords
  '(("\n"
     (0 (progn
	  (unless (and (get-text-property (match-beginning 0) 'hard)
		       (char-equal (char-before (match-beginning 0)) ?\n))
	    (put-text-property (match-beginning 0)
			       (match-end 0)
			       'display
			       (concat
				(propertize " " 
					    'face 
					    'rtf-internal-height-2
					    'intangible t)
				"\n"))))))))
	   

(define-derived-mode rtf-experimental-mode text-mode "EXP"
  ""
  (use-hard-newlines 1 'guess)
  (setq font-lock-defaults
	(list rtf-experimental-font-lock-keywords))
  (set (make-local-variable 'font-lock-extra-managed-props)
       (list 'display))
  )
  

;; (defun rtf-experimental ()
;;   (interactive)
;;   (switch-to-buffer (generate-new-buffer "*tmp*"))
;;   (rtf-internal-insert-text)
;;   (rtf-experimental-mode)

;;   )

;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (search-forward "\n" nil t)
;;       (unless (eq (char-before (1- (point))) ?\n)
;; 	(put-text-property (match-beginning 0)
;; 			   (match-end 0)
;; 			   'display ))))

;;; Filling



(provide 'rtf)
;;; rtf.el ends here

;;; rtf-par.el --- Code dealing with paragraph formatting in RTF mode.

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
(defun rtf-fill-region-as-paragraph (from to &optional indent bstring props)
  (let ((newline-before (or (eq (char-before from)
				?\n)
			    (= from (point-min))))
	(newline-after (or (eq (char-after to)
			       ?\n)
			   (= to (point-max)))))
    (save-restriction
      (narrow-to-region from to)
      ;; Deal with trailing white-space.
      (goto-char (point-max))
      (skip-chars-backward " \n\t")
      (delete-region (point) (point-max))
      ;; Delete leading whitespace and all newline-chars.
      (goto-char (point-min))
      (delete-region (point) (progn (skip-chars-forward " \t\n")
				    (point)))
      (fill-delete-newlines (point-min) (point-max) nil nil nil)
      (goto-char (point-min))
      (unless newline-before (newline 1))
      ;;     (let ((indentation (if indent
      ;; 			   (apply 'propertize " " (rtf-formatting-space-props indent))
      ;; 			 nil)))
      ;;       (when bstring
      ;; 	;; The first indentation is special insofar we have to add
      ;; 	;; STRING.
      ;; 	(insert (propertize bstring 'rtf-ephemeral t
      ;; 			    'rear-nonsticky '(rtf-ephemeral))))
      ;; Indent the text (if required) and break the lines.
      (let (linebeg)
	(while (not (eobp))
	  ;; 	  (when indentation
	  ;; 	    (insert indentation))
	  (setq linebeg (point))
	  (move-to-column (1+ (current-fill-column)))
	  (if (progn (when (not (eobp))
		       ;; Find the position where we'll break the line.
		       (fill-move-to-break-point linebeg)
		       ;; Check again to see if we got to the end of the
		       ;; paragraph.
		       (skip-chars-forward " \t"))
		     (not (eobp)))
	      ;; Found a place to cut.
	      (fill-newline)
	    (goto-char (point-max))
	    (delete-horizontal-space))))
      (unless newline-after (newline 1)))))
  
(defun rtf-fill-paragraph (&optional ignore)
  (interactive)
  (let ((beg (rtf-extend-to-rogue-newline (rtf-paragraph-beginning-position) -1))
	(end (rtf-extend-to-rogue-newline (rtf-paragraph-end-position) 1)))
    (save-excursion (rtf-fill-region-as-paragraph beg end)))
  (point))

(defun rtf-paragraph-beginning-position (&optional pos)
  (or pos (setq pos (point)))
  (if (or (= pos (point-min))
	  (not (eq (get-text-property pos 'rtf-paragraph)
		   (get-text-property (1- pos) 'rtf-paragraph))))
      pos
    (previous-single-property-change pos
				     'rtf-paragraph (current-buffer) (point-min))))

(defun rtf-paragraph-end-position (&optional pos)
  (or pos (setq pos (point)))
;;   (if (or (= pos (point-max))
;; 	  (not (eq (get-text-property pos 'rtf-paragraph)
;; 		   (get-text-property (1+ pos) 'rtf-paragraph))))
;;       pos
  (next-single-property-change pos
			       'rtf-paragraph (current-buffer) (point-max)))

(defun rtf-extend-to-rogue-newline (pos direction)
  (let ((par-prop (get-text-property pos 'rtf-paragraph (current-buffer))))
    (while (and (eq (char-after (+ pos direction)) ?\n)
		(not (get-text-property (+ pos direction) 'rtf-paragraph (current-buffer))))
      (setq pos (+ direction pos)))
    pos))

(defun rtf-forward-paragraph ()
  (interactive)
  (goto-char (rtf-paragraph-end-position)))

(defun rtf-fill-region (beg end)
  (interactive "r")
  (setq beg
	(rtf-extend-to-rogue-newline
	 (rtf-paragraph-beginning-position beg) -1))
  (setq end (rtf-extend-to-rogue-newline
	     (rtf-paragraph-end-position end) 1))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (rtf-fill-paragraph)
      (forward-char)
      (rtf-forward-paragraph)))))


(provide 'rtf-par)
;;; rtf-par.el ends here

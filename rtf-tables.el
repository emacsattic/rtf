;;; rtf-tables.el --- Table support for RTF mode

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

;;;; EXPERIMENTAL

(defun rtf-insert-cell (column width str)
  (let (list)
    (with-temp-buffer
      (insert str)
      (let ((fill-column width))
	(fill-region (point-min) (point-max)))
      (beginning-of-line)
      (push (buffer-substring (point) (line-end-position))
	    list)
      (while (= 0 (forward-line -1))
	(push (buffer-substring (point) (line-end-position))
	      list)))
    (save-excursion
      (dolist (line list)
	(end-of-line)
	(unless (= (line-beginning-position) (point))
	  (insert (propertize " " 'display `(space :align-to ,column))))
	(insert line)
	(unless (= 0 (forward-line 1))
	  (newline))))))

(defun rtf-test-string ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "COPYING" data-directory)
			  nil 322 588)
    (buffer-substring (point-min) (point-max))))

(defun rtf-test ()
  (interactive)
  (switch-to-buffer "*tmp*")
  (rtf-insert-cell 0 20 (rtf-test-string))
  (rtf-insert-cell 22 20 (rtf-test-string))
  (rtf-insert-cell 44 20 (rtf-test-string))
  (goto-char (point-max))
  (newline)
  (rtf-insert-cell 0 20 (rtf-test-string))
  (rtf-insert-cell 22 20 (rtf-test-string))
  (rtf-insert-cell 44 20 (rtf-test-string)))

;; (defsubst rtf-move-to-line (line)
;;   (goto-line 

;; (defun rtf-insert-cell-line (row column str)

(defconst rtf-image-buffer-tag
  (make-symbol "This is an RTF image buffer."))

(defvar rtf-image-buffer nil)

(defmacro with-rtf-image (spec &rest body)
  `(with-temp-buffer
     (set (make-local-variable 'rtf-image-buffer) rtf-image-buffer-tag)
     (dotimes (x ,(car spec))
       (dotimes (y ,(cadr spec))
	 (insert ?0))
       (insert ?\n))
     ,@body
     (goto-char (point-min))
     (insert (format "P1\n%d %d\n" ,(car spec) ,(cadr spec)))
     (while (not (eobp))
       (forward-char)
       (if (char-equal (char-after) ?\n)
	   (forward-char)
	 (insert ?\ )))
     (buffer-substring-no-properties (point-min) (point-max))))

(defun rtf-draw-line (from to)
  (if (not (eq rtf-image-buffer rtf-image-buffer-tag))
      (error "Not inside a `with-rtf-image' macro")
    (unless (or (= (car from) (car to))
		(= (cdr from) (cdr to)))
      ;; I am too lazy to look up the Bensenham (sp?) Algorithm right
      ;; now.
      (error "Only horizontal or vertical drawing supported so far"))
    (cond ((= (car from) (car to))
	   (let ((x (car from))
		 (height (- (cdr to) (cdr from))))
	     (goto-line (cdr from))
	     (dotimes (ignore height)
	       (move-to-column x)
	       (delete-char 1)
	       (insert ?1)
	       (forward-line 1))))
	  ((= (cdr from) (cdr to))
	   (goto-line (cdr from))
	   (move-to-column (car from))
	   (delete-region (point) (save-excursion
				    (move-to-column (car to))
				    (point)))
	   (insert-char ?1 (- (car to) (car from)))))))


;; (defun rtf-test ()
;;   (let ((width (aref (font-info (face-font 'default)) 3))
;; 	(height (face-attribute 
;;   (insert (create-image (with-

(provide 'rtf-tables)
;;; rtf-tables.el ends here

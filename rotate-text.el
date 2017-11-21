;;; rotate-text.el --- rotate text through a sets of words -*- lexical-binding: t; -*-

;; Copyright (C) 2017 SuJiKiNen

;; Author: http://www.emacswiki.org/emacs/RotateText
;; Maintainer: SuJiKiNen <SuJiKiNen@gmail.com>
;; URL: https://github.com/SuJiKiNen/rotate-text
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  replace a match text in a set list with next element.

;;
;;; Code:
;;;; rotate text

;;;###autoload
(defvar rotate-text-rotations '(("True" "False")
                                ("yes" "no")
                                ("1" "0")) "List of text rotation sets.")

(defvar rotate-text-bound 'symbol
  "the value can be word or symbol")

;;;###autoload
(make-variable-buffer-local 'rotate-text-rotations)

;;;###autoload
(defun rotate-text-region (beg end)
  "Rotate all matches in `rotate-text-rotations' between point and mark."
  (interactive "r")
  (let ((regexp (rotate-text-convert-rotations-to-regexp
                 rotate-text-rotations))
        (end-mark (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp (marker-position end-mark) t)
        (let* ((found (match-string 0))
               (replace (rotate-text-next found)))
          (replace-match replace))))))

;;;###autoload
(defun rotate-text-string (string &optional rotations)
  "Rotate all matches in STRING using associations in ROTATIONS.
If ROTATIONS are not given it defaults to `rotate-text-rotations'."
  (let ((regexp (rotate-text-convert-rotations-to-regexp
                 (or rotations rotate-text-rotations)))
        (start 0))
    (while (string-match regexp string start)
      (let* ((found (match-string 0 string))
             (replace (rotate-text-next
                       found
                       (or rotations rotate-text-rotations))))
        (setq start (+ (match-end 0)
                       (- (length replace) (length found))))
        (setq string (replace-match replace nil t string))))
    string))

;;;###autoload
(defun rotate-text-next (string &optional rotations)
  "Return the next element after STRING in ROTATIONS."
  (let ((rots (rotate-text-get-rotations-for
               string
               (or rotations rotate-text-rotations))))
    (if (> (length rots) 1)
        (error (format "Ambiguous rotation for %s" string))
      (if (< (length rots) 1)
          ;; If we get this far, this should not occur:
          (error (format "Unknown rotation for %s" string))
        (let ((occurs-in-rots (member string (car rots))))
          (if (null occurs-in-rots)
              ;; If we get this far, this should *never* occur:
              (error (format "Unknown rotation for %s" string))
            (if (null (cdr occurs-in-rots))
                (caar rots)
              (cadr occurs-in-rots))))))))

;;;###autoload
(defun rotate-text-get-rotations-for (string &optional rotations)
  "Return the string rotations for STRING in ROTATIONS."
  (remq nil (mapcar (lambda (rot) (if (member string rot) rot))
                    (or rotations rotate-text-rotations))))

;;;###autoload
(defun rotate-text-convert-rotations-to-regexp (rotations)
  (regexp-opt (rotate-text-flatten-list rotations)
              (cond ('word 'words)
                    ('symbol 'symbols))))

;;;###autoload
(defun rotate-text-flatten-list (list-of-lists)
  "Flatten LIST-OF-LISTS to a single list.
Example:
  (rotate-text-flatten-list '((a b c) (1 ((2 3)))))
    => (a b c 1 2 3)"
  (if (null list-of-lists)
      list-of-lists
    (if (listp list-of-lists)
        (append (rotate-text-flatten-list (car list-of-lists))
                (rotate-text-flatten-list (cdr list-of-lists)))
      (list list-of-lists))))

;;;###autoload
(defun rotate-text-at-point ()
  "Rotate word at point based on sets in `rotate-text-rotations'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word))
        (opoint (point)))
    (when (consp bounds)
      (let ((beg (car bounds))
            (end (copy-marker (cdr bounds))))
        (rotate-text-region beg end)
        (goto-char (if (> opoint end) end opoint))))))

;;;###autoload
(defun rotate-text-at-current-line ()
  "Rotate word at line based on sets in `rotate-text-rotations'."
  (interactive)
  (rotate-text-region (line-beginning-position) (line-end-position)))

(defun rotate-text-or-fallback (fallback)
  "If point is at end of a word, then else call the fallback."
  (interactive)
  (if (looking-at "\\>")
      (rotate-text-region (save-excursion (forward-word -1) (point))
                          (point))
    (when (foundp fallback)
      (funcall fallback))))

(provide 'rotate-text)

;;; rotate-text.el ends here

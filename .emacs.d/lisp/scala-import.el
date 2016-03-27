;;; scala-import.el --- Manage Scala imports

;; Copyright (C) 2016 Ankur Dave

;; Author: Ankur Dave <ankurdave@gmail.com>
;; Url: https://github.com/ankurdave/dotfiles
;; Created: 17 Jan 2015
;; Version: 1.0
;; Package-Requires: ((dash "2.5.0") (s "1.9.0") (emacs "24"))

;; This file is not a part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for managing imports in Scala projects. Provides
;; `scala-import-organize' to organize import statements in the current file;
;; `scala-import-goto-class-at-point' to open the file containing the class at
;; point; and `scala-import-class-at-point' to find the package of the class at
;; point and import it, prompting if there are multiple classes with the same
;; name. The latter two require the repository to be stored in Git.

;;; Code:

(require 'dash)
(require 's)
(require 'projectile)

;;;###autoload
(defun scala-import-organize ()
  "Organize Scala imports in current file."
  (interactive)
  (let* ((import-block (scala-import--get-import-block))
         (beg (car import-block))
         (end (cdr import-block))
         (new-imports
          (scala-import--organize-import-string
           (buffer-substring-no-properties beg end)
           (scala-import--get-package))))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert new-imports))))

;;;###autoload
(defun scala-import-class-at-point ()
  "Import the class at point at the top of the file.
If the class name is ambiguous in the current repository, present
the choices to the user."
  (interactive)
  (let* ((class (symbol-name (symbol-at-point)))
         (fully-qualified-class (scala-import--get-package-for-class class))
         (import-statement (format "import %s\n" fully-qualified-class)))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "^package ")
      (forward-line)
      (insert "\n")
      (insert import-statement)
      (scala-import-organize))))

;;;###autoload
(defun scala-import-goto-class-at-point ()
  "Open the class at point.
If the class name is ambiguous in the current repository, present
the choices to the user."
  (interactive)
  (if (not (symbol-at-point))
      (call-interactively #'find-tag)
    (let ((class (symbol-name (symbol-at-point))))
      (if (and (eq last-command 'scala-import-goto-class-at-point) find-tag-history)
          (find-tag (car find-tag-history) t)
        (find-tag (scala-import--get-package-for-class class)))
      (search-forward class (line-end-position))
      (setq this-command 'scala-import-goto-class-at-point))))

(defun scala-import--parse-imports (str)
  "Return a list of imports in the given string."
  (split-string str "^import " t "[[:space:]\n]+"))

(defun scala-import--get-package ()
  "Get the package of the current file."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^package ")
    (let ((beg (point)))
      (end-of-line)
      (buffer-substring-no-properties beg (point)))))

(defun scala-import--list-organize-by-predicates (elems preds)
  "Reorder ELEMS into chunks that match each of PREDS in order,
returning a list of chunks."
  (if preds
      (let* ((first-pred (car preds))
             (matches-rest (-separate first-pred elems))
             (matches (first matches-rest))
             (rest-elems (second matches-rest))
             (rest-preds (cdr preds)))
        (cons matches (scala-import--list-organize-by-predicates
                       rest-elems rest-preds)))
    (list elems)))

(defun scala-import--organize-import-string (import-string package)
  "Organize imports in IMPORT-STRING assuming we are in PACKAGE."
  (let* ((imports (scala-import--parse-imports import-string))
         (sorted-imports (-distinct (sort imports 'string<)))
         (grouped-imports
          (scala-import--list-organize-by-predicates
           sorted-imports
           (list (lambda (s) (string-prefix-p "java." s))
                 (lambda (s) (string-prefix-p "javax." s))
                 (lambda (s) (string-prefix-p "scala." s))
                 ;; Put same-package imports in their own group
                 (lambda (s) (not (string-prefix-p package s))))))
         (grouped-imports-no-nulls (-filter #'identity grouped-imports))
         (import-string
          (mapconcat
           (lambda (group)
             (mapconcat (lambda (s) (format "import %s\n" s)) group ""))
           grouped-imports-no-nulls "\n")))
    (concat import-string "\n")))

(defun scala-import--get-import-block ()
  "Return the imports at the top of the file as a string."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^import ")
    (beginning-of-line)
    (let ((beg (point)))
      (while (or (looking-at "^import ")
                 (looking-at "^\\s-")
                 (looking-at "^$"))
        (forward-line))
      (cons beg (point)))))

(defun scala-import--obarray-to-list (obarray)
  (let ((result '()))
    (mapatoms (lambda (x) (push x result)) obarray)
    result))

(defun scala-import--get-package-for-class (class)
  "Return the package for the specified class.
If the class name is ambiguous in the current repository, present
the choices to the user."
  (let* ((tags (-map #'symbol-name
                     (scala-import--obarray-to-list (tags-completion-table))))
         (fully-qualified-class-list
          (let ((case-fold-search nil))
            (-filter
             (lambda (tag)
               (string-match-p (concat "\\b" (regexp-quote class) "$") tag))
             tags)))
         (selected
          (pcase fully-qualified-class-list
            (`nil (user-error "No declaration found for %s (%s candidates)"
                              class (length tags)))
            (`(,unique-match) unique-match)
            (match-list
             (completing-read "Fully qualified class: " match-list)))))
    selected))

(provide 'scala-import)

;;; scala-import.el ends here

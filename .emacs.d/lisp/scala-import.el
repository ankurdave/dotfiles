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
  (let* ((class (symbol-at-point))
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
  (let* ((class (symbol-at-point))
         (location (scala-import--get-location-for-class class))
         (filename (car location))
         (line-number (cdr location)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line-number))))

(defvar scala-import-dependencies nil
  "A list of paths to other Git repos where classes may reside.
Each path should end in a slash. You can set this for a project
using a '.dir-locals.el' file as described in Info node
`Per-Directory Local Variables'.")

(defun scala-import-add-dependency (dep)
  (interactive "D")
  (save-current-buffer
    (let ((default-directory (projectile-project-root)))
      (add-dir-local-variable
       nil 'scala-import-dependencies (cons dep scala-import-dependencies)))
    (save-buffer))
  (hack-dir-local-variables-non-file-buffer))

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

(defvar scala-import--class-object-trait-posix-re
  "\\<(class|object|trait|type)\\>"
  "Regexp matching declarations for classes and other importable identifiers.
This should return accurate results for all files matched by
`scala-import--scala-java-file-glob'.")

(defvar scala-import--scala-java-file-glob "*.{scala,java}"
  "Glob matching files that could contain a declaration.")

(defun scala-import--run-command-in-project-and-dependencies
    (command &optional prepend-dir)
  "Run shell COMMAND in the current project root and dependencies.
Return the output as a single string, with each line prepended by
the directory in which the command was run if PREPEND-DIR is
non-nil. The list of dependent projects is specified by
`scala-import-dependencies'."
  (mapconcat
   (lambda (default-directory)
     (if prepend-dir
         (s-join "\n"
                 (-map (lambda (line) (concat default-directory line))
                       (s-split "\n" (shell-command-to-string command) t)))
       (shell-command-to-string command)))
   (cons (projectile-project-root) scala-import-dependencies)
   ""))

(defun scala-import--get-package-for-class (class)
  "Return the package for the specified class.
If the class name is ambiguous in the current repository, present
the choices to the user."
  (let* ((command
          (format "git --no-pager grep -h --all-match --extended-regexp --no-color -e %s -e ^package -- %s"
                  (shell-quote-argument
                   (format "%s\\s+%s\\>"
                           scala-import--class-object-trait-posix-re class))
                  scala-import--scala-java-file-glob))
         (matches-string
          (scala-import--run-command-in-project-and-dependencies command))
         (matches-list (split-string matches-string "\n" t))
         (package-list
          (scala-import--handle-package-objects
           (-filter (lambda (str) (string-match-p "^package" str))
                    matches-list)))
         (fully-qualified-class-list
          (-uniq (-map
                  (lambda (package) (format "%s.%s" package class))
                  package-list)))
         (selected
          (pcase fully-qualified-class-list
            (`nil (user-error "No declaration found for %s" class))
            (`(,unique-match) unique-match)
            (match-list
             (completing-read "Fully qualified class: " match-list)))))
    selected))

(defun scala-import--handle-package-objects (package-list &optional cur-package)
  "Prefix package objects in PACKAGE-LIST with the preceding package."
  (when package-list
    (if (string-match-p "^package object" (car package-list))
        (cons
         (format
          "%s.%s" cur-package
          (s-trim
           (replace-regexp-in-string
            "{.*$" ""
            (replace-regexp-in-string
             "^package object" "" (car package-list)))))
         (scala-import--handle-package-objects (cdr package-list) cur-package))
      (let ((new-cur-package
             (s-trim (replace-regexp-in-string
                      "^package" "" (car package-list)))))
        (cons new-cur-package
              (scala-import--handle-package-objects
               (cdr package-list) new-cur-package))))))

(defun scala-import--get-location-for-class (class)
  "Return the location where the specified class is declared.
The location is represented as a cons cell of the filename and
the line. If the class name is ambiguous in the current
repository, present the choices to the user."
  (let* ((command
          (format "git --no-pager grep --extended-regexp --full-name --no-color --line-number -e %s -- %s"
                  (shell-quote-argument
                   (format "%s\\s+%s\\>"
                           scala-import--class-object-trait-posix-re class))
                  scala-import--scala-java-file-glob))
         (matches-string
          (scala-import--run-command-in-project-and-dependencies command t))
         (matches-list (split-string matches-string "\n" t))
         (matches-parsed
          (-map (lambda (match-line) (split-string match-line ":"))
                matches-list))
         (matching-files
          (-uniq (-map (lambda (match) (nth 0 match)) matches-parsed)))
         (selected-file
          (pcase matching-files
            (`nil (user-error "No declaration found for %s" class))
            (`(,unique-match) unique-match)
            (match-list (completing-read "File name: " match-list))))
         (selected-line
          (nth 1 (car (-filter (lambda (match)
                                 (equal selected-file (nth 0 match)))
                               matches-parsed)))))
    (cons selected-file
          (string-to-number selected-line))))

(provide 'scala-import)

;;; scala-import.el ends here

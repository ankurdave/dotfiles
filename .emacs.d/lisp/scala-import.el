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

(defvar scala-import--makefile
  "tags:\n\tsctags -e -f %s.tags -R $(realpath ../)")

;;;###autoload
(defun scala-import-init ()
  "Initialize project tags using sctags."
  (interactive)
  (make-directory (concat (projectile-project-root) ".tags") t)
  (let ((default-directory
          (file-name-as-directory (concat (projectile-project-root) ".tags"))))
    (write-region
     (format scala-import--makefile (projectile-project-name)) nil "Makefile" nil nil nil t)
    (compile "make")))

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
         (fully-qualified-class
          (first (scala-import--get-location-for-class class)))
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
         (file (nth 1 location))
         (line (nth 2 location)))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun scala-import--parse-imports (str)
  "Return a list of imports in the given string."
  (-filter #'s-present?
           (-map #'s-trim
                 (split-string str "^import " t))))

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

(defun scala-import--parse-match-line (match-line)
  (let* ((fqid-loc (split-string match-line "\t"))
         (fqid (first fqid-loc))
         (file-line (split-string (second fqid-loc) ":"))
         (file (first file-line))
         (line (string-to-number (second file-line))))
    (list fqid file line)))

;; TODO: order options by similarity to current package
;; TODO: handle multiple identifiers with same fully-qualified name (eg class
;; and companion object) by showing both with disambiguation when jumping, and
;; by deduplicating when importing
;; TODO: store tags in sqlite with indexing
;; TODO: support Java
;; TODO: browse all tags more quickly by defining a helm source
;; TODO: integrate with eldoc
(defun scala-import--get-location-for-class (class)
  "Return the location for the specified class.
The location is represented as the list (fqid file line), where
fqid is the fully-qualified version of CLASS. If the class name
is ambiguous in the current repository, present the choices to
the user. If CLASS is nil, present all available tags to the user."
  (let* ((command-template
          (if (not class)
              "cat *.tags"
            (if (executable-find "ag")
                "ag --nofilename --follow --case-sensitive %s *.tags"
              "grep -h -e %s *.tags")))
         (command
          (format command-template
                  (shell-quote-argument (format "\\b%s\t" class))))
         (tags-directory (concat (projectile-project-root)
                                 (file-name-as-directory ".tags")))
         (matches-string
          (let ((default-directory tags-directory))
            (with-timer "search tags"
              (shell-command-to-string command))))
         (matches-list (split-string matches-string "\n" t))
         (matches-parsed
          (append
           (-map #'scala-import--parse-match-line matches-list)
           (if class
               (scala-import--get-locations-for-java-class class)
             nil))))
    (pcase matches-parsed
      (`nil (user-error "No declaration found for %s" class))
      (`(,unique-match) unique-match)
      (match-list
       (assoc
        (completing-read "Fully qualified class: " match-list nil t)
        match-list)))))

(defun scala-import--prefix-class-with-preceding-package
    (class matches-parsed &optional cur-package)
  "Prefix classes in MATCHES-PARSED with the preceding package."
  (cond
   ((not matches-parsed) nil)
   ((string-match-p "^package\\>" (nth 2 (car matches-parsed)))
    (scala-import--prefix-class-with-preceding-package
     class
     (cdr matches-parsed)
     (s-trim
      (replace-regexp-in-string
       ";.*" ""
       (replace-regexp-in-string "^package\\>" "" (nth 2 (car matches-parsed)))))))
   (t
    (let ((file (nth 0 (car matches-parsed)))
          (line (string-to-number (nth 1 (car matches-parsed))))
          (contents (nth 2 (car matches-parsed))))
      (cons
       (list file line (format "%s.%s" cur-package class))
       (scala-import--prefix-class-with-preceding-package
        class (cdr matches-parsed) cur-package))))))

(defun scala-import--get-locations-for-java-class (class)
  "Return all locations for the specified Java class.
The location is represented as the list (fqid file line), where
fqid is the fully-qualified version of CLASS."
  (let* ((command
          (format "git --no-pager grep --line-number --full-name --all-match --extended-regexp --no-color -e %s -e ^package -- %s"
                  (shell-quote-argument (format "\\<(class|enum)\\s+%s\\>" class))
                   (shell-quote-argument "*.java")))
         (matches-string
          (let ((default-directory (projectile-project-root)))
            (shell-command-to-string command)))
         (matches-list (split-string matches-string "\n" t))
         (matches-parsed
          (-map (lambda (match-line) (split-string match-line ":"))
                matches-list))
         (fully-qualified-class-list
          (scala-import--prefix-class-with-preceding-package
           class matches-parsed))
         (locations-absolute
          (-map (lambda (location)
                  (let ((file (nth 0 location))
                        (line (nth 1 location))
                        (fqid (nth 2 location)))
                    (list fqid (concat (projectile-project-root) file) line)))
                fully-qualified-class-list)))
    locations-absolute))

(provide 'scala-import)

;;; scala-import.el ends here

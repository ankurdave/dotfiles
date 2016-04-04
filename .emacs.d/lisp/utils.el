(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun smart-split ()
  "Split the frame into as many 80-column windows as possible."
  (interactive)
  (delete-other-windows)
  (let ((num-windows (/ (frame-width) 80)))
    (dotimes (i (1- num-windows))
      (split-window-right)))
  (balance-windows))

;; From http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun switch-to-other-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (defun get-2nd-mru-window ()
    (let (best-window best-time time)
      (dolist (window (window-list-1 nil 'nomini nil))
        (when (not (equal window (selected-window)))
          (setq time (window-use-time window))
          (when (or (not best-time) (> time best-time))
            (setq best-time time)
            (setq best-window window))))
      best-window))
  (cond
   ((minibufferp) nil)
   ((equal (count-windows) 1) (switch-to-buffer (other-buffer)))
   ((equal (count-windows) 2) (other-window 1))
   (t (select-window (get-2nd-mru-window)))))

(defun new-eshell (&optional name)
  "Opens an eshell in the current directory named according to NAME.

By default, NAME is the current directory. If given a prefix
argument, prompt for NAME."
  (interactive
   (list
    (if current-prefix-arg
        (read-string "Shell name: ")
      (abbreviate-file-name default-directory))))
  (let ((eshell-buffer-name (format "*eshell %s*" name)))
    (eshell)))

(defun new-shell (&optional name)
  "Open a shell in the current directory named according to NAME.

By default, NAME is the current directory. If given a prefix
argument, prompt for NAME."
  (interactive
   (list
    (if current-prefix-arg
        (read-string "Shell name: ")
      (abbreviate-file-name default-directory))))
  (shell (format "*shell-%s*" name))
  (delete-region (point-min) (point-max))
  (rename-shell name))

(defun rename-shell (&optional name)
  "Rename the current shell buffer according to NAME."
  (interactive "sShell name: ")
  (rename-buffer (format "*shell-%s*" name))
  (comint-simple-send
   (get-buffer-process (current-buffer))
   (format "export PS1=\"\033[33m%s\033[0m:\033[35m\\W\033[0m$ \"" name)))

(defun add-to-env-path (variable &rest values)
  "Add each string in VALUES to the colon-separated VARIABLE.

Deduplicate values in VARIABLE, and return the new value of
VARIABLE."
  (setenv variable
          (mapconcat
           'identity
           (delete-dups
            (append values (split-string (getenv variable) ":" t)))
           ":")))

(defmacro make-backward-kill-word-fn (backward-kill-word-fn
                                      &optional backward-kill-word-args)
  "Construct a function that kills the region if active,
otherwise invokes BACKWARD-KILL-WORD-FN, which must be an unquoted symbol."
  (let* ((bkwf-name (symbol-name backward-kill-word-fn))
         (defun-name (intern (concat "kill-region-or-" bkwf-name)))
         (docstring (format "Kill the region if active, otherwise invoke %s."
                            bkwf-name)))
    `(defun ,defun-name ()
       ,docstring
       (interactive)
       (if (region-active-p)
          (kill-region (region-beginning) (region-end))
         (apply (quote ,backward-kill-word-fn) (quote ,backward-kill-word-args))))))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun sort-sexps (reverse beg end)
  "Sort sexps in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse
                 #'ignore
                 #'end-of-defun
                 (lambda () (forward-sexp) (backward-sexp))))))

(defun sort-package-configurations ()
  (interactive)
  (let ((saved-pos (point)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^;;; Package configuration:")
      (forward-line)
      (let ((beg (point)))
        (re-search-forward "^;;; Package configuration ends here")
        (forward-line -1)
        (sort-sexps nil beg (point))))
    (goto-char saved-pos)))

(defun sexp-beginning-position ()
  "Return position of the first character inside the current sexp."
  (require 'smartparens)
  (save-excursion
    (sp-beginning-of-sexp)
    (point)))

(defun sexp-end-position ()
  "Return position of the last character inside the current sexp."
  (require 'smartparens)
  (save-excursion
    (sp-end-of-sexp)
    (point)))

(defun sort-symbols-in-sexp (reverse)
  "Sort symbols in sexp alphabetically, in REVERSE if negative."
  (interactive "*P")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&"
                      (sexp-beginning-position) (sexp-end-position)))

;; From http://mgalgs.github.io/2011/11/30/elisp-pretty-numbers.html
(defun my-thousands-separate (num)
  "Formats the (possibly floating point) number with a thousands
separator."
  (let* ((nstr (number-to-string num))
         (dot-ind (string-match "\\." nstr))
         (nstr-no-decimal (if dot-ind
                              (substring nstr 0 dot-ind)
                            nstr))
         (nrest (if dot-ind
                    (substring nstr dot-ind)
                  nil))
         (pretty nil)
         (cnt 0))
    (dolist (c (reverse (append nstr-no-decimal nil)))
      (if (and (zerop (% cnt 3)) (> cnt 0))
          (setq pretty (cons ?, pretty)))
      (setq pretty (cons c pretty))
      (setq cnt (1+ cnt)))
    (concat pretty nrest)))

(defun format-number-at-point ()
  (interactive)
  (let ((prettified (my-thousands-separate (number-at-point))))
    (delete-region (point) (save-excursion (forward-word) (point)))
    (insert prettified)))

;; https://github.com/DinoChiesa/dpchiesa-elisp/blob/master/dired-fixups.el
(defun format-file-size (file-size)
  "This is a redefinition of the function from `dired.el'. This
fixes the formatting of file sizes in dired mode, to support very
large files. Without this change, dired supports 8 digits max,
which is up to 10gb. Some files are larger than that.
"
  (if (< file-size 1024)
      (format (if (zerop file-size) "0" "%d") file-size)
    (do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
         ;; kilo, mega, giga, tera, peta, exa
         (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
        ((< file-size 1024) (format "%.1f%s" file-size (car post-fixes))))))

(defun format-file-size-at-point ()
  (interactive)
  (let ((n (number-at-point)))
    (delete-region (point) (save-excursion (forward-word) (point)))
    (insert (format-file-size n))))

(defun next-line-same-column ()
  (interactive)
  (let ((column-number (count-matches "\t" (line-beginning-position) (point))))
    (beginning-of-line 2)
    (search-forward "\t" (line-end-position) t column-number)))

(defun next-nonempty-column ()
  (interactive)
  (let ((match-pos
         (save-excursion
           (next-line-same-column)
           (while (looking-at "\t")
             (next-line-same-column))
           (point))))
    (if (eq match-pos (point-max))
        (user-error "Reached end of buffer")
      (push-mark)               ; Save the user's current position before moving
      (goto-char match-pos))))

(defun date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun simplify-time-at-point ()
  "Converts, e.g., \"12m34.6s\" to \"754.6 s\"."
  (interactive)
  (let* ((minutes-start (point))
         (minutes (string-to-number
                   (buffer-substring-no-properties
                    minutes-start
                    (progn
                      (while (looking-at "[0-9].")
                        (forward-char))
                      (point))))))
    (forward-char)
    (let* ((seconds-start (point))
           (seconds (string-to-number
                     (buffer-substring-no-properties
                      seconds-start
                      (progn
                        (while (looking-at "[0-9.]")
                          (forward-char))
                        (point))))))
      (delete-region minutes-start (point))
      (insert (format "%f " (+ (* minutes 60) seconds))))))

(defun copy-filename-as-kill ()
  "Copy full path to current file into the kill ring."
  (interactive)
  (kill-new buffer-file-name)
  (message "%s" buffer-file-name))

(defun copy-relative-filename-as-kill ()
  "Copy project-relative path of current file into the kill ring."
  (interactive)
  (eval-when-compile
    (require 's))
  (let ((rel-name (s-chop-prefix (projectile-project-root) buffer-file-name)))
    (kill-new rel-name)
    (message "%s" rel-name)))

(defun projectile-find-file-other-window ()
  "Jump to a project's file in another window."
  (interactive)
  (require 'cl)
  (cl-letf (((symbol-function 'find-file) (symbol-function 'find-file-other-window)))
    (projectile-find-file)))

(defun org-indent-item-or-cycle ()
  "If before a list, indent it, otherwise cycle visibility."
  (interactive)
  (eval-when-compile
    (require 'org-list))
  (if (org-at-item-p)
      (org-indent-item-tree)
    (org-cycle)))

(defun org-outdent-item-or-shifttab ()
  "If before a list, outdent it, otherwise globally cycle visibility."
  (interactive)
  (eval-when-compile
    (require 'org-list))
  (if (org-at-item-p)
      (org-outdent-item-tree)
    (org-shifttab)))

(defun screenshot-frame ()
  "Take a screenshot of the Emacs frame."
  (interactive)
  (shell-command-to-string "screencapture -w ~/Desktop/`date +%s`.png"))

(defun new-journal-entry (name)
  "Prompt for name, then start entry with date and name in scratch."
  (interactive "MName of entry: ")
  (find-file "~/Dropbox/scratch")
  (goto-char (point-max))
  (insert (format "* %s %s\n" (format-time-string "%Y-%m-%d") name)))

(defun prompt-and-insert (string)
  (interactive "MInsert: ")
  (insert string))

;; From http://www.emacswiki.org/emacs/InsertFileName
(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canonicalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

(defun notmuch-search-mark-read ()
  "Mark this email as read."
  (interactive)
  (notmuch-search-remove-tag (list "-unread"))
  (notmuch-search-next-thread))

(defun notmuch-mark-deleted ()
  "Mark this email as deleted."
  (interactive)
  (when (y-or-n-p "Are you sure you want to this message?")
    (notmuch-show-add-tag (list "+deleted"))
    (notmuch-show-next-thread)))

(defun notmuch-tree-next-matching-message-and-mark-read ()
  "Move to next matching message and mark it as read."
  (interactive)
  (notmuch-tree-next-matching-message)
  (notmuch-tree-remove-tag (list "-unread")))

(defun american-to-iso (american)
  "Return the ISO-8601 date corresponding to the given AMERICAN date.
For example, (american-to-iso \"9/7/2014\") returns
\"2014-09-07\". If only the day and month are specified, the
current year is used."
  (let* ((elems (split-string american "/"))
         (month (string-to-number (first elems)))
         (day (string-to-number (second elems)))
         (year
          (string-to-number
           (if (third elems)
               (third elems)
             (format-time-string "%Y")))))
    (format "%d-%02d-%02d" year month day)))

(defun american-to-iso-region (beg end)
  "When the region contains an American date, replace it with the ISO-8601 date.
See `american-to-iso'."
  (interactive "r")
  (let* ((american (buffer-substring-no-properties beg end))
         (iso (american-to-iso american)))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert iso))))

(defun american-to-iso-buffer ()
  "Change the first column of a CSV file from American to ISO-8601 dates.
See `american-to-iso'."
  (interactive)
  (while (re-search-forward "^\\([0-9/]+\\)," nil t)
    (american-to-iso-region (match-beginning 1) (match-end 1))
    (redisplay)))

;;; WIP
;; (defun scala-cycle-paren-type ()
;;   (interactive)
;;   (smartparens-mode 1)
;;   (let* ((current-paren-type (plist-get (sp-get-enclosing-sexp) :op))
;;          (new-paren-type
;;           (cond
;;             ((string-equal current-paren-type "(") "{" )
;;             ((string-equal current-paren-type "{") "(" )
;;             (t (error "Unknown paren type: " current-paren-type)))))
;;     (sp-rewrap-sexp new-paren-type)))

;; From http://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(defun point-in-comment-p ()
  "Determine if the point is inside a comment"
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun escape-capturing-groups (re)
  (s-replace "\\(" "\\(?:" re))

(defun helm-run-open-dir-or-find-file ()
  "In helm-find-files, persistently enter the selected directory
or else find the selected file."
  (interactive)
  (let ((candidate (helm-get-selection)))
    (cond
     ((file-directory-p candidate)
      (helm-find-files-persistent-action candidate))
     (t
      (helm-maybe-exit-minibuffer)))))

(defvar scala-project-code-buffer nil)
(defvar scala-project-repl-file-name nil)
(defvar scala-project-repl-buffer nil)
(defvar scala-project-compilation-buffer nil)

(defun scala-project-init (code-buffer)
  (interactive "bCode buffer: ")
  (setq scala-project-code-buffer code-buffer)
  (setq scala-project-repl-file-name
        (expand-file-name "./bin/spark-shell" (projectile-project-root)))
  (let ((default-directory (projectile-project-root)))
    (let ((shell-file-name scala-project-repl-file-name))
      (setenv "SPARK_PREPEND_CLASSES" "true")
      (shell "*scala-repl*"))
    (let ((compilation-buffer-name-function #'(lambda (name-of-mode) "*scala-compilation*")))
      (compile "sbt/sbt ~compile")))
  (setq scala-project-repl-buffer (get-buffer "*scala-repl*"))
  (setq scala-project-compilation-buffer (get-buffer "*scala-compilation*")))

(defun scala-project-rerun ()
  (interactive)
  (let ((shell-file-name scala-project-repl-file-name)
              (code (with-current-buffer scala-project-code-buffer (buffer-string))))
          (pop-to-buffer scala-project-repl-buffer)
          (comint-send-eof)
          (sit-for 3)
          (shell scala-project-repl-buffer)
          (sit-for 5)
          (comint-send-string scala-project-repl-buffer code)))

(defun copy-matches-as-kill (regexp)
  "Copy all matches for REGEXP to the kill ring, one match per line."
  (interactive "sRegexp to match: ")
  (save-match-data
    (save-excursion
      (kill-new "")
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (kill-append (concat (match-string 0) "\n") nil)))))

(defun calc-eval-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert " = " (calc-eval (buffer-substring-no-properties beg end)))))

(defun find-symbol-at-point ()
  (interactive)
  (let ((symb (symbol-at-point)))
    (cond
     ((fboundp symb) (find-function symb))
     ((boundp symb) (find-variable symb))
     ((featurep symb) (find-library (symbol-name symb)))
     (t (user-error "No match for %s" (symbol-name symb))))))

(defun eval-last-sexp-other-buffer ()
  (interactive)
  (let ((sexp (preceding-sexp))
        (lexical-binding-saved lexical-binding))
    (save-selected-window
      (switch-to-other-buffer)
      (eval-last-sexp-print-value
       (eval sexp lexical-binding-saved)))))

;; From http://emacs.stackexchange.com/questions/539/how-do-i-measure-performance-of-elisp-code
(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.3fs)" ,title elapsed))))))

;; From http://emacs.1067599.n5.nabble.com/Portable-dir-path-separator-tp290263p290268.html
(defun split-path (path)
  (split-path-1 path ()))
(defun split-path-1 (path accum)
  (let ((dir  (directory-file-name (file-name-directory path)))
        (name (file-name-nondirectory path)))
    (if (equal dir path)
        accum
      (split-path-1 dir (cons name accum)))))

(provide 'utils)

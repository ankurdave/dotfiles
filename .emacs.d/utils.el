(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun smart-split ()
  "Split the frame into as many 80-column sub-windows as possible."
  (interactive)
  (defun ordered-window-list ()
    "Return a list of windows starting from the top left."
    (window-list (selected-frame) 'no-minibuf (frame-first-window)))
  (defun resize-windows-destructively (windows)
    "Resize each window in WINDOWS to 80 characters wide.

If there's not enough space to do that, delete the appropriate
window until there is space."
    (when windows
      (condition-case nil
          (progn (adjust-window-trailing-edge (first windows) (- 80 (window-width (first windows))) t)
                 (resize-windows-destructively (cdr windows)))
        (error (if (cdr windows)
                   (progn (delete-window (cadr windows))
                          (resize-windows-destructively (cons (car windows) (cddr windows))))
                 (ignore-errors
                   (delete-window (car windows))))))))
  (defun subsplit (w)
    "Split the window W into multiple 80-column windows if possible."
    (when (> (window-width w) (* 2 81))
      (let ((w2 (split-window w 82 t)))
        (save-excursion
          (select-window w2)
          (switch-to-buffer (other-buffer (window-buffer w)))))
      (subsplit w)))
  (resize-windows-destructively (ordered-window-list))
  (walk-windows 'subsplit)
  (balance-windows))

(defun prompt-quit-emacs ()
  "Prompt before quitting Emacs."
  (interactive)
  (if (y-or-n-p (format "Really quit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
(when window-system
  (global-set-key (kbd "C-x C-c") 'prompt-quit-emacs))

(defun run-line-in-scala ()
  "Run the current line in an inferior Scala shell."
  (interactive)
  (save-excursion
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (ensime-inf-switch)
    (yank)
    (newline)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Rename the current buffer and the file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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
(global-set-key (kbd "C-x C-x") 'switch-to-other-buffer)

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

;; From misc-cmds.el: http://www.emacswiki.org/emacs/misc-cmds.el
(defun goto-longest-line (beg end)
  "Go to the first of the longest lines in the region or buffer.
If the region is active, it is checked.
If not, the buffer (or its restriction) is checked.

Returns a list of three elements:

 (LINE LINE-LENGTH OTHER-LINES LINES-CHECKED)

LINE is the first of the longest lines measured.
LINE-LENGTH is the length of LINE.
OTHER-LINES is a list of other lines checked that are as long as LINE.
LINES-CHECKED is the number of lines measured.

Interactively, a message displays this information.

If there is only one line in the active region, then the region is
deactivated after this command, and the message mentions only LINE and
LINE-LENGTH.

If this command is repeated, it checks for the longest line after the
cursor.  That is *not* necessarily the longest line other than the
current line.  That longest line could be before or after the current
line.

To search only from the current line forward, not throughout the
buffer, you can use `C-SPC' to set the mark, then use this
\(repeatedly)."
  (interactive
   (if (or (not mark-active) (null (mark)))
       (list (point-min) (point-max))
     (if (< (point) (mark))
         (list (point) (mark))
       (list (mark) (point)))))
  (when (and (not mark-active) (= beg end))
    (error "The buffer is empty"))
  (when (and mark-active (> (point) (mark))) (exchange-point-and-mark))
  (when (< end beg) (setq end (prog1 beg (setq beg end))))
  (when (eq this-command last-command)
    (forward-line 1) (setq beg (point)))
  (goto-char beg)
  (when (eobp) (error "End of buffer"))
  (cond ((<= end (save-excursion (goto-char beg) (forward-line 1) (point)))
         (let ((inhibit-field-text-motion  t))  (beginning-of-line))
         (when (and (> emacs-major-version 21) (require 'hl-line nil t))
           (let ((hl-line-mode  t))  (hl-line-highlight))
           (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
         (let ((lineno  (line-number-at-pos))
               (chars   (let ((inhibit-field-text-motion t))
                          (save-excursion (end-of-line) (current-column)))))
           (message "Only line %d: %d chars" lineno chars)
           (let ((visible-bell  t))  (ding))
           (setq mark-active  nil)
           (list lineno chars nil 1)))
        (t
         (let* ((start-line                 (line-number-at-pos))
                (max-width                  0)
                (line                       start-line)
                (inhibit-field-text-motion  t)
                long-lines col)
           (when (eobp) (error "End of buffer"))
           (while (and (not (eobp)) (< (point) end))
             (end-of-line)
             (setq col  (current-column))
             (when (>= col max-width)
               (setq long-lines  (if (= col max-width)
                                     (cons line long-lines)
                                   (list line))
                     max-width   col))
             (forward-line 1)
             (setq line  (1+ line)))
           (setq long-lines  (nreverse long-lines))
           (let ((lines  long-lines))
             (while (and lines (> start-line (car lines))) (pop lines))
             (goto-char (point-min))
             (when (car lines) (forward-line (1- (car lines)))))
           (when (and (> emacs-major-version 21) (require 'hl-line nil t))
             (let ((hl-line-mode  t))  (hl-line-highlight))
             (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
           (when (interactive-p)
             (let ((others  (cdr long-lines)))
               (message "Line %d: %d chars%s (%d lines measured)"
                        (car long-lines) max-width
                        (concat
                         (and others
                              (format ", Others: {%s}" (mapconcat
                                                        (lambda (line) (format "%d" line))
                                                        (cdr long-lines) ", "))))
                        (- line start-line))))
           (list (car long-lines) max-width (cdr long-lines) (- line start-line))))))

(defun fit-frame-width-to-buffer ()
  "Adjust width of current frame to fit longest line in current buffer."
  (interactive)
  (let ((longest-line-length
         (save-excursion
           (cadr (goto-longest-line (point-min) (point-max))))))
    (set-frame-width (selected-frame)
                     (max fill-column longest-line-length))))

;; TODO: on TAB, auto complete if tab would not change indentation

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

(defun rgrep-in-project ()
  "Perform rgrep in the project on files with the same extension
as the current one."
  (interactive)
  (let ((roots (projectile-get-project-directories))
        (search-regexp (if (and transient-mark-mode mark-active)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string (projectile-prepend-project-name "Grep for: ")
                                      (projectile-symbol-at-point)))))
    (dolist (root-dir roots)
      (require 'grep)
      ;; paths for find-grep should relative and without trailing /
      (let ((grep-find-ignored-directories (-union (-map (lambda (dir) (s-chop-suffix "/" (s-chop-prefix root-dir dir)))
                                                         (cdr (projectile-ignored-directories))) grep-find-ignored-directories))
            (grep-find-ignored-files (-union (-map (lambda (file) (s-chop-prefix root-dir file)) (projectile-ignored-files)) grep-find-ignored-files)))
        (grep-compute-defaults)
        (rgrep search-regexp (format "*.%s" (file-name-extension buffer-file-name)) root-dir)))))

(defun copy-filename-as-kill ()
  "Copy full path to current file into the kill ring."
  (interactive)
  (kill-new buffer-file-name)
  (message "%s" buffer-file-name))

(defun copy-relative-filename-as-kill ()
  "Copy project-relative path of current file into the kill ring."
  (interactive)
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
  (if (org-at-item-p)
      (org-indent-item-tree)
    (org-cycle)))

(defun org-outdent-item-or-shifttab ()
  "If before a list, outdent it, otherwise globally cycle visibility."
  (interactive)
  (if (org-at-item-p)
      (org-outdent-item-tree)
    (org-shifttab)))

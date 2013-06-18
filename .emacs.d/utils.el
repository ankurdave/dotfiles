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
      (when (not (equalp window (selected-window)))
        (setq time (window-use-time window))
        (when (or (not best-time) (> time best-time))
          (setq best-time time)
          (setq best-window window))))
    best-window))
  (cond
   ((equalp (count-windows) 1) (switch-to-buffer (other-buffer)))
   ((equalp (count-windows) 2) (other-window 1))
   (t (get-mru-window) (select-window (get-2nd-mru-window)))))
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

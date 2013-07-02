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

(defun kill-region-or-backward-word ()
  "Kill the region if active, otherwise kill the previous word."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(defun sp-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (sp-backward-kill-word)))

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

(defun beginning-of-hn-comment ()
  "Move to the beginning of the Hacker News comment at point in w3m."
  (interactive)
  (search-backward "| link")
  (beginning-of-line 2)
  (point))

(defun end-of-hn-comment ()
  "Move to the end of the Hacker News comment at point in w3m."
  (interactive)
  (search-forward "| link")
  (beginning-of-line)
  (point))

(defun dim-all-but-hn-comment-at-point ()
  (interactive)
  (let ((dim '(font-lock-face (:foreground "DimGrey")))
        (comment-start (save-excursion (beginning-of-hn-comment)))
        (comment-end (save-excursion (end-of-hn-comment))))
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) dim)
      (add-text-properties (point-min) comment-start dim)
      (add-text-properties comment-end (point-max) dim))))

(defun narrow-to-hn-comment-at-point ()
  "Narrow to the Hacker News comment at point in w3m."
  (interactive)
  (widen)
  (narrow-to-region (save-excursion (beginning-of-hn-comment))
                    (save-excursion (end-of-hn-comment))))

(defcustom hn-comment-narrow-function nil
  "Whether or not the HN comment functions should narrow to the
  current comment."
  :type '(choice (function-item narrow-to-hn-comment-at-point)
                 (function-item dim-all-but-hn-comment-at-point)
                 (const nil)))

(defun next-hn-comment ()
  "Move to the next Hacker News comment in w3m."
  (interactive)
  (widen)
  (search-forward-regexp "^\\s-*reply\\s-*$")
  (beginning-of-line)
  (when hn-comment-narrow-function
    (funcall hn-comment-narrow-function)))

(eval-after-load "w3m"
  '(define-key w3m-mode-map (kbd "n") 'next-hn-comment))

(defun previous-hn-comment ()
  "See `next-hn-comment'."
  (interactive)
  (widen)
  (search-backward-regexp "| link\\s-*$")
  (beginning-of-line)
  (when hn-comment-narrow-function
    (funcall hn-comment-narrow-function)))

(eval-after-load "w3m"
  '(define-key w3m-mode-map (kbd "p") 'previous-hn-comment))

(defun parent-hn-comment ()
  "Narrow to the parent Hacker News comment in w3m."
  (interactive)
  (widen)
  ;; Go to first line of this comment
  (search-backward "| link")
  (move-beginning-of-line 3)
  (search-forward "[s]" (line-end-position) t) ; skip [s] images
  (while (looking-at " ")
    (forward-char))
  ;; Move backward until we reach the body text of the parent comment (i.e.,
  ;; while we are either in indentation or in a comment header)
  (while (or (equal (preceding-char) ?\s)
             (save-excursion
               (search-forward "| link " (line-end-position) t)))
    (previous-line))
  (when hn-comment-narrow-function
    (funcall hn-comment-narrow-function)))

(eval-after-load "w3m"
  '(define-key w3m-mode-map (kbd "P") 'parent-hn-comment))

(defun next-sibling-hn-comment ()
  "Narrow to the next sibling Hacker News comment in w3m."
  (interactive)
  (widen)
  ;; Go to last line of this comment
  (search-forward "| link")
  (move-beginning-of-line -1)
  (search-forward "[s]" (line-end-position) t) ; skip [s] images
  (while (looking-at " ")
    (forward-char))
  (next-line)
  ;; Move forward until we reach the body text of the next sibling comment
  ;; (i.e., while we are either in indentation or in a comment header)
  (while (or (equal (following-char) ?\s)
             (save-excursion
               (search-forward "| link " (line-end-position) t)))
    (next-line)
    (sit-for 0)  ; Avoid strange behavior where we skip past the sibling comment
    )
  (when hn-comment-narrow-function
    (funcall hn-comment-narrow-function)))

;; (eval-after-load "w3m"
;;   '(define-key w3m-mode-map (kbd "N") 'next-sibling-hn-comment))

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

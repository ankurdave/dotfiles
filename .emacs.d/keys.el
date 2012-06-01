(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

;; Use smex for completing M-x
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Make C-- undo, in addition to C-/ and C-_
(global-set-key (kbd "C--") 'undo-tree-undo)
(global-set-key (kbd "C-?") 'undo-tree-redo)

;; Swap C-x C-b and C-x b
(let ((switch-buffers (key-binding (kbd "C-x b")))
      (list-buffers (key-binding (kbd "C-x C-b"))))
  (global-set-key (kbd "C-x C-b") switch-buffers)
  (global-set-key (kbd "C-x b") list-buffers))

(defun get-2nd-mru-window ()
  (let (best-window best-time time)
    (dolist (window (window-list-1 nil 'nomini nil))
      (when (not (equalp window (selected-window)))
        (setq time (window-use-time window))
        (when (or (not best-time) (> time best-time))
          (setq best-time time)
          (setq best-window window))))
    best-window))

(defun switch-to-other-buffer ()
  "Switches to the most recently used buffer."
  (interactive)
  (cond
   ((equalp (count-windows) 1) (switch-to-buffer (other-buffer)))
   ((equalp (count-windows) 2) (other-window 1))
   (t (get-mru-window) (select-window (get-2nd-mru-window)))))
(global-set-key (kbd "C-x C-x") 'switch-to-other-buffer)

(when window-system
  (global-set-key (kbd "C-x C-c") 'prompt-quit-emacs))

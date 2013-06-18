(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

(autoload 'zap-up-to-char "misc")
(global-set-key "\M-z" 'zap-up-to-char)

(defun kill-region-or-backward-word ()
  "Kill the region if active, otherwise kill the previous word."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key "\C-w" 'kill-region-or-backward-word)

(defun paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))
(define-key paredit-mode-map (kbd "C-w") 'paredit-kill-region-or-backward-word)

(global-set-key (kbd "M-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-s") 'isearch-forward-regexp)

;; Swap C-x C-b and C-x b
;; (let ((switch-buffers (key-binding (kbd "C-x b")))
;;       (list-buffers (key-binding (kbd "C-x C-b"))))
;;   (global-set-key (kbd "C-x C-b") switch-buffers)
;;   (global-set-key (kbd "C-x b") list-buffers))

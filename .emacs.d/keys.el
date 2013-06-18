(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

(autoload 'zap-up-to-char "misc")
(global-set-key "\M-z" 'zap-up-to-char)

;;; C-w for backward-kill-word
(defun rebind-backward-kill-word (&optional locally)
  "Rebind C-w to backward-kill-word, and C-c C-k to
kill-region. If LOCALLY is non-nil, do the rebinding in the
buffer's local key map."
  (interactive)
  (let ((set-key-function (if locally 'local-set-key 'global-set-key)))
    (global-unset-key (kbd "M-DEL"))
    (global-unset-key (kbd "<C-backspace>"))
    (funcall set-key-function "\C-w" 'backward-kill-word)
    (funcall set-key-function "\C-c\C-k" 'kill-region)))
(rebind-backward-kill-word)
(add-hook 'eshell-mode-hook (lambda () (rebind-backward-kill-word t)))
;;; C-w for kill-region
(defun undo-rebind-backward-kill-word ()
  "Undo the effects of rebind-backward-kill-word."
  (interactive)
  (global-set-key (kbd "M-DEL") 'kill-region)
  (global-set-key (kbd "C-<backspace>") 'backward-kill-word)
  (global-set-key "\C-w" 'kill-region))

(global-set-key (kbd "M-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-s") 'isearch-forward-regexp)

;; Swap C-x C-b and C-x b
;; (let ((switch-buffers (key-binding (kbd "C-x b")))
;;       (list-buffers (key-binding (kbd "C-x C-b"))))
;;   (global-set-key (kbd "C-x C-b") switch-buffers)
;;   (global-set-key (kbd "C-x b") list-buffers))

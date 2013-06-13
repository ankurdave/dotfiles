(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

(autoload 'zap-up-to-char "misc")
(global-set-key "\M-z" 'zap-up-to-char)

;; Swap C-x C-b and C-x b
;; (let ((switch-buffers (key-binding (kbd "C-x b")))
;;       (list-buffers (key-binding (kbd "C-x C-b"))))
;;   (global-set-key (kbd "C-x C-b") switch-buffers)
;;   (global-set-key (kbd "C-x b") list-buffers))

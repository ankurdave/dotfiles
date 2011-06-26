(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

;; Use smex for completing M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Make C-- undo, in addition to C-/ and C-_
(global-set-key (kbd "C--") 'undo-tree-undo)

(when window-system
  (global-set-key (kbd "C-x C-c") 'prompt-quit-emacs))

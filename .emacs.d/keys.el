(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)
(global-set-key (kbd "C-x C-x") 'switch-to-other-buffer)

(when window-system
  (global-set-key (kbd "C-x C-c") 'prompt-quit-emacs))

;; Use smex for completing M-x
(eval-after-load 'smex
  '(progn
     (global-set-key (kbd "M-x") 'smex)
     (global-set-key (kbd "M-X") 'smex-major-mode-commands)
     (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

;; Make C-- undo, in addition to C-/ and C-_
(eval-after-load 'undo-tree
  '(progn
     (global-set-key (kbd "C--") 'undo-tree-undo)
     (global-set-key (kbd "C-?") 'undo-tree-redo)))

;; Swap C-x C-b and C-x b
;; (let ((switch-buffers (key-binding (kbd "C-x b")))
;;       (list-buffers (key-binding (kbd "C-x C-b"))))
;;   (global-set-key (kbd "C-x C-b") switch-buffers)
;;   (global-set-key (kbd "C-x b") list-buffers))

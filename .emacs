(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(word-wrap t))

(transient-mark-mode t)
(setq-default scroll-conservatively 5)

;; Indentation
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq sgml-basic-offset 4)

(setq browse-url-generic-program (executable-find "google-chrome")
      browse-url-browser-function 'browse-url-generic)

(require 'cc-mode)
(setq-default c-default-style "k&r")
(setq-default c-basic-offset 4)
(c-set-offset 'defun-close '-)
(c-set-offset 'arglist-close 0)

;; (require 'css-mode)
(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

(add-to-list 'load-path "~/.emacs.d")
(require 'mercurial)
;; (require 'php-mode)
;; (require 'undo-tree)
(load-file "~/.emacs.d/cs61a.el")
(add-to-list 'load-path "~/.emacs.d/scala")
(require 'scala-mode-auto)

;; Utility functions
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(server-start)
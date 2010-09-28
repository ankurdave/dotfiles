(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(backward-delete-char-untabify-method nil)
 '(c-basic-offset 4)
 '(inhibit-startup-screen t)
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 4)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(sgml-basic-offset 4)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(word-wrap t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 87 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(transient-mark-mode t)
(setq-default scroll-conservatively 5)

;; Indentation
(setq-default tab-width 4
	  backward-delete-function nil
	  indent-tabs-mode t)
(setq indent-line-function 'insert-tab)

(setq browse-url-generic-program (executable-find "google-chrome")
	browse-url-browser-function 'browse-url-generic)

(require 'cc-mode)
(setq-default c-default-style "k&r")
(setq-default c-basic-offset 4)
(c-set-offset 'defun-close '-)
(c-set-offset 'arglist-close 0)

(require 'css-mode)
(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

(add-to-list 'load-path "~/.emacs.d")
(require 'mercurial)
(require 'php-mode)
(require 'undo-tree)
(load-file "~/.emacs.d/cs61a.el")
(add-to-list 'load-path "~/.emacs.d/scala")
(require 'scala-mode-auto

;; Utility functions
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))


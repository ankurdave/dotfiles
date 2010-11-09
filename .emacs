(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
(setq scroll-conservatively 5)

(setq word-wrap t)

(transient-mark-mode t)
(show-paren-mode t)

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

;; Site-local config
(when (file-exists-p "~/.emacs.d/site-local.el")
  (load-file "~/.emacs.d/site-local.el"))

(server-start)
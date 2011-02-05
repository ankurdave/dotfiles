(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style "k&r")
 '(delete-selection-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 5)
 '(show-paren-mode t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(word-wrap t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "#65b042"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "#cf6a4c")))))

;; Utility functions
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/scala")
(require 'mercurial)
(require 'scala-mode-auto)
(require 'color-theme)
(require 'color-theme-sunburst)

(color-theme-tm)
(setq term-default-bg-color "#000")
(setq term-default-fg-color "#fff")

;; Site-local config
(when (file-exists-p "~/.emacs-site-local")
  (load-file "~/.emacs-site-local"))

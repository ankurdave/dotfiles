(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(ansi-color-for-comint-mode t)
 '(c-basic-offset 4)
 '(c-default-style "k&r")
 '(delete-selection-mode t)
 '(dtrt-indent-mode t nil (dtrt-indent))
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
 '(diff-removed ((t (:inherit diff-changed :foreground "#cf6a4c"))))
 '(variable-pitch ((t (:family "Georgia")))))

(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

(defun variable-pitch ()
  "Go into text editing style."
  (interactive)
  (variable-pitch-mode t))

(defun larger ()
  "Increase the text size by one step over default."
  (interactive)
  (text-scale-increase 1))

(add-hook 'LaTeX-mode-hook 'variable-pitch)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'larger)
(add-hook 'BibTeX-mode-hook 'variable-pitch)

;; Utility functions
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun cs61a ()
  "Loads cs61a utilities."
  (interactive)
  (require 'cs61a))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/scala")
(add-to-list 'load-path "~/.emacs.d/undo-tree")
(add-to-list 'load-path "~/.emacs.d/dtrt-indent")
(require 'mercurial)
(require 'scala-mode-auto)
(require 'undo-tree)
(require 'js2-mode)
(load "~/.emacs.d/nxhtml/autostart.el")
(require 'dtrt-indent)

;; Site-local config
(when (file-exists-p "~/.emacs-site-local")
  (load-file "~/.emacs-site-local"))

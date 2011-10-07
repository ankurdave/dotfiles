;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(defun save-and-compile ()
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file -1))

(add-hook 'LaTeX-mode-hook (lambda () (local-set-key (kbd "C-x C-s") 'save-and-compile)))


;; Use Python mode for TARGETS files
(setq auto-mode-alist (cons '("\\/TARGETS$" . python-mode) auto-mode-alist))

;; Use C++ mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Use 80-char line wrapping
(defun enable-80-wrap ()
  (auto-fill-mode 1)
  (highlight-80+-mode))
(add-hook 'c-mode-common-hook 'enable-80-wrap)
(add-hook 'python-mode 'enable-80-wrap)
(add-hook 'scala-mode 'enable-80-wrap)

;; Jump to words within CamelCase identifiers
(add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))

;; sudo+ssh for TRAMP
(set-default 'tramp-default-proxies-alist '((".*" "\\`root\\'" "/ssh:%h:")
                                            ("dave-server" "www-data" "/ssh:%h:")))

;; Interpret ANSI color codes in compiles
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

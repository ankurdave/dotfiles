(add-hook 'LaTeX-mode-hook 'variable-pitch)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'larger)
(add-hook 'BibTeX-mode-hook 'variable-pitch)

;; Use Python mode for TARGETS files
(setq auto-mode-alist (cons '("\\/TARGETS$" . python-mode) auto-mode-alist))

;; Use C++ mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Jump to words within CamelCase identifiers
(add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))

;; sudo+ssh for TRAMP
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

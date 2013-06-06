(defun package-install-all ()
  (interactive)
  (package-install 'smex)
  (package-install 'markdown-mode)
  (package-install 'switch-window)
  (package-install 'scala-mode)
  (package-install 'undo-tree)
  (package-install 'maxframe)
  (package-install 'dtrt-indent)
  (package-install 'auctex)
  (package-install 'csv-mode)
  (package-install 'molokai-theme))

(when (fboundp 'package-initialize)
  (package-initialize))

(require 'undo-tree nil t)
(load "auctex" t)
(load "preview-latex" t)
(load "scala-mode-auto" t)
(load "smex" t)
(load "auto-complete" t)
(load "auto-complete-config" t)
(require 'eshell nil t)
(require 'em-smart nil t)
(eval-after-load 'em-smart
  '(eshell-smart-initialize))
(eval-after-load 'dtrt-indent
  '(dtrt-indent-mode t))
(eval-after-load 'auto-complete
  '(global-auto-complete-mode))
(eval-after-load 'auto-complete-config
  '(ac-config-default))

(add-to-list 'load-path "~/.emacs.d")
(autoload 'typing-test "typing-test" nil t)

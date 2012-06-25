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

(package-initialize)

(require 'undo-tree)
(load "auctex.el")
(load "preview-latex.el")
(load "scala-mode-auto.el")

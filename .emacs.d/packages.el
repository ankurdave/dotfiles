(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

(defun package-install-all ()
  (interactive)
  (package-install 'smex)
  (package-install 'markdown-mode)
  (package-install 'switch-window)
  (package-install 'scala-mode)
  (package-install 'undo-tree)
  (package-install 'maxframe)
  (package-install 'dtrt-indent)
  (package-install 'auctex))

(require 'undo-tree)
(load "auctex.el")
(load "preview-latex.el")


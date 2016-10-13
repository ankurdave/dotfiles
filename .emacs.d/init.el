
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(load "~/.emacs.d/site-local.el" t)
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/misc.el")

;; Load customizations last so that setting them through Customize is
;; guaranteed to work.
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file)

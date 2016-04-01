(load "~/.emacs.d/site-local.el" t)
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/misc.el")

;; Load customizations last so that setting them through Customize is
;; guaranteed to work.
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file)

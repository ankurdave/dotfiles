;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(load "~/.emacs.d/site-local.el" t)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/utils.el")
(load "~/.emacs.d/keys.el")
(load "~/.emacs.d/hooks.el")

;; Load customizations last so that setting them through Customize is
;; guaranteed to work.
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file)

(require 'server)
(if (server-running-p)
    (message "Server already running")
  (server-start))

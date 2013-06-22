;; Load site-specific customization first to allow setting up `load-path' and
;; similar.
(load "~/.emacs.d/site-local.el" t)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/utils.el")
(load "~/.emacs.d/keys.el")
(load "~/.emacs.d/hooks.el")

;; Load customizations last so that setting them through Customize is
;; guaranteed to work.
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file)

;; Finally, accept connections from emacsclient.
(require 'server)
(if (server-running-p)
    (message "Server already running")
  (message "Starting server")
  (server-start))

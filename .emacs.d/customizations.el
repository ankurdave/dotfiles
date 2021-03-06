(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-master (quote dwim))
 '(TeX-save-query nil)
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("open" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "open")
     (output-html "xdg-open"))))
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(c-default-style
   (quote
    ((c++-mode . "google")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(compilation-scroll-output (quote first-error))
 '(cursor-type (quote (bar . 2)))
 '(custom-safe-themes
   (quote
    ("f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef" default)))
 '(default-input-method "TeX")
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(electric-indent-mode t)
 '(enable-recursive-minibuffers t)
 '(ffap-machine-p-known (quote reject))
 '(fill-column 80)
 '(find-function-recenter-line nil)
 '(fringe-mode 0 nil (fringe))
 '(help-window-select t)
 '(history-length 500)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(message-citation-line-format "At %Y-%m-%d %T %z, %f wrote:")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(message-cite-reply-position (quote traditional))
 '(message-cite-style nil)
 '(message-confirm-send t)
 '(message-fill-column nil)
 '(minibuffer-depth-indicate-mode t)
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote meta))
 '(pop-up-windows t)
 '(recentf-max-saved-items 200)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote before-save-hook)
           (quote sort-package-configurations)
           nil t)
     (eval add-to-list
           (quote before-save-hook)
           (quote sort-package-configurations)))))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(show-paren-delay 0.0)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type (quote ssl))
 '(split-height-threshold 100)
 '(split-width-threshold 200)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "Ankur Dave")
 '(user-mail-address "ankurdave@gmail.com")
 '(vc-follow-symlinks t)
 '(vc-handled-backends nil)
 '(visible-bell nil)
 '(visual-line-mode nil t)
 '(word-wrap t)
 '(xterm-mouse-mode t))

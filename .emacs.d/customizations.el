(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-master (quote dwim))
 '(TeX-save-query nil)
 '(TeX-view-program-list (quote (("open" "open -a Skim %o"))))
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
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 211 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(auto-compile-on-load-mode t)
 '(auto-compile-on-save-mode t)
 '(auto-revert-verbose nil)
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style "k&r")
 '(column-number-mode t)
 '(company-idle-delay nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(cs164-basic-offset 2)
 '(cs164-indent-automatically t)
 '(cursor-type (quote (bar . 2)))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes t)
 '(debug-on-error nil)
 '(default-input-method "TeX")
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-details-hidden-string "")
 '(doc-view-continuous t)
 '(edit-server-new-frame nil)
 '(electric-indent-mode t)
 '(enable-recursive-minibuffers t)
 '(eshell-buffer-shorthand nil)
 '(eshell-cmpl-cycle-completions nil)
 '(eshell-review-quick-commands (quote not-even-short-output))
 '(fci-rule-color "#383838")
 '(ffap-machine-p-known (quote reject))
 '(fill-column 80)
 '(find-function-recenter-line nil)
 '(flycheck-emacs-lisp-initialize-packages t)
 '(fringe-mode 0 nil (fringe))
 '(global-git-commit-mode t)
 '(helm-M-x-fuzzy-match t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-file-cache-fuzzy-match t)
 '(helm-flx-mode t)
 '(helm-projectile-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(helm-split-window-default-side (quote below))
 '(helm-split-window-in-side-p nil)
 '(highlight-symbol-idle-delay 1.5)
 '(highlight-symbol-on-navigation-p t)
 '(history-length 10000)
 '(htmlize-output-type (quote inline-css))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-file-extensions-order (quote (".tex" ".pdf")))
 '(ido-max-file-prompt-width 0.6)
 '(ido-ubiquitous-command-exceptions (quote (w3m-goto-url w3m-goto-url-new-session)))
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(ido-use-virtual-buffers t)
 '(ido-vertical-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(large-file-warning-threshold 100000000)
 '(linum-format " %7i ")
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(message-citation-line-format "At %Y-%m-%d %T %z, %f wrote:")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(message-cite-reply-position (quote traditional))
 '(message-cite-style nil)
 '(message-confirm-send t)
 '(message-fill-column nil)
 '(minibuffer-depth-indicate-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 1))))
 '(mumamo-chunk-coloring 2)
 '(notmuch-fcc-dirs nil)
 '(notmuch-hello-thousands-separator ",")
 '(notmuch-saved-searches
   (quote
    ((:name "must-read" :query "(tag:inbox OR tag:is-reply OR tag:graphx) AND is:unread")
     (:name "should-read" :query "(tag:notifications) AND is:unread")
     (:name "may-read" :query "(tag:spark-dev OR tag:amplab-lists OR tag:berkeley-lists) AND is:unread"))))
 '(notmuch-search-line-faces
   (quote
    (("unread" :weight bold)
     ("flagged" :foreground "#8CD0D3"))))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-all-tags-list t)
 '(notmuch-show-indent-messages-width 1)
 '(notmuch-show-indent-multipart nil)
 '(notmuch-show-part-button-default-action (quote notmuch-show-interactively-view-part))
 '(notmuch-tag-formats
   (quote
    (("unread"
      (propertize tag
                  (quote face)
                  (quote
                   (:foreground "#CC9393"))))
     ("flagged"
      (notmuch-tag-format-image-data tag
                                     (notmuch-tag-star-icon))))))
 '(notmuch-unread-search-term
   "(tag:inbox OR tag:is-reply OR tag:graphx OR tag:notifications) AND is:unread")
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote meta))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(org-agenda-files (quote ("~/Dropbox/scratch")))
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(pop-up-windows t)
 '(projectile-global-mode t)
 '(projectile-use-git-grep t)
 '(py-indent-offset 2)
 '(python-indent-offset 2)
 '(recentf-max-saved-items 200)
 '(require-final-newline t)
 '(savehist-mode t)
 '(sbt:program-name "~/repos/spark/sbt/sbt")
 '(scala-indent:align-parameters nil)
 '(scala-indent:use-javadoc-style t)
 '(scroll-bar-mode nil)
 '(semantic-default-submodes
   (quote
    (global-semantic-highlight-func-mode global-semantic-decoration-mode global-semantic-stickyfunc-mode global-semantic-idle-completions-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-mru-bookmark-mode)))
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(shift-select-mode nil)
 '(show-paren-delay 0.0)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type (quote ssl))
 '(sp-base-key-bindings (quote sp))
 '(sp-highlight-pair-overlay nil)
 '(sp-navigate-consider-symbols t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo-history/"))))
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "Ankur Dave")
 '(user-mail-address "ankurdave@gmail.com")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks t)
 '(vc-handled-backends nil)
 '(visible-bell nil)
 '(visual-line-mode nil t)
 '(w3m-confirm-leaving-secure-page nil)
 '(w3m-use-cookies t)
 '(w3m-use-favicon nil)
 '(w3m-use-title-buffer-name t)
 '(whitespace-global-modes (quote (scala-mode)))
 '(whitespace-line-column nil)
 '(winner-mode t)
 '(winsize-desired-width 80)
 '(word-wrap t)
 '(ws-butler-global-mode t)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-symbol-face ((t (:background "black" :foreground "yellow"))))
 '(magit-diff-add ((t (:inherit diff-added)))))

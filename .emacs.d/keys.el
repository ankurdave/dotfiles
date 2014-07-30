(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

(global-set-key (kbd "M-`") 'other-frame)

(autoload 'zap-up-to-char "misc")
(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key "\C-w" (make-backward-kill-word-fn backward-kill-word (1)))

(global-set-key (kbd "C-x 4 p") 'projectile-find-file-other-window)

(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x m") 'magit-status))

(when (fboundp 'er/expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

(when (fboundp 'ace-jump-mode)
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode))

(when (boundp 'popwin:keymap)
  (define-key (current-global-map) (kbd "C-c w") popwin:keymap))

(when (fboundp 'projectile-find-file)
  (global-set-key (kbd "M-p") 'projectile-find-file))

(when (fboundp 'browse-kill-ring)
  (global-set-key (kbd "C-M-y") 'browse-kill-ring))

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "<tab>") 'org-indent-item-or-cycle)
     (define-key org-mode-map (kbd "<S-tab>") 'org-outdent-item-or-shifttab)))

(eval-after-load 'subword-mode
  '(define-key subword-mode-map (kbd "C-w")
    (make-backward-kill-word-fn subword-backward-kill (1))))

(eval-after-load 'ido
  '(progn
     (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
     (define-key ido-common-completion-map (kbd "C-a") 'move-beginning-of-line)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-w")
       (make-backward-kill-word-fn paredit-backward-kill-word))

     ;; Define some keys everywhere
     (global-set-key (kbd "M-(") 'paredit-wrap-round)))

(eval-after-load 'smartparens
  '(progn
     ;; TODO: bind paredit-wrap-{round,square,curly} to a sequence containing
     ;; that char

     (global-set-key (kbd "C-M-a") 'sp-beginning-of-sexp)
     (global-set-key (kbd "C-M-e") 'sp-end-of-sexp)

     (define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
     (define-key sp-keymap (kbd "C-M-e") 'sp-end-of-sexp)

     ;; To free C-M-d in OS X, see
     ;; http://superuser.com/questions/326223/how-do-i-disable-the-command-control-d-word-definition-keyboard-shortcut-in-os-x
     (define-key sp-keymap (kbd "C-M-d") 'sp-kill-sexp)

     ;; Emulate paredit in smartparens-mode
     (define-key sp-keymap (kbd "M-J") 'sp-join-sexp)
     (define-key sp-keymap (kbd "<M-up>") 'sp-splice-sexp-killing-backward)))

(eval-after-load "w3m"
  '(progn
     (require 'w3m-hacker-news)
     (define-key w3m-mode-map (kbd "n") 'next-hn-comment)
     (define-key w3m-mode-map (kbd "p") 'previous-hn-comment)
     (define-key w3m-mode-map (kbd "P") 'parent-hn-comment)
     (define-key w3m-mode-map (kbd "C-c n") 'next-sibling-hn-comment)
     (define-key w3m-mode-map (kbd "C-x n c") 'narrow-to-hn-comment-at-point)

     (require 'w3m-isearch-links)
     (define-key w3m-mode-map (kbd "/") 'w3m-isearch-links)))

(eval-after-load "god-mode"
  '(progn
     (global-set-key (kbd "<escape>") 'god-mode-all)
     (define-key god-local-mode-map (kbd "/") 'undo-tree-undo)
     (define-key god-local-mode-map (kbd ".") 'repeat)
     (define-key god-local-mode-map (kbd "i") 'prompt-and-insert)))

(eval-after-load "notmuch"
  '(progn
     (define-key notmuch-search-mode-map (kbd "I") 'notmuch-search-mark-read)
     (define-key notmuch-search-mode-map (kbd "g") 'notmuch-refresh-this-buffer)))

(eval-after-load "notmuch-show"
  '(progn
     (define-key notmuch-show-mode-map (kbd "d") 'notmuch-mark-deleted)
     (define-key notmuch-show-mode-map (kbd "o") 'goto-address-at-point)))

(eval-after-load "notmuch-hello"
  '(define-key notmuch-hello-mode-map (kbd "g") 'notmuch-refresh-this-buffer))

(eval-after-load "notmuch-tree"
  '(progn
     (define-key notmuch-tree-mode-map (kbd "n") 'notmuch-tree-next-matching-message-and-mark-read)))

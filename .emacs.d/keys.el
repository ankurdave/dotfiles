(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

(autoload 'zap-up-to-char "misc")
(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key "\C-w" 'kill-region-or-backward-word)

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

(eval-after-load 'ido
  '(progn
     (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
     (define-key ido-common-completion-map (kbd "C-a") 'move-beginning-of-line)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-w")
       'paredit-kill-region-or-backward-word)

     ;; Define some keys everywhere
     (global-set-key (kbd "M-(") 'paredit-wrap-round)))

(eval-after-load 'smartparens
  '(progn
     ;; TODO: bind paredit-wrap-{round,square,curly} to a sequence containing
     ;; that char

     (global-set-key (kbd "C-M-a") 'sp-beginning-of-sexp)
     (global-set-key (kbd "C-M-e") 'sp-end-of-sexp)

     ;; Emulate paredit in smartparens-mode
     (define-key sp-keymap (kbd "M-J") 'sp-join-sexp)))

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

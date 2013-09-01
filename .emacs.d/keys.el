(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

(autoload 'zap-up-to-char "misc")
(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key "\C-w" 'kill-region-or-backward-word)

(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x m") 'magit-status))

(when (fboundp 'er/expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-w")
       'paredit-kill-region-or-backward-word)

     ;; Define some keys everywhere
     (define-key global-map (kbd "M-(") 'paredit-wrap-round)))

(eval-after-load 'smartparens
  '(progn
     ;; TODO: check out `sp-base-key-bindings'
     (define-key sp-keymap (kbd "C-w")
       'sp-kill-region-or-backward-word)

     ;; TODO: bind paredit-wrap-{round,square,curly} to a sequence containing
     ;; that char

     (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
     (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

     (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
     (define-key sp-keymap (kbd "C-M-u") 'sp-up-sexp)
     (define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
     (define-key sp-keymap (kbd "C-M-e") 'sp-end-of-sexp)

     ;; Also define these keys for paredit
     (eval-after-load 'paredit
       '(progn
          (define-key paredit-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
          (define-key paredit-mode-map (kbd "C-M-e") 'sp-end-of-sexp)))

     (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

     (define-key sp-keymap (kbd "C-k") 'sp-kill-sexp)

     (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
     (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
     (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
     (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

     (define-key sp-keymap (kbd "M-s") 'sp-splice-sexp)
     (define-key sp-keymap (kbd "M-<up>") 'sp-splice-sexp-killing-backward)
     (define-key sp-keymap (kbd "M-<down>") 'sp-splice-sexp-killing-forward)
     (define-key sp-keymap (kbd "M-r") 'sp-splice-sexp-killing-around)

     (define-key sp-keymap (kbd "M-J") 'sp-join-sexp)
     (define-key sp-keymap (kbd "M-S") 'sp-split-sexp)))

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

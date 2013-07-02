(global-unset-key "\C-z")
(global-set-key "\M-/" 'hippie-expand)

(autoload 'zap-up-to-char "misc")
(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key "\C-w" 'kill-region-or-backward-word)

(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x m") 'magit-status))

(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd "C-w")
     'paredit-kill-region-or-backward-word))

(eval-after-load 'smartparens
  '(progn
     (define-key sp-keymap (kbd "C-w")
       'sp-kill-region-or-backward-word)

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
     ;; (define-key w3m-mode-map (kbd "N") 'next-sibling-hn-comment)
     ))

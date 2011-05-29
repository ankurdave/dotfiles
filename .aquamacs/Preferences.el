;; This is the Aquamacs Preferences file.
;; Add Emacs-Lisp code here that should be executed whenever
;; you start Aquamacs Emacs. If errors occur, Aquamacs will stop
;; evaluating this file and print errors in the *Messags* buffer.
;; Use this file in place of ~/.emacs (which is loaded as well.)

(global-set-key (kbd "M-RET") 'aquamacs-toggle-full-frame)
(global-set-key (kbd "<C-tab>") 'next-tab-or-buffer)
(global-set-key (kbd "<C-S-tab>") 'previous-tab-or-buffer)
(global-set-key (kbd "C--") 'undo-tree-undo)

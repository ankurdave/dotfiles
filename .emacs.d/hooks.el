;; Org mode
(add-hook 'org-mode-hook 'org-indent-mode)

;; LaTeX mode
(defun save-and-compile ()
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file -1))
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "C-x C-s") 'save-and-compile)))

(add-hook 'LaTeX-mode-hook 'adaptive-wrap-prefix-mode)

;; HTML mode
(add-hook 'html-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'html-mode-hook (lambda () (toggle-word-wrap 0)))

;; Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Scala mode
(defun toggle-truncate-lines-on ()
  (toggle-truncate-lines 1))
(add-hook 'scala-mode-hook 'toggle-truncate-lines-on)
(add-hook 'scala-mode-hook (lambda () (setq fill-column 100)))

;; C-like languages
(add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))

;; Custom keys for eshell
(defun eshell-unbind-arrow-keys ()
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line))
(add-hook 'eshell-mode-hook 'eshell-unbind-arrow-keys)
(add-hook 'eshell-mode-hook
          (lambda () (eshell/export "EDITOR=emacsclient --alternate-editor=")))

;; Use Python mode for TARGETS files
(setq auto-mode-alist (cons '("\\/TARGETS$" . python-mode) auto-mode-alist))

;; Use C++ mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Interpret ANSI color codes in compiles
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Use semantic highlighting for ENSIME
(setq ensime-sem-high-faces
      '((var . font-lock-variable-name-face)
        (val . font-lock-variable-name-face)
        (varField . font-lock-variable-name-face)
        (valField . font-lock-variable-name-face)
        (functionCall . default)
        (param . font-lock-variable-name-face)
        (class . font-lock-type-face)
        (trait . font-lock-type-face)
        (object . font-lock-type-face)
        (package . font-lock-preprocessor-face)))

;; Shell customizations
(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (let ((inhibit-read-only t)
        (output-end (process-mark (get-buffer-process (current-buffer)))))
    (put-text-property comint-last-output-start output-end 'read-only t)))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

;; All backups in temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq tramp-auto-save-directory temporary-file-directory)

;; auto-complete for eshell
;; TODO: Try https://gist.github.com/878213
;; (eval-after-load 'auto-complete-config
;;   '(progn
;;      (add-to-list 'ac-modes 'eshell-mode)
;;      (ac-define-source pcomplete
;;        '((candidates . pcomplete-completions)))
;;      (add-hook
;;       'eshell-mode-hook
;;       (lambda ()
;;         (setq ac-sources '(ac-source-pcomplete))))))

;; Oxford brackets for TeX input method
;; (mapc (lambda (pair)
;;             (quail-defrule (car pair) (cadr pair) "TeX"))
;;        '(("\\llbracket" "⟦")
;;          ("\\rrbracket" "⟧")))

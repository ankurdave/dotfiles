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
(when (fboundp 'turn-on-show-smartparens-mode)
  (add-hook 'html-mode-hook 'turn-on-show-smartparens-mode))
(when (fboundp 'turn-on-smartparens-mode)
  (add-hook 'html-mode-hook 'turn-on-smartparens-mode))

;; Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(when (fboundp 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
(when (fboundp 'turn-on-fci-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-fci-mode))

;; Python mode
(when (fboundp 'turn-on-fci-mode)
  (add-hook 'python-mode-hook 'turn-on-fci-mode))

;; Scala mode
(add-hook 'scala-mode-hook (lambda () (setq fill-column 100)))
(when (fboundp 'turn-on-fci-mode)
  (add-hook 'scala-mode-hook 'turn-on-fci-mode))

;; Dired mode
(add-hook 'dired-mode-hook (lambda () (toggle-truncate-lines 1)))

;; C-like languages
(add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))
(when (fboundp 'turn-on-fci-mode)
  (add-hook 'c-mode-common-hook 'turn-on-fci-mode))

;; Custom keys for eshell
(defun eshell-unbind-arrow-keys ()
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line))
(add-hook 'eshell-mode-hook 'eshell-unbind-arrow-keys)
(add-hook 'eshell-mode-hook
          (lambda () (eshell/export "EDITOR=emacsclient")))

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
      `(("." . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq tramp-auto-save-directory temporary-file-directory)

;; Keep region when undoing in region
;; from https://github.com/magnars/.emacs.d/blob/de3b35fa41ced10c273f86d2d50d2232eb7e4a6b/my-misc.el#L4
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

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
(eval-after-load "quail/latin-ltx"
  '(mapc (lambda (pair)
          (quail-defrule (car pair) (cadr pair) "TeX"))
        '(("\\llbracket" "⟦")
          ("\\rrbracket" "⟧"))))

;; Enable key repeat in OS X Lion
(when (fboundp 'ns-set-resource)
  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO"))

;; popwin configuration
(push '("\\*magit: .*\\*" :regexp t) popwin:special-display-config)
(push '("*Buffer List*") popwin:special-display-config)
(push '("*Backtrace*") popwin:special-display-config)

;; Avoid *Buffer List* undo history warning; see
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2013-04/msg00497.html
(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)

;; Customize molokai theme
(when (require 'molokai-theme nil t)
  (custom-theme-set-faces
   'molokai
   '(w3m-anchor
     ((((class color) (background light))
       (:foreground "blue"))
      (((class color) (background dark))
       (:foreground "SkyBlue1"))
      (t
       (:underline
        (:color foreground-color :style line)))))
   '(w3m-arrived-anchor
     ((((class color) (background light))
       (:foreground "navy"))
      (((class color) (background dark))
       (:foreground "SkyBlue4"))
      (t
       (:underline
        (:color foreground-color :style line)))))))

(put 'narrow-to-region 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)

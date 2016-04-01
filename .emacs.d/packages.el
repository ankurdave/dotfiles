;;; `use-package' setup:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;;; Package configuration:

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package adaptive-wrap
  :init
  (add-hook 'html-mode-hook #'adaptive-wrap-prefix-mode)
  (add-hook 'LaTeX-mode-hook #'adaptive-wrap-prefix-mode))

(use-package auto-compile)

(use-package autorevert
  :diminish auto-revert-mode
  :defer t)

(use-package browse-kill-ring
  :bind ("C-M-y" . browse-kill-ring))

(use-package c++-mode
  :ensure cc-mode
  :mode "\\.h\\'")

(use-package color-identifiers-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'color-identifiers-mode)
  :diminish color-identifiers-mode)

(use-package comint
  :ensure nil
  :init
  (defun make-my-shell-output-read-only (text)
    "Add to comint-output-filter-functions to make stdout read only in my shells."
    (let ((inhibit-read-only t)
          (output-end (process-mark (get-buffer-process (current-buffer)))))
      (put-text-property comint-last-output-start output-end 'read-only t)))
  (add-hook 'comint-output-filter-functions #'make-my-shell-output-read-only))

(use-package company
  :bind (("M-/" . company-complete-common)
         (:map company-active-map
               ("C-w" . nil)))
  :config (global-company-mode)
  :demand
  :diminish company-mode)

(use-package compat
  :load-path "lisp/"
  :ensure nil)

(use-package dash)

(use-package diminish)

(use-package dired
  :ensure nil
  :init
  (add-hook 'dired-mode-hook (lambda () (toggle-truncate-lines 1))))

(use-package dired-details
  :functions dired-details-install
  :config
  (dired-details-install)
  :demand)

(use-package dtrt-indent
  :config
  (setq global-mode-string
        (remove 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-mode t)
  :diminish dtrt-indent-mode)

(use-package eldoc
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  :diminish eldoc-mode)

(use-package em-smart
  :ensure nil
  :after esh-mode
  :commands eshell-smart-initialize
  :config
  (eshell-smart-initialize)
  :defer t)

(use-package ensime
  :disabled
  :init
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
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
              ("<up>" . previous-line)
              ("<down>" . next-line)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (add-to-list 'exec-path-from-shell-variables "SCALA_HOME")
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package files
  :ensure nil
  :config
  (setq backup-directory-alist
        `(("." . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package fill-column-indicator
  :if window-system
  :init
  (add-hook 'c-mode-common-hook #'turn-on-fci-mode)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-fci-mode)
  (add-hook 'python-mode-hook #'turn-on-fci-mode)
  (add-hook 'scala-mode-hook #'turn-on-fci-mode))

(use-package frame
  :ensure nil
  :bind ("M-`" . other-frame))

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package gnuplot-mode
  :mode "\\.plt\\'")

(use-package god-mode
  :disabled
  :bind (("<escape>" . god-mode-all)
         (:map god-local-mode-map
               ("/" . undo-tree-undo)
               ("." . repeat)
               ("i" . prompt-and-insert))))

;;; TODO move binds into :bind
(use-package helm
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))
  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map)
  (unbind-key "C-SPC" helm-map)
  (unbind-key "C-k" helm-map)
  (unbind-key "C-w" helm-map)
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :diminish helm-mode
  :demand)

(use-package helm-git-grep)

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helm-themes)

(use-package highlight-symbol
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-symbol-mode)
  (add-hook 'scala-mode-hook #'highlight-symbol-mode)
  :diminish highlight-symbol-mode)

(use-package htmlize
  :defer t)

(use-package lisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("M-n" . highlight-symbol-next)
              ("M-p" . highlight-symbol-prev)
              ("M-." . find-symbol-at-point)
              ("C-c e" . eval-last-sexp-other-buffer)))

(use-package markdown-mode
  :defer t)

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package notmuch
  :bind (:map notmuch-search-mode-map
              ("I" . notmuch-search-mark-read)
              ("g" . notmuch-refresh-this-buffer))
  :bind (:map notmuch-show-mode-map
              ("d" . notmuch-mark-deleted)
              ("o" . goto-address-at-point))
  :bind (:map notmuch-hello-mode-map
              ("g" . notmuch-refresh-this-buffer))
  :bind (:map notmuch-tree-mode-map
              ("n" . notmuch-tree-next-matching-message-and-mark-read)))

(use-package org
  :bind (:map org-mode-map
              ("<tab>" . org-indent-item-or-cycle)
              ("<S-tab>" . org-outdent-item-or-shifttab))
  :diminish org-indent-mode)

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config
  (with-eval-after-load 'utils
    (bind-key "C-w" (make-backward-kill-word-fn paredit-backward-kill-word)
              paredit-mode-map))
  :bind ("M-(" . paredit-wrap-round)
  :diminish paredit-mode)

(use-package projectile
  :config
  ;; file-exists-p -> file-regular-p
  (defun projectile-visit-project-tags-table ()
    "Visit the current project's tags table."
    (when (projectile-project-p)
      (let ((tags-file (projectile-expand-root projectile-tags-file-name)))
        (when (file-regular-p tags-file)
          (with-demoted-errors "Error loading tags-file: %s"
            (visit-tags-table tags-file t)))))))

(use-package python
  :ensure nil
  :mode ("\\/TARGETS$" . python-mode))

(use-package rainbow-mode
  :defer t)

(use-package s)

(use-package scala-import
  :load-path "lisp/"
  :ensure nil
  :commands
  (scala-import-init
   scala-import-organize
   scala-import-class-at-point
   scala-import-goto-class-at-point))

(use-package scala-mode2
  :init
  (add-hook 'scala-mode-hook
            (lambda ()
              (setq fill-column 100)
              (toggle-truncate-lines 1)))
  :bind (:map scala-mode-map
              ("M-." . scala-import-goto-class-at-point)
              ("M-," . scala-import-class-at-point)
              ("M-n" . highlight-symbol-next)
              ("M-p" . highlight-symbol-prev))
  :config
  ;; Scala mode indentation: extra indent step for parameters
  (defadvice scala-indent:resolve-block-step
      (around extra-indent-step-for-parameters (start anchor) activate)
    "Add an extra indent step for parameters to a class or method."
    (let ((in-parameter-list
           (save-excursion
             (condition-case nil
                 (and
                  (goto-char start)
                  ;; Jump out of parameter list
                  (sp-backward-up-sexp)
                  ;; Check list paren type
                  (scala-syntax:looking-at "(")
                  ;; Move backward across all previous parameter lists
                  (progn
                    (while (scala-syntax:looking-back-token ")")
                      (scala-syntax:backward-sexp))
                    t)
                  ;; Move backward across constructor modifiers
                  (progn
                    (while (scala-syntax:looking-back-token scala-syntax:modifiers-re)
                      (scala-syntax:backward-sexp)
                      (when (scala-syntax:looking-at "]")
                        (backward-list)))
                    t)
                  ;; Move backward across type parameter list
                  (progn
                    (when (scala-syntax:looking-back-token "]")
                      (scala-syntax:backward-sexp))
                    t)
                  ;; Move backward across class/object/method name
                  (progn (scala-syntax:backward-sexp) t)
                  ;; Move backward across class/object/method keyword
                  (progn (scala-syntax:backward-sexp) t)
                  ;; Check keyword
                  (scala-syntax:looking-at (regexp-opt '("class" "object" "def") 'words)))))))
      (if in-parameter-list
          (setq ad-return-value (+ ad-do-it scala-indent:step))
        ad-do-it))))

(use-package server
  :init
  (add-hook 'after-init-hook #'server-start t))

(use-package sgml-mode
  :init
  (add-hook 'html-mode-hook (lambda () (toggle-word-wrap 0)))
  :defer t)

(use-package smartparens
  :init
  (add-hook 'html-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'html-mode-hook #'turn-on-show-smartparens-mode)
  (add-hook 'scala-mode-hook #'turn-on-smartparens-mode)
  :bind (:map smartparens-mode-map
              ("<M-up>" . sp-splice-sexp-killing-backward))
  :diminish smartparens-mode)

(use-package smartparens-config
  :ensure smartparens)

(use-package subword
  :ensure nil
  :commands subword-backward-kill
  :config
  (bind-key "C-w" (make-backward-kill-word-fn subword-backward-kill (1))
            subword-mode-map)
  :defer t)

(use-package tex
  :ensure auctex
  :init
  (autoload 'TeX-command "tex-buf" nil nil)
  (defun LaTeX-compile ()
    (TeX-command "LaTeX" 'TeX-master-file -1))
  (defun LaTeX-compile-after-save ()
    (add-hook 'after-save-hook #'LaTeX-compile nil t))
  (add-hook 'LaTeX-mode-hook #'LaTeX-compile-after-save)
  :defer t)

(use-package typing-test
  :load-path "lisp/"
  :ensure nil
  :commands typing-test)

(use-package undo-tree
  :bind (("C--" . undo-tree-undo)
         ("C-?" . undo-tree-redo))
  :config
  (global-undo-tree-mode)
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
  :demand)

(use-package utils
  :load-path "lisp/"
  :ensure nil
  :config
  (bind-key "C-w" (make-backward-kill-word-fn backward-kill-word (1)))
  :bind (("C-x 4 p" . projectile-find-file-other-window)
         ("C-x C-x" . switch-to-other-buffer)
         ("C-c d" . date))
  :demand)

(use-package vc-git
  :ensure vc
  :commands vc-git-grep)

(use-package visual-regexp
  :disabled)

(use-package visual-regexp-steroids
  :disabled)

(use-package w3m
  :disabled)

(use-package w3m-hacker-news
  :disabled
  :load-path "lisp/"
  :ensure nil
  :after w3m
  :defines w3m-mode-map
  :bind (:map w3m-mode-map
              ("n" . next-hn-comment)
              ("p" . previous-hn-comment)
              ("P" . parent-hn-comment)
              ("C-c n" . next-sibling-hn-comment)
              ("C-x n c" . narrow-to-hn-comment-at-point)))

(use-package w3m-isearch-links
  :disabled
  :load-path "lisp/"
  :ensure nil
  :bind (:map w3m-mode-map
              ("/" . w3m-isearch-links)))

(use-package whitespace
  :diminish global-whitespace-mode
  :defer t)

(use-package ws-butler
  :diminish ws-butler-mode)

(use-package zenburn-theme)

(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 5))
  (use-package git-commit))

(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 5))
  (use-package helm-flx))

(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 5))
  (use-package magit
    :bind ("C-x m" . magit-status)
    :diminish magit-auto-revert-mode))

;;; Package configuration ends here

;; Local Variables:
;; eval: (add-to-list (quote before-save-hook) (quote sort-package-configurations))
;; End:

;;; `use-package' setup:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("org" . "https://orgmode.org/elpa/"))
;; Do not save the value of package-selected-packages to customizations.el
;; because it interferes with version control.
(defun package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE, but do not save it."
  (when value
    (setq package-selected-packages value)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;;; Package configuration:

(use-package ace-jump-mode
  :bind ("M-j" . ace-jump-mode))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?e ?u ?h ?t ?n ?s))
  (setq aw-dispatch-alist
        '((?o aw-flip-window)))
  (setq aw-dispatch-always t))

(use-package adaptive-wrap
  :init
  (add-hook 'html-mode-hook #'adaptive-wrap-prefix-mode)
  (add-hook 'LaTeX-mode-hook #'adaptive-wrap-prefix-mode))

(use-package auto-compile)

(use-package auto-package-update
  :config
  (auto-package-update-maybe))

(use-package autorevert
  :diminish auto-revert-mode
  :defer t)

(use-package bazel-mode)

(use-package beancount-mode
  :ensure nil
  :mode ("\\.beancount\\'"))

(use-package c++-mode
  :ensure cc-mode
  :mode "\\.h\\'"
  :mode "\\.ino\\'"
  :init
  (add-hook 'c++-mode-hook (lambda () (toggle-truncate-lines 1)))
  (add-hook 'c++-mode-hook (lambda () (set-fill-column 100)))
  (add-hook 'c++-mode-hook #'turn-on-auto-fill)
  (eval-when-compile (require 'cc-mode))
  :bind (:map c++-mode-map
              ("C-c C-c" . print-line-counters)
              ("M-j" . ace-jump-mode)
              ("M-n" . highlight-symbol-next)
              ("M-p" . highlight-symbol-prev)))

(use-package cmake-mode)

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

(use-package compat
  :load-path "lisp/"
  :ensure nil)

(use-package compile
  :ensure nil
  :init
  (defun bury-successful-compilation-buffer (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        (bury-buffer buf)))
  (add-hook 'compilation-finish-functions #'bury-successful-compilation-buffer))

(use-package conf-mode
  :ensure nil
  :config
  (defun indent-ssh-config-line ()
    (goto-char (line-beginning-position))
    (delete-horizontal-space)
    (unless (looking-at "Host\\b")
      (indent-to (indent-next-tab-stop 0))))
  (defun ssh-config-setup-indent ()
    (when (s-ends-with-p "/.ssh/config" (buffer-file-name))
      (setq-local indent-line-function #'indent-ssh-config-line)))
  (add-hook 'conf-space-mode-hook #'ssh-config-setup-indent))

(use-package counsel
  :config
  (defun ankurdave--counsel-delete-filename-or-up-directory ()
    (interactive)
    (if (string-empty-p ivy-text)
        (counsel-up-directory)
      (ivy-kill-line)))
  :bind (:map ivy-minibuffer-map
              ("C-l" . ankurdave--counsel-delete-filename-or-up-directory))
  :diminish counsel-mode)

(use-package counsel-projectile)

(use-package csv-mode)

(use-package cuda-mode)

(use-package dash)

(use-package dash-functional)

(use-package diminish)

(use-package dired
  :ensure nil
  :init
  (add-hook 'dired-mode-hook (lambda () (toggle-truncate-lines 1))))

(use-package dockerfile-mode)

(use-package dtrt-indent
  :config
  (setq global-mode-string
        (remove 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-global-mode)
  :diminish dtrt-indent-mode)

(use-package dumb-jump
  :bind (("M-." . dumb-jump-go)))

(use-package edit-server
  :config
  ;; Back up buffers to local directory before exiting.
  ;; From https://emacs.stackexchange.com/a/16539.
  (defvar edit-server-save-dir (locate-user-emacs-file "edit-server-saves"))
  (unless (file-exists-p edit-server-save-dir)
    (make-directory edit-server-save-dir))
  (defun save-edit-server-buffer ()
    (let ((backup-directory-alist (list (cons "." edit-server-save-dir))))
      (save-excursion
        (write-region nil nil (car (find-backup-file-name (buffer-name))) nil 0))))
  (add-hook 'edit-server-done-hook #'save-edit-server-buffer)
  (edit-server-start))

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

(use-package files
  :ensure nil
  :config
  (setq backup-directory-alist
        `(("." . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package flx)

(use-package frame
  :ensure nil
  :bind ("M-`" . other-frame))

(use-package git-commit)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package gnuplot-mode
  :mode "\\.plt\\'")

(use-package google-c-style
  :hook (c++-mode . google-set-c-style))

(use-package highlight-symbol
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-symbol-mode)
  (add-hook 'scala-mode-hook #'highlight-symbol-mode)
  (add-hook 'c++-mode-hook #'highlight-symbol-mode)
  :diminish highlight-symbol-mode)

(use-package htmlize
  :defer t)

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :bind (("C-x b" . ivy-switch-buffer))
  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package lisp-extra-font-lock
  :config
  (lisp-extra-font-lock-global-mode 1))

(use-package lisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("M-n" . highlight-symbol-next)
              ("M-p" . highlight-symbol-prev)
              ("M-." . find-symbol-at-point)
              ("C-c e" . eval-last-sexp-other-buffer)
              ("C-M-w" . backward-kill-sexp)
              ("C-M-d" . kill-sexp)
              ("C-w" . paredit-kill-region)))

(use-package mac-win
  :ensure nil
  :bind ("<C-tab>" . mac-next-tab))

(use-package magit
  :bind ("C-x m" . magit-status)
  :diminish magit-auto-revert-mode)

(use-package markdown-mode
  :defer t)

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package notmuch
  :disabled
  :bind (:map notmuch-search-mode-map
              ("I" . notmuch-search-mark-read)
              ("g" . notmuch-refresh-this-buffer))
  :bind (:map notmuch-show-mode-map
              ("d" . notmuch-mark-deleted)
              ("o" . goto-address-at-point)
              ("v" . notmuch-view-html)
              ("g" . notmuch-refresh-this-buffer))
  :bind (:map notmuch-hello-mode-map
              ("g" . notmuch-refresh-this-buffer))
  :bind (:map notmuch-tree-mode-map
              ("n" . notmuch-tree-next-matching-message-and-mark-read)
              ("g" . notmuch-refresh-this-buffer))
  :config
  (setq notmuch-saved-searches
        '((:name "Unread" :query "is:unread AND tag:inbox" :search-type tree)
          (:name "Personal" :query "tag:inbox AND NOT tag:notifications AND NOT tag:spark-lists AND NOT tag:berkeley-lists AND NOT tag:amplab-lists AND date:3months..")
          (:name "Inbox" :query "(tag:inbox OR tag:is-reply) AND date:3months..")
          (:name "Sent" :query "tag:sent AND date:3months..")
          (:name "Archive" :query "NOT (tag:inbox OR tag:sent) AND date:3months..")))
  (setq notmuch-search-result-format
   `(("date" . "%12s ")
     ("count" . "%-7s ")
     ("authors" . "%-20s ")
     ("tags" . "%-20.20s ")
     ("subject" . "%s")))
  (setq notmuch-tag-formats
        '(("inbox")
          ("notifications" "notif")
          ("lists")
          ("berkeley-lists" "berk")
          ("amplab-lists" "amp")
          ("spark-lists" "spark")
          ("attachment" "attach")
          ("lists/.*" (substring tag 6 (s-index-of "." tag)))
          ("unread" (propertize tag 'face '(:foreground "#CC9393")))
          ("flagged"
           (propertize tag 'face '(:foreground "blue"))))))

(use-package org
  :ensure nil
  :bind (:map org-mode-map
              ("<tab>" . org-indent-item-or-cycle)
              ("<S-tab>" . org-outdent-item-or-shifttab)))

(use-package org-indent
  :ensure nil
  :diminish org-indent-mode)

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config
  (with-eval-after-load 'utils
    (bind-key "C-w" (make-backward-kill-word-fn paredit-backward-kill-word)
              paredit-mode-map))
  :bind (("M-(" . paredit-wrap-round))
  :bind (:map paredit-mode-map
              ("C-M-d" . nil))
  :diminish paredit-mode)

(use-package paren-face
  :config
  (with-eval-after-load 'zenburn-theme
    (zenburn-with-color-variables
      (custom-theme-set-faces
       'zenburn
       `(parenthesis ((t (:foreground ,zenburn-fg-1))))))))

(use-package php-mode)

(use-package projectile
  :config
  ;; file-exists-p -> file-regular-p
  (defun projectile-visit-project-tags-table ()
    "Visit the current project's tags table."
    (when (projectile-project-p)
      (let ((tags-file (projectile-expand-root projectile-tags-file-name)))
        (when (file-regular-p tags-file)
          (with-demoted-errors "Error loading tags-file: %s"
            (visit-tags-table tags-file t))))))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package protobuf-mode
  ;; Use protobuf-mode for Flatbuffers schema files
  :mode "\\.fbs\\'"
  :init
  (add-hook 'protobuf-mode-hook
            (lambda () (setq c-basic-offset 2))))

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

(use-package scala-mode
  :init
  (add-hook 'scala-mode-hook
            (lambda ()
              (setq fill-column 100)
              (toggle-truncate-lines 1)))
  :bind (:map scala-mode-map
              ("M-," . scala-import-class-at-point)
              ("M-n" . highlight-symbol-next)
              ("M-p" . highlight-symbol-prev)))

(use-package server
  :init
  (add-hook 'after-init-hook #'server-start t))

(use-package sgml-mode
  :init
  (add-hook 'html-mode-hook (lambda () (toggle-word-wrap 0)))
  :defer t)

(use-package simple
  :ensure nil
  :init
  ;; C-x C-n is bound to `set-goal-column' by default
  (unbind-key "C-x C-n")
  :diminish auto-fill-function)

(use-package smartparens
  :init
  (add-hook 'html-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'html-mode-hook #'turn-on-show-smartparens-mode)
  (add-hook 'scala-mode-hook #'turn-on-smartparens-mode)
  :bind (:map smartparens-mode-map
              ("<M-up>" . sp-splice-sexp-killing-backward)
              ("C-M-w"))
  :diminish smartparens-mode)

(use-package smartparens-config
  :ensure smartparens)

;; Enables counsel-M-x to sort by recently used
(use-package smex)

(use-package subword
  :ensure nil
  :commands subword-backward-kill
  :config
  (with-eval-after-load 'utils
    (bind-key "C-w" (make-backward-kill-word-fn subword-backward-kill (1))
              subword-mode-map))
  :defer t)

(use-package terraform-mode
  :config
  (terraform-format-on-save-mode 1))

(use-package tex
  :disabled
  :ensure auctex
  :init
  (autoload 'TeX-command "tex-buf" nil nil)
  (defun LaTeX-compile ()
    (TeX-command "LaTeX" 'TeX-master-file -1))
  (defun LaTeX-compile-after-save ()
    (add-hook 'after-save-hook #'LaTeX-compile nil t))
  (add-hook 'LaTeX-mode-hook #'LaTeX-compile-after-save)
  :defer t)

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
  :init
  (autoload 'profiler-report "profiler")
  (autoload 'profiler-stop "profiler")
  :load-path "lisp/"
  :ensure nil
  :config
  (bind-key "C-w" (make-backward-kill-word-fn backward-kill-word (1)))
  (defun my-profiler-start ()
    (interactive)
    (profiler-start 'cpu))
  (defun my-profiler-stop ()
    (interactive)
    (profiler-report)
    (profiler-stop))
  :bind (("C-x 4 p" . projectile-find-file-other-window)
         ("C-x C-x" . switch-to-other-buffer)
         ("C-c d" . date)
         ("C-c e" . calc-eval-region)
         ("C-c r" . my-profiler-start)
         ("C-c s" . my-profiler-stop))
  :demand)

(use-package web-mode)

(use-package which-key
  :bind (("C-c k" . which-key-show-top-level)))

(use-package ws-butler
  :diminish ws-butler-mode)

(use-package xt-mouse
  :ensure nil
  :bind (("<mouse-4>" . scroll-down-line)
         ("<mouse-5>" . scroll-up-line)))

(use-package zenburn-theme
  :init
  (when (not window-system)
    (defvar zenburn-override-colors-alist
      '(("zenburn-fg-1"     . "#5f5f5f"))))
  :config
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(diff-added          ((t (:background ,zenburn-bg :foreground ,zenburn-green))))
     `(diff-changed        ((t (:background "#5F5F00" :foreground ,zenburn-yellow-1))))
     `(diff-removed        ((t (:background ,zenburn-bg :foreground ,zenburn-red-2))))
     `(diff-refine-added   ((t (:background "#5F875F" :foreground ,zenburn-green+4))))
     `(diff-refine-changed ((t (:background "#878700" :foreground ,zenburn-yellow))))
     `(diff-refine-removed ((t (:background "#875F5F" :foreground ,zenburn-red))))
     `(magit-diff-added    ((t (:background ,zenburn-bg :foreground ,zenburn-green))))
     `(magit-diff-added-highlight ((t (:background ,zenburn-bg-1 :foreground ,zenburn-green))))
     `(magit-diff-removed  ((t (:background ,zenburn-bg :foreground ,zenburn-red-2))))
     `(magit-diff-removed-highlight ((t (:background ,zenburn-bg-1 :foreground ,zenburn-red-2))))
     `(magit-diff-context-highlight ((t (:background ,zenburn-bg-1)))))))

;;; Package configuration ends here

;; Local Variables:
;; eval: (add-hook (quote before-save-hook) (quote sort-package-configurations) nil t)
;; End:

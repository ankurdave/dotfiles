;;; `use-package' setup: -*- lexical-binding: t -*-

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))
;; (add-hook 'after-init-hook #'use-package-report)

;; Set up quelpa to allow use-package to fetch packages from GitHub.
(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice))

;;; Package configuration:

(use-package ace-window
  :bind (
         ;; Jump to window. Mnemonic: "Jump".
         ("M-j" . ace-window))
  :config
  (setq aw-keys '(?a ?e ?u ?h ?t ?n ?s))
  (setq aw-dispatch-alist
        '((?o aw-flip-window)))
  (setq aw-dispatch-always t))

(use-package adaptive-wrap
  :ensure t
  :init
  (add-hook 'html-mode-hook #'adaptive-wrap-prefix-mode)
  (add-hook 'LaTeX-mode-hook #'adaptive-wrap-prefix-mode))

(use-package auto-compile
  :ensure t
  :custom
  (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 1)
  (auto-package-update-prompt-before-update t)
  :init
  (add-hook 'after-init-hook #'auto-package-update-maybe))

(use-package autorevert
  :diminish auto-revert-mode
  :defer t
  :custom
  (auto-revert-verbose nil))

(use-package bazel
  :defer t)

(use-package beancount
  :quelpa (beancount
           :fetcher github
           :repo "beancount/beancount-mode")
  :mode ("\\.beancount\\'" . beancount-mode)
  :bind (("C-c >" . beancount-goto-next-transaction)
         ("C-c <" . beancount-goto-previous-transaction))
  :config
  (defun beancount-check ()
  "Run `beancount-check-program'."
  (interactive)
  (let ((compilation-read-command nil))
    (beancount--run beancount-check-program "main.beancount"))))

(use-package cc-mode
  :mode ("\\.ino\\'" . c++-mode)
  :init
  (add-hook 'c++-mode-hook (lambda () (toggle-truncate-lines 1)))
  (add-hook 'c++-mode-hook (lambda () (set-fill-column 100)))
  (add-hook 'c++-mode-hook #'turn-on-auto-fill))

(use-package cmake-mode
  :defer t)

(use-package color-identifiers-mode
  :hook (emacs-lisp-mode-hook . color-identifiers-mode)
  :diminish color-identifiers-mode)

(use-package comint
  :ensure nil
  :defer t
  :init
  (defun make-my-shell-output-read-only (text)
    "Add to comint-output-filter-functions to make stdout read only in my shells."
    (let ((inhibit-read-only t)
          (output-end (process-mark (get-buffer-process (current-buffer)))))
      (put-text-property comint-last-output-start output-end 'read-only t)))
  (add-hook 'comint-output-filter-functions #'make-my-shell-output-read-only))

(use-package comp
  :ensure nil
  :config
  (setq native-comp-async-report-warnings-errors nil))

(use-package ankurdave-compat
  :load-path "lisp/"
  :ensure nil)

(use-package compile
  :ensure nil
  :defer t
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

(use-package copilot
  :quelpa (copilot
           :fetcher github
           :repo "zerolfx/copilot.el"
           :branch "main"
           :files ("dist" "*.el"))
  :config
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))

(use-package counsel
  :custom
  (counsel-mode t)
  :config
  (defun ankurdave--counsel-delete-filename-or-up-directory ()
    (interactive)
    (if (string-empty-p ivy-text)
        (counsel-up-directory)
      (ivy-kill-line)))
  (setq ivy-initial-inputs-alist nil)
  :bind (:map ivy-minibuffer-map
              ("C-l" . ankurdave--counsel-delete-filename-or-up-directory))
  :diminish counsel-mode)

(use-package counsel-projectile
  :bind (
         ;; Grep within project. Mnemonic: "Search".
         ("M-s" . counsel-projectile-grep))
  :custom
  (counsel-projectile-mode t))

(use-package csv-mode
  :defer t)

(use-package dash)

(use-package diminish)

(use-package dired
  :ensure nil
  :init
  (add-hook 'dired-mode-hook (lambda () (toggle-truncate-lines 1))))

(use-package dockerfile-mode
  :defer t)

(use-package dtrt-indent
  :config
  (setq global-mode-string
        (remove 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-global-mode)
  :diminish dtrt-indent-mode)

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  :config
  ;; Use M-. to call `xref-find-definitions'.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package edit-server
  :custom
  (edit-server-default-major-mode 'gfm-mode)
  (edit-server-new-frame nil)
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

(use-package elec-pair
  :ensure nil
  :custom
  (electric-pair-mode t))

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

(use-package files
  :ensure nil
  :custom
  (confirm-kill-emacs 'y-or-n-p)
  :config
  (setq backup-directory-alist
        `(("." . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package flx)

(use-package frame
  :ensure nil
  :init
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  :bind ("M-`" . other-frame)
  :custom
  (blink-cursor-mode nil))

(use-package gnuplot-mode
  :mode "\\.plt\\'")

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

(use-package highlight-symbol
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-symbol-mode)
  (add-hook 'scala-mode-hook #'highlight-symbol-mode)
  (add-hook 'c++-mode-hook #'highlight-symbol-mode)
  (add-hook 'c++-ts-mode-hook #'highlight-symbol-mode)
  (add-hook 'java-mode-hook #'highlight-symbol-mode)
  (add-hook 'python-mode-hook #'highlight-symbol-mode)
  :diminish highlight-symbol-mode
  :custom
  (highlight-symbol-idle-delay 1.5)
  (highlight-symbol-on-navigation-p t)
  :custom-face
  (highlight-symbol-face ((t (:background "black" :foreground "yellow"))))
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev)))

(use-package htmlize
  :defer t)

(use-package ivy
  :diminish ivy-mode
  :custom
  (ivy-mode t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  (ivy-count-format "(%d/%d) ")
  (ivy-extra-directories nil)
  (ivy-fixed-height-minibuffer t)
  (ivy-height 20)
  :custom-face
  (ivy-current-match ((t (:background "#4F4F4F" :foreground "#F0DFAF" :underline nil :weight normal))))
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :bind (("C-x b")
         ;; Mnemonic: "switch buffeR".
         ("M-r" . ivy-switch-buffer))
  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  ;; Highlight the full line of the current selection, not just the text portion.
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package lisp-extra-font-lock
  :config
  (lisp-extra-font-lock-global-mode 1))

(use-package lisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("M-." . find-symbol-at-point)
              ("C-c e" . eval-last-sexp-other-buffer)
              ("C-M-w" . backward-kill-sexp)
              ("C-M-d" . kill-sexp)))

(use-package magit
  :bind ("C-x m" . magit-status)
  :diminish magit-auto-revert-mode
  :custom
  (magit-diff-arguments '("--stat"))
  (magit-diff-highlight-hunk-body t)
  (magit-diff-highlight-trailing nil)
  (magit-diff-paint-whitespace nil)
  (magit-diff-refine-hunk t)
  (magit-refresh-status-buffer t)
  (magit-refresh-verbose t)
  (magit-revision-insert-related-refs nil)
  (magit-status-headers-hook
   '(magit-insert-error-header
     magit-insert-diff-filter-header
     magit-insert-head-branch-header
     magit-insert-upstream-branch-header
     magit-insert-push-branch-header))
  (magit-status-sections-hook
   '(magit-insert-status-headers
     magit-insert-merge-log
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     magit-insert-bisect-output
     magit-insert-bisect-rest
     magit-insert-bisect-log
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes))
  (magit-completing-read-function 'ivy-completing-read))

(use-package markdown-mode
  :defer t)

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount
   '(1
     ((shift)
      . 1)
     ((control))))
  (mouse-wheel-progressive-speed nil))

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
  :custom
  (notmuch-fcc-dirs nil)
  (notmuch-hello-thousands-separator ",")
  (notmuch-search-line-faces
   '(("unread" :weight bold)
     ("flagged" :foreground "#8CD0D3")))
  (notmuch-search-oldest-first nil)
  (notmuch-show-all-tags-list t)
  (notmuch-show-indent-messages-width 1)
  (notmuch-show-indent-multipart nil)
  (notmuch-show-logo nil)
  (notmuch-show-part-button-default-action (quote notmuch-show-interactively-view-part))
  (notmuch-unread-search-term
   "(tag:inbox OR tag:is-reply OR tag:graphx OR tag:notifications) AND is:unread")

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
              ("<S-tab>" . org-outdent-item-or-shifttab))
  :custom
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-startup-truncated nil))

(use-package org-indent
  :ensure nil
  :diminish org-indent-mode
  :defer t)

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config
  (with-eval-after-load 'utils
    (bind-key "C-w" (make-backward-kill-word-fn paredit-backward-kill-word)
              paredit-mode-map))
  :bind (("M-(" . paredit-wrap-round))
  :bind (:map paredit-mode-map
              ("C-M-d")
              ("M-s")
              ("M-r"))
  :diminish paredit-mode)

(use-package paren-face
  :custom
  (global-paren-face-mode t)
  :config
  (with-eval-after-load 'zenburn-theme
    (zenburn-with-color-variables
      (custom-theme-set-faces
       'zenburn
       `(parenthesis ((t (:foreground ,zenburn-fg-1))))))))

(use-package php-mode
  :defer t)

(use-package projectile
  :bind
  (
   ;; Find file within project. Mnemonic: "Open file".
   ("M-o" . ankurdave--projectile-find-file-dwim)
   ;; Go to corresponding header or source file. Mnemonic: "Header".
   ("M-h" . projectile-find-other-file))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-global-mode t)
  (projectile-mode t)
  (projectile-mode-line nil)
  (projectile-use-git-grep t)
  :config
  ;; file-exists-p -> file-regular-p
  (defun projectile-visit-project-tags-table ()
    "Visit the current project's tags table."
    (when (projectile-project-p)
      (let ((tags-file (projectile-expand-root projectile-tags-file-name)))
        (when (file-regular-p tags-file)
          (with-demoted-errors "Error loading tags-file: %s"
            (visit-tags-table tags-file t))))))
  (defun ankurdave--projectile-find-file-dwim (&optional invalidate-cache)
    "Wrapper around `projectile-find-file-dwim' that prompts for
a project if necessary."
    (interactive "P")
    (projectile-ensure-project (projectile-project-root))
    (projectile--find-file-dwim invalidate-cache)))

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
              ("M-," . scala-import-class-at-point))
  :custom
  (scala-indent:align-parameters nil)
  (scala-indent:use-javadoc-style t))

(use-package server
  :init
  (add-hook 'after-init-hook #'server-start t))

(use-package sgml-mode
  :init
  (add-hook 'html-mode-hook (lambda () (toggle-word-wrap 0)))
  :defer t)

(use-package simple
  :ensure nil
  :bind (;; C-x C-n is bound to `set-goal-column' by default
         ("C-x C-n")
         ("M-g" . goto-line))
  :diminish auto-fill-function
  :custom
  (compilation-auto-jump-to-first-error nil)
  (column-number-mode t)
  (shift-select-mode nil))

;; Enables counsel-M-x to sort by recently used
(use-package smex)

(use-package sql-indent
  :hook (sql-mode-hook . sqlind-minor-mode)
  :diminish sqlind-minor-mode)

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
  (terraform-format-on-save-mode 1)
  :defer t)

(use-package tex
  :ensure nil
  :init
  (autoload 'TeX-command "tex-buf" nil nil)
  (defun LaTeX-compile ()
    (TeX-command "LaTeX" 'TeX-master-file -1))
  (defun LaTeX-compile-after-save ()
    (add-hook 'after-save-hook #'LaTeX-compile nil t))
  (add-hook 'LaTeX-mode-hook #'LaTeX-compile-after-save)
  :defer t)

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (c "https://github.com/tree-sitter/tree-sitter-c")))
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang))))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-hook 'c++-ts-mode-hook (lambda () (toggle-truncate-lines 1)))
  (add-hook 'c++-ts-mode-hook (lambda () (set-fill-column 100)))
  (add-hook 'c++-ts-mode-hook #'turn-on-auto-fill)
  (defun ankurdave--treesit-left-child-while (node pred)
    "Return the deepest left child of NODE that satisfies PRED."
    (let ((last nil))
      (while (and node (funcall pred node))
        (setq last node
              node (treesit-node-child-by-field-name node "left")))
      last))
  (defun ankurdave--c-ts-mode--deepest-binary-expression-left-shift-operator (_n parent &rest _)
    "Anchor to the `<<' operator of the deepest binary expression within
PARENT."
    (save-excursion
      (treesit-node-start
       (treesit-node-child-by-field-name
        (ankurdave--treesit-left-child-while
         parent
         (lambda (node)
           (and node
                (string-match-p "binary_expression" (treesit-node-type node))
                (string-match-p "<<" (treesit-node-string
                                      (treesit-node-child-by-field-name node "operator"))))))
        "operator"))))
  (defun ankurdave--set-treesit-node-as-region (node)
    "For debugging treesit indentation."
    (set-mark (treesit-node-start node))
    (goto-char (treesit-node-end node))
    (activate-mark))
  (defun ankurdave--highlight-treesit-node-parent ()
    "For debugging treesit indentation."
    (interactive)
    (ankurdave--set-treesit-node-as-region (treesit-node-parent (treesit-node-at (point)))))
  (defun ankurdave--highlight-treesit-node-standalone-parent ()
    "For debugging treesit indentation."
    (interactive)
    (let ((standalone-parent
           ((lambda (_n parent &rest _)
              (save-excursion
                (catch 'term
                  (while parent
                    (goto-char (treesit-node-start parent))
                    (when (looking-back (rx bol (* whitespace))
                                        (line-beginning-position))
                      (throw 'term parent))
                    (setq parent (treesit-node-parent parent))))))
            nil
            (treesit-node-parent (treesit-node-at (point))))))
      (ankurdave--set-treesit-node-as-region standalone-parent)))
  (defun ankurdave--c-ts-mode--parent-operator-is-left-shift (node parent bol &rest _)
    (and
     (string-match-p "binary_expression" (treesit-node-type parent))
     (string-match-p "<<" (treesit-node-string
                           (treesit-node-child-by-field-name parent "operator")))
     (not (equal
           (treesit-node-start node)
           (ankurdave--c-ts-mode--deepest-binary-expression-left-shift-operator node parent)))))
  (defun ankurdave--c-ts-mode--parent-is-standalone-binary-expression (node parent bol &rest _)
    (and
     (string-match-p "binary_expression" (treesit-node-type parent))
     (save-excursion
       (goto-char (treesit-node-start parent))
       (looking-back (rx bol (* whitespace))
                     (line-beginning-position)))))
  ;; Uncomment when debugging indent styles.
  ;; (setq treesit--indent-verbose t)
  (defun google-c-style-ts-indent-style ()
    "Google C/C++ style for tree-sitter.

See `treesit-simple-indent-rules' and
`treesit-simple-indent-presets' for details on the syntax.

Make sure to test against `.emacs.d/lisp/treesit-test.cc' when
modifying.
"
    `(
      ;; Similar to BSD style, but use `standalone-parent' instead of
      ;; `parent-bol'. This handles cases like the third line below:
      ;;
      ;;   int main(
      ;;       int a) {
      ;;   }
      ((node-is "}") standalone-parent 0)
      ((node-is "labeled_statement") standalone-parent c-ts-mode-indent-offset)

      ;; Align function arguments and parameters to the start of the first one, offset if
      ;; standalone. For example:
      ;;
      ;;   int foo(int a,
      ;;           int b) {}
      ;;   int foo(
      ;;       int a, int b) {}
      ((and (match nil "argument_list" nil 1 1)
            (not (node-is ")")))
       parent-bol ,(* c-ts-mode-indent-offset 2))
      ((parent-is "argument_list") (nth-sibling 1) 0)
      ((and (match nil "parameter_list" nil 1 1)
            (not (node-is ")")))
       parent-bol ,(* c-ts-mode-indent-offset 2))
      ((parent-is "parameter_list") (nth-sibling 1) 0)

      ;; The ":" in field initializer lists should be offset. For example:
      ;;
      ;;   Foo::Foo(int bar)
      ;;       : bar_(bar) {}
      ((node-is "field_initializer_list") standalone-parent ,(* c-ts-mode-indent-offset 2))
      ;; Field initializers should line up, or should be offset if standalone. For example:
      ;;
      ;;   Foo::Foo(int bar, int baz) :
      ;;       bar_(bar),
      ;;       baz_(baz) {}
      ((match nil "field_initializer_list" nil 1 1) standalone-parent ,(* c-ts-mode-indent-offset 2))
      ((parent-is "field_initializer_list") (nth-sibling 1) 0)

      ;; Class/struct members should be indented one step. Access specifiers
      ;; should be indented half a step. For example:
      ((and (node-is "access_specifier")
            (parent-is "field_declaration_list"))
       standalone-parent ,(/ c-ts-mode-indent-offset 2))
      ((parent-is "field_declaration_list") standalone-parent c-ts-mode-indent-offset)

      ;; Indent inside case blocks. For example:
      ;;
      ;;  switch (a) {
      ;;    case 0:
      ;;      123;
      ;;    case 1: {
      ;;      456;
      ;;    }
      ;;    default:
      ;;  }
      ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)

      ;; Do not indent preprocessor statements.
      ((node-is "preproc") column-0 0)

      ;; Don't indent inside namespaces.
      ((n-p-gp nil nil "namespace_definition") grand-parent 0)

      ;; Offset line continuations. For example, indent the second line as follows:
      ;;
      ;;   int64_t foo =
      ;;       bar - baz;
      ;;   foo =
      ;;       bar - baz;
      ((parent-is "init_declarator") parent-bol ,(* c-ts-mode-indent-offset 2))
      ((parent-is "assignment_expression") parent-bol ,(* c-ts-mode-indent-offset 2))
      ((parent-is "conditional_expression") parent 0)

      ;; For the left-shift operator as used with iostreams, line up the operators.
      ;;
      ;;   LOG(INFO) << "hello"
      ;;             << "world" << "foo" << "bar";
      (ankurdave--c-ts-mode--parent-operator-is-left-shift
       ankurdave--c-ts-mode--deepest-binary-expression-left-shift-operator 0)

      ;; Offset children of standalone binary expressions. For example:
      ;;   DCHECK(foo ||
      ;;          bar)
      ;;       << "Failed";
      (ankurdave--c-ts-mode--parent-is-standalone-binary-expression
       parent ,(* c-ts-mode-indent-offset 2))

      ;; Align non-standalone binary expressions to their parent. For example:
      ;;
      ;;   foo + (bar *
      ;;          baz);
      ;;   abc = b + c
      ;;         + d + e;
      ((parent-is "binary_expression") parent 0)

      ((parent-is "labeled_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "compound_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "if_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "for_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "while_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "switch_statement") standalone-parent c-ts-mode-indent-offset)
      ((parent-is "do_statement") standalone-parent c-ts-mode-indent-offset)

      ;; For example, indents the third line as follows:
      ;;   void foo(
      ;;       int bar) {
      ;;     baz();
      ;;   }
      ((or (match nil "compound_statement" nil 1 1)
           (match null "compound_statement"))
       standalone-parent c-ts-mode-indent-offset)
      ,@(alist-get 'common (c-ts-mode--indent-styles 'cpp))))
  (setq c-ts-mode-indent-style #'google-c-style-ts-indent-style))

(use-package undo-tree
  :bind (("C--" . undo-tree-undo)
         ("C-?" . undo-tree-redo))
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history/")))
  (undo-tree-mode-lighter "")
  :config
  (global-undo-tree-mode)
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

(use-package web-mode
  :defer t)

(use-package which-key
  :bind (("C-c k" . which-key-show-top-level))
  :diminish
  :custom
  (which-key-mode t)
  (which-key-side-window-max-height 0.5)
  (which-key-side-window-max-width 0.5))

(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode 1))

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
     `(diff-added          ((t (:background ,zenburn-green-5))))
     `(magit-diff-added    ((t (:background ,zenburn-green-5))))
     `(magit-diff-added-highlight ((t (:background ,zenburn-green-5))))
     `(diff-refine-added   ((t (:background ,zenburn-green-3))))

     `(diff-changed        ((t (:background "#5F5F00" :foreground ,zenburn-yellow-1))))
     `(diff-refine-changed ((t (:background "#878700" :foreground ,zenburn-yellow))))

     `(diff-removed        ((t (:background ,zenburn-red-6))))
     `(magit-diff-removed  ((t (:background ,zenburn-red-6))))
     `(magit-diff-removed-highlight ((t (:background ,zenburn-red-6))))
     `(diff-refine-removed ((t (:background ,zenburn-red-4))))

     `(magit-diff-context-highlight ((t (:background ,zenburn-bg-1))))))
  (load-theme 'zenburn t))

;;; Package configuration ends here

;; Local Variables:
;; eval: (add-hook (quote before-save-hook) (quote sort-package-configurations) nil t)
;; End:

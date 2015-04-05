;; Initialize the package system early. This is necessary because the
;; following configuration does not assume that any packages are
;; installed. Without initializing early, there would be no way to
;; distinguish between a nonexistent package and a package that hasn't
;; yet been initialized, because `package-installed-p' is undefined
;; until after initialization.
(when (fboundp 'package-initialize)
  (package-initialize)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

(defun init--install-packages ()
  "Install core packages."
  (let ((packages-to-install
         '(ace-jump-mode
           adaptive-wrap
           browse-kill-ring
           color-identifiers-mode
           company
           dash
           diminish
           dired-details
           dtrt-indent
           exec-path-from-shell
           expand-region
           fill-column-indicator
           flx-ido
           git-commit-mode
           gitconfig-mode
           gitignore-mode
           god-mode
           helm
           helm-git-grep
           helm-projectile
           highlight-symbol
           ido-ubiquitous
           ido-vertical-mode
           magit
           molokai-theme
           notmuch
           notmuch-unread
           paredit
           popwin
           projectile
           rainbow-mode
           s
           scala-mode2
           smartparens
           smex
           undo-tree
           visual-regexp
           visual-regexp-steroids
           zenburn-theme)))
    (dolist (pkg-name packages-to-install)
      (unless (package-installed-p pkg-name)
        (package-install pkg-name)))))

(defun init--update-packages ()
  "Update all packages. Assumes (package-refresh-contents) was already called."
  (interactive)
  (save-current-buffer
    (package-list-packages-no-fetch)
    (when (package-menu--find-upgrades)
      (package-menu-mark-upgrades)
      (package-menu-execute))))

(when (fboundp 'package-refresh-contents)
  (condition-case nil
      (init--install-packages)
    (error
     (package-refresh-contents)
     (init--install-packages))))

;;; Eagerly loaded packages
(when (require 'company nil t)
  (global-company-mode))

;; Must enable `dtrt-indent-mode' here rather than with Customize because it
;; requires the mode when setting with Customize, which fails if it is not
;; installed.
(when (fboundp 'dtrt-indent-mode)
  (dtrt-indent-mode t))

(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode)
  (global-set-key (kbd "C--") 'undo-tree-undo)
  (global-set-key (kbd "C-?") 'undo-tree-redo))

(when (fboundp 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(when (fboundp 'ido-mode)
  (ido-mode 1))       ; must run before enabling `ido-ubiquitous-mode'

(when (fboundp 'ido-ubiquitous-mode)
  (ido-ubiquitous-mode))

(when (fboundp 'flx-ido-mode)
  (flx-ido-mode 1)
  (setq gc-cons-threshold 20000000))

(when (and (require 'eshell nil t)
           (require 'em-smart nil t))
  (eshell-smart-initialize))

(when (fboundp 'exec-path-from-shell-initialize)
  (with-demoted-errors
      (exec-path-from-shell-initialize)))

;; (require 'smooth-scrolling nil t)

(when (require 'popwin nil t)
  (popwin-mode 1))

(require 'smartparens-config nil t)

(when (require 'dired-details nil t)
  (dired-details-install))

(require 'projectile nil t)
(when (require 'helm-projectile nil t)
  (helm-projectile-on))

;;; Autoloaded packages
(when (fboundp 'diminish)
  (with-eval-after-load "eldoc" (diminish 'eldoc-mode))
  ;; (with-eval-after-load "paredit" (diminish 'paredit-mode))
  (with-eval-after-load "whitespace" (diminish 'global-whitespace-mode))
  (with-eval-after-load "auto-complete" (diminish 'auto-complete-mode))
  (with-eval-after-load "magit" (diminish 'magit-auto-revert-mode)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(load "scala-import")
(autoload 'typing-test "typing-test" nil t)

(autoload 'vc-git-grep "vc-git" nil t)

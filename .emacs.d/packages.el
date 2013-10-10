;; Initialize the package system early. This is necessary because the
;; following configuration does not assume that any packages are
;; installed. Without initializing early, there would be no way to
;; distinguish between a nonexistent package and a package that hasn't
;; yet been initialized, because `package-installed-p' is undefined
;; until after initialization.
(when (fboundp 'package-initialize)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t))

(defun init--install-packages ()
  "Install core packages."
  (let ((packages-to-install
         '(adaptive-wrap
           auto-complete
           diminish
           dired-details
           exec-path-from-shell
           expand-region
           fill-column-indicator
           flx
           git-commit-mode
           gitconfig-mode
           gitignore-mode
           ido-ubiquitous
           ido-vertical-mode
           magit
           molokai-theme
           paredit
           popwin
           projectile
           smartparens
           smex
           smooth-scrolling
           undo-tree)))
    (dolist (pkg-name packages-to-install)
      (unless (package-installed-p pkg-name)
        (package-install pkg-name)))))

(when (fboundp 'package-refresh-contents)
  (condition-case nil
      (init--install-packages)
    (error
     (package-refresh-contents)
     (init--install-packages))))

;;; Eagerly loaded packages
(when (require 'auto-complete-config nil t)
  (ac-config-default))

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
  (exec-path-from-shell-initialize))

(require 'smooth-scrolling nil t)

(when (require 'popwin nil t)
  (popwin-mode 1))

(require 'smartparens-config nil t)

(when (require 'dired-details nil t)
  (dired-details-install))

;;; Autoloaded packages
(when (fboundp 'diminish)
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  ;; (eval-after-load "paredit" '(diminish 'paredit-mode))
  (eval-after-load "whitespace" '(diminish 'global-whitespace-mode))
  (eval-after-load "auto-complete" '(diminish 'auto-complete-mode)))

(add-to-list 'load-path "~/.emacs.d")
(autoload 'typing-test "typing-test" nil t)

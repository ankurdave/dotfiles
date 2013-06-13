;; Initialize the package system early. This is necessary because the
;; following configuration does not assume that any packages are
;; installed. Without initializing early, there would be no way to
;; distinguish between a nonexistent package and a package that hasn't
;; yet been initialized, because package-installed-p is undefined
;; until after initialization.
(when (fboundp 'package-initialize)
  (package-initialize)
  (setq package-enable-at-startup nil))

(defun package-install-all ()
  "Install the core packages for my customizations."
  (interactive)
  (let ((packages-to-install
         '(undo-tree
           smex
           auto-complete
           adaptive-wrap
           ido-ubiquitous)))
    (dolist (pkg-name packages-to-install)
      (unless (package-installed-p pkg-name)
        (package-install pkg-name)))))

;;; Eagerly loaded packages
(when (require 'auto-complete-config nil t)
  (ac-config-default))

(when (fboundp 'dtrt-indent-mode)
  (dtrt-indent-mode t))

(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode)
  (global-set-key (kbd "C--") 'undo-tree-undo)
  (global-set-key (kbd "C-?") 'undo-tree-redo))

(when (fboundp 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(when (fboundp 'ido-mode)
  (ido-mode 1))

(when (fboundp 'ido-ubiquitous-mode)
  (ido-ubiquitous-mode))

;;; Autoloaded packages
(autoload 'eshell-smart-initialize "em-smart")
(eval-after-load 'eshell
  '(eshell-smart-initialize))

(add-to-list 'load-path "~/.emacs.d")
(autoload 'typing-test "typing-test" nil t)

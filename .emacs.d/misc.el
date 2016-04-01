(with-eval-after-load "quail/latin-ltx"
  (mapc (lambda (pair)
          (quail-defrule (car pair) (cadr pair) "TeX"))
        '(("\\llbracket" "⟦")
          ("\\rrbracket" "⟧")
          ("\\naturals" "ℕ")
          ("\\reals" "ℝ"))))

;; Enable key repeat in OS X Lion
(when (and window-system (fboundp 'ns-set-resource))
  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO"))

;; Avoid *Buffer List* undo history warning; see
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2013-04/msg00497.html
(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

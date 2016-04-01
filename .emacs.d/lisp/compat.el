(when (not (fboundp 'with-eval-after-load))
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))

(when (not (fboundp 'first))
  (defalias 'first #'car))

(when (not (fboundp 'second))
  (defalias 'second #'cadr))

(provide 'compat)

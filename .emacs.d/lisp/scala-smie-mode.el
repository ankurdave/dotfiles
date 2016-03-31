(require 'smie)

(defconst scala-smie:grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((literal)
      (expr1 (literal)
             (expr1 "*" expr1))
      (expr2 (expr1)
             ))
    '((assoc "*") (assoc "+")))))

(defun scala-smie:rules (kind token)
  (message "kind %s, token %s, hanging %s" kind token (smie-rule-hanging-p))
  (pcase (cons kind token)
    (`(:elem . basic) 2)
    (`(:elem . arg) 4)
    ;; (`(:after . "=") 2)
    (`(:before . "+") 2)
    (`(:before . "*") 2)
    (`(:before . ,(or `"(" `"{" `"["))
     (if (smie-rule-hanging-p) (smie-rule-parent)))))

(defvar scala-smie:syntax-table
  (let ((syntab (make-syntax-table)))
    ;; 1. start by reseting the syntax table: only (){}[] are
    ;; parentheses, so all others marked as parentheses in the parent
    ;; table must be marked as symbols, nothing is a punctuation
    ;; unless otherwise stated
    (map-char-table
     #'(lambda (key value)
         (when (or (= (syntax-class value) 4) ; open
                   (= (syntax-class value) 5) ; close
                   (= (syntax-class value) 1)) ; punctuation
           (modify-syntax-entry key "_" syntab)))
     (char-table-parent syntab))

    ;; Below 'space', everything is either illegal or whitespace.
    ;; Consider as whitespace, unless otherwise stated below.
    (modify-syntax-entry '(0 . 32) " " syntab)

    ;; The scala parentheses
    (modify-syntax-entry ?\( "()" syntab)
    (modify-syntax-entry ?\[ "(]" syntab)
    (modify-syntax-entry ?\{ "(}" syntab)
    (modify-syntax-entry ?\) ")(" syntab)
    (modify-syntax-entry ?\] ")[" syntab)
    (modify-syntax-entry ?\} "){" syntab)

    ;; _ is upper-case letter, but will be modified to be symbol
    ;; constituent when in reserved symbol position by
    ;; syntax-propertize-function
    (modify-syntax-entry ?\_ "w" syntab)

    ;; by default all opchars are punctuation, but they will be
    ;; modified by syntax-propertize-function to be symbol
    ;; constituents when a part of varid or capitalid
    (dolist (char (mapcar 'identity "!#%&*+/:<=>?@^|~-\u21D2\u2190")) ;; TODO: Sm, So
      (modify-syntax-entry char "." syntab))

    ;; for clarity, the \ is alone here and not in the string above
    (modify-syntax-entry ?\\ "." syntab)

    ;; scala strings cannot span lines, so we mark
    ;; " as punctuation, but do the real stuff
    ;; in syntax-propertize-function for properly
    ;; formatted strings.
    (modify-syntax-entry ?\" "." syntab)

    ;; backquote is given paired delimiter syntax so that
    ;; quoted ids are parsed as one sexp. Fontification
    ;; is done separately.
    (modify-syntax-entry ?\` "$" syntab)

    ;; ' is considered an expression prefix, since it can
    ;; both start a Symbol and is a char quote. It
    ;; will be given string syntax by syntax-propertize-function
    ;; for properly formatted char literals.
    (modify-syntax-entry ?\' "'" syntab)

    ;; punctuation as specified by SLS
    (modify-syntax-entry ?\. "." syntab)
    (modify-syntax-entry ?\; "." syntab)
    (modify-syntax-entry ?\, "." syntab)

    ;; comments
    ;; the `n' means that comments can be nested
    (modify-syntax-entry ?\/  ". 124b" syntab)
    (modify-syntax-entry ?\*  ". 23n"   syntab)
    (modify-syntax-entry ?\n  "> b" syntab)
    (modify-syntax-entry ?\r  "> b" syntab)

    syntab))

(define-derived-mode scala-smie-mode prog-mode "Scala (SMIE)"
  :syntax-table scala-smie:syntax-table
  (setq-local syntax-propertize-function 'scala-syntax:propertize)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)[ \t]*")
  (setq-local comment-multi-line t)
  (setq-local indent-tabs-mode nil)

  (smie-setup scala-smie:grammar #'scala-smie:rules))

(provide 'scala-smie-mode)

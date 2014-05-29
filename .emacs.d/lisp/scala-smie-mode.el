(require 'smie)
(require 'scala-mode2-syntax)

;; (defun scala-smie:forward-re-if-match (re)
;;   (if (scala-syntax:looking-at re)
;;       (progn
;;         (goto-char (match-end 0))
;;         t)
;;     nil))

(defun scala-smie:forward-token ()
  (if (looking-at "\n")
      ((forward-whitespace) 'nl)
    (smie-default-forward-token)))

(defun scala-smie:forward-token ()
  (let ((pos (point)))
    (scala-syntax:skip-forward-ignorable)
    (cond
     ;; ;; Tokenize newlines as semicolon if necessary
     ;; ((< pos (line-beginning-position)) ; we moved to the next line
     ;;  'nl)
     ;; Tokenize everything else normally
     (t
      (smie-default-forward-token)))))

(defun scala-smie:forward-token-interactive ()
  (interactive)
  (message (prin1-to-string (scala-smie:forward-token))))

(defconst scala-smie:grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc ";")
      (assoc "=")))
))

(defun scala-smie:rules (kind token)
  (message "kind %s, token %s" kind token)
  (pcase (cons kind token)
    (`(:elem . basic) scala-indent:step)
    (`(:elem . arg) 0)
    (`(:before . ";")
     (smie-rule-parent scala-indent:step))
    ;; (`(,_ . ",") (smie-rule-separator kind))
    ;; (`(:before . ,(or `"begin" `"(" `"{"))
    ;;  (if (smie-rule-hanging-p) (smie-rule-parent)))
    ;; (`(:before . "if")
    ;;  (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
    ;;       (smie-rule-parent)))
    ))

(define-derived-mode scala-smie-mode fundamental-mode "Scala (SMIE)"
  :syntax-table scala-syntax:syntax-table
  (scala-mode:make-local-variables
   ;; 'post-self-insert-hook
   'syntax-propertize-function
   'font-lock-syntactic-face-function
   'font-lock-defaults
   ;; 'paragraph-start
   ;; 'paragraph-separate
   ;; 'fill-paragraph-function
   ;; 'adaptive-fill-function
   ;; 'adaptive-fill-first-line-regexp
   'comment-start
   'comment-end
   'comment-start-skip
   'comment-column
   'comment-multi-line
   ;; 'forward-sexp-function
   ;; 'indent-line-function
   ;; 'indent-tabs-mode
   ;; 'join-line
   )

  (add-hook 'syntax-propertize-extend-region-functions
            'scala-syntax:propertize-extend-region)

  (setq scala-mode:debug-messages       nil

        syntax-propertize-function      'scala-syntax:propertize
        parse-sexp-lookup-properties    t

        ;; TODO: font-lock
        font-lock-defaults              '(scala-font-lock:keywords
                                          nil)
        font-lock-syntactic-face-function 'scala-font-lock:syntactic-face-function

        ;; TODO: beginning-of-defun-function, end-of-defun-function

        ;; comments
        ;; paragraph-start                 scala-paragraph:paragraph-start-re
        ;; paragraph-separate              scala-paragraph:paragraph-separate-re
        ;; fill-paragraph-function         'scala-paragraph:fill-paragraph
        ;; adaptive-fill-function          'scala-paragraph:fill-function
        ;; adaptive-fill-first-line-regexp scala-paragraph:fill-first-line-re
        comment-start                   "// "
        comment-end                     ""
        comment-start-skip              "\\(//+\\|/\\*+\\)[ \t]*"
        comment-column                  0
        comment-multi-line              t

        ;; forward-sexp-function           'scala-mode:forward-sexp-function
        ;; indent-line-function            'scala-indent:indent-line
        indent-tabs-mode                nil
        ;; join-line                       'scala-indent:join-line
        )

  (use-local-map scala-mode-map)

  (smie-setup scala-smie:grammar 'scala-smie:rules))

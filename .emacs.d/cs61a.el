(setq scheme-program-name "stk-simply")
(defun run-stk () "
   Remove the *scheme* buffer unless it is running Stk.  If there is no
   *scheme* buffer running Stk, create one.  Switch to the *scheme* buffer."
   (interactive)
   (set-buffer (get-buffer-create "*scheme*"))
   (let ((proc (get-buffer-process "*scheme*")))
     (if (and proc (not (string-match 
			 "stk$" (car (process-command proc)))))
	 (progn 
	   (set-process-buffer proc nil)
	   (kill-process proc))))
   (run-scheme "stk-simply"))

(defun run-half-scheme () "
   Run Scheme in half a window."
   (interactive)
   (split-window-vertically nil)
   (other-window 1)
   (call-interactively 'run-scheme))

;; Additional local key and menu definitions in Scheme mode.

(define-key scheme-mode-map [menu-bar scheme]
  (cons "Scheme" (make-sparse-keymap "Scheme")))

(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-load-file-and-go] '("Send Scheme File & Go" . scheme-load-file-and-go))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-load-file] '("Send Scheme File" . scheme-load-file))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-send-region-and-go] '("Send Region & Go". scheme-send-region-and-go))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-send-region] '("Send Region" . scheme-send-region))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-send-defn-and-go]
  '("Send Definition & Go" . scheme-send-definition-and-go))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-send-defn] '("Send Definition" . scheme-send-definition))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-indent-sexp] '("Indent S-expression" . scheme-indent-sexp))
(setq menu-bar-final-items (cons 'scheme menu-bar-final-items))


(define-key scheme-mode-map "\C-c\M-l" 'scheme-load-file-and-go)
(define-key scheme-mode-map "\r" 'newline-and-indent)

(defun scheme-load-file-and-go (file-name)
  "Load Scheme file FILE-NAME into the inferior Scheme process and then 
go to Scheme buffer."
  (interactive (comint-get-source "Load Scheme file: " scheme-prev-l/c-dir/file
				  scheme-source-modes t)) ; T because LOAD 
                                                          ; needs an exact name
  (scheme-load-file file-name)
  (switch-to-scheme t))

(add-hook 'scheme-mode-hook
	  (function
	   (lambda ()
	     (setq comment-start ";; "))))

(defun scheme-send-enclosing-definition () "
  Send the definition containing point to the *scheme* process."
  (interactive)
  (forward-char 7)
  (search-backward-regexp "^(define")
  (forward-sexp)
  (scheme-send-last-sexp)
  (if (not (null (search-forward "(define" nil t)))
    (backward-char 7)))

(global-set-key "\M-p"          'scheme-send-enclosing-definition)
(global-set-key "\M-s"          'run-half-scheme)
(define-key esc-map "\C-q"	'scheme-indent-sexp)

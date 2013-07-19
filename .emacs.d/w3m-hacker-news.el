(defun beginning-of-hn-comment ()
  "Move to the beginning of the Hacker News comment at point in w3m."
  (interactive)
  (condition-case nil
      (progn
        (search-backward "| link")
        (beginning-of-line 2)
        (point))
    (search-failed (point-min))))

(defun end-of-hn-comment ()
  "Move to the end of the Hacker News comment at point in w3m."
  (interactive)
  (condition-case nil
      (progn
        (search-forward "| link")
        (beginning-of-line)
        (point))
    (search-failed (point-max))))

(defun dim-all-but-hn-comment-at-point ()
  "Dim other Hacker News comments in w3m."
  (interactive)
  (let ((dim '(font-lock-face (:foreground "DimGrey")))
        (comment-start (save-excursion (beginning-of-hn-comment)))
        (comment-end (save-excursion (end-of-hn-comment))))
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) dim)
      (add-text-properties (point-min) comment-start dim)
      (add-text-properties comment-end (point-max) dim))))

(defun narrow-to-hn-comment-at-point ()
  "Narrow to the Hacker News comment at point in w3m."
  (interactive)
  (widen)
  (narrow-to-region (save-excursion (beginning-of-hn-comment))
                    (save-excursion (end-of-hn-comment))))

(defcustom hn-comment-narrow-function nil
  "Whether or not the HN comment functions should narrow to the
current comment."
  :type '(choice (function-item narrow-to-hn-comment-at-point)
                 (function-item dim-all-but-hn-comment-at-point)
                 (const nil)))

(defun next-hn-comment ()
  "Move to the next Hacker News comment in w3m."
  (interactive)
  (widen)
  (condition-case nil
      (progn
        (search-forward-regexp "^\\s-*reply\\s-*$")
        (beginning-of-line)
        (when hn-comment-narrow-function
          (funcall hn-comment-narrow-function)
          (recenter -1))
        t)
    (search-failed nil)))

(defun previous-hn-comment ()
  "Move to the previous Hacker News comment in w3m."
  (interactive)
  (widen)
  (condition-case nil
      (progn
        (search-backward-regexp "| link\\s-*$")
        (beginning-of-line)
        (when hn-comment-narrow-function
          (funcall hn-comment-narrow-function)
          (recenter -1))
        t)
    (search-failed nil)))

(defun hn-comment-indentation-level ()
  "Return the indentation level of the Hacker News comment at point in w3m."
  (save-excursion
    (beginning-of-hn-comment)
    (forward-line)
    (search-forward "[s]" (line-end-position) t) ; skip [s] images
    (while (looking-at " ")
      (forward-char))
    (current-column)))

(defun parent-hn-comment ()
  "Move to the parent Hacker News comment in w3m."
  (interactive)
  (widen)
  (condition-case nil
      (progn
        (let ((start-indentation-level (hn-comment-indentation-level))
              (previous-comment-position (point)))
          (let ((hn-comment-narrow-function nil))
            (while (and (>= (hn-comment-indentation-level) start-indentation-level)
                        (previous-hn-comment))))
          (when hn-comment-narrow-function
            (funcall hn-comment-narrow-function)
            (recenter -1))
          (push-mark previous-comment-position t)
          (message "Mark set to previous comment")))
    (search-failed nil)))

(defun next-sibling-hn-comment ()
  "Move to the next sibling Hacker News comment in w3m."
  (interactive)
  (widen)
  (condition-case nil
      (progn
        (let ((start-indentation-level (hn-comment-indentation-level))
              (previous-comment-position (point)))
          (let ((hn-comment-narrow-function nil))
            (next-hn-comment)
            (while (and (> (hn-comment-indentation-level) start-indentation-level)
                        (next-hn-comment))))
          (when hn-comment-narrow-function
            (funcall hn-comment-narrow-function)
            (recenter -1))
          (push-mark previous-comment-position t)
          (message "Mark set to previous comment")))
    (search-failed nil)))

(provide 'w3m-hacker-news)

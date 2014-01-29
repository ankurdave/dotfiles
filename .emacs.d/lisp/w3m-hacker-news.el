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

(defun narrow-to-hn-comment-at-point ()
  "Narrow to the Hacker News comment at point in w3m."
  (interactive)
  (narrow-to-region (save-excursion (beginning-of-hn-comment))
                    (save-excursion (end-of-hn-comment))))

(defun next-hn-comment ()
  "Move to the next Hacker News comment in w3m.
Return t if movement succeeded, and nil otherwise."
  (interactive)
  (let ((narrow (buffer-narrowed-p)))
    (when narrow
      (widen))
    (condition-case nil
        (progn
          (search-forward-regexp "^\\s-*reply\\s-*$\\| points?written ")
          (when narrow
            (narrow-to-hn-comment-at-point)
            (recenter -1))
          t)
      (search-failed nil))))

(defun previous-hn-comment ()
  "Move to the previous Hacker News comment in w3m.
Return t if movement succeeded, and nil otherwise."
  (interactive)
  (let ((narrow (buffer-narrowed-p)))
    (when narrow
      (widen))
    (condition-case nil
        (progn
          (search-backward-regexp "| link\\s-*$\\| points?written ")
          (beginning-of-line)
          (when narrow
            (narrow-to-hn-comment-at-point)
            (recenter -1))
          t)
      (search-failed nil))))

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
  "Move to the parent Hacker News comment in w3m.
Return t if movement succeeded, and nil otherwise."
  (interactive)
  (let ((narrow (buffer-narrowed-p)))
    (when narrow
      (widen))
    (condition-case nil
        (progn
          (let ((start-indentation-level (hn-comment-indentation-level))
                (previous-comment-position (point)))
            (let ((hn-comment-narrow-function nil))
              (while (and (>= (hn-comment-indentation-level)
                              start-indentation-level)
                          (previous-hn-comment))))
            (when narrow
              (narrow-to-hn-comment-at-point)
              (recenter -1))
            (push-mark previous-comment-position t)
            (message "Mark set to previous comment")
            t))
      (search-failed nil))))

(defun next-sibling-hn-comment ()
  "Move to the next sibling Hacker News comment in w3m.
Return t if movement succeeded, and nil otherwise."
  (interactive)
  (let ((narrow (buffer-narrowed-p)))
    (when narrow
      (widen))
    (condition-case nil
        (progn
          (let ((start-indentation-level (hn-comment-indentation-level))
                (previous-comment-position (point)))
            (next-hn-comment)
            (while (and (> (hn-comment-indentation-level) start-indentation-level)
                        (next-hn-comment)))
            (when narrow
              (narrow-to-hn-comment-at-point)
              (recenter -1))
            (push-mark previous-comment-position t)
            (message "Mark set to previous comment")
            t))
      (search-failed nil))))

(provide 'w3m-hacker-news)

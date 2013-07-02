(defun beginning-of-hn-comment ()
  "Move to the beginning of the Hacker News comment at point in w3m."
  (interactive)
  (search-backward "| link")
  (beginning-of-line 2)
  (point))

(defun end-of-hn-comment ()
  "Move to the end of the Hacker News comment at point in w3m."
  (interactive)
  (search-forward "| link")
  (beginning-of-line)
  (point))

(defun dim-all-but-hn-comment-at-point ()
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
  (search-forward-regexp "^\\s-*reply\\s-*$")
  (beginning-of-line)
  (when hn-comment-narrow-function
    (funcall hn-comment-narrow-function)))

(defun previous-hn-comment ()
  "See `next-hn-comment'."
  (interactive)
  (widen)
  (search-backward-regexp "| link\\s-*$")
  (beginning-of-line)
  (when hn-comment-narrow-function
    (funcall hn-comment-narrow-function)))

(defun parent-hn-comment ()
  "Narrow to the parent Hacker News comment in w3m."
  (interactive)
  (widen)
  ;; Go to first line of this comment
  (search-backward "| link")
  (move-beginning-of-line 3)
  (search-forward "[s]" (line-end-position) t) ; skip [s] images
  (while (looking-at " ")
    (forward-char))
  ;; Move backward until we reach the body text of the parent comment (i.e.,
  ;; while we are either in indentation or in a comment header)
  (while (or (equal (preceding-char) ?\s)
             (save-excursion
               (search-forward "| link " (line-end-position) t)))
    (previous-line))
  (when hn-comment-narrow-function
    (funcall hn-comment-narrow-function)))

(defun next-sibling-hn-comment ()
  "Narrow to the next sibling Hacker News comment in w3m."
  (interactive)
  (widen)
  ;; Go to last line of this comment
  (search-forward "| link")
  (move-beginning-of-line -1)
  (search-forward "[s]" (line-end-position) t) ; skip [s] images
  (while (looking-at " ")
    (forward-char))
  (next-line)
  ;; Move forward until we reach the body text of the next sibling comment
  ;; (i.e., while we are either in indentation or in a comment header)
  (while (or (equal (following-char) ?\s)
             (save-excursion
               (search-forward "| link " (line-end-position) t)))
    (next-line)
    (sit-for 0)  ; Avoid strange behavior where we skip past the sibling comment
    )
  (when hn-comment-narrow-function
    (funcall hn-comment-narrow-function)))

(provide 'w3m-hacker-news)

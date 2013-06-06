;; typing-test.el 7/17/2004, Steve Yegge (stevey@amazon.com)
;;
;; Stevey's Typing Tester.  For people who want to take a typing test
;; within Emacs, so you know how fast your net speed is under normal
;; editing circumstances.
;;
;; To install:
;;  - put the directory where you unzipped these files into your load-path,
;;    e.g. (add-to-list 'load-path "~/emacs/ttest")
;;  - add the following line anywhere in your .emacs:
;;
;;    (autoload 'typing-test "typing-test" nil t)
;;
;; To take a typing test:
;;  - type M-x typing-test
;;  - your Emacs screen will split into multiple windows for the test:
;;    - an instructions-window showing basic help
;;    - a content window containing the text you'll be typing
;;    - a status window showing time remaining and current WPM
;;    - a typing area where you demonstrate your studliNess
;;  - As soon as you start typing, the timer starts.  When it's over,
;;    the buffer becomes read-only, and your results are displayed.
;;
;; Customizations provided:
;;  - you can select from different content passages, or use your own
;;  - you can customize the test duration
;;  - you can choose to have auto-fill on or off, and set the fill column
;;
;; Notes:
;;  - unlike many online typing tests, this test doesn't prevent you
;;    from going back and fixing mistakes as you go.  You get to type
;;    just you would under normal circumstances.
;;  - but using dabbrev-expand or copy/paste is cheating. ;-)
;;  - type M-x typing-test to start the test over at any time
;;
;; Computing words per minute:
;;
;; The universal standard is to compute gross words per minute (WPM)
;; by dividing total characters typed per minute by 5.  I think most
;; of them include the whitespace chars you typed, but this one
;; doesn't.  Net wpm is computed by subtracting your error count
;; from your gross wpm.
;;
;; Detecting errors:
;;
;; There are various ways to make typing mistakes: inserted words,
;; skipped words, misspelled words, joined words, split words, and
;; multiple errors in a row.
;;
;; Most online typing tests deal very poorly with skipped or added
;; words: They consider everything afterwards to be an error.  I'm not
;; sure if this is an industry standard definition, or lazy coding.
;; They also barf if you split a word, or join more than 2 words.
;;
;; Their error algorithm can basically only recognize:
;;  - sequences of misspellings, by skipping pairs until a match is found
;;  - single joins, by special-casing it (probably strcmp w/ car + cadr)
;;
;; We don't want the lame algorithm:  one missed/added/split word and
;; the whole test is ruined.  The classic diff algorithm would work,
;; but is serious overkill.  Instead we'll do a simplified diff that
;; relies on locality of mistakes; we assume the user gets back on
;; track relatively quickly.  Degenerate worst-case is n^2, but very
;; unlikely to happen; the user would have to insert an extra word
;; after every correctly-typed word.
;;
;; Whenever we find a mismatch, we skip forward in both lists until we
;; find a word common to both lists (or run out of words).  We count
;; intervening skipped/joined/split/inserted/misspelled words as
;; errors, using rules I made up, but they seem reasonable. This
;; algorithm is more robust and accurate than the one used by online
;; typing tests.
;;
;; (Note:  some people might feel that a word-join is one error,
;; not two; that could eventually be a customization option.)
;;
;; Examples:
;;
;;  Content:   I am a very fast typist, and enjoy typing a lot.
;;  User #1:   I am a veryfast typist, and enjoytypinga lot.
;;                    ^^^^^^^^             ^^^^^^^^^^^^
;;
;;  In the example above, they joined 2 words, then 3 words, for
;;  a total of 5 errors.  Their screwups (^^^) are shown in red.
;;  The count comes from content words skipped to get to `typist'
;;  and `lot', when we encountered differences.
;;
;;  Content:   I am a very fast typist, and enjoy typing a lot.
;;  User #2:   I am a vry fast typist, enjoy typing a lot.
;;                    ^^^
;;
;;  Here they left out `and' and `a', and misspelled `very', for a
;;  total of 3 errors.  Only one error is actually highlighted, but
;;  all three are counted in their score.
;;
;;  Content:   I am a very fast typist, and enjoy typing a lot.
;;  User #3:   I am a very very fast typist, and enjoy typing a lot.
;;                         ^^^^
;;
;;  They've inserted a word, so it's highlighted and counted as an
;;  error in their score.  This is the only case in which we use
;;  the skipped user-text length for counting errors; we know it's
;;  inserted words because the skipped content is nil.
;;
;; One oddball edge case:  the user typed in more words than there
;; were in the original content - i.e. some extra stuff at the end.
;; We flag each extra word at the end as an extra error.
;;
;; You can run (ttest-run-unit-tests) to see examples of how the
;; error scoring is done.
;;
;; TODO:
;;  - turn defvars into defcustoms
;;  - need a timer display, so you can see how much time you've got left
;;  - highlighting is still screwy in some cases; doesn't match real diffs
;;  - test edge cases for highlighting (maybe try working into unit tests)
;;  - word-joins and word-splits should really count as one error each
;;  - print some qualitative analysis of speed and accuracy in results

(require 'cl)

(defvar tt-instructions
  "                 STEVEY'S TYPING TEST:  INSTRUCTIONS

Start typing the text below to start the test.  When your time is up,
the test will stop automatically.  Type M-x ttest-help for more details."
  "Instructions to put in the top of the content buffer.")

;; set to short value (5 or 10) while debugging, or nil not to use timer
(defvar tt-duration-seconds 10 "The length of the typing test.")

(defvar tt-current-content "huck-finn.txt"
  "Path to file containing contents for the next typing test.
If no directory is specified, looks in load-path.")

(defvar tt-use-auto-fill t
  "Set to nil if you don't want the text to auto-wrap as you type.")

(defvar tt-show-expected-word t
  "Set to nil if you don't want test to underline next expected word.")

(defvar tt-fill-column 65
  "Column to wrap the text at, if tt-use-auto-fill is on.")

(defvar tt-test-area-buffer-name "*Typing Test Typing Area*"
  "Name of the typing buffer for the Typing Test.")

(defvar tt-content-area-buffer-name "*Typing Test Content*"
  "Name of the content buffer for the Typing Test.")

(defvar tt-instruction-buffer-name "*Typing Test Instructions*"
  "Name of the instructions buffer for the Typing Test.")

(defvar tt-results-buffer-name "*Typing Test Results*"
  "Name of the typing-test buffer for showing test results.")

(defvar tt-debug-unit-tests nil
  "Typing-test internal debugging variable - t to turn on
detailed output during unit testing.")

(defvar tt-start-time 0       "For internal use by typing test.")
(defvar tt-end-time 0 	      "For internal use by typing test.")
(defvar tt-timer-variable nil "For internal use by typing test.")

;; beginning and end of word user's currently expected to type
(defvar tt-expected-start 1   "For internal use by typing test.")
(defvar tt-expected-end   1   "For internal use by typing test.")
(defvar tt-overlay nil	      "For internal use by typing test.")

(defvar tt-typing-diffs nil   "For internal use by typing test.")

;;-----------------------------------------------------------------------------
;; Help
;;-----------------------------------------------------------------------------

(defvar tt-help-text
  "Welcome to the Cheesy (yet somehow cool) Emacs Typing Test.
The idea is to see how fast you *really* type when you have
access to your b-loved macros, shortcuts and so on.

During the typing test, your screen is split into 3 windows:
a short instructions area, a content area, and a typing area.
The test lasts 60 seconds, or whatever you've set the variable
`tt-duration-seconds' to.  To start the timer, just start typing.

As you type, the next word you should type is underlined.  If
you get onto the wrong word, you can either fix your mistakes,
or just make sure your next word is the expected one, so you're
back in sync with the test.

When your time is up, your typing buffer will become read-only,
and the test will display your results.  You can type `q' in
any typing-test buffer, or M-x ttest-quit, to leave the test.
Type M-x typing-test at any time to start over from scratch.")

(defun ttest-help ()
  "Shows detailed help for the typing test."
  (interactive)
  (with-output-to-temp-buffer "*Typing Test Help*"
    (save-excursion
      (set-buffer standard-output)
      (insert tt-help-text))))

;;-----------------------------------------------------------------------------
;; Test initialization, and utility commands for setting variables
;;-----------------------------------------------------------------------------

(defun typing-test ()
  "Starts a fresh typing test, using tt-current-content."
  (interactive)

  (let ((content-buf (tt-make-content-buffer))
	(typing-buf (tt-make-typing-buffer))
	(instruct-buf (tt-make-instructions-buffer)))

    (setq tt-typing-diffs nil) ;; clear cache
    (pop-to-buffer typing-buf)
    (delete-other-windows)
    (split-window-vertically)  ;; cursor goes to top window
    (split-window-vertically)
    (show-buffer (selected-window) instruct-buf)
    (fit-window-to-buffer)
    (other-window 1)
    (show-buffer (selected-window) content-buf)
    (other-window 1))
  (message "Start typing to begin the test."))

(defun typing-test-set-duration (sec)
  "Sets the test duration, in seconds.  Not persistent.
Use `customize-variable' to set its value more permanently."
  (interactive "nLength of typing test in seconds (-1 to disable timer): ")
  (if (< sec 0)
      (progn
	(setq tt-duration-seconds nil)
	(message "Tests this session will now be untimed."))
    (progn
      (setq tt-duration-seconds sec)
      (message (format "Tests will run for %d seconds for this session." sec)))))

(defun typing-test-set-content (file)
  "Sets the content file to use for the test to FILE.  Not persistent.
Use `customize-variable' to set its value more permanently."
  (interactive "fExisting file to use for test content: ")
  (setq tt-current-content file)
  (message (format "Test content set to %s for current session." file)))

;;-----------------------------------------------------------------------------
;; Buffer setup and management
;;-----------------------------------------------------------------------------

(defun tt-insert-content (filename)
  "Insert contents of FILENAME, searching load path for FILENAME
if the full path isn't specified."
  (if (file-exists-p filename)
      (insert-file-contents filename)
    (let ((paths (copy-list load-path)))
      (while paths
	(let ((file (concat (pop paths) "/" filename)))
	  (when (file-exists-p file)
	    (insert-file-contents file)
	    (setq paths nil)))))))

(defun tt-make-read-only (buf)
  (save-excursion
    (set-buffer buf)
    (toggle-read-only 1)))

(defun tt-make-writeable (buf)
  (save-excursion
    (set-buffer buf)
    (toggle-read-only -1)))

(defun tt-make-buffer (name content &optional direct)
  "Create a read-only buffer called NAME and puts the specified content in it.
CONTENT is considered a filename unless DIRECT is non-nil, in which case
CONTENT is inserted directly."
  (let ((buf (get-buffer-create name)))
    (save-excursion
      (tt-make-writeable buf)
      (switch-to-buffer buf)
      (erase-buffer)
      (if direct
	  (insert content)
	(tt-insert-content content))
      (beginning-of-buffer)
      (set-buffer-modified-p nil)
      (tt-make-read-only buf)
      buf)))

(defun tt-make-instructions-buffer ()
  "Create the buffer for displaying the test instructions."
  (let ((buf (tt-make-buffer tt-instruction-buffer-name
			     tt-instructions t))
	(start))
    (save-excursion
      (tt-make-writeable buf)
      (switch-to-buffer buf)
      (ttest-content-mode)
      (beginning-of-buffer)
      (skip-syntax-forward "\\s-")
      (setq start (point))
      (end-of-line)
      (put-text-property start (point) 'face '((:foreground "firebrick")))
      (beginning-of-buffer)
      (set-buffer-modified-p nil)
      (tt-make-read-only buf))
    buf))

(defun ttest-quit ()
  "Kills all the typing-test buffers."
  (interactive)
  (tt-stop-timer)
  (let (nuke-buf)
    (fset 'nuke-buf (lambda (buf) (if buf (kill-buffer buf))))
    (nuke-buf (tt-get-typing-buffer))
    (nuke-buf (tt-get-content-buffer))
    (nuke-buf (tt-get-instructions-buffer))
    (nuke-buf (tt-get-results-buffer)))
  (delete-other-windows)
  (message "Stevey's typing test:  See you next time!"))

(defun tt-make-content-buffer ()
  "Create a read-only buffer with instructions and content."
  (let ((buf (tt-make-buffer tt-content-area-buffer-name
			     tt-current-content)))
    (save-excursion
      (switch-to-buffer buf)

      ;; I'd like the box cursor to be hidden when the window isn't
      ;; focused, but show the cursor when the user switches to this
      ;; window.  Hmmm...
      ;(set (make-local-variable 'cursor-type) nil)

      (ttest-content-mode)
      (and tt-show-expected-word (tt-highlight-first-word)))
    buf))

(defun tt-make-typing-buffer ()
  "Create buffer that you type into while taking the test."
  (let ((buf (get-buffer-create tt-test-area-buffer-name)))
    (save-excursion
      (tt-make-writeable buf)
      (switch-to-buffer buf)
      (erase-buffer)
      (ttest-mode)
      buf)))

(defun tt-get-typing-buffer ()
  "Return the typing test 'test area' buffer, or nil if not found."
  (get-buffer tt-test-area-buffer-name))

(defun tt-get-content-buffer ()
  "Return the typing test 'content/results area' buffer, or nil if not found."
  (get-buffer tt-content-area-buffer-name))

(defun tt-get-instructions-buffer ()
  "Return the typing test 'instructions area' buffer, or nil if not found."
  (get-buffer tt-instruction-buffer-name))

(defun tt-get-results-buffer ()
  "Return the typing test 'results display area' buffer, or nil if not found."
  (get-buffer tt-results-buffer-name))

;;-----------------------------------------------------------------------------
;; Major modes for typing area and static-content areas
;;-----------------------------------------------------------------------------

(defvar ttest-mode-hook nil)
(defvar ttest-mode-map (copy-keymap text-mode-map)
  "Keymap used in typing-test typing buffer.")

(defun ttest-mode ()
  "Major mode for the Typing Test typing-area buffer."
  (interactive)
  (text-mode)
  (setq major-mode 'ttest-mode)
  (setq mode-name "Typing Test")
  (use-local-map ttest-mode-map)
  (set-buffer-modified-p nil)
  (add-hook 'first-change-hook 'ttest-start nil t)
  (and tt-use-auto-fill
       (setq fill-column tt-fill-column)
       (auto-fill-mode 1))
  (tt-install-change-hooks)
  (tt-avoid-dabbrev-cheat)
  (run-hooks 'ttest-mode-hook))

(defvar ttest-content-mode-hook nil)
(defvar ttest-content-mode-map (copy-keymap text-mode-map)
  "Keymap used in typing-test content buffers.")
(define-key ttest-content-mode-map [?q] 'ttest-quit)
(define-key ttest-content-mode-map "\C-t" 'ttest-restart)

(defvar ttest-word-syntax-table (copy-syntax-table text-mode-syntax-table))

;; Change the syntax table in the typing test so that punctuation
;; is considered part of a word, for purposes of error counting."
(with-syntax-table ttest-word-syntax-table
  (loop for i from 0 to 255 do
	(if (= (char-syntax i) ?.)
	    (modify-syntax-entry i "w" ttest-word-syntax-table))))
		       
(defun ttest-content-mode ()
  "Major mode for the Typing Test content and instructions buffers."
  (interactive)
  (text-mode)
  (use-local-map ttest-content-mode-map)
  (setq major-mode 'ttest-content-mode)
  (set-syntax-table ttest-word-syntax-table)
  (setq mode-name "Typing Test Content")
  (run-hooks 'ttest-content-mode-hook))

(defun ttest-restart ()
  "Starts a new typing test."
  (interactive)
  (ttest-quit)
  (typing-test))

(defun ttest-start ()
  "Start the typing-test timer on first keystroke."
  (tt-start-timer)
  (message "Timer started!"))

(defun tt-avoid-dabbrev-cheat ()
  "Makes it so that dabbrev-expand only looks in the
current buffer for possible expansions, so you only get
the benefit, during the typing test, of words you've
already successfully typed in this session."
  (make-local-variable 'dabbrev-expand)
  (setq dabbrev-expand
	(lambda (buf)
	  (cond
	   ((not (bufferp buf))
	    (error "%s is not a buffer" buf))
	   (t (string= (buffer-name buf)
		       tt-test-area-buffer-name))))))

(defun tt-install-change-hooks ()
  "Registers a function to call after textual changes are
made to the typing buffer.  Handles checking accuracy as you go."
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 'tt-typing-callback))

;; this algorithm is getting better; it's very good now about waiting
;; for you to catch up, if you get behind.  It still doesn't handle
;; things well if you get too far ahead, though.  Right now it just
;; handles the case where you got one word ahead, by checking, after
;; it advances the overlay, if it's now on the word you just typed,
;; and if so, advances again.  (That breaks for doubled words in the
;; content, which is fixable by checking the content for that case.)
;;
;; To check properly if they've gotten ahead, e.g. by joining several
;; words in a row, or simply skipping some words, we might do any of
;; the following:
;;
;;  - look ahead in the content for the just-typed word, and if we
;;    find it, jump 1 word ahead of that.  Could restrict to just-typed
;;    words of 5 chars or longer, to minimize risk of accidental match.
;;
;;  - harder, but could look for just-typed word -and- word before it
;;    up ahead in the content.  2 or 3 words in a row improves chances
;;    that we really are behind.
;;
;; You can easily get ahead by joining 3 or more words (leaving spaces
;; out) or by simply skipping words.

(defun tt-typing-callback (start end len)
  "Function called after the text of the buffer changes.
START is where the change began, END where it ended, and LENGTH
is zero for insertions, or size of deletion.  Moves the content
buffer's expected-word overlay."

  (let* ((change (buffer-substring-no-properties start end))
	 ;; count any whitespace or newline as whitespace
	 (typed-space (string-match "^\\(\\s-\\|\n\\|\r\\)+$" change))
	 (at-end (tt-eobp)))

    (cond
     ((and typed-space (tt-content-exhausted))
      (tt-timer-expired) t)

     ((not tt-show-expected-word) nil) ;; user option:  no overlay

     ((tt-maybe-advance-overlay typed-space at-end) t)

     (t
      (if (and (not typed-space)
	       (not (eq start end))) ;; do nothing on text deletions
	  (setq tt-moved-word nil))))))

(defun tt-maybe-advance-overlay (typed-space at-end)
  "Checks if user is ahead or behind, and possibly advances the
overlay to the next expected word.  Returns t if overlay moved."

  (let* ((word-just-typed (current-word))
	 (initial-expected-word (tt-expected-word))
	 (initial-previous-word (tt-word-before-overlay))
	 (was-one-word-behind (string= word-just-typed initial-previous-word)))
    (when
	(and
	 ;; basic checks for moving it:  typed space at eob, basically
	 at-end typed-space (not tt-moved-word)
       
	 ;; if they just typed the word before the overlay, assume they were
	 ;; 1 word behind, and are caught up now, so don't move the overlay
	 (not was-one-word-behind)

	 ;; check if they're even further behind (typed length < cur spot)
	 (not (< (buffer-size (tt-get-typing-buffer)) tt-expected-start)))

      ;; no need to wait for them to catch up.  Move overlay to next word.
      (tt-highlight-next-word)

      ;; check if they're 1 word ahead, by seeing if current == expected.
      ;; if so, jump highlight ahead one word.
      (cond
       ;; if they just typed the expected word, we're done.
       ((string= (current-word) initial-expected-word) 
	'done)

       ;; check 1 word ahead
       ((string= (current-word) (tt-expected-word))
	(tt-highlight-next-word))

       (t
	;; the way-ahead check is still a bit broken, and frequently
	;; jumps too far ahead in the stream, getting very confused.
	;; disabling for now.
	nil))
	;; otherwise check if they're far ahead
	;(tt-check-user-way-ahead)))

      ;; don't let multiple space chars keep advancing the overlay
      (setq tt-moved-word t))))

(defun tt-check-user-way-ahead ()
  "Checks if user has typed far ahead of expected-word overlay.
If so, advances the overlay to the correct location."

  ;; we look at the word the user just typed.  if it's longer than
  ;; 5 chars (meaning more likely unique), and we see it -ahead- of
  ;; our current expected word position in the content, then we'll
  ;; assume they're ahead, and advance to that point in the content.

  (let ((just-typed (current-word)))
    (if (> (length just-typed) 5)
	(let (found)
	  (save-excursion
	    (set-buffer (tt-get-content-buffer)) ;; look in content
	    (setq found
		  (string-match
		   (concat "\\b" just-typed "\\b")
		   (buffer-substring-no-properties
		    tt-expected-end ;; start looking beyond overlay
		    (point-max)))) ;; through end of content buffer

	    ;; if we found user's just-typed long word, move overlay
	    ;; to the word AFTER the just-typed word, in content buf
	    (when found
	      (goto-char (+ tt-expected-end found))
	      (forward-word 1)
	      (setq tt-expected-end (point))
	      (tt-highlight-next-word)))))))

(defun tt-content-exhausted ()
  "Checks if we just typed the last word of the test.
Only works if overlay-following is turned on."
  (save-excursion
    (set-buffer (tt-get-content-buffer))
    (goto-char tt-expected-end)
    (tt-eobp)))

;;-----------------------------------------------------------------------------
;; Timer functions
;;-----------------------------------------------------------------------------

(defun tt-start-timer ()
  "Starts timer to go off in 60 seconds, or however long
is specified in tt-duration-seconds."
  (when tt-duration-seconds ;; set to nil for debugging => no timer
    (setq tt-start-time (float-time))
    (setq tt-timer-variable
	  (run-at-time tt-duration-seconds
		       nil ;; no repeats
		       'tt-timer-expired))))

(defun tt-timer-running ()
  "Function that tells if the timer is running."
  tt-timer-variable)

(defun tt-stop-timer ()
  "Stops the typing-test timer, if running."
  (when (tt-timer-running)
    (cancel-timer tt-timer-variable)
    (setq tt-timer-variable nil)))

(defun tt-timer-expired ()
  "Stops typing test and prints out your results."
  (tt-stop-timer)
  (when (tt-get-typing-buffer) ;; do nothing if buffer was killed
    (tt-stop-test)
    (setq tt-end-time (float-time))
    (tt-display-results)))

(defun tt-stop-test ()
  "Marks typing-test typing buffer as read only when timer expires."
  (let ((tbuf (tt-get-typing-buffer)))
    (when tbuf
      (tt-make-read-only tbuf)
      (save-excursion
	(set-buffer tbuf)
	(use-local-map ttest-content-mode-map)))))

;;-----------------------------------------------------------------------------
;; Score display
;;-----------------------------------------------------------------------------

(defun tt-display-results ()
  "Shows how well you did in the typing test."
  (let* ((buf (tt-get-typing-buffer))
	 (win (if buf (get-buffer-window buf) nil)))
    (and
     buf win
     (progn
       (tt-highlight-errors)
       (select-window win)
       (split-window-vertically)
       (with-output-to-temp-buffer tt-results-buffer-name
	 (save-excursion
	   (set-buffer standard-output)
	   (insert (tt-format-results))))
       (let ((tbuf (get-buffer tt-results-buffer-name)))
	 (set-buffer tbuf)
	 (tt-results-mode)
	 (select-window (get-buffer-window tbuf))
	 (toggle-read-only 1)
	 (beginning-of-buffer))))))

(defun tt-results-mode ()
  "Major mode for Typing Test results buffer."
  (interactive)
  (text-mode)
  (use-local-map ttest-content-mode-map)
  (setq major-mode 'ttest-results-mode)
  (setq mode-name "Typing Test Results")
  (run-hooks 'ttest-content-mode-hook))

(defun tt-format-results ()
  "Computes, formats, and returns your typing test results."
  (concat
   (format "Your results:\n\n")
   (format "Total time:	%d seconds\n"		 	   (tt-get-total-time))
   (format "Words typed:	%.2f (5 chars per word)\n" (tt-get-total-words))
   (format "Chars typed:	%d\n" 			   (tt-get-total-chars))
   (format "Gross WPM:	%.2f\n"	 		 	   (tt-compute-gross-wpm))
   (format "Chars per min:	%.2f\n" 		   (tt-compute-cpm))
   (format "Errors:		%d\n" 		 	   (tt-get-num-errors))
   (format "Net WPM:	%d\n"	 		 	   (tt-compute-net-wpm))
   (format "Accuracy:	%.2f%%\n"	 		   (tt-compute-accuracy))
   (format "\nType 'Ctrl-t', or M-x typing-test, to start over.\n")))

;;-----------------------------------------------------------------------------
;; Computation functions
;;-----------------------------------------------------------------------------

(defun tt-get-total-time ()
  "Returns total time of the test, in seconds.
It may not be exactly equal to tt-duration-seconds, if
Emacs wasn't able to deliver the timer exactly on time.
This can happen if the user was typing too fast."
  (round (- tt-end-time tt-start-time)))

(defun tt-get-total-chars ()
  "Returns the total number of characters the user
typed during the test, including spaces and newlines."

  ;; not sure how typing tests usually score CPM, but
  ;; if we have to skip whitespace, it'll be easy to fix.
  (save-excursion
    (set-buffer (tt-get-typing-buffer))
    (length (buffer-string))))

(defun tt-trunc-float (num)
  "Truncates a number to 2 decimal places."
  (/ (round (* 100 num)) 100.0))

(defun tt-get-total-words ()
  "Returns the total number of 5-letter words the user
typed during the test, including count of spaces and newlines."
  (tt-trunc-float (/ (tt-get-total-chars) 5.0)))

(defun tt-minutes-elapsed ()
  "Returns the (floating-point) number of elapsed minutes."
  (/ (float (tt-get-total-time)) 60))

(defun tt-compute-cpm ()
  "Returns the total chars per minute the user typed."
  (/ (float (tt-get-total-chars)) (tt-minutes-elapsed)))

(defun tt-compute-gross-wpm ()
  "Returns the total words per minute the user typed."
  (/ (tt-get-total-words) (tt-minutes-elapsed)))

(defun tt-compute-net-wpm ()
  "Returns the net total words per minute the user typed.
Net wpm is gross wpm minus the number of errors made."
  (- (tt-compute-gross-wpm) (tt-get-num-errors)))

(defun tt-get-num-errors ()
  "Returns the number of typing mistakes made during the last test."
  ;; sum the 3rd elements of all the descriptors
  (apply '+ (mapcar 'third (tt-get-diffs))))

(defun tt-compute-accuracy ()
  "Returns your net typing accuracy on the last test.
Accuracy is computed as (words - errors) / words.
Returns a floating-point percentage value from 0.0 to 100.0"
  (let ((words (tt-get-total-words)))
    (tt-trunc-float (* 100.0 (/ (- words (tt-get-num-errors)) words)))))

;;-----------------------------------------------------------------------------
;; Text highlighting functions
;;-----------------------------------------------------------------------------

(defvar tt-moved-word nil "For typing-test internal use.")

(defun tt-eobp ()
  "Returns non-nil if there are no non-whitespace characters after the cursor."
  (let ((tail (buffer-substring-no-properties (point) (point-max))))
    (not (string-match "[^ \t\r\n]" tail))))

(defun tt-highlight-first-word ()
  (save-excursion
    (set-buffer (tt-get-content-buffer))
    (toggle-read-only -1)
    (beginning-of-buffer)
    (skip-syntax-forward "^\\s-")
    (setq tt-overlay (make-overlay 1 (point)))
    (overlay-put tt-overlay 'face '((:foreground "blue") (:underline t)))
    (toggle-read-only 1)
    (setq tt-expected-start 1)
    (setq tt-expected-end (point))))

(defun tt-highlight-next-word ()
  "Move expected-word overlay to next word in content buffer."
  (save-excursion
    (set-buffer (tt-get-content-buffer))
    (goto-char tt-expected-end)
    (skip-syntax-forward "\\s-")
    (setq tt-expected-start (point))
    (skip-syntax-forward "^\\s-")
    (setq tt-expected-end (point))
    (move-overlay tt-overlay tt-expected-start tt-expected-end)))

(defun tt-expected-word ()
  "Returns the word the overlay is currently highlighting."
  (save-excursion
    (set-buffer (tt-get-content-buffer))
    (buffer-substring-no-properties tt-expected-start tt-expected-end)))

(defun tt-word-before-overlay ()
  "Gets the word just before the currently expected word.
Returns nil if we're on the very first word in the content."
  (save-excursion
    (let ((on-first-word (= tt-expected-start 1)))
      (set-buffer (tt-get-content-buffer))
      (goto-char tt-expected-start)
      (backward-word 1)
      (if (or (bobp) on-first-word)
	  nil
	(current-word)))))

(defun tt-flag-error ()
  "Turns the current word red to show it's an error."
  (save-excursion
    (let (start)
      (skip-syntax-backward "^\\s-")
      (setq start (point))
      (skip-syntax-forward "^\\s-")
      (put-text-property start (point) 'face '((:foreground "red"))))))

(defun tt-highlight-errors ()
  "Goes through text you typed and highlights any errors you made."
  (save-excursion
    (set-buffer (tt-get-typing-buffer))
    (toggle-read-only -1)
    (beginning-of-buffer)
    (forward-word 1)
    (mapcar
     (lambda (dsc)
       (destructuring-bind (word code count) dsc
	 (unless (eq code 'ok)
	   (tt-flag-error))
	 (forward-word 1)))
     (tt-get-diffs))

    (toggle-read-only 1)))

;;-----------------------------------------------------------------------------
;; Error calculation functions
;;-----------------------------------------------------------------------------

(defun tt-get-buffer-words (buf)
  "Returns a list of all whitespace-delimited strings in buffer BUF."
  (save-excursion
    (set-buffer buf)
    (split-string (buffer-substring-no-properties (point-min) (point-max)))))

(defun tt-get-content-words ()
  "Returns a list of all the words in the content buffer.  For our
purposes, a 'word' is any string delimited by whitespace."
  (tt-get-buffer-words (tt-get-content-buffer)))

(defun tt-get-typed-words ()
  "Returns a list of all the words in the content buffer.  For our
purposes, a 'word' is any string delimited by whitespace."
  (tt-get-buffer-words (tt-get-typing-buffer)))

(defun tt-get-diffs ()
  "Returns the (word) diffs between the content and typing buffers.
Uses the current contents of the buffers, which are frozen after the test.
Returns a list of descriptors.  Each descriptor is a list of three items:
a word, a descriptive symbol such as 'ok/'skipped/'extra, and an error
count for that word."
  (or tt-typing-diffs
      (setq tt-typing-diffs
	    (tt-compute-diffs
	     (tt-get-typed-words)
	     (tt-get-content-words)))))

;; We insert skipped and misspelled words into the result stream,
;; so we can show mistakes in the output.

;; Example:
;; expected: I am a very fast typist, and enjoy typing a lot.
;; typed:    I am a vry fast typist, enjoy typing a lot.
;; result:   I am a (vry) fast typist, (and) enjoy typing a lot.

(defun tt-compute-diffs (typed original)
  "Computes diffs between two word streams.  Returns a list of
descriptors, each with 3 items: a word, a descriptive symbol, and an
error count."

  ;; if we got strings, not word lists, convert them for convenience
  (if (stringp typed) (setq typed (split-string typed)))
  (if (stringp original) (setq original (split-string original)))

  ;; helper alist for shuffling pass/return values around
  (let ((data
	 (list (cons 's1 typed)
	       (cons 's2 original)
	       (cons 'output nil))))

    (defun pop-words-save-descriptor ()
      (tt-add-descriptor data (list (tt-first-word data 's1) 'ok 0))
      (tt-pop-first-words data))

    (setq case-fold-search nil) ;; makes it buffer-local

    (while (tt-streams-not-empty data)
      (cond

       ;; if first words match, remove them, save, and continue
       ((tt-same-first-words data)
	(pop-words-save-descriptor))

       ;; mismatch - look for (car s1) downstream in s2
       ((tt-find-match data 's1 's2) nil)

       ;; look for (car s2) downstream in s1
       ((tt-find-match data 's2 's1) nil)

       ((tt-check-incomplete-last-word data)
	(pop-words-save-descriptor))

       ;; can't find either list's first word in the other list.
       ;; treat (car s1) as a misspelling of (car s2), pop both,
       ;; record an error, and keep going.
       ;;
       ;; Example:
       ;;  s2: I type very fast now.
       ;;  s1: I type ve ry fast now.
       ;;
       ;; After skipping non-mistake words "I" and "type":
       ;;   s1 = ("ve" "ry" "fast" "now."), s2 = ("very" "fast" "now")
       ;;   (car s1) is 've', not downstream in s2
       ;;   (car s2) is 'very', not downstream in s1
       ;;
       ;; So assume 've' is a misspelling of 'very'.
       ;; Next loop, 'ry' will be considered an added word.
       ;; (Someday maybe we should count split words as 1 mistake.)
       (t
	(tt-add-descriptor data (list (tt-first-word data 's1) 'bad 1))
	(tt-pop-first-words data))))

    ;; edge case:  nonempty a (user typed extra stuff)
    (let ((typed (tt-get-stream data 's1)))
      (if typed (tt-handle-extra-words data typed)))

    ;; edge case:  last word is incomplete but correct so far
    ;;(tt-check-incomplete-last-word data)

    ;; return the list of descriptors we built up
    (tt-get-stream data 'output)))

(defun tt-check-incomplete-last-word (data)
  "If test ends in middle of a word, don't count it as an error.
Special-cases the last word in the stream if it's correct so far."
  (let ((typed (tt-get-stream data 's1))
	(orig (tt-get-stream data 's2)))

    ;; make sure just 1 typed word and at least 1 expected word
    (and orig (= 1 (length typed))

      ;; if last typed word is shorter than next expected word, and
      ;; is a proper substring of next expected word, it's incomplete
      (let ((last-word (car typed))
	    (expected (car orig)))

	(if (symbolp last-word) (setq last-word (symbol-name last-word)))
	(if (symbolp expected) (setq expected (symbol-name expected)))

	(and (< (length last-word) (length expected))
	     (string= last-word (substring expected 0 (length last-word))))))))

(defun tt-streams-not-empty (data)
  "Returns t if both word-streams in DATA are non-nil."
  (and (tt-get-stream data 's1)
       (tt-get-stream data 's2)))

(defun tt-get-stream (data stream)
  "Returns stream indexed by 's1, 's2 or 'output in DATA."
  (cdr (assq stream data)))

(defun tt-set-stream (data stream value)
  "Sets stream indexed by 's1, 's2, or 'output in DATA, to VALUE."
  (setcdr (assq stream data) value))

(defun tt-pop-word (data stream)
  "Destructively removes and returns first word from STREAM in DATA."
  (let* ((s (assq stream data))  ;; e.g. (s1 . word-list)
	 (word (cadr s)))
    (setcdr s (cddr s))
    word))

(defun tt-pop-first-words (data)
  "Destructively pops the first word off of both input streams."
  (tt-pop-word data 's1)
  (tt-pop-word data 's2))

(defun tt-first-word (data stream)
  "Returns first word in specified stream."
  (cadr (assq stream data)))

(defun tt-add-descriptor (data descriptor)
  "Adds a descriptor for a word the user typed.  It's a list of
3 elements: the word (which may have been a skipped word), a symbol
describing the type of mistake, and an error count."
  (let ((out (assq 'output data)))
    (if (null (cdr out))
	(setcdr out (list descriptor))
      (setcdr out (append (cdr out) (list descriptor))))))

(defun tt-add-descriptors (data descriptors)
  "Appends a list of descriptors to the output stream."
  (mapcar (lambda (d) (tt-add-descriptor data d)) descriptors))

(defun tt-same-first-words (data)
  "Returns true if the first word in each stream is the same."
  (string-equal (car (tt-get-stream data 's1))
		(car (tt-get-stream data 's2))))

(defun tt-partition-list (seq n)
  "Splits SEQ into left and right partitions.  0-indexed.
Passed index N is the first element in the right partition."
  (cond
   ((null n) (list seq nil))
   ((eq n 0) (list nil seq))
   (t (list (subseq seq 0 n) (subseq seq n)))))

(defun tt-find-match (data a b)
  "Looks for (car A) downstream in B.  A and B are either 's1 or 's2,
specifying the streams to search in DATA.  If found, it fast-forwards
B to that point, then pops the first word off both lists, creates a
descriptor, and returns t.  Otherwise returns nil, with no changes."

  (let ((s1 (tt-get-stream data a))
	(s2 (tt-get-stream data b)))

    ;; split stream s2 into skipped words and remainder
    (multiple-value-bind (skip rest)
	(tt-partition-list s2
			   (position (car s1) s2 :test 'string=))

      ;; if rest is nonempty, we found a match downstream in s2
      (and rest
	   (tt-set-stream data b rest)

	   ;; add descriptor for each skipped word to result stream
	   (tt-add-descriptors
	    data
	    (mapcar (lambda (w) (list w 'bad 1)) skip))))))


(defun tt-handle-extra-words (data stream)
  "Adds an error descriptor for each additional word in STREAM."
  (mapcar (lambda (w)
	    (tt-add-descriptor data (list w 'extra 1)))
	  stream))

;;-----------------------------------------------------------------------------
;; Unit tests for the word-diff algorithm.
;;-----------------------------------------------------------------------------

(defvar ttest-unit-tests
  '(
    ("no errors expected"
      0
      (I am a very fast typist and enjoy typing a lot)
      (I am a very fast typist and enjoy typing a lot))

    ("3 word joins"
     ;; this should really be 3 errors, but since they
     ;; screwed up 5 words in the original, it's 5.
     5
     (I am a very fast typist and enjoy typing a lot)
     (I am a veryfast typist and enjoytypinga lot))

    ("1 typo and 1 deleted word"
     2
     (I am a very fast typist and enjoy typing a lot)
     (I am a vry fast typist enjoy typing a lot))

    ("1 added word"
     1
     (I am a very fast typist and enjoy typing a lot)
     (I am a very very fast typist and enjoy typing a lot))

    ("last-word typo"
     1
     (I am fast)
     (I am faster))

    ("last word incomplete"
     0
     (I am fast)
     (I am fas))

    ("1 extra word"
     1
     (I am fast)
     (I am fast too))

    ("1 early word join"
     2
     (This is about me and I think it should be good)
     (This is aboutme and I think it should be good))

    ("3 extra words"
     3
     (I am a very fast typist and enjoy typing a lot)
     (I am a very fast typist and enjoy typing a lot ha ha ha))

    ("2 split words in a row"
     ;; again, this counts "ve" "re" "fa" and "st" as individual
     ;; errors, so it reports 4 instead of 2, which is OK.
     4
     (I am a very fast typist and enjoy typing a lot)
     (I am a ve ry fa st typist and enjoy typing a lot))

    ("insanity for a while then gets better downstream."
     7
     (I am a very fast typist and enjoy typing a lot)
     (I would say that I enjoy typing immensely))

    ("almost complete mismatch"
     8
     (I would say that I enjoy typing immensely)
     (I am a very fast typist and enjoy typing a lot))

    ("long insane prologue"
     ;; this is really counting too many errors...
     12
     (I am a very fast typist and enjoy typing a lot)
     (When feeling down in the dumps I enjoy typing a lot)))

  "Unit test data for typing test.")

(defun tt-do-test (want-err original typed)
  "Performs a single unit test.  WANT-ERR is the expected
number of typing errors.  ORIGINAL is the correct content,
and TYPED is what the user typed.  Returns t if the test
succeeds, or an error string if it fails."

  (defun join (title lst)
    (concat title (mapconcat 'prin1-to-string lst " ") "\n"))

  (insert (join "orig: " original))
  (insert (join "user: " typed))

  (let ((errors 0)
	(diffs (tt-compute-diffs typed original)))

    (if tt-debug-unit-tests
	(insert "\n<diffs:  " (prin1-to-string diffs) " >\n\n"))

    (insert "errs: ")

    (mapcar
     (lambda (dsc)
       (destructuring-bind (word code count) dsc
	 (if (eq code 'ok)
	     (insert (prin1-to-string word) " ")
	   (progn
	     (incf errors count)
	     (insert "(" (prin1-to-string word) ") ")))))
     diffs)

    (let ((msg (if (= errors 1) "error" "errors")))
      (insert (format "\n -- %d %s\n" errors msg)))

    (or (= errors want-err)
	(format "TEST FAILED:  expected %d errors, got %d\n\n"
		want-err errors))))

(defun tt-unit-test (want-err original typed)
  "Runs a single unit test and prints result.
Returns t, or nil if failed."
  (let ((result (tt-do-test want-err original typed)))
    (if (stringp result)
	(progn
	  (insert result)
	  nil)
      (progn
	(insert "Test succeeded.\n\n")
	t))))

(defun tt-run-unit-tests ()
  "Runs unit tests on word-diff algorithm.  Run in *scratch* buffer."
  (let ((passed 0)
	(failed 0))
    (mapc
     (lambda (test)
       (destructuring-bind (msg errs orig typed) test
	 (insert "Running test: " msg "\n")
	 (let ((result (tt-unit-test errs orig typed)))
	   (if result
	       (incf passed)
	     (incf failed)))))
     ttest-unit-tests)

    (let* ((total (+ passed failed))
	   (percent (* 100.0 (/ (float passed) total))))
      (cond
       ((= failed 0)
	(insert (format "Ran %d tests.  %d (100%%) of tests passed.\n\n"
			total passed))
	t)
       (t
	(insert (format "Ran %d tests.  %d failed, %d (%d%%) of passed.\n\n"
			total failed passed percent))
	nil)))))

(provide 'typing-test)

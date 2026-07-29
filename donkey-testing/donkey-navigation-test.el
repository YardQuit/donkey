;;; donkey-navigation-test.el --- Tests for DONKEY navigation commands -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;; Ensure dynamic scoping for position-tracking and visual-selection variables
(defvar donkey--position-ring)
(defvar donkey--position-index)
(defvar donkey--last-tracked-state)
(defvar donkey-position-ring-max)
(defvar donkey-visual-anchor)

(defmacro donkey--goto-line (n)
  "Move point to the start of absolute line N (1-based) in the current buffer."
  `(progn (goto-char (point-min)) (forward-line (1- ,n))))

(defmacro donkey--bol (n)
  "Return the buffer position starting absolute line N (1-based)."
  `(save-excursion (donkey--goto-line ,n) (line-beginning-position)))

(defmacro donkey--eol (n)
  "Return the buffer position ending absolute line N (1-based)."
  `(save-excursion (donkey--goto-line ,n) (line-end-position)))

;;; ---------------------------------------------------------------------------
;;; donkey-goto-line
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-goto-line-go-to-line-1 ()
  "Go to line 1 in a non-empty buffer."
  (let ((target-line 1))
    (with-temp-buffer
      (insert "line one\nline two\n")
      (goto-char 10)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (donkey-goto-line))
      (should (= (point) 1)))))

(ert-deftest donkey-goto-line-go-to-line-2 ()
  "Go to line 2 in a multi-line buffer."
  (let ((target-line 2))
    (with-temp-buffer
      (insert "line one\nline two\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (donkey-goto-line))
      (should (= (point) 10)))))

(ert-deftest donkey-goto-line-from-end-of-buffer ()
  "Go to a middle line when starting from end."
  (let ((target-line 2))
    (with-temp-buffer
      (insert "one\ntwo\nthree\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (donkey-goto-line))
      (should (= (point) 5)))))

(ert-deftest donkey-goto-line-line-beyond-buffer ()
  "Request line number beyond buffer end: `forward-line' stops at end."
  (let ((target-line 10))
    (with-temp-buffer
      (insert "one\ntwo\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (donkey-goto-line))
      (should (= (point) (point-max))))))

(ert-deftest donkey-goto-line-read-number-prompted ()
  "User is prompted via `read-number'."
  (let (read-number-called)
    (with-temp-buffer
      (insert "test\n")
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _)
                   (setq read-number-called t)
                   1)))
        (call-interactively #'donkey-goto-line))
      (should read-number-called))))

(ert-deftest donkey-goto-line-empty-buffer-line-1 ()
  "Empty buffer, request line 1: point stays at 1."
  (let ((target-line 1))
    (with-temp-buffer
      (goto-char (point-min))
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (donkey-goto-line))
      (should (= (point) 1)))))

(ert-deftest donkey-goto-line-request-zero-or-negative ()
  "Requesting line 0 or a negative line leaves point at `point-min'.

Request line 0 (undocumented, unvalidated input): no error, point stays
at point-min since `forward-line' -1 there has nowhere to go."
  (let ((target-line 0))
    (with-temp-buffer
      (insert "line one\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (donkey-goto-line))
      (should (= (point) 1)))))

(ert-deftest donkey-goto-line-large-line-number ()
  "Request very large line number: Emacs stops at end of buffer."
  (let ((target-line 1000000))
    (with-temp-buffer
      (insert "short\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (donkey-goto-line))
      (should (= (point) (point-max))))))

(ert-deftest donkey-goto-line-fractional-input-rounds-instead-of-erroring ()
  "A fractional line number is rounded instead of signaling an error.

Regression test: `read-number' happily returns a float (e.g. typing
\"3.5\" at the prompt, an easy typo for \"35\" or \"3\"), which used to
be passed straight to `forward-line' -- signaling a raw
`wrong-type-argument' error, since `forward-line' requires an integer,
instead of just going to a line. Now rounded to the nearest whole line."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 3.5)))
      (donkey-goto-line))
    (should (= (line-number-at-pos) 4))))

(ert-deftest donkey-goto-line-preserves-buffer-text ()
  "After `goto-line', buffer text is unchanged."
  (let ((original-text "original text\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) 1)))
        (donkey-goto-line))
      (should (string= (buffer-string) original-text)))))

;;; ---------------------------------------------------------------------------
;;; donkey--track-position / donkey-jump-back
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-track-position-first-call-sets-state-only ()
  "First call has no previous state, so no marker is pushed."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 3)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (should (null donkey--position-ring))
      (should (equal donkey--last-tracked-state
                     (cons (current-buffer) 3))))))

(ert-deftest donkey-track-position-same-position-no-push ()
  "When buffer and point are unchanged, no marker is pushed."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 3)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (donkey--track-position)
      (should (null donkey--position-ring)))))

(ert-deftest donkey-track-position-different-point-pushes-marker ()
  "When point changes, a marker recording the OLD position is pushed."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (goto-char 5)
      (donkey--track-position)
      (should (= (length donkey--position-ring) 1))
      (let ((m (car donkey--position-ring)))
        (should (eq (marker-buffer m) (current-buffer)))
        (should (= (marker-position m) 1))))))

(ert-deftest donkey-track-position-resets-index-on-new-record ()
  "When a new position is recorded, index resets to 0."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (setq donkey--position-index 5)
      (donkey--track-position)
      (goto-char 5)
      (donkey--track-position)
      (should (= donkey--position-index 0)))))

(ert-deftest donkey-track-position-enforces-ring-max ()
  "Ring should not exceed donkey-position-ring-max entries."
  (with-temp-buffer
    (insert "hello\n")
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 3))
      (donkey--track-position)  ; First doesn't push
      (dotimes (i 5)
        (goto-char (+ 2 i))
        (donkey--track-position))
      (should (= (length donkey--position-ring) 3)))))

(ert-deftest donkey-track-position-shrinks-a-ring-that-is-already-too-long ()
  "Lowering the maximum takes effect, on a ring that has outgrown it.

Regression test: the trim dropped exactly ONE marker per call and every
tracked move pushes exactly one, so the two canceled and the ring
stayed at whatever length it first reached.  With the ring at 10 and
the option lowered to 2, five further moves left it at 10 and `S'
walked back through six positions where two were configured.

Growing from empty always honored the limit -- which is what
`donkey-track-position-enforces-ring-max' above covers, and why this
went unnoticed: the option looks like it works until it is lowered
mid-session.

One move is enough to converge, so the assertion is on the first."
  (with-temp-buffer
    (insert "hello there\n")
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (dotimes (i 9)
        (goto-char (+ 2 i))
        (donkey--track-position))
      (should (= (length donkey--position-ring) 9))
      ;; Now lower it and move once more.
      (let ((donkey-position-ring-max 2))
        (goto-char 11)
        (donkey--track-position)
        (should (= (length donkey--position-ring) 2))))))

(ert-deftest donkey-track-position-releases-every-marker-it-drops ()
  "Markers trimmed off the ring are pointed nowhere, not just the last.

The old trim released one marker per call because it only ever dropped
one.  Trimming to the limit in a single step drops many at once, and
each has to be released or the buffer keeps them alive for nothing."
  (with-temp-buffer
    (insert "hello there\n")
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (dotimes (i 6)
        (goto-char (+ 2 i))
        (donkey--track-position))
      (let ((before (copy-sequence donkey--position-ring)))
        (let ((donkey-position-ring-max 1))
          (goto-char 11)
          (donkey--track-position))
        (should (= (length donkey--position-ring) 1))
        ;; Everything no longer in the ring points nowhere.
        (should (seq-every-p
                 (lambda (m) (or (memq m donkey--position-ring)
                                 (null (marker-position m))))
                 before))))))

(ert-deftest donkey-track-position-drop-to-zero-empties-a-filled-ring ()
  "Lowering the maximum to 0 empties the ring rather than stranding one.

Zero is a documented way to switch tracking off.  Reaching it from a
ring that already holds entries goes through the multi-element trim,
where `nbutlast' would have left the variable pointing at the original
cons -- the failure the old code's comment describes, reached by the
other route."
  (with-temp-buffer
    (insert "hello there\n")
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (dotimes (i 5)
        (goto-char (+ 2 i))
        (donkey--track-position))
      (should (> (length donkey--position-ring) 0))
      (let ((donkey-position-ring-max 0))
        (goto-char 11)
        (donkey--track-position)
        (should (null donkey--position-ring))
        ;; And the ordinary report, not "Marker does not point anywhere".
        (should-error (donkey-jump-back) :type 'user-error)))))

(ert-deftest donkey-track-position-skips-minibuffer ()
  "Tracking should not happen when minibuffer is active."
  (with-temp-buffer
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (let ((initial-state donkey--last-tracked-state))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () t)))
          (donkey--track-position))
        (should (null donkey--position-ring))
        (should (equal donkey--last-tracked-state initial-state))))))

(ert-deftest donkey-jump-back-no-positions-error ()
  "With empty ring, signals user-error."
  (let ((donkey--position-ring nil)
        (donkey--position-index 0)
        (donkey--last-tracked-state nil)
        (donkey-position-ring-max 10))
    (should-error (donkey-jump-back) :type 'user-error)))

(ert-deftest donkey-position-ring-max-zero-leaves-no-dead-marker ()
  "A ring max of 0 empties the ring instead of keeping a dead marker.

Regression: the trim did `(nbutlast donkey--position-ring)' for effect
only, and `nbutlast' cannot destructively empty a ONE-element list --
it returns nil while the variable still points at the original cons.
With the max at 0 every trim is exactly that case, so the ring kept the
marker whose position had just been cleared."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (goto-char (point-min))
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 0))
      (donkey--track-position)
      (goto-char 3)
      (donkey--track-position)
      (should-not donkey--position-ring)
      (should-not (seq-find (lambda (m) (null (marker-position m)))
                            donkey--position-ring)))))

(ert-deftest donkey-jump-back-with-ring-max-zero-reports-no-positions ()
  "With tracking switched off via a 0 ring max, jumping back is a clean error.

Regression: the ring retained a marker pointing nowhere, so this
signalled a bare `error' reading \"Marker does not point anywhere\"
instead of the ordinary `user-error' for an empty ring."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (goto-char (point-min))
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 0))
      (donkey--track-position)
      (goto-char 3)
      (donkey--track-position)
      (should-error (donkey-jump-back) :type 'user-error))))

(ert-deftest donkey-track-position-survives-a-non-numeric-ring-max ()
  "A non-numeric ring max must not make the tracker signal.

Regression: `donkey--track-position' compared the ring length against
`donkey-position-ring-max' directly, so setting it to nil -- the obvious
guess for \"disabled\", and the defcustom already blesses 0 for exactly
that -- raised `wrong-type-argument' from `post-command-hook'.  Emacs
REMOVES a hook function that errors, so one keypress switched position
tracking off for the rest of the session and repairing the variable did
not bring it back."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (goto-char (point-min))
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max nil))
      (donkey--track-position)
      (goto-char 3)
      (should (progn (donkey--track-position) t))
      ;; Read as 0: tracking off, which is what nil was reaching for.
      (should-not donkey--position-ring)
      (should-error (donkey-jump-back) :type 'user-error))))

(ert-deftest donkey-track-position-survives-other-junk-ring-maxes ()
  "Every wrong-typed ring max is read as \"off\" rather than signaling.

A string and a symbol stand in for whatever a hand-written init file
might put there.  A negative count already meant \"keep nothing\"; it is
pinned here so making the junk cases safe did not quietly change it."
  (dolist (junk '("10" some-symbol (10) -3))
    (with-temp-buffer
      (insert "a\nb\nc\n")
      (goto-char (point-min))
      (let ((donkey--position-ring nil)
            (donkey--position-index 0)
            (donkey--last-tracked-state nil)
            (donkey-position-ring-max junk))
        (donkey--track-position)
        (goto-char 3)
        (should (progn (donkey--track-position) t))
        (should-not donkey--position-ring)))))

(ert-deftest donkey-track-position-truncates-a-float-ring-max ()
  "A float ring max keeps working, truncated rather than rejected.

It happened to work before the guard went in -- `>' accepts floats --
so reading it as 0 would have been a regression dressed up as a fix."
  (with-temp-buffer
    (insert "abcdefghij\n")
    (goto-char (point-min))
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 3.0))
      (dolist (pos '(2 3 4 5 6 7))
        (goto-char pos)
        (donkey--track-position))
      (should (= (length donkey--position-ring) 3)))))

(ert-deftest donkey-jump-back-single-position-jumps ()
  "With one position, jumps to it and wraps the index."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (goto-char 5)
      (donkey--track-position)
      (should (= (length donkey--position-ring) 1))
      (donkey-jump-back)
      (should (= (point) 1))
      ;; The counter is advanced past the entry just used; with one entry
      ;; it wraps at the next use rather than eagerly here.
      (should (= donkey--position-index 1))
      (donkey-jump-back)
      (should (= (point) 1)))))

(ert-deftest donkey-jump-back-multiple-positions-rotate ()
  "Positions are visited most-recent first: 5 -> 3 -> 1.

Was 3 -> 1 -> 5.  Point had just come from 5, so the first press skipped
the position it was meant to return to and landed on the one before --
the whole reason `S' exists is to take back the jump just made."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (goto-char 3)
      (donkey--track-position)
      (goto-char 5)
      (donkey--track-position)
      (goto-char 7)
      (donkey--track-position)
      (should (= (length donkey--position-ring) 3))
      (donkey-jump-back)
      (should (= (point) 5))
      (donkey-jump-back)
      (should (= (point) 3))
      (donkey-jump-back)
      (should (= (point) 1)))))

(ert-deftest donkey-jump-back-wraps-around-ring ()
  "After exhausting the ring, wraps back to the most recent entry."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (goto-char 3)
      (donkey--track-position)
      (goto-char 5)
      (donkey--track-position)
      (should (= (length donkey--position-ring) 2))
      ;; Most recent first, then wrapping back round to it.
      (donkey-jump-back)
      (should (= (point) 3))
      (donkey-jump-back)
      (should (= (point) 1))
      (donkey-jump-back)
      (should (= (point) 3)))))

(ert-deftest donkey-jump-back-updates-tracking-state ()
  "After jumping, tracking state is updated to new position."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (goto-char 5)
      (donkey--track-position)
      (donkey-jump-back)
      (should (equal donkey--last-tracked-state
                     (cons (current-buffer) 1))))))

(ert-deftest donkey-jump-back-call-interactively ()
  "Can be called interactively."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (goto-char 5)
      (donkey--track-position)
      (call-interactively #'donkey-jump-back)
      (should (= (point) 1)))))

(ert-deftest donkey-jump-back-shows-progress-message ()
  "Displays a \"Position N/M\" progress message."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)
      (goto-char 5)
      (donkey--track-position)
      (let (msg)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq msg (apply #'format fmt args)))))
          (donkey-jump-back))
        (should msg)
        (should (string-match "Position 1/1" msg))))))

(ert-deftest donkey-position-ring-is-buffer-local ()
  "Each buffer accumulates its own position ring, independent of others."
  (let ((buf-a (generate-new-buffer "*test-buffer-local-a*"))
        (buf-b (generate-new-buffer "*test-buffer-local-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (insert "aaaa\n")
            (goto-char 1)
            (donkey--track-position)
            (goto-char 3)
            (donkey--track-position))
          (with-current-buffer buf-b
            (insert "bbbb\n")
            (goto-char 1)
            (donkey--track-position)
            (goto-char 4)
            (donkey--track-position)
            (goto-char 2)
            (donkey--track-position))
          (should (= (length (buffer-local-value 'donkey--position-ring buf-a)) 1))
          (should (= (length (buffer-local-value 'donkey--position-ring buf-b)) 2)))
      (dolist (b (list buf-a buf-b))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest donkey-jump-back-stays-within-current-buffer ()
  "Jumping back in one buffer never visits a position recorded in another."
  (let ((buf-a (generate-new-buffer "*test-jump-local-a*"))
        (buf-b (generate-new-buffer "*test-jump-local-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (insert "aaaa\n")
            (goto-char 1)
            (donkey--track-position)
            (goto-char 3)
            (donkey--track-position))
          (with-current-buffer buf-b
            (insert "bbbb\n")
            (goto-char 1)
            (donkey--track-position)
            (goto-char 4)
            (donkey--track-position))
          (with-current-buffer buf-a
            (donkey-jump-back)
            (should (eq (current-buffer) buf-a))
            (should (= (point) 1)))
          (with-current-buffer buf-b
            (donkey-jump-back)
            (should (eq (current-buffer) buf-b))
            (should (= (point) 1))))
      (dolist (b (list buf-a buf-b))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest donkey-jump-back-empty-in-fresh-buffer-with-other-buffer-history ()
  "A fresh buffer has no recorded positions even when another buffer does."
  (let ((buf-a (generate-new-buffer "*test-jump-fresh-a*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (insert "aaaa\n")
            (goto-char 1)
            (donkey--track-position)
            (goto-char 3)
            (donkey--track-position))
          (with-temp-buffer
            (should (null donkey--position-ring))
            (should-error (donkey-jump-back) :type 'user-error)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a)))))

;;; ---------------------------------------------------------------------------
;;; donkey-switch-other-buffer
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-switch-other-buffer-calls-other-buffer ()
  "Calls `other-buffer' with the current buffer."
  (let (other-arg)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (setq other-arg buf) buf)))
      (donkey-switch-other-buffer))
    (should other-arg)
    (should (eq other-arg (current-buffer)))))

(ert-deftest donkey-switch-other-buffer-call-order ()
  "`other-buffer' executes before `switch-to-buffer'."
  (let (order)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (push 'other order) buf))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (push 'switch order))))
      (donkey-switch-other-buffer))
    (should (eq (nth 0 order) 'switch))
    (should (eq (nth 1 order) 'other))
    (should (= (length order) 2))))

(ert-deftest donkey-switch-other-buffer-switches-back-and-forth ()
  "Calling twice passes `current-buffer' each time."
  (let ((results)
        (buf-a (generate-new-buffer "*donkey-test-aa*"))
        (buf-b (generate-new-buffer "*donkey-test-bb*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'other-buffer)
                     (lambda (buf)
                       (if (eq buf buf-b)
                           buf-a
                         buf-b)))
                    ((symbol-function 'switch-to-buffer)
                     (lambda (buf)
                       (push buf results)
                       (set-buffer buf))))
            (set-buffer buf-b)
            (donkey-switch-other-buffer)
            (should (eq (current-buffer) buf-a))
            (donkey-switch-other-buffer)
            (should (eq (current-buffer) buf-b))))
      (dolist (b (list buf-a buf-b))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest donkey-switch-other-buffer-other-buffer-returns-nil ()
  "If `other-buffer' returns nil, `switch-to-buffer' receives nil without error."
  (let (switch-arg)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) nil))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (setq switch-arg buf))))
      (donkey-switch-other-buffer))
    (should-not switch-arg)))

(ert-deftest donkey-switch-other-buffer-call-interactively ()
  "Can be called via `call-interactively'."
  (let (other-called switch-called)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (setq other-called t) buf))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (setq switch-called t))))
      (call-interactively #'donkey-switch-other-buffer))
    (should other-called)
    (should switch-called)))

;;; ---------------------------------------------------------------------------
;;; donkey-visual-line-toggle
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-visual-line-toggle-activates-region ()
  "Without an active region, sets the anchor and activates the current line."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 3)
    (let ((donkey-visual-anchor nil))
      (donkey-visual-line-toggle)
      (should (region-active-p))
      (should (= donkey-visual-anchor (line-beginning-position 1)))
      (should (= (mark) (donkey--bol 1)))
      (should (= (point) (donkey--eol 1))))))

(ert-deftest donkey-visual-line-toggle-message-names-the-real-extend-keys ()
  "The toggle message names the real whole-line extend keys.

Regression test: the start-of-session message must distinguish the
whole-line extend keys from the character-wise ones.

It used to say only \"j/k to extend\", but lowercase `j'/`k' are bound
to plain `next-line'/`previous-line': they move point without
re-anchoring, so the selection stops mid-line at whatever column point
lands on rather than covering whole lines.  Confirmed live in `emacs
-nw' on \"ab\\nlonger line here\": `V' then `j' selected \"ab\\nlo\",
while `V' then `J' correctly selected \"ab\\nlonger line here\".  Both
are legitimate -- the message just has to say which does which."
  (let (shown)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 3)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (let ((donkey-visual-anchor nil))
          (donkey-visual-line-toggle))))
    ;; `case-fold-search' must be nil here: with the default t, "j/k"
    ;; matches "J/K" and these assertions cannot tell them apart.
    (let ((case-fold-search nil))
      (should (string-match-p "J/K" shown))
      (should (string-match-p "j/k" shown)))
    ;; The named keys must really be what the message claims.
    (should (eq (lookup-key donkey-normal-mode-map "J") #'donkey-visual-next-line))
    (should (eq (lookup-key donkey-normal-mode-map "K") #'donkey-visual-previous-line))
    (should (eq (lookup-key donkey-normal-mode-map "j") #'next-line))
    (should (eq (lookup-key donkey-normal-mode-map "k") #'previous-line))))

(ert-deftest donkey-visual-line-session-survives-character-wise-motion ()
  "A visual-line session survives character-wise motion.

Regression test: `j'/`k' (plain `next-line'/`previous-line') must not
end a visual-line session -- a following `J'/`K' has to still
recognize it and re-anchor to whole lines.

The session check used to key off `last-command' being one of the
visual-line commands, so any intervening `j' broke the chain and the
next `J' fell through to a plain `forward-line', silently losing the
whole-line anchoring for the rest of the session.  It now checks that
the mark still sits where a visual-line command left it (at the anchor,
or at the anchor line's end), which `j'/`k' never disturb since they
only move point."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "ab\nlonger line here\nxyz\n")
      (goto-char (point-min))
      (donkey-visual-line-toggle)
      (should (donkey--visual-line-session-active-p))
      ;; Character-wise motion: point moves, mark stays at the anchor.
      (let ((last-command 'donkey-visual-line-toggle))
        (next-line 1))
      (setq last-command 'next-line)
      (should (donkey--visual-line-session-active-p))
      ;; A following J must extend to WHOLE lines from the same anchor.
      (donkey-visual-next-line)
      (should (string= (buffer-substring-no-properties
                        (region-beginning) (region-end))
                       "ab\nlonger line here\nxyz")))))

(ert-deftest donkey-visual-line-session-still-rejects-stale-anchor ()
  "The relaxed session check still rejects a stale anchor.

The relaxed session check must still ignore an anchor left over from
an earlier session once an unrelated command has moved the mark.

This is the case `donkey--visual-line-session-active-p' exists for: a
mark command run while a region is already active never triggers
`deactivate-mark-hook', so the old anchor is never cleared.  Such
commands do move the mark somewhere unrelated to the old anchor's
line, which is exactly what the check keys off now."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "one\ntwo\nthree\nfour\n")
      (goto-char (point-min))
      (donkey-visual-line-toggle)
      (should (donkey--visual-line-session-active-p))
      ;; Simulate an unrelated mark command repositioning the region
      ;; without ever deactivating it, leaving the anchor stale.
      (goto-char (point-max))
      (push-mark (- (point-max) 5) t t)
      (should-not (donkey--visual-line-session-active-p)))))

(ert-deftest donkey-visual-line-toggle-cancels-active-region ()
  "Toggling during an active session cancels the region.

With a genuinely active visual-line session, deactivates the mark
and clears the anchor."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (let ((donkey-visual-anchor (point))
          (last-command 'donkey-visual-line-toggle))
      (set-mark 1)
      (activate-mark)
      (donkey-visual-line-toggle)
      (should-not (region-active-p))
      (should-not donkey-visual-anchor))))

(ert-deftest donkey-visual-line-toggle-twice-returns-to-inactive ()
  "Toggling on then off leaves no active region and no anchor."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey-visual-anchor nil)
          (last-command nil))
      (donkey-visual-line-toggle)
      (should (region-active-p))
      (setq last-command 'donkey-visual-line-toggle)
      (donkey-visual-line-toggle)
      (should-not (region-active-p))
      (should-not donkey-visual-anchor))))

(ert-deftest donkey-visual-line-toggle-with-unrelated-region-starts-fresh-session ()
  "Toggling over an unrelated region starts a fresh session.

Regression test: pressing this with an active region that is NOT a
genuine visual-line session (e.g. a `donkey-mark-inner' selection,
where `last-command' is not one of the visual-line commands) must
start a fresh visual-line session anchored at the current line,
rather than just canceling the unrelated region and reporting a
misleading \"Visual line: canceled\" for a selection that was never
a visual-line session to begin with.

Confirmed live in `emacs -nw': selecting \"hello\" via `donkey-mark-inner'
then pressing `V' used to print \"Visual line: canceled\" instead of
starting a new visual-line selection on the current line."
  (with-temp-buffer
    (insert "\"hello\" world\n")
    (goto-char 1)
    (let ((donkey-visual-anchor nil)
          (last-command 'donkey-mark-inner))
      ;; Simulate donkey-mark-inner's own selection: mark/point active,
      ;; but no visual-line anchor and an unrelated last-command.
      (set-mark 1)
      (goto-char 8)
      (activate-mark)
      (donkey-visual-line-toggle)
      (should (region-active-p))
      (should (= donkey-visual-anchor (donkey--bol 1)))
      (should (= (mark) (donkey--bol 1)))
      (should (= (point) (donkey--eol 1))))))

(ert-deftest donkey-visual-line-toggle-call-interactively ()
  "Can be called via `call-interactively'."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey-visual-anchor nil))
      (call-interactively #'donkey-visual-line-toggle)
      (should (region-active-p)))))

(ert-deftest donkey-visual-anchor-is-buffer-local ()
  "`donkey-visual-anchor' must be buffer-local.

Regression test: it used
to be a plain `defvar', so starting a visual-line selection in one
buffer leaked its anchor position into any other buffer that
separately activated a region (e.g. via `set-mark-command'), causing
`donkey-visual-next-line' to extend the selection using a position
that belongs to a completely different buffer."
  (let ((buf-a (generate-new-buffer "donkey-visual-anchor-buf-a"))
        (buf-b (generate-new-buffer "donkey-visual-anchor-buf-b")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (dotimes (_ 50) (insert "line in buffer A\n"))
            (goto-char (point-min))
            (forward-line 20)
            (donkey-visual-line-toggle))
          (with-current-buffer buf-b
            (insert "short buffer B\nline2\nline3\n")
            (goto-char (point-min))
            ;; buf-b must not see buf-a's anchor at all.
            (should-not donkey-visual-anchor)
            ;; An unrelated region activation in buf-b (no anchor of
            ;; its own) must behave like a plain downward motion:
            ;; the mark must be left untouched.
            (set-mark (point))
            (activate-mark)
            (forward-char 3)
            (let ((mark-before (mark)))
              (donkey-visual-next-line)
              (should (= (mark) mark-before)))))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

(ert-deftest donkey-visual-anchor-cleared-on-external-deactivate-mark ()
  "`donkey-visual-anchor' is cleared on any mark deactivation.

The anchor must be cleared whenever the mark is
deactivated, not just when canceled via `donkey-visual-line-toggle'
itself.  Regression test: if some other command deactivated the
mark (e.g. `keyboard-quit'), the anchor was left stale in the SAME buffer,
so a later, unrelated region activation (e.g. via `set-mark-command')
would have its selection hijacked by the leftover anchor position."
  (with-temp-buffer
    (dotimes (_ 20) (insert "line\n"))
    (goto-char (point-min))
    (forward-line 5)
    (donkey-visual-line-toggle)
    (should donkey-visual-anchor)
    ;; Something else deactivates the mark, bypassing
    ;; donkey-visual-line-toggle's own cancel branch entirely.
    (deactivate-mark)
    (should-not donkey-visual-anchor)
    ;; A later, unrelated selection must not be hijacked by a stale anchor.
    (goto-char (point-min))
    (set-mark (point))
    (activate-mark)
    (forward-char 2)
    (let ((mark-before (mark)))
      (donkey-visual-next-line)
      (should (= (mark) mark-before)))))

(ert-deftest donkey-visual-anchor-stale-does-not-hijack-region-set-without-deactivating ()
  "A stale anchor does not hijack a region set without a deactivation.

Regression test: an unrelated command that sets a NEW region WITHOUT
first deactivating the old one (e.g. `donkey-mark-inner', which calls
`push-mark'/`activate-mark' directly) must not have its selection
hijacked by a stale `donkey-visual-anchor' left over from an earlier
`donkey-visual-line-toggle' session.

Unlike `donkey-visual-anchor-cleared-on-external-deactivate-mark', the
mark here is never deactivated in between -- it stays continuously
active, just repositioned -- so `deactivate-mark-hook' never fires and
`donkey-visual-anchor' is never cleared.  Confirmed live in `emacs
-nw': selecting \"hello\" via `donkey-mark-inner' right after a
visual-line session, then pressing `J' (`donkey-visual-next-line'),
snapped the region all the way back to the visual-line session's
original anchor line instead of extending \"hello\" by one line."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n\"hello\"\nline5\nline6\n")
    (goto-char (point-min))
    (donkey-visual-line-toggle)
    (donkey-visual-next-line)
    (donkey-visual-next-line)
    (should donkey-visual-anchor)
    ;; donkey-mark-inner replaces the region without ever deactivating
    ;; it first -- last-command afterward is donkey-mark-inner, not one
    ;; of the visual-line commands.
    (goto-char (+ (point-min) 18))
    (let ((last-command 'donkey-mark-inner))
      (donkey-mark-inner)
      (let ((mark-before (mark))
            (point-before (point)))
        (donkey-visual-next-line)
        ;; A plain motion (forward-line), not a hijack back to the
        ;; stale anchor: mark stays put, point simply advances.
        (should (= (mark) mark-before))
        (should (> (point) point-before))))))

;;; ---------------------------------------------------------------------------
;;; donkey-visual-next-line
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-visual-next-line-no-region-moves-down ()
  "Without visual selection active, just moves down one line."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (donkey--goto-line 1)
    (let ((donkey-visual-anchor nil))
      (donkey-visual-next-line)
      (should (= (point) (donkey--bol 2))))))

(ert-deftest donkey-visual-next-line-from-bottom-stays-no-error ()
  "Already at bottom of buffer (no trailing newline): no error."
  (with-temp-buffer
    (insert "single line")
    (goto-char (point-min))
    (end-of-line)
    (let ((eol-pos (point)))
      (let ((donkey-visual-anchor nil))
        (donkey-visual-next-line)
        (should (= (point) eol-pos))))))

(ert-deftest donkey-visual-next-line-at-anchor-extends-to-line-begin ()
  "Point at L2, anchor at L3. Move down to L3 (same as anchor)."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-next-line)
        (should (region-active-p))
        (should (= (point) (donkey--bol 3)))
        (should (= (mark) (donkey--eol 3)))))))

(ert-deftest donkey-visual-next-line-above-anchor-extends-selection ()
  "Point above anchor.  Move down twice, crossing then passing the anchor."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-next-line)
        (should (= (point) (donkey--bol 3)))
        (should (= (mark) (donkey--eol 3)))
        (setq last-command 'donkey-visual-next-line)
        (donkey-visual-next-line)
        (should (= (point) (donkey--eol 4)))
        (should (= (mark) anchor))))))

(ert-deftest donkey-visual-next-line-below-anchor-sets-mark-to-anchor-beg ()
  "Point below anchor.  Moving further down keeps mark pinned to the anchor."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 4)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-next-line)
        (should (region-active-p))
        (should (= (point) (donkey--eol 5)))
        (should (= (mark) anchor))))))

(ert-deftest donkey-visual-next-line-preserves-buffer-content ()
  "Moving down doesn't modify buffer content."
  (let ((original "hello\nworld\n"))
    (with-temp-buffer
      (insert original)
      (donkey--goto-line 1)
      (let ((donkey-visual-anchor nil))
        (donkey-visual-next-line)
        (donkey-visual-next-line))
      (should (string= (buffer-string) original)))))

(ert-deftest donkey-visual-next-line-empty-lines ()
  "Works correctly with empty lines in buffer."
  (with-temp-buffer
    (insert "hello\n\nworld\n")
    (let ((anchor (donkey--bol 1)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-next-line)
        (should (= (point) (donkey--eol 3)))
        (should (= (mark) anchor))))))

(ert-deftest donkey-visual-next-line-call-interactively-with-selection ()
  "Can be called interactively with visual selection active."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (donkey--bol 2)))
      (donkey--goto-line 1)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (call-interactively #'donkey-visual-next-line)
        (should (region-active-p))
        (should (= (point) (donkey--bol 2)))
        (should (= (mark) (donkey--eol 2)))))))

(ert-deftest donkey-visual-next-line-keeps-anchor-intact ()
  "Anchor position doesn't change during movement."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3))
          (donkey-visual-anchor (donkey--bol 3)))
      (donkey--goto-line 4)
      (set-mark anchor)
      (end-of-line)
      (activate-mark)
      (donkey-visual-next-line)
      (donkey-visual-next-line)
      (should (= donkey-visual-anchor anchor)))))

;;; ---------------------------------------------------------------------------
;;; donkey-visual-previous-line
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-visual-previous-line-no-region-moves-up ()
  "Without visual selection active, just moves up one line."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (donkey--goto-line 3)
    (let ((donkey-visual-anchor nil))
      (donkey-visual-previous-line)
      (should (= (point) (donkey--bol 2))))))

(ert-deftest donkey-visual-previous-line-from-top-stays-no-error ()
  "Already at top of buffer: no error, point unchanged."
  (with-temp-buffer
    (insert "single line\n")
    (donkey--goto-line 1)
    (let ((donkey-visual-anchor nil))
      (donkey-visual-previous-line)
      (should (= (point) 1)))))

(ert-deftest donkey-visual-previous-line-at-anchor-extends-to-line-end ()
  "Point at L4, anchor at L3. Move up to L3 (same as anchor)."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 4)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (donkey--eol 3)))
        (should (= (mark) anchor))))))

(ert-deftest donkey-visual-previous-line-below-anchor-extends-selection ()
  "Point below anchor.  Move up twice, crossing then passing the anchor."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 4)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (= (point) (donkey--eol 3)))
        (should (= (mark) anchor))
        (setq last-command 'donkey-visual-previous-line)
        (donkey-visual-previous-line)
        (should (= (point) (donkey--bol 2)))
        (should (= (mark) (donkey--eol 3)))))))

(ert-deftest donkey-visual-previous-line-above-anchor-sets-mark-to-anchor-eol ()
  "Point at L2, anchor at L3. Move up to L1."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (donkey--bol 1)))
        (should (= (mark) (donkey--eol 3)))))))

(ert-deftest donkey-visual-previous-line-preserves-buffer-content ()
  "Moving up doesn't modify buffer content."
  (let ((original "hello\nworld\n"))
    (with-temp-buffer
      (insert original)
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor nil))
        (donkey-visual-previous-line)
        (donkey-visual-previous-line))
      (should (string= (buffer-string) original)))))

(ert-deftest donkey-visual-previous-line-single-line-with-region-no-error ()
  "Single line buffer with visual selection: no error."
  (with-temp-buffer
    (insert "single line\n")
    (goto-char (point-min))
    (let ((donkey-visual-anchor (point-min))
          (last-command 'donkey-visual-line-toggle))
      (set-mark (point-min))
      (end-of-line)
      (activate-mark)
      (donkey-visual-previous-line)
      (should (= (point) 12))
      (should (= (mark) 1))
      (should (region-active-p)))))

(ert-deftest donkey-visual-previous-line-empty-lines ()
  "Works correctly with empty lines in buffer."
  (with-temp-buffer
    (insert "hello\n\nworld\n")
    (let ((anchor (donkey--bol 1)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (= (point) (donkey--eol 1)))
        (should (= (mark) anchor))))))

(ert-deftest donkey-visual-previous-line-call-interactively-with-selection ()
  "Can be called interactively with visual selection active."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (donkey--bol 2)))
      (donkey--goto-line 3)
      (let ((donkey-visual-anchor anchor)
            (last-command 'donkey-visual-line-toggle))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (call-interactively #'donkey-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (donkey--eol 2)))
        (should (= (mark) anchor))))))

(ert-deftest donkey-visual-previous-line-keeps-anchor-intact ()
  "Anchor position doesn't change during movement."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3))
          (donkey-visual-anchor (donkey--bol 3)))
      (donkey--goto-line 4)
      (set-mark anchor)
      (end-of-line)
      (activate-mark)
      (donkey-visual-previous-line)
      (donkey-visual-previous-line)
      (should (= donkey-visual-anchor anchor)))))

;;; ---------------------------------------------------------------------------
;;; Counts on J/K
;;; ---------------------------------------------------------------------------

(defmacro donkey-test--visual-buffer (&rest body)
  "Run BODY in a six-line DONKEY buffer, returning (LINE . REGION)."
  `(with-temp-buffer
     (donkey-mode 1)
     (let ((transient-mark-mode t))
       (insert "L1\nL2\nL3\nL4\nL5\nL6\n")
       ,@body
       (cons (line-number-at-pos)
             (and (region-active-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end)))))))

(ert-deftest donkey-visual-next-line-count-matches-repeated-presses ()
  "`C-u 3 J' lands exactly where three separate `J' presses would.

Regression: `J' and `K' took no count at all while `j' and `k' -- bound
straight to `next-line' and `previous-line' -- have always taken one, so
\\[universal-argument] 5 J moved a single line."
  (should (equal (donkey-test--visual-buffer
                  (goto-char (point-min))
                  (forward-line 1)
                  (donkey-visual-line-toggle)
                  (donkey-visual-next-line 3))
                 (donkey-test--visual-buffer
                  (goto-char (point-min))
                  (forward-line 1)
                  (donkey-visual-line-toggle)
                  (donkey-visual-next-line 1)
                  (donkey-visual-next-line 1)
                  (donkey-visual-next-line 1)))))

(ert-deftest donkey-visual-previous-line-count-matches-repeated-presses ()
  "`C-u 3 K' lands exactly where three separate `K' presses would."
  (should (equal (donkey-test--visual-buffer
                  (goto-char (point-min))
                  (forward-line 4)
                  (donkey-visual-line-toggle)
                  (donkey-visual-previous-line 3))
                 (donkey-test--visual-buffer
                  (goto-char (point-min))
                  (forward-line 4)
                  (donkey-visual-line-toggle)
                  (donkey-visual-previous-line 1)
                  (donkey-visual-previous-line 1)
                  (donkey-visual-previous-line 1)))))

(ert-deftest donkey-visual-count-crossing-the-anchor-matches-single-presses ()
  "A count that carries point past the anchor re-anchors the same way.

The selection is re-derived from the anchor and wherever point lands
rather than accumulated, so the branch a count ends on is the branch a
run of single presses would end on -- including the one that flips which
side of the anchor the selection grows from."
  (should (equal (donkey-test--visual-buffer
                  (goto-char (point-min))
                  (forward-line 3)
                  (donkey-visual-line-toggle)
                  (donkey-visual-previous-line 2))
                 (donkey-test--visual-buffer
                  (goto-char (point-min))
                  (forward-line 3)
                  (donkey-visual-line-toggle)
                  (donkey-visual-previous-line 1)
                  (donkey-visual-previous-line 1)))))

(ert-deftest donkey-visual-next-line-negative-count-moves-up ()
  "A negative count moves the other way, as it does for `forward-line'."
  (should (equal (car (donkey-test--visual-buffer
                       (goto-char (point-min))
                       (forward-line 4)
                       (donkey-visual-next-line -2)))
                 3)))

(ert-deftest donkey-visual-previous-line-negative-count-moves-down ()
  "A negative count moves the other way, as it does for `forward-line'."
  (should (equal (car (donkey-test--visual-buffer
                       (goto-char (point-min))
                       (donkey-visual-previous-line -3)))
                 4)))

(ert-deftest donkey-visual-line-counts-work-without-a-session ()
  "Outside a visual-line session the count still reaches `forward-line'."
  (should (equal (car (donkey-test--visual-buffer
                       (goto-char (point-min))
                       (donkey-visual-next-line 3)))
                 4)))

;;; ---------------------------------------------------------------------------
;;; donkey-jump-back and narrowing
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-jump-back-skips-positions-outside-narrowing ()
  "A narrowed buffer cycles only through positions it is showing.

Regression: marker positions are absolute and narrowing does not move
them, so a ring recorded before narrowing mostly holds positions the
buffer no longer shows.  `goto-char' silently clamps to the narrowing
edge rather than signaling, so those entries landed point on the first
visible character while still reporting a jump to a recorded position."
  (with-temp-buffer
    (donkey-mode 1)
    (insert "aaaa\nbbbb\ncccc\ndddd\n")
    (dolist (p '(2 8 14 19))
      (goto-char p)
      (donkey--track-position))
    (narrow-to-region 11 15)
    (goto-char 12)
    (donkey-jump-back)
    (should (= (point) 14))
    (should (<= (point-min) (point)))
    (should (<= (point) (point-max)))))

(ert-deftest donkey-jump-back-reports-when-nothing-is-visible ()
  "With every recorded position hidden, it says so instead of clamping."
  (with-temp-buffer
    (donkey-mode 1)
    (insert "aaaa\nbbbb\ncccc\ndddd\n")
    (dolist (p '(2 3 4))
      (goto-char p)
      (donkey--track-position))
    (narrow-to-region 11 15)
    (goto-char 12)
    (let ((err (should-error (donkey-jump-back) :type 'user-error)))
      (should (equal (cadr err)
                     "No recorded position in the visible portion")))
    ;; and point did not move to the narrowing edge on the way out
    (should (= (point) 12))))

(ert-deftest donkey-jump-back-counts-only-visible-positions ()
  "The reported total matches what pressing again will cycle through."
  (with-temp-buffer
    (donkey-mode 1)
    (insert "aaaa\nbbbb\ncccc\ndddd\n")
    (dolist (p '(2 8 14 19))
      (goto-char p)
      (donkey--track-position))
    ;; The ring holds the PREVIOUS point at each move, so it is (14 8 2);
    ;; narrowing to 6..20 leaves 14 and 8 visible and hides 2.
    (narrow-to-region 6 20)
    (goto-char 12)
    (let (shown)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-jump-back))
      ;; Two things asserted: the TOTAL is the visible count, not the
      ;; ring's 3, and the INDEX is 1 -- the first press goes to the most
      ;; recent visible entry.  It used to report 2/2, because the counter
      ;; was advanced before use and the first press skipped an entry.
      (should (equal shown "Position 1/2"))
      (should (<= (point-min) (point)))
      (should (<= (point) (point-max))))))

(ert-deftest donkey-jump-back-unaffected-without-narrowing ()
  "Widened buffers cycle through the whole ring, as before."
  (with-temp-buffer
    (donkey-mode 1)
    (insert "abcdefghij")
    (dolist (p '(2 4 6))
      (goto-char p)
      (donkey--track-position))
    (should (equal (list (progn (donkey-jump-back) (point))
                         (progn (donkey-jump-back) (point))
                         (progn (donkey-jump-back) (point)))
                   ;; Most recent first: 4 was the position just left.
                   '(4 2 4)))))

(ert-deftest donkey-visual-session-ignores-an-anchor-hidden-by-narrowing ()
  "An anchor outside the accessible portion is not a continuable session.

Regression: buffer positions are absolute and narrowing does not move
them, so `V' followed by \\[narrow-to-region] left the anchor pointing at
text the buffer no longer shows.  `goto-char' and `set-mark' both clamp
there rather than signaling, so `J' quietly re-anchored the selection on
the narrowing edge while still presenting itself as the session started
higher up."
  (with-temp-buffer
    (donkey-mode 1)
    (let ((transient-mark-mode t))
      (insert "L1\nL2\nL3\nL4\nL5\nL6\n")
      (goto-char (point-min))
      (forward-line 1)
      (donkey-visual-line-toggle)
      (narrow-to-region 10 19)
      (should-not (donkey--visual-line-session-active-p)))))

(ert-deftest donkey-visual-anchor-survives-widening ()
  "The hidden anchor is ignored, not destroyed -- widening restores it."
  (with-temp-buffer
    (donkey-mode 1)
    (let ((transient-mark-mode t))
      (insert "L1\nL2\nL3\nL4\nL5\nL6\n")
      (goto-char (point-min))
      (forward-line 1)
      (donkey-visual-line-toggle)
      (narrow-to-region 10 19)
      (should-not (donkey--visual-line-session-active-p))
      (widen)
      (should (donkey--visual-line-session-active-p)))))

(ert-deftest donkey-visual-session-started-inside-narrowing-still-extends ()
  "A session whose anchor IS visible keeps working under narrowing."
  (with-temp-buffer
    (donkey-mode 1)
    (let ((transient-mark-mode t))
      (insert "L1\nL2\nL3\nL4\nL5\nL6\n")
      (narrow-to-region 4 16)
      (goto-char (point-min))
      (donkey-visual-line-toggle)
      (should (donkey--visual-line-session-active-p))
      (donkey-visual-next-line 1)
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "L2\nL3")))))

(ert-deftest donkey-jump-back-returns-to-the-position-just-left ()
  "The first press returns to where point was, not one entry further back.

Regression, reported as \"S is one line off\".  The counter was advanced
BEFORE it was used, so the first press skipped the most recent entry --
and since a position is recorded on every movement, the entry before it
is one line away after `j'/`k' and one character away after `h'/`l'.
That made a key whose whole purpose is taking back the jump just made
land consistently just short of it."
  (with-temp-buffer
    (donkey-mode 1)
    (dotimes (i 20) (insert (format "line %02d\n" (1+ i))))
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      ;; walk down to line 5 the way j does, recording each step
      (goto-char (point-min))
      (donkey--track-position)
      (dotimes (_ 4) (forward-line 1) (donkey--track-position))
      (should (= (line-number-at-pos) 5))
      ;; a big jump, then take it back
      (goto-char (point-max))
      (donkey--track-position)
      (donkey-jump-back)
      (should (= (line-number-at-pos) 5)))))

(ert-deftest donkey-jump-back-is-not-one-character-off ()
  "The same off-by-one, as it appears after character-wise motion."
  (with-temp-buffer
    (donkey-mode 1)
    (insert "abcdefghij\n")
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      (goto-char (point-min))
      (donkey--track-position)
      (dotimes (_ 4) (forward-char 1) (donkey--track-position))
      (should (= (point) 5))
      (goto-char (point-max))
      (donkey--track-position)
      (donkey-jump-back)
      (should (= (point) 5)))))

(ert-deftest donkey-jump-back-walks-further-back-on-repeat ()
  "Repeats keep walking back rather than sticking or skipping."
  (with-temp-buffer
    (donkey-mode 1)
    (dotimes (i 20) (insert (format "line %02d\n" (1+ i))))
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      (goto-char (point-min))
      (donkey--track-position)
      (dotimes (_ 4) (forward-line 1) (donkey--track-position))
      (goto-char (point-max))
      (donkey--track-position)
      (should (equal (list (progn (donkey-jump-back) (line-number-at-pos))
                           (progn (donkey-jump-back) (line-number-at-pos))
                           (progn (donkey-jump-back) (line-number-at-pos)))
                     '(5 4 3))))))

(provide 'donkey-navigation-test)

;;; donkey-navigation-test.el ends here

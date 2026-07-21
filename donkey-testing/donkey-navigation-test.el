;;; donkey-navigation-test.el --- Tests for DONKEY navigation commands -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;; Ensure dynamic scoping for position-tracking and visual-selection variables
(defvar donkey--position-ring)
(defvar donkey--position-index)
(defvar donkey--last-tracked-state)
(defvar donkey-position-ring-max)
(defvar donkey-visual-anchor)

;; Helper: go to absolute line N (1-based) in current buffer
(defmacro donkey--goto-line (n)
  `(progn (goto-char (point-min)) (forward-line (1- ,n))))

;; Helper: absolute line-beginning-position for line N
(defmacro donkey--bol (n)
  `(save-excursion (donkey--goto-line ,n) (line-beginning-position)))

;; Helper: absolute line-end-position for line N
(defmacro donkey--eol (n)
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
  "Request line number beyond buffer end: forward-line stops at end."
  (let ((target-line 10))
    (with-temp-buffer
      (insert "one\ntwo\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (donkey-goto-line))
      (should (= (point) (point-max))))))

(ert-deftest donkey-goto-line-read-number-prompted ()
  "User is prompted via read-number."
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
  "Request line 0 (undocumented, unvalidated input): no error, point stays
at point-min since forward-line -1 there has nowhere to go."
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

(ert-deftest donkey-goto-line-preserves-buffer-text ()
  "After goto-line, buffer text is unchanged."
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
      (should (= donkey--position-index 0)))))

(ert-deftest donkey-jump-back-multiple-positions-rotate ()
  "Multiple positions rotate through correctly: visits 3 -> 1 -> 5."
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
      (should (= (point) 3))
      (donkey-jump-back)
      (should (= (point) 1))
      (donkey-jump-back)
      (should (= (point) 5)))))

(ert-deftest donkey-jump-back-wraps-around-ring ()
  "After exhausting ring, wraps back to start."
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
      (donkey-jump-back)
      (should (= (point) 1))
      (donkey-jump-back)
      (should (= (point) 3))
      (donkey-jump-back)
      (should (= (point) 1)))))

(ert-deftest donkey-jump-back-skips-killed-buffer-marker ()
  "Markers in killed buffers are skipped."
  (with-temp-buffer
    (insert "main\n")
    (let ((buf-a (generate-new-buffer "*test-killed-jump*"))
          (donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (goto-char 1)
      (donkey--track-position)
      (set-buffer buf-a)
      (insert "killed\n")
      (goto-char 5)
      (donkey--track-position)
      (kill-buffer buf-a)
      (set-buffer (current-buffer))
      (goto-char 3)
      (donkey--track-position)
      (should (= (length donkey--position-ring) 2))
      (donkey-jump-back)
      (should (= (point) 1)))))

(ert-deftest donkey-jump-back-all-invalid-user-error ()
  "When all markers point to killed buffers, jumping signals an error."
  (let ((buf-a (generate-new-buffer "*test-jump-a*"))
        (buf-b (generate-new-buffer "*test-jump-b*")))
    (unwind-protect
        (progn
          (set-buffer buf-a)
          (insert "a\n")
          (goto-char 1)
          (let ((donkey--position-ring nil)
                (donkey--position-index 0)
                (donkey--last-tracked-state nil)
                (donkey-position-ring-max 10))
            (donkey--track-position)
            (set-buffer buf-b)
            (insert "b\n")
            (goto-char 1)
            (donkey--track-position)
            (kill-buffer buf-a)
            (kill-buffer buf-b)
            (with-temp-buffer
              (insert "temp\n")
              (should-error (donkey-jump-back)))))
      (dolist (b (list buf-a buf-b))
        (when (buffer-live-p b) (kill-buffer b))))))

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

;;; ---------------------------------------------------------------------------
;;; donkey-switch-other-buffer
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-switch-other-buffer-calls-other-buffer ()
  "Calls other-buffer with the current buffer."
  (let (other-arg)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (setq other-arg buf) buf)))
      (donkey-switch-other-buffer))
    (should other-arg)
    (should (eq other-arg (current-buffer)))))

(ert-deftest donkey-switch-other-buffer-call-order ()
  "other-buffer executes before switch-to-buffer."
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
  "Calling twice passes current-buffer each time."
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
  "If other-buffer returns nil, switch-to-buffer receives nil without error."
  (let (switch-arg)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) nil))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (setq switch-arg buf))))
      (donkey-switch-other-buffer))
    (should-not switch-arg)))

(ert-deftest donkey-switch-other-buffer-call-interactively ()
  "Can be called via call-interactively."
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

(ert-deftest donkey-visual-line-toggle-cancels-active-region ()
  "With an active region, deactivates the mark and clears the anchor."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (let ((donkey-visual-anchor (point)))
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
    (let ((donkey-visual-anchor nil))
      (donkey-visual-line-toggle)
      (should (region-active-p))
      (donkey-visual-line-toggle)
      (should-not (region-active-p))
      (should-not donkey-visual-anchor))))

(ert-deftest donkey-visual-line-toggle-call-interactively ()
  "Can be called via call-interactively."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey-visual-anchor nil))
      (call-interactively #'donkey-visual-line-toggle)
      (should (region-active-p)))))

(ert-deftest donkey-visual-anchor-is-buffer-local ()
  "`donkey-visual-anchor' must be buffer-local.  Regression test: it used
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
  "`donkey-visual-anchor' must be cleared whenever the mark is
deactivated, not just when cancelled via `donkey-visual-line-toggle'
itself.  Regression test: if some other command deactivated the mark
(e.g. `keyboard-quit'), the anchor was left stale in the SAME buffer,
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
      (let ((donkey-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-next-line)
        (should (region-active-p))
        (should (= (point) (donkey--bol 3)))
        (should (= (mark) (donkey--eol 3)))))))

(ert-deftest donkey-visual-next-line-above-anchor-extends-selection ()
  "Point above anchor. Move down twice, crossing then passing the anchor."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-next-line)
        (should (= (point) (donkey--bol 3)))
        (should (= (mark) (donkey--eol 3)))
        (donkey-visual-next-line)
        (should (= (point) (donkey--eol 4)))
        (should (= (mark) anchor))))))

(ert-deftest donkey-visual-next-line-below-anchor-sets-mark-to-anchor-beg ()
  "Point below anchor. Moving further down keeps mark pinned to the anchor."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 4)
      (let ((donkey-visual-anchor anchor))
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
      (let ((donkey-visual-anchor anchor))
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
      (let ((donkey-visual-anchor anchor))
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
      (let ((donkey-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (donkey--eol 3)))
        (should (= (mark) anchor))))))

(ert-deftest donkey-visual-previous-line-below-anchor-extends-selection ()
  "Point below anchor. Move up twice, crossing then passing the anchor."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 4)
      (let ((donkey-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (= (point) (donkey--eol 3)))
        (should (= (mark) anchor))
        (donkey-visual-previous-line)
        (should (= (point) (donkey--bol 2)))
        (should (= (mark) (donkey--eol 3)))))))

(ert-deftest donkey-visual-previous-line-above-anchor-sets-mark-to-anchor-eol ()
  "Point at L2, anchor at L3. Move up to L1."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor))
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
    (let ((donkey-visual-anchor (point-min)))
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
      (let ((donkey-visual-anchor anchor))
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
      (let ((donkey-visual-anchor anchor))
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

(provide 'donkey-navigation-test)

;;; donkey-navigation-test.el ends here

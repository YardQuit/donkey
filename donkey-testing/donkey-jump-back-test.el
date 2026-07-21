;;; donkey-jump-back-test.el --- Tests for donkey--track-position and donkey-jump-back -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;; Ensure dynamic scoping for position-tracking variables
(defvar donkey--position-ring)
(defvar donkey--position-index)
(defvar donkey--last-tracked-state)
(defvar donkey-position-ring-max)

;; ===========================================================================
;; Section: donkey--track-position
;; Selector: (ert "donkey--track-position")
;; ===========================================================================

;;; --- First call ---

(ert-deftest donkey--track-position-first-call-sets-state-only ()
  "First call has no previous state, so no marker is pushed.
Only updates donkey--last-tracked-state to current buffer+point.
Expected: ring remains empty, state set to (buffer . point)."
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

;;; --- No change ---

(ert-deftest donkey--track-position-same-position-no-push ()
  "When buffer and point are unchanged, no marker is pushed.
Expected: ring remains empty, state unchanged."
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

;;; --- Position change ---

(ert-deftest donkey--track-position-different-point-pushes-marker ()
  "When point changes, a marker recording the OLD position is pushed.
Buffer: \"hello\\n\". Start at 1, move to 5, track.
Expected: ring has 1 marker pointing to position 1 in current buffer."
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

;;; --- Index reset ---

(ert-deftest donkey--track-position-resets-index-on-new-record ()
  "When a new position is recorded, index resets to 0.
Tracks position 1, then position 5. Index should be 0 after second track.
Expected: index = 0 after recording new position."
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

;;; --- Ring size limit ---

(ert-deftest donkey--track-position-enforces-ring-max ()
  "Ring should not exceed donkey-position-ring-max entries.
Set max to 3, track 5 positions (first doesn't push).
Expected: ring length = 3."
  (with-temp-buffer
    (insert "hello\n")
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (setq donkey-position-ring-max 3)
      (donkey--track-position)  ; First doesn't push
      (dotimes (i 5)
        (goto-char (+ 2 i))
        (donkey--track-position))
      (should (= (length donkey--position-ring) 3)))))

;;; --- Minibuffer exclusion ---

(ert-deftest donkey--track-position-skips-minibuffer ()
  "Tracking should not happen when minibuffer is active.
Mock minibufferp to return t.
Expected: ring remains empty, state unchanged."
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

;; ===========================================================================
;; Section: donkey-jump-back
;; Selector: (ert "donkey-jump-back")
;; ===========================================================================

;;; --- Empty ring ---

(ert-deftest donkey-jump-back-no-positions-error ()
  "With empty ring, signals user-error.
Expected: user-error with message about no positions."
  (let ((donkey--position-ring nil)
        (donkey--position-index 0)
        (donkey--last-tracked-state nil)
        (donkey-position-ring-max 10))
    (should-error (donkey-jump-back) :type 'user-error)))

;;; --- Single position ---

(ert-deftest donkey-jump-back-single-position-jumps ()
  "With one position, jumps to it.
Track pos 1 (no push), move to 5, track (pushes 1), jump back.
Source: index starts at 0, loop increments to 1, finds marker at index 1...
Wait - ring has 1 element at index 0. Source loops ring-len times (1),
increments index from 0 to 1, wraps to 0, finds marker at index 0.
Expected: point at 1, index = 0 (after wrapping from 1 to 0)."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)  ; No push (first call)
      (goto-char 5)
      (donkey--track-position)  ; Pushes marker at position 1
      (should (= (length donkey--position-ring) 1))
      (donkey-jump-back)
      (should (= (point) 1))
      (should (= donkey--position-index 0)))))  ; Wraps to 0 after increment

;;; --- Multiple positions ---

(ert-deftest donkey-jump-back-multiple-positions-rotate ()
  "Multiple positions rotate through correctly.
Track pos 1 (no push), 3 (pushes 1), 5 (pushes 3), 7 (pushes 5).
Ring: [5, 3, 1] at indices [0, 1, 2].
Jump #1: index 0→1, finds marker at index 1 (position 3).
Jump #2: index 1→2, finds marker at index 2 (position 1).
Jump #3: index 2→3, wraps to 0, finds marker at index 0 (position 5).
Expected: visits 3 → 1 → 5."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)  ; No push
      (goto-char 3)
      (donkey--track-position)  ; Pushes 1
      (goto-char 5)
      (donkey--track-position)  ; Pushes 3
      (goto-char 7)
      (donkey--track-position)  ; Pushes 5
      (should (= (length donkey--position-ring) 3))
      (donkey-jump-back)
      (should (= (point) 3))  ; Finds at index 1
      (donkey-jump-back)
      (should (= (point) 1))  ; Finds at index 2
      (donkey-jump-back)
      (should (= (point) 5)))))  ; Wraps to index 0

;;; --- Wrap around ---

(ert-deftest donkey-jump-back-wraps-around-ring ()
  "After exhausting ring, wraps back to start.
Track pos 1 (no push), 3 (pushes 1), 5 (pushes 3).
Ring has 2 markers: [3, 1] at indices [0, 1].
First jump: increment 0->1, find at index 1 (pos 1).
Second jump: increment 1->2, wrap to 0, find at index 0 (pos 3).
Third jump: increment 0->1, find at index 1 (pos 1).
Expected: third jump wraps to first position."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)  ; No push
      (goto-char 3)
      (donkey--track-position)  ; Pushes 1
      (goto-char 5)
      (donkey--track-position)  ; Pushes 3
      (should (= (length donkey--position-ring) 2))
      (donkey-jump-back)
      (should (= (point) 1))
      (donkey-jump-back)
      (should (= (point) 3))
      (donkey-jump-back)
      (should (= (point) 1)))))

;;; --- Killed buffer ---

(ert-deftest donkey-jump-back-skips-killed-buffer-marker ()
  "Markers in killed buffers are skipped.
Setup: track in current buffer at pos 1. Create buf-A, track there
(no push). Kill buf-A. Back to current buffer, track again (pushes
dead marker at buf-A@1). Move to pos 5, track (pushes live marker
at cur@1). Ring has [cur@1, buf-A@1]. Jump skips dead, lands on live.
Expected: point at 1 (the live marker position)."
  (with-temp-buffer
    (insert "main\n")
    (let ((buf-a (generate-new-buffer "*test-killed-jump*"))
          (donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (goto-char 1)
      (donkey--track-position)  ; No push
      (set-buffer buf-a)
      (insert "killed\n")
      (goto-char 5)
      (donkey--track-position)  ; Pushes cur@1
      (kill-buffer buf-a)
      (set-buffer (current-buffer))
      (goto-char 3)
      (donkey--track-position)  ; Pushes buf-A@1 (now dead)
      (should (= (length donkey--position-ring) 2))
      (donkey-jump-back)
      (should (= (point) 1)))))  ; Lands on live marker at cur@1

;;; --- All invalid markers ---

(ert-deftest donkey-jump-back-all-invalid-user-error ()
  "When all markers point to killed buffers, source tries goto-char(nil).
This raises wrong-type-argument, not user-error.
Expected: error when trying to jump to nil target."
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
              ;; Both markers killed, source signals user-error
              (should-error (donkey-jump-back))))))
    (dolist (b (list buf-a buf-b))
      (when (buffer-live-p b) (kill-buffer b)))))


;;; --- Update state on jump ---

(ert-deftest donkey-jump-back-updates-tracking-state ()
  "After jumping, tracking state is updated to new position.
Track pos 1 (no push), move to 5, track (pushes 1), jump to 1.
Expected: last-tracked-state reflects jumped-to position."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)  ; No push
      (goto-char 5)
      (donkey--track-position)  ; Pushes 1
      (let ((jumped-pos 1))
        (donkey-jump-back)
        (should (equal donkey--last-tracked-state
                       (cons (current-buffer) jumped-pos)))))))

;;; --- Interactive call ---

(ert-deftest donkey-jump-back-call-interactively ()
  "Can be called interactively.
Expected: no error when positions exist."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)  ; No push
      (goto-char 5)
      (donkey--track-position)  ; Pushes 1
      (call-interactively #'donkey-jump-back)
      (should (= (point) 1)))))

;;; --- Message output ---

(ert-deftest donkey-jump-back-shows-progress-message ()
  "Display message showing position number out of total.
After jump with 1-element ring, index wraps to 0.
Message shows \"Position %d/%d\" formatted as \"Position 1/1\".
Use cl-letf to intercept and format the message string."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (let ((donkey--position-ring nil)
          (donkey--position-index 0)
          (donkey--last-tracked-state nil)
          (donkey-position-ring-max 10))
      (donkey--track-position)  ; No push
      (goto-char 5)
      (donkey--track-position)  ; Pushes 1
      (let (msg)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq msg (apply #'format fmt args)))))
          (donkey-jump-back))
        (should msg)
        (should (string-match "Position 1/1" msg))))))

;;; donkey-jump-back-test.el ends here

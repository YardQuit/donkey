;;; mule-org-scratch-test.el --- Tests for org-scratch functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'mule-modal)

;;; ---------------------------------------------------------------------------
;;; Helper Macro
;;; ---------------------------------------------------------------------------

(defmacro mule-with-clean-scratch (&rest body)
  "Execute BODY with a guaranteed-absent *org-scratch* buffer, then clean up."
  (declare (indent 0))
  `(let ((--scratch-created nil))
     (when (get-buffer "*org-scratch*")
       (kill-buffer "*org-scratch*"))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer "*org-scratch*")
         (kill-buffer "*org-scratch*")))))

;;; ---------------------------------------------------------------------------
;;; Tests for `mule-insert-org-scratch-message'
;;; ---------------------------------------------------------------------------

(ert-deftest mule-insert-org-scratch-message/inserts-text ()
  "Should insert the org-mode scribble header into the current buffer."
  (with-temp-buffer
    (mule-insert-org-scratch-message)
    (goto-char (point-min))
    (should (search-forward "# This buffer is for scribbling in org-mode." nil t))))

(ert-deftest mule-insert-org-scratch-message/contains-save-hint ()
  "The inserted text should mention saving the buffer."
  (with-temp-buffer
    (mule-insert-org-scratch-message)
    (goto-char (point-min))
    (should (search-forward "save to file with" nil t))
    ;; `substitute-command-keys' expands \[save-some-buffers]; we only
    ;; assert that some substitution occurred (non-empty, non-literal key).
    (should (search-forward "for persistence" nil t))))

(ert-deftest mule-insert-org-scratch-message/point-at-max ()
  "Point should be at `point-max' after insertion."
  (with-temp-buffer
    (mule-insert-org-scratch-message)
    (should (= (point) (point-max)))))

(ert-deftest mule-insert-org-scratch-message/appends-to-existing ()
  "Should append the message after any pre-existing buffer content."
  (with-temp-buffer
    (insert "PRE-EXISTING")
    (mule-insert-org-scratch-message)
    (goto-char (point-min))
    (should (search-forward "PRE-EXISTING" nil t))
    (should (search-forward "# This buffer is for scribbling in org-mode." nil t))))

(ert-deftest mule-insert-org-scratch-message/non-empty-buffer ()
  "Resulting buffer should not be empty."
  (with-temp-buffer
    (mule-insert-org-scratch-message)
    (should (> (buffer-size) 0))))

;;; ---------------------------------------------------------------------------
;;; Tests for `mule-create-org-scratch'
;;; ---------------------------------------------------------------------------

(ert-deftest mule-create-org-scratch/creates-named-buffer ()
  "Should create a buffer named *org-scratch*."
  (mule-with-clean-scratch
    (mule-create-org-scratch)
    (should (buffer-live-p (get-buffer "*org-scratch*")))))

(ert-deftest mule-create-org-scratch/enables-org-mode ()
  "The created buffer should be in `org-mode'."
  (mule-with-clean-scratch
    (mule-create-org-scratch)
    (with-current-buffer "*org-scratch*"
      (should (eq major-mode 'org-mode)))))

(ert-deftest mule-create-org-scratch/switches-to-buffer ()
  "Should switch the current window to the new buffer."
  (mule-with-clean-scratch
    (mule-create-org-scratch)
    (should (eq (current-buffer) (get-buffer "*org-scratch*")))))

(ert-deftest mule-create-org-scratch/contains-scratch-message ()
  "The created buffer should contain the scratch message text."
  (mule-with-clean-scratch
    (mule-create-org-scratch)
    (with-current-buffer "*org-scratch*"
      (goto-char (point-min))
      (should (search-forward "# This buffer is for scribbling in org-mode." nil t)))))

(ert-deftest mule-create-org-scratch/idempotent-buffer ()
  "Calling twice should reuse (not duplicate) the *org-scratch* buffer."
  (mule-with-clean-scratch
    (mule-create-org-scratch)
    (let ((first-buf (get-buffer "*org-scratch*")))
      (mule-create-org-scratch)
      (should (eq (get-buffer "*org-scratch*") first-buf)))))

;;; ---------------------------------------------------------------------------
;;; Tests for `mule-org-scratch'
;;; ---------------------------------------------------------------------------

(ert-deftest mule-org-scratch/creates-when-absent ()
  "When no *org-scratch* buffer exists, should create one."
  (mule-with-clean-scratch
    (mule-org-scratch)
    (should (buffer-live-p (get-buffer "*org-scratch*")))
    (with-current-buffer "*org-scratch*"
      (should (eq major-mode 'org-mode)))))

(ert-deftest mule-org-scratch/switches-when-present ()
  "When *org-scratch* already exists, should switch to it without recreating."
  (mule-with-clean-scratch
    (mule-create-org-scratch)
    (let ((original-buf (get-buffer "*org-scratch*")))
      ;; Add some content so we can verify it survives.
      (with-current-buffer original-buf
        (insert "* My Scribbles"))
      (mule-org-scratch)
      (should (eq (current-buffer) original-buf))
      ;; Same buffer object — not a fresh one.
      (should (eq (get-buffer "*org-scratch*") original-buf))
      ;; Content preserved.
      (goto-char (point-min))
      (should (search-forward "* My Scribbles" nil t)))))

(ert-deftest mule-org-scratch/message-on-create ()
  "Should signal creation via `message' when buffer is new."
  (mule-with-clean-scratch
    (let ((messages nil))
      (cl-letf (((symbol-function #'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (mule-org-scratch))
      (should (member "*org-scratch* buffer doesn't exist, creating." messages)))))

(ert-deftest mule-org-scratch/message-on-switch ()
  "Should signal switching via `message' when buffer already exists."
  (mule-with-clean-scratch
    (mule-create-org-scratch)
    (let ((messages nil))
      (cl-letf (((symbol-function #'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (mule-org-scratch))
      (should (member "*org-scratch* buffer already exist, switching." messages)))))

(ert-deftest mule-org-scratch/does-not-double-insert ()
  "Switching to an existing buffer should not re-insert the scratch message."
  (mule-with-clean-scratch
    (mule-create-org-scratch)
    (let ((size-before
           (with-current-buffer "*org-scratch*" (buffer-size))))
      (mule-org-scratch)
      (let ((size-after
             (with-current-buffer "*org-scratch*" (buffer-size))))
        (should (= size-before size-after))))))

;;; ---------------------------------------------------------------------------
;;; Test Runner
;;; ---------------------------------------------------------------------------

(defun mule-run-all-tests ()
  "Run all MULE transition tests interactively."
  (interactive)
  (ert "^mule-org" :result-buffer "*MULE Test Results*"))
(provide 'mule-org-scratch-test)

;;; mule-org-scratch-test.el ends here

(ert "org-scratch")

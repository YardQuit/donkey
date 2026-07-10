;;; mule-switch-other-buffer-test.el --- Tests for mule-switch-other-buffer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule-switch-other-buffer
;; Selector: (ert "mule-switch-other-buffer")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- Basic functionality (mocked) ---

(ert-deftest mule-switch-other-buffer-calls-other-buffer ()
  "Calls other-buffer with the current buffer.
Expected: other-buffer invoked, receives current-buffer."
  (let (other-arg)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (setq other-arg buf) buf)))
      (mule-switch-other-buffer))
    (should other-arg)
    (should (eq other-arg (current-buffer)))))

(ert-deftest mule-switch-other-buffer-calls-switch-to-buffer ()
  "Passes the result of other-buffer to switch-to-buffer.
Expected: switch-to-buffer invoked with the buffer returned by other-buffer."
  (let (switch-arg returned-buf)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (setq returned-buf buf) buf))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (setq switch-arg buf))))
      (mule-switch-other-buffer))
    (should switch-arg)
    (should (eq switch-arg returned-buf))))

(ert-deftest mule-switch-other-buffer-call-order ()
  "other-buffer executes before switch-to-buffer.
Uses push to track order (newest at front).
Expected: nth 0 = switch (called last), nth 1 = other (called first)."
  (let (order))
  (let (order)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (push 'other order) buf))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (push 'switch order))))
      (mule-switch-other-buffer))
    (should (eq (nth 0 order) 'switch))
    (should (eq (nth 1 order) 'other))
    (should (= (length order) 2))))

(ert-deftest mule-switch-other-buffer-other-buffer-called-once ()
  "other-buffer is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (cl-incf call-count) buf))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) nil)))
      (mule-switch-other-buffer))
    (should (= call-count 1))))

(ert-deftest mule-switch-other-buffer-switch-to-buffer-called-once ()
  "switch-to-buffer is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) buf))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (cl-incf call-count))))
      (mule-switch-other-buffer))
    (should (= call-count 1))))

;;; --- Current buffer passed correctly ---

(ert-deftest mule-switch-other-buffer-passes-current-buffer-explicitly ()
  "other-buffer receives exactly the buffer that was current at call time.
Creates a named buffer to have a distinguishable buffer object.
Expected: other-buffer receives that specific buffer object."
  (let (other-arg test-buf)
    (with-temp-buffer
      (setq test-buf (current-buffer))
      (cl-letf (((symbol-function 'other-buffer)
                 (lambda (buf) (setq other-arg buf) buf))
                ((symbol-function 'switch-to-buffer)
                 (lambda (buf) nil)))
        (mule-switch-other-buffer)))
    (should (eq other-arg test-buf))))

;;; --- Real buffer switching ---

(ert-deftest mule-switch-other-buffer-switches-to-real-buffer ()
  "Verifies other-buffer receives current-buffer and its return value
is passed to switch-to-buffer. Uses mocks since the ERT window is
dedicated and cannot host arbitrary buffers.
Expected: switch-to-buffer receives whatever other-buffer returns."
  (let (switch-arg returned-buf)
    (with-temp-buffer
      (let ((cur (current-buffer)))
        (cl-letf (((symbol-function 'other-buffer)
                   (lambda (buf)
                     (should (eq buf cur))
                     (setq returned-buf (generate-new-buffer " *fake-other*"))
                     returned-buf))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buf) (setq switch-arg buf))))
          (mule-switch-other-buffer))
        (should (eq switch-arg returned-buf))
        (when (buffer-live-p returned-buf) (kill-buffer returned-buf))))))

(ert-deftest mule-switch-other-buffer-switches-back-and-forth ()
  "Verifies that calling twice passes current-buffer each time.
First call returns buf-A; second call (now in buf-A context) returns buf-B.
Expected: switch-to-buffer receives the correct buffer on each call."
  (let ((results)
        (buf-a (generate-new-buffer "*mule-test-aa*"))
        (buf-b (generate-new-buffer "*mule-test-bb*")))
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
            (mule-switch-other-buffer)
            (should (eq (current-buffer) buf-a))
            (mule-switch-other-buffer)
            (should (eq (current-buffer) buf-b))))
      (dolist (b (list buf-a buf-b))
        (when (buffer-live-p b) (kill-buffer b))))))

;;; --- Edge cases ---

(ert-deftest mule-switch-other-buffer-single-buffer ()
  "With only one buffer visible, other-buffer may return the same buffer.
Expected: no error, switch-to-buffer is still called."
  (let (switch-called)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) buf))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (setq switch-called t))))
      (mule-switch-other-buffer))
    (should switch-called)))

(ert-deftest mule-switch-other-buffer-other-buffer-returns-nil ()
  "If other-buffer returns nil, switch-to-buffer receives nil.
The real switch-to-buffer would prompt for a buffer, but we mock it.
Expected: switch-to-buffer called with nil, no error."
  (let (switch-arg)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) nil))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (setq switch-arg buf))))
      (mule-switch-other-buffer))
    (should-not switch-arg)))

;;; --- Interactive call ---

(ert-deftest mule-switch-other-buffer-call-interactively ()
  "Can be called via call-interactively.
Expected: no error, other-buffer and switch-to-buffer execute."
  (let (other-called switch-called)
    (cl-letf (((symbol-function 'other-buffer)
               (lambda (buf) (setq other-called t) buf))
              ((symbol-function 'switch-to-buffer)
               (lambda (buf) (setq switch-called t))))
      (call-interactively #'mule-switch-other-buffer))
    (should other-called)
    (should switch-called)))

;;; --- Ignores prefix arg ---

(ert-deftest mule-switch-other-buffer-ignores-prefix-arg ()
  "Function ignores current-prefix-arg.
Expected: other-buffer called regardless of prefix arg, receives current-buffer."
  (let (other-arg)
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'other-buffer)
                 (lambda (buf) (setq other-arg buf) buf))
                ((symbol-function 'switch-to-buffer)
                 (lambda (buf) nil)))
        (call-interactively #'mule-switch-other-buffer)))
    (should other-arg)
    (should (eq other-arg (current-buffer)))))

;;; mule-switch-other-buffer-test.el ends here

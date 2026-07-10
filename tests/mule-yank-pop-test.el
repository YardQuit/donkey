;;; mule-yank-pop-test.el --- Tests for mule-yank-pop -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule-yank-pop
;; Selector: (ert "mule-yank-pop")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- No region: simple yank-pop ---

(ert-deftest mule-yank-pop-no-region-calls-yank-pop ()
  "Without an active region, calls yank-pop directly.
Expected: yank-pop invoked."
  (let (popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (mule-yank-pop)))
    (should popped)))

(ert-deftest mule-yank-pop-no-region-skips-delete-active-region ()
  "Without an active region, delete-active-region is not called.
Expected: delete-active-region not invoked."
  (let (deleted)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'yank-pop)
                 (lambda () nil)))
        (mule-yank-pop)))
    (should-not deleted)))

(ert-deftest mule-yank-pop-no-region-yank-pop-called-once ()
  "Without region, yank-pop called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'yank-pop)
                 (lambda () (cl-incf call-count))))
        (mule-yank-pop)))
    (should (= call-count 1))))

;;; --- Region active: delete then yank-pop ---

(ert-deftest mule-yank-pop-region-deletes-then-pops ()
  "With an active region, calls delete-active-region then yank-pop.
Expected: both functions called."
  (let (deleted popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (mule-yank-pop)))
    (should deleted)
    (should popped)))

(ert-deftest mule-yank-pop-region-call-order ()
  "With region active, delete-active-region executes before yank-pop.
Uses push to track order (newest at front).
Expected: nth 0 = pop (called last), nth 1 = delete (called first)."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (push 'delete order)))
                ((symbol-function 'yank-pop)
                 (lambda () (push 'pop order))))
        (mule-yank-pop)))
    (should (eq (nth 0 order) 'pop))
    (should (eq (nth 1 order) 'delete))
    (should (= (length order) 2))))

(ert-deftest mule-yank-pop-region-yank-pop-called-once ()
  "With region, yank-pop called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () nil))
                ((symbol-function 'yank-pop)
                 (lambda () (cl-incf call-count))))
        (mule-yank-pop)))
    (should (= call-count 1))))

(ert-deftest mule-yank-pop-region-delete-active-region-called-once ()
  "With region, delete-active-region called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (cl-incf call-count)))
                ((symbol-function 'yank-pop)
                 (lambda () nil)))
        (mule-yank-pop)))
    (should (= call-count 1))))

;;; --- Buffer content verification ---

(ert-deftest mule-yank-pop-no-region-inserts-content ()
  "Without region, yank-pop inserts content at point.
Mock yank-pop to insert \"world\".
Buffer: \"hello\\n\" — point at 1.
After: \"worldhello\\n\" — 10 chars.
Expected: buffer starts with 'world'."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'yank-pop)
               (lambda () (insert "world"))))
      (mule-yank-pop))
    (should (string= (buffer-substring 1 6) "world"))))

(ert-deftest mule-yank-pop-region-replaces-with-content ()
  "With region, deletes region then yank-pops content.
Mock delete-active-region to actually delete, yank-pop to insert.
Buffer: \"hello world\\n\" — 12 chars.
Region: mark at 1, point at 6.
After delete-active-region: \" world\\n\".
After yank-pop (inserts 'hey'): \"hey world\\n\".
Expected: buffer starts with 'hey'."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'delete-active-region)
               (lambda () (delete-region 1 6)))
              ((symbol-function 'yank-pop)
               (lambda () (insert "hey"))))
      (mule-yank-pop))
    (should (string= (buffer-substring 1 4) "hey"))))

;;; --- Edge cases ---

(ert-deftest mule-yank-pop-empty-buffer-no-region ()
  "Empty buffer, no region. yank-pop inserts at point-min.
Mock yank-pop to insert \"text\".
After: \"text\" — 4 chars.
Expected: buffer has 4 chars."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'yank-pop)
               (lambda () (insert "text"))))
      (mule-yank-pop))
    (should (= (buffer-size) 4))
    (should (string= (buffer-string) "text"))))

(ert-deftest mule-yank-pop-empty-buffer-with-region ()
  "Empty buffer cannot have a real region, but mock forces it.
delete-active-region on empty buffer does nothing.
yank-pop inserts 'text'.
Expected: buffer has 'text'."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'delete-active-region)
               (lambda () nil))
              ((symbol-function 'yank-pop)
               (lambda () (insert "text"))))
      (mule-yank-pop))
    (should (= (buffer-size) 4))
    (should (string= (buffer-string) "text"))))

(ert-deftest mule-yank-pop-region-covers-entire-buffer ()
  "Region covers entire buffer. delete-active-region clears it,
then yank-pop inserts replacement.
Buffer: \"hello\\n\" — 6 chars.
Region: mark at 1, point at 7 (point-max).
After: \"world\\n\".
Expected: buffer-string = 'world\\n'."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-max))
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'delete-active-region)
               (lambda () (delete-region 1 7)))
              ((symbol-function 'yank-pop)
               (lambda () (insert "world\n"))))
      (mule-yank-pop))
    (should (string= (buffer-string) "world\n"))))

;;; --- Interactive call ---

(ert-deftest mule-yank-pop-call-interactively-no-region ()
  "Can be called interactively without a region.
Expected: no error, yank-pop executes."
  (let (popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (call-interactively #'mule-yank-pop))
      (should popped))))

(ert-deftest mule-yank-pop-call-interactively-with-region ()
  "Can be called interactively with a region.
Expected: no error, both delete-active-region and yank-pop execute."
  (let (deleted popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (call-interactively #'mule-yank-pop))
      (should deleted)
      (should popped))))

;;; --- Ignores prefix arg ---

(ert-deftest mule-yank-pop-ignores-prefix-arg ()
  "Function ignores current-prefix-arg for path selection.
Expected: yank-pop called regardless of prefix arg."
  (let (popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((current-prefix-arg '(4)))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () nil))
                  ((symbol-function 'yank-pop)
                   (lambda () (setq popped t))))
          (call-interactively #'mule-yank-pop)))
      (should popped))))

;;; mule-yank-pop-test.el ends here

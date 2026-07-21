;;; donkey-yank-test.el --- Tests for donkey-yank -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;; ===========================================================================
;; Section: donkey-yank
;; Selector: (ert "donkey-yank")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- No region: simple yank ---

(ert-deftest donkey-yank-no-region-calls-clipboard-yank ()
  "Without an active region, calls clipboard-yank directly.
Expected: clipboard-yank invoked."
  (let (yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq yanked t))))
        (donkey-yank)))
    (should yanked)))

(ert-deftest donkey-yank-no-region-skips-delete-active-region ()
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
                ((symbol-function 'clipboard-yank)
                 (lambda () nil)))
        (donkey-yank)))
    (should-not deleted)))

(ert-deftest donkey-yank-no-region-clipboard-yank-called-once ()
  "Without region, clipboard-yank called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'clipboard-yank)
                 (lambda () (cl-incf call-count))))
        (donkey-yank)))
    (should (= call-count 1))))

;;; --- Region active: delete then yank ---

(ert-deftest donkey-yank-region-deletes-then-yanks ()
  "With an active region, calls delete-active-region then clipboard-yank.
Expected: both functions called."
  (let (deleted yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq yanked t))))
        (donkey-yank)))
    (should deleted)
    (should yanked)))

(ert-deftest donkey-yank-region-call-order ()
  "With region active, delete-active-region executes before clipboard-yank.
Uses push to track order (newest at front).
Expected: nth 0 = yank (called last), nth 1 = delete (called first)."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (push 'delete order)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (push 'yank order))))
        (donkey-yank)))
    (should (eq (nth 0 order) 'yank))
    (should (eq (nth 1 order) 'delete))
    (should (= (length order) 2))))

(ert-deftest donkey-yank-region-clipboard-yank-called-once ()
  "With region, clipboard-yank called exactly once.
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
                ((symbol-function 'clipboard-yank)
                 (lambda () (cl-incf call-count))))
        (donkey-yank)))
    (should (= call-count 1))))

(ert-deftest donkey-yank-region-delete-active-region-called-once ()
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
                ((symbol-function 'clipboard-yank)
                 (lambda () nil)))
        (donkey-yank)))
    (should (= call-count 1))))

;;; --- Buffer content verification ---

(ert-deftest donkey-yank-no-region-inserts-clipboard-content ()
  "Without region, clipboard-yank inserts clipboard text.
Mock clipboard-yank to insert \"world\".
Buffer: \"hello\\n\" — point at 1.
After: \"worldhello\\n\" — 10 chars.
Expected: buffer starts with 'world'."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "world"))))
      (donkey-yank))
    (should (string= (buffer-substring 1 6) "world"))))

(ert-deftest donkey-yank-region-replaces-with-clipboard-content ()
  "With region, deletes region then yanks clipboard content.
Mock delete-active-region to actually delete, clipboard-yank to insert.
Buffer: \"hello world\\n\" — 12 chars.
Region: mark at 1, point at 6.
After delete-active-region: \" world\\n\".
After clipboard-yank (inserts 'hey'): \"hey world\\n\".
Expected: buffer starts with 'hey'."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'delete-active-region)
               (lambda () (delete-region 1 6)))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "hey"))))
      (donkey-yank))
    (should (string= (buffer-substring 1 4) "hey"))))

(ert-deftest donkey-yank-no-region-preserves-existing-text ()
  "Without region, existing text is preserved; clipboard content
is inserted at point without removing anything.
Mock clipboard-yank to insert \"X\".
Buffer: \"hello\\n\" — point at 3.
After: \"heXllo\\n\".
Expected: 'he' at 1-2, 'llo\\n' at 4-7."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 3)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "X"))))
      (donkey-yank))
    (should (string= (buffer-substring 1 3) "he"))
    (should (string= (buffer-substring 4 8) "llo\n"))))


;;; --- Edge cases ---

(ert-deftest donkey-yank-empty-buffer-no-region ()
  "Empty buffer, no region. clipboard-yank inserts at point-min.
Mock clipboard-yank to insert \"text\".
After: \"text\" — 4 chars.
Expected: buffer has 4 chars."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "text"))))
      (donkey-yank))
    (should (= (buffer-size) 4))
    (should (string= (buffer-string) "text"))))

(ert-deftest donkey-yank-empty-buffer-with-region ()
  "Empty buffer cannot have a real region, but mock forces it.
delete-active-region on empty buffer does nothing.
clipboard-yank inserts 'text'.
Expected: buffer has 'text'."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'delete-active-region)
               (lambda () nil))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "text"))))
      (donkey-yank))
    (should (= (buffer-size) 4))
    (should (string= (buffer-string) "text"))))

(ert-deftest donkey-yank-region-covers-entire-buffer ()
  "Region covers entire buffer. delete-active-region clears it,
then clipboard-yank inserts replacement.
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
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "world\n"))))
      (donkey-yank))
    (should (string= (buffer-string) "world\n"))))

;;; --- Interactive call ---

(ert-deftest donkey-yank-call-interactively-no-region ()
  "Can be called interactively without a region.
Expected: no error, clipboard-yank executes."
  (let (yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq yanked t))))
        (call-interactively #'donkey-yank)))
    (should yanked)))

(ert-deftest donkey-yank-call-interactively-with-region ()
  "Can be called interactively with a region.
Expected: no error, both delete-active-region and clipboard-yank execute."
  (let (deleted yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq yanked t))))
        (call-interactively #'donkey-yank))
      (should deleted)
      (should yanked))))

;;; --- Ignores prefix arg ---

(ert-deftest donkey-yank-ignores-prefix-arg ()
  "Function ignores current-prefix-arg.
Expected: clipboard-yank called regardless of prefix arg."
  (let (yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((current-prefix-arg '(4)))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () nil))
                  ((symbol-function 'clipboard-yank)
                   (lambda () (setq yanked t))))
          (call-interactively #'donkey-yank)))
      (should yanked))))

;;; donkey-yank-test.el ends here

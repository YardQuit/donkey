;;; mule-goto-line-test.el --- Tests for mule-goto-line -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule-goto-line
;; Selector: (ert "mule-goto-line")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- Basic navigation ---

(ert-deftest mule-goto-line-go-to-line-1 ()
  "Go to line 1 in a non-empty buffer.
Buffer: \"line one\\nline two\\n\" — 18 chars.
Expected: point moves to position 1 (point-min)."
  (let ((target-line 1))
    (with-temp-buffer
      (insert "line one\nline two\n")
      (goto-char 10)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 1)))))

(ert-deftest mule-goto-line-go-to-line-2 ()
  "Go to line 2 in a multi-line buffer.
Buffer: \"line one\\nline two\\n\" — Line 1: 1-9, Line 2: 10-18.
Expected: point at position 10 (beginning of line 2)."
  (let ((target-line 2))
    (with-temp-buffer
      (insert "line one\nline two\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 10)))))

(ert-deftest mule-goto-line-go-to-line-3 ()
  "Go to line 3 in a three-line buffer.
Buffer: \"a\\nb\\nc\\n\" — Line 1: 1-2, Line 2: 3-4, Line 3: 5-6.
Expected: point at position 5 (beginning of line 3)."
  (let ((target-line 3))
    (with-temp-buffer
      (insert "a\nb\nc\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 5)))))

(ert-deftest mule-goto-line-from-non-minimum-position ()
  "Go to a previous line when starting from beyond it.
Start at line 3, go to line 1.
Buffer: \"one\\ntwo\\nthree\\n\".
Expected: point at position 1 regardless of starting position."
  (let ((target-line 1))
    (with-temp-buffer
      (insert "one\ntwo\nthree\n")
      (goto-char 10)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 1)))))

(ert-deftest mule-goto-line-from-end-of-buffer ()
  "Go to a middle line when starting from end.
Start at end, go to line 2.
Buffer: \"one\\ntwo\\nthree\\n\" — Line 2 begins at position 5.
Expected: point at position 5."
  (let ((target-line 2))
    (with-temp-buffer
      (insert "one\ntwo\nthree\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 5)))))

;;; --- Boundary conditions ---

(ert-deftest mule-goto-line-line-beyond-buffer ()
  "Request line number beyond buffer end.
Buffer: \"one\\ntwo\\n\" — only 2 lines.
Request: line 10.
forward-line on excess lines stops at end of buffer.
Expected: point at end of buffer (point-max)."
  (let ((target-line 10))
    (with-temp-buffer
      (insert "one\ntwo\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) (point-max))))))

(ert-deftest mule-goto-line-line-at-buffer-end ()
  "Request last line of buffer.
Buffer: \"one\\ntwo\\nthree\\n\" — 3 lines, 14 chars.
Line 1: 1-4, Line 2: 5-8, Line 3: 9-14.
Request: line 3.
Expected: point at position 9 (beginning of line 3)."
  (let ((target-line 3))
    (with-temp-buffer
      (insert "one\ntwo\nthree\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 9)))))

(ert-deftest mule-goto-line-read-number-prompted ()
  "User is prompted via read-number.
This test verifies read-number is called interactively.
Expected: read-number is invoked."
  (let (read-number-called)
    (with-temp-buffer
      (insert "test\n")
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _)
                   (setq read-number-called t)
                   1)))
        (call-interactively #'mule-goto-line))
      (should read-number-called))))

(ert-deftest mule-goto-line-input-is-captured ()
  "The number returned by read-number determines destination.
Mock read-number to return 5. Verify point lands at line 5.
Buffer: \"1\\n2\\n3\\n4\\n5\\n\" — 10 chars.
Line 5 starts at position 9.
Expected: point at position 9."
  (let ((input-number 5))
    (with-temp-buffer
      (insert "1\n2\n3\n4\n5\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) input-number)))
        (mule-goto-line))
      (should (= (point) 9)))))

;;; --- Edge cases ---

(ert-deftest mule-goto-line-empty-buffer-line-1 ()
  "Empty buffer, request line 1.
point-min = point-max = 1. forward-line 0 stays at 1.
Expected: point stays at 1."
  (let ((target-line 1))
    (with-temp-buffer
      (goto-char (point-min))
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 1)))))

(ert-deftest mule-goto-line-single-line-buffer ()
  "Single line buffer without trailing newline.
Buffer: \"hello\" — 5 chars, only one line.
Request: line 1.
Expected: point at position 1."
  (let ((target-line 1))
    (with-temp-buffer
      (insert "hello")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 1)))))

(ert-deftest mule-goto-line-single-line-with-newline ()
  "Single line buffer with trailing newline.
Buffer: \"hello\\n\" — 6 chars.
Request: line 1.
Expected: point at position 1."
  (let ((target-line 1))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 1)))))

(ert-deftest mule-goto-line-request-zero-or-negative ()
  "Request line 0. Source computes (1- 0) = -1 for forward-line.
forward-line -1 from point-min stays at point-min (no lines above).
This documents that the source does not validate input.
Expected: no error, point stays at 1."
  (let ((target-line 0))
    (with-temp-buffer
      (insert "line one\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 1)))))

(ert-deftest mule-goto-line-large-line-number ()
  "Request very large line number.
Emacs gracefully handles overflow by stopping at end of buffer.
Expected: no error, point reaches buffer end."
  (let ((target-line 1000000))
    (with-temp-buffer
      (insert "short\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) (point-max))))))

(ert-deftest mule-goto-line-preserves-buffer-text ()
  "After goto-line, buffer text is unchanged.
Expected: buffer-string equals original text."
  (let ((original-text "original text\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 1)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) 1)))
        (mule-goto-line))
      (should (string= (buffer-string) original-text)))))

;;; --- Algorithm verification ---

(ert-deftest mule-goto-line-goes-to-min-first ()
  "Algorithm first goes to (point-min), then forward-lines.
Starting at end, requesting line 2 should still work correctly.
Buffer: \"first\\nsecond\\nthird\\n\" — 19 chars.
Line 2 starts at position 7.
Expected: point lands at line 2 start (position 7)."
  (let ((target-line 2))
    (with-temp-buffer
      (insert "first\nsecond\nthird\n")
      (goto-char 19)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) target-line)))
        (mule-goto-line))
      (should (= (point) 7)))))

;;; mule-goto-line-test.el ends here

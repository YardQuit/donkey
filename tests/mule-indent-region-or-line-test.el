;;; mule-indent-region-or-line-test.el --- Tests for mule-indent-region-or-line -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule-indent-region-or-line
;; Selector: (ert "mule-indent-region-or-line")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- Path selection: use-region-p ---

(ert-deftest mule-indent-region-or-line-use-region-p-truthy-takes-region-path ()
  "When use-region-p is true, calls indent-region with region bounds.
Buffer: \"line1\\nline2\\nline3\\n\" — 18 chars.
Region: pos 1 to 12.
Expected: indent-region called with (1, 12)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "line1\nline2\nline3\n")
      (goto-char 1)
      (push-mark 12)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 12))))

(ert-deftest mule-indent-region-or-line-use-region-p-falsy-takes-line-path ()
  "When use-region-p is false, calls indent-region with line bounds.
Buffer: \"hello\\n\" — 6 chars.
line-beginning-position = 1, line-end-position = 6.
Expected: indent-region called with (1, 6)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 6))))

;;; --- Region path ---

(ert-deftest mule-indent-region-or-line-indent-single-line-region ()
  "Indent a region covering one line.
Buffer: \"hello\\nworld\\n\" — 12 chars.
Region: pos 1 to 6 (first line).
Expected: indent-region called with (1, 6)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "hello\nworld\n")
      (goto-char 1)
      (push-mark 6)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 6))))

(ert-deftest mule-indent-region-or-line-indent-multi-line-region ()
  "Indent a region covering multiple lines.
Buffer: \"one\\ntwo\\nthree\\n\" — 14 chars, point-max = 15.
Region: pos 1 to 15 (entire buffer).
Expected: indent-region called with (1, 15)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "one\ntwo\nthree\n")
      (goto-char 1)
      (push-mark 15)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 15))))

(ert-deftest mule-indent-region-or-line-indent-partial-line-region ()
  "Indent a region covering part of a line.
Buffer: \"hello world\\n\" — 12 chars.
Region: pos 1 to 6 (just \"hello\").
Expected: indent-region called with (1, 6)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (push-mark 6)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 6))))

(ert-deftest mule-indent-region-or-line-region-from-middle-of-buffer ()
  "Indent region starting from middle of buffer.
Buffer: \"abc\\ndef\\nghi\\n\" — 12 chars.
Region: pos 5 to 8 (middle line only).
Expected: indent-region called with (5, 8)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "abc\ndef\nghi\n")
      (goto-char 5)
      (push-mark 8)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 5))
    (should (= (cadr indented-bounds) 8))))

;;; --- Line path (no region) ---

(ert-deftest mule-indent-region-or-line-indent-current-line-simple ()
  "Indent current line when no region.
Buffer: \"hello\\n\" — 6 chars.
line-beginning-position = 1, line-end-position = 6.
Expected: indent-region called with (1, 6)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 6))))

(ert-deftest mule-indent-region-or-line-indent-current-line-middle ()
  "Indent current line when point is in middle of line.
Buffer: \"hello world\\n\" — 12 chars.
Point at position 6 (between 'hello' and space).
line-beginning-position = 1, line-end-position = 12.
Expected: indent-region called with (1, 12)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 12))))

(ert-deftest mule-indent-region-or-line-indent-second-line ()
  "Indent second line when no region.
Buffer: \"first\\nsecond\\n\" — 13 chars.
Line 1: 1-6, Line 2: 7-13 (newline at 13).
Point at line 2.
line-beginning-position = 7, line-end-position = 13.
Expected: indent-region called with (7, 13)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "first\nsecond\n")
      (goto-char 7)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 7))
    (should (= (cadr indented-bounds) 13))))

(ert-deftest mule-indent-region-or-line-indent-third-line-from-end ()
  "Indent third line, point at start of line.
Buffer: \"a\\nb\\nc\\n\" — 6 chars.
Line 1: 1-2, Line 2: 3-4, Line 3: 5-6.
Point at position 5 (start of \"c\").
line-beginning-position = 5, line-end-position = 6.
Expected: indent-region called with (5, 6)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "a\nb\nc\n")
      (goto-char 5)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 5))
    (should (= (cadr indented-bounds) 6))))

(ert-deftest mule-indent-region-or-line-indent-empty-line ()
  "Indent empty line (just newline).
Buffer: \"\\n\\n\" — 2 chars.
Line 1 is empty (newline at position 1).
line-beginning-position = 1, line-end-position = 1.
Expected: indent-region called with (1, 1)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "\n\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 1))))

(ert-deftest mule-indent-region-or-line-indent-single-char-line ()
  "Indent line with single character.
Buffer: \"x\\n\" — 2 chars.
Point at line 1.
line-beginning-position = 1, line-end-position = 2.
Expected: indent-region called with (1, 2)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "x\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 2))))

(ert-deftest mule-indent-region-or-line-indent-last-line-no-newline ()
  "Indent last line without trailing newline.
Buffer: \"line1\\nline2\" — 11 chars, point-max = 12.
Line 1: 1-6, Line 2: 7-12.
Point at line 2.
line-beginning-position = 7, line-end-position = 12 (point-max).
Expected: indent-region called with (7, 12)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "line1\nline2")
      (goto-char 7)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (mule-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 7))
    (should (= (cadr indented-bounds) 12))))

(ert-deftest mule-indent-region-or-line-indents-whole-buffer-once ()
  "Verify indent-region is called exactly once.
Expected: exactly one call."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "test\n")
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end)
                   (cl-incf call-count)
                   (ignore beg end))))
        (mule-indent-region-or-line)))
    (should (= call-count 1))))

(ert-deftest mule-indent-region-or-line-preserves-buffer-text ()
  "After indent, buffer text is unchanged (mocked indent-region does nothing).
Expected: buffer-string equals original text."
  (let ((original-text "original text\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (ignore beg end))))
        (mule-indent-region-or-line))
      (should (string= (buffer-string) original-text)))))

;;; --- Interactive call ---

(ert-deftest mule-indent-region-or-line-call-interactively ()
  "Can be called interactively via call-interactively.
Expected: no error."
  (with-temp-buffer
    (insert "test\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'indent-region)
               (lambda (beg end) (ignore beg end))))
      (call-interactively #'mule-indent-region-or-line))
    t))

;;; mule-indent-region-or-line-test.el ends here

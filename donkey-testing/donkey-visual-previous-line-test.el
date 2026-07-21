;;; donkey-visual-previous-line-test.el --- Tests for donkey-visual-previous-line -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;; Ensure dynamic scoping for anchor variable
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

;; ===========================================================================
;; Section: donkey-visual-previous-line
;; Selector: (ert "donkey-visual-previous-line")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- No visual selection ---

(ert-deftest donkey-visual-previous-line-no-region-moves-up ()
  "Without visual selection active, just moves up one line.
Buffer: \"line1\\nline2\\nline3\\n\". Point at L3 begin (13).
After: point at L2 begin (7).
Expected: point = 7."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (donkey--goto-line 3)
    (let ((donkey-visual-anchor nil))
      (donkey-visual-previous-line)
      (should (= (point) (donkey--bol 2))))))

(ert-deftest donkey-visual-previous-line-no-anchor-moves-up ()
  "Visual anchor set but no active region - just moves up.
Expected: region inactive, point moved up one line to L2."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (donkey--goto-line 3)
    (let ((donkey-visual-anchor (donkey--bol 3)))
      (donkey-visual-previous-line)
      (should (not (region-active-p)))
      (should (= (point) (donkey--bol 2))))))

(ert-deftest donkey-visual-previous-line-from-top-stays-no-error ()
  "Already at top of buffer. forward-line -1 does NOT signal beginning-of-buffer;
it returns -1 and stays at point-min.
Expected: no error, point unchanged at 1."
  (with-temp-buffer
    (insert "single line\n")
    (donkey--goto-line 1)
    (let ((donkey-visual-anchor nil))
      (donkey-visual-previous-line)
      (should (= (point) 1)))))

;;; --- Visual selection active, moving to/below anchor line ---

(ert-deftest donkey-visual-previous-line-at-anchor-extends-to-line-end ()
  "Point at L4, anchor at L3. Move up to L3 (same as anchor).
line-beginning-position == anchor, so else branch: mark=anchor, point=eol.
Expected: point = L3 eol (17), mark = L3 begin (13)."
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

(ert-deftest donkey-visual-previous-line-from-below-anchor-twice ()
  "Point at L4, anchor at L3. Move up twice.
Jump 1: to L3, mark=anchor(13), point=L3 eol(17).
Jump 2: to L2, line-bol < anchor, mark=anchor eol(17), point=L2 bol(7).
Expected after 2nd: point = 7, mark = 17."
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

(ert-deftest donkey-visual-previous-line-below-anchor-extends-selection ()
  "Point at L4 (below anchor), anchor at L3. Move up twice.
Jump 1: to L3, line-bol == anchor, else branch: mark=anchor(13), point=L3 eol(17).
Jump 2: to L2, line-bol < anchor, if branch: mark=anchor eol(17), point=L2 bol(7).
Expected: point = 7, mark = 17."
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

;;; --- Visual selection active, moving above anchor ---

(ert-deftest donkey-visual-previous-line-above-anchor-sets-mark-to-anchor-eol ()
  "Point at L2, anchor at L3. Move up to L1.
line-bol(1) < anchor(L3 bol), so if branch: mark=anchor eol(17), point=L1 bol(1).
Expected: point = 1, mark = 17."
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

(ert-deftest donkey-visual-previous-line-above-anchor-twice ()
  "Point at L2, anchor at L3. Move up twice.
Jump 1: to L1, mark=anchor eol(17), point=L1 bol(1).
Jump 2: forward-line -1 from L1 stays at L1 (point-min).
line-bol(1) < anchor, so mark=anchor eol(17), point=L1 bol(1).
Expected: point = 1, mark = 17."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\n")
    (let ((anchor (donkey--bol 3)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (= (point) (donkey--bol 1)))
        (donkey-visual-previous-line)
        (should (= (point) (donkey--bol 1)))
        (should (= (mark) (donkey--eol 3)))))))

;;; --- Boundary cases ---

(ert-deftest donkey-visual-previous-line-at-anchor-from-same-line ()
  "Point at L2 (same as anchor), region active. Move up.
forward-line -1 goes to L1. line-bol(1) < anchor(L2 bol), so:
mark=anchor eol(L2 end), point=L1 bol.
Expected: point = 1, mark = L2 eol."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (donkey--bol 2)))
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (donkey--bol 1)))
        (should (= (mark) (donkey--eol 2)))))))

(ert-deftest donkey-visual-previous-line-preserves-buffer-content ()
  "Moving up doesn't modify buffer content.
Expected: buffer-string unchanged."
  (let ((original "hello\nworld\n"))
    (with-temp-buffer
      (insert original)
      (donkey--goto-line 2)
      (let ((donkey-visual-anchor nil))
        (donkey-visual-previous-line)
        (donkey-visual-previous-line))
      (should (string= (buffer-string) original)))))

;;; --- Edge cases ---

(ert-deftest donkey-visual-previous-line-single-line-with-region-no-error ()
  "Single line buffer with visual selection. Point starts at eol (pos 12).
forward-line -1 from eol on line 1 returns -1 and stays at 12 (same pos).
line-bol(1) == anchor, so else branch: mark=anchor, point=eol.
Expected: point stays at 12, mark at 1, region active."
  (with-temp-buffer
    (insert "single line\n")
    (goto-char (point-min))
    (let ((donkey-visual-anchor (point-min)))
      (set-mark (point-min))
      (end-of-line)
      (activate-mark)
      (should (= (point) 12))
      (donkey-visual-previous-line)
      (should (= (point) 12))
      (should (= (mark) 1))
      (should (region-active-p)))))

(ert-deftest donkey-visual-previous-line-empty-lines ()
  "Works correctly with empty lines in buffer.
Buffer: \"hello\\n\\nworld\\n\".
L1: begin=1, end=5. L2: begin=7, end=6 (empty). L3: begin=8, end=12.
Anchor at L1 begin (1). Start at L2, region active. Move up.
forward-line -1 from L2 goes to L1. line-bol(1) == anchor, else branch:
mark=anchor(1), point=L1 eol(5).
Expected: point = 5, mark = 1."
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

;;; --- Interactive call ---

(ert-deftest donkey-visual-previous-line-call-interactively-no-selection ()
  "Can be called interactively without visual selection.
Expected: no error, point moves up to L2."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (donkey--goto-line 3)
    (let ((donkey-visual-anchor nil))
      (call-interactively #'donkey-visual-previous-line)
      (should (= (point) (donkey--bol 2))))))

(ert-deftest donkey-visual-previous-line-call-interactively-with-selection ()
  "Can be called interactively with visual selection active.
Point at L3, anchor at L2. Move up to L2.
Expected: region active, point = L2 eol, mark = L2 begin."
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

;;; --- State management ---

(ert-deftest donkey-visual-previous-line-keeps-anchor-intact ()
  "Anchor position doesn't change during movement.
Expected: donkey-visual-anchor unchanged after operations."
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

(ert-deftest donkey-visual-previous-line-reactivates-mark ()
  "Mark is explicitly re-activated after each move.
Expected: region-active-p true after command."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (donkey--bol 2)))
      (donkey--goto-line 3)
      (let ((donkey-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (donkey-visual-previous-line)
        (should (region-active-p))))))

;;; donkey-visual-previous-line-test.el ends here

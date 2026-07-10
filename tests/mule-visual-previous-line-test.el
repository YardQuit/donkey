;;; mule-visual-previous-line-test.el --- Tests for mule-visual-previous-line -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; Ensure dynamic scoping for anchor variable
(defvar mule-visual-anchor)

;; Helper: go to absolute line N (1-based) in current buffer
(defmacro mule--goto-line (n)
  `(progn (goto-char (point-min)) (forward-line (1- ,n))))

;; Helper: absolute line-beginning-position for line N
(defmacro mule--bol (n)
  `(save-excursion (mule--goto-line ,n) (line-beginning-position)))

;; Helper: absolute line-end-position for line N
(defmacro mule--eol (n)
  `(save-excursion (mule--goto-line ,n) (line-end-position)))

;; ===========================================================================
;; Section: mule-visual-previous-line
;; Selector: (ert "mule-visual-previous-line")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- No visual selection ---

(ert-deftest mule-visual-previous-line-no-region-moves-up ()
  "Without visual selection active, just moves up one line.
Buffer: \"line1\\nline2\\nline3\\n\". Point at L3 begin (13).
After: point at L2 begin (7).
Expected: point = 7."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (mule--goto-line 3)
    (let ((mule-visual-anchor nil))
      (mule-visual-previous-line)
      (should (= (point) (mule--bol 2))))))

(ert-deftest mule-visual-previous-line-no-anchor-moves-up ()
  "Visual anchor set but no active region - just moves up.
Expected: region inactive, point moved up one line to L2."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (mule--goto-line 3)
    (let ((mule-visual-anchor (mule--bol 3)))
      (mule-visual-previous-line)
      (should (not (region-active-p)))
      (should (= (point) (mule--bol 2))))))

(ert-deftest mule-visual-previous-line-from-top-stays-no-error ()
  "Already at top of buffer. forward-line -1 does NOT signal beginning-of-buffer;
it returns -1 and stays at point-min.
Expected: no error, point unchanged at 1."
  (with-temp-buffer
    (insert "single line\n")
    (mule--goto-line 1)
    (let ((mule-visual-anchor nil))
      (mule-visual-previous-line)
      (should (= (point) 1)))))

;;; --- Visual selection active, moving to/below anchor line ---

(ert-deftest mule-visual-previous-line-at-anchor-extends-to-line-end ()
  "Point at L4, anchor at L3. Move up to L3 (same as anchor).
line-beginning-position == anchor, so else branch: mark=anchor, point=eol.
Expected: point = L3 eol (17), mark = L3 begin (13)."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 4)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (mule--eol 3)))
        (should (= (mark) anchor))))))

(ert-deftest mule-visual-previous-line-from-below-anchor-twice ()
  "Point at L4, anchor at L3. Move up twice.
Jump 1: to L3, mark=anchor(13), point=L3 eol(17).
Jump 2: to L2, line-bol < anchor, mark=anchor eol(17), point=L2 bol(7).
Expected after 2nd: point = 7, mark = 17."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 4)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-previous-line)
        (should (= (point) (mule--eol 3)))
        (should (= (mark) anchor))
        (mule-visual-previous-line)
        (should (= (point) (mule--bol 2)))
        (should (= (mark) (mule--eol 3)))))))

(ert-deftest mule-visual-previous-line-below-anchor-extends-selection ()
  "Point at L4 (below anchor), anchor at L3. Move up twice.
Jump 1: to L3, line-bol == anchor, else branch: mark=anchor(13), point=L3 eol(17).
Jump 2: to L2, line-bol < anchor, if branch: mark=anchor eol(17), point=L2 bol(7).
Expected: point = 7, mark = 17."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 4)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-previous-line)
        (should (= (point) (mule--eol 3)))
        (should (= (mark) anchor))
        (mule-visual-previous-line)
        (should (= (point) (mule--bol 2)))
        (should (= (mark) (mule--eol 3)))))))

;;; --- Visual selection active, moving above anchor ---

(ert-deftest mule-visual-previous-line-above-anchor-sets-mark-to-anchor-eol ()
  "Point at L2, anchor at L3. Move up to L1.
line-bol(1) < anchor(L3 bol), so if branch: mark=anchor eol(17), point=L1 bol(1).
Expected: point = 1, mark = 17."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (mule--bol 1)))
        (should (= (mark) (mule--eol 3)))))))

(ert-deftest mule-visual-previous-line-above-anchor-twice ()
  "Point at L2, anchor at L3. Move up twice.
Jump 1: to L1, mark=anchor eol(17), point=L1 bol(1).
Jump 2: forward-line -1 from L1 stays at L1 (point-min).
line-bol(1) < anchor, so mark=anchor eol(17), point=L1 bol(1).
Expected: point = 1, mark = 17."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-previous-line)
        (should (= (point) (mule--bol 1)))
        (mule-visual-previous-line)
        (should (= (point) (mule--bol 1)))
        (should (= (mark) (mule--eol 3)))))))

;;; --- Boundary cases ---

(ert-deftest mule-visual-previous-line-at-anchor-from-same-line ()
  "Point at L2 (same as anchor), region active. Move up.
forward-line -1 goes to L1. line-bol(1) < anchor(L2 bol), so:
mark=anchor eol(L2 end), point=L1 bol.
Expected: point = 1, mark = L2 eol."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (mule--bol 2)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (mule--bol 1)))
        (should (= (mark) (mule--eol 2)))))))

(ert-deftest mule-visual-previous-line-preserves-buffer-content ()
  "Moving up doesn't modify buffer content.
Expected: buffer-string unchanged."
  (let ((original "hello\nworld\n"))
    (with-temp-buffer
      (insert original)
      (mule--goto-line 2)
      (let ((mule-visual-anchor nil))
        (mule-visual-previous-line)
        (mule-visual-previous-line))
      (should (string= (buffer-string) original)))))

;;; --- Edge cases ---

(ert-deftest mule-visual-previous-line-single-line-with-region-no-error ()
  "Single line buffer with visual selection. Point starts at eol (pos 12).
forward-line -1 from eol on line 1 returns -1 and stays at 12 (same pos).
line-bol(1) == anchor, so else branch: mark=anchor, point=eol.
Expected: point stays at 12, mark at 1, region active."
  (with-temp-buffer
    (insert "single line\n")
    (goto-char (point-min))
    (let ((mule-visual-anchor (point-min)))
      (set-mark (point-min))
      (end-of-line)
      (activate-mark)
      (should (= (point) 12))
      (mule-visual-previous-line)
      (should (= (point) 12))
      (should (= (mark) 1))
      (should (region-active-p)))))

(ert-deftest mule-visual-previous-line-empty-lines ()
  "Works correctly with empty lines in buffer.
Buffer: \"hello\\n\\nworld\\n\".
L1: begin=1, end=5. L2: begin=7, end=6 (empty). L3: begin=8, end=12.
Anchor at L1 begin (1). Start at L2, region active. Move up.
forward-line -1 from L2 goes to L1. line-bol(1) == anchor, else branch:
mark=anchor(1), point=L1 eol(5).
Expected: point = 5, mark = 1."
  (with-temp-buffer
    (insert "hello\n\nworld\n")
    (let ((anchor (mule--bol 1)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-previous-line)
        (should (= (point) (mule--eol 1)))
        (should (= (mark) anchor))))))

;;; --- Interactive call ---

(ert-deftest mule-visual-previous-line-call-interactively-no-selection ()
  "Can be called interactively without visual selection.
Expected: no error, point moves up to L2."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (mule--goto-line 3)
    (let ((mule-visual-anchor nil))
      (call-interactively #'mule-visual-previous-line)
      (should (= (point) (mule--bol 2))))))

(ert-deftest mule-visual-previous-line-call-interactively-with-selection ()
  "Can be called interactively with visual selection active.
Point at L3, anchor at L2. Move up to L2.
Expected: region active, point = L2 eol, mark = L2 begin."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (mule--bol 2)))
      (mule--goto-line 3)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (call-interactively #'mule-visual-previous-line)
        (should (region-active-p))
        (should (= (point) (mule--eol 2)))
        (should (= (mark) anchor))))))

;;; --- State management ---

(ert-deftest mule-visual-previous-line-keeps-anchor-intact ()
  "Anchor position doesn't change during movement.
Expected: mule-visual-anchor unchanged after operations."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3))
          (mule-visual-anchor (mule--bol 3)))
      (mule--goto-line 4)
      (set-mark anchor)
      (end-of-line)
      (activate-mark)
      (mule-visual-previous-line)
      (mule-visual-previous-line)
      (should (= mule-visual-anchor anchor)))))

(ert-deftest mule-visual-previous-line-reactivates-mark ()
  "Mark is explicitly re-activated after each move.
Expected: region-active-p true after command."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (mule--bol 2)))
      (mule--goto-line 3)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-previous-line)
        (should (region-active-p))))))

;;; mule-visual-previous-line-test.el ends here

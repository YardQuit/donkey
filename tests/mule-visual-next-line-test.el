;;; mule-visual-next-line-test.el --- Tests for mule-visual-next-line -*- lexical-binding: t; -*-

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
;; Section: mule-visual-next-line
;; Selector: (ert "mule-visual-next-line")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- No visual selection ---

(ert-deftest mule-visual-next-line-no-region-moves-down ()
  "Without visual selection active, just moves down one line.
Buffer: \"line1\\nline2\\nline3\\n\". Point at L1 begin (1).
After: point at L2 begin (7).
Expected: point = 7."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (mule--goto-line 1)
    (let ((mule-visual-anchor nil))
      (mule-visual-next-line)
      (should (= (point) (mule--bol 2))))))

(ert-deftest mule-visual-next-line-no-anchor-moves-down ()
  "Visual anchor set but no active region - just moves down.
Expected: region inactive, point moved down one line to L2."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (mule--goto-line 1)
    (let ((mule-visual-anchor (mule--bol 1)))
      (mule-visual-next-line)
      (should (not (region-active-p)))
      (should (= (point) (mule--bol 2))))))

(ert-deftest mule-visual-next-line-from-bottom-stays-no-error ()
  "Already at bottom of buffer (no trailing newline).
forward-line 1 returns 1 and stays at eol.
Expected: no error, point unchanged."
  (with-temp-buffer
    (insert "single line")
    (goto-char (point-min))
    (end-of-line)
    (let ((eol-pos (point)))
      (let ((mule-visual-anchor nil))
        (mule-visual-next-line)
        (should (= (point) eol-pos))))))

;;; --- Visual selection active, moving to/above anchor line ---

(ert-deftest mule-visual-next-line-at-anchor-extends-to-line-begin ()
  "Point at L2, anchor at L3. Move down to L3 (same as anchor).
Initial: mark=anchor(13), point=L2 eol(12). Region active.
After: forward-line to L3. line-bol(13) > anchor(13)? No (equal).
else branch: mark=anchor eol(17), point=L3 bol(13).
Expected: point = 13, mark = 17."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-next-line)
        (should (region-active-p))
        (should (= (point) (mule--bol 3)))
        (should (= (mark) (mule--eol 3)))))))

(ert-deftest mule-visual-next-line-from-above-anchor-twice ()
  "Point at L2, anchor at L3. Move down twice.
Initial: mark=anchor(13), point=L2 eol(12).
Jump 1: to L3, line-bol(13) == anchor, else: mark=anchor-eol(17), point=L3-bol(13).
Jump 2: to L4, line-bol(19) > anchor, else: mark=anchor(13), point=L4-eol(24).
Expected after 2nd: point = 24, mark = 13."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-next-line)
        (should (= (point) (mule--bol 3)))
        (should (= (mark) (mule--eol 3)))
        (mule-visual-next-line)
        (should (= (point) (mule--eol 4)))
        (should (= (mark) anchor))))))

(ert-deftest mule-visual-next-line-above-anchor-extends-selection ()
  "Point at L2 (above anchor), anchor at L3. Move down twice.
Same as from-above-anchor-twice. Expected: point = 24, mark = 13."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-next-line)
        (should (= (point) (mule--bol 3)))
        (should (= (mark) (mule--eol 3)))
        (mule-visual-next-line)
        (should (= (point) (mule--eol 4)))
        (should (= (mark) anchor))))))

;;; --- Visual selection active, moving below anchor ---

(ert-deftest mule-visual-next-line-below-anchor-sets-mark-to-anchor-beg ()
  "Point at L4, anchor at L3. Move down to L5.
Initial: mark=anchor(13), point=L4 eol(24).
After: forward-line to L5. line-bol(19) > anchor(13)? Yes.
else branch: mark=anchor(13), point=L5-eol(29).
Expected: point = 29, mark = 13."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 4)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-next-line)
        (should (region-active-p))
        (should (= (point) (mule--eol 5)))
        (should (= (mark) anchor))))))

(ert-deftest mule-visual-next-line-below-anchor-twice ()
  "Point at L4, anchor at L3. Move down twice.
Jump 1: to L5, mark=anchor(13), point=L5 eol(30).
Jump 2: forward-line goes past trailing \\n to point-max(31).
line-bol(31) > anchor(13), mark=anchor(13), point=eol(31).
Expected: point = 31, mark = 13."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5\n")
    (let ((anchor (mule--bol 3)))
      (mule--goto-line 4)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-next-line)
        (should (= (point) (mule--eol 5)))
        (should (= (mark) anchor))
        (mule-visual-next-line)
        (should (= (point) (point-max)))
        (should (= (mark) anchor))))))

;;; --- Boundary cases ---

(ert-deftest mule-visual-next-line-at-anchor-from-same-line ()
  "Point at L2 (same as anchor), region active. Move down.
Initial: mark=anchor(7), point=L2 eol(12).
After: forward-line to L3. line-bol(13) > anchor(7)? Yes.
else: mark=anchor(7), point=L3-eol(17).
Expected: point = 17, mark = 7."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (mule--bol 2)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-next-line)
        (should (region-active-p))
        (should (= (point) (mule--eol 3)))
        (should (= (mark) anchor))))))

(ert-deftest mule-visual-next-line-preserves-buffer-content ()
  "Moving down doesn't modify buffer content.
Expected: buffer-string unchanged."
  (let ((original "hello\nworld\n"))
    (with-temp-buffer
      (insert original)
      (mule--goto-line 1)
      (let ((mule-visual-anchor nil))
        (mule-visual-next-line)
        (mule-visual-next-line))
      (should (string= (buffer-string) original)))))

;;; --- Edge cases ---

(ert-deftest mule-visual-next-line-single-line-with-region-no-error ()
  "Single line buffer with visual selection.
forward-line 1 at end of last line moves to point-max.
line-bol(point-max) > anchor, mark=anchor, point=eol.
Expected: no error, region active."
  (with-temp-buffer
    (insert "single line\n")
    (goto-char (point-min))
    (let ((mule-visual-anchor (point-min)))
      (set-mark (point-min))
      (end-of-line)
      (activate-mark)
      (mule-visual-next-line)
      (should (region-active-p))
      (should (= (mark) (point-min))))))

(ert-deftest mule-visual-next-line-empty-lines ()
  "Works correctly with empty lines in buffer.
Buffer: \"hello\\n\\nworld\\n\".
L1: hello\\n = pos 1-6. L2: \\n at pos 7 (empty). L3: world\\n = pos 8-13.
Anchor at L1 begin (1). Start at L2, region active.
Initial: mark=anchor(1), point=L2 eol(7).
After: forward-line to L3. line-bol(8) > anchor(1)? Yes.
else: mark=anchor(1), point=L3-eol(13).
Expected: point = 13, mark = 1."
  (with-temp-buffer
    (insert "hello\n\nworld\n")
    (let ((anchor (mule--bol 1)))
      (mule--goto-line 2)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (mule-visual-next-line)
        (should (= (point) (mule--eol 3)))
        (should (= (mark) anchor))))))

;;; --- Interactive call ---

(ert-deftest mule-visual-next-line-call-interactively-no-selection ()
  "Can be called interactively without visual selection.
Expected: no error, point moves down to L2."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (mule--goto-line 1)
    (let ((mule-visual-anchor nil))
      (call-interactively #'mule-visual-next-line)
      (should (= (point) (mule--bol 2))))))

(ert-deftest mule-visual-next-line-call-interactively-with-selection ()
  "Can be called interactively with visual selection active.
Point at L1, anchor at L2. Move down to L2.
Initial: mark=anchor(7), point=L1 eol(6).
After: forward-line to L2. line-bol(7) > anchor(7)? No (equal).
else: mark=anchor-eol(12), point=L2-bol(7).
Expected: point = 7, mark = 12."
  (with-temp-buffer
    (insert "line1\nline2\nline3\n")
    (let ((anchor (mule--bol 2)))
      (mule--goto-line 1)
      (let ((mule-visual-anchor anchor))
        (set-mark anchor)
        (end-of-line)
        (activate-mark)
        (call-interactively #'mule-visual-next-line)
        (should (region-active-p))
        (should (= (point) (mule--bol 2)))
        (should (= (mark) (mule--eol 2)))))))

;;; --- State management ---

(ert-deftest mule-visual-next-line-keeps-anchor-intact ()
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
      (mule-visual-next-line)
      (mule-visual-next-line)
      (should (= mule-visual-anchor anchor)))))

(ert-deftest mule-visual-next-line-reactivates-mark ()
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
        (mule-visual-next-line)
        (should (region-active-p))))))

;;; mule-visual-next-line-test.el ends here

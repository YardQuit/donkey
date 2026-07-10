;;; mule-insert-end-of-line-test.el --- Tests for mule-insert-end-of-line -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule-insert-end-of-line
;; Selector: (ert "mule-insert-end-of-line")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- Basic functionality ---

(ert-deftest mule-insert-end-of-line-moves-and-enters-insert ()
  "Moves point to end of line, then enters insert mode.
Buffer: \"hello world\\n\" — 12 chars.
Point at 1, moves to 12 (newline at position 12).
Expected: mule-enter-insert called, point at 12."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-insert-end-of-line))
      (should entered)
      (should (= (point) 12)))))

(ert-deftest mule-insert-end-of-line-call-order ()
  "move-end-of-line executes before mule-enter-insert.
Uses push to track order (newest at front).
Expected: nth 0 = enter (called last), nth 1 = eol (called first)."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-eol (symbol-function 'move-end-of-line)))
        (cl-letf (((symbol-function 'move-end-of-line)
                   (lambda (n)
                     (push 'eol order)
                     (funcall orig-eol n)))
                  ((symbol-function 'mule-enter-insert)
                   (lambda ()
                     (push 'enter order))))
          (mule-insert-end-of-line))))
    (should (eq (nth 0 order) 'enter))
    (should (eq (nth 1 order) 'eol))
    (should (= (length order) 2))))

;;; --- Position changes ---

(ert-deftest mule-insert-end-of-line-from-middle-of-line ()
  "Point in middle of line moves to end of line.
Buffer: \"hello world\\n\" — 12 chars.
Point at 6 (space), moves to 12 (newline position).
Expected: point at 12."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 12))))

(ert-deftest mule-insert-end-of-line-from-beginning-of-line ()
  "Point at beginning of line moves to end of line.
Buffer: \"hello world\\n\" — 12 chars.
Point at 1, moves to 12.
Expected: point at 12."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 12))))

(ert-deftest mule-insert-end-of-line-from-second-line ()
  "Point on second line moves to end of second line.
Buffer: \"one\\ntwo\\n\" — 8 chars.
Line 1: 1-4, Line 2: 5-8.
Point at 5, moves to 8 (newline at position 8).
Expected: point at 8."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 8))))

(ert-deftest mule-insert-end-of-line-from-third-line ()
  "Point on third line moves to end of third line.
Buffer: \"a\\nb\\nc\\n\" — 6 chars.
Line 1: 1-2, Line 2: 3-4, Line 3: 5-6.
Point at 5, moves to 6 (newline at position 6).
Expected: point at 6."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 6))))

;;; --- Edge cases ---

(ert-deftest mule-insert-end-of-line-already-at-end ()
  "Point already at end of line stays at same position.
Buffer: \"hello\\n\" — 6 chars.
Point at 6 (newline), stays at 6.
Expected: point at 6, mule-enter-insert still called."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 6)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-insert-end-of-line))
      (should entered)
      (should (= (point) 6)))))

(ert-deftest mule-insert-end-of-line-empty-buffer ()
  "Empty buffer, point at 1.
move-end-of-line stays at 1.
Expected: point at 1, mule-enter-insert called."
  (let (entered)
    (with-temp-buffer
      (goto-char (point-min))
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-insert-end-of-line))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest mule-insert-end-of-line-single-char-buffer ()
  "Single character, no newline.
Buffer: \"x\" — 1 char, point-max = 2.
Point at 1, moves to 2 (point-max).
Expected: point at 2."
  (with-temp-buffer
    (insert "x")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 2))))

(ert-deftest mule-insert-end-of-line-single-char-with-newline ()
  "Single character with newline.
Buffer: \"x\\n\" — 2 chars.
Point at 1, moves to 2 (newline position).
Expected: point at 2."
  (with-temp-buffer
    (insert "x\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 2))))

(ert-deftest mule-insert-end-of-line-empty-line ()
  "Point on an empty line (just newline).
Buffer: \"\\n\\n\" — 2 chars.
Line 1: 1-2 (empty + newline).
Point at 1, stays at 1 (end of empty line is same as beginning).
Expected: point at 1."
  (with-temp-buffer
    (insert "\n\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 1))))

(ert-deftest mule-insert-end-of-line-empty-line-second ()
  "Point on second empty line moves to its end.
Buffer: \"\\n\\n\" — 2 chars.
Point at 2 (start of second empty line), stays at 2.
Expected: point at 2."
  (with-temp-buffer
    (insert "\n\n")
    (goto-char 2)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 2))))

(ert-deftest mule-insert-end-of-line-no-trailing-newline ()
  "Last line without trailing newline.
Buffer: \"one\\ntwo\" — 7 chars, point-max = 8.
Line 1: 1-4, Line 2: 5-8.
Point at 5, moves to 8 (point-max).
Expected: point at 8."
  (with-temp-buffer
    (insert "one\ntwo")
    (goto-char 5)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 8))))

(ert-deftest mule-insert-end-of-line-multi-line-last-char ()
  "Point at last character of a multi-line buffer.
Buffer: \"a\\nb\\nc\\n\" — 6 chars.
Point at 6 (last newline), stays at 6.
Expected: point at 6."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 6))))

;;; --- Trailing whitespace ---

(ert-deftest mule-insert-end-of-line-skips-trailing-whitespace ()
  "move-end-of-line 1 moves to end of visible text, after trailing whitespace.
Buffer: \"hello   \\n\" — 9 chars (hello + 3 spaces + newline).
Point at 1, moves to 9 (after last space, at newline position).
Expected: point at 9."
  (with-temp-buffer
    (insert "hello   \n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 9))))

(ert-deftest mule-insert-end-of-line-with-tabs ()
  "move-end-of-line handles tabs correctly.
Buffer: \"\\thello\\n\" — 7 chars (tab + hello + newline).
Point at 1, moves to 7 (newline position).
Expected: point at 7."
  (with-temp-buffer
    (insert "\thello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-end-of-line))
    (should (= (point) 7))))

;;; --- Buffer integrity ---

(ert-deftest mule-insert-end-of-line-preserves-buffer-text ()
  "After the operation, buffer text is unchanged.
Expected: buffer-string equals original text."
  (let ((original-text "hello world\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 6)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (mule-insert-end-of-line))
      (should (string= (buffer-string) original-text)))))

(ert-deftest mule-insert-end-of-line-calls-eol-exactly-once ()
  "move-end-of-line is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-eol (symbol-function 'move-end-of-line)))
        (cl-letf (((symbol-function 'move-end-of-line)
                   (lambda (n)
                     (cl-incf call-count)
                     (funcall orig-eol n)))
                  ((symbol-function 'mule-enter-insert)
                   (lambda () nil)))
          (mule-insert-end-of-line))))
    (should (= call-count 1))))

(ert-deftest mule-insert-end-of-line-calls-enter-insert-exactly-once ()
  "mule-enter-insert is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (cl-incf call-count))))
        (mule-insert-end-of-line)))
    (should (= call-count 1))))

;;; --- Interactive call ---

(ert-deftest mule-insert-end-of-line-call-interactively ()
  "Can be called via call-interactively.
Expected: no error, point at end of line, insert entered."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'mule-insert-end-of-line))
      (should entered)
      (should (= (point) 12)))))

;;; --- Ignores prefix arg ---

(ert-deftest mule-insert-end-of-line-ignores-prefix-arg ()
  "Function ignores current-prefix-arg.
move-end-of-line is always called with arg 1.
Expected: point at end of line, regardless of prefix arg."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (call-interactively #'mule-insert-end-of-line)))
    (should (= (point) 12))))

;;; mule-insert-end-of-line-test.el ends here

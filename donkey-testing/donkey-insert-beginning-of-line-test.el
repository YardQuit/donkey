;;; donkey-insert-beginning-of-line-test.el --- Tests for donkey-insert-beginning-of-line -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;; ===========================================================================
;; Section: donkey-insert-beginning-of-line
;; Selector: (ert "donkey-insert-beginning-of-line")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- Basic functionality ---

(ert-deftest donkey-insert-beginning-of-line-moves-and-enters-insert ()
  "Moves point to beginning of line, then enters insert mode.
Buffer: \"hello world\\n\" — 12 chars.
Point at 6, moves to 1 (beginning of line).
Expected: donkey-enter-insert called, point at 1."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-beginning-of-line))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-insert-beginning-of-line-call-order ()
  "beginning-of-line executes before donkey-enter-insert.
Uses push to track order (newest at front).
Expected: nth 0 = enter (called last), nth 1 = bol (called first)."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (let ((orig-bol (symbol-function 'beginning-of-line)))
        (cl-letf (((symbol-function 'beginning-of-line)
                   (lambda ()
                     (push 'bol order)
                     (funcall orig-bol)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda ()
                     (push 'enter order))))
          (donkey-insert-beginning-of-line))))
    (should (eq (nth 0 order) 'enter))
    (should (eq (nth 1 order) 'bol))
    (should (= (length order) 2))))

;;; --- Position changes ---

(ert-deftest donkey-insert-beginning-of-line-from-middle-of-line ()
  "Point in middle of line moves to line start.
Buffer: \"hello world\\n\" — 12 chars.
Point at 7 (space), moves to 1.
Expected: point at 1."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 7)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 1))))

(ert-deftest donkey-insert-beginning-of-line-from-end-of-line ()
  "Point at end of line (newline char) moves to line start.
Buffer: \"hello\\n\" — 6 chars.
Point at 6 (newline), moves to 1.
Expected: point at 1."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 1))))

(ert-deftest donkey-insert-beginning-of-line-from-second-line ()
  "Point on second line moves to start of second line.
Buffer: \"one\\ntwo\\n\" — 8 chars.
Line 1: 1-4, Line 2: 5-8.
Point at 7 (within 'two'), moves to 5.
Expected: point at 5."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char 7)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 5))))

(ert-deftest donkey-insert-beginning-of-line-from-third-line ()
  "Point on third line moves to start of third line.
Buffer: \"a\\nb\\nc\\n\" — 6 chars.
Line 1: 1-2, Line 2: 3-4, Line 3: 5-6.
Point at 6 (newline of line 3), moves to 5.
Expected: point at 5."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 5))))

;;; --- Edge cases ---

(ert-deftest donkey-insert-beginning-of-line-already-at-beginning ()
  "Point already at beginning of line stays at same position.
Buffer: \"hello\\n\" — 6 chars.
Point at 1, moves to 1 (no movement).
Expected: point at 1, donkey-enter-insert still called."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-beginning-of-line))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-insert-beginning-of-line-empty-buffer ()
  "Empty buffer, point at 1.
beginning-of-line stays at 1.
Expected: point at 1, donkey-enter-insert called."
  (let (entered)
    (with-temp-buffer
      (goto-char (point-min))
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-beginning-of-line))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-insert-beginning-of-line-single-char-buffer ()
  "Single character, no newline.
Buffer: \"x\" — 1 char, point-max = 2.
Point at 1, stays at 1.
Expected: point at 1."
  (with-temp-buffer
    (insert "x")
    (goto-char 1)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 1))))

(ert-deftest donkey-insert-beginning-of-line-single-char-with-newline ()
  "Single character with newline.
Buffer: \"x\\n\" — 2 chars.
Point at 2 (newline), moves to 1.
Expected: point at 1."
  (with-temp-buffer
    (insert "x\n")
    (goto-char 2)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 1))))

(ert-deftest donkey-insert-beginning-of-line-empty-line ()
  "Point on an empty line (just newline).
Buffer: \"\\n\\n\" — 2 chars.
Line 1: 1-2 (empty + newline), Line 2: 2 (empty line).
Point at 1, stays at 1.
Expected: point at 1."
  (with-temp-buffer
    (insert "\n\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 1))))

(ert-deftest donkey-insert-beginning-of-line-empty-line-second ()
  "Point on second empty line moves to its start.
Buffer: \"\\n\\n\" — 2 chars.
Point at 2 (start of second empty line), stays at 2.
Expected: point at 2."
  (with-temp-buffer
    (insert "\n\n")
    (goto-char 2)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 2))))

(ert-deftest donkey-insert-beginning-of-line-no-trailing-newline ()
  "Last line without trailing newline, point on last line.
Buffer: \"one\\ntwo\" — 7 chars, point-max = 8.
Line 1: 1-4, Line 2: 5-8.
Point at 7 (within 'two'), moves to 5.
Expected: point at 5."
  (with-temp-buffer
    (insert "one\ntwo")
    (goto-char 7)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 5))))

;;; --- Buffer integrity ---

(ert-deftest donkey-insert-beginning-of-line-preserves-buffer-text ()
  "After the operation, buffer text is unchanged.
Expected: buffer-string equals original text."
  (let ((original-text "hello world\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 6)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (donkey-insert-beginning-of-line))
      (should (string= (buffer-string) original-text)))))

(ert-deftest donkey-insert-beginning-of-line-calls-bol-exactly-once ()
  "beginning-of-line is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (let ((orig-bol (symbol-function 'beginning-of-line)))
        (cl-letf (((symbol-function 'beginning-of-line)
                   (lambda ()
                     (cl-incf call-count)
                     (funcall orig-bol)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda () nil)))
          (donkey-insert-beginning-of-line))))
    (should (= call-count 1))))

(ert-deftest donkey-insert-beginning-of-line-calls-enter-insert-exactly-once ()
  "donkey-enter-insert is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (cl-incf call-count))))
        (donkey-insert-beginning-of-line)))
    (should (= call-count 1))))

;;; --- Interactive call ---

(ert-deftest donkey-insert-beginning-of-line-call-interactively ()
  "Can be called via call-interactively.
Expected: no error, point at beginning of line, insert entered."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-insert-beginning-of-line))
      (should entered)
      (should (= (point) 1)))))

;;; --- Ignores prefix arg ---

(ert-deftest donkey-insert-beginning-of-line-ignores-prefix-arg ()
  "Function ignores current-prefix-arg.
Beginning-of-line is always called without argument (defaults to 1).
Expected: point at beginning of line, regardless of prefix arg."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (call-interactively #'donkey-insert-beginning-of-line)))
    (should (= (point) 1))))

;;; donkey-insert-beginning-of-line-test.el ends here

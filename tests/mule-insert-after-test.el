;;; mule-insert-after-test.el --- Tests for mule-insert-after -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule-insert-after
;; Selector: (ert "mule-insert-after")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- Basic functionality ---

(ert-deftest mule-insert-after-moves-forward-and-enters-insert ()
  "Moves point forward by 1, then enters insert mode.
Buffer: \"hello\\n\" — 6 chars.
Point starts at 1, moves to 2.
Expected: mule-enter-insert called, point at 2."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-insert-after))
      (should entered)
      (should (= (point) 2)))))

(ert-deftest mule-insert-after-forward-char-before-enter-insert ()
  "forward-char executes before mule-enter-insert.
If mule-enter-insert moved point, it must not affect the forward-char.
Verified by checking call order.
Expected: forward-char called first, mule-enter-insert second."
  (let (order)
    (with-temp-buffer
      (insert "ab\n")
      (goto-char 1)
      (let ((orig-forward-char (symbol-function 'forward-char)))
        (cl-letf (((symbol-function 'forward-char)
                   (lambda (&optional n)
                     (push 'forward order)
                     (funcall orig-forward-char n)))
                  ((symbol-function 'mule-enter-insert)
                   (lambda ()
                     (push 'enter order))))
          (mule-insert-after))))
    (should (eq (nth 0 order) 'enter))
    (should (eq (nth 1 order) 'forward))))

;;; --- Position changes ---

(ert-deftest mule-insert-after-from-middle-of-line ()
  "Point in middle of line advances by 1.
Buffer: \"hello world\\n\" — 12 chars.
Point at 6 (between 'hello' and space), moves to 7.
Expected: point at 7."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-after))
    (should (= (point) 7))))

(ert-deftest mule-insert-after-from-second-character ()
  "Point at second character advances to third.
Buffer: \"abc\\n\" — 4 chars.
Point at 2, moves to 3.
Expected: point at 3."
  (with-temp-buffer
    (insert "abc\n")
    (goto-char 2)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-after))
    (should (= (point) 3))))

(ert-deftest mule-insert-after-from-last-char-before-newline ()
  "Point at last visible character before newline.
Buffer: \"hello\\n\" — 6 chars.
Point at 5 ('o'), moves to 6 (newline position).
Expected: point at 6."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-after))
    (should (= (point) 6))))

;;; --- Edge cases ---

(ert-deftest mule-insert-after-at-newline-char ()
  "Point at the newline character itself.
Buffer: \"hello\\n\" — 6 chars.
Point at 6 (newline), forward-char moves to 7 (after newline).
Expected: point at 7."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-after))
    (should (= (point) 7))))

(ert-deftest mule-insert-after-at-end-of-buffer-graceful ()
  "Point at point-max: forward-char would error, but condition-case
catches it and execution continues to mule-enter-insert.
Expected: no error, mule-enter-insert called, point stays at point-max."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-insert-after))
      (should entered)
      (should (= (point) (point-max))))))

(ert-deftest mule-insert-after-at-end-of-no-newline-buffer ()
  "Buffer without trailing newline, point at last char.
Buffer: \"hello\" — 5 chars, point-max = 6.
Point at 5 ('o'), moves to 6 (point-max).
Expected: point at 6."
  (with-temp-buffer
    (insert "hello")
    (goto-char 5)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-after))
    (should (= (point) 6))))

(ert-deftest mule-insert-after-single-char-buffer ()
  "Buffer with single character and no newline.
Buffer: \"x\" — 1 char, point-max = 2.
Point at 1, moves to 2 (point-max).
Expected: point at 2, mule-enter-insert called."
  (let (entered)
    (with-temp-buffer
      (insert "x")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-insert-after))
      (should entered)
      (should (= (point) 2)))))

(ert-deftest mule-insert-after-single-char-with-newline ()
  "Buffer with single character and newline.
Buffer: \"x\\n\" — 2 chars.
Point at 1, moves to 2 (newline position).
Expected: point at 2."
  (with-temp-buffer
    (insert "x\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-after))
    (should (= (point) 2))))

(ert-deftest mule-insert-after-multi-line-from-line-start ()
  "Multi-line buffer, starting at beginning of second line.
Buffer: \"one\\ntwo\\n\" — 8 chars.
Point at 5 (start of 'two'), moves to 6.
Expected: point at 6."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-insert-after))
    (should (= (point) 6))))

;;; --- Buffer integrity ---

(ert-deftest mule-insert-after-preserves-buffer-text ()
  "After insert-after, buffer text is unchanged.
Expected: buffer-string equals original text."
  (let ((original-text "hello world\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (mule-insert-after))
      (should (string= (buffer-string) original-text)))))

(ert-deftest mule-insert-after-exactly-one-forward-char ()
  "forward-char is called exactly once with arg 1.
Expected: one call, arg = 1."
  (let (forward-args)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-forward-char (symbol-function 'forward-char)))
        (cl-letf (((symbol-function 'forward-char)
                   (lambda (&optional n)
                     (push n forward-args)
                     (funcall orig-forward-char n)))
                  ((symbol-function 'mule-enter-insert)
                   (lambda () nil)))
          (mule-insert-after))))
    (should (= (length forward-args) 1))
    (should (eq (car forward-args) 1))))

(ert-deftest mule-insert-after-mule-enter-insert-called-once ()
  "mule-enter-insert is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (cl-incf call-count))))
        (mule-insert-after)))
    (should (= call-count 1))))

;;; --- Interactive call ---

(ert-deftest mule-insert-after-call-interactively ()
  "Can be called via call-interactively.
Expected: no error, both forward-char and mule-enter-insert execute."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'mule-insert-after))
      (should entered)
      (should (= (point) 2)))))

;;; --- Ignores prefix arg ---

(ert-deftest mule-insert-after-ignores-prefix-arg ()
  "Function always moves exactly 1 char forward regardless of prefix arg.
The function does not use current-prefix-arg.
Expected: point advances by 1, not by 4."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (call-interactively #'mule-insert-after)))
    (should (= (point) 2))))

;;; mule-insert-after-test.el ends here

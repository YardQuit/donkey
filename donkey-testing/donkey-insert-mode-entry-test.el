;;; donkey-insert-mode-entry-test.el --- Tests for DONKEY commands entering INSERT state -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'donkey)
(require 'donkey-test-keys)

(defvar rectangle-mark-mode)

;;; ---------------------------------------------------------------------------
;;; donkey-insert-here
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-insert-here-enters-insert ()
  "Calling donkey-insert-here enters insert mode without moving point."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-here))
      (should entered)
      (should (= (point) 3)))))

(ert-deftest donkey-insert-here-deactivates-active-region ()
  "When `mark-active' and a region is active, deactivates the mark."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (push-mark 6)
      (cl-letf (((symbol-function 'mark-active) t)
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t)))
                ((symbol-function 'donkey-enter-insert) (lambda () nil)))
        (donkey-insert-here))
      (should deactivated))))

(ert-deftest donkey-insert-here-skips-deactivate-without-region ()
  "When no region is active, the function `deactivate-mark' is not called."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t)))
                ((symbol-function 'donkey-enter-insert) (lambda () nil)))
        (donkey-insert-here))
      (should-not deactivated))))

(ert-deftest donkey-insert-here-preserves-buffer-text ()
  "Buffer contents are unchanged after calling donkey-insert-here."
  (let ((original "unchanged text\n"))
    (with-temp-buffer
      (insert original)
      (goto-char 5)
      (cl-letf (((symbol-function 'donkey-enter-insert) (lambda () nil)))
        (donkey-insert-here))
      (should (string= (buffer-string) original)))))

(ert-deftest donkey-insert-here-empty-buffer ()
  "Works without error in an empty buffer."
  (let (entered)
    (with-temp-buffer
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-here))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-insert-here-call-interactively ()
  "Can be called via `call-interactively' without error."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-insert-here))
      (should entered)
      (should (= (point) 3)))))

;;; ---------------------------------------------------------------------------
;;; donkey-insert-after
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-insert-after-moves-forward-and-enters-insert ()
  "Moves point forward by 1, then enters insert mode."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-after))
      (should entered)
      (should (= (point) 2)))))

(ert-deftest donkey-insert-after-forward-char-before-enter-insert ()
  "`forward-char' executes before donkey-enter-insert."
  (let (order)
    (with-temp-buffer
      (insert "ab\n")
      (goto-char 1)
      (let ((orig-forward-char (symbol-function 'forward-char)))
        (cl-letf (((symbol-function 'forward-char)
                   (lambda (&optional n)
                     (push 'forward order)
                     (funcall orig-forward-char n)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda ()
                     (push 'enter order))))
          (donkey-insert-after))))
    (should (eq (nth 0 order) 'enter))
    (should (eq (nth 1 order) 'forward))))

(ert-deftest donkey-insert-after-from-middle-of-line ()
  "Point in middle of line advances by 1."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-after))
    (should (= (point) 7))))

(ert-deftest donkey-insert-after-at-end-of-buffer-graceful ()
  "Inserting after point at `point-max' still enters Insert state.

At point-max, forward-char's end-of-buffer error is caught and
donkey-enter-insert still runs."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-after))
      (should entered)
      (should (= (point) (point-max))))))

(ert-deftest donkey-insert-after-at-end-of-no-newline-buffer ()
  "Buffer without trailing newline, point at last char, advances to point-max."
  (with-temp-buffer
    (insert "hello")
    (goto-char 5)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-after))
    (should (= (point) 6))))

(ert-deftest donkey-insert-after-single-char-buffer ()
  "Buffer with single character and no newline."
  (let (entered)
    (with-temp-buffer
      (insert "x")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-after))
      (should entered)
      (should (= (point) 2)))))

(ert-deftest donkey-insert-after-preserves-buffer-text ()
  "After insert-after, buffer text is unchanged."
  (let ((original-text "hello world\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (donkey-insert-after))
      (should (string= (buffer-string) original-text)))))

(ert-deftest donkey-insert-after-exactly-one-forward-char ()
  "`forward-char' is called exactly once with arg 1."
  (let (forward-args)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-forward-char (symbol-function 'forward-char)))
        (cl-letf (((symbol-function 'forward-char)
                   (lambda (&optional n)
                     (push n forward-args)
                     (funcall orig-forward-char n)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda () nil)))
          (donkey-insert-after))))
    (should (= (length forward-args) 1))
    (should (eq (car forward-args) 1))))

(ert-deftest donkey-insert-after-call-interactively ()
  "Can be called via `call-interactively'."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-insert-after))
      (should entered)
      (should (= (point) 2)))))

(ert-deftest donkey-insert-after-ignores-prefix-arg ()
  "Always moves exactly 1 char forward regardless of prefix arg."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (call-interactively #'donkey-insert-after)))
    (should (= (point) 2))))

(ert-deftest donkey-insert-after-deactivates-active-region ()
  "When a non-empty region is active, deactivates the mark before proceeding."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t)))
                ((symbol-function 'donkey-enter-insert) (lambda () nil)))
        (donkey-insert-after))
      (should deactivated))))

(ert-deftest donkey-insert-after-skips-deactivate-when-no-region ()
  "When no region is active, the function `deactivate-mark' is not called.

Regression test: donkey-insert-after previously called the plain
function `deactivate-mark' unconditionally, inconsistent with its sibling
Insert-entry commands (donkey-insert-here, -beginning-of-line,
-end-of-line, donkey-open-below/-above), all of which use
`donkey--deactivate-region-if-active' and so leave an EMPTY active
region alone.  Confirmed live: pushing an empty active region (mark ==
point) then calling donkey-insert-after left `mark-active' nil
afterward, unlike every sibling command in the same category."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t)))
                ((symbol-function 'donkey-enter-insert) (lambda () nil)))
        (donkey-insert-after))
      (should-not deactivated))))

;;; ---------------------------------------------------------------------------
;;; donkey-insert-beginning-of-line
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-insert-beginning-of-line-moves-and-enters-insert ()
  "Moves point to beginning of line, then enters insert mode."
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
  "Point moves to beginning-of-line before Insert state is entered."
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

(ert-deftest donkey-insert-beginning-of-line-from-second-line ()
  "Point on second line moves to start of second line."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char 7)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 5))))

(ert-deftest donkey-insert-beginning-of-line-already-at-beginning ()
  "Point already at beginning of line stays at same position."
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
  "Empty buffer, point stays at 1."
  (let (entered)
    (with-temp-buffer
      (goto-char (point-min))
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-beginning-of-line))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-insert-beginning-of-line-no-trailing-newline ()
  "Last line without trailing newline, point on last line."
  (with-temp-buffer
    (insert "one\ntwo")
    (goto-char 7)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-beginning-of-line))
    (should (= (point) 5))))

(ert-deftest donkey-insert-beginning-of-line-preserves-buffer-text ()
  "Buffer text is unchanged."
  (let ((original-text "hello world\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 6)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (donkey-insert-beginning-of-line))
      (should (string= (buffer-string) original-text)))))

(ert-deftest donkey-insert-beginning-of-line-call-interactively ()
  "Can be called via `call-interactively'."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-insert-beginning-of-line))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-insert-beginning-of-line-ignores-prefix-arg ()
  "Ignores `current-prefix-arg'."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (call-interactively #'donkey-insert-beginning-of-line)))
    (should (= (point) 1))))

;;; ---------------------------------------------------------------------------
;;; donkey-insert-end-of-line
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-insert-end-of-line-moves-and-enters-insert ()
  "Moves point to end of line, then enters insert mode."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-end-of-line))
      (should entered)
      (should (= (point) 12)))))

(ert-deftest donkey-insert-end-of-line-call-order ()
  "`move-end-of-line' executes before donkey-enter-insert."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-eol (symbol-function 'move-end-of-line)))
        (cl-letf (((symbol-function 'move-end-of-line)
                   (lambda (n)
                     (push 'eol order)
                     (funcall orig-eol n)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda ()
                     (push 'enter order))))
          (donkey-insert-end-of-line))))
    (should (eq (nth 0 order) 'enter))
    (should (eq (nth 1 order) 'eol))
    (should (= (length order) 2))))

(ert-deftest donkey-insert-end-of-line-from-second-line ()
  "Point on second line moves to end of second line."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-end-of-line))
    (should (= (point) 8))))

(ert-deftest donkey-insert-end-of-line-already-at-end ()
  "Point already at end of line stays at same position."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 6)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-end-of-line))
      (should entered)
      (should (= (point) 6)))))

(ert-deftest donkey-insert-end-of-line-empty-buffer ()
  "Empty buffer, point stays at 1."
  (let (entered)
    (with-temp-buffer
      (goto-char (point-min))
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-insert-end-of-line))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-insert-end-of-line-no-trailing-newline ()
  "Last line without trailing newline moves to point-max."
  (with-temp-buffer
    (insert "one\ntwo")
    (goto-char 5)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-end-of-line))
    (should (= (point) 8))))

(ert-deftest donkey-insert-end-of-line-skips-trailing-whitespace ()
  "`move-end-of-line' moves past trailing whitespace to the newline position."
  (with-temp-buffer
    (insert "hello   \n")
    (goto-char 1)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-end-of-line))
    (should (= (point) 9))))

(ert-deftest donkey-insert-end-of-line-with-tabs ()
  "`move-end-of-line' handles tabs correctly."
  (with-temp-buffer
    (insert "\thello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-end-of-line))
    (should (= (point) 7))))

(ert-deftest donkey-insert-end-of-line-preserves-buffer-text ()
  "Buffer text is unchanged."
  (let ((original-text "hello world\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 6)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (donkey-insert-end-of-line))
      (should (string= (buffer-string) original-text)))))

(ert-deftest donkey-insert-end-of-line-call-interactively ()
  "Can be called via `call-interactively'."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-insert-end-of-line))
      (should entered)
      (should (= (point) 12)))))

(ert-deftest donkey-insert-end-of-line-ignores-prefix-arg ()
  "Ignores `current-prefix-arg'."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 1)
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (call-interactively #'donkey-insert-end-of-line)))
    (should (= (point) 12))))

;;; ---------------------------------------------------------------------------
;;; donkey-open-above
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-open-above-opens-line-above-and-enters-insert ()
  "Moves to bol, inserts newline, moves up, indents, enters insert."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-open-above))
      (should entered)
      (should (= (point) 1))
      (should (= (buffer-size) 7)))))

(ert-deftest donkey-open-above-call-order ()
  "Executes bol, newline, `forward-line' -1, indent, then enter-insert."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (let ((orig-bol (symbol-function 'move-beginning-of-line))
            (orig-forward-line (symbol-function 'forward-line)))
        (cl-letf (((symbol-function 'region-active-p)
                   (lambda () nil))
                  ((symbol-function 'move-beginning-of-line)
                   (lambda (n)
                     (push 'bol order)
                     (funcall orig-bol n)))
                  ((symbol-function 'newline-and-indent)
                   (lambda ()
                     (push 'newline order)
                     (insert "\n")))
                  ((symbol-function 'forward-line)
                   (lambda (n)
                     (push 'forward-line order)
                     (funcall orig-forward-line n)))
                  ((symbol-function 'indent-according-to-mode)
                   (lambda ()
                     (push 'indent order)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda ()
                     (push 'enter order))))
          (donkey-open-above))))
    (should (eq (nth 0 order) 'enter))
    (should (eq (nth 1 order) 'indent))
    (should (eq (nth 2 order) 'forward-line))
    (should (eq (nth 3 order) 'newline))
    (should (eq (nth 4 order) 'bol))
    (should (= (length order) 5))))

(ert-deftest donkey-open-above-deactivates-active-region ()
  "When region is active, deactivates the mark before proceeding."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t)))
                ((symbol-function 'newline-and-indent)
                 (lambda () (insert "\n")))
                ((symbol-function 'forward-line)
                 (lambda (n) (ignore n)))
                ((symbol-function 'indent-according-to-mode)
                 (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (donkey-open-above)))
    (should deactivated)))

(ert-deftest donkey-open-above-skips-deactivate-when-no-region ()
  "When no region is active, the function `deactivate-mark' is not called."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t)))
                ((symbol-function 'newline-and-indent)
                 (lambda () (insert "\n")))
                ((symbol-function 'forward-line)
                 (lambda (n) (ignore n)))
                ((symbol-function 'indent-according-to-mode)
                 (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (donkey-open-above)))
    (should-not deactivated)))

(ert-deftest donkey-open-above-from-second-line ()
  "Point on second line; opens above second line."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-open-above))
    (should (= (point) 5))))

(ert-deftest donkey-open-above-on-empty-buffer ()
  "Empty buffer; creates first line above."
  (let (entered)
    (with-temp-buffer
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-open-above))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-open-above-on-line-without-newline ()
  "Buffer without trailing newline; adds newline above."
  (let (entered)
    (with-temp-buffer
      (insert "hello")
      (goto-char 3)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-open-above))
      (should entered)
      (should (= (point) 1)))))

(ert-deftest donkey-open-above-preserves-existing-content ()
  "Existing content is preserved; only a new line is added above."
  (with-temp-buffer
    (insert "first\nsecond\n")
    (goto-char 3)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-open-above))
    (should (string= (buffer-substring 2 7) "first"))
    (should (string= (buffer-substring 8 14) "second"))))

(ert-deftest donkey-open-above-forward-line-called-with-minus-one ()
  "`forward-line' is called with argument -1 to move up after inserting."
  (let (forward-arg)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-forward-line (symbol-function 'forward-line)))
        (cl-letf (((symbol-function 'newline-and-indent)
                   (lambda () (insert "\n")))
                  ((symbol-function 'forward-line)
                   (lambda (n)
                     (setq forward-arg n)
                     (funcall orig-forward-line n)))
                  ((symbol-function 'indent-according-to-mode)
                   (lambda () nil))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda () nil)))
          (donkey-open-above))))
    (should (eq forward-arg -1))))

(ert-deftest donkey-open-above-call-interactively ()
  "Can be called via `call-interactively'."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-open-above))
      (should entered)
      (should (= (point) 1)))))

;;; ---------------------------------------------------------------------------
;;; donkey-open-below
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-open-below-moves-to-eol-newlines-and-enters-insert ()
  "Moves to end of line, inserts newline+indent, enters insert mode."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-open-below))
      (should entered)
      (should (= (point) 7))
      (should (= (buffer-size) 7)))))

(ert-deftest donkey-open-below-call-order ()
  "Executes eol, newline, then enter-insert, in that order."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-eol (symbol-function 'move-end-of-line))
            (orig-newline (symbol-function 'newline-and-indent)))
        (cl-letf (((symbol-function 'region-active-p)
                   (lambda () nil))
                  ((symbol-function 'move-end-of-line)
                   (lambda (n)
                     (push 'eol order)
                     (funcall orig-eol n)))
                  ((symbol-function 'newline-and-indent)
                   (lambda ()
                     (push 'newline order)
                     (funcall orig-newline)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda ()
                     (push 'enter order))))
          (donkey-open-below))))
    (should (eq (nth 0 order) 'enter))
    (should (eq (nth 1 order) 'newline))
    (should (eq (nth 2 order) 'eol))
    (should (= (length order) 3))))

(ert-deftest donkey-open-below-deactivates-active-region ()
  "When region is active, deactivates the mark before proceeding."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t)))
                ((symbol-function 'newline-and-indent)
                 (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (donkey-open-below)))
    (should deactivated)))

(ert-deftest donkey-open-below-skips-deactivate-when-no-region ()
  "When no region is active, the function `deactivate-mark' is not called."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t)))
                ((symbol-function 'newline-and-indent)
                 (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (donkey-open-below)))
    (should-not deactivated)))

(ert-deftest donkey-open-below-from-second-line ()
  "Point on second line; opens below second line."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-open-below))
    (should (= (point) 9))))

(ert-deftest donkey-open-below-on-empty-buffer ()
  "Empty buffer; creates first line."
  (let (entered)
    (with-temp-buffer
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-open-below))
      (should entered)
      (should (= (point) 2)))))

(ert-deftest donkey-open-below-preserves-existing-content ()
  "Existing content is preserved; only a new line is added."
  (with-temp-buffer
    (insert "first\nsecond\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-open-below))
    (should (string= (buffer-substring 1 6) "first"))
    (should (string= (buffer-substring 8 14) "second"))))

(ert-deftest donkey-open-below-call-interactively ()
  "Can be called via `call-interactively'."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-open-below))
      (should entered)
      (should (= (point) 7)))))

(ert-deftest donkey-a-counted-open-below-adds-blank-lines-under-the-cursor ()
  "`C-u 3 o' opens the usual line and two empty lines under it.

The cursor stays on the line a bare press opens, next to the line it
came from; the extra lines are the room asked for, beyond it."
  (donkey-test-keys--harness "*donkey-open-count*" #'text-mode ()
      "abc\ndef\n" "C-u 3 o"
    (should (equal (buffer-string) "abc\n\n\n\ndef\n"))
    (should (= (point) 5))
    (should (bound-and-true-p donkey-insert-mode))))

(ert-deftest donkey-a-counted-open-above-adds-blank-lines-over-the-cursor ()
  "`C-u 3 O' opens the usual line above and two empty lines above that.

The cursor stays on the line directly above the one it came from."
  (donkey-test-keys--harness "*donkey-open-count*" #'text-mode ()
      "abc\ndef\n" "j C-u 3 O"
    (should (equal (buffer-string) "abc\n\n\n\ndef\n"))
    (should (= (point) 7))
    (should (bound-and-true-p donkey-insert-mode))))

(ert-deftest donkey-a-counted-open-indents-only-the-cursor-line ()
  "Under a mode that indents, the blanks stay blank and the cursor line does not.

Only the line to be typed on is opened by the mode's indentation; an
empty line left with whitespace on it is exactly the stray the old
docstring objected to."
  (donkey-test-keys--harness "*donkey-open-count*" #'emacs-lisp-mode ()
      "(progn\n  (a)\n  (b))\n" "j C-u 2 o"
    (should (equal (buffer-string) "(progn\n  (a)\n  \n\n  (b))\n"))
    (should (= (point) 16)))
  (donkey-test-keys--harness "*donkey-open-count*" #'emacs-lisp-mode ()
      "(progn\n  (a)\n  (b))\n" "j j C-u 2 O"
    (should (equal (buffer-string) "(progn\n  (a)\n\n  \n  (b))\n"))
    (should (= (point) 17))))

(ert-deftest donkey-open-keys-read-a-count-below-one-as-one ()
  "`C-u 0 o', `C-u - 2 o' and their `O' twins open one line, as a bare press does."
  (dolist (keys '("C-u 0 o" "C-u - 2 o"))
    (donkey-test-keys--harness "*donkey-open-count*" #'text-mode ()
        "abc\ndef\n" keys
      (should (equal (buffer-string) "abc\n\ndef\n"))
      (should (= (point) 5))))
  (dolist (keys '("C-u 0 O" "C-u - 2 O"))
    (donkey-test-keys--harness "*donkey-open-count*" #'text-mode ()
        "abc\ndef\n" (concat "j " keys)
      (should (equal (buffer-string) "abc\n\ndef\n"))
      (should (= (point) 5)))))

;;; ---------------------------------------------------------------------------
;;; donkey-change
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-change-no-region-deletes-single-char ()
  "Without an active region, deletes the character at point and inserts.

Asserts the effect rather than which primitive does it -- see the
matching `donkey-delete' test."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-change))
      (should (equal (buffer-string) "ello\n"))
      (should entered))))

(ert-deftest donkey-change-no-region-from-middle ()
  "Deletes the character at point in the middle of a line."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 3)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-change))
    (should (= (buffer-size) 5))
    (should (= (point) 3))))

(ert-deftest donkey-change-no-region-empty-buffer-still-enters-insert ()
  "Change in an empty buffer still enters Insert state.

Regression test: an empty buffer has nothing to delete, but `c' must
still enter INSERT state rather than aborting in Normal state.

`delete-char' signals `end-of-buffer' here.  That used to propagate
out of `donkey-change' before `donkey-enter-insert' ran, so pressing
\"change\" left the user in Normal state with only an \"End of
buffer\" message to explain it -- entering INSERT is this command's
entire purpose.  Confirmed live in `emacs -nw': the modeline stayed
on DONKEY[N]."
  (let (entered)
    (with-temp-buffer
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-change))
      (should entered)
      (should (string= (buffer-string) "")))))

(ert-deftest donkey-change-no-region-at-end-of-buffer-still-enters-insert ()
  "Change at `point-max' still enters Insert state.

Point at `point-max' with no region: nothing is deleted, but INSERT
state is still entered.  See
`donkey-change-no-region-empty-buffer-still-enters-insert'."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-change))
      (should entered)
      ;; Buffer untouched -- the guard swallows the error, it does not
      ;; delete something else instead.
      (should (string= (buffer-string) "hello\n")))))

(ert-deftest donkey-change-no-region-mid-buffer-still-deletes ()
  "Change mid-buffer still deletes the character at point.

The end-of-buffer guard must not suppress the ordinary delete: with a
character actually present at point, `c' still removes exactly it."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-change))
      (should entered)
      (should (string= (buffer-string) "ello\n")))))

(ert-deftest donkey-change-region-kills-region ()
  "With an active region (not rectangle), KILLS from mark to point.

`kill-region' rather than `delete-region', so what the change replaced
can be pasted back -- the same store `donkey-delete' fills for the same
selection.  This test asserted the call to `delete-region' and so kept
passing for as long as `c' saved nothing at all; the `kill-ring' check
below is the part that would have noticed."
  (let (entered deleted-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (let ((orig-kill-region (symbol-function 'kill-region))
            (kill-ring nil) (kill-ring-yank-pointer nil)
            (select-enable-clipboard nil) (interprogram-cut-function nil))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () t))
                  ((symbol-function 'kill-region)
                   (lambda (beg end &optional region)
                     (setq deleted-bounds (list beg end))
                     (funcall orig-kill-region beg end region)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (let ((rectangle-mark-mode nil))
            (donkey-change)))
        (should entered)
        (should deleted-bounds)
        (should (= (car deleted-bounds) 1))
        (should (= (cadr deleted-bounds) 6))
        (should (equal (car kill-ring) "hello"))))))

(ert-deftest donkey-change-region-skips-delete-char ()
  "With an active region, `delete-char' is not called."
  (let (delete-char-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-region)
                 (lambda (beg end) (ignore beg end)))
                ((symbol-function 'delete-char)
                 (lambda (n) (setq delete-char-called t)))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (donkey-change)))
      (should-not delete-char-called))))

(ert-deftest donkey-change-region-point-before-mark ()
  "Region with point before mark: `kill-region' receives (mark, point)."
  (let (deleted-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (push-mark 6)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'kill-region)
                 (lambda (beg end &optional _region)
                   (setq deleted-bounds (list beg end))))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (donkey-change)))
      (should (= (car deleted-bounds) 6))
      (should (= (cadr deleted-bounds) 1)))))

(ert-deftest donkey-change-rectangle-mode-calls-string-rectangle ()
  "Change over a rectangle delegates to `string-rectangle'.

With region active and `rectangle-mark-mode' enabled, delegates to
`string-rectangle' via `call-interactively'."
  (let (called-cmd)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd)))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode t))
          (donkey-change)))
      (should (eq called-cmd 'string-rectangle)))))

(ert-deftest donkey-change-rectangle-mode-skips-delete-region ()
  "In rectangle mode, `delete-region' is not called."
  (let (delete-region-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (ignore cmd)))
                ((symbol-function 'delete-region)
                 (lambda (beg end) (setq delete-region-called t)))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode t))
          (donkey-change)))
      (should-not delete-region-called))))

(ert-deftest donkey-change-rectangle-mode-falls-back-when-disabled ()
  "Change over a plain region falls back to `kill-region'.

When `rectangle-mark-mode' is nil and region is active, falls back to
`kill-region' rather than `string-rectangle'."
  (let (delete-called ci-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq ci-called t)))
                ((symbol-function 'kill-region)
                 (lambda (beg end &optional _region)
                   (ignore beg end)
                   (setq delete-called t)))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (donkey-change)))
      (should delete-called)
      (should-not ci-called))))

(ert-deftest donkey-change-rectangle-mode-stays-in-normal-state ()
  "Change over a rectangle ends in Normal state.

Regression test: under `rectangle-mark-mode', `c' must end in NORMAL
state, not INSERT.

`string-rectangle' prompts for the replacement text itself and applies
it to every covered line, so by the time it returns the edit is
already finished and there is nothing left to type.  Entering INSERT
there meant the next navigation keypress self-inserted instead of
moving.  Confirmed live in `emacs -nw': after `m v', `c', a
replacement string and RET, pressing `j' then `l' typed a literal
\"jl\" into the buffer."
  (with-temp-buffer
    (donkey-normal-mode 1)
    (insert "hello\n")
    (goto-char 1)
    (push-mark 3)
    (cl-letf (((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'call-interactively) (lambda (_cmd) nil)))
      (let ((rectangle-mark-mode t))
        (donkey-change)))
    (should (bound-and-true-p donkey-normal-mode))
    (should-not (bound-and-true-p donkey-insert-mode))))

(ert-deftest donkey-change-plain-region-still-enters-insert-state ()
  "Change over a plain region still enters Insert state.

The rectangle special case must not change the ordinary path: a plain
\(non-rectangle) active region still deletes and enters INSERT, since
there the user does still have to type the replacement."
  (with-temp-buffer
    (donkey-normal-mode 1)
    (insert "hello\n")
    (goto-char 1)
    (push-mark 3)
    (cl-letf (((symbol-function 'use-region-p) (lambda () t)))
      (let ((rectangle-mark-mode nil))
        (donkey-change)))
    (should (bound-and-true-p donkey-insert-mode))
    (should-not (bound-and-true-p donkey-normal-mode))))

(ert-deftest donkey-change-no-region-preserves-surrounding-text ()
  "Deleting one char leaves the rest of the buffer intact."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-change))
    (should (string= (buffer-substring 1 6) "hello"))
    (should (string= (buffer-substring 6 11) "world"))))

(ert-deftest donkey-change-region-deletes-correct-text ()
  "Deleting a region removes exactly the marked text."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (let ((rectangle-mark-mode nil))
        (donkey-change)))
    (should (string= (buffer-substring 1 7) " world"))))

(ert-deftest donkey-change-call-interactively-no-region ()
  "Can be called interactively without a region."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-change))
      (should entered)
      (should (= (buffer-size) 5)))))

(ert-deftest donkey-change-call-interactively-with-region ()
  "Can be called interactively with a region."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (let ((rectangle-mark-mode nil))
          (call-interactively #'donkey-change)))
      (should entered)
      (should (= (buffer-size) 7)))))

;;; ---------------------------------------------------------------------------
;;; donkey-wrap-region
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-wrap-region-no-region-falls-through-to-undefined ()
  "With no active region, delegates to `undefined' and does nothing else."
  (let (undefined-called insert-mode-called self-insert-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'undefined)
                 (lambda () (interactive) (setq undefined-called t)))
                ((symbol-function 'donkey-insert-mode)
                 (lambda (&rest _) (setq insert-mode-called t)))
                ((symbol-function 'self-insert-command)
                 (lambda (&rest _) (setq self-insert-called t))))
        (donkey-wrap-region))
      (should undefined-called)
      (should-not insert-mode-called)
      (should-not self-insert-called))))

(defvar donkey-wrap-test--fell-through nil
  "Set by the stand-in command a wrap key can fall back to.")

(defvar donkey-wrap-test--typed nil
  "Set by the stand-in command that stands for a mode that types.")

(defun donkey-wrap-test--fall-back ()
  "Stand in for a command a major mode puts on a wrap key."
  (interactive)
  (setq donkey-wrap-test--fell-through t))

(defun donkey-wrap-test--type ()
  "Stand in for a major mode command that types, without typing."
  (interactive)
  (setq donkey-wrap-test--typed t))

(define-derived-mode donkey-wrap-fallback-test-mode text-mode "Fallback"
  "A stand-in mode with commands of its own on two wrap keys.

`+' is what a mode like Dired puts on a key DONKEY borrows for
wrapping; `/' is what a mode like `html-mode' puts there, a command
that types.")

(keymap-set donkey-wrap-fallback-test-mode-map "+" 'donkey-wrap-test--fall-back)
(keymap-set donkey-wrap-fallback-test-mode-map "/" 'donkey-wrap-test--type)

(ert-deftest donkey-a-wrap-key-with-nothing-to-wrap-is-the-buffers-again ()
  "In a buffer that cannot be edited, a wrap key runs what the mode put there.

Most of the punctuation is a wrap key now, and a wrap key is borrowed
only for as long as a selection lasts.  Dired binds `+', Info binds
`[', and pressing one with nothing selected has to reach them."
  (setq donkey-wrap-test--fell-through nil)
  (donkey-test-keys--harness "*donkey-wrap-fallback*"
      #'donkey-wrap-fallback-test-mode
      () "row" ""
    (setq buffer-read-only t)
    (execute-kbd-macro (kbd "+"))
    (should donkey-wrap-test--fell-through)
    (should (equal (buffer-string) "row"))))

(ert-deftest donkey-a-wrap-key-with-nothing-to-wrap-stays-donkeys-where-text-can-be-typed ()
  "In a buffer that can be edited, a wrap key with no selection does nothing.

The mode is not asked at all there, so a command of its own that types
never gets the chance -- which is the whole reason the fall-back is
read-only buffers and not every buffer."
  (setq donkey-wrap-test--fell-through nil)
  (cl-letf (((symbol-function 'ding) #'ignore))
    (donkey-test-keys--harness "*donkey-wrap-fallback*"
        #'donkey-wrap-fallback-test-mode
        () "row" "+"
      (should-not donkey-wrap-test--fell-through)
      (should (equal (buffer-string) "row"))
      (should (equal donkey-test-keys--said "+ is undefined")))))

(ert-deftest donkey-a-wrap-key-never-falls-back-on-a-command-that-types ()
  "A fall-back that types is refused, read-only buffer or not.

`donkey-self-insert-commands' is asked a second time here, where the
key came through the wrap rather than through a remap: a read-only
buffer would refuse the insertion itself, but Normal state answers
before the buffer has to.  The stand-in command sets a flag instead of
typing, so what is asserted is that it was never CALLED and not merely
that the buffer survived it."
  (setq donkey-wrap-test--typed nil)
  (cl-letf (((symbol-function 'ding) #'ignore))
    (let ((donkey-self-insert-commands
           (cons 'donkey-wrap-test--type donkey-self-insert-commands)))
      (donkey-test-keys--harness "*donkey-wrap-fallback*"
          #'donkey-wrap-fallback-test-mode
          () "row" ""
        (setq buffer-read-only t)
        (execute-kbd-macro (kbd "/"))
        (should-not donkey-wrap-test--typed)
        (should (equal (buffer-string) "row"))))))

(ert-deftest donkey-a-wrap-hands-back-the-pressed-key-and-nothing-else ()
  "Only the press of a single delimiter is handed back to the buffer.

A reader may put `donkey-wrap-region' on a sequence of their own, and
`M-x' reaches it too.  Neither is a delimiter press, so neither is
handed anywhere: what the rest of such a sequence means to the buffer
is not this command to decide."
  (setq donkey-wrap-test--fell-through nil)
  ;; The WHOLE of `C-c' is put back, not just the key under it: binding
  ;; a sequence makes a prefix keymap, and an emptied one left behind
  ;; still answers `C-c' instead of letting it through.
  (let ((was (keymap-lookup donkey-normal-mode-map "C-c")))
    (unwind-protect
        (cl-letf (((symbol-function 'ding) #'ignore))
          (keymap-set donkey-normal-mode-map "C-c w" #'donkey-wrap-region)
          (keymap-set donkey-wrap-fallback-test-mode-map "C-c w"
                      'donkey-wrap-test--fall-back)
          (donkey-test-keys--harness "*donkey-wrap-fallback*"
              #'donkey-wrap-fallback-test-mode
              () "row" ""
            (setq buffer-read-only t)
            (execute-kbd-macro (kbd "C-c w"))
            (should-not donkey-wrap-test--fell-through)
            (should (equal (buffer-string) "row"))))
      (keymap-unset donkey-wrap-fallback-test-mode-map "C-c w")
      (if was
          (keymap-set donkey-normal-mode-map "C-c" was)
        (keymap-unset donkey-normal-mode-map "C-c")))))

(ert-deftest donkey-the-delimiters-donkey-holds-itself-are-not-reported ()
  "The default says nothing about the two table pairs it cannot wrap.

`donkey-wrap-delimiters' is `all', so the characters come from
`donkey-mark-pair-delimiters', a table `m i' reads first.  Two of them
are keys DONKEY answers itself -- `:' goes to a line, `>' indents --
and a report that named them would name them in every session anybody
ever started.  A reader who names characters outright is asking, and
is answered."
  (should (equal (donkey--delimiters-that-cannot-wrap) nil))
  (let ((donkey-wrap-delimiters (list ?: ?<)))
    (should (equal (mapcar #'car (donkey--delimiters-that-cannot-wrap))
                   '(?: ?>)))))

(ert-deftest donkey-wrap-region-rectangle-mark-mode-wraps-each-line ()
  "Wrapping a rectangle wraps each of its lines.

Regression test: with `rectangle-mark-mode' active, wraps each line
of the rectangle at its own start/end column instead of delegating to
`self-insert-command' or `undefined'.

`self-insert-command' operates on `region-beginning'/`region-end' as a
single linear span.  Run directly against a rectangle selection, that
used to insert the delimiters at the rectangle's linear start/end
buffer positions instead of on each covered line, corrupting the
buffer -- confirmed live: a rectangle spanning columns 0-1 across
three lines produced \"(aaa\\nbbb\\nc)cc\\n\".  Falling through to
`undefined' was a safe stopgap for that corruption, but
`donkey--wrap-rectangle-region' now wraps every line correctly instead
of doing nothing."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "aaa\n" "bbb\n" "ccc\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 2)
      (forward-char 1)
      (rectangle-mark-mode 1)
      (let ((last-command-event ?\())
        (donkey-wrap-region))
      (should (string= (buffer-string) "(a)aa\n(b)bb\n(c)cc\n")))))

(ert-deftest donkey-wrap-region-rectangle-mark-mode-symmetric-delimiter ()
  "A symmetric delimiter wraps a rectangle with the same character.

A symmetric delimiter (not a recognized pair in
`donkey-mark-pair-delimiters', e.g. `\"') wraps each rectangle line
with the SAME character on both sides."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "aaa\n" "bbb\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 1)
      (forward-char 1)
      (rectangle-mark-mode 1)
      (let ((last-command-event ?\"))
        (donkey-wrap-region))
      (should (string= (buffer-string) "\"a\"aa\n\"b\"bb\n")))))

(ert-deftest donkey-wrap-region-rectangle-mark-mode-pads-short-lines ()
  "Rectangle wrapping pads lines shorter than the rectangle.

A line shorter than the rectangle's columns is padded with spaces up
to each column before wrapping, same as `string-rectangle-line' and
other rectangle commands do for short lines, rather than bunching both
delimiters together at end of line."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "aaaa\n" "bb\n" "cccc\n")
      (goto-char (point-min))
      (forward-char 2)
      (push-mark (point) t t)
      (forward-line 2)
      (forward-char 4)
      (rectangle-mark-mode 1)
      (let ((last-command-event ?\())
        (donkey-wrap-region))
      (should (string= (buffer-string) "aa(aa)\nbb(  )\ncc(cc)\n")))))

(ert-deftest donkey-wrap-region-rectangle-mark-mode-stays-active ()
  "Rectangle wrapping leaves the rectangle selection active.

It does not deactivate the mark or exit
`rectangle-mark-mode', matching the linear case's own
\"does-not-deactivate-mark-itself\" behavior."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "aaa\n" "bbb\n" "ccc\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 2)
      (forward-char 1)
      (rectangle-mark-mode 1)
      (let ((last-command-event ?\())
        (donkey-wrap-region))
      (should (bound-and-true-p rectangle-mark-mode))
      (should (use-region-p)))))

(ert-deftest donkey-wrap-close-char-uses-mark-pair-delimiters ()
  "`donkey--wrap-close-char' resolves the close side of a pair.

It resolves the close side of a recognized
`donkey-mark-pair-delimiters' entry, falling back to OPEN-CHAR itself
when it is not a recognized pair (symmetric delimiters, or any
character a user has not added to `donkey-mark-pair-delimiters')."
  (should (equal (donkey--wrap-close-char ?\() ?\)))
  (should (equal (donkey--wrap-close-char ?\[) ?\]))
  (should (equal (donkey--wrap-close-char ?\") ?\"))
  (should (equal (donkey--wrap-close-char ?!) ?!)))

(ert-deftest donkey-a-delegated-wrap-enters-insert-inserts-then-exits ()
  "Delegating, a wrap enters Insert, self-inserts, then leaves for Normal.

With an active region and `donkey-wrap-region-engine' set to
`pairing-package', enters Insert, self-inserts, then leaves for
Normal, in that order -- through `donkey--leave-insert', the state
change alone, and not `donkey--exit-insert', which is the `C-g' key
and stops a recording macro on its way out.  DONKEY's own wrap enters
no state at all."
  (let (calls (donkey-wrap-region-engine 'pairing-package))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      ;; The delimiter comes from `last-command-event'; bound here because
      ;; `donkey-wrap-region' now falls through for a non-character event.
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'donkey-insert-mode)
                 (lambda (&rest _) (push 'insert-mode calls)))
                ((symbol-function 'self-insert-command)
                 (lambda (&rest _) (push 'self-insert calls)))
                ((symbol-function 'donkey--leave-insert)
                 (lambda (&optional _keep) (push 'leave-insert calls))))
        (let ((last-command-event ?\())
          (donkey-wrap-region)))
      (should (equal (nreverse calls) '(insert-mode self-insert leave-insert))))))

(ert-deftest donkey-a-wrap-key-does-not-stop-a-recording-macro ()
  "A wrap key pressed while a keyboard macro is recording records on.

Regression: `donkey-wrap-region' returned to Normal through
`donkey--exit-insert', the `C-g' key, whose errands include stopping a
recording macro -- so `v w (' inside \\[kmacro-start-macro] ended the
recording silently, while the rectangle path, which never enters
INSERT, recorded on.  Real keys, with a real recording started around
them: the variable `defining-kbd-macro' must still be non-nil once the
wrap is done, and the wrap itself must have happened."
  (unwind-protect
      (progn
        (start-kbd-macro nil)
        (donkey-test-keys--harness "*donkey-wrap-macro*" #'text-mode
            ((donkey-wrap-region-engine 'pairing-package))
            "alpha beta\n" "v w ("
          (should defining-kbd-macro)
          (should (equal (buffer-string) "alpha( beta\n"))
          (should (bound-and-true-p donkey-normal-mode))
          (should-not (bound-and-true-p donkey-insert-mode))))
    (when defining-kbd-macro
      (end-kbd-macro))))

(ert-deftest donkey-a-delegated-wrap-does-not-deactivate-mark-itself ()
  "Delegating, a wrap does not deactivate the mark itself.

It does not call the function `deactivate-mark' before self-inserting --
the region must stay active for packages hooking
`self-insert-command' (such as Smartparens' region-wrap) to see it."
  (let (deactivated (donkey-wrap-region-engine 'pairing-package))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'donkey-insert-mode) (lambda (&rest _) nil))
                ((symbol-function 'self-insert-command) (lambda (&rest _) nil))
                ((symbol-function 'donkey--leave-insert) (lambda (&optional _keep) nil))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t))))
        (donkey-wrap-region))
      (should-not deactivated))))

(ert-deftest donkey-a-delegated-wrap-with-nothing-pairing-inserts-at-point ()
  "Delegating with no pairing package, the character lands at point.

End-to-end with a real (non-mocked) self-insert-command: nothing is
listening, so the pressed character lands in the buffer at point, same
as ordinary self-insert, and the selection is not wrapped.  This is
what `donkey-wrap-region-engine' set to `pairing-package' promises and
the whole of what it can promise."
  (with-temp-buffer
    (let ((transient-mark-mode t)
          (donkey-wrap-region-engine 'pairing-package)
          (donkey-mode t))
      (insert "hello world")
      (goto-char 1)
      (push-mark (point) t t)
      (goto-char 6)
      (let ((last-command-event ?\())
        (donkey-wrap-region))
      (should (string= (buffer-string) "hello( world")))))

(ert-deftest donkey-wrap-region-wraps-region-with-electric-pair-mode ()
  "With `electric-pair-mode', the region is really wrapped.

With Emacs's built-in `electric-pair-mode', the selection is really
WRAPPED rather than the delimiter merely inserted at point.

`donkey-wrap-region' deliberately keeps the region active across its
`self-insert-command' call so a pairing package can act on it.  The
README used to describe that as needing Smartparens; `electric-pair-mode'
is enough and ships with Emacs.  Confirmed live in `emacs -nw':
selecting \"hello\" and pressing `(' produced \"(hello) world\".

Locks the capability in so it cannot regress silently -- e.g. if the
region were ever deactivated before the insertion, this would fall
back to the bare \"hello( world\" of the test above."
  (with-temp-buffer
    (let ((transient-mark-mode t)
          (donkey-wrap-region-engine 'pairing-package)
          (donkey-mode t))
      (electric-pair-local-mode 1)
      (unwind-protect
          (progn
            (insert "hello world")
            (goto-char 1)
            (push-mark (point) t t)
            (goto-char 6)
            (let ((last-command-event ?\())
              (donkey-wrap-region))
            (should (string= (buffer-string) "(hello) world")))
        (electric-pair-local-mode -1)))))

(ert-deftest donkey-a-counted-delegated-wrap-inserts-one-and-loses-nothing ()
  "Delegating, a count before a wrap delimiter wraps once and deletes nothing.

Pinned two ways.  A function on `post-self-insert-hook' records
`current-prefix-arg' as the delimiter is inserted, and it must be nil
whatever count the keys carried -- that is what discriminates on every
Emacs, not only where the damage shows.  And the buffer itself: under
`electric-pair-local-mode', each of the counted key runs must end in
the plain wrap, the same text a bare delimiter press produces.

Regression test.  Emacs 31's `electric-pair-post-self-insert-function'
reads `current-prefix-arg' for itself to learn how many characters the
self-insert put down, and deletes that many before wrapping.  Handed a
live count together with `self-insert-command' 1, it deleted text the
user never typed: v w \\[universal-argument] 3 ( on \"alpha beta\" left
\"(((alp))) beta\", a negative count ate the space after the selection,
a count of 9 signaled args-out-of-range.  Emacs 30 hard-codes one and
never showed it, so the recorder is the part of this test that fails
there."
  (dolist (case '(("v w C-u 3 (" . "(alpha) beta")
                  ("m w C-u 3 (" . "(alpha) beta")
                  ("v w C-u 0 (" . "(alpha) beta")
                  ("v w C-u - (" . "(alpha) beta")
                  ("v w C-u 3 \"" . "\"alpha\" beta")))
    (let (seen)
      (donkey-test-keys--harness "*donkey-wrap-count*"
          (lambda () (fundamental-mode) (electric-pair-local-mode 1))
          ((donkey-wrap-region-engine 'pairing-package)
           (post-self-insert-hook
            (cons (lambda () (push current-prefix-arg seen))
                  post-self-insert-hook)))
          "alpha beta" (car case)
        (should (equal (cons (car case) (buffer-string)) case))
        ;; The hook runs once per character electric-pair puts down, so
        ;; SEEN has several entries; every one of them must be nil.
        (should (equal (cons (car case) (delete-dups seen))
                       (list (car case) nil)))))))

(ert-deftest donkey-wrap-region-delete-selection-mode-does-not-eat-region ()
  "`delete-selection-mode' must not swallow the selection here.

That mode acts from `pre-command-hook' on `this-command's
`delete-selection' property, and `this-command' is `donkey-wrap-region'
\(no such property), not the `self-insert-command' invoked from inside
it -- so the region survives to be wrapped instead of being replaced
by the delimiter.  Confirmed live in `emacs -nw' with
`delete-selection-mode' enabled."
  (with-temp-buffer
    (let ((transient-mark-mode t)
          (donkey-wrap-region-engine 'pairing-package)
          (donkey-mode t))
      (insert "hello world")
      (goto-char 1)
      (push-mark (point) t t)
      (goto-char 6)
      (let ((last-command-event ?\()
            (delete-selection-mode t))
        (donkey-wrap-region))
      ;; "hello" still present -- not replaced by "(".
      (should (string= (buffer-string) "hello( world")))))

(ert-deftest donkey-wrap-region-returns-to-normal-state ()
  "After wrapping, DONKEY ends up back in Normal state, not stuck in Insert."
  (with-temp-buffer
    (donkey-normal-mode 1)
    (let ((transient-mark-mode t)
          (donkey-mode t))
      (insert "hello")
      (goto-char 1)
      (push-mark (point) t t)
      (goto-char 3)
      (let ((last-command-event ?\())
        (donkey-wrap-region))
      (should (bound-and-true-p donkey-normal-mode))
      (should-not (bound-and-true-p donkey-insert-mode)))))

(ert-deftest donkey-wrap-region-returns-to-normal-when-insertion-errors ()
  "Wrapping returns to Normal state even when the insertion errors.

Regression test: DONKEY must end up back in Normal state even when the
insertion itself signals.

`donkey-wrap-region' is the one command that enters Insert state
BEFORE doing its real work rather than as the last step, so an error
from `self-insert-command' used to skip the transition back and strand
the buffer in Insert -- from a key the user pressed in Normal state.
Confirmed live in `emacs -nw': with a region active in a read-only
buffer, pressing a wrap delimiter reported \"Buffer is read-only\" and
silently left the modeline on DONKEY[I].

The error comes from `post-self-insert-hook' here rather than from a
read-only buffer, because a read-only buffer is now refused BEFORE
INSERT state is entered -- see the test after this one -- and would no
longer reach the cleanup this pins.  The error must still reach the
user; only the state cleanup is guaranteed."
  (with-temp-buffer
    (donkey-normal-mode 1)
    (let ((transient-mark-mode t)
          (donkey-mode t))
      (insert "hello")
      (goto-char 1)
      (push-mark (point) t t)
      (goto-char 3)
      (let ((last-command-event ?\()
            (donkey-wrap-region-engine 'pairing-package)
            (post-self-insert-hook (list (lambda () (error "Pairing broke")))))
        (should-error (donkey-wrap-region)))
      (should (bound-and-true-p donkey-normal-mode))
      (should-not (bound-and-true-p donkey-insert-mode)))))

(ert-deftest donkey-wrap-region-in-a-read-only-buffer-keeps-the-selection ()
  "A wrap key in a read-only buffer refuses and leaves the region active.

Regression test.  The refusal used to come from `self-insert-command',
after INSERT state had been entered, and leaving INSERT again through
`donkey--exit-insert' deactivated the mark -- so `v w (' in a read-only
buffer said \"Buffer is read-only\" and threw the selection away with
it, where the rectangle path kept its block.  The buffer is asserted
unchanged as well, the refusal being the whole of what the press does."
  (with-temp-buffer
    (donkey-normal-mode 1)
    (let ((transient-mark-mode t)
          (donkey-mode t))
      (insert "hello")
      (goto-char 1)
      (push-mark (point) t t)
      (goto-char 3)
      (setq buffer-read-only t)
      (let ((last-command-event ?\())
        (should-error (donkey-wrap-region) :type 'buffer-read-only))
      (should (region-active-p))
      (should (= (mark) 1))
      (should (= (point) 3))
      (should (string= (buffer-string) "hello"))
      (should (bound-and-true-p donkey-normal-mode)))))

(ert-deftest donkey-a-borrowed-wrap-key-is-never-a-keyboard-macro ()
  "A wrap key whose buffer binds it to a keyboard macro is not borrowed.

Found by audit, reproduced end to end: in a read-only buffer a wrap
key with no selection goes back to the mode underneath, and a keyboard
macro there satisfies `commandp' and then signals `wrong-type-argument'
inside `call-interactively' -- an error raised by a keypress.  What
the macro would type cannot be read beforehand either, so there is no
way to tell whether borrowing it would type, which NORMAL state does
not do.

The control below is the point of the test: an ordinary command in the
same position IS borrowed, so the refusal is about the macro and not
about the setup."
  (let ((ran nil))
    (cl-letf (((symbol-function 'donkey-wrap-macro-test--probe)
               (lambda () (interactive) (setq ran t))))
      ;; Control: a real command underneath is handed the key.
      (with-temp-buffer
        (donkey-normal-mode 1)
        (let ((map (make-sparse-keymap))
              (donkey-mode t))
          (define-key map (kbd "(") #'donkey-wrap-macro-test--probe)
          (use-local-map map)
          (insert "hello")
          (setq buffer-read-only t)
          (let ((last-command-event ?\())
            (cl-letf (((symbol-function 'this-command-keys-vector)
                       (lambda () (vector ?\())))
              (donkey--wrap-pass-the-key-on))
            (should ran))))
      ;; A keyboard macro is refused, and refusing it does not signal.
      (with-temp-buffer
        (donkey-normal-mode 1)
        (let ((map (make-sparse-keymap))
              (donkey-mode t))
          (define-key map (kbd "(") "abc")
          (use-local-map map)
          (insert "hello")
          (setq buffer-read-only t)
          (let ((last-command-event ?\())
            (cl-letf (((symbol-function 'this-command-keys-vector)
                       (lambda () (vector ?\()))
                      ((symbol-function 'ding) #'ignore))
              (should (eq 'refused
                          (condition-case nil
                              (progn (donkey--wrap-pass-the-key-on) 'refused)
                            (error 'signalled))))))
          (should (string= (buffer-string) "hello")))))))

(defun donkey-wrap-macro-test--probe ()
  "Stand in for a command a mode puts on a wrap key."
  (interactive)
  nil)

(ert-deftest donkey-wrap-region-over-read-only-text-keeps-the-selection ()
  "A wrap key over text with the `read-only' property refuses and keeps the region.

Regression, the text-property twin of the read-only buffer: the
refusal comes from `self-insert-command' inside INSERT state, and the
way back deactivated the mark, so `v w (' over such text said \"Text
is read-only\" and threw the selection away.  The buffer is asserted
unchanged, the state Normal, and the error still signaled -- shaped
like the read-only buffer test above it, with point at the region's
end where `v w' leaves it."
  (with-temp-buffer
    (donkey-normal-mode 1)
    (let ((transient-mark-mode t)
          (donkey-mode t)
          (inhibit-read-only t))
      (insert "alpha beta")
      (put-text-property 1 6 'read-only t))
    (let ((transient-mark-mode t)
          (donkey-mode t))
      (push-mark 1 t t)
      (goto-char 6)
      (let ((last-command-event ?\())
        (should-error (donkey-wrap-region) :type 'text-read-only))
      (should (region-active-p))
      (should (= (mark) 1))
      (should (= (point) 6))
      (should (string= (buffer-string) "alpha beta"))
      (should (bound-and-true-p donkey-normal-mode))
      (should-not (bound-and-true-p donkey-insert-mode)))))

(ert-deftest donkey-insertion-read-only-p-agrees-with-emacs ()
  "The predicate answers as a real insertion would, for every stickiness shape.

Every spelling of the property that changes the answer -- bare, with
`rear-nonsticky' as t or as a list, with `front-sticky' as t or as a
list, both at once, with `inhibit-read-only', and the `fence' value --
over five spans of a five-character buffer, at all six positions."
  (dolist (props '((read-only t)
                   (read-only t rear-nonsticky t)
                   (read-only t rear-nonsticky (read-only))
                   (read-only t front-sticky t)
                   (read-only t front-sticky (read-only))
                   (read-only t front-sticky t rear-nonsticky t)
                   (read-only t inhibit-read-only t)
                   (read-only fence)))
    (dolist (span '((1 . 3) (3 . 5) (2 . 4) (1 . 6) (4 . 6)))
      (with-temp-buffer
        (insert "abcde")
        (add-text-properties (car span) (cdr span) props)
        (dotimes (i 6)
          (let* ((pos (1+ i))
                 (real (save-excursion
                         (goto-char pos)
                         (condition-case nil
                             (progn (insert "x") (delete-region pos (1+ pos)) nil)
                           (text-read-only t)))))
            (should (equal (list props span pos
                                 (and (donkey--insertion-read-only-p pos) t))
                           (list props span pos real)))))))))

(ert-deftest donkey-wrap-over-read-only-text-is-refused-before-insert-state ()
  "The refusal comes before INSERT is entered, so no pairing package gets a turn.

Regression: under `electric-pair-mode' with the selection's last
character read-only, the opener went in before the closer was refused,
leaving \"(alpha beta\" behind.  Real keys, `electric-pair-mode' on:
the buffer is unchanged, the selection kept, and Insert state never
entered."
  (let (entered)
    (unwind-protect
        (progn
          (electric-pair-mode 1)
          (donkey-test-keys--harness "*donkey-wrap-ro-text*" #'text-mode ()
              "alpha beta\n" "m w"
            (let ((inhibit-read-only t))
              (put-text-property 5 6 'read-only t))
            (cl-letf* ((orig (symbol-function 'donkey-insert-mode))
                       ((symbol-function 'donkey-insert-mode)
                        (lambda (&rest args) (setq entered t) (apply orig args))))
              (should-error (execute-kbd-macro (kbd "(")) :type 'text-read-only))
            (should-not entered)
            (should (equal (buffer-string) "alpha beta\n"))
            (should (region-active-p))
            (should (= (region-beginning) 1))
            (should (= (region-end) 6))
            (should (bound-and-true-p donkey-normal-mode))))
      (electric-pair-mode -1))))

(ert-deftest donkey-rectangle-wrap-over-read-only-text-wraps-no-row ()
  "A read-only character on a middle row refuses the whole block, before any row is wrapped."
  (donkey-test-keys--harness "*donkey-wrap-ro-rect*" #'text-mode ()
      "abcd\nefgh\nijkl\n" "m v l l j j"
    (let ((inhibit-read-only t))
      (put-text-property 8 9 'read-only t))
    (should-error (execute-kbd-macro (kbd "(")) :type 'text-read-only)
    (should (equal (buffer-string) "abcd\nefgh\nijkl\n"))
    (should (bound-and-true-p rectangle-mark-mode))))

(ert-deftest donkey-wrap-beside-non-sticky-read-only-text-still-wraps ()
  "Read-only text that does not stick to an insertion does not refuse the wrap.

The rule is Emacs's, not \"any read-only character nearby\": a
`rear-nonsticky' property on the character before the closer's spot
lets the closer in, and the wrap goes through."
  (donkey-test-keys--harness "*donkey-wrap-ro-ns*" #'text-mode ()
      "alpha beta\n" "m w"
    (let ((inhibit-read-only t))
      (add-text-properties 5 6 '(read-only t rear-nonsticky t)))
    (execute-kbd-macro (kbd "("))
    (should (equal (buffer-string) "(alpha) beta\n"))))

(ert-deftest donkey-leave-insert-can-keep-the-mark ()
  "`donkey--leave-insert' with KEEP-MARK leaves the region active.

The one caller is `donkey-wrap-region' after a refused insertion; the
bare call still lets the mark go, as `C-g' must."
  (with-temp-buffer
    (donkey-mode 1)
    (unwind-protect
        (let ((transient-mark-mode t))
          (insert "alpha")
          (push-mark 1 t t)
          (goto-char 3)
          (donkey-enter-insert)
          (donkey--leave-insert t)
          (should (region-active-p))
          (should (bound-and-true-p donkey-normal-mode))
          (donkey-enter-insert)
          (donkey--leave-insert)
          (should-not (region-active-p))
          (should (bound-and-true-p donkey-normal-mode)))
      (donkey-mode -1))))

(ert-deftest donkey-wrap-region-bound-for-each-default-delimiter ()
  "Every delimiter the variable asks for, and its closer, is a wrap key.

`donkey-wrap-delimiters' is `all' by default, which is every pair
`donkey-mark-pair-delimiters' knows -- what `m i' can select, a key
can wrap -- and the closing half of each is bound too, so `)' is a
wrap key as much as `(' is.  The two exceptions are the keys another
DONKEY command holds, `:' and `>'."
  (dolist (ch (donkey--wrap-delimiter-characters))
    ;; Except the two whose keys belong to a command of DONKEY's own,
    ;; which the claim rule leaves alone and the report names.
    (unless (memq ch '(?: ?>))
      (should (eq (lookup-key donkey-normal-mode-map (char-to-string ch))
                  #'donkey-wrap-region))
      (let ((close (donkey--wrap-close-char ch)))
        (unless (or (eq close ch) (memq close '(?: ?>)))
          (should (eq (lookup-key donkey-normal-mode-map (char-to-string close))
                      #'donkey-wrap-region)))))))

(ert-deftest donkey-a-wrap-delimiter-never-takes-a-key-that-is-spoken-for ()
  "A character whose key already runs something is not made a wrap key.

`x' deletes, `:' goes to a line and `>' indents, whatever
`donkey-wrap-delimiters' says: the reader who put them in a list of
delimiters did not ask for their commands to disappear.  A character
whose key is free is taken, and both halves of a pair are asked
separately -- `<' can wrap while `>' goes on indenting."
  (unwind-protect
      (let ((donkey-wrap-delimiters (append (donkey--wrap-delimiter-characters)
                                        '(?x ?#))))
        (donkey--claim-wrap-keys)
        (should (eq (keymap-lookup donkey-normal-mode-map "x") #'donkey-delete))
        (should (eq (keymap-lookup donkey-normal-mode-map ":") #'donkey-goto-line))
        (should (eq (keymap-lookup donkey-normal-mode-map ">")
                    #'donkey-indent-region-or-line))
        (should (eq (keymap-lookup donkey-normal-mode-map "<") #'donkey-wrap-region))
        (should (eq (keymap-lookup donkey-normal-mode-map "#") #'donkey-wrap-region))
        ;; and the ones it could not take are named
        (should (equal (mapcar #'car (donkey--delimiters-that-cannot-wrap))
                       '(?: ?> ?x))))
    ;; Outside the `let\=', so the claim that puts the keys back is made
    ;; against the list as it really is.  Inside it, the cleanup claimed
    ;; the test\='s own delimiters all over again and left them bound --
    ;; found by the shuffle seeds, not by this file.
    (donkey--claim-wrap-keys)))

(ert-deftest donkey-what-may-wrap-is-read-from-the-keymap-not-a-list-of-names ()
  "Whether a key is free is asked of the keymap, so a reader may move things.

`x' deletes and `;' does nothing by default, and a reader who swaps
them round has swapped which of the two may become a wrap key.
Nothing here knows the name `x': what is asked is whether the key runs
anything, so an `ignore' put on a key to make it harmless leaves it
free and a command put on a key takes it."
  (let ((was-x (keymap-lookup donkey-normal-mode-map "x"))
        (was-semicolon (keymap-lookup donkey-normal-mode-map ";")))
    (unwind-protect
        (let ((donkey-wrap-delimiters (append (donkey--wrap-delimiter-characters)
                                              (list ?x ?\;))))
          (keymap-set donkey-normal-mode-map "x" #'ignore)
          (keymap-set donkey-normal-mode-map ";" #'donkey-delete)
          (donkey--claim-wrap-keys)
          (should (eq (keymap-lookup donkey-normal-mode-map "x") #'donkey-wrap-region))
          (should (eq (keymap-lookup donkey-normal-mode-map ";") #'donkey-delete)))
      (keymap-set donkey-normal-mode-map "x" was-x)
      (keymap-set donkey-normal-mode-map ";" was-semicolon)
      (donkey--claim-wrap-keys))
    (should (eq (keymap-lookup donkey-normal-mode-map "x") #'donkey-delete))
    (should (eq (keymap-lookup donkey-normal-mode-map ";") #'undefined))))

(ert-deftest donkey-a-key-let-go-of-gets-back-what-it-held ()
  "A wrap key given up goes back to its old binding, not to nothing.

An UNBOUND key is the one kind that falls through to the major mode,
so unbinding a key DONKEY had blocked would open it rather than close
it: a reader who adds `(?, . ?,)' and thinks better of it would be
left with `,' reaching whatever the mode puts there."
  (let ((was donkey-mark-pair-delimiters))
    (unwind-protect
        (progn
          (should (eq (keymap-lookup donkey-normal-mode-map ",") #'undefined))
          (setq donkey-mark-pair-delimiters (cons (cons ?, ?,) was))
          (donkey--claim-wrap-keys)
          (should (eq (keymap-lookup donkey-normal-mode-map ",")
                      #'donkey-wrap-region))
          (setq donkey-mark-pair-delimiters was)
          (donkey--claim-wrap-keys)
          (should (eq (keymap-lookup donkey-normal-mode-map ",") #'undefined))
          ;; and a key that really was unbound is unbound again
          (setq donkey-mark-pair-delimiters (cons (cons ?# ?#) was))
          (donkey--claim-wrap-keys)
          (should (eq (keymap-lookup donkey-normal-mode-map "#")
                      #'donkey-wrap-region))
          (setq donkey-mark-pair-delimiters was)
          (donkey--claim-wrap-keys)
          (should-not (keymap-lookup donkey-normal-mode-map "#")))
      (setq donkey-mark-pair-delimiters was)
      (donkey--claim-wrap-keys))))

(defun donkey-wrap-test--claimed ()
  "Return the printable keys that currently run `donkey-wrap-region'."
  (seq-filter (lambda (c)
                (eq (keymap-lookup donkey-normal-mode-map
                                   (key-description (vector c)))
                    'donkey-wrap-region))
              (number-sequence ?! ?~)))

(defmacro donkey-wrap-test--engine-restored (&rest body)
  "Run BODY and put the engine and the wrap keys back afterwards."
  `(let ((was donkey-wrap-region-engine))
     (unwind-protect (progn ,@body)
       (set-default 'donkey-wrap-region-engine was)
       (donkey--claim-wrap-keys))))

(ert-deftest donkey-toggling-the-engine-changes-who-wraps-and-says-so ()
  "The toggle flips the engine, claims the keys again, and reports.

Reached by name: a setting changed to compare two behaviors is not
something fingers repeat.  It says which engine is in force because
the two differ in what a press does and in which keys are wrap keys at
all."
  (donkey-wrap-test--engine-restored
   (set-default 'donkey-wrap-region-engine 'donkey)
   (donkey--claim-wrap-keys)
   (let ((wide (donkey-wrap-test--claimed))
         said)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (when fmt (setq said (apply #'format fmt args))))))
       (donkey-toggle-wrap-engine))
     (should (eq donkey-wrap-region-engine 'pairing-package))
     (should (string-match-p "the pairing package wraps now" said))
     ;; and the keys followed the engine
     (let ((narrow (donkey-wrap-test--claimed)))
       (should (< (length narrow) (length wide)))
       (should (equal narrow (sort (delete-dups
                                    (append donkey--wrap-delegated-delimiters
                                            (mapcar #'donkey--wrap-close-char
                                                    donkey--wrap-delegated-delimiters)))
                                   #'<))))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (when fmt (setq said (apply #'format fmt args))))))
       (donkey-toggle-wrap-engine))
     (should (eq donkey-wrap-region-engine 'donkey))
     (should (string-match-p "DONKEY wraps and unwraps now" said))
     (should (equal (donkey-wrap-test--claimed) wide)))))

(ert-deftest donkey-the-toggle-says-whether-anything-is-pairing-here ()
  "Handed the press, a buffer with no pairing package inserts one character.

That is the setting doing what it says, and not what a reader who
forgot to turn the package on is expecting, so the moment of switching
is where it is worth saying."
  (donkey-wrap-test--engine-restored
   (with-temp-buffer
     (set-default 'donkey-wrap-region-engine 'donkey)
     (let (said)
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args) (when fmt (setq said (apply #'format fmt args))))))
         (donkey-toggle-wrap-engine))
       (should (string-match-p "nothing this package knows of is pairing" said))
       (set-default 'donkey-wrap-region-engine 'donkey)
       (electric-pair-local-mode 1)
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'message)
                        (lambda (fmt &rest args)
                          (when fmt (setq said (apply #'format fmt args))))))
               (donkey-toggle-wrap-engine))
             (should (string-match-p "electric-pair-mode is on in this buffer" said)))
         (electric-pair-local-mode -1))))))

(ert-deftest donkey-a-named-list-is-taken-as-it-stands-under-either-engine ()
  "`all' is narrowed while delegating; a list the reader named is not.

A reader who has taught their pairing package another pair says so by
naming the characters, and naming is always obeyed -- the narrowing is
only of what `all' DERIVES."
  (donkey-wrap-test--engine-restored
   (let ((donkey-wrap-delimiters (list ?= ?#)))
     (set-default 'donkey-wrap-region-engine 'donkey)
     (donkey--claim-wrap-keys)
     (should (equal (donkey-wrap-test--claimed) (list ?# ?=)))
     (set-default 'donkey-wrap-region-engine 'pairing-package)
     (donkey--claim-wrap-keys)
     (should (equal (donkey-wrap-test--claimed) (list ?# ?=))))))

(ert-deftest donkey-setting-the-engine-through-customize-claims-the-keys ()
  "Setting the engine with Customize binds the keys the engine asks for."
  (donkey-wrap-test--engine-restored
   (customize-set-variable 'donkey-wrap-region-engine 'pairing-package)
   (should-not (memq ?= (donkey-wrap-test--claimed)))
   (customize-set-variable 'donkey-wrap-region-engine 'donkey)
   (should (memq ?= (donkey-wrap-test--claimed)))))

(ert-deftest donkey-a-delimiter-dropped-from-the-list-gives-its-key-back ()
  "A character taken out of `donkey-wrap-delimiters' stops wrapping.

And only a key that is still DONKEY's own wrap is let go of: whoever
took it in the meantime keeps it, since a variable this package reads
is no licence to unbind somebody else's key."
  (let ((donkey-wrap-delimiters (append (donkey--wrap-delimiter-characters) '(?#))))
    (donkey--claim-wrap-keys)
    (should (eq (keymap-lookup donkey-normal-mode-map "#") #'donkey-wrap-region)))
  (donkey--claim-wrap-keys)
  (should-not (keymap-lookup donkey-normal-mode-map "#"))
  ;; somebody else's key, named in the list and then dropped from it
  (unwind-protect
      (let ((donkey-wrap-delimiters (append (donkey--wrap-delimiter-characters) '(?#))))
        (donkey--claim-wrap-keys)
        (keymap-set donkey-normal-mode-map "#" #'ignore-preserving-kill-region)
        (setq donkey-wrap-delimiters (delq ?# donkey-wrap-delimiters))
        (donkey--claim-wrap-keys)
        (should (eq (keymap-lookup donkey-normal-mode-map "#")
                    #'ignore-preserving-kill-region)))
    (keymap-unset donkey-normal-mode-map "#")
    (donkey--claim-wrap-keys)))

(ert-deftest donkey-the-wrap-keys-follow-the-variable-being-set ()
  "Setting `donkey-wrap-delimiters' through Customize binds the keys at once.

The variable used to be read only while the file loaded, so a reader
who set it afterwards had a list that meant nothing.  A plain `setq'
still needs \\[donkey-refresh-wrap-keys], which is what the command is
for."
  (let ((was donkey-wrap-delimiters))
    (unwind-protect
        (progn
          (customize-set-variable 'donkey-wrap-delimiters
                                  (append (donkey--wrap-delimiter-characters) '(?#)))
          (should (eq (keymap-lookup donkey-normal-mode-map "#") #'donkey-wrap-region)))
      (customize-set-variable 'donkey-wrap-delimiters was)
      (should-not (keymap-lookup donkey-normal-mode-map "#")))))

(ert-deftest donkey-claiming-wrap-keys-does-not-trust-the-list ()
  "Anything in `donkey-wrap-delimiters' that is not a character is skipped.

A defcustom holds whatever a reader put in it, and the shapes a slip
actually takes -- a string of two characters, a vector, a float --
signal from `key-description' rather than being ignored, at load time,
where a signal is the whole package failing to come up."
  (unwind-protect
      (dolist (junk (list "(" "ab" nil 'paren '(?a . ?b) [?a] 1.5 -3))
        (let ((donkey-wrap-delimiters (list ?\( junk)))
          (should (equal (list junk
                               (condition-case err
                                   (progn (donkey--claim-wrap-keys) 'no-signal)
                                 (error (car err))))
                         (list junk 'no-signal)))
          (should (eq (keymap-lookup donkey-normal-mode-map "(") #'donkey-wrap-region))))
    (donkey--claim-wrap-keys))
  (should (eq (keymap-lookup donkey-normal-mode-map "\"") #'donkey-wrap-region)))

;;; ---------------------------------------------------------------------------
;;; donkey-wrap-region with a real pairing package
;;; ---------------------------------------------------------------------------

(defmacro donkey-wrap-test--with-electric-pair (text keys &rest body)
  "Run KEYS over TEXT in a displayed buffer with `electric-pair-mode' on.

Every other wrap test runs with NO pairing package, where the
documented behavior is a bare insert -- so the delegating path, the
one a configured user actually gets, was covered by nothing.
`electric-pair-mode' is built in, so this needs no dependency and does
not skip, unlike the Smartparens tests.  `donkey-wrap-region-engine' is
bound to `pairing-package' throughout, that being the whole point of
these: DONKEY's own wrap never reaches the hook at all.

Real keys, and a buffer switched to rather than merely current:
`electric-pair-mode' works off `post-self-insert-hook', which only
runs from the command loop, and the command loop acts on the SELECTED
WINDOW's buffer."
  (declare (indent 2))
  `(unwind-protect
       (progn
         (when (get-buffer "*donkey-wrap-test*") (kill-buffer "*donkey-wrap-test*"))
         (donkey-mode 1)
         (let ((transient-mark-mode t)
               (donkey-wrap-region-engine 'pairing-package)
               (prefix-arg nil) (current-prefix-arg nil)
               (this-command nil) (last-command nil))
           (switch-to-buffer (get-buffer-create "*donkey-wrap-test*"))
           (text-mode)
           (electric-pair-mode 1)
           (erase-buffer)
           (insert ,text)
           (goto-char (point-min))
           (donkey-enter-normal)
           (execute-kbd-macro (kbd ,keys))
           ,@body))
     (electric-pair-mode -1)
     (when (get-buffer "*donkey-wrap-test*") (kill-buffer "*donkey-wrap-test*"))
     (donkey-mode -1)))

(ert-deftest donkey-wrap-region-wraps-the-pairs-electric-pair-knows ()
  "The four delimiters `electric-pair-mode' treats as pairs do wrap.

The delegating path: `donkey-wrap-region' self-inserts with the region
still active and the pairing package does the wrapping.  Asserted
end to end on the buffer text, because the point of the design is what
the user ends up looking at."
  (dolist (case '(("(" "(alpha) beta")
                  ("[" "[alpha] beta")
                  ("{" "{alpha} beta")
                  ("\"" "\"alpha\" beta")))
    (cl-destructuring-bind (delim expected) case
      (donkey-wrap-test--with-electric-pair "alpha beta" (concat "m w " delim)
        (should (equal (cons delim (buffer-string))
                       (cons delim expected)))))))

(ert-deftest donkey-wrap-region-quote-and-backtick-do-not-wrap-under-electric-pair ()
  "Delegating, `\\='' and `\\=`' leave a stray quote where they cannot pair.

Both are in `donkey-wrap-delimiters' by default, and
`electric-pair-mode' pairs neither, so with
`donkey-wrap-region-engine' set to `pairing-package' a selection plus
`\\='' leaves a stray quote at its start rather than wrapping.  Nothing
is lost -- the selection's text is still there -- but no wrap happened,
and that is what the setting buys and costs.  DONKEY's own wrap, the
default, wraps all six; the test after this one pins that.

Not the major mode's syntax table: `fundamental-mode', `text-mode' and
`emacs-lisp-mode' all behave this way, though the character has word,
punctuation and expression-prefix syntax across the three."
  (dolist (case '(("'" "'alpha beta")
                  ("`" "`alpha beta")))
    (cl-destructuring-bind (delim expected) case
      (donkey-wrap-test--with-electric-pair "alpha beta" (concat "m w " delim)
        (should (equal (cons delim (buffer-string))
                       (cons delim expected)))))))

(ert-deftest donkey-wrap-region-returns-to-normal-after-a-real-wrap ()
  "A real wrap ends in NORMAL state with no region left active.

The command enters Insert to self-insert and must come back.  Covered
for the mocked path already; this is the same guarantee with a pairing
package really running and really modifying the buffer."
  (donkey-wrap-test--with-electric-pair "alpha beta" "m w ("
    (should (equal (buffer-string) "(alpha) beta"))
    (should (bound-and-true-p donkey-normal-mode))
    (should-not (bound-and-true-p donkey-insert-mode))
    (should-not (region-active-p))))

(ert-deftest donkey-wrap-region-wraps-a-rectangle-under-electric-pair ()
  "A rectangle is wrapped by DONKEY itself, pairing package or not.

`donkey--wrap-rectangle-region' does the work directly rather than
delegating, so this must not change when a pairing package is present
-- and in particular must not wrap twice."
  (donkey-wrap-test--with-electric-pair "abcd\nefgh\nijkl\n" "m v j j l ("
    (should (equal (buffer-string) "(ab)cd\n(ef)gh\n(ij)kl\n"))))

;;; ---------------------------------------------------------------------------
;;; donkey-wrap-region, DONKEY's own wrap
;;; ---------------------------------------------------------------------------

(defmacro donkey-wrap-test--own (mode text keys &rest body)
  "Type KEYS over TEXT in MODE with DONKEY doing its own wrapping.

The engine is bound rather than left at its default, so a test of the
default cannot start passing for the reason a changed default would
give it."
  (declare (indent 3))
  `(donkey-test-keys--harness "*donkey-wrap-own*" ,mode
       ((donkey-wrap-region-engine 'donkey))
       ,text ,keys
     ,@body))

(ert-deftest donkey-a-wrap-key-wraps-the-selection-with-nothing-pairing ()
  "Each default delimiter wraps the selection with no pairing package.

The point of the default engine: the six keys wrap the same way in a
bare Emacs as under a pairing package, where delegating leaves a
single character at point for the two the package declines."
  (dolist (case '(("(" "(alpha) beta")
                  ("[" "[alpha] beta")
                  ("{" "{alpha} beta")
                  ("\"" "\"alpha\" beta")
                  ("'" "'alpha' beta")
                  ("`" "`alpha` beta")))
    (cl-destructuring-bind (delim expected) case
      (donkey-wrap-test--own #'text-mode "alpha beta" (concat "m w " delim)
        (should (equal (cons delim (buffer-string))
                       (cons delim expected)))))))

(ert-deftest donkey-a-closing-delimiter-wraps-in-the-pair-its-opener-names ()
  "A closing delimiter wraps as the opener of its pair does.

`m i' has always taken either half of a pair; the wrap keys take
either half too, so a reader who reaches for the key nearer the
selection's end gets the wrap rather than a beep."
  (dolist (case '((")" "(alpha) beta")
                  ("]" "[alpha] beta")
                  ("}" "{alpha} beta")))
    (cl-destructuring-bind (delim expected) case
      (donkey-wrap-test--own #'text-mode "alpha beta" (concat "m w " delim)
        (should (equal (cons delim (buffer-string))
                       (cons delim expected)))))))

(ert-deftest donkey-a-wrap-key-takes-off-the-pair-already-around-it ()
  "A delimiter whose pair already stands around the selection takes it off.

The selection says which of the two a press means: `m i' selects what
a pair holds, so the pair is outside the selection and the next press
removes it.  Both halves of an asymmetric pair say the same thing."
  ;; One press, not two: `w w' lands ON the closing delimiter, so `m i'
  ;; resolves it from the buffer and the press after it is the one that
  ;; acts.  A second would find no selection and ring the bell -- which
  ;; a terminal frame turns into an aborted macro and `--batch' does
  ;; not, so the extra key hid here until the live suite ran.
  (dolist (case '(("m i \"" "say word now")
                  ("m i (" "say word now")
                  ("m i )" "say word now")))
    (cl-destructuring-bind (keys expected) case
      (donkey-wrap-test--own #'text-mode
          (if (string-prefix-p "m i \"" keys) "say \"word\" now" "say (word) now")
          (concat "w w " keys)
        (should (equal (cons keys (buffer-string))
                       (cons keys expected)))))))

(ert-deftest donkey-a-wrap-puts-the-pair-on-and-nothing-else ()
  "A wrap is the two characters and nothing else, in every mode alike.

Nothing inside is escaped, so a pair around a pair reads as one: `m a
\"' selects the quotes as well and the press puts a plain pair around
them.  A programming mode and a prose mode do the same thing, the
syntax table having no say in it, and a selection that ends in a
backslash gets the same treatment as any other."
  (dolist (mode (list #'emacs-lisp-mode #'text-mode))
    (dolist (case '(("% \"" "\"word\"")
                    ("% \" % \"" "\"\"word\"\"")
                    ("% \" % \" % \"" "\"\"\"word\"\"\"")
                    ("% (" "(word)")
                    ("% ( % (" "((word))")))
      (cl-destructuring-bind (keys expected) case
        (donkey-wrap-test--own mode "word" keys
          (should (equal (list mode keys (buffer-string))
                         (list mode keys expected)))))))
  (donkey-wrap-test--own #'emacs-lisp-mode "ab\\ cd" "v l l l \""
    (should (equal (buffer-string) "\"ab\\\" cd"))))

(ert-deftest donkey-a-take-off-is-the-exact-inverse-of-a-wrap ()
  "The press that takes a pair off undoes the press that put it on.

Two characters go where two came from and nothing between them moves,
so a layer at a time comes off in the order it went on, whatever the
mode.  The selection is the inner text each time, which is what `m i'
hands over."
  (dolist (mode (list #'emacs-lisp-mode #'text-mode))
    (dolist (case '(("\"\"word\"\"" 3 7 ?\" "\"word\"")
                    ("\"word\"" 2 6 ?\" "word")
                    ("((word))" 3 7 ?\( "(word)")
                    ;; Backslashes inside are text like any other:
                    ;; they went in untouched and come out untouched.
                    ("\"ab\\\\\"" 2 6 ?\" "ab\\\\")))
      (cl-destructuring-bind (text beg end char expected) case
        (donkey-test-keys--harness "*donkey-wrap-inverse*" mode
            ((donkey-wrap-region-engine 'donkey))
            text ""
          (goto-char beg)
          (push-mark beg t t)
          (goto-char end)
          (let ((last-command-event char))
            (donkey-wrap-region))
          (should (equal (list mode text (buffer-string))
                         (list mode text expected))))))))

(ert-deftest donkey-a-delimiter-on-one-side-only-is-not-a-wrap ()
  "Both sides have to be there before a press takes anything off.

A delimiter standing before the selection with nothing answering it
after -- or the other way round -- is not a pair around it, so the
press wraps.  Taking one off on that evidence would delete a character
belonging to something else."
  (dolist (case '(("say \"word here" 6 10 ?\" "say \"\"word\" here")
                  ("say word\" here" 5 9 ?\" "say \"word\"\" here")))
    (cl-destructuring-bind (text beg end char expected) case
      (donkey-test-keys--harness "*donkey-wrap-one-side*" #'text-mode
          ((donkey-wrap-region-engine 'donkey))
          text ""
        (goto-char beg)
        (push-mark beg t t)
        (goto-char end)
        (let ((last-command-event char))
          (donkey-wrap-region))
        (should (equal (list text (buffer-string)) (list text expected)))))))

(ert-deftest donkey-an-escaped-delimiter-outside-the-selection-is-no-wrap ()
  "An escaped opener is not the pair being taken off.

Only an unescaped delimiter counts as already standing around the
selection.  The shape that reaches the rule is an escaped opener with
an unescaped closer -- where both are escaped, the backslash before
the closer is what sits next to the selection and the question never
arises.  Taking that pair off would leave the backslash behind, so the
selection gets a pair of its own instead.  Where the coverage stops,
and the docstring says so."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "a \\\"word\" b")
    ;; a . \ " w o r d " . b
    ;; 1 2 3 4 5       9
    (should (eq (char-before 5) ?\"))
    (should (eq (char-after 9) ?\"))
    (should (donkey--wrap-escaped-p 4))
    (should-not (donkey--wrap-escaped-p 9))
    (should-not (donkey--wrap-already-wrapped-p 5 9 ?\" ?\"))))

(ert-deftest donkey-a-counted-wrap-of-donkeys-own-wraps-once ()
  "A count before a wrap delimiter wraps once, whatever the count says.

One delimiter press, one wrap: the count is DONKEY's and this command
has no use for it.  Nothing reads it either -- no pairing package is
called, so the Emacs 31 reading of `current-prefix-arg' that the
delegating path has to bind away cannot arise here at all."
  (dolist (keys '("m w C-u 3 (" "m w C-u 0 (" "m w C-u - ("))
    (donkey-wrap-test--own #'text-mode "alpha beta" keys
      (should (equal (cons keys (buffer-string))
                     (cons keys "(alpha) beta"))))))

(ert-deftest donkey-a-wrap-leaves-point-on-the-first-character-it-wrapped ()
  "A wrap and a take-off both leave point on the first character held.

The same place either way, so a press and the press that undoes it
leave the cursor where the reader is looking."
  (donkey-wrap-test--own #'text-mode "alpha beta" "m w ("
    (should (equal (buffer-string) "(alpha) beta"))
    (should (= (point) 2)))
  (donkey-wrap-test--own #'text-mode "(alpha) beta" "l m w ("
    (should (equal (buffer-string) "alpha beta"))
    (should (= (point) 1))))

(ert-deftest donkey-a-wrap-key-wraps-a-rectangle-with-nothing-pairing ()
  "A rectangle is wrapped line by line, and a closer wraps it too.

The rectangle path is DONKEY's own under either engine; the closing
delimiter reaches it through the same resolution the linear path
uses."
  (donkey-wrap-test--own #'text-mode "abcd\nefgh\nijkl\n" "m v j j l )"
    (should (equal (buffer-string) "(ab)cd\n(ef)gh\n(ij)kl\n"))))

(ert-deftest donkey-the-wrap-engine-reads-an-unknown-value-as-donkey ()
  "Any value but `pairing-package' selects DONKEY's own wrap.

A defcustom is not to be trusted: it is coerced where it is read, so a
mistyped value wraps rather than signaling."
  (dolist (value '(nil donkey smartparens "pairing-package" 42))
    (donkey-test-keys--harness "*donkey-wrap-engine*" #'text-mode
        ((donkey-wrap-region-engine value))
        "alpha beta" "m w ("
      (should (equal (cons value (buffer-string))
                     (cons value "(alpha) beta"))))))

(ert-deftest donkey-the-pairing-package-engine-takes-no-pair-off ()
  "Delegating, a press whose pair already stands there wraps again.

Taking a pair off is DONKEY's own doing and belongs to its own engine;
the setting that hands the press to the pairing package hands over the
whole press, so `m i \"' then `\"' adds a pair rather than removing
one, as it did before there was an engine to choose."
  (donkey-test-keys--harness "*donkey-wrap-engine*" #'text-mode
      ((donkey-wrap-region-engine 'pairing-package))
      "say \"word\" now" "w w m i \""
    (should (equal (buffer-string) "say \"\"word\" now"))))

(ert-deftest donkey-a-take-off-over-read-only-delimiters-is-refused ()
  "A take-off whose delimiters are read-only refuses and changes nothing.

The guard is the one a deletion is judged by -- the `read-only'
property where it stands, stickiness having no part in it -- and it
runs before either delimiter goes, so the pair cannot be left half
removed."
  (donkey-wrap-test--own #'text-mode "say (word) now" ""
    ;; Point ON the opener, so `m i' resolves it from the buffer and no
    ;; delimiter has to be typed to select with -- one typed now would
    ;; act, which is what the press below is for.
    (goto-char 5)
    (execute-kbd-macro (kbd "m i"))
    (let ((inhibit-read-only t))
      (put-text-property 5 6 'read-only t))
    (should-error (execute-kbd-macro (kbd "(")) :type 'text-read-only)
    (should (equal (buffer-string) "say (word) now"))
    (should (region-active-p))))

(ert-deftest donkey-a-digraph-wraps-a-selection-and-inserts-without-one ()
  "The digraph key wraps a live selection and inserts when there is none.

`SPC i &' reads the same mnemonic either way; what differs is what the
character it names does.  The closing character comes from
`donkey-mark-pair-delimiters', so `&<<' wraps in the two guillemets
rather than in two openers, and a selection those already stand around
loses them."
  (donkey-wrap-test--own #'text-mode "say word now" "w SPC i & < <"
    (should (equal (buffer-string) "say« word now")))
  (donkey-wrap-test--own #'text-mode "say word now" "w m w SPC i & < <"
    (should (equal (buffer-string) "say «word» now")))
  (donkey-wrap-test--own #'text-mode "say word now" "w m w SPC i & \" 6"
    (should (equal (buffer-string) "say “word” now")))
  ;; A guillemet is a pair `m i' knows but no key in Normal state, so
  ;; `SPC i &' is the way to take the pair off as well as to put it on.
  ;; Point is placed rather than moved to: from a motion it can land ON
  ;; the closing guillemet, where `m i' resolves its delimiter from the
  ;; buffer and never prompts, and the keys meant for the prompt run as
  ;; commands instead.
  (donkey-wrap-test--own #'text-mode "say «word» now" ""
    (goto-char 7)
    ;; The same keys either way: `SPC i &' names the pair for `m i',
    ;; and takes it off once the pair is selected.
    (execute-kbd-macro (kbd "m i SPC i & < < SPC i & < <"))
    (should (equal (buffer-string) "say word now"))))

(ert-deftest donkey-a-digraph-wraps-a-selection-however-it-was-made ()
  "`SPC i &' wraps whatever is selected, whoever selected it.

A selection is a selection: the key that wraps one does not ask which
command made it, so `m w', `v', `V', `m i', `m a', a mark run and a
rectangle all hand it the same thing.  Pinned across the family
because a wrap that worked after one of them and not another would be
a rule nobody could state."
  (dolist (case '(("m w"        "say word now" "w m w SPC i & < <"        "say «word» now")
                  ("v + motion" "say word now" "w v w SPC i & < <"        "say« word» now")
                  ("V"          "say word now" "V SPC i & < <"            "«say word now»")
                  ("mark run"   "say word now" "w M w SPC i & < <"        "say «word now»")))
    (cl-destructuring-bind (what text keys expected) case
      (donkey-wrap-test--own #'text-mode text keys
        (should (equal (list what (buffer-string)) (list what expected))))))
  ;; `m i' and `m a' with point placed rather than travelled to: from a
  ;; motion point can land ON a delimiter, where they resolve it from
  ;; the buffer and the keys meant for the prompt act instead.
  (dolist (case '((5 "m i SPC i & < <" "say («word») now")
                  (5 "m a SPC i & < <" "say «(word)» now")))
    (cl-destructuring-bind (pos keys expected) case
      (donkey-wrap-test--own #'text-mode "say (word) now" ""
        (goto-char pos)
        (execute-kbd-macro (kbd keys))
        (should (equal (list keys (buffer-string)) (list keys expected))))))
  ;; and a rectangle, which wraps row by row
  (donkey-wrap-test--own #'text-mode "ab\ncd\n" "m v j l SPC i & < <"
    (should (equal (buffer-string) "«ab»\n«cd»\n"))))

(ert-deftest donkey-a-digraph-wrap-ignores-its-count ()
  "A count wraps once, where without a selection it repeats the character.

The count belongs to the insertion; a wrap is one press, one wrap, the
way the delimiter keys read a count."
  (donkey-wrap-test--own #'text-mode "say word now" "w C-u 3 SPC i & < <"
    (should (equal (buffer-string) "say««« word now")))
  (donkey-wrap-test--own #'text-mode "say word now" "w m w C-u 3 SPC i & < <"
    (should (equal (buffer-string) "say «word» now"))))

;;; ---------------------------------------------------------------------------
;;; What the insert-entry keys do to a selection, a rectangle and a bank
;;; ---------------------------------------------------------------------------

(defmacro donkey-entry-test (text keys &rest body)
  "Type KEYS into a DONKEY buffer of TEXT, then run BODY.

A thin skin over `donkey-test-keys--harness\=', which carries the
rationale for the displayed buffer and the real keys: selection state
is settled by the command loop rather than by the command, so a
directly called entry command reports a selection still active no
matter what it did."
  (declare (indent 2))
  `(donkey-test-keys--harness "*donkey-entry-test*" #'text-mode ()
       ,text ,keys
     ,@body))

(ert-deftest donkey-insert-entry-keys-drop-a-selection-and-enter-insert ()
  "Every insert-entry key drops an active selection and starts typing.

Asserted across the six together because the property is what they have
in common: each deactivates the region, moves where its own name says,
and enters INSERT.  A seventh key joining them without the first step
would leave a selection standing that the next self-inserting character
replaces."
  (dolist (key '("i" "a" "I" "A" "o" "O"))
    (donkey-entry-test "ab\ncd\n" (concat "v l " key)
      (should (equal (list key (region-active-p) donkey-insert-mode)
                     (list key nil t))))))

(ert-deftest donkey-insert-entry-keys-drop-a-rectangle-rather-than-filling-it ()
  "A drawn rectangle is dropped, not block-inserted into.

The one place these keys could plausibly do something clever and do not:
vi\\='s `I\\=' over a visual block inserts on every row.  DONKEY has no
block insert, and `donkey-change\\=' is the key that acts on a rectangle
-- it replaces every row via `string-rectangle\\='.  So the two sit
beside each other answering differently, which is why the docstrings now
say so and why this pins it.

The buffer is asserted unchanged for the four that only move, since
\"dropped the rectangle\" and \"quietly edited every row\" would
otherwise look alike from the state flags."
  (dolist (key '("i" "a" "I" "A"))
    (donkey-entry-test "ab\ncd\n" (concat "m v j l " key)
      (should (equal (list key (bound-and-true-p rectangle-mark-mode)
                           donkey-insert-mode (buffer-string))
                     (list key nil t "ab\ncd\n")))))
  ;; `o' and `O' add their line, and still leave no rectangle behind.
  (dolist (key '("o" "O"))
    (donkey-entry-test "ab\ncd\n" (concat "m v j l " key)
      (should (equal (list key (bound-and-true-p rectangle-mark-mode)
                           donkey-insert-mode)
                     (list key nil t))))))

(ert-deftest donkey-insert-entry-keys-leave-a-bank-standing ()
  "Banked lines survive entering INSERT, as they survive `donkey-change\\='.

`donkey-copy\\=', `donkey-delete\\=' and `donkey-yank\\=' spend a bank.
Entering INSERT is not an operation on a selection, so there is nothing
for a bank to mean here and it is left alone -- which nothing asserted
until now, in either direction."
  (dolist (key '("i" "a" "I" "A" "o" "O"))
    (donkey-entry-test "ab\ncd\n" (concat "m l " key)
      (should (equal (list key (length donkey--banked-overlays)
                           donkey-insert-mode)
                     (list key 1 t))))))

(ert-deftest donkey-insert-after-at-end-of-buffer-still-enters-insert ()
  "`a\\=' at the very end of the buffer starts typing instead of refusing.

There is no character to step over there, so the `forward-char\\=' would
signal and abort before the state change -- leaving Normal state active
with only an end-of-buffer message to explain it.  Guarded in the source,
and asserted here alongside the ordinary case so the guard cannot be
mistaken for dead code."
  (donkey-entry-test "ab\n" "G a"
    (should donkey-insert-mode)
    (should (= (point) (point-max))))
  (donkey-entry-test "ab\n" "a"
    (should donkey-insert-mode)
    (should (= (point) 2))))

(provide 'donkey-insert-mode-entry-test)

;;; donkey-insert-mode-entry-test.el ends here

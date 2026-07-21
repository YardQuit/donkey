;;; donkey-insert-mode-entry-test.el --- Tests for DONKEY commands entering INSERT state -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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
  "When mark-active and a region is active, deactivates the mark."
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
  "When no region is active, deactivate-mark is not called."
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
  "Can be called via call-interactively without error."
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
  "forward-char executes before donkey-enter-insert."
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
  "At point-max, forward-char's end-of-buffer error is caught and
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
  "forward-char is called exactly once with arg 1."
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
  "Can be called via call-interactively."
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
  "beginning-of-line executes before donkey-enter-insert."
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
  "Can be called via call-interactively."
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
  "Ignores current-prefix-arg."
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
  "move-end-of-line executes before donkey-enter-insert."
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
  "move-end-of-line moves past trailing whitespace to the newline position."
  (with-temp-buffer
    (insert "hello   \n")
    (goto-char 1)
    (cl-letf (((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (donkey-insert-end-of-line))
    (should (= (point) 9))))

(ert-deftest donkey-insert-end-of-line-with-tabs ()
  "move-end-of-line handles tabs correctly."
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
  "Can be called via call-interactively."
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
  "Ignores current-prefix-arg."
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
  "Executes bol, newline, forward-line -1, indent, then enter-insert."
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
      (cl-letf (((symbol-function 'region-active-p)
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
  "When no region active, does not call deactivate-mark."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'region-active-p)
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
  "forward-line is called with argument -1 to move up after inserting."
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
  "Can be called via call-interactively."
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
      (cl-letf (((symbol-function 'region-active-p)
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
  "When no region active, does not call deactivate-mark."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'region-active-p)
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
  "Can be called via call-interactively."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'donkey-open-below))
      (should entered)
      (should (= (point) 7)))))

;;; ---------------------------------------------------------------------------
;;; donkey-change
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-change-no-region-deletes-single-char ()
  "Without an active region, deletes the character at point."
  (let (entered deleted-arg)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-delete-char (symbol-function 'delete-char)))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () nil))
                  ((symbol-function 'delete-char)
                   (lambda (n)
                     (setq deleted-arg n)
                     (funcall orig-delete-char n)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (donkey-change)))
      (should entered)
      (should (eq deleted-arg 1))
      (should (= (buffer-size) 5)))))

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

(ert-deftest donkey-change-no-region-empty-buffer-errors ()
  "Empty buffer with no region signals end-of-buffer (unguarded)."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (should-error (donkey-change) :type 'end-of-buffer))))

(ert-deftest donkey-change-no-region-at-end-of-buffer-errors ()
  "Point at point-max with no region signals end-of-buffer."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-max))
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'donkey-enter-insert)
               (lambda () nil)))
      (should-error (donkey-change) :type 'end-of-buffer))))

(ert-deftest donkey-change-region-deletes-region ()
  "With an active region (not rectangle), deletes from mark to point."
  (let (entered deleted-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (let ((orig-delete-region (symbol-function 'delete-region)))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () t))
                  ((symbol-function 'delete-region)
                   (lambda (beg end)
                     (setq deleted-bounds (list beg end))
                     (funcall orig-delete-region beg end)))
                  ((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (let ((rectangle-mark-mode nil))
            (donkey-change))))
      (should entered)
      (should deleted-bounds)
      (should (= (car deleted-bounds) 1))
      (should (= (cadr deleted-bounds) 6)))))

(ert-deftest donkey-change-region-skips-delete-char ()
  "With an active region, delete-char is not called."
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
  "Region with point before mark: delete-region receives (mark, point)."
  (let (deleted-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (push-mark 6)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-region)
                 (lambda (beg end) (setq deleted-bounds (list beg end))))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (donkey-change)))
      (should (= (car deleted-bounds) 6))
      (should (= (cadr deleted-bounds) 1)))))

(ert-deftest donkey-change-rectangle-mode-calls-string-rectangle ()
  "With region active and rectangle-mark-mode enabled, delegates to
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
  "In rectangle mode, delete-region is not called."
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
  "When rectangle-mark-mode is nil and region is active, falls back to
delete-region rather than string-rectangle."
  (let (delete-called ci-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq ci-called t)))
                ((symbol-function 'delete-region)
                 (lambda (beg end) (setq delete-called t)))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (donkey-change)))
      (should delete-called)
      (should-not ci-called))))

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

(provide 'donkey-insert-mode-entry-test)

;;; donkey-insert-mode-entry-test.el ends here

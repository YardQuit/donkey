;;; mule-open-below-test.el --- Tests for mule-open-below -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule-open-below
;; Selector: (ert "mule-open-below")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- Basic functionality ---

(ert-deftest mule-open-below-moves-to-eol-newlines-and-enters-insert ()
  "Moves to end of line, inserts newline+indent, enters insert mode.
Buffer: \"hello\\n\" — 6 chars.
After: \"hello\\n\\n\" — point at 7 (after the inserted newline).
Expected: mule-enter-insert called, point at 7, buffer has 7 chars."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-open-below))
      (should entered)
      (should (= (point) 7))
      (should (= (buffer-size) 7)))))

(ert-deftest mule-open-below-call-order ()
  "Executes in order: deactivate-mark (if region), move-end-of-line,
newline-and-indent, mule-enter-insert.
Uses push to track order (newest at front).
Expected: nth 0 = enter, nth 1 = newline, nth 2 = eol."
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
                  ((symbol-function 'mule-enter-insert)
                   (lambda ()
                     (push 'enter order))))
          (mule-open-below))))
    (should (eq (nth 0 order) 'enter))
    (should (eq (nth 1 order) 'newline))
    (should (eq (nth 2 order) 'eol))
    (should (= (length order) 3))))

;;; --- Region handling ---

(ert-deftest mule-open-below-deactivates-active-region ()
  "When region is active, deactivates the mark before proceeding.
Expected: deactivate-mark called."
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
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (mule-open-below)))
    (should deactivated)))

(ert-deftest mule-open-below-skips-deactivate-when-no-region ()
  "When no region active, does not call deactivate-mark.
Expected: deactivate-mark not called."
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
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (mule-open-below)))
    (should-not deactivated)))

;;; --- Position after opening ---

(ert-deftest mule-open-below-from-middle-of-line ()
  "Point in middle of line; opens below and places point on new line.
Buffer: \"hello world\\n\" — 12 chars.
After: \"hello world\\n\\n\" — point at 13 (after inserted newline).
Expected: point at 13."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-open-below))
    (should (= (point) 13))))

(ert-deftest mule-open-below-from-second-line ()
  "Point on second line; opens below second line.
Buffer: \"one\\ntwo\\n\" — 8 chars.
After: \"one\\ntwo\\n\\n\" — point at 9 (after inserted newline).
Expected: point at 9."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-open-below))
    (should (= (point) 9))))

(ert-deftest mule-open-below-from-beginning-of-line ()
  "Point at beginning of line; still moves to end of line first.
Buffer: \"hello\\n\" — 6 chars.
After: \"hello\\n\\n\" — point at 7.
Expected: point at 7."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-open-below))
    (should (= (point) 7))))

;;; --- Edge cases ---

(ert-deftest mule-open-below-on-empty-buffer ()
  "Empty buffer; creates first line.
Buffer: \"\" — 0 chars.
After: \"\\n\" — point at 2 (after the newline created by newline-and-indent).
Expected: point at 2, mule-enter-insert called."
  (let (entered)
    (with-temp-buffer
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-open-below))
      (should entered)
      (should (= (point) 2)))))

(ert-deftest mule-open-below-on-line-without-newline ()
  "Buffer without trailing newline; adds newline at end.
Buffer: \"hello\" — 5 chars, point-max = 6.
After: \"hello\\n\" — point at 7 (after the inserted newline from newline-and-indent).
Expected: point at 7."
  (let (entered)
    (with-temp-buffer
      (insert "hello")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-open-below))
      (should entered)
      (should (= (point) 7)))))

(ert-deftest mule-open-below-on-empty-line ()
  "Point on an empty line (just newline).
Buffer: \"\\n\" — 1 char.
After: \"\\n\\n\" — point at 2 (after the inserted newline).
Expected: point at 2."
  (with-temp-buffer
    (insert "\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-open-below))
    (should (= (point) 2))))

(ert-deftest mule-open-below-single-char-buffer ()
  "Single character, no newline.
Buffer: \"x\" — 1 char, point-max = 2.
After: \"x\\n\\n\" — point at 3 (after the inserted newline).
Expected: point at 3."
  (with-temp-buffer
    (insert "x")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-open-below))
    (should (= (point) 3))))

(ert-deftest mule-open-below-last-line-with-newline ()
  "Point on last line that has a trailing newline.
Buffer: \"a\\nb\\n\" — 4 chars.
Point at 3 (start of 'b'), after: \"a\\nb\\n\\n\" — point at 5.
Expected: point at 5."
  (with-temp-buffer
    (insert "a\nb\n")
    (goto-char 3)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-open-below))
    (should (= (point) 5))))

(ert-deftest mule-open-below-preserves-existing-content ()
  "Existing content is preserved; only a new line is added.
Buffer: \"first\\nsecond\\n\" — 13 chars.
After open-below inserts a newline after first line:
  \"first\\n\\nsecond\\n\" — 14 chars.
buffer-substring is exclusive of END, so use (1 6) for 5 chars.
Expected: 'first' at 1-5, 'second' at 8-13."
  (with-temp-buffer
    (insert "first\nsecond\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-open-below))
    (should (string= (buffer-substring 1 6) "first"))
    (should (string= (buffer-substring 8 14) "second"))))

;;; --- Buffer integrity ---

(ert-deftest mule-open-below-calls-newline-and-indent-exactly-once ()
  "newline-and-indent is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'newline-and-indent)
                 (lambda ()
                   (cl-incf call-count)
                   (insert "\n")))
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (mule-open-below)))
    (should (= call-count 1))))

(ert-deftest mule-open-below-calls-enter-insert-exactly-once ()
  "mule-enter-insert is called exactly once.
Expected: call-count = 1."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'newline-and-indent)
                 (lambda () (insert "\n")))
                ((symbol-function 'mule-enter-insert)
                 (lambda () (cl-incf call-count))))
        (mule-open-below)))
    (should (= call-count 1))))

;;; --- Interactive call ---

(ert-deftest mule-open-below-call-interactively ()
  "Can be called via call-interactively.
Expected: no error, new line created, insert entered."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'mule-open-below))
      (should entered)
      (should (= (point) 7)))))

;;; mule-open-below-test.el ends here

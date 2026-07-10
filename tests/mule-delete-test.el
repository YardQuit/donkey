;;; mule-delete-test.el --- Tests for mule-delete -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

(defvar rectangle-mark-mode)

;; ===========================================================================
;; Section: mule-delete
;; Selector: (ert "mule-delete")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- No region: deletes single char ---

(ert-deftest mule-delete-no-region-deletes-single-char ()
  "Without an active region, deletes the character at point.
Buffer: \"hello\\n\" — 6 chars. Point at 1.
After: \"ello\\n\" — 5 chars.
Expected: delete-char called with 1, buffer has 5 chars."
  (let (deleted-arg)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((orig-delete-char (symbol-function 'delete-char)))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () nil))
                  ((symbol-function 'delete-char)
                   (lambda (n)
                     (setq deleted-arg n)
                     (funcall orig-delete-char n))))
          (mule-delete)))
      (should (eq deleted-arg 1))
      (should (= (buffer-size) 5)))))

(ert-deftest mule-delete-no-region-from-middle ()
  "Deletes the character at point in the middle of a line.
Buffer: \"hello\\n\" — 6 chars. Point at 3 ('l').
After: \"helo\\n\" — 5 chars.
Expected: buffer has 5 chars, point at 3."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 3)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (mule-delete))
    (should (= (buffer-size) 5))
    (should (= (point) 3))))

(ert-deftest mule-delete-no-region-last-char-before-newline ()
  "Deletes the last character before newline.
Buffer: \"hello\\n\" — 6 chars. Point at 5 ('o').
After: \"hell\\n\" — 5 chars.
Expected: buffer has 5 chars, point at 5."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (mule-delete))
    (should (= (buffer-size) 5))
    (should (= (point) 5))))

(ert-deftest mule-delete-no-region-does-not-enter-insert ()
  "mule-delete does not enter insert mode (unlike mule-change).
Expected: mule-enter-insert not called."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'delete-char)
                 (lambda (n) (ignore n)))
                ((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (mule-delete)))
    (should-not entered)))

(ert-deftest mule-delete-no-region-empty-buffer-errors ()
  "Empty buffer, no region. delete-char 1 signals end-of-buffer.
Expected: end-of-buffer error signaled."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (should-error (mule-delete) :type 'end-of-buffer))))

(ert-deftest mule-delete-no-region-at-end-of-buffer-errors ()
  "Point at point-max, no region. delete-char 1 signals end-of-buffer.
Expected: end-of-buffer error signaled."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-max))
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (should-error (mule-delete) :type 'end-of-buffer))))

;;; --- Region active: kill-region ---

(ert-deftest mule-delete-region-kills-region ()
  "With an active region (not rectangle), kills from mark to point.
Buffer: \"hello world\\n\" — 12 chars.
Point at 6, mark at 1.
Expected: kill-region called with (1, 6)."
  (let (killed-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (let ((orig-kill-region (symbol-function 'kill-region)))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () t))
                  ((symbol-function 'kill-region)
                   (lambda (beg end)
                     (setq killed-bounds (list beg end))
                     (funcall orig-kill-region beg end))))
          (let ((rectangle-mark-mode nil))
            (mule-delete))))
      (should killed-bounds)
      (should (= (car killed-bounds) 1))
      (should (= (cadr killed-bounds) 6)))))

(ert-deftest mule-delete-region-mark-before-point ()
  "Region with mark before point.
Buffer: \"hello world\\n\" — 12 chars.
Mark at 1, point at 6.
kill-region called with (mark, point) = (1, 6).
Expected: kill-region receives (1, 6)."
  (let (killed-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'kill-region)
                 (lambda (beg end) (setq killed-bounds (list beg end))))
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (mule-delete)))
      (should (= (car killed-bounds) 1))
      (should (= (cadr killed-bounds) 6)))))

(ert-deftest mule-delete-region-point-before-mark ()
  "Region with point before mark.
Buffer: \"hello world\\n\" — 12 chars.
Point at 1, mark at 6.
kill-region called with (mark, point) = (6, 1).
The source uses (kill-region (mark) (point)), so mark is first arg.
Expected: kill-region receives (6, 1)."
  (let (killed-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (push-mark 6)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'kill-region)
                 (lambda (beg end) (setq killed-bounds (list beg end))))
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (mule-delete)))
      (should (= (car killed-bounds) 6))
      (should (= (cadr killed-bounds) 1)))))

(ert-deftest mule-delete-region-skips-delete-char ()
  "With an active region, delete-char is not called.
Expected: delete-char not invoked."
  (let (delete-char-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'kill-region)
                 (lambda (beg end) (ignore beg end)))
                ((symbol-function 'delete-char)
                 (lambda (n) (setq delete-char-called t))))
        (let ((rectangle-mark-mode nil))
          (mule-delete)))
      (should-not delete-char-called))))

(ert-deftest mule-delete-region-does-not-enter-insert ()
  "mule-delete with region does not enter insert mode.
Expected: mule-enter-insert not called."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'kill-region)
                 (lambda (beg end) (ignore beg end)))
                ((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (let ((rectangle-mark-mode nil))
          (mule-delete)))
      (should-not entered))))

;;; --- Rectangle mode ---

(ert-deftest mule-delete-rectangle-mode-calls-kill-rectangle ()
  "With an active region and rectangle-mark-mode enabled,
delegates to call-interactively kill-rectangle.
Expected: call-interactively called with kill-rectangle."
  (let (called-cmd)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd))))
        (let ((rectangle-mark-mode t))
          (mule-delete))))
    (should (eq called-cmd 'kill-rectangle))))

(ert-deftest mule-delete-rectangle-mode-skips-kill-region ()
  "In rectangle mode, kill-region is not called.
Expected: kill-region not invoked."
  (let (kill-region-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (ignore cmd)))
                ((symbol-function 'kill-region)
                 (lambda (beg end) (setq kill-region-called t))))
        (let ((rectangle-mark-mode t))
          (mule-delete))))
    (should-not kill-region-called)))

(ert-deftest mule-delete-rectangle-mode-skips-delete-char ()
  "In rectangle mode, delete-char is not called.
Expected: delete-char not invoked."
  (let (delete-char-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (ignore cmd)))
                ((symbol-function 'delete-char)
                 (lambda (n) (setq delete-char-called t))))
        (let ((rectangle-mark-mode t))
          (mule-delete)))
      (should-not delete-char-called))))

(ert-deftest mule-delete-rectangle-mode-falls-back-when-disabled ()
  "When rectangle-mark-mode is nil and region is active,
falls back to kill-region.
Expected: kill-region called, call-interactively not called."
  (let (kill-called ci-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq ci-called t)))
                ((symbol-function 'kill-region)
                 (lambda (beg end) (setq kill-called t))))
        (let ((rectangle-mark-mode nil))
          (mule-delete)))
      (should kill-called)
      (should-not ci-called))))

;;; --- Buffer integrity ---

(ert-deftest mule-delete-no-region-preserves-surrounding-text ()
  "Deleting one char leaves the rest of the buffer intact.
Buffer: \"hello world\\n\" — 12 chars. Point at 6 (space).
After: \"helloworld\\n\" — 11 chars.
Expected: 'hello' at 1-5, 'world' at 6-10."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (mule-delete))
    (should (string= (buffer-substring 1 6) "hello"))
    (should (string= (buffer-substring 6 11) "world"))))

(ert-deftest mule-delete-region-kills-correct-text ()
  "Killing a region removes exactly the marked text.
Buffer: \"hello world\\n\" — 12 chars.
Mark at 1, point at 6. Kills 'hello'.
After: \" world\\n\" — 7 chars.
Expected: buffer starts with ' world'."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t)))
      (let ((rectangle-mark-mode nil))
        (mule-delete)))
    (should (string= (buffer-substring 1 7) " world"))))

;;; --- Interactive call ---

(ert-deftest mule-delete-call-interactively-no-region ()
  "Can be called interactively without a region.
Expected: no error, delete-char executes."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (call-interactively #'mule-delete))
    (should (= (buffer-size) 5))))

(ert-deftest mule-delete-call-interactively-with-region ()
  "Can be called interactively with a region.
Expected: no error, kill-region executes."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t)))
      (let ((rectangle-mark-mode nil))
        (call-interactively #'mule-delete)))
    (should (= (buffer-size) 7))))

;;; mule-delete-test.el ends here

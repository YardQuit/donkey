;;; mule-change-test.el --- Tests for mule-change -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

(defvar rectangle-mark-mode)

;; ===========================================================================
;; Section: mule-change
;; Selector: (ert "mule-change")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- No region: deletes single char ---

(ert-deftest mule-change-no-region-deletes-single-char ()
  "Without an active region, deletes the character at point.
    Buffer: \"hello\\n\" — 6 chars. Point at 1.
    After delete-char 1: \"ello\\n\" — 5 chars.
    Expected: delete-char called with 1, buffer has 5 chars."
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
                  ((symbol-function 'mule-enter-insert)
                   (lambda () (setq entered t))))
          (mule-change)))
      (should entered)
      (should (eq deleted-arg 1))
      (should (= (buffer-size) 5)))))

(ert-deftest mule-change-no-region-enters-insert ()
  "Without region, enters insert mode after deletion.
    Expected: mule-enter-insert called exactly once."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'delete-char)
                 (lambda (n) (ignore n)))
                ((symbol-function 'mule-enter-insert)
                 (lambda () (cl-incf call-count))))
        (mule-change)))
    (should (= call-count 1))))

(ert-deftest mule-change-no-region-from-middle ()
  "Deletes the character at point in the middle of a line.
    Buffer: \"hello\\n\" — 6 chars. Point at 3 ('l').
    After delete-char 1: \"helo\\n\" — 5 chars.
    Expected: buffer has 5 chars, point at 3."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 3)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-change))
    (should (= (buffer-size) 5))
    (should (= (point) 3))))

(ert-deftest mule-change-no-region-last-char ()
  "Deletes the last character before newline.
    Buffer: \"hello\\n\" — 6 chars. Point at 5 ('o').
    After delete-char 1: \"hell\\n\" — 5 chars.
    Expected: buffer has 5 chars, point at 5."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 5)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-change))
    (should (= (buffer-size) 5))
    (should (= (point) 5))))

(ert-deftest mule-change-no-region-empty-buffer-errors ()
  "Empty buffer, no region. delete-char 1 signals end-of-buffer.
    The function does not guard against this.
    Expected: end-of-buffer error signaled."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (should-error (mule-change) :type 'end-of-buffer))))

(ert-deftest mule-change-no-region-at-end-of-buffer-errors ()
  "Point at point-max, no region. delete-char 1 signals end-of-buffer.
    Expected: end-of-buffer error signaled."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-max))
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (should-error (mule-change) :type 'end-of-buffer))))

    ;;; --- Region active: delete-region ---

(ert-deftest mule-change-region-deletes-region ()
  "With an active region (not rectangle), deletes from mark to point.
    Buffer: \"hello world\\n\" — 12 chars.
    Point at 6, mark at 1.
    delete-region deletes positions 1-5, leaving \" world\\n\".
    Expected: delete-region called with (1, 6), mule-enter-insert called."
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
                  ((symbol-function 'mule-enter-insert)
                   (lambda () (setq entered t))))
          (let ((rectangle-mark-mode nil))
            (mule-change))))
      (should entered)
      (should deleted-bounds)
      (should (= (car deleted-bounds) 1))
      (should (= (cadr deleted-bounds) 6)))))

(ert-deftest mule-change-region-enters-insert ()
  "With an active region (not rectangle), enters insert mode after
    deletion.
    Expected: mule-enter-insert called exactly once."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-region)
                 (lambda (beg end) (ignore beg end)))
                ((symbol-function 'mule-enter-insert)
                 (lambda () (cl-incf call-count))))
        (let ((rectangle-mark-mode nil))
          (mule-change)))
      (should (= call-count 1)))))

(ert-deftest mule-change-region-skips-delete-char ()
  "With an active region, delete-char is not called.
    Expected: delete-char not invoked."
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
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (mule-change)))
      (should-not delete-char-called))))

(ert-deftest mule-change-region-mark-before-point ()
  "Region with mark before point.
    Buffer: \"hello world\\n\" — 12 chars.
    Mark at 1, point at 6.
    delete-region called with (mark, point) = (1, 6).
    Expected: delete-region receives (1, 6)."
  (let (deleted-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-region)
                 (lambda (beg end) (setq deleted-bounds (list beg end))))
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (mule-change)))
      (should (= (car deleted-bounds) 1))
      (should (= (cadr deleted-bounds) 6)))))

(ert-deftest mule-change-region-point-before-mark ()
  "Region with point before mark.
    Buffer: \"hello world\\n\" — 12 chars.
    Point at 1, mark at 6.
    delete-region called with (mark, point) = (6, 1).
    The source uses (delete-region (mark) (point)), so mark is first arg.
    Expected: delete-region receives (6, 1)."
  (let (deleted-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (push-mark 6)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-region)
                 (lambda (beg end) (setq deleted-bounds (list beg end))))
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (mule-change)))
      (should (= (car deleted-bounds) 6))
      (should (= (cadr deleted-bounds) 1)))))

(ert-deftest mule-change-region-single-char-region ()
  "Region covers a single character.
    Buffer: \"abc\\n\" — 4 chars.
    Point at 1, mark at 2.
    delete-region called with (2, 1).
    Expected: delete-region receives (2, 1)."
  (let (deleted-bounds)
    (with-temp-buffer
      (insert "abc\n")
      (goto-char 1)
      (push-mark 2)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-region)
                 (lambda (beg end) (setq deleted-bounds (list beg end))))
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (mule-change)))
      (should (= (car deleted-bounds) 2))
      (should (= (cadr deleted-bounds) 1)))))

    ;;; --- Rectangle mode ---

(ert-deftest mule-change-rectangle-mode-calls-string-rectangle ()
  "With an active region and rectangle-mark-mode enabled,
    delegates to call-interactively string-rectangle.
    Expected: call-interactively called with string-rectangle."
  (let (called-cmd)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd)))
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode t))
          (mule-change)))
      (should (eq called-cmd 'string-rectangle)))))

(ert-deftest mule-change-rectangle-mode-skips-delete-region ()
  "In rectangle mode, delete-region is not called.
    Expected: delete-region not invoked."
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
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode t))
          (mule-change)))
      (should-not delete-region-called))))

(ert-deftest mule-change-rectangle-mode-enters-insert ()
  "In rectangle mode, still enters insert mode after string-rectangle.
    Expected: mule-enter-insert called."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (ignore cmd)))
                ((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (let ((rectangle-mark-mode t))
          (mule-change)))
      (should entered))))

(ert-deftest mule-change-rectangle-mode-falls-back-when-disabled ()
  "When rectangle-mark-mode is nil and region is active,
    falls back to delete-region.
    Expected: delete-region called, call-interactively not called."
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
                ((symbol-function 'mule-enter-insert)
                 (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (mule-change)))
      (should delete-called)
      (should-not ci-called))))

    ;;; --- Buffer integrity ---

(ert-deftest mule-change-no-region-preserves-surrounding-text ()
  "Deleting one char leaves the rest of the buffer intact.
    Buffer: \"hello world\\n\" — 12 chars. Point at 6 (space).
    After delete-char 1: \"helloworld\\n\" — 11 chars.
    Expected: 'hello' at 1-5, 'world' at 6-10."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (mule-change))
    (should (string= (buffer-substring 1 6) "hello"))
    (should (string= (buffer-substring 6 11) "world"))))

(ert-deftest mule-change-region-deletes-correct-text ()
  "Deleting a region removes exactly the marked text.
    Buffer: \"hello world\\n\" — 12 chars.
    Mark at 1, point at 6. Deletes 'hello'.
    After: \" world\\n\" — 7 chars.
    Expected: buffer starts with ' world'."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'mule-enter-insert)
               (lambda () nil)))
      (let ((rectangle-mark-mode nil))
        (mule-change)))
    (should (string= (buffer-substring 1 7) " world"))))

    ;;; --- Interactive call ---

(ert-deftest mule-change-call-interactively-no-region ()
  "Can be called interactively without a region.
    Expected: no error, delete-char and mule-enter-insert execute."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (call-interactively #'mule-change))
      (should entered)
      (should (= (buffer-size) 5)))))

(ert-deftest mule-change-call-interactively-with-region ()
  "Can be called interactively with a region.
    Expected: no error, delete-region and mule-enter-insert execute."
  (let (entered)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'mule-enter-insert)
                 (lambda () (setq entered t))))
        (let ((rectangle-mark-mode nil))
          (call-interactively #'mule-change)))
      (should entered)
      (should (= (buffer-size) 7)))))

    ;;; mule-change-test.el ends here

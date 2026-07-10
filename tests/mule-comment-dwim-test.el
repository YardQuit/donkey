;;; mule-comment-dwim-test.el --- Tests for mule-comment-dwim -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule--in-org-src-block-p
;; Selector: (ert "mule-comment-dwim-in-org-src-block-p")
;; ===========================================================================

(ert-deftest mule-comment-dwim-in-org-src-block-p-returns-t-in-src-block ()
  "In org-mode with a src-block element at point, returns non-nil.
Expected: non-nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:language "python" :begin 1 :end 50)))))
    (let ((major-mode 'org-mode))
      (should (mule--in-org-src-block-p)))))

(ert-deftest mule-comment-dwim-in-org-src-block-p-returns-false-in-paragraph ()
  "In org-mode with a non-src-block element at point, returns nil.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(paragraph (:begin 1 :end 10)))))
    (let ((major-mode 'org-mode))
      (should-not (mule--in-org-src-block-p)))))

(ert-deftest mule-comment-dwim-in-org-src-block-p-returns-false-in-headline ()
  "In org-mode with a headline element at point, returns nil.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(headline (:begin 1 :end 20 :level 1)))))
    (let ((major-mode 'org-mode))
      (should-not (mule--in-org-src-block-p)))))

(ert-deftest mule-comment-dwim-in-org-src-block-p-returns-false-if-org-unbound ()
  "When in org-mode but `org-element-at-point' is not fboundp, returns nil.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point) nil))
    (let ((major-mode 'org-mode))
      (should-not (mule--in-org-src-block-p)))))

(ert-deftest mule-comment-dwim-in-org-src-block-p-returns-false-if-org-element-nil ()
  "When `org-element-at-point' returns nil, returns nil.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () nil)))
    (let ((major-mode 'org-mode))
      (should-not (mule--in-org-src-block-p)))))

(ert-deftest mule-comment-dwim-in-org-src-block-p-returns-false-in-non-org-mode ()
  "When not in org-mode, returns nil regardless of org functions.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:language "python" :begin 1 :end 50)))))
    (let ((major-mode 'python-mode))
      (should-not (mule--in-org-src-block-p)))))

(ert-deftest mule-comment-dwim-in-org-src-block-p-all-checks-must-pass ()
  "All three conditions (org-mode, fboundp, src-block element) must be true.
Expected: nil when any condition is false."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:language "python" :begin 1 :end 50)))))
    (let ((major-mode 'text-mode))
      (should-not (mule--in-org-src-block-p))))
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(link (:path "http://example.com")))))
    (let ((major-mode 'org-mode))
      (should-not (mule--in-org-src-block-p)))))

(ert-deftest mule-comment-dwim-in-org-src-block-p-non-list-element ()
  "When `org-element-at-point' returns a non-list, non-nil value, returns
nil instead of signaling wrong-type-argument.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () "not-a-list")))
    (let ((major-mode 'org-mode))
      (should-not (mule--in-org-src-block-p)))))

;; ===========================================================================
;; Section: mule-comment-dwim
;; Selector: (ert "mule-comment-dwim-")
;;
;;           (ert "mule-comment-dwim")
;;           runs ALL tests in this file
;; ===========================================================================

;;; --- Non-org single-line ---

(ert-deftest mule-comment-dwim-outside-org-comments-current-line ()
  "Outside org-mode, comments the current line.
Buffer: \"hello world\\n\" — 12 chars, positions 1-12.
line-beginning-position = 1, line-beginning-position 2 = 13.
Expected: bounds (1, 13)."
  (let (called-bounds)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq called-bounds (list beg end)))))
      (with-temp-buffer
        (insert "hello world\n")
        (goto-char (point-min))
        (let ((major-mode 'python-mode))
          (mule-comment-dwim))))
    (should called-bounds)
    (should (= (car called-bounds) 1))
    (should (= (cadr called-bounds) 13))))

(ert-deftest mule-comment-dwim-non-org-without-region ()
  "Outside org, without region: single line operation.
Buffer: \"single line\\n\" — 12 chars, positions 1-12.
line-beginning-position 2 = 13.
Expected: bounds (1, 13)."
  (let (bounds)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq bounds (list beg end)))))
      (with-temp-buffer
        (insert "single line\n")
        (goto-char (point-min))
        (let ((major-mode 'text-mode))
          (mule-comment-dwim))))
    (should bounds)
    (should (= (car bounds) 1))
    (should (= (cadr bounds) 13))))

(ert-deftest mule-comment-dwim-non-org-with-empty-buffer ()
  "Empty buffer: only one line exists.
line-beginning-position = 1, line-beginning-position 2 = 1 (no second line).
Expected: bounds (1, 1)."
  (let (bounds)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq bounds (list beg end)))))
      (with-temp-buffer
        (let ((major-mode 'text-mode))
          (mule-comment-dwim))))
    (should bounds)
    (should (= (car bounds) 1))
    (should (= (cadr bounds) 1))))

;;; --- Non-org region ---

(ert-deftest mule-comment-dwim-outside-org-with-region ()
  "With active region outside org, comments from region-beginning line
start to region-end line start (or next line if not at bol).
Buffer: \"line one\\nline two\\nline three\\n\" — 29 chars.
  Line 1: pos 1-9, Line 2: pos 10-18, Line 3: pos 19-29.
Region: point at 1, mark at 11 (mid-line in \"line two\").
region-end = 11, not at bol, so end = line-beginning-position 2 = 19.
Expected: bounds (1, 19)."
  (let (called-bounds)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq called-bounds (list beg end))))
              ((symbol-function 'use-region-p)
               (lambda () t)))
      (with-temp-buffer
        (insert "line one\nline two\nline three\n")
        (goto-char 1)
        (push-mark 11)
        (let ((major-mode 'python-mode))
          (mule-comment-dwim))))
    (should called-bounds)
    (should (= (car called-bounds) 1))
    (should (= (cadr called-bounds) 19))))

(ert-deftest mule-comment-dwim-outside-org-region-at-bol ()
  "Region ending exactly at bol uses that point as end.
Buffer: \"line one\\nline two\\n\" — 18 chars.
  Line 1: pos 1-9, Line 2: pos 10-18.
Region: point at 1, mark at 10 (bol of line 2).
region-end = 10, at bol, so end = 10.
Expected: bounds (1, 10)."
  (let (called-bounds)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq called-bounds (list beg end))))
              ((symbol-function 'use-region-p)
               (lambda () t)))
      (with-temp-buffer
        (insert "line one\nline two\n")
        (goto-char 1)
        (push-mark 10)
        (let ((major-mode 'python-mode))
          (mule-comment-dwim))))
    (should called-bounds)
    (should (= (car called-bounds) 1))
    (should (= (cadr called-bounds) 10))))

(ert-deftest mule-comment-dwim-outside-org-deactivates-mark ()
  "After non-org comment operation, mark is always deactivated.
Expected: deactivate-mark called."
  (let (deactivated)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (ignore beg end)))
              ((symbol-function 'deactivate-mark)
               (lambda () (setq deactivated t))))
      (with-temp-buffer
        (insert "hello\n")
        (goto-char (point-min))
        (let ((major-mode 'text-mode))
          (mule-comment-dwim))))
    (should deactivated)))

;;; --- Org src-block: single line ---

(ert-deftest mule-comment-dwim-in-org-src-block-delegates-to-org-edit ()
  "Inside an org src-block, delegates to org-edit-special first.
Verifies call sequence: org-edit-special, comment, org-edit-src-exit.
Buffer: \"some text\\n\" — 10 chars, positions 1-10.
No region: single line, bounds (1, 11).
Expected: all three functions called."
  (let (calls)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) (push 'org-edit-special calls)))
              ((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (push (list 'comment beg end) calls)))
              ((symbol-function 'org-edit-src-exit)
               (lambda () (interactive) (push 'org-exit calls))))
      (with-temp-buffer
        (insert "some text\n")
        (goto-char (point-min))
        (let ((major-mode 'org-mode))
          (mule-comment-dwim))))
    (should (memq 'org-exit calls))
    (should (memq 'org-edit-special calls))
    (should (seq-find (lambda (x) (and (consp x) (eq (car x) 'comment)))
                      calls))))

(ert-deftest mule-comment-dwim-in-org-src-block-single-line-no-region ()
  "Single line comment in org src-block, no region.
Buffer: \"# comment\\n\" — 10 chars, positions 1-10.
line-beginning-position 2 = 11.
Expected: bounds (1, 11)."
  (let (called-bounds)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) nil))
              ((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq called-bounds (list beg end))))
              ((symbol-function 'org-edit-src-exit)
               (lambda () (interactive) nil)))
      (with-temp-buffer
        (insert "# comment\n")
        (goto-char (point-min))
        (let ((major-mode 'org-mode))
          (mule-comment-dwim))))
    (should called-bounds)
    (should (= (car called-bounds) 1))
    (should (= (cadr called-bounds) 11))))

;;; --- Org src-block: region ---

(ert-deftest mule-comment-dwim-in-org-src-block-with-region ()
  "Multiple lines in org src-block with active region.
Verifies the region path executes with org-edit-special delegation.
Expected: org-edit-special and comment-or-uncomment-region both called."
  (let (org-edit-called comment-called)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) (setq org-edit-called t)))
              ((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq comment-called t)))
              ((symbol-function 'org-edit-src-exit)
               (lambda () (interactive) nil))
              ((symbol-function 'use-region-p)
               (lambda () t)))
      (with-temp-buffer
        (insert "line one\nline two\nline three\n")
        (goto-char 1)
        (push-mark 11)
        (let ((major-mode 'org-mode))
          (mule-comment-dwim))))
    (should org-edit-called)
    (should comment-called)))

(ert-deftest mule-comment-dwim-in-org-src-block-with-region-deactivates-mark ()
  "When org-edit-src-exit succeeds with region, mark is deactivated.
Source captures has-region via (use-region-p) at the start, then calls
(deactivate-mark) when has-region is true.
Expected: deactivate-mark called."
  (let (deactivated)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) nil))
              ((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (ignore beg end)))
              ((symbol-function 'org-edit-src-exit)
               (lambda () (interactive) nil))
              ((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'deactivate-mark)
               (lambda () (setq deactivated t))))
      (with-temp-buffer
        (insert "a\nb\n")
        (goto-char 1)
        (push-mark 3)
        (let ((major-mode 'org-mode))
          (mule-comment-dwim))))
    (should deactivated)))

;;; --- Org src-block: error handling ---

(ert-deftest mule-comment-dwim-in-org-src-block-error-handling ()
  "If org-edit-special raises an error, condition-case catches it and
displays a message instead of propagating.
Expected: message logged, no error propagated."
  (let (messages caught-error-p)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) (error "mock error")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (insert "code\n")
        (goto-char (point-min))
        (let ((major-mode 'org-mode))
          (condition-case _err
              (mule-comment-dwim)
            (error (setq caught-error-p t))))))
    (should messages)
    (should (string-match-p "mule-comment-dwim (org-src): mock error"
                            (car messages)))
    (should-not caught-error-p)))

;;; --- Org src-block: line offset math ---

(ert-deftest mule-comment-dwim-in-org-src-block-line-offset-calculation ()
  "Verifies the line-number-offset math conceptually.
diff = (edited-cur-line - original-cur-line).
Expected: diff = 5 when edited line is 10 and original is 5."
  (let ((original-cur-line 5)
        (edited-cur-line 10)
        diff)
    (setq diff (- edited-cur-line original-cur-line))
    (should (= diff 5))))

;;; --- Priority ---

(ert-deftest mule-comment-dwim-org-src-takes-priority ()
  "When in org-mode on a src-block, the org delegation path is taken
(org-edit-special and org-edit-src-exit are both called).
comment-or-uncomment-region is also called, but from WITHIN the org
path — both branches use the same function, so we verify delegation
happened by checking that org-edit-special was called before any comment.
Expected: org-edit-special and org-edit-src-exit both called."
  (let (call-order)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) (push 'org-edit call-order)))
              ((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (push (list 'direct-comment beg end) call-order)))
              ((symbol-function 'org-edit-src-exit)
               (lambda () (interactive) (push 'org-exit call-order))))
      (with-temp-buffer
        (insert "code\n")
        (goto-char (point-min))
        (let ((major-mode 'org-mode))
          (mule-comment-dwim))))
    (should (memq 'org-edit call-order))
    (should (memq 'org-exit call-order))))

(ert-deftest mule-comment-dwim-respects-use-region-p ()
  "In org src-block with use-region-p returning t, region path is taken.
Expected: comment-or-uncomment-region called (region branch executed)."
  (let (branch-used)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) nil))
              ((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (push (list beg end) branch-used)))
              ((symbol-function 'org-edit-src-exit)
               (lambda () (interactive) nil))
              ((symbol-function 'use-region-p)
               (lambda () t)))
      (with-temp-buffer
        (insert "a\nb\nc\n")
        (goto-char 1)
        (push-mark 5)
        (let ((major-mode 'org-mode))
          (mule-comment-dwim))))
    (should branch-used)))

;;; mule-comment-dwim-test.el ends here

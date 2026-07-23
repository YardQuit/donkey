;;; donkey-editing-test.el --- Tests for DONKEY delete/yank/indent/comment commands -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

(defvar rectangle-mark-mode)

;;; ---------------------------------------------------------------------------
;;; donkey-copy
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-copy-no-region-copies-single-char ()
  "Without an active region, copies the character at point via
kill-ring-save with an explicit (point . point+1) range."
  (let (copied-bounds)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'kill-ring-save)
                 (lambda (beg end) (setq copied-bounds (list beg end))))
                ((symbol-function 'deactivate-mark) (lambda () nil)))
        (donkey-copy))
      (should (equal copied-bounds '(3 4))))))

(ert-deftest donkey-copy-no-region-does-not-use-stale-mark ()
  "Regression test: without an ACTIVE region, donkey-copy must copy
only the character at point, never the raw mark position.

kill-ring-save's own interactive spec reads region-beginning/
region-end, which use wherever the mark last happened to be
regardless of whether the region is actually active -- a mark left
over from an earlier, unrelated command (e.g. a stale
donkey-mark-inner selection, or any prior push-mark) would silently
get copied instead of the single character at point.  Confirmed live
in emacs -nw: with mark left at position 10 and point moved to
position 20 (region inactive), a real 'y' keypress copied
\"jklmnopqrs\" (mark to point) instead of the single character under
the cursor."
  (let (copied-bounds)
    (with-temp-buffer
      (insert "abcdefghijklmnopqrstuvwxyz")
      (goto-char 1)
      (push-mark 10 nil t)
      (deactivate-mark)
      (goto-char 20)
      (cl-letf (((symbol-function 'kill-ring-save)
                 (lambda (beg end) (setq copied-bounds (list beg end)))))
        (donkey-copy))
      (should (equal copied-bounds '(20 21))))))

(ert-deftest donkey-copy-no-region-at-end-of-buffer-no-error ()
  "At point-max with no region, there is nothing to copy but this must
not error -- the range clamps to an empty span instead of extending
past the end of the buffer."
  (let (copied-bounds)
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'kill-ring-save)
                 (lambda (beg end) (setq copied-bounds (list beg end)))))
        (donkey-copy))
      (should (equal copied-bounds (list (point-max) (point-max)))))))

(ert-deftest donkey-copy-region-copies-region ()
  "With an active region (not rectangle), copies from region-beginning
to region-end."
  (let (copied-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'kill-ring-save)
                 (lambda (beg end) (setq copied-bounds (list beg end))))
                ((symbol-function 'deactivate-mark) (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (donkey-copy)))
      (should (equal copied-bounds '(1 6))))))

(ert-deftest donkey-copy-region-deactivates-mark ()
  "After copying a region, the mark is deactivated -- the selection
does not linger once yanked."
  (let (deactivated)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 6)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'kill-ring-save) (lambda (beg end) nil))
                ((symbol-function 'deactivate-mark)
                 (lambda () (setq deactivated t))))
        (let ((rectangle-mark-mode nil))
          (donkey-copy)))
      (should deactivated))))

(ert-deftest donkey-copy-rectangle-mode-calls-copy-rectangle-as-kill ()
  "With region active and rectangle-mark-mode enabled, delegates to
copy-rectangle-as-kill via call-interactively."
  (let (called-cmd)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd)))
                ((symbol-function 'deactivate-mark) (lambda () nil)))
        (let ((rectangle-mark-mode t))
          (donkey-copy))))
    (should (eq called-cmd 'copy-rectangle-as-kill))))

(ert-deftest donkey-copy-rectangle-mode-falls-back-when-disabled ()
  "When rectangle-mark-mode is nil, falls back to plain kill-ring-save."
  (let (copy-called ci-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'kill-ring-save)
                 (lambda (beg end) (setq copy-called t)))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq ci-called t)))
                ((symbol-function 'deactivate-mark) (lambda () nil)))
        (let ((rectangle-mark-mode nil))
          (donkey-copy))))
    (should copy-called)
    (should-not ci-called)))

;;; ---------------------------------------------------------------------------
;;; donkey-delete
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-delete-no-region-deletes-single-char ()
  "Without an active region, deletes the character at point."
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
          (donkey-delete)))
      (should (eq deleted-arg 1))
      (should (= (buffer-size) 5)))))

(ert-deftest donkey-delete-no-region-from-middle ()
  "Deletes the character at point in the middle of a line."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 3)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (donkey-delete))
    (should (= (buffer-size) 5))
    (should (= (point) 3))))

(ert-deftest donkey-delete-no-region-does-not-enter-insert ()
  "donkey-delete does not enter insert mode (unlike donkey-change)."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'delete-char)
                 (lambda (n) (ignore n)))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (donkey-delete)))
    (should-not entered)))

(ert-deftest donkey-delete-no-region-empty-buffer-errors ()
  "Empty buffer, no region: delete-char signals end-of-buffer."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (should-error (donkey-delete) :type 'end-of-buffer))))

(ert-deftest donkey-delete-no-region-at-end-of-buffer-errors ()
  "Point at point-max, no region: delete-char signals end-of-buffer."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-max))
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (should-error (donkey-delete) :type 'end-of-buffer))))

(ert-deftest donkey-delete-region-kills-region ()
  "With an active region (not rectangle), kills from mark to point."
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
            (donkey-delete))))
      (should killed-bounds)
      (should (= (car killed-bounds) 1))
      (should (= (cadr killed-bounds) 6)))))

(ert-deftest donkey-delete-region-point-before-mark ()
  "Region with point before mark: kill-region receives (mark, point)."
  (let (killed-bounds)
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char 1)
      (push-mark 6)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'kill-region)
                 (lambda (beg end) (setq killed-bounds (list beg end)))))
        (let ((rectangle-mark-mode nil))
          (donkey-delete)))
      (should (= (car killed-bounds) 6))
      (should (= (cadr killed-bounds) 1)))))

(ert-deftest donkey-delete-region-skips-delete-char ()
  "With an active region, delete-char is not called."
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
          (donkey-delete)))
      (should-not delete-char-called))))

(ert-deftest donkey-delete-region-does-not-enter-insert ()
  "donkey-delete with region does not enter insert mode."
  (let (entered)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 3)
      (push-mark 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'kill-region)
                 (lambda (beg end) (ignore beg end)))
                ((symbol-function 'donkey-enter-insert)
                 (lambda () (setq entered t))))
        (let ((rectangle-mark-mode nil))
          (donkey-delete)))
      (should-not entered))))

(ert-deftest donkey-delete-rectangle-mode-calls-kill-rectangle ()
  "With region active and rectangle-mark-mode enabled, delegates to
`kill-rectangle' via `call-interactively'."
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
          (donkey-delete))))
    (should (eq called-cmd 'kill-rectangle))))

(ert-deftest donkey-delete-rectangle-mode-falls-back-when-disabled ()
  "When rectangle-mark-mode is nil, falls back to kill-region."
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
          (donkey-delete)))
      (should kill-called)
      (should-not ci-called))))

(ert-deftest donkey-delete-no-region-preserves-surrounding-text ()
  "Deleting one char leaves the rest of the buffer intact."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (donkey-delete))
    (should (string= (buffer-substring 1 6) "hello"))
    (should (string= (buffer-substring 6 11) "world"))))

(ert-deftest donkey-delete-region-kills-correct-text ()
  "Killing a region removes exactly the marked text."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t)))
      (let ((rectangle-mark-mode nil))
        (donkey-delete)))
    (should (string= (buffer-substring 1 7) " world"))))

(ert-deftest donkey-delete-call-interactively-no-region ()
  "Can be called interactively without a region."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil)))
      (call-interactively #'donkey-delete))
    (should (= (buffer-size) 5))))

(ert-deftest donkey-delete-call-interactively-with-region ()
  "Can be called interactively with a region."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t)))
      (let ((rectangle-mark-mode nil))
        (call-interactively #'donkey-delete)))
    (should (= (buffer-size) 7))))

;;; ---------------------------------------------------------------------------
;;; donkey-yank
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-yank-no-region-calls-clipboard-yank ()
  "Without an active region, calls clipboard-yank directly."
  (let (yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq yanked t))))
        (donkey-yank)))
    (should yanked)))

(ert-deftest donkey-yank-no-region-skips-delete-active-region ()
  "Without an active region, delete-active-region is not called."
  (let (deleted)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'clipboard-yank)
                 (lambda () nil)))
        (donkey-yank)))
    (should-not deleted)))

(ert-deftest donkey-yank-region-deletes-then-yanks ()
  "With an active region, calls delete-active-region then clipboard-yank,
in that order."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (push 'delete order)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (push 'yank order))))
        (donkey-yank)))
    (should (eq (nth 0 order) 'yank))
    (should (eq (nth 1 order) 'delete))
    (should (= (length order) 2))))

(ert-deftest donkey-yank-no-region-inserts-clipboard-content ()
  "Without region, clipboard-yank inserts clipboard text at point."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "world"))))
      (donkey-yank))
    (should (string= (buffer-substring 1 6) "world"))))

(ert-deftest donkey-yank-region-replaces-with-clipboard-content ()
  "With region, deletes region then yanks clipboard content."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'delete-active-region)
               (lambda () (delete-region 1 6)))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "hey"))))
      (donkey-yank))
    (should (string= (buffer-substring 1 4) "hey"))))

(ert-deftest donkey-yank-empty-buffer-no-region ()
  "Empty buffer, no region: clipboard-yank inserts at point-min."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "text"))))
      (donkey-yank))
    (should (= (buffer-size) 4))
    (should (string= (buffer-string) "text"))))

(ert-deftest donkey-yank-region-covers-entire-buffer ()
  "Region covers entire buffer: cleared then replaced."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char (point-max))
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'delete-active-region)
               (lambda () (delete-region 1 7)))
              ((symbol-function 'clipboard-yank)
               (lambda () (insert "world\n"))))
      (donkey-yank))
    (should (string= (buffer-string) "world\n"))))

(ert-deftest donkey-yank-call-interactively-with-region ()
  "Can be called interactively with a region."
  (let (deleted yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq yanked t))))
        (call-interactively #'donkey-yank))
      (should deleted)
      (should yanked))))

(ert-deftest donkey-yank-ignores-prefix-arg ()
  "clipboard-yank is called regardless of prefix arg."
  (let (yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((current-prefix-arg '(4)))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () nil))
                  ((symbol-function 'clipboard-yank)
                   (lambda () (setq yanked t))))
          (call-interactively #'donkey-yank)))
      (should yanked))))

(ert-deftest donkey-yank-rectangle-mode-falls-through-to-undefined ()
  "Regression test: with rectangle-mark-mode active, must not delete the
region and then paste linearly.  `donkey--delete-active-region-safe'
correctly deletes the whole rectangle (via `region-extract-function',
which rect.el advises to respect `rectangle-mark-mode'), but that
deletion deactivates the mark, which auto-disables
`rectangle-mark-mode' via its own hook -- so a plain linear yank
immediately after would land on only one row, silently leaving every
other row of the just-deleted rectangle with nothing to replace it.
Must call `undefined' instead, same as `donkey-wrap-region' does."
  (let (called-cmd deleted yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd)))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq yanked t))))
        (let ((rectangle-mark-mode t))
          (donkey-yank))))
    (should (eq called-cmd 'undefined))
    (should-not deleted)
    (should-not yanked)))

(ert-deftest donkey-yank-rectangle-mode-falls-back-when-disabled ()
  "When rectangle-mark-mode is nil, yanks normally as before."
  (let (called-cmd deleted yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd)))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq yanked t))))
        (let ((rectangle-mark-mode nil))
          (donkey-yank))))
    (should-not called-cmd)
    (should deleted)
    (should yanked)))

;;; ---------------------------------------------------------------------------
;;; donkey-yank-pop
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-yank-pop-no-region-calls-yank-pop ()
  "Without an active region, calls yank-pop directly."
  (let (popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (donkey-yank-pop)))
    (should popped)))

(ert-deftest donkey-yank-pop-region-deletes-then-pops ()
  "With an active region, calls delete-active-region then yank-pop,
in that order."
  (let (order)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (push 'delete order)))
                ((symbol-function 'yank-pop)
                 (lambda () (push 'pop order))))
        (donkey-yank-pop)))
    (should (eq (nth 0 order) 'pop))
    (should (eq (nth 1 order) 'delete))
    (should (= (length order) 2))))

(ert-deftest donkey-yank-pop-no-region-inserts-content ()
  "Without region, yank-pop inserts content at point."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'yank-pop)
               (lambda () (insert "world"))))
      (donkey-yank-pop))
    (should (string= (buffer-substring 1 6) "world"))))

(ert-deftest donkey-yank-pop-region-replaces-with-content ()
  "With region, deletes region then yank-pops content."
  (with-temp-buffer
    (insert "hello world\n")
    (goto-char 6)
    (push-mark 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () t))
              ((symbol-function 'delete-active-region)
               (lambda () (delete-region 1 6)))
              ((symbol-function 'yank-pop)
               (lambda () (insert "hey"))))
      (donkey-yank-pop))
    (should (string= (buffer-substring 1 4) "hey"))))

(ert-deftest donkey-yank-pop-empty-buffer-no-region ()
  "Empty buffer, no region: yank-pop inserts at point-min."
  (with-temp-buffer
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'yank-pop)
               (lambda () (insert "text"))))
      (donkey-yank-pop))
    (should (= (buffer-size) 4))
    (should (string= (buffer-string) "text"))))

(ert-deftest donkey-yank-pop-call-interactively-with-region ()
  "Can be called interactively with a region."
  (let (deleted popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (call-interactively #'donkey-yank-pop))
      (should deleted)
      (should popped))))

(ert-deftest donkey-yank-pop-ignores-prefix-arg ()
  "yank-pop is called regardless of prefix arg."
  (let (popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (let ((current-prefix-arg '(4)))
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () nil))
                  ((symbol-function 'yank-pop)
                   (lambda () (setq popped t))))
          (call-interactively #'donkey-yank-pop)))
      (should popped))))

(ert-deftest donkey-yank-pop-rectangle-mode-falls-through-to-undefined ()
  "Regression test: same guard as `donkey-yank', for the same reason --
see `donkey-yank-rectangle-mode-falls-through-to-undefined'."
  (let (called-cmd deleted popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd)))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (let ((rectangle-mark-mode t))
          (donkey-yank-pop))))
    (should (eq called-cmd 'undefined))
    (should-not deleted)
    (should-not popped)))

(ert-deftest donkey-yank-pop-rectangle-mode-falls-back-when-disabled ()
  "When rectangle-mark-mode is nil, pops normally as before."
  (let (called-cmd deleted popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd)))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (let ((rectangle-mark-mode nil))
          (donkey-yank-pop))))
    (should-not called-cmd)
    (should deleted)
    (should popped)))

;;; ---------------------------------------------------------------------------
;;; donkey-indent-region-or-line
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-indent-region-or-line-use-region-p-truthy-takes-region-path ()
  "When use-region-p is true, calls indent-region with region bounds."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "line1\nline2\nline3\n")
      (goto-char 1)
      (push-mark 12)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (donkey-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 12))))

(ert-deftest donkey-indent-region-or-line-use-region-p-falsy-takes-line-path ()
  "When use-region-p is false, calls indent-region with line bounds."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (donkey-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 6))))

(ert-deftest donkey-indent-region-or-line-indent-multi-line-region ()
  "Indent a region covering multiple lines."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "one\ntwo\nthree\n")
      (goto-char 1)
      (push-mark 15)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (donkey-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 15))))

(ert-deftest donkey-indent-region-or-line-region-from-middle-of-buffer ()
  "Indent region starting from middle of buffer."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "abc\ndef\nghi\n")
      (goto-char 5)
      (push-mark 8)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () t))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (donkey-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 5))
    (should (= (cadr indented-bounds) 8))))

(ert-deftest donkey-indent-region-or-line-indent-second-line ()
  "Indent second line when no region."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "first\nsecond\n")
      (goto-char 7)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (donkey-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 7))
    (should (= (cadr indented-bounds) 13))))

(ert-deftest donkey-indent-region-or-line-indent-empty-line ()
  "Indent empty line (just newline)."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "\n\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (donkey-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 1))
    (should (= (cadr indented-bounds) 1))))

(ert-deftest donkey-indent-region-or-line-indent-last-line-no-newline ()
  "Indent last line without trailing newline."
  (let ((indented-bounds nil))
    (with-temp-buffer
      (insert "line1\nline2")
      (goto-char 7)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (setq indented-bounds (list beg end)))))
        (donkey-indent-region-or-line)))
    (should indented-bounds)
    (should (= (car indented-bounds) 7))
    (should (= (cadr indented-bounds) 12))))

(ert-deftest donkey-indent-region-or-line-indents-whole-buffer-once ()
  "indent-region is called exactly once."
  (let ((call-count 0))
    (with-temp-buffer
      (insert "test\n")
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end)
                   (cl-incf call-count)
                   (ignore beg end))))
        (donkey-indent-region-or-line)))
    (should (= call-count 1))))

(ert-deftest donkey-indent-region-or-line-preserves-buffer-text ()
  "After indent, buffer text is unchanged (mocked indent-region does nothing)."
  (let ((original-text "original text\n"))
    (with-temp-buffer
      (insert original-text)
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'indent-region)
                 (lambda (beg end) (ignore beg end))))
        (donkey-indent-region-or-line))
      (should (string= (buffer-string) original-text)))))

(ert-deftest donkey-indent-region-or-line-call-interactively ()
  "Can be called interactively via call-interactively."
  (with-temp-buffer
    (insert "test\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p)
               (lambda () nil))
              ((symbol-function 'indent-region)
               (lambda (beg end) (ignore beg end))))
      (call-interactively #'donkey-indent-region-or-line))))

;;; ---------------------------------------------------------------------------
;;; donkey--in-org-src-block-p
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-in-org-src-block-p-returns-t-in-src-block ()
  "In org-mode with a src-block element at point, returns non-nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:language "python" :begin 1 :end 50)))))
    (let ((major-mode 'org-mode))
      (should (donkey--in-org-src-block-p)))))

(ert-deftest donkey-in-org-src-block-p-returns-false-in-paragraph ()
  "In org-mode with a non-src-block element at point, returns nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(paragraph (:begin 1 :end 10)))))
    (let ((major-mode 'org-mode))
      (should-not (donkey--in-org-src-block-p)))))

(ert-deftest donkey-in-org-src-block-p-returns-false-if-org-unbound ()
  "When `org-element-at-point' is not fboundp, returns nil."
  (cl-letf (((symbol-function 'org-element-at-point) nil))
    (let ((major-mode 'org-mode))
      (should-not (donkey--in-org-src-block-p)))))

(ert-deftest donkey-in-org-src-block-p-returns-false-in-non-org-mode ()
  "When not in org-mode, returns nil regardless of org functions."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:language "python" :begin 1 :end 50)))))
    (let ((major-mode 'python-mode))
      (should-not (donkey--in-org-src-block-p)))))

(ert-deftest donkey-in-org-src-block-p-non-list-element ()
  "When `org-element-at-point' returns a non-list, non-nil value, returns
nil instead of signaling wrong-type-argument."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () "not-a-list")))
    (let ((major-mode 'org-mode))
      (should-not (donkey--in-org-src-block-p)))))

;;; ---------------------------------------------------------------------------
;;; donkey-comment-dwim
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-comment-dwim-outside-org-comments-current-line ()
  "Outside org-mode, comments the current line."
  (let (called-bounds)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq called-bounds (list beg end)))))
      (with-temp-buffer
        (insert "hello world\n")
        (goto-char (point-min))
        (let ((major-mode 'python-mode))
          (donkey-comment-dwim))))
    (should called-bounds)
    (should (= (car called-bounds) 1))
    (should (= (cadr called-bounds) 13))))

(ert-deftest donkey-comment-dwim-non-org-with-empty-buffer ()
  "Empty buffer: only one line exists."
  (let (bounds)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (setq bounds (list beg end)))))
      (with-temp-buffer
        (let ((major-mode 'text-mode))
          (donkey-comment-dwim))))
    (should bounds)
    (should (= (car bounds) 1))
    (should (= (cadr bounds) 1))))

(ert-deftest donkey-comment-dwim-outside-org-with-region ()
  "With active region outside org, comments from region start's line
start to region end's line start (or next line if not at bol)."
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
          (donkey-comment-dwim))))
    (should called-bounds)
    (should (= (car called-bounds) 1))
    (should (= (cadr called-bounds) 19))))

(ert-deftest donkey-comment-dwim-outside-org-region-at-bol ()
  "Region ending exactly at bol uses that point as end."
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
          (donkey-comment-dwim))))
    (should called-bounds)
    (should (= (car called-bounds) 1))
    (should (= (cadr called-bounds) 10))))

(ert-deftest donkey-comment-dwim-outside-org-deactivates-mark ()
  "After non-org comment operation, mark is always deactivated."
  (let (deactivated)
    (cl-letf (((symbol-function 'comment-or-uncomment-region)
               (lambda (beg end) (ignore beg end)))
              ((symbol-function 'deactivate-mark)
               (lambda () (setq deactivated t))))
      (with-temp-buffer
        (insert "hello\n")
        (goto-char (point-min))
        (let ((major-mode 'text-mode))
          (donkey-comment-dwim))))
    (should deactivated)))

(ert-deftest donkey-comment-dwim-in-org-src-block-delegates-to-org-edit ()
  "Inside an org src-block, delegates to org-edit-special first."
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
          (donkey-comment-dwim))))
    (should (memq 'org-exit calls))
    (should (memq 'org-edit-special calls))
    (should (seq-find (lambda (x) (and (consp x) (eq (car x) 'comment)))
                      calls))))

(ert-deftest donkey-comment-dwim-in-org-src-block-with-region ()
  "Multiple lines in org src-block with active region: both org-edit-special
and comment-or-uncomment-region are called."
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
          (donkey-comment-dwim))))
    (should org-edit-called)
    (should comment-called)))

(ert-deftest donkey-comment-dwim-in-org-src-block-with-region-deactivates-mark ()
  "When org-edit-src-exit succeeds with a region, mark is deactivated."
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
          (donkey-comment-dwim))))
    (should deactivated)))

(ert-deftest donkey-comment-dwim-in-org-src-block-error-handling ()
  "If org-edit-special raises an error, condition-case catches it and
displays a message instead of propagating."
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
              (donkey-comment-dwim)
            (error (setq caught-error-p t))))))
    (should messages)
    (should (string-match-p "donkey-comment-dwim (org-src): mock error"
                            (car messages)))
    (should-not caught-error-p)))

(ert-deftest donkey-comment-dwim-in-org-src-block-exits-edit-buffer-on-comment-error ()
  "Regression test: when `comment-or-uncomment-region' errors AFTER
`org-edit-special' already succeeded (e.g. the src block's language,
such as `fundamental-mode', has no comment syntax defined),
`org-edit-src-exit' still runs -- returning to the Org buffer instead
of stranding the user in the temporary edit buffer/window.

Confirmed live in `emacs -nw': pressing the comment-dwim key on a
`#+begin_src fundamental' block opened the `*Org Src ...*' edit
buffer/window, `comment-or-uncomment-region' signalled \"No comment
syntax is defined\", and without this fix the edit buffer/window was
left open rather than being cleaned up by `org-edit-src-exit'."
  (let (org-edit-called org-exit-called)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "fundamental" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) (setq org-edit-called t)))
              ((symbol-function 'comment-or-uncomment-region)
               (lambda (&rest _) (error "No comment syntax is defined")))
              ((symbol-function 'org-edit-src-exit)
               (lambda () (interactive) (setq org-exit-called t))))
      (with-temp-buffer
        (insert "some text\n")
        (goto-char (point-min))
        (let ((major-mode 'org-mode))
          (donkey-comment-dwim))))
    (should org-edit-called)
    (should org-exit-called)))

(ert-deftest donkey-comment-dwim-org-src-takes-priority ()
  "When in org-mode on a src-block, the org delegation path is taken."
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
          (donkey-comment-dwim))))
    (should (memq 'org-edit call-order))
    (should (memq 'org-exit call-order))))

(provide 'donkey-editing-test)

;;; donkey-editing-test.el ends here

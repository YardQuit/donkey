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

(ert-deftest donkey-copy-no-region-at-end-of-buffer-copies-nothing ()
  "Regression test: at `point-max' with no region there is no character
to copy, so the `kill-ring' must be left alone entirely.

Copying the empty range there instead (the original behavior) silently
pushed an empty string, displacing whatever was previously copied as
the entry a following \"p\" pastes -- so one stray \"y\" past the last
character made the next paste insert nothing, with no error to explain
it.  Confirmed live in `emacs -nw': with \"IMPORTANT\" freshly copied,
pressing \"y\" at `point-max' left the newest kill-ring entry as \"\".

Must still not signal: erroring on a stray copy would be its own
annoyance, so this reports via `message' instead."
  (let (kill-ring-save-called)
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'kill-ring-save)
                 (lambda (&rest _) (setq kill-ring-save-called t))))
        (donkey-copy))
      (should-not kill-ring-save-called))))

(ert-deftest donkey-copy-no-region-at-end-of-buffer-preserves-kill-ring ()
  "End to end: a previously copied entry survives a stray `y' at
`point-max', so a following paste still yields that entry."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil))
    (kill-new "IMPORTANT")
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-max))
      (donkey-copy)
      (should (equal (car kill-ring) "IMPORTANT"))
      (should (= (length kill-ring) 1)))))

(ert-deftest donkey-copy-no-region-before-end-still-copies-char ()
  "The end-of-buffer guard must not suppress the ordinary case: with a
character present at point, `y' still copies exactly that character."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil))
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-min))
      (donkey-copy)
      (should (equal (car kill-ring) "h")))))

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
  (let (called-cmd donkey--last-kill-rectangle-p)
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
  (let (called-cmd donkey--last-kill-rectangle-p)
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
;;; donkey--last-kill-rectangle-p / kill-new / kill-append advice
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-clear-last-kill-rectangle-flag-sets-nil ()
  "Directly clears the flag regardless of prior value."
  (let ((donkey--last-kill-rectangle-p t))
    (donkey--clear-last-kill-rectangle-flag)
    (should-not donkey--last-kill-rectangle-p)))

(ert-deftest donkey-kill-new-advice-clears-last-kill-rectangle-flag ()
  "Regression test: `kill-new' -- the function ANY kill-ring push funnels
through, including ones with no Donkey wrapper at all (e.g. `kill-line'
bound directly to \"D\", or any stock kill command reached via Insert
state's passthrough) -- must clear a stale rectangle-freshness flag, so
a later `donkey-yank' doesn't paste an old rectangle copy instead of
the more recent, ordinary kill."
  (let ((donkey--last-kill-rectangle-p t)
        (kill-ring nil)
        (kill-ring-yank-pointer nil))
    (kill-new "some text")
    (should-not donkey--last-kill-rectangle-p)))

(ert-deftest donkey-kill-append-advice-clears-last-kill-rectangle-flag ()
  "Same as `donkey-kill-new-advice-clears-last-kill-rectangle-flag', for
`kill-append' (used when consecutive kill commands append to the same
kill-ring entry instead of pushing a new one)."
  (let ((donkey--last-kill-rectangle-p t)
        (kill-ring '("existing"))
        (kill-ring-yank-pointer nil))
    (setq kill-ring-yank-pointer kill-ring)
    (kill-append " more" nil)
    (should-not donkey--last-kill-rectangle-p)))

(ert-deftest donkey-set-last-kill-rectangle-flag-sets-t ()
  "Directly sets the flag regardless of prior value."
  (let (donkey--last-kill-rectangle-p)
    (donkey--set-last-kill-rectangle-flag)
    (should donkey--last-kill-rectangle-p)))

(ert-deftest donkey-copy-rectangle-as-kill-advice-sets-flag-directly ()
  "Regression test: calling `copy-rectangle-as-kill' any way OTHER than
through `donkey-copy' -- directly via `M-x', or from any third-party
code -- must still set `donkey--last-kill-rectangle-p', or
`donkey-yank' would treat the freshly populated `killed-rectangle' as
stale and, outside `rectangle-mark-mode', crash into
`donkey--clipboard-yank's kill-ring/clipboard fallback with a raw
\"Kill ring is empty\" error, since a rectangle copy never touches the
kill ring itself."
  (let (donkey--last-kill-rectangle-p)
    (with-temp-buffer
      (insert "hello\n")
      (copy-rectangle-as-kill 1 3))
    (should donkey--last-kill-rectangle-p)))

(ert-deftest donkey-kill-rectangle-advice-sets-flag-directly ()
  "Same as `donkey-copy-rectangle-as-kill-advice-sets-flag-directly', for
`kill-rectangle'."
  (let (donkey--last-kill-rectangle-p)
    (with-temp-buffer
      (insert "hello\n")
      (kill-rectangle 1 3))
    (should donkey--last-kill-rectangle-p)))

;;; ---------------------------------------------------------------------------
;;; donkey--replace-rectangle-selection-with-killed-rectangle
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-rectangle-top-left-returns-first-row-start ()
  "Returns the buffer position of the rectangle's top-left corner,
regardless of which diagonal corners point/mark actually sit at."
  (with-temp-buffer
    (insert "aaXXbb\nccXXdd\neeXXff\n")
    (goto-char (point-min))
    (forward-char 2)
    (let ((start (point)))
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 4)
      (should (= (donkey--rectangle-top-left start (point)) start)))))

(ert-deftest donkey-replace-rectangle-selection-replaces-matching-rows ()
  "Integration test: replaces a same-row-count rectangle selection with
`killed-rectangle', using `delete-rectangle' (not `kill-rectangle') so
the source content survives the destination's own deletion."
  (with-temp-buffer
    (insert ";;aaa\n;;bbb\n;;ccc\n,,ddd\n,,eee\n,,fff\n")
    (goto-char (point-min))
    (forward-line 3)
    (rectangle-mark-mode 1)
    (forward-line 2)
    (forward-char 2)
    (let ((killed-rectangle '(";;" ";;" ";;")))
      (donkey--replace-rectangle-selection-with-killed-rectangle)
      (should (string= (buffer-string)
                       ";;aaa\n;;bbb\n;;ccc\n;;ddd\n;;eee\n;;fff\n"))
      ;; Source rectangle must survive the destination's own deletion.
      (should (equal killed-rectangle '(";;" ";;" ";;"))))))

(ert-deftest donkey-replace-rectangle-selection-refuses-row-count-mismatch ()
  "Refuses via `user-error', without touching the buffer at all, when
the selection's row count doesn't match `killed-rectangle's row count."
  (with-temp-buffer
    (insert ";;aaa\n;;bbb\n;;ccc\n,,ddd\n,,eee\n,,fff\n")
    (goto-char (point-min))
    (forward-line 3)
    (rectangle-mark-mode 1)
    (forward-line 1)                   ; only 2 rows selected, not 3
    (forward-char 2)
    (let ((killed-rectangle '(";;" ";;" ";;")) ; 3 rows
          (original (buffer-string)))
      (should-error (donkey--replace-rectangle-selection-with-killed-rectangle)
                    :type 'user-error)
      (should (string= (buffer-string) original)))))

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
Must call `undefined' instead, same as `donkey-wrap-region' does.

Explicitly binds `donkey--last-kill-rectangle-p' to nil: with it set,
`donkey-yank' instead delegates to
`donkey--replace-rectangle-selection-with-killed-rectangle' -- see the
dedicated tests for that path."
  (let (called-cmd deleted yanked donkey--last-kill-rectangle-p)
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
  (let (called-cmd deleted yanked donkey--last-kill-rectangle-p)
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

(ert-deftest donkey-yank-rectangle-mode-with-flag-calls-replace-function ()
  "With rectangle-mark-mode active and the flag set, must delegate to
`donkey--replace-rectangle-selection-with-killed-rectangle' instead of
falling through to `undefined'."
  (let ((donkey--last-kill-rectangle-p t)
        replace-called called-cmd)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (cl-letf (((symbol-function
                  'donkey--replace-rectangle-selection-with-killed-rectangle)
                 (lambda () (setq replace-called t)))
                ((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd))))
        (let ((rectangle-mark-mode t))
          (donkey-yank))))
    (should replace-called)
    (should-not called-cmd)))

(ert-deftest donkey-yank-pastes-rectangle-when-flag-set ()
  "Regression test: after `donkey-copy'/`donkey-delete' kill a rectangle
elsewhere (rectangle-mark-mode no longer active here), `donkey-yank'
must paste it back via `yank-rectangle' instead of pulling from the
clipboard/kill ring -- otherwise there is no way to paste a
rectangle-copied selection anywhere outside of `rectangle-mark-mode'
itself."
  (let ((donkey--last-kill-rectangle-p t)
        rectangle-yanked clipboard-yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'yank-rectangle)
                 (lambda () (setq rectangle-yanked t)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq clipboard-yanked t))))
        (let (rectangle-mark-mode)
          (donkey-yank))))
    (should rectangle-yanked)
    (should-not clipboard-yanked)))

(ert-deftest donkey-yank-rectangle-flag-deletes-active-region-first ()
  "With an active (non-rectangle) region and the flag set, deletes the
region before pasting the rectangle, same as the ordinary yank path."
  (let ((donkey--last-kill-rectangle-p t)
        deleted rectangle-yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 4)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'delete-active-region)
                 (lambda () (setq deleted t)))
                ((symbol-function 'yank-rectangle)
                 (lambda () (setq rectangle-yanked t))))
        (let (rectangle-mark-mode)
          (donkey-yank))))
    (should deleted)
    (should rectangle-yanked)))

(ert-deftest donkey-yank-does-not-paste-rectangle-when-flag-nil ()
  "With the flag nil (no rectangle kill since the last ordinary kill),
yanks normally via clipboard-yank, not yank-rectangle."
  (let (donkey--last-kill-rectangle-p
        rectangle-yanked clipboard-yanked)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'yank-rectangle)
                 (lambda () (setq rectangle-yanked t)))
                ((symbol-function 'clipboard-yank)
                 (lambda () (setq clipboard-yanked t))))
        (let (rectangle-mark-mode)
          (donkey-yank))))
    (should-not rectangle-yanked)
    (should clipboard-yanked)))

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

(ert-deftest donkey-yank-pop-signals-error-right-after-rectangle-paste ()
  "Regression test: `yank-rectangle' (unlike `yank'/`clipboard-yank')
never sets `this-command' to `yank', so calling `donkey-yank-pop'
immediately after a rectangle paste must not delegate to `yank-pop' --
which would otherwise fail confusingly deep inside its own
`yank-from-kill-ring'/`read-from-kill-ring' fallback path.  Signals a
clear `user-error' instead, since `killed-rectangle' has no history to
pop through regardless."
  (let ((donkey--last-kill-rectangle-p t)
        (last-command 'donkey-yank)
        popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (should-error (donkey-yank-pop) :type 'user-error)))
    (should-not popped)))

(ert-deftest donkey-yank-pop-pops-normally-after-non-rectangle-yank ()
  "Even with last-command `donkey-yank', a nil flag (the previous
donkey-yank was an ordinary clipboard/kill-ring paste) must still pop
normally."
  (let (donkey--last-kill-rectangle-p
        (last-command 'donkey-yank)
        popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (donkey-yank-pop)))
    (should popped)))

(ert-deftest donkey-yank-pop-pops-normally-when-last-command-is-not-donkey-yank ()
  "Even with the flag set, if the immediately preceding command wasn't
donkey-yank (e.g. the rectangle copy happened long ago and other
commands ran since), must still fall through to plain yank-pop rather
than signalling the rectangle-specific error."
  (let ((donkey--last-kill-rectangle-p t)
        (last-command 'some-other-command)
        popped)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'yank-pop)
                 (lambda () (setq popped t))))
        (donkey-yank-pop)))
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

;;; ---------------------------------------------------------------------------
;;; donkey-comment-dwim: REAL org-src round trips (no mocking)
;;; ---------------------------------------------------------------------------
;;;
;;; Every other org-src test above fully mocks `org-edit-special'/
;;; `org-edit-src-exit'/`comment-or-uncomment-region', so none of them
;;; exercise the actual org-buffer-line -> edit-buffer-line arithmetic that
;;; `donkey-comment-dwim' performs between those calls.  These drive the real
;;; Org machinery instead, which is how the region-clamping bug below was
;;; found (live in `emacs -nw', then reproduced here).

(ert-deftest donkey-comment-dwim-real-org-src-region-inside-block ()
  "A region wholly inside a src block comments exactly its own lines."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (org-mode)
      (insert "Intro.\n\n#+begin_src python\nline_a\nline_b\nline_c\n#+end_src\n")
      (goto-char (point-min))
      (search-forward "line_a")
      (goto-char (line-beginning-position))
      (push-mark (point) t t)
      (search-forward "line_b")
      (donkey-comment-dwim)
      (should (string= (buffer-string)
                       (concat "Intro.\n\n#+begin_src python\n"
                               "  # line_a\n  # line_b\n  line_c\n"
                               "#+end_src\n"))))))

(ert-deftest donkey-comment-dwim-real-org-src-region-overflows-block-start ()
  "Regression test: a region starting OUTSIDE the src block (in ordinary
Org prose above it) and ending inside must comment only the in-block
part of the selection.

`donkey-comment-dwim' maps org-buffer line numbers into the temporary
`org-edit-special' buffer by a fixed offset, but the region can extend
past either end of the block, putting the mapped start line outside the
edit buffer entirely.  `forward-line' silently clamps such
out-of-range motions -- but only AFTER the range's width was already
computed from the unclamped numbers, so the whole range slid downward
and commented the wrong lines.  Confirmed live in `emacs -nw':
selecting from \"Intro.\" down through `line_b' commented all THREE
block lines, including `line_c' which was never in the selection, while
leaving `line_a'..`line_b' correct only by coincidence of the shift."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (org-mode)
      (insert "Intro.\n\n#+begin_src python\nline_a\nline_b\nline_c\n#+end_src\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (search-forward "line_b")
      (donkey-comment-dwim)
      (should (string= (buffer-string)
                       (concat "Intro.\n\n#+begin_src python\n"
                               "  # line_a\n  # line_b\n  line_c\n"
                               "#+end_src\n"))))))

(ert-deftest donkey-comment-dwim-real-org-src-region-overflows-block-end ()
  "Mirror of the previous test: a region running PAST the block's end
clamps at its last line instead of commenting outside it.

Point (not mark) must be the in-block end of the region here: the
org-src path is chosen by `donkey--in-org-src-block-p', which tests
point, so a region whose point end sits outside the block takes the
ordinary non-org branch instead and is not this code path at all."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (org-mode)
      (insert "#+begin_src python\nline_a\nline_b\nline_c\n#+end_src\n\nOutro.\n")
      (let ((line-b-start (progn (goto-char (point-min))
                                 (search-forward "line_b")
                                 (line-beginning-position))))
        (goto-char (point-max))
        (push-mark (point) t t)
        (goto-char line-b-start))
      (donkey-comment-dwim)
      (should (string= (buffer-string)
                       (concat "#+begin_src python\n"
                               "  line_a\n  # line_b\n  # line_c\n"
                               "#+end_src\n\nOutro.\n"))))))

(ert-deftest donkey-comment-dwim-real-org-src-no-region-single-line ()
  "With no region, only the line point is on gets commented, and the
surrounding block lines are untouched."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src python\nline_a\nline_b\nline_c\n#+end_src\n")
    (goto-char (point-min))
    (search-forward "line_b")
    (donkey-comment-dwim)
    (should (string= (buffer-string)
                     (concat "#+begin_src python\n"
                             "  line_a\n  # line_b\n  line_c\n"
                             "#+end_src\n")))))


;;; ---------------------------------------------------------------------------
;;; Banked line selection (donkey-bank-selection)
;;; ---------------------------------------------------------------------------

(defun donkey--test-lines-buffer (n)
  "Insert N lines named r0..rN-1 and return to `point-min'."
  (dotimes (i n) (insert (format "r%d\n" i)))
  (goto-char (point-min)))

(ert-deftest donkey-bank-selection-single-line-no-region ()
  "With no region, banks the current line only."
  (with-temp-buffer
    (donkey--test-lines-buffer 4)
    (forward-line 1)
    (donkey-bank-selection)
    (should (equal (donkey--banked-spans)
                   (list (cons (save-excursion (forward-line 0) (point))
                               (save-excursion (forward-line 1) (point))))))))

(ert-deftest donkey-bank-selection-does-not-outlive-the-buffer-being-refilled ()
  "Regression test: a bank must not survive the text it banked.

Emptying a buffer collapses an overlay to zero width rather than
removing it, and banked overlays advance with text inserted at their
end -- so refilling the buffer regrew the overlay over whatever
replaced the banked line.  Banking one line of three and then
refilling reported the entire new buffer as banked, while
`donkey--banked-line-count' still said one line, so `y'/`d' acted on
text that was never picked out.

`donkey--prune-banked-overlays' cannot catch this: the insertion
re-expands the overlay before the spans are next asked for, so it never
looks collapsed.  The overlays evaporate instead."
  (with-temp-buffer
    (donkey--test-lines-buffer 3)
    (goto-char (point-min))
    (donkey-bank-selection)
    (should (= 1 (length (donkey--banked-spans))))
    ;; Anything that empties and refills the buffer.
    (erase-buffer)
    (insert "one\ntwo\nthree\nfour\n")
    (should (null (donkey--banked-spans)))
    (should (zerop (donkey--banked-line-count)))
    (should-not (donkey--banked-selection-p))))

(ert-deftest donkey-bank-selection-does-not-outlive-its-line ()
  "A banked line deleted by ordinary editing takes its bank with it."
  (with-temp-buffer
    (donkey--test-lines-buffer 3)
    (goto-char (point-min))
    (donkey-bank-selection)
    (should (= 1 (length (donkey--banked-spans))))
    (delete-region (point-min) (save-excursion (forward-line 1) (point)))
    (should (null (donkey--banked-spans)))))

(ert-deftest donkey-bank-selection-toggles-off-on-same-line ()
  "Regression test: banking twice on one line unbanks just that line,
leaving any adjacent banked line alone.

Overlays are stored one per line for exactly this reason.  When
adjacent lines were absorbed into a single overlay, unbanking any line
of the run dropped the whole run -- confirmed live in `emacs -nw':
banking two adjacent lines then pressing the bank key again on the
second reported \"Unbanked this line (0 total)\"."
  (with-temp-buffer
    (donkey--test-lines-buffer 4)
    (donkey-bank-selection)                 ; r0
    (forward-line 1)
    (donkey-bank-selection)                 ; r1, adjacent to r0
    (should (= 2 (donkey--banked-line-count)))
    (donkey-bank-selection)                 ; toggle r1 back off
    (should (= 1 (donkey--banked-line-count)))))

(ert-deftest donkey-bank-selection-banks-whole-lines-of-region ()
  "A partial region banks every whole line it touches."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (donkey--test-lines-buffer 4)
      (forward-char 1)                      ; mid-way into r0
      (push-mark (point) t t)
      (forward-line 1)
      (forward-char 1)                      ; mid-way into r1
      (donkey-bank-selection)
      (should (= 2 (donkey--banked-line-count)))
      ;; Banking releases the mark so navigation can continue.
      (should-not (use-region-p)))))

(ert-deftest donkey-copy-banked-lines-concatenates-in-buffer-order ()
  "`donkey-copy' copies every banked line as one kill, in buffer order,
skipping the lines between them."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (with-temp-buffer
      (donkey--test-lines-buffer 6)
      (donkey-bank-selection)               ; r0
      (forward-line 2)
      (donkey-bank-selection)               ; r2
      (forward-line 2)
      (donkey-bank-selection)               ; r4
      (donkey-copy)
      (should (equal (car kill-ring) "r0\nr2\nr4\n"))
      ;; Consumed once used.
      (should-not (donkey--banked-selection-p)))))

(ert-deftest donkey-copy-banked-includes-live-region ()
  "The region active at the time counts too, without banking it first."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil)
        (transient-mark-mode t))
    (with-temp-buffer
      (donkey--test-lines-buffer 5)
      (donkey-bank-selection)               ; r0
      (forward-line 3)                      ; r3
      (push-mark (point) t t)
      (goto-char (line-end-position))
      (donkey-copy)
      (should (equal (car kill-ring) "r0\nr3\n")))))

(ert-deftest donkey-delete-banked-lines-removes-only-those-lines ()
  "`donkey-delete' removes exactly the banked lines, leaving the rest."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (with-temp-buffer
      (donkey--test-lines-buffer 6)
      (forward-line 1)
      (donkey-bank-selection)               ; r1
      (forward-line 2)
      (donkey-bank-selection)               ; r3
      (donkey-delete)
      (should (equal (buffer-string) "r0\nr2\nr4\nr5\n"))
      (should (equal (car kill-ring) "r1\nr3\n"))
      (should-not (donkey--banked-selection-p)))))

(ert-deftest donkey-banked-adjacent-lines-merge-as-one-piece ()
  "Adjacent banked lines are copied as one contiguous run, not split.

They are stored as separate per-line overlays, so this checks that
`donkey--effective-line-spans' merges them back at use time."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (with-temp-buffer
      (donkey--test-lines-buffer 5)
      (donkey-bank-selection)               ; r0
      (forward-line 1)
      (donkey-bank-selection)               ; r1 (adjacent)
      (should (= 1 (length (donkey--effective-line-spans))))
      (donkey-copy)
      (should (equal (car kill-ring) "r0\nr1\n")))))

(ert-deftest donkey-copy-without-banked-lines-is-unchanged ()
  "With nothing banked, `donkey-copy' keeps its original character/region
behavior -- the banked path must not hijack the ordinary case."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-min))
      (donkey-copy)
      (should (equal (car kill-ring) "h")))))

(ert-deftest donkey-clear-banked-selection-discards-everything ()
  "`donkey-clear-banked-selection' drops all banks without touching text."
  (with-temp-buffer
    (donkey--test-lines-buffer 4)
    (donkey-bank-selection)
    (forward-line 2)
    (donkey-bank-selection)
    (should (= 2 (donkey--banked-line-count)))
    (donkey-clear-banked-selection)
    (should-not (donkey--banked-selection-p))
    (should (equal (buffer-string) "r0\nr1\nr2\nr3\n"))))

(ert-deftest donkey-bank-selection-last-line-without-newline ()
  "A final line with no trailing newline banks and deletes cleanly."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (with-temp-buffer
      (insert "r0\nr1\nlast-no-newline")
      (goto-char (point-max))
      (donkey-bank-selection)
      (donkey-delete)
      (should (equal (buffer-string) "r0\nr1\n")))))

(ert-deftest donkey-banked-copy-counts-blank-lines ()
  "A banked blank line counts toward the reported line total.

Regression: the count came from splitting the copied text on newlines
with omit-nulls, so a blank line vanished from the tally -- banking
three lines where the middle one was empty reported \"Copied 2 lines\"."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil) shown)
    (with-temp-buffer
      (insert "a\n\nb\n")
      (goto-char (point-min))
      (donkey-bank-selection)
      (forward-line 1)
      (donkey-bank-selection)
      (forward-line 1)
      (donkey-bank-selection)
      (should (= 3 (donkey--banked-line-count)))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-copy))
      (should (equal shown "Copied 3 lines"))
      (should (equal (current-kill 0) "a\n\nb\n")))))

(ert-deftest donkey-banked-delete-counts-blank-lines ()
  "Deleting banked lines reports blank lines in the total too."
  (let ((kill-ring nil) (kill-ring-yank-pointer nil) shown)
    (with-temp-buffer
      (insert "a\n\nb\n")
      (goto-char (point-min))
      (donkey-bank-selection)
      (forward-line 1)
      (donkey-bank-selection)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-delete))
      (should (equal shown "Deleted 2 lines"))
      (should (equal (buffer-string) "b\n")))))

(ert-deftest donkey-clear-banked-selection-is-silent-when-non-interactive ()
  "Programmatic clearing reports nothing; only the key press does."
  (let (shown)
    (with-temp-buffer
      (donkey--test-lines-buffer 3)
      (donkey-bank-selection)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-clear-banked-selection))
      (should-not shown))))

(ert-deftest donkey-clear-banked-selection-reports-when-called-interactively ()
  "Pressing the clear key reports how many banked lines were discarded."
  (let (shown)
    (with-temp-buffer
      (donkey--test-lines-buffer 4)
      (donkey-bank-selection)
      (forward-line 2)
      (donkey-bank-selection)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (call-interactively #'donkey-clear-banked-selection))
      (should (equal shown "Discarded 2 banked lines"))
      (should-not (donkey--banked-selection-p)))))

(ert-deftest donkey-banked-overlay-collapsed-by-editing-is-pruned ()
  "Deleting a banked line's text drops the bank instead of leaving a ghost.

Regression: the overlay collapsed to zero width, which highlights
nothing, but still counted as a live bank."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (forward-line 1)
    (donkey-bank-selection)
    (should (donkey--banked-selection-p))
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (line-beginning-position) (line-beginning-position 2))
    (should-not (donkey--banked-selection-p))
    (should (= 0 (donkey--banked-line-count)))
    (should-not donkey--banked-overlays)))

(ert-deftest donkey-copy-after-collapsed-bank-does-not-clobber-kill-ring ()
  "A ghost bank must not turn `y' into a silent empty kill.

Regression: `donkey--banked-selection-p' stayed true for a zero-width
overlay, so `donkey-copy' took the banked branch, reported \"Copied 0
lines\" and pushed \"\" over whatever was previously copied -- the same
empty-kill failure already guarded against at `point-max'."
  (let ((kill-ring (list "IMPORTANT")) kill-ring-yank-pointer)
    (with-temp-buffer
      (insert "one\ntwo\nthree\n")
      (goto-char (point-min))
      (forward-line 1)
      (donkey-bank-selection)
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (line-beginning-position) (line-beginning-position 2))
      (goto-char (point-min))
      (donkey-copy)
      ;; Falls through to the character at point, as with no bank at all.
      (should (equal (current-kill 0) "o")))))

(ert-deftest donkey-collapsed-bank-does-not-block-banking-that-line-again ()
  "A ghost bank must not make its line permanently un-bankable.

Regression: `donkey--banked-overlay-at' requires POS strictly inside
the overlay, which no position ever is for an empty range -- so the
ghost could neither be toggled off nor banked over."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (forward-line 1)
    (donkey-bank-selection)
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (line-beginning-position) (line-beginning-position 2))
    (goto-char (point-min))
    (donkey-bank-selection)
    (should (= 1 (donkey--banked-line-count)))
    (should (equal (donkey--banked-spans) (list (cons 1 5))))))

(ert-deftest donkey-mode-disable-clears-banked-lines ()
  "Turning off variable `donkey-mode' clears banked highlights.

Regression: they survived, and the only command that removes them is
reachable solely through a Normal-state key that no longer exists once
the mode is off -- so the highlights were permanent."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (donkey-mode 1)
    (unwind-protect
        (progn
          (goto-char (point-min))
          (donkey-bank-selection)
          (should (= 1 (donkey--banked-line-count)))
          (donkey-mode -1)
          (should (= 0 (donkey--banked-line-count)))
          (should-not donkey--banked-overlays))
      (when (bound-and-true-p donkey-mode)
        (donkey-mode -1)))))

(ert-deftest donkey-clear-banked-selection-bound-to-backspace-and-delete ()
  "Both Backspace and Delete clear the bank, under the `m' prefix.

`DEL' is Emacs's name for ASCII 127, which is what BACKSPACE sends; the
physical Delete key is a different key that arrives as `<deletechar>'
in a terminal or `<delete>' on a graphical frame.  Regression: only
`m DEL' was bound, so pressing `m' and the Delete key reported
\"m <deletechar> is undefined\" -- confirmed live before binding it."
  (dolist (key '("m DEL" "m <deletechar>" "m <delete>"))
    (should (eq (keymap-lookup donkey-normal-mode-map key)
                #'donkey-clear-banked-selection))))

(ert-deftest donkey-bank-selection-region-over-banked-block-unbanks-it ()
  "Re-selecting an already-banked block unbanks the whole block.

Previously a region press could only ever ADD: `donkey--bank-span'
skips lines that are already banked, so taking back a multi-line bank
meant toggling each line individually or clearing every bank."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (donkey--test-lines-buffer 5)
      (push-mark (point) t t)
      (forward-line 3)
      (donkey-bank-selection)
      (should (= 3 (donkey--banked-line-count)))
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 3)
      (donkey-bank-selection)
      (should (= 0 (donkey--banked-line-count)))
      (should-not donkey--banked-overlays))))

(ert-deftest donkey-bank-selection-unbanking-a-block-keeps-other-banks ()
  "Unbanking a block leaves banks outside it alone."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (donkey--test-lines-buffer 6)
      (forward-line 5)
      (donkey-bank-selection)
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 3)
      (donkey-bank-selection)
      (should (= 4 (donkey--banked-line-count)))
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 3)
      (donkey-bank-selection)
      (should (= 1 (donkey--banked-line-count)))
      ;; The survivor is the last line, banked separately.
      (should (equal (donkey--banked-spans)
                     (list (cons (save-excursion (goto-char (point-min))
                                                 (forward-line 5)
                                                 (point))
                                 (point-max))))))))

(ert-deftest donkey-bank-selection-region-over-partial-block-completes-it ()
  "A region over a partly-banked block banks the rest instead of clearing.

A block only toggles off once it is uniformly on, matching the
single-line toggle's rule."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (donkey--test-lines-buffer 4)
      (forward-line 1)
      (donkey-bank-selection)
      (should (= 1 (donkey--banked-line-count)))
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 3)
      (donkey-bank-selection)
      (should (= 3 (donkey--banked-line-count))))))

(ert-deftest donkey-bank-selection-region-unbank-reports-unbanked ()
  "Unbanking a block says so, rather than reporting it as banked."
  (let ((transient-mark-mode t) shown)
    (with-temp-buffer
      (donkey--test-lines-buffer 4)
      (push-mark (point) t t)
      (forward-line 2)
      (donkey-bank-selection)
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 2)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-bank-selection))
      (should (equal shown "Unbanked 2 lines (0 total)")))))

(ert-deftest donkey-unbank-line-removes-only-that-line ()
  "`m u' drops the line at point and nothing else."
  (with-temp-buffer
    (donkey--test-lines-buffer 4)
    (dolist (n '(0 1 2))
      (goto-char (point-min))
      (forward-line n)
      (donkey-bank-selection))
    (should (= 3 (donkey--banked-line-count)))
    (goto-char (point-min))
    (forward-line 1)
    (donkey-unbank-line)
    (should (= 2 (donkey--banked-line-count)))))

(ert-deftest donkey-unbank-line-on-unbanked-line-changes-nothing ()
  "`m u' only ever removes; on an unbanked line it reports and stops."
  (let (shown)
    (with-temp-buffer
      (donkey--test-lines-buffer 4)
      (donkey-bank-selection)
      (forward-line 2)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-unbank-line))
      (should (equal shown "No banked line at point"))
      (should (= 1 (donkey--banked-line-count))))))

(ert-deftest donkey-unbank-section-removes-the-whole-run ()
  "`m U' drops every banked line adjacent to point, and only those."
  (with-temp-buffer
    (donkey--test-lines-buffer 6)
    (dolist (n '(0 1 2 5))
      (goto-char (point-min))
      (forward-line n)
      (donkey-bank-selection))
    (should (= 4 (donkey--banked-line-count)))
    (goto-char (point-min))
    (forward-line 2)
    (donkey-unbank-section)
    ;; The isolated bank on the last line survives.
    (should (= 1 (donkey--banked-line-count)))
    (should (equal (donkey--banked-spans)
                   (list (cons (save-excursion (goto-char (point-min))
                                               (forward-line 5)
                                               (point))
                               (point-max)))))))

(ert-deftest donkey-unbank-section-on-unbanked-line-changes-nothing ()
  "`m U' reports rather than guessing at a run point is not standing on."
  (let (shown)
    (with-temp-buffer
      (donkey--test-lines-buffer 5)
      (donkey-bank-selection)
      (forward-line 3)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-unbank-section))
      (should (equal shown "No banked section at point"))
      (should (= 1 (donkey--banked-line-count))))))

(ert-deftest donkey-banked-final-line-without-newline-toggles-at-point-max ()
  "A banked final line with no trailing newline can be unbanked at `point-max'.

Regression: `donkey--banked-overlay-at' tested POS strictly inside the
overlay, and there the overlay ends exactly at point -- so the lookup
found nothing and the bank key re-banked the line instead of toggling
it off."
  (with-temp-buffer
    (insert "a\nb\nlast-no-newline")
    (goto-char (point-max))
    (donkey-bank-selection)
    (should (= 1 (donkey--banked-line-count)))
    (donkey-bank-selection)
    (should (= 0 (donkey--banked-line-count)))))

(ert-deftest donkey-unbank-line-works-at-point-max-without-newline ()
  "`m u' reaches a banked final line that has no trailing newline."
  (with-temp-buffer
    (insert "a\nb\nlast-no-newline")
    (goto-char (point-max))
    (donkey-bank-selection)
    (donkey-unbank-line)
    (should (= 0 (donkey--banked-line-count)))))

(ert-deftest donkey-unbank-keys-are-bound ()
  "`m u' and `m U' reach the two unbank commands."
  (should (eq (keymap-lookup donkey-normal-mode-map "m u")
              #'donkey-unbank-line))
  (should (eq (keymap-lookup donkey-normal-mode-map "m U")
              #'donkey-unbank-section)))

(ert-deftest donkey-bank-selection-bound-to-m-l ()
  "Banking is reached with `m l', and `m SPC' no longer shadows it.

`l' alone stays `forward-char'; `m l' is a separate sequence under the
mark prefix."
  (should (eq (keymap-lookup donkey-normal-mode-map "m l")
              #'donkey-bank-selection))
  (should-not (keymap-lookup donkey-normal-mode-map "m SPC"))
  (should (eq (keymap-lookup donkey-normal-mode-map "l")
              #'forward-char)))

(ert-deftest donkey-docstrings-contain-no-control-characters ()
  "No Donkey docstring contains a stray control character.

Regression: `donkey-insert-mode-map' was written as
\"`\\donkey--exit-insert'\", and `\\d' is the Emacs Lisp string escape
for DEL -- so the backslash silently ate the \"d\" and the docstring
rendered as \"`^?onkey--exit-insert'\", a symbol reference that could
never resolve.

Caught by the byte-compiler only on Emacs 30.1, which added the check;
Emacs 29 compiles it without complaint, so this test is what keeps the
package's declared minimum honest."
  (let (offenders)
    (mapatoms
     (lambda (sym)
       (when (string-prefix-p "donkey" (symbol-name sym))
         (dolist (doc (list (and (fboundp sym) (documentation sym))
                            (get sym 'variable-documentation)))
           (when (stringp doc)
             (dotimes (i (length doc))
               (let ((c (aref doc i)))
                 (when (or (= c 127)
                           (and (< c 32) (/= c ?\n) (/= c ?\t)))
                   (push (format "%s: #x%x at %d" (symbol-name sym) c i)
                         offenders)))))))))
    (should-not offenders)))

(provide 'donkey-editing-test)

;;; donkey-editing-test.el ends here

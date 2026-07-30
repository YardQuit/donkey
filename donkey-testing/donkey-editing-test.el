;;; donkey-editing-test.el --- Tests for DONKEY delete/yank/indent/comment commands -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'donkey)

(defvar rectangle-mark-mode)

;;; ---------------------------------------------------------------------------
;;; donkey-copy
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-copy-no-region-copies-single-char ()
  "Without an active region, copies the character at point.
Uses `kill-ring-save' with an explicit (point . point+1) range."
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
  "Without an ACTIVE region, only the character at point is copied.
Regression test: never the raw mark position.

kill-ring-save's own interactive spec reads region-beginning/
region-end, which use wherever the mark last happened to be
regardless of whether the region is actually active -- a mark left
over from an earlier, unrelated command (e.g. a stale
donkey-mark-inner selection, or any prior `push-mark') would silently
get copied instead of the single character at point.  Confirmed live
in Emacs -nw: with mark left at position 10 and point moved to
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
  "At `point-max' with no region, the `kill-ring' is left alone.
Regression test: there is no character to copy there.

Copying the empty range there instead (the original behavior) silently
pushed an empty string, displacing whatever was previously copied as
the entry a following \"p\" pastes -- so one stray \"y\" past the last
character made the next paste insert nothing, with no error to explain
it.  Confirmed live in `emacs -nw': with \"IMPORTANT\" freshly copied,
pressing \"y\" at `point-max' left the newest `kill-ring' entry as \"\".

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
  "A previously copied entry survives a stray `y' at `point-max'.
End to end: a following paste still yields that entry."
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
  "The end-of-buffer guard must not suppress the ordinary case.
With a character present at point, `y' still copies exactly that
character."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil))
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-min))
      (donkey-copy)
      (should (equal (car kill-ring) "h")))))

(ert-deftest donkey-copy-region-copies-region ()
  "An active linear region is copied whole.
From `region-beginning' to `region-end', rectangles excepted."
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
  "After copying a region, the mark is deactivated.
The selection does not linger once yanked."
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
  "A live rectangle selection is copied as a rectangle.
Delegates to `copy-rectangle-as-kill' via `call-interactively'."
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
  "When `rectangle-mark-mode' is nil, falls back to plain `kill-ring-save'."
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
  "Without an active region, deletes the character at point.

Asserts the effect rather than which primitive does it: deletion goes
through `delete-region' so a count can clamp at `point-max' instead of
signaling, and pinning `delete-char' here only tied the test to the
implementation."
  (with-temp-buffer
    (insert "hello\n")
    (goto-char 1)
    (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
      (donkey-delete))
    (should (equal (buffer-string) "ello\n"))))

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
  "`donkey-delete' does not enter insert mode (unlike `donkey-change')."
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

(ert-deftest donkey-delete-no-region-empty-buffer-reports ()
  "Empty buffer, no region: reports rather than signaling.

Previously asserted the raw `end-of-buffer' signal, which documented the
bug as if it were intended."
  (let (shown)
    (with-temp-buffer
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-delete))
      (should (equal shown "End of buffer -- nothing to delete")))))

(ert-deftest donkey-delete-no-region-at-end-of-buffer-reports ()
  "Point at `point-max', no region: reports and leaves the text alone.

Previously asserted the raw `end-of-buffer' signal, which documented the
bug as if it were intended."
  (let (shown)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-delete))
      (should (equal shown "End of buffer -- nothing to delete"))
      (should (equal (buffer-string) "hello\n")))))

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
  "Region with point before mark kills exactly the region.

Asserts the span rather than the argument ORDER it arrives in.  The
bounds now come from `donkey--visual-line-region-bounds', which reports
them low-to-high, and `kill-region' reads them either way round -- so
pinning the order tied the test to the call shape rather than to what
gets killed."
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
      (should (equal (sort (copy-sequence killed-bounds) #'<) '(1 6))))))

(ert-deftest donkey-delete-region-skips-delete-char ()
  "With an active region, `delete-char' is not called."
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
  "`donkey-delete' with region does not enter insert mode."
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
  "A live rectangle selection is cut as a rectangle.
Delegates to `kill-rectangle' via `call-interactively'."
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
  "When `rectangle-mark-mode' is nil, falls back to `kill-region'."
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
;;; donkey-copy / donkey-delete rectangle handling
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-rectangle-top-left-returns-first-row-start ()
  "Returns the buffer position of the rectangle's top-left corner.
Whichever diagonal corners point and mark actually sit at."
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
  "A same-row-count rectangle selection is replaced by `killed-rectangle'.
Integration test: uses `delete-rectangle' (not `kill-rectangle') so
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
  "A row-count mismatch is refused without touching the buffer.
Signals a `user-error' when the selection and `killed-rectangle' differ
in height."
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
  "Without an active region, calls `clipboard-yank' directly."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
    (let (yanked)
      (with-temp-buffer
        (insert "hello\n")
        (goto-char 1)
        (cl-letf (((symbol-function 'use-region-p)
                   (lambda () nil))
                  ((symbol-function 'clipboard-yank)
                   (lambda () (setq yanked t))))
          (donkey-yank)))
      (should yanked))))

(ert-deftest donkey-yank-no-region-skips-delete-active-region ()
  "Without an active region, the function `delete-active-region' is not called."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
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
      (should-not deleted))))

(ert-deftest donkey-yank-region-deletes-then-yanks ()
  "An active region is removed before the paste lands.
Calls the function `delete-active-region' then `clipboard-yank', in that
order."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
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
      (should (= (length order) 2)))))

(ert-deftest donkey-yank-no-region-inserts-clipboard-content ()
  "Without region, `clipboard-yank' inserts clipboard text at point."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'clipboard-yank)
                 (lambda () (insert "world"))))
        (donkey-yank))
      (should (string= (buffer-substring 1 6) "world")))))

(ert-deftest donkey-yank-region-replaces-with-clipboard-content ()
  "With region, deletes region then yanks clipboard content."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
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
      (should (string= (buffer-substring 1 4) "hey")))))

(ert-deftest donkey-yank-empty-buffer-no-region ()
  "Empty buffer, no region: `clipboard-yank' inserts at point-min."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'use-region-p)
                 (lambda () nil))
                ((symbol-function 'clipboard-yank)
                 (lambda () (insert "text"))))
        (donkey-yank))
      (should (= (buffer-size) 4))
      (should (string= (buffer-string) "text")))))

(ert-deftest donkey-yank-region-covers-entire-buffer ()
  "Region covers entire buffer: cleared then replaced."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
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
      (should (string= (buffer-string) "world\n")))))

(ert-deftest donkey-yank-call-interactively-with-region ()
  "Can be called interactively with a region."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
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
        (should yanked)))))

(ert-deftest donkey-yank-ignores-prefix-arg ()
  "`clipboard-yank' is called regardless of prefix arg."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
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
        (should yanked)))))

(ert-deftest donkey-yank-outside-rectangle-mode-pastes-normally ()
  "Without `rectangle-mark-mode', \"p\" deletes the region and yanks."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
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
      (should yanked))))

(ert-deftest donkey-yank-pastes-linear-text-not-a-rectangle ()
  "`donkey-yank' goes through `clipboard-yank', never `yank-rectangle'.

The two stores are reached by two keys; \"p\" only ever names the kill
ring and the system clipboard."
  ;; `kill-ring' is bound because `donkey-yank' checks there is
  ;; something to paste BEFORE acting: with the ring empty it
  ;; correctly does nothing and never reaches the mock below.  Left
  ;; ambient this passed only when an earlier test had stocked the
  ;; ring, which running the suite shuffled showed it relying on.
  (let ((kill-ring (list "something")))
    (let (
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
      (should clipboard-yanked))))

;;; ---------------------------------------------------------------------------
;;; donkey-indent-region-or-line
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-indent-region-or-line-use-region-p-truthy-takes-region-path ()
  "When `use-region-p' is true, calls `indent-region' with region bounds."
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
  "When `use-region-p' is false, calls `indent-region' with line bounds."
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
  "`indent-region' is called exactly once."
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
  "After indent, buffer text is unchanged (mocked `indent-region' does nothing)."
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
  "Can be called interactively via `call-interactively'."
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
  "In `org-mode' with a src-block element at point, returns non-nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:language "python" :begin 1 :end 50)))))
    (let ((major-mode 'org-mode))
      (should (donkey--in-org-src-block-p)))))

(ert-deftest donkey-in-org-src-block-p-returns-false-in-paragraph ()
  "In `org-mode' with a non-src-block element at point, returns nil."
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
  "When not in `org-mode', returns nil regardless of org functions."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:language "python" :begin 1 :end 50)))))
    (let ((major-mode 'python-mode))
      (should-not (donkey--in-org-src-block-p)))))

(ert-deftest donkey-in-org-src-block-p-non-list-element ()
  "A non-list org element yields nil rather than a wrong-type-argument.

When `org-element-at-point' returns a non-list, non-nil value, returns
nil instead of signaling wrong-type-argument."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () "not-a-list")))
    (let ((major-mode 'org-mode))
      (should-not (donkey--in-org-src-block-p)))))

;;; ---------------------------------------------------------------------------
;;; donkey-comment-dwim
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-comment-dwim-outside-org-comments-current-line ()
  "Outside `org-mode', comments the current line."
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
  "Outside Org, a region is commented on whole-line boundaries.

With active region outside org, comments from region start's line
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
  "A region inside an Org src block routes through the edit buffer.

Multiple lines in org src-block with active region: both org-edit-special
and `comment-or-uncomment-region' are called."
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
  "An error from `org-edit-special' is caught and reported, not propagated.

If org-edit-special raises an error, `condition-case' catches it and
displays a message instead of propagating."
  (let (messages caught-error-p)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-edit-special)
               (lambda () (interactive) (error "Mock error")))
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
    (should (string-match-p "donkey-comment-dwim (org-src): Mock error"
                            (car messages)))
    (should-not caught-error-p)))

(ert-deftest donkey-comment-dwim-in-org-src-block-exits-edit-buffer-on-comment-error ()
  "A commenting error still exits the Org src edit buffer.

Regression test: when `comment-or-uncomment-region' errors AFTER
`org-edit-special' already succeeded (e.g. the src block's language,
such as `fundamental-mode', has no comment syntax defined),
`org-edit-src-exit' still runs -- returning to the Org buffer instead
of stranding the user in the temporary edit buffer/window.

Confirmed live in `emacs -nw': pressing the `comment-dwim' key on a
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
  "When in `org-mode' on a src-block, the org delegation path is taken."
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
  "A region overflowing the src block's start is clamped to the block.

Regression test: a region starting OUTSIDE the src block (in ordinary
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
  "A region overflowing the src block's end is clamped to the block.

Mirror of the previous test: a region running PAST the block's end
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
  "With no region, only the current src-block line is commented.

With no region, only the line point is on gets commented, and the
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

(ert-deftest donkey-banked-spans-is-the-public-name-for-the-same-spans ()
  "`donkey-banked-spans' reports what `donkey--banked-spans' does.
It exists so other packages have a name that is not free to change: csvdt
reads banked lines through it.  Nil rather than an empty list when nothing
is banked, since callers test it for truth."
  (with-temp-buffer
    (donkey--test-lines-buffer 4)
    (should (null (donkey-banked-spans)))
    (forward-line 1)
    (donkey-bank-selection)
    (should (equal (donkey-banked-spans) (donkey--banked-spans)))
    (should (= (length (donkey-banked-spans)) 1))))

(ert-deftest donkey-banked-spans-covers-whole-lines ()
  "Every span runs from a line beginning to the beginning of the line after.
The docstring promises this to callers, and it holds because banking goes
through `donkey--whole-line-span' -- so a partial selection still banks the
whole line, and a caller can use a span without widening it first."
  (with-temp-buffer
    (donkey--test-lines-buffer 4)
    ;; A region covering the middle of line 1 and the middle of line 2.
    (forward-line 1)
    (let ((start (1+ (point))))
      (forward-line 1)
      (push-mark start t t)
      (goto-char (1+ (point)))
      (donkey-bank-selection))
    (dolist (span (donkey-banked-spans))
      (should (equal (car span)
                     (save-excursion (goto-char (car span))
                                     (line-beginning-position))))
      (should (equal (cdr span)
                     (save-excursion (goto-char (cdr span))
                                     (line-beginning-position)))))))

(ert-deftest donkey-banked-spans-does-not-merge-touching-banks ()
  "Two banked blocks that touch arrive as two spans, not one.
The docstring says so, and a caller merging them itself depends on it:
`donkey--effective-line-spans' is the one that merges."
  (with-temp-buffer
    (donkey--test-lines-buffer 4)
    (forward-line 1)
    (donkey-bank-selection)
    (forward-line 1)
    (donkey-bank-selection)
    (let ((spans (donkey-banked-spans)))
      (should (= (length spans) 2))
      ;; Touching: the first ends exactly where the second starts.
      (should (equal (cdr (nth 0 spans)) (car (nth 1 spans))))
      ;; And merging them would give one, which is the caller's business.
      (should (= (length (donkey--merge-spans spans)) 1)))))

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
  "Banking a line twice unbanks only that line.

Regression test: banking twice on one line unbanks just that line,
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
  "Banked lines are copied as one kill, in buffer order.

`donkey-copy' copies every banked line as one kill, in buffer order,
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
  "With nothing banked, `donkey-copy' behaves exactly as before.

With nothing banked, `donkey-copy' keeps its original character/region
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

(ert-deftest donkey-bank-selection-empty-final-line-reports-nothing-banked ()
  "Banking the empty final line says so instead of claiming a bank.

Regression: the span there is zero width -- `line-beginning-position'
and the clamped end both land on `point-max' -- so `donkey--bank-span'
never entered its per-line loop and created no overlay, yet the message
still read \"Banked this line (0 total)\": a claimed success and a count
of zero in the same breath.  Most files end in a newline, so `g e'
lands on exactly this spot."
  (let (shown)
    (with-temp-buffer
      (insert "a\nb\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-bank-selection))
      (should (equal shown "Nothing to bank -- empty final line"))
      (should (= 0 (donkey--banked-line-count)))
      (should-not donkey--banked-overlays))))

(ert-deftest donkey-bank-selection-empty-buffer-reports-nothing-banked ()
  "An empty buffer has no line to bank, and says so."
  (let (shown)
    (with-temp-buffer
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-bank-selection))
      (should (equal shown "Nothing to bank -- empty final line"))
      (should (= 0 (donkey--banked-line-count))))))

(ert-deftest donkey-bank-selection-final-line-without-newline-still-banks ()
  "A final line with no trailing newline is real text and still banks.

Guards the empty-final-line check against over-reaching: that line also
ends at `point-max', but its span is not empty."
  (let (shown)
    (with-temp-buffer
      (insert "a\nlast")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-bank-selection))
      (should (equal shown "Banked this line (1 total) -- navigate, then y/d/p"))
      (should (= 1 (donkey--banked-line-count))))))

(ert-deftest donkey-banked-copy-ignores-spans-outside-narrowing ()
  "Banked lines outside the accessible portion do not crash `y'.

Regression: overlay positions are absolute and unaffected by narrowing,
so a line banked before a `narrow-to-region' still reported its original
positions, and `buffer-substring' signalled a bare `args-out-of-range'.
Confirmed live: bank a line, narrow past it, press \"y\" -> \"Args out of
range: #<buffer *live*>, 1, 6\"."
  (let ((kill-ring nil) kill-ring-yank-pointer)
    (with-temp-buffer
      (insert "n0\nn1\nn2\nn3\nn4\n")
      (goto-char (point-min))
      (donkey-bank-selection)
      (narrow-to-region 4 13)
      (should-not (donkey--banked-selection-p))
      (goto-char (point-min))
      ;; Falls through to the character at point, as with no bank at all.
      (donkey-copy)
      (should (equal (current-kill 0) "n")))))

(ert-deftest donkey-banked-delete-ignores-spans-outside-narrowing ()
  "Banked lines outside the accessible portion do not crash `d'."
  (let ((kill-ring nil) kill-ring-yank-pointer)
    (with-temp-buffer
      (insert "d0\nd1\nd2\nd3\nd4\n")
      (goto-char (point-min))
      (donkey-bank-selection)
      (narrow-to-region 4 13)
      (goto-char (point-min))
      (donkey-delete)
      ;; One character removed from the accessible text, nothing outside.
      (should (equal (buffer-string) "1\nd2\nd3\n"))
      (widen)
      (should (equal (buffer-string) "d0\n1\nd2\nd3\nd4\n")))))

(ert-deftest donkey-banked-spans-return-after-widening ()
  "Narrowing hides banked spans; widening brings them back untouched.

Filtered rather than pruned: narrowing is temporary, so the overlays
must survive it."
  (with-temp-buffer
    (insert "w0\nw1\nw2\nw3\nw4\n")
    (goto-char (point-min))
    (donkey-bank-selection)
    (goto-char (point-min))
    (forward-line 4)
    (donkey-bank-selection)
    (should (= 2 (donkey--banked-line-count)))
    (narrow-to-region 4 13)
    (should (= 0 (donkey--banked-line-count)))
    (widen)
    (should (= 2 (donkey--banked-line-count)))
    (should (equal (donkey--banked-spans) (list (cons 1 4) (cons 13 16))))))

(ert-deftest donkey-delete-at-point-max-reports-instead-of-signaling ()
  "`d'/`x' at `point-max' reports rather than signaling `end-of-buffer'.

Regression: `delete-char' signals a bare `end-of-buffer' there, which
pops the debugger for anyone running with `debug-on-error' on.
`donkey-copy' and `donkey-change' both already guarded this exact
position; `donkey-delete' was missed."
  (let (shown)
    (with-temp-buffer
      (insert "abc\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-delete))
      (should (equal shown "End of buffer -- nothing to delete"))
      (should (equal (buffer-string) "abc\n")))))

(ert-deftest donkey-delete-before-point-max-still-deletes ()
  "The `point-max' guard does not stop an ordinary delete."
  (with-temp-buffer
    (insert "abc\n")
    (goto-char (point-min))
    (donkey-delete)
    (should (equal (buffer-string) "bc\n"))))

(ert-deftest donkey-wrap-region-non-character-event-falls-through ()
  "A non-character invoking event wraps nothing instead of signaling.

The delimiter comes from `last-command-event', so the command only means
anything when that is a character.  Reached from
\\[execute-extended-command] or a function-key binding: the rectangle
path handed a symbol to `string' and signalled `wrong-type-argument'."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "hello world\n")
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char 6)
      (rectangle-mark-mode 1)
      ;; `undefined' rings the bell and returns; it does not signal.
      (let ((last-command-event 'f5))
        (donkey-wrap-region))
      (should (equal (buffer-string) "hello world\n")))))

(ert-deftest donkey-banking-a-large-region-is-not-quadratic ()
  "Banking scales with the number of lines, not their square.

Regression: `donkey--banked-overlay-at' scanned `donkey--banked-overlays'
linearly and `donkey--bank-span' calls it once per line, so banking cost
quadratic time -- 0.01s for 200 lines, 0.22s for 1000 and 1.81s for 3000,
a visible freeze for selecting a whole file and banking it.  Candidates
now come from `overlays-in', which Emacs answers from its position index.

Asserts the SHAPE of the growth, never an absolute time, so it does not
become a benchmark of whatever machine happens to run it.

It was a flaky benchmark anyway, and measuring showed why.  It compared
500 lines against 2000 -- 4x, where linear predicts 4 and quadratic 16 --
and asserted a ratio below 8.  The real figure ranged 3.7 to 6.9 across
trials, so the pass mark sat inside the noise: one run in twelve failed
with nothing wrong, and it failed twice in this suite while passing five
times in a row on its own.

Three changes, each measured rather than guessed:

  8x the lines, not 4x   linear predicts 8, quadratic 64.  The gap the
                         test is trying to detect is now wide enough to
                         see through the noise.
  minimum of three runs  the minimum is the run least disturbed by
                         scheduling; a mean carries every interruption.
  `garbage-collect' first  so a collection triggered by the setup does
                         not land inside the timed section.

Together those give 12.9 to 13.6 over five trials -- a spread of 0.7,
against 8 for linear and 64 for quadratic.  The threshold of 25 is
roughly twice the observed figure and less than half the quadratic one,
so both a false failure and a false pass need something to change by a
factor of two."
  (let ((transient-mark-mode t))
    (cl-flet* ((bank-n (n)
                 (with-temp-buffer
                   (dotimes (i n) (insert (format "line %d\n" i)))
                   (goto-char (point-min))
                   (push-mark (point) t t)
                   (goto-char (point-max))
                   (garbage-collect)
                   (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
                     (let ((start (float-time)))
                       (donkey-bank-selection)
                       (should (= n (donkey--banked-line-count)))
                       (- (float-time) start)))))
               (best-of-3 (n) (min (bank-n n) (bank-n n) (bank-n n))))
      ;; Warm up, so first-call overheads do not land in the ratio.
      (bank-n 200)
      (let* ((small (max (best-of-3 500) 0.0005))
             (large (best-of-3 4000))
             (ratio (/ large small)))
        (should (< ratio 25))))))

(ert-deftest donkey-docstring-first-lines-are-complete-sentences ()
  "Every docstring in donkey.el opens with a one-line sentence.

Checkdoc enforces this, but it lives in the lint job alone.  A wrapped
first line reached master once already -- `donkey-banked-spans' opened
with \"...as a list of (START . END), in buffer\" and carried the rest
onto a second line -- merged because the four test jobs were green and
the single red job was the easy one to overlook.  Asserting it here puts
the same rule in front of whoever reads the test results.

Scoped by `symbol-file' to symbols defined in donkey.el, matching what
checkdoc is actually run against: the test helpers are not linted and
are free to wrap."
  (let ((package (expand-file-name "donkey.el"
                                   (file-name-directory
                                    (or (symbol-file 'donkey-copy 'defun)
                                        default-directory))))
        offenders)
    (mapatoms
     (lambda (sym)
       (when (string-prefix-p "donkey" (symbol-name sym))
         (dolist (entry (list (cons (and (fboundp sym) (documentation sym))
                                    (symbol-file sym 'defun))
                              (cons (get sym 'variable-documentation)
                                    (symbol-file sym 'defvar))))
           (let ((doc (car entry))
                 (file (cdr entry)))
             (when (and (stringp doc)
                        (not (string-empty-p doc))
                        file
                        (equal (expand-file-name file) package))
               (let ((first (car (split-string doc "\n"))))
                 ;; A complete sentence ends in punctuation, and all of it
                 ;; has to fit on the first line -- an opening sentence
                 ;; that wraps is exactly what checkdoc rejects.
                 (unless (string-match-p "[.!?]\"?\\'" first)
                   (push (format "%s: %s" (symbol-name sym) first)
                         offenders))))))))
     )
    (should-not offenders)))

(ert-deftest donkey-editing-commands-honor-a-count ()
  "`d'/`x', `y' and `c' act on COUNT characters.

Before this, a count was accepted and silently discarded: the stock keys
bound alongside them honored it -- `C-u 3 D' killed three lines, `C-u 3
j' moved three -- while `C-u 3 x' deleted a single character with nothing
to explain the difference."
  (with-temp-buffer
    (insert "abcdefgh\n")
    (goto-char 1)
    (donkey-delete 3)
    (should (equal (buffer-string) "defgh\n")))
  (let ((kill-ring nil) kill-ring-yank-pointer)
    (with-temp-buffer
      (insert "abcdefgh\n")
      (goto-char 1)
      (donkey-copy 4)
      (should (equal (current-kill 0) "abcd"))))
  (with-temp-buffer
    (insert "abcdefgh\n")
    (goto-char 1)
    (donkey-change 3)
    (should (equal (buffer-string) "defgh\n"))))

(ert-deftest donkey-count-clamps-at-point-max-instead-of-signaling ()
  "A count larger than the text left deletes what there is, quietly."
  (with-temp-buffer
    (insert "abc\n")
    (goto-char 1)
    (donkey-delete 99)
    (should (equal (buffer-string) ""))))

(ert-deftest donkey-editing-commands-without-a-count-are-unchanged ()
  "No count still means one character."
  (with-temp-buffer
    (insert "abcdefgh\n")
    (goto-char 1)
    (donkey-delete)
    (should (equal (buffer-string) "bcdefgh\n"))))

;;; ---------------------------------------------------------------------------
;;; Zero and negative counts
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-delete-with-zero-count-deletes-nothing ()
  "A count of zero deletes nothing, exactly as `delete-char' would.

Regression: the count was clamped with `(max 1 count)', so asking to
delete zero characters removed one -- data loss from a request that
explicitly asked for none."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 4)
    (donkey-delete 0)
    (should (equal (buffer-string) "abcdef"))
    (should (= (point) 4))))

(ert-deftest donkey-delete-with-negative-count-deletes-backward ()
  "A negative count deletes that many characters BEFORE point.

Regression: the `(max 1 count)' clamp turned every negative count into
1, so \\[universal-argument] -2 then \"d\" deleted one character
forwards -- the opposite end of the buffer from the one asked for.
Matches `delete-char', which on the same input leaves \"adef\"."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 4)
    (donkey-delete -2)
    (should (equal (buffer-string) "adef"))
    (should (= (point) 2))))

(ert-deftest donkey-delete-negative-count-clamps-at-point-min ()
  "A negative count larger than the text before point stops at the start."
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 4)
    (donkey-delete -99)
    (should (equal (buffer-string) "def"))))

(ert-deftest donkey-delete-negative-count-at-point-min-reports ()
  "With nothing before point a negative count reports rather than signals."
  (let (msg)
    (with-temp-buffer
      (insert "abcdef")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (donkey-delete -1))
      (should (equal (buffer-string) "abcdef"))
      (should (equal msg "Beginning of buffer -- nothing to delete")))))

(ert-deftest donkey-copy-with-zero-count-leaves-kill-ring-alone ()
  "A count of zero copies nothing rather than pushing an empty string.

Same reasoning as copying at `point-max': an empty entry would displace
whatever a following \"p\" was about to paste."
  (let ((kill-ring (list "KEEP")))
    (with-temp-buffer
      (insert "abcdef")
      (goto-char 4)
      (donkey-copy 0)
      (should (equal (car kill-ring) "KEEP")))))

(ert-deftest donkey-copy-with-negative-count-copies-backward ()
  "A negative count copies that many characters before point."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "abcdef")
      (goto-char 4)
      (donkey-copy -2)
      (should (equal (car kill-ring) "bc")))))

(ert-deftest donkey-copy-negative-count-at-point-min-reports ()
  "With nothing before point a negative count reports rather than copies."
  (let ((kill-ring (list "KEEP"))
        msg)
    (with-temp-buffer
      (insert "abcdef")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (donkey-copy -1))
      (should (equal (car kill-ring) "KEEP"))
      (should (equal msg "Beginning of buffer -- nothing to copy")))))

(ert-deftest donkey-change-with-zero-count-still-enters-insert ()
  "A count of zero changes no text but still enters INSERT state.

Entering INSERT is the whole point of the command; only the deletion is
governed by the count."
  (with-temp-buffer
    (donkey-mode 1)
    (insert "abcdef")
    (goto-char 4)
    (donkey-change 0)
    (should (equal (buffer-string) "abcdef"))
    (should donkey-insert-mode)))

(ert-deftest donkey-change-with-negative-count-changes-backward ()
  "A negative count deletes that many characters before point."
  (with-temp-buffer
    (donkey-mode 1)
    (insert "abcdef")
    (goto-char 4)
    (donkey-change -2)
    (should (equal (buffer-string) "adef"))))

;;; ---------------------------------------------------------------------------
;;; Paste: banked lines, and nothing to paste
;;; ---------------------------------------------------------------------------

(defmacro donkey-test--paste-buffer (&rest body)
  "Run BODY in a five-row DONKEY buffer with an isolated kill ring.

`donkey-mode' turned back off afterwards -- it is a GLOBAL minor mode,
so leaving it on leaks into every later test in the process.  See
`donkey--with-test-buffer', which documents the same hazard."
  `(unwind-protect
       (with-temp-buffer
         (donkey-mode 1)
         (let ((transient-mark-mode t)
               (kill-ring nil)
               (kill-ring-yank-pointer nil)
               (donkey--clipboard-warning-shown nil)
               (this-command nil) (last-command nil))
           (insert "AAA one\nBBB two\nCCC three\nDDD four\nEEE five\n")
           ,@body))
     (donkey-mode -1)))

(ert-deftest donkey-yank-replaces-banked-lines ()
  "Banked lines are a selection, so a paste replaces them.

Regression: `donkey-yank' was the one command that could not see the
bank.  It pasted at point and left the highlighted rows sitting there
untouched and still banked, while `donkey-copy' and `donkey-delete' both
acted on them and consumed them."
  (donkey-test--paste-buffer
   (kill-new "ZZZ\n")
   (donkey-test--row 1) (donkey-bank-selection)
   (donkey-test--row 3) (donkey-bank-selection)
   (donkey-test--row 5)
   (donkey-yank)
   (should (equal (buffer-string) "ZZZ\nBBB two\nDDD four\nEEE five\n"))))

(ert-deftest donkey-yank-over-banked-lines-consumes-the-bank ()
  "The bank is spent by the paste, the way `y' and `d' spend it."
  (donkey-test--paste-buffer
   (kill-new "ZZZ\n")
   (donkey-test--row 1) (donkey-bank-selection)
   (donkey-test--row 3) (donkey-bank-selection)
   (donkey-yank)
   (should (= 0 (donkey--banked-line-count)))))

(ert-deftest donkey-yank-replaces-banked-lines-and-the-live-region ()
  "The region counts too, exactly as it does for `y' and `d'."
  (donkey-test--paste-buffer
   (kill-new "ZZZ\n")
   (donkey-test--row 1) (donkey-bank-selection)
   (donkey-test--row 4) (push-mark (point-max) t t)
   (donkey-yank)
   (should (equal (buffer-string) "ZZZ\nBBB two\nCCC three\n"))))

(ert-deftest donkey-yank-over-banked-lines-does-not-clobber-the-paste ()
  "The replaced lines are deleted, not killed.

Killing them would push them onto the kill ring, and the paste that
follows would then pull those back instead of what was being pasted."
  (donkey-test--paste-buffer
   (kill-new "ZZZ\n")
   (donkey-test--row 1) (donkey-bank-selection)
   (donkey-test--row 3) (donkey-bank-selection)
   (donkey-yank)
   (should (equal (car kill-ring) "ZZZ\n"))))

(ert-deftest donkey-yank-with-nothing-to-paste-leaves-the-region-alone ()
  "With nothing to paste, `p' does nothing at all.

Regression: the region was deleted and only THEN did the yank discover
it had nothing to insert, leaving the selected text gone, nothing
pasted, and a bare \"Kill ring is empty\" on screen -- gone for real,
since the region is deleted rather than killed.  Confirmed live: two
selected rows vanished with the kill ring still empty afterwards."
  (donkey-test--paste-buffer
   (donkey-test--row 1)
   (push-mark (save-excursion (donkey-test--row 3) (point)) t t)
   (donkey-yank)
   (should (equal (buffer-string)
                  "AAA one\nBBB two\nCCC three\nDDD four\nEEE five\n"))
   (should (region-active-p))))

(ert-deftest donkey-yank-with-nothing-to-paste-leaves-the-bank-alone ()
  "The same guard protects banked lines."
  (donkey-test--paste-buffer
   (donkey-test--row 1) (donkey-bank-selection)
   (donkey-test--row 3) (donkey-bank-selection)
   (donkey-yank)
   (should (equal (buffer-string)
                  "AAA one\nBBB two\nCCC three\nDDD four\nEEE five\n"))
   (should (= 2 (donkey--banked-line-count)))))

(ert-deftest donkey-yank-with-nothing-to-paste-reports ()
  "It says so rather than failing silently or signaling."
  (let (shown)
    (donkey-test--paste-buffer
     (donkey-test--row 1)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
       (donkey-yank))
     (should (equal shown "Nothing to paste")))))

(ert-deftest donkey-yank-count-inserts-that-many-copies ()
  "A count on `p' inserts that many copies, as it does in vi.

Emacs reads a prefix argument on `C-y' as WHICH `kill-ring' entry to pull
instead.  Nothing is given up by taking the vi reading here: `C-y' is
untouched in INSERT state, so Emacs' own meaning is still available on
the key it belongs to."
  (donkey-test--paste-buffer
   (kill-new "X")
   (goto-char (point-min))
   (donkey-yank 3)
   (should (equal (buffer-substring-no-properties (point-min) 4) "XXX"))))

(ert-deftest donkey-yank-without-a-count-inserts-one-copy ()
  "No count still means one."
  (donkey-test--paste-buffer
   (kill-new "X")
   (goto-char (point-min))
   (donkey-yank)
   (should (equal (buffer-substring-no-properties (point-min) 5) "XAAA"))))

(ert-deftest donkey-yank-count-of-zero-inserts-nothing ()
  "A count of zero pastes nothing, like zero counts elsewhere."
  (donkey-test--paste-buffer
   (kill-new "X")
   (goto-char (point-min))
   (donkey-yank 0)
   (should (equal (buffer-string)
                  "AAA one\nBBB two\nCCC three\nDDD four\nEEE five\n"))))

(ert-deftest donkey-yank-negative-count-inserts-nothing ()
  "Negative gets the same answer as zero.

A paste has no backward direction for a negative count to mean, unlike
`donkey-delete', so there is nothing for it to do but nothing."
  (donkey-test--paste-buffer
   (kill-new "X")
   (goto-char (point-min))
   (donkey-yank -2)
   (should (equal (buffer-string)
                  "AAA one\nBBB two\nCCC three\nDDD four\nEEE five\n"))))

(ert-deftest donkey-yank-count-over-a-region-replaces-with-copies ()
  "The selection is replaced once, then the paste repeats."
  (donkey-test--paste-buffer
   (kill-new "X")
   (goto-char (point-min))
   (push-mark (save-excursion (donkey-test--row 2) (point)) t t)
   (donkey-yank 3)
   (should (equal (buffer-string)
                  "XXXBBB two\nCCC three\nDDD four\nEEE five\n"))))

(ert-deftest donkey-yank-count-of-zero-over-a-region-is-a-delete ()
  "Replacing a selection with nothing is a delete, and says so by doing it."
  (donkey-test--paste-buffer
   (kill-new "X")
   (goto-char (point-min))
   (push-mark (save-excursion (donkey-test--row 2) (point)) t t)
   (donkey-yank 0)
   (should (equal (buffer-string)
                  "BBB two\nCCC three\nDDD four\nEEE five\n"))))

(ert-deftest donkey-yank-count-over-banked-lines-replaces-with-copies ()
  "Banked lines are removed once, then the paste repeats."
  (donkey-test--paste-buffer
   (kill-new "ZZ\n")
   (donkey-test--row 1) (donkey-bank-selection)
   (donkey-test--row 3) (donkey-bank-selection)
   (donkey-yank 2)
   (should (equal (buffer-string) "ZZ\nZZ\nBBB two\nDDD four\nEEE five\n"))))

(ert-deftest donkey-yank-count-of-zero-pastes-nothing ()
  "A count below 1 pastes nothing."
  (with-temp-buffer
    (donkey-mode 1)
    ;; `killed-rectangle' is deliberately global, not
    ;; buffer-local, so a rectangle copied here would otherwise be seen by
    ;; every later test in the run -- which is exactly what happened: three
    ;; `donkey-yank-region-*' tests started taking the rectangle branch.
    (let ((transient-mark-mode t) (kill-ring nil) (killed-rectangle nil))
      (insert "AAA one\nBBB two\nCCC three\nDDD four\n")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (goto-char (point-min))
        (push-mark 20 t t)
        (rectangle-mark-mode 1)
        (donkey-copy)
        (deactivate-mark)
        (goto-char (point-max))
        (donkey-yank 0))
      (should (equal (buffer-string)
                     "AAA one\nBBB two\nCCC three\nDDD four\n")))))

(ert-deftest donkey-banked-line-count-follows-a-split-banked-line ()
  "Typing a newline inside a banked line makes it count as two.

Regression: the count was the number of OVERLAYS, on the invariant that
`donkey--bank-span' makes one per line.  An overlay advances with text
inserted at its end, so a newline typed inside a banked line grows it
over both -- and the count then reported one while `y' and `d' acted on
two.  `donkey--bank-span' documents the same divergence for the emptied
-buffer case that `evaporate' handles; evaporating cannot help here,
because the text was never deleted."
  (with-temp-buffer
    (donkey-mode 1)
    (let ((transient-mark-mode t) (kill-ring nil))
      (insert "r1 aaa\nr2 bbb\nr3 ccc\n")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (goto-char (point-min))
        (forward-line 1)
        (donkey-bank-selection)
        (should (= 1 (donkey--banked-line-count)))
        (goto-char (point-min))
        (forward-line 1)
        (end-of-line)
        (insert "\nSPLIT")
        (should (= 2 (donkey--banked-line-count)))))))

(ert-deftest donkey-banked-line-count-agrees-with-what-copy-reports ()
  "Every message about the bank counts the same way.

`donkey-copy' and `donkey-delete' report via `donkey--span-line-count';
so does the total in `donkey-bank-selection's own message now, so the two
cannot drift apart."
  (with-temp-buffer
    (donkey-mode 1)
    (let ((transient-mark-mode t) (kill-ring nil) shown)
      (insert "r1 aaa\nr2 bbb\nr3 ccc\n")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (goto-char (point-min))
        (forward-line 1)
        (donkey-bank-selection)
        (goto-char (point-min))
        (forward-line 1)
        (end-of-line)
        (insert "\nSPLIT"))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-copy))
      (should (equal shown "Copied 2 lines")))))

;;; ---------------------------------------------------------------------------
;;; Banks survive narrowing when y/d/p spend them
;;; ---------------------------------------------------------------------------

(defmacro donkey-test--hidden-bank (&rest body)
  "Bank AAA and DDD, hide AAA by narrowing, then run BODY.

`donkey-mode' is turned back off in an `unwind-protect', exactly as
`donkey--with-test-buffer' does and for the reason its docstring gives:
it is a GLOBAL minor mode, so enabling it installs
`after-change-major-mode-hook' process-wide.  Leaving it on made a much
later test find DONKEY already active in a buffer it had just created,
where `g' is a prefix rather than `revert-buffer'.

The other bindings are the globals a paste touches: the one-shot
clipboard-tip flag, `kill-ring-yank-pointer' (which `kill-new' sets even
when only `kill-ring' is bound, leaving it aimed at a discarded list),
and `this-command', which `yank' sets and which batch has no command loop
to reset."
  `(unwind-protect
       (with-temp-buffer
         (donkey-mode 1)
         (let ((transient-mark-mode t)
               (kill-ring nil)
               (kill-ring-yank-pointer nil)
               (donkey--clipboard-warning-shown nil)
               (this-command nil)
               (last-command nil))
           (insert "AAA one\nBBB two\nCCC three\nDDD four\nEEE five\n")
           (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
             (goto-char (point-min))
             (donkey-bank-selection)
             (goto-char (point-min))
             (search-forward "DDD")
             (beginning-of-line)
             (donkey-bank-selection)
             (narrow-to-region
              (save-excursion (goto-char (point-min)) (forward-line 1) (point))
              (save-excursion (goto-char (point-min)) (forward-line 4) (point)))
             ,@body)))
     (donkey-mode -1)))

(ert-deftest donkey-copy-does-not-discard-a-bank-it-could-not-see ()
  "`y' spends only the banks it acted on, not every bank in the buffer.

Regression: it copied the visible bank -- correctly -- and then cleared
the whole list, throwing away a bank the narrowing had hidden without
ever copying it.  `donkey--banked-spans' promises in its own docstring
that filtered-out overlays \"survive untouched and count again once the
buffer is widened\"; clearing the list broke that promise."
  (donkey-test--hidden-bank
   (donkey-copy)
   (should (equal (car kill-ring) "DDD four\n"))
   (widen)
   (should (= 1 (donkey--banked-line-count)))))

(ert-deftest donkey-delete-does-not-discard-a-bank-it-could-not-see ()
  "`d' spends only what it deleted."
  (donkey-test--hidden-bank
   (donkey-delete)
   (widen)
   (should (= 1 (donkey--banked-line-count)))
   (should (string-match-p "AAA one" (buffer-string)))))

(ert-deftest donkey-yank-does-not-discard-a-bank-it-could-not-see ()
  "`p' spends only what it replaced."
  (donkey-test--hidden-bank
   (kill-new "ZZZ\n")
   (donkey-yank)
   (widen)
   (should (= 1 (donkey--banked-line-count)))))

(ert-deftest donkey-clear-banked-selection-still-discards-everything ()
  "`m DEL' is the explicit discard-everything command and stays that way.

The contrast with `y'/`d'/`p' is the point: those spend what they used,
this one is named for clearing the lot and does."
  (donkey-test--hidden-bank
   (donkey-clear-banked-selection)
   (widen)
   (should (= 0 (donkey--banked-line-count)))))

(ert-deftest donkey-banked-consumers-still-spend-everything-unnarrowed ()
  "With nothing hidden, all of it is still spent."
  (dolist (op (list (lambda () (donkey-copy))
                    (lambda () (donkey-delete))
                    (lambda () (kill-new "Z\n") (donkey-yank))))
    (unwind-protect
        (with-temp-buffer
          (donkey-mode 1)
          (let ((transient-mark-mode t)
                (kill-ring nil)
                (kill-ring-yank-pointer nil)
                (donkey--clipboard-warning-shown nil)
                (this-command nil)
                (last-command nil))
            (insert "AAA\nBBB\nCCC\nDDD\n")
            (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
              (goto-char (point-min))
              (donkey-bank-selection)
              (goto-char (point-min))
              (forward-line 2)
              (donkey-bank-selection)
              (funcall op)
              (should (= 0 (donkey--banked-line-count))))))
      (donkey-mode -1))))

(ert-deftest donkey-map-line-spans-terminates-on-a-stale-span ()
  "A span reaching past `point-max' stops rather than spinning.

`donkey--whole-line-span' returns the position it was given once point
is at `point-max', so the walk cannot advance -- and jumping to END does
not help, because `goto-char' clamps there too.  A hung Emacs is a far
worse failure than a span walked one line short, so the loop stops."
  (with-temp-buffer
    (insert "AAA\nBBB\n")
    (let ((calls 0))
      ;; deliberately past the end
      (donkey--map-line-spans 1 500 (lambda (_span) (cl-incf calls)))
      (should (< calls 100)))))

;;; ---------------------------------------------------------------------------
;;; A rectangle copy stays out of the clipboard, on purpose
;;; ---------------------------------------------------------------------------

(defmacro donkey-test--with-clipboard-spy (var &rest body)
  "Run BODY with the system clipboard captured into VAR.

`interprogram-cut-function' is what Emacs hands text to on its way to the
window system, so binding it records exactly what would have left Emacs."
  (declare (indent 1))
  `(let ((,var nil))
     (unwind-protect
         (with-temp-buffer
           (donkey-mode 1)
           (let ((transient-mark-mode t)
                 (kill-ring nil)
                 (kill-ring-yank-pointer nil)
                 (killed-rectangle nil)
                 (donkey--clipboard-warning-shown nil)
                 (this-command nil)
                 (last-command nil)
                 (interprogram-cut-function
                  (lambda (text) (push text ,var))))
             (insert "AAA one\nBBB two\nCCC three\n")
             (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
               ,@body)))
       (donkey-mode -1))))

(ert-deftest donkey-rectangle-copy-does-not-reach-the-clipboard ()
  "A rectangle copy fills `killed-rectangle' and nothing else.

DELIBERATE, and pinned here because it reads like an oversight every
time someone looks -- it has been raised, investigated and set aside
more than once.  A rectangle has no meaning outside a buffer that would
survive the trip through a flat clipboard, and stock
`copy-rectangle-as-kill' behaves the same way.

If this test ever fails because someone made the copy reach the
clipboard, that is a decision to take on purpose, not a bug to fix in
passing -- and note that a rectangle is pasted by its own key, so
pushing the text onto the `kill-ring' as well would put one copy in two
stores that are emptied independently."
  (donkey-test--with-clipboard-spy sent
    (goto-char (point-min))
    (push-mark 20 t t)
    (rectangle-mark-mode 1)
    (donkey-copy)
    (should (equal killed-rectangle '("AAA" "BBB" "CCC")))
    (should (null kill-ring))
    (should (null sent))))

(ert-deftest donkey-rectangle-cut-does-not-reach-the-clipboard ()
  "`d' on a rectangle behaves the same way as `y' does."
  (donkey-test--with-clipboard-spy sent
    (goto-char (point-min))
    (push-mark 20 t t)
    (rectangle-mark-mode 1)
    (donkey-delete)
    (should (equal killed-rectangle '("AAA" "BBB" "CCC")))
    (should (null kill-ring))
    (should (null sent))))

(ert-deftest donkey-ordinary-and-banked-copies-do-reach-the-clipboard ()
  "The contrast that makes the rectangle case a deliberate exception.

Asserted alongside it so the difference is visible as a choice rather
than looking like the rectangle path was simply forgotten."
  (donkey-test--with-clipboard-spy sent
    (goto-char (point-min))
    (push-mark 8 t t)
    (donkey-copy)
    (should (equal sent '("AAA one"))))
  (donkey-test--with-clipboard-spy sent
    (goto-char (point-min))
    (donkey-bank-selection)
    (goto-char (point-min))
    (forward-line 2)
    (donkey-bank-selection)
    (donkey-copy)
    (should (equal sent '("AAA one\nCCC three\n")))))

;;; ---------------------------------------------------------------------------
;;; Where `c' deliberately parts company with `y' and `d'
;;; ---------------------------------------------------------------------------

(defmacro donkey-test--change-buffer (&rest body)
  "Run BODY over a three-line buffer with DONKEY on and messages silenced.

`donkey-mode' is turned back off in an `unwind-protect' -- it is a GLOBAL
minor mode, so leaving it on leaks into every later test."
  `(unwind-protect
       (with-temp-buffer
         (donkey-mode 1)
         (let ((transient-mark-mode t)
               (kill-ring nil)
               (kill-ring-yank-pointer nil)
               (this-command nil)
               (last-command nil))
           (insert "alpha\nbeta\ngamma\n")
           (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
             ,@body)))
     (donkey-mode -1)))

(ert-deftest donkey-visual-line-change-empties-the-line-and-keeps-it ()
  "`V c' clears the line and leaves point on it, vi's linewise change.

The deliberate counterpart to `V d', which takes the newline too.  Pinned
because the two now read as inconsistent unless you know `cc' -- and a
later tidy-up that routed `c' through
`donkey--visual-line-region-bounds' for symmetry would silently turn
\"change this line\" into \"delete it and type on the next\"."
  (donkey-test--change-buffer
   (goto-char (point-min))
   (forward-line 1)
   (donkey-visual-line-toggle)
   (donkey-change 1)
   (should (equal (buffer-string) "alpha\n\ngamma\n"))
   (should (bound-and-true-p donkey-insert-mode))))

(ert-deftest donkey-visual-line-change-collapses-a-span-to-one-blank-line ()
  "`V J c' clears both lines and leaves a single empty one, as vi does."
  (donkey-test--change-buffer
   (goto-char (point-min))
   (forward-line 1)
   (donkey-visual-line-toggle)
   (donkey-visual-next-line 1)
   (donkey-change 1)
   (should (equal (buffer-string) "alpha\n\n"))))

(ert-deftest donkey-visual-line-delete-removes-the-line-outright ()
  "`V d' takes the newline, the contrast that makes `V c' meaningful."
  (donkey-test--change-buffer
   (goto-char (point-min))
   (forward-line 1)
   (donkey-visual-line-toggle)
   (donkey-delete 1)
   (should (equal (buffer-string) "alpha\ngamma\n"))))

(ert-deftest donkey-change-does-not-act-on-banked-lines ()
  "`c' changes the character at point and leaves banks standing.

Documented rather than fixed: nothing has been decided about what
changing a multi-line bank should do.  Pinned so the split is a known
one -- if `c' ever learns to honor banks, this test is the place that
says the change was deliberate."
  (donkey-test--change-buffer
   (goto-char (point-min))
   (forward-line 1)
   (donkey-bank-selection)
   (goto-char (point-min))
   (should (= (length (donkey--banked-spans)) 1))
   (donkey-change 1)
   (should (equal (buffer-string) "lpha\nbeta\ngamma\n"))
   (should (= (length (donkey--banked-spans)) 1))))

(ert-deftest donkey-delete-does-act-on-banked-lines ()
  "`d' consumes the bank -- the contrast `c' is measured against."
  (donkey-test--change-buffer
   (goto-char (point-min))
   (forward-line 1)
   (donkey-bank-selection)
   (goto-char (point-min))
   (donkey-delete 1)
   (should (equal (buffer-string) "alpha\ngamma\n"))
   (should (= (length (donkey--banked-spans)) 0))))

(ert-deftest donkey-bank-prompt-names-every-command-that-spends-a-bank ()
  "The banking prompt says y/d/p, not y/d.

`donkey-yank' replaces a bank as well, and said so nowhere on screen."
  (donkey-test--change-buffer
   (let (shown)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
       (goto-char (point-min))
       (donkey-bank-selection))
     (should (string-suffix-p "-- navigate, then y/d/p" shown)))))

;;; ---------------------------------------------------------------------------
;;; donkey-join-line
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-join-line-pulls-the-following-line-up ()
  "Joining absorbs the line BELOW point, the direction vi and Helix use.

The old `C-j' binding ran `join-line' with no argument, which joins with
the PREVIOUS line -- while the README had always described the key as
\"Join line with next\".  The documentation promised the modal reading
and the key delivered the Emacs one."
  (with-temp-buffer
    (insert "alpha\n    beta\ngamma\n")
    (goto-char (point-min))
    (donkey-join-line 1)
    (should (equal (buffer-string) "alpha beta\ngamma\n"))))

(ert-deftest donkey-join-line-fixes-up-whitespace ()
  "Leading indentation on the absorbed line collapses to a single space."
  (with-temp-buffer
    (insert "one\n\t\t   two\n")
    (goto-char (point-min))
    (donkey-join-line 1)
    (should (equal (buffer-string) "one two\n"))))

(ert-deftest donkey-join-line-count-joins-that-many-lines ()
  "A count collapses that many following lines into this one."
  (with-temp-buffer
    (insert "a\nb\nc\nd\ne\n")
    (goto-char (point-min))
    (donkey-join-line 3)
    (should (equal (buffer-string) "a b c d\ne\n"))))

(ert-deftest donkey-join-line-zero-or-negative-count-joins-nothing ()
  "A count below 1 does nothing rather than reversing direction.

`join-line' reads any nil-or-non-positive argument as \"join to the
PREVIOUS line\", so passing a user's zero straight through would have
silently flipped the direction of a command they asked to do less of."
  (dolist (n '(0 -1 -5))
    (with-temp-buffer
      (insert "alpha\nbeta\ngamma\n")
      (goto-char (point-min))
      (forward-line 1)
      (donkey-join-line n)
      (should (equal (buffer-string) "alpha\nbeta\ngamma\n")))))

(ert-deftest donkey-join-line-at-last-line-is-harmless ()
  "Nothing below to pull up, and no error."
  (with-temp-buffer
    (insert "only line\n")
    (goto-char (point-min))
    (should (progn (donkey-join-line 1) t))))

(ert-deftest donkey-join-line-is-bound-to-g-j ()
  "The command lives at `g j'."
  (should (eq (lookup-key donkey-normal-mode-map (kbd "g j"))
              #'donkey-join-line)))

(ert-deftest donkey-normal-state-leaves-c-j-alone ()
  "`C-j' is not bound in Normal state, so `*scratch*' keeps its eval key.

`C-j' looks free and is not: globally `electric-newline-and-maybe-indent',
and `eval-print-last-sexp' in `lisp-interaction-mode'.  A minor-mode map
outranks the major mode, so binding it here cost the scratch buffer its
evaluate-and-print key -- the one stock Emacs command Normal state took
away that a user would actually miss."
  (should-not (lookup-key donkey-normal-mode-map (kbd "C-j"))))

(ert-deftest donkey-normal-state-keeps-eval-print-last-sexp-in-scratch ()
  "The end-to-end version of the above, through real key lookup."
  (unwind-protect
      (with-temp-buffer
        (lisp-interaction-mode)
        (donkey-mode 1)
        (donkey-normal-mode 1)
        (should (eq (key-binding (kbd "C-j")) #'eval-print-last-sexp)))
    (donkey-mode -1)))

(ert-deftest donkey-join-line-does-not-shadow-emacs-own-direction ()
  "`M-^' still joins the other way, and is still reachable from Normal state."
  (unwind-protect
      (with-temp-buffer
        (donkey-mode 1)
        (donkey-normal-mode 1)
        (should (eq (key-binding (kbd "M-^")) #'delete-indentation)))
    (donkey-mode -1)))

;;; ---------------------------------------------------------------------------
;;; Banks and rectangles: the live selection wins
;;; ---------------------------------------------------------------------------

(defmacro donkey-test--bank-and-rectangle (&rest body)
  "Run BODY over a four-line buffer with DONKEY on and messages silenced.

Binds every global a rectangle or a paste touches, including
`killed-rectangle'.  `donkey-mode' goes back off in an
`unwind-protect': it is a GLOBAL minor mode, so leaving it on leaks
into every later test."
  `(unwind-protect
       (with-temp-buffer
         (donkey-mode 1)
         (let ((transient-mark-mode t)
               (kill-ring nil)
               (kill-ring-yank-pointer nil)
               (killed-rectangle nil)
               (donkey--clipboard-warning-shown nil)
               (this-command nil)
               (last-command nil))
           (insert "AAA one\nBBB two\nCCC three\nDDD four\n")
           (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
             ,@body)))
     (donkey-mode -1)))

(defun donkey-test--row (n)
  "Move point to the beginning of line N, counting from `point-min'."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun donkey-test--draw-rectangle (row1 row2 cols)
  "Select a rectangle from ROW1 to ROW2 spanning COLS columns."
  (donkey-test--row row1)
  (set-mark (point))
  (donkey-test--row row2)
  (forward-char cols)
  (rectangle-mark-mode 1))

(ert-deftest donkey-copy-takes-the-rectangle-over-a-bank ()
  "`y' over a live rectangle copies the rectangle, banks or not.

Regression: the banked branch came first, so `y' copied whole banked
lines while a rectangle sat selected on screen -- and left
`killed-rectangle' nil, so the rectangle the user thought they had
copied was not even available to paste back."
  (donkey-test--bank-and-rectangle
   (donkey-test--row 1)
   (donkey-bank-selection)
   (donkey-test--draw-rectangle 2 3 3)
   (donkey-copy 1)
   (should (equal killed-rectangle '("BBB" "CCC")))
   (should-not kill-ring)))

(ert-deftest donkey-copy-leaves-the-bank-standing-after-a-rectangle ()
  "Taking the rectangle spends nothing: the banks are still there."
  (donkey-test--bank-and-rectangle
   (donkey-test--row 1)
   (donkey-bank-selection)
   (donkey-test--draw-rectangle 2 3 3)
   (donkey-copy 1)
   (should (= (length (donkey--banked-spans)) 1))))

(ert-deftest donkey-delete-takes-the-rectangle-over-a-bank ()
  "`d' over a live rectangle kills the rectangle, not the banked lines.

Regression, and worse than the `y' case: drawing a rectangle over two
rows and pressing `d' deleted three whole banked lines, taking text the
rectangle never covered."
  (donkey-test--bank-and-rectangle
   (donkey-test--row 1)
   (donkey-bank-selection)
   (donkey-test--draw-rectangle 2 3 3)
   (donkey-delete 1)
   (should (equal (buffer-string) "AAA one\n two\n three\nDDD four\n"))
   (should (equal killed-rectangle '("BBB" "CCC")))
   (should (= (length (donkey--banked-spans)) 1))))

(ert-deftest donkey-yank-over-a-bank-with-nothing-to-paste-keeps-the-bank ()
  "A paste with an empty kill ring reports, and does not eat the bank.

The emptiness check has to happen inside the banked branch now that the
branch runs before the rectangle one -- hoisting it above would have
made a pending rectangle unpasteable whenever the kill ring was empty,
which is exactly the round trip it exists for."
  (donkey-test--bank-and-rectangle
   (donkey-test--row 1)
   (donkey-bank-selection)
   (let ((interprogram-paste-function (lambda () nil)))
     (donkey-yank 1))
   (should (equal (buffer-string) "AAA one\nBBB two\nCCC three\nDDD four\n"))
   (should (= (length (donkey--banked-spans)) 1))))

(ert-deftest donkey-bank-and-rectangle-precedence-is-the-same-in-all-three ()
  "The three commands agree: live selection wins, bank is the fallback.

They used to disagree, each silently -- `y' and `d' took the bank over a
rectangle on screen, `p' took a pending rectangle over banks on screen.
The inconsistency was the actual defect; this pins the single rule."
  ;; y and d: a drawn rectangle beats a bank.
  (donkey-test--bank-and-rectangle
   (donkey-test--row 1)
   (donkey-bank-selection)
   (donkey-test--draw-rectangle 2 3 3)
   (donkey-copy 1)
   (should (equal killed-rectangle '("BBB" "CCC")))
   (should (= (length (donkey--banked-spans)) 1)))
  ;; p: a bank beats a rectangle that is only in `killed-rectangle'.
  (donkey-test--bank-and-rectangle
   (setq killed-rectangle '("XX" "YY")
         )
   (kill-new "ZZZ\n")
   (donkey-test--row 1)
   (donkey-bank-selection)
   (donkey-test--row 4)
   (donkey-yank 1)
   (should (string-prefix-p "ZZZ\n" (buffer-string)))
   (should (= (length (donkey--banked-spans)) 0))))

(ert-deftest donkey-join-line-on-the-last-line-keeps-the-final-newline ()
  "`g j\=' on the last line does nothing, rather than eating the EOF newline.

Regression, found by a live audit: `join-line\=' pulls up the empty
position after the final newline, which removes it.  Nothing visible
changes -- the screen is identical and point sits at `point-max\=' either
way -- so the first sign is a diff reporting \"\\ No newline at end of
file\".  Checked in both buffer shapes, since the last line is a
different place depending on whether the buffer ends in a newline."
  (dolist (text '("alpha\nbeta\n" "alpha\n" "alpha\nbeta"))
    (with-temp-buffer
      (insert text)
      (goto-char (point-max))
      (beginning-of-line)
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (donkey-join-line 1))
      (should (equal (buffer-string) text)))))

(ert-deftest donkey-join-line-count-past-the-end-keeps-the-final-newline ()
  "A count that overshoots stops at the last line instead of eating it.

The same defect on the last iteration: `C-u 99 g j\=' joined everything
and then took the newline as well."
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      (donkey-join-line 99))
    (should (equal (buffer-string) "a b c\n"))))

(ert-deftest donkey-join-line-still-joins-when-there-is-a-line-below ()
  "The guard must not cost the command its actual job."
  (with-temp-buffer
    (insert "a\nb\nc\nd\n")
    (goto-char (point-min))
    (donkey-join-line 1)
    (should (equal (buffer-string) "a b\nc\nd\n")))
  (with-temp-buffer
    (insert "a\nb\nc\nd\n")
    (goto-char (point-min))
    (donkey-join-line 2)
    (should (equal (buffer-string) "a b c\nd\n")))
  ;; a blank line between two lines is still joined away
  (with-temp-buffer
    (insert "a\n\nb\n")
    (goto-char (point-min))
    (donkey-join-line 1)
    (should (equal (buffer-string) "a\nb\n"))))

(ert-deftest donkey-join-line-says-so-when-there-is-nothing-below ()
  "It reports rather than appearing to have done something."
  (with-temp-buffer
    (insert "only\n")
    (goto-char (point-min))
    (let (shown)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
        (donkey-join-line 1))
      (should (equal shown "No line below to join")))))

(ert-deftest donkey-whole-line-commands-all-keep-the-final-newline ()
  "Every whole-line command leaves the buffer\='s last newline alone.

`g j\=' was the only one that did not, which is what made it stand out.
Pinned as a family so the next one added is measured against them."
  (dolist (act (list (lambda () (donkey-visual-line-toggle) (donkey-delete 1))
                     (lambda () (donkey-visual-line-toggle) (donkey-copy 1))
                     (lambda () (donkey-bank-selection) (donkey-copy 1))
                     (lambda () (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
                                  (donkey-join-line 1)))))
    (unwind-protect
        (with-temp-buffer
          (donkey-mode 1)
          (let ((transient-mark-mode t) (kill-ring nil) (kill-ring-yank-pointer nil))
            (insert "alpha\nbeta\n")
            (goto-char (point-max))
            (beginning-of-line)
            (forward-line -1)
            (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
              (funcall act))
            (should (or (string-suffix-p "\n" (buffer-string))
                        (equal (buffer-string) "")))))
      (donkey-mode -1))))

;;; ---------------------------------------------------------------------------
;;; Only a selection reaches the kill ring
;;; ---------------------------------------------------------------------------

(defmacro donkey-killrule-test (&rest body)
  "Run BODY in a DONKEY buffer with an isolated, pre-loaded `kill-ring'.

The ring starts holding \"PREVIOUS\" so that \"nothing was saved\" and
\"the removed text was saved\" are told apart by WHAT is on the ring
rather than by whether anything is -- an empty ring cannot distinguish a
command that saved nothing from one that saved and lost it.  The
clipboard hooks are unbound so a developer's real clipboard is neither
read nor written."
  (declare (indent 0))
  `(with-temp-buffer
     (text-mode)
     (donkey-mode 1)
     (let ((transient-mark-mode t) (this-command nil) (last-command nil)
           (inhibit-message t)
           (kill-ring (list "PREVIOUS")) (kill-ring-yank-pointer nil)
           (killed-rectangle '("PREVIOUS"))
           (select-enable-clipboard nil)
           (interprogram-cut-function nil)
           (interprogram-paste-function nil))
       ,@body)))

(ert-deftest donkey-delete-and-change-save-a-selection ()
  "What `d' and `c' remove from a SELECTION goes on the `kill-ring'.

The half of the rule that makes a replaced selection recoverable.  `c'
saved nothing at all before -- not even over a region, where `d' always
did -- so changing a marked word and pasting produced whatever happened
to be on the ring already."
  (dolist (command '(donkey-delete donkey-change))
    (donkey-killrule-test
      (insert "alpha beta")
      (goto-char 1)
      (push-mark 1 t t)
      (goto-char 6)
      (funcall command)
      (should (equal (cons command (car kill-ring))
                     (cons command "alpha"))))))

(ert-deftest donkey-delete-and-change-leave-the-ring-alone-without-a-selection ()
  "With NO selection, `d' and `c' save nothing -- counted runs included.

The other half, and the deliberate one: a character removed under the
cursor is a typo being fixed rather than a cut, and filling the ring with
single characters would push out what was put there on purpose.  A COUNT
does not make it a selection, so \\`C-u 3 d' does not save either.

Asserted for both because the rule is the same rule, and stated in both
docstrings as such -- if one command starts saving here, the other has
either drifted or been changed on purpose, and this fails either way."
  (dolist (command '(donkey-delete donkey-change))
    (dolist (count '(nil 1 3 99))
      (donkey-killrule-test
        (insert "abcdef")
        (goto-char 1)
        (if count (funcall command count) (funcall command))
        (should (equal (list command count (car kill-ring))
                       (list command count "PREVIOUS")))))))

(ert-deftest donkey-copy-saves-even-a-single-character ()
  "`y' is exempt from the rule, because saving is the whole point.

A copy that declined to save a single character would do nothing at all,
so `y' fills the ring whether or not a selection was made.  Pinned so
that the rule for `d' and `c' is never generalized onto it."
  (dolist (count '(nil 1 3))
    (donkey-killrule-test
      (insert "abcdef")
      (goto-char 1)
      (if count (donkey-copy count) (donkey-copy))
      (should (equal (list count (car kill-ring))
                     (list count (substring "abcdef" 0 (or count 1))))))))

(ert-deftest donkey-change-over-a-rectangle-saves-the-old-columns ()
  "A changed rectangle goes to `killed-rectangle', not the `kill-ring'.

A rectangle is a selection, so the same rule applies -- but it applies in
the rectangle's own store, which is where `donkey-delete' and
`donkey-copy' put theirs and where \\[donkey-yank-rectangle] pastes from.
`string-rectangle' replaces in place and saves nothing itself, so before
this the old columns were unrecoverable.

The `kill-ring' is asserted untouched alongside it: a rectangle never
reaches the linear ring, which is what `donkey-copy' documents at
length."
  (donkey-killrule-test
    (insert "abcd\nefgh\n")
    (goto-char 1)
    (rectangle-mark-mode 1)
    (goto-char 8)
    ;; Stubbed interactively, since `donkey-change' reaches it through
    ;; `call-interactively' and the real one prompts in the minibuffer.
    ;; What it would insert is beside the point here -- the question is
    ;; whether the columns it replaces were saved first.
    (cl-letf (((symbol-function 'string-rectangle)
               (lambda (start end string)
                 (interactive (list nil nil ""))
                 (ignore start end string))))
      (donkey-change))
    (should (equal killed-rectangle '("ab" "ef")))
    (should (equal (car kill-ring) "PREVIOUS"))))

(provide 'donkey-editing-test)

;;; ---------------------------------------------------------------------------
;;; donkey-yank / donkey-yank-rectangle -- the two-key split
;;; ---------------------------------------------------------------------------

(defmacro donkey-test--with-clipboard (clipboard &rest body)
  "Run BODY with a system clipboard simulated as read AND write.

CLIPBOARD non-nil gives a working clipboard, nil a session without one.
Both halves matter.  Stubbing only the read side models a session where
copies go nowhere, which no real Emacs behaves like, and it produced a
result that looked like a defect until the write side was added.

The read/write pair is also the only way the bug this split fixes is
visible at all: `current-kill' calls `kill-new' to import the clipboard
during a PASTE, and it was that import -- not any kill -- that used to
retire a pending rectangle.  With no clipboard there is no import, so
`--batch' saw the old code behave correctly and the suite stayed green
while real sessions did not."
  (declare (indent 1))
  `(let ((donkey-test--clip nil))
     (let ((interprogram-cut-function
            (if ,clipboard (lambda (text) (setq donkey-test--clip text)) #'ignore))
           (interprogram-paste-function
            (if ,clipboard (lambda () donkey-test--clip) (lambda () nil))))
       ,@body)))

(defvar donkey-test--clip nil
  "Stand-in for the system clipboard in `donkey-test--with-clipboard'.")

(ert-deftest donkey-yank-never-reaches-for-the-rectangle-store ()
  "`p' pastes linear text even when `killed-rectangle' is full.

The everyday case, and the one that rules out the obvious simplification
of dropping the flag and having \"p\" check `killed-rectangle' directly.
`killed-rectangle' is never cleared by anything -- `rect.el' only ever
assigns it -- so one rectangle copy anywhere in a session would make
\"p\" unable to paste ordinary text again for the rest of that session.
Confirmed by building that version: after a rectangle copy, copying a
line and pressing \"p\" pasted the rectangle."
  (donkey-test--bank-and-rectangle
   (donkey-test--draw-rectangle 1 2 3)
   (donkey-copy 1)
   (deactivate-mark)
   (should killed-rectangle)
   ;; An ordinary copy afterwards is what "p" must honor.
   (donkey-test--row 3)
   (donkey-visual-line-toggle)
   (donkey-copy 1)
   (donkey-test--row 4)
   (end-of-line)
   (donkey-yank 1)
   (should (string-match-p "CCC three" (buffer-string)))
   (should-not (string-match-p "AAA\nBBB" (buffer-string)))))

(ert-deftest donkey-yank-rectangle-pastes-from-the-rectangle-store ()
  "`P' pastes `killed-rectangle', at point, as a block."
  (donkey-test--bank-and-rectangle
   (donkey-test--draw-rectangle 1 2 3)
   (donkey-copy 1)
   (deactivate-mark)
   (goto-char (point-max))
   (donkey-yank-rectangle 1)
   (should (equal (buffer-string)
                  "AAA one\nBBB two\nCCC three\nDDD four\nAAA\nBBB"))))

(ert-deftest donkey-yank-rectangle-reports-when-there-is-nothing-to-paste ()
  "`P' with an empty `killed-rectangle' reports instead of signaling.

Reaching for a paste with nothing to paste is an ordinary mistake, and
`donkey-yank' answers it the same way."
  (let (messages)
    (donkey-test--bank-and-rectangle
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
       (should-not killed-rectangle)
       (donkey-yank-rectangle 1))
     (should (equal (buffer-string) "AAA one\nBBB two\nCCC three\nDDD four\n")))
    (should (member "No rectangle to paste" messages))))

(ert-deftest donkey-yank-rectangle-deletes-an-ordinary-region-first ()
  "`P' over a plain region replaces it, as any paste over a selection does."
  (donkey-test--bank-and-rectangle
   (donkey-test--draw-rectangle 1 2 3)
   (donkey-copy 1)
   (deactivate-mark)
   (donkey-test--row 4)
   (push-mark (line-end-position) t t)
   (donkey-yank-rectangle 1)
   (should-not (string-match-p "DDD four" (buffer-string)))))

(ert-deftest donkey-yank-rectangle-over-a-live-rectangle-replaces-it ()
  "`P' inside `rectangle-mark-mode' delegates to the row-checked replace.

Not the at-point path: replacing a drawn rectangle has to refuse a
mismatched row count rather than paste half of it."
  (let (replace-called)
    (with-temp-buffer
      (insert "hello\n")
      (goto-char 1)
      (push-mark 3)
      (let ((killed-rectangle '("xx")))
        (cl-letf (((symbol-function
                    'donkey--replace-rectangle-selection-with-killed-rectangle)
                   (lambda () (setq replace-called t))))
          (let ((rectangle-mark-mode t))
            (donkey-yank-rectangle 1)))))
    (should replace-called)))

(ert-deftest donkey-yank-in-rectangle-mode-falls-through-to-undefined ()
  "A live rectangle is never deleted and then pasted back linearly.

Regression test: `donkey--delete-active-region-safe' correctly deletes
the whole rectangle (via `region-extract-function', which rect.el
advises to respect `rectangle-mark-mode'), but that deletion deactivates
the mark, which auto-disables `rectangle-mark-mode' via its own hook --
so a plain linear yank immediately after would land on only one row,
silently leaving every other row of the just-deleted rectangle with
nothing to replace it.  Must call `undefined' instead, same as
`donkey-wrap-region' does.  Pasting over a rectangle selection is
\\[donkey-yank-rectangle]'s job.

The deletion and the yank are both asserted absent, not just the
fall-through: reaching `undefined' matters less than not having eaten
the rectangle on the way there."
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
          (donkey-yank 1))))
    (should (eq called-cmd 'undefined))
    (should-not deleted)
    (should-not yanked)))

(ert-deftest donkey-yank-rectangle-count-widens-rather-than-stacking ()
  "A count on `P' repeats each ROW, giving a wider block.

Calling `yank-rectangle' N times pastes the second block wherever the
first left point -- partway down and across it -- so two copies of a
three-row block came out as a staircase."
  (donkey-test--bank-and-rectangle
   (donkey-test--draw-rectangle 1 2 3)
   (donkey-copy 1)
   (deactivate-mark)
   (goto-char (point-max))
   (donkey-yank-rectangle 2)
   (should (equal (buffer-string)
                  "AAA one\nBBB two\nCCC three\nDDD four\nAAAAAA\nBBBBBB"))))

(ert-deftest donkey-yank-rectangle-count-below-one-inserts-nothing ()
  "A count below 1 on `P' inserts nothing, as it does on `p'.

Zero and negative counts do nothing across the editing commands, and
the two paste keys have no reason to differ.  Pinned because the
docstring now says so: `donkey--yank-rectangle-times' takes the count
apart itself, so this is not inherited from anything that would fail
loudly if it changed."
  (dolist (count '(0 -1 -2))
    (donkey-test--bank-and-rectangle
     (donkey-test--draw-rectangle 1 2 3)
     (donkey-copy 1)
     (deactivate-mark)
     (goto-char (point-max))
     (let ((before (buffer-string)))
       (donkey-yank-rectangle count)
       (should (equal (buffer-string) before))))))

(ert-deftest donkey-yank-rectangle-leaves-banked-lines-alone ()
  "`P' does not treat banked lines as a selection; `p' does.

The asymmetry is deliberate and the README states it: linear text can
stand in for whole lines, so `p' replaces them, while a block of
columns cannot, so `P' lands at point and the bank keeps waiting.

Pinned because the natural reading of \"banked lines are a selection,
and a paste replaces a selection\" is that BOTH paste keys consume
them.  If `P' is ever made to spend the bank, this fails and the
README and the `donkey-yank-rectangle' docstring have to move with it."
  (donkey-test--bank-and-rectangle
   (donkey-test--draw-rectangle 1 2 3)
   (donkey-copy 1)
   (deactivate-mark)
   (donkey-test--row 3)
   (donkey-bank-selection)
   (should (= 1 (length (donkey-banked-spans))))
   (goto-char (point-max))
   (donkey-yank-rectangle 1)
   ;; The banked line is still there, still banked, and the block landed
   ;; where point was rather than on top of it.
   (should (string-match-p "CCC three" (buffer-string)))
   (should (= 1 (length (donkey-banked-spans))))
   (should (string-suffix-p "AAA\nBBB" (buffer-string)))))

(ert-deftest donkey-yank-takes-the-bank-and-leaves-the-rectangle-alone ()
  "`p' replaces banked lines, and the rectangle store is untouched.

The live-selection rule still applies to taking: banks are on screen,
`killed-rectangle' is not.  What changed is that the rectangle is no
longer competing for this key at all."
  (donkey-test--bank-and-rectangle
   (donkey-test--draw-rectangle 1 2 3)
   (donkey-copy 1)
   (deactivate-mark)
   (donkey-test--row 3)
   (donkey-visual-line-toggle)
   (donkey-copy 1)
   (deactivate-mark)
   (donkey-test--row 4)
   (donkey-bank-selection)
   (donkey-yank 1)
   (should (= (length (donkey--banked-spans)) 0))
   (should (equal killed-rectangle '("AAA" "BBB")))))

;;; --- regression guards for the environment-dependence this split removed ---

(ert-deftest donkey-paste-keys-agree-with-and-without-a-clipboard ()
  "The same keys give the same buffer whether or not a clipboard exists.

THE regression guard for this design.  \"p\" used to decide between the
kill ring and `killed-rectangle' by tracking which had been written more
recently, in a flag maintained by advice on `kill-new'.  But
`current-kill' calls `kill-new' to import the system clipboard, so a
PASTE reached that advice and retired the pending rectangle -- in
graphical sessions only.  The identical key sequence produced different
buffers on a GUI and in a terminal, and the tutor could not state which.

Asserted as equality between the two runs rather than against a literal,
so it keeps meaning what it means if the fixture text ever changes."
  (let (results)
    (dolist (clipboard '(nil t))
      (donkey-test--with-clipboard clipboard
        (donkey-test--bank-and-rectangle
         (donkey-test--row 3)
         (donkey-visual-line-toggle)
         (donkey-copy 1)
         (deactivate-mark)
         (donkey-test--row 1)
         (donkey-bank-selection)
         (donkey-test--draw-rectangle 2 3 3)
         (donkey-copy 1)
         (deactivate-mark)
         (donkey-yank 1)
         (goto-char (point-max))
         (donkey-yank-rectangle 1)
         (push (buffer-string) results))))
    (should (= 2 (length results)))
    (should (equal (nth 0 results) (nth 1 results)))))

(ert-deftest donkey-rectangle-survives-any-number-of-ordinary-kills ()
  "A copied rectangle stays pasteable no matter what else is killed.

Under the flag, every `kill-new' retired it: an ordinary copy, a paste
that imported the clipboard, or any package calling `kill-new' at all.
`killed-rectangle' is its own store and nothing but another rectangle
command writes it, so \"P\" keeps working."
  (donkey-test--with-clipboard t
    (donkey-test--bank-and-rectangle
     (donkey-test--draw-rectangle 1 2 3)
     (donkey-copy 1)
     (deactivate-mark)
     ;; Plenty of unrelated kill-ring traffic.
     (dotimes (_ 3)
       (donkey-test--row 3)
       (donkey-visual-line-toggle)
       (donkey-copy 1)
       (deactivate-mark)
       (goto-char (point-max))
       (donkey-yank 1))
     (goto-char (point-max))
     (donkey-yank-rectangle 1)
     (should (string-match-p "AAA\nBBB" (buffer-string))))))

;;; donkey-editing-test.el ends here

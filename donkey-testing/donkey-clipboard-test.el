;;; donkey-clipboard-test.el --- Tests for clipboard and yank functionality  -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;;; ---------------------------------------------------------------------------
;;; Clipboard Tool Detection: donkey--detect-clipboard-tools
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-detect-clipboard-tools-returns-t-on-darwin ()
  "Test `donkey--detect-clipboard-tools' returns t on macOS.
Expected: t when system-type is darwin, regardless of external tools."
  (cl-letf (((symbol-function 'system-type) 'darwin))
    (should (eq (donkey--detect-clipboard-tools) t))))

(ert-deftest donkey-detect-clipboard-tools-returns-t-on-windows ()
  "Test `donkey--detect-clipboard-tools' returns t on Windows.
Expected: t when system-type is windows-nt, regardless of external tools."
  (cl-letf (((symbol-function 'system-type) 'windows-nt))
    (should (eq (donkey--detect-clipboard-tools) t))))

(ert-deftest donkey-detect-clipboard-tools-returns-t-when-wl-copy-found ()
  "Test `donkey--detect-clipboard-tools' returns t when wl-copy is found.
Expected: t on Linux when wl-copy executable exists."
  (let ((wl-exists (executable-find "wl-copy")))
    (if wl-exists
        (should (eq (donkey--detect-clipboard-tools) t))
      (ert-skip "wl-copy not available for testing"))))

(ert-deftest donkey-detect-clipboard-tools-returns-t-when-xclip-found ()
  "Test `donkey--detect-clipboard-tools' returns t when xclip is found.
Expected: t on Linux when xclip executable exists and wl-copy is absent."
  (let ((xclip-exists (executable-find "xclip"))
        (wl-exists (executable-find "wl-copy")))
    (if (and xclip-exists (not wl-exists))
        (should (eq (donkey--detect-clipboard-tools) t))
      (ert-skip "Cannot isolate xclip-only test environment"))))

(ert-deftest donkey-detect-clipboard-tools-returns-t-when-xsel-found ()
  "Test `donkey--detect-clipboard-tools' returns t when xsel is found.
Expected: t on Linux when xsel executable exists and wl-copy/xclip absent."
  (let ((xsel-exists (executable-find "xsel"))
        (wl-exists (executable-find "wl-copy"))
        (xclip-exists (executable-find "xclip")))
    (if (and xsel-exists (not wl-exists) (not xclip-exists))
        (should (eq (donkey--detect-clipboard-tools) t))
      (ert-skip "Cannot isolate xsel-only test environment"))))

(ert-deftest donkey-detect-clipboard-tools-returns-t-in-gui-mode ()
  "Test `donkey--detect-clipboard-tools' returns t in GUI mode.
Expected: t when display-graphic-p returns non-nil, regardless of tools."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
            ((symbol-function 'display-graphic-p) (lambda () t))
            ((symbol-function 'system-type) 'gnu/linux))
    (should (eq (donkey--detect-clipboard-tools) t))))

(ert-deftest donkey-detect-clipboard-tools-returns-nil-when-no-tools-and-terminal ()
  "Test `donkey--detect-clipboard-tools' returns nil when no tools found
in terminal mode on Linux.
Expected: nil when no executables found and not in GUI mode."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
            ((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'system-type) 'gnu/linux))
    (should (null (donkey--detect-clipboard-tools)))))

;;; ---------------------------------------------------------------------------
;;; Clipboard Yank: donkey--clipboard-yank
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-clipboard-yank-uses-clipboard-yank-when-available ()
  "Test `donkey--clipboard-yank' calls clipboard-yank when fboundp.
Expected: clipboard-yank is called, kill-ring yank is not called."
  (let ((clipboard-called nil)
        (yank-called nil))
    (cl-letf (((symbol-function 'clipboard-yank)
               (lambda () (setq clipboard-called t)))
              ((symbol-function 'yank)
               (lambda () (setq yank-called t))))
      (donkey--clipboard-yank)
      (should clipboard-called)
      (should-not yank-called))))

(ert-deftest donkey-clipboard-yank-falls-back-to-yank-when-clipboard-undefined ()
  "Test `donkey--clipboard-yank' falls back to yank when clipboard-yank
is not fboundp.
Expected: yank is called, clipboard-yank is not called."
  (let ((yank-called nil))
    (cl-letf (((symbol-function 'clipboard-yank) nil)
              ((symbol-function 'yank)
               (lambda () (setq yank-called t)))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (not (eq sym 'clipboard-yank)))))
      (donkey--clipboard-yank)
      (should yank-called))))

(ert-deftest donkey-clipboard-yank-falls-back-to-yank-on-clipboard-error ()
  "Test `donkey--clipboard-yank' falls back to yank when clipboard-yank
signals an error.
Expected: yank is called as fallback when clipboard-yank errors."
  (let ((yank-called nil))
    (cl-letf (((symbol-function 'clipboard-yank)
               (lambda () (signal 'error "Clipboard inaccessible")))
              ((symbol-function 'yank)
               (lambda () (setq yank-called t))))
      (donkey--clipboard-yank)
      (should yank-called))))

;;; ---------------------------------------------------------------------------
;;; Region Safe Deletion: donkey--delete-active-region-safe
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-delete-active-region-safe-noop-when-no-region ()
  "Test `donkey--delete-active-region-safe' does nothing when no region
is active.
Expected: no error, no side effects when use-region-p returns nil."
  (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
    (should-not (donkey--delete-active-region-safe))))

(ert-deftest donkey-delete-active-region-safe-calls-kill-active-region-when-available ()
  "Test `donkey--delete-active-region-safe' prefers kill-active-region
on Emacs 29+.
Expected: kill-active-region called, delete-active-region not called."
  (let ((kill-called nil)
        (delete-called nil))
    (cl-letf (((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'kill-active-region)
               (lambda () (setq kill-called t)))
              ((symbol-function 'delete-active-region)
               (lambda () (setq delete-called t))))
      (donkey--delete-active-region-safe)
      (should kill-called)
      (should-not delete-called))))

(ert-deftest donkey-delete-active-region-safe-falls-back-to-delete-active-region ()
  "Test `donkey--delete-active-region-safe' falls back to
delete-active-region when kill-active-region is not fboundp.
Expected: delete-active-region called, kill-active-region not called."
  (let ((kill-called nil)
        (delete-called nil))
    (cl-letf (((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'kill-active-region) nil)
              ((symbol-function 'delete-active-region)
               (lambda () (setq delete-called t)))
              ((symbol-function 'fboundp)
               (lambda (sym)
                 (not (eq sym 'kill-active-region)))))
      (donkey--delete-active-region-safe)
      (should delete-called)
      (should-not kill-called))))

;;; ---------------------------------------------------------------------------
;;; Yank Commands: donkey-yank, donkey-yank-pop
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-yank-deletes-region-then-yanks ()
  "Test `donkey-yank' deletes active region before yanking.
Expected: delete-active-region-safe called, then clipboard-yank called."
  (let ((delete-called nil)
        (yank-called nil))
    (cl-letf (((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'kill-active-region)
               (lambda () (setq delete-called t)))
              ((symbol-function 'clipboard-yank)
               (lambda () (setq yank-called t))))
      (donkey-yank)
      (should delete-called)
      (should yank-called))))

(ert-deftest donkey-yank-yanks-without-region ()
  "Test `donkey-yank' yanks directly when no region is active.
Expected: clipboard-yank called, no region deletion."
  (let ((delete-called nil)
        (yank-called nil))
    (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
              ((symbol-function 'clipboard-yank)
               (lambda () (setq yank-called t))))
      (donkey-yank)
      (should-not delete-called)
      (should yank-called))))

(ert-deftest donkey-yank-pop-deletes-region-then-pops ()
  "Test `donkey-yank-pop' deletes active region before yank-pop.
Expected: region deleted, then yank-pop called."
  (let ((delete-called nil)
        (pop-called nil))
    (cl-letf (((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'kill-active-region)
               (lambda () (setq delete-called t)))
              ((symbol-function 'yank-pop)
               (lambda () (setq pop-called t))))
      (donkey-yank-pop)
      (should delete-called)
      (should pop-called))))

(ert-deftest donkey-yank-pop-pops-without-region ()
  "Test `donkey-yank-pop' pops directly when no region is active.
Expected: yank-pop called, no region deletion."
  (let ((pop-called nil))
    (cl-letf (((symbol-function 'use-region-p) (lambda () nil))
              ((symbol-function 'yank-pop)
               (lambda () (setq pop-called t))))
      (donkey-yank-pop)
      (should pop-called))))

;;; ---------------------------------------------------------------------------
;;; Warning Guard: donkey--clipboard-warning-shown
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-clipboard-warning-shown-starts-nil ()
  "Test `donkey--clipboard-warning-shown' starts nil at session start.
Expected: nil before any yank operation triggers the warning."
  (should (null donkey--clipboard-warning-shown)))

;;; donkey-clipboard-test.el ends here

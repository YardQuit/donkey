;;; donkey-cursor-clipboard-test.el --- Tests for DONKEY cursor, clipboard, and platform diagnostics -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;;; ---------------------------------------------------------------------------
;;; donkey--cursor-type-to-decscusr
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cursor-type-to-decscusr-box ()
  "Box cursor maps to DECSCUSR steady block sequence."
  (should (string= (donkey--cursor-type-to-decscusr 'box) "\e[2 q")))

(ert-deftest donkey-cursor-type-to-decscusr-hollow ()
  "Hollow cursor maps to DECSCUSR blinking block sequence."
  (should (string= (donkey--cursor-type-to-decscusr 'hollow) "\e[0 q")))

(ert-deftest donkey-cursor-type-to-decscusr-bar ()
  "Bar cursor maps to DECSCUSR steady bar sequence."
  (should (string= (donkey--cursor-type-to-decscusr 'bar) "\e[6 q")))

(ert-deftest donkey-cursor-type-to-decscusr-bar-with-width ()
  "(bar . N) cursor maps to DECSCUSR steady bar sequence, width ignored."
  (should (string= (donkey--cursor-type-to-decscusr '(bar . 2)) "\e[6 q")))

(ert-deftest donkey-cursor-type-to-decscusr-hbar ()
  "(hbar . N) cursor maps to DECSCUSR steady underline."
  (should (string= (donkey--cursor-type-to-decscusr '(hbar . 2)) "\e[4 q")))

(ert-deftest donkey-cursor-type-to-decscusr-unknown-type-fallback ()
  "Unknown cursor type maps to DECSCUSR default sequence."
  (should (string= (donkey--cursor-type-to-decscusr 'unknown-shape) "\e[0 q")))

;;; ---------------------------------------------------------------------------
;;; donkey--terminal-supports-decscusr-p
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-terminal-supports-decscusr-p-returns-nil-in-gui ()
  "Nil when display-graphic-p returns t, even if terminal type would
otherwise qualify."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () t)))
    (should (null (donkey--terminal-supports-decscusr-p)))))

(ert-deftest donkey-terminal-supports-decscusr-p-returns-nil-when-tty-type-is-dumb ()
  "Nil when tty-type returns 'dumb'."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'tty-type) (lambda () "dumb")))
    (should (null (donkey--terminal-supports-decscusr-p)))))

(ert-deftest donkey-terminal-supports-decscusr-p-returns-nil-when-tty-type-is-linux ()
  "Nil for Linux framebuffer console."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'tty-type) (lambda () "linux")))
    (should (null (donkey--terminal-supports-decscusr-p)))))

(ert-deftest donkey-terminal-supports-decscusr-p-returns-nil-for-cons25 ()
  "Nil for cons25 terminals."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'tty-type) (lambda () "cons25")))
    (should (null (donkey--terminal-supports-decscusr-p)))))

(ert-deftest donkey-terminal-supports-decscusr-p-returns-t-for-xterm ()
  "Non-nil for xterm-256color."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'tty-type) (lambda () "xterm-256color")))
    (should (donkey--terminal-supports-decscusr-p))))

(ert-deftest donkey-terminal-supports-decscusr-p-falls-back-to-TERM-env ()
  "Uses TERM env var when tty-type returns nil."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'tty-type) (lambda () nil))
            ((symbol-function 'getenv) (lambda (var) "xterm-256color")))
    (should (donkey--terminal-supports-decscusr-p))))

(ert-deftest donkey-terminal-supports-decscusr-p-returns-nil-when-both-tty-and-term-nil ()
  "Nil when no terminal type can be determined at all."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'tty-type) (lambda () nil))
            ((symbol-function 'getenv) (lambda (var) nil)))
    (should (null (donkey--terminal-supports-decscusr-p)))))

(ert-deftest donkey-terminal-supports-decscusr-p-accepts-terms-that-contain-denied-prefix ()
  "Allows terminal types that contain (but don't start with) a denied prefix."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'tty-type) (lambda () "xterm-dumb")))
    (should (donkey--terminal-supports-decscusr-p))))

;;; ---------------------------------------------------------------------------
;;; donkey--send-cursor-sequence
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-send-cursor-sequence-noop-in-gui ()
  "No-op in GUI mode."
  (let ((send-called nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () t))
              ((symbol-function 'send-string-to-terminal)
               (lambda (&rest _) (setq send-called t))))
      (donkey--send-cursor-sequence 'box)
      (should-not send-called))))

(ert-deftest donkey-send-cursor-sequence-sends-in-supported-terminal ()
  "Sends sequence twice (double-send for reliability) in a supported terminal."
  (let ((send-count 0))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
              ((symbol-function 'tty-type) (lambda () "xterm-256color"))
              ((symbol-function 'send-string-to-terminal)
               (lambda (&rest _) (cl-incf send-count)))
              ((symbol-function 'sit-for) (lambda (&rest _) t)))
      (donkey--send-cursor-sequence 'box)
      (should (= send-count 2)))))

(ert-deftest donkey-send-cursor-sequence-suppressed-for-denied-terminal ()
  "Suppressed for denied ('dumb') terminals."
  (let ((send-called nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
              ((symbol-function 'tty-type) (lambda () "dumb"))
              ((symbol-function 'send-string-to-terminal)
               (lambda (&rest _) (setq send-called t))))
      (donkey--send-cursor-sequence 'box)
      (should-not send-called))))

(ert-deftest donkey-send-cursor-sequence-swallows-io-errors ()
  "Silently absorbs I/O errors from send-string-to-terminal."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
            ((symbol-function 'tty-type) (lambda () "xterm-256color"))
            ((symbol-function 'send-string-to-terminal)
             (lambda (&rest _) (signal 'file-error "I/O failure")))
            ((symbol-function 'sit-for) (lambda (&rest _) t)))
    (should-not (donkey--send-cursor-sequence 'box))))

;;; ---------------------------------------------------------------------------
;;; donkey--apply-cursor-setting
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-apply-cursor-setting-sets-local-when-non-nil ()
  "Sets cursor-type buffer-local when given a non-nil setting."
  (with-temp-buffer
    (donkey--apply-cursor-setting 'bar)
    (should (local-variable-p 'cursor-type))
    (should (eq cursor-type 'bar))))

(ert-deftest donkey-apply-cursor-setting-kills-local-when-nil ()
  "Kills local cursor-type when given a nil setting."
  (with-temp-buffer
    (setq-local cursor-type 'bar)
    (should (local-variable-p 'cursor-type))
    (donkey--apply-cursor-setting nil)
    (should-not (local-variable-p 'cursor-type))))

(ert-deftest donkey-apply-cursor-setting-sends-decscusr-in-terminal ()
  "Sends DECSCUSR in terminal mode."
  (let ((send-called nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
              ((symbol-function 'tty-type) (lambda () "xterm-256color"))
              ((symbol-function 'send-string-to-terminal)
               (lambda (&rest _) (setq send-called t)))
              ((symbol-function 'sit-for) (lambda (&rest _) t)))
      (with-temp-buffer
        (donkey--apply-cursor-setting 'bar))
      (should send-called))))

;;; ---------------------------------------------------------------------------
;;; donkey--update-cursor
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-update-cursor-applies-normal-settings ()
  "Applies normal mode cursor settings."
  (let ((original-value donkey-cursor-normal))
    (unwind-protect
        (progn
          (setq donkey-cursor-normal 'hollow)
          (donkey-enter-normal)
          (should (or (eq cursor-type 'hollow)
                      (not (local-variable-p 'cursor-type)))))
      (setq donkey-cursor-normal original-value))))

(ert-deftest donkey-update-cursor-applies-insert-settings ()
  "Applies insert mode cursor settings."
  (let ((original-value donkey-cursor-insert))
    (unwind-protect
        (progn
          (setq donkey-cursor-insert 'box)
          (donkey-enter-insert)
          (should (or (eq cursor-type 'box)
                      (not (local-variable-p 'cursor-type)))))
      (setq donkey-cursor-insert original-value))))

;;; ---------------------------------------------------------------------------
;;; donkey--decscusr-denied-terminals / donkey--add-denylist-entry /
;;; donkey--remove-denylist-entry
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-decscusr-denied-terminals-default-contains-dumb-and-linux ()
  "Default entries include 'dumb' and 'linux'."
  (should (member "dumb" donkey--decscusr-denied-terminals))
  (should (member "linux" donkey--decscusr-denied-terminals)))

(ert-deftest donkey-add-denylist-entry-adds-new-prefix ()
  "Adds a new prefix to the denylist and persists via customize."
  (let ((donkey--decscusr-denied-terminals '("dumb" "linux"))
        (saved-value nil))
    (cl-letf (((symbol-function 'customize-set-variable)
               (lambda (sym val) (set sym val)))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val) (setq saved-value val))))
      (donkey--add-denylist-entry "vt100"))
    (should (member "vt100" donkey--decscusr-denied-terminals))
    (should (member "vt100" saved-value))))

(ert-deftest donkey-add-denylist-entry-skips-duplicate ()
  "Does not duplicate an already-present prefix."
  (let ((donkey--decscusr-denied-terminals '("dumb" "linux"))
        (save-called nil))
    (cl-letf (((symbol-function 'customize-set-variable)
               (lambda (sym val) (set sym val)))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val) (setq save-called t))))
      (donkey--add-denylist-entry "dumb"))
    (should-not save-called)
    (should (equal donkey--decscusr-denied-terminals '("dumb" "linux")))))

(ert-deftest donkey-remove-denylist-entry-removes-existing-prefix ()
  "Removes an existing prefix from the denylist and persists via customize."
  (let ((donkey--decscusr-denied-terminals '("dumb" "linux" "vt100"))
        (saved-value nil))
    (cl-letf (((symbol-function 'customize-set-variable)
               (lambda (sym val) (set sym val)))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val) (setq saved-value val))))
      (donkey--remove-denylist-entry "vt100"))
    (should-not (member "vt100" donkey--decscusr-denied-terminals))
    (should-not (member "vt100" saved-value))))

(ert-deftest donkey-remove-denylist-entry-skips-absent-prefix ()
  "Does nothing when the prefix isn't present in the denylist."
  (let ((donkey--decscusr-denied-terminals '("dumb" "linux"))
        (save-called nil))
    (cl-letf (((symbol-function 'customize-set-variable)
               (lambda (sym val) (set sym val)))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val) (setq save-called t))))
      (donkey--remove-denylist-entry "vt100"))
    (should-not save-called)
    (should (equal donkey--decscusr-denied-terminals '("dumb" "linux")))))

;;; ---------------------------------------------------------------------------
;;; donkey--detect-clipboard-tools
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-detect-clipboard-tools-returns-t-on-darwin ()
  "t on macOS, regardless of external tools.
`system-type' is a variable, not a function -- it must be let-bound,
not mocked via `symbol-function', or the test silently checks the
real platform instead of the intended one."
  (let ((system-type 'darwin))
    (should (eq (donkey--detect-clipboard-tools) t))))

(ert-deftest donkey-detect-clipboard-tools-returns-t-on-windows ()
  "t on Windows, regardless of external tools."
  (let ((system-type 'windows-nt))
    (should (eq (donkey--detect-clipboard-tools) t))))

(ert-deftest donkey-detect-clipboard-tools-returns-t-in-gui-mode ()
  "t in GUI mode, regardless of tools found."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'display-graphic-p) (lambda () t)))
      (should (eq (donkey--detect-clipboard-tools) t)))))

(ert-deftest donkey-detect-clipboard-tools-returns-nil-when-no-tools-and-terminal ()
  "Nil when no executables found and not in GUI mode."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'display-graphic-p) (lambda () nil)))
      (should (null (donkey--detect-clipboard-tools))))))

(ert-deftest donkey-detect-clipboard-tools-returns-t-when-wl-copy-found ()
  "t when wl-copy is found on the search path."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (equal name "wl-copy")))
              ((symbol-function 'display-graphic-p) (lambda () nil)))
      (should (eq (donkey--detect-clipboard-tools) t)))))

;;; ---------------------------------------------------------------------------
;;; donkey--clipboard-yank
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-clipboard-yank-uses-clipboard-yank-when-available ()
  "Calls clipboard-yank when fboundp; kill-ring yank is not called."
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
  "Falls back to yank when clipboard-yank is not fboundp."
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
  "Falls back to yank when clipboard-yank signals an error."
  (let ((yank-called nil))
    (cl-letf (((symbol-function 'clipboard-yank)
               (lambda () (signal 'error "Clipboard inaccessible")))
              ((symbol-function 'yank)
               (lambda () (setq yank-called t))))
      (donkey--clipboard-yank)
      (should yank-called))))

;;; ---------------------------------------------------------------------------
;;; donkey--delete-active-region-safe
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-delete-active-region-safe-noop-when-no-region ()
  "Does nothing when no region is active."
  (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
    (should-not (donkey--delete-active-region-safe))))

(ert-deftest donkey-delete-active-region-safe-calls-kill-active-region-when-available ()
  "Prefers kill-active-region on Emacs 29+."
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
  "Falls back to delete-active-region when kill-active-region is not fboundp."
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
;;; donkey-yank / donkey-yank-pop (clipboard-layer coverage)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-yank-deletes-region-then-yanks ()
  "Deletes active region before yanking."
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

(ert-deftest donkey-yank-pop-deletes-region-then-pops ()
  "Deletes active region before yank-pop."
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

;;; ---------------------------------------------------------------------------
;;; donkey--clipboard-warning-shown
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-clipboard-warning-shown-starts-nil ()
  "Starts nil at session start."
  (should (null donkey--clipboard-warning-shown)))

;;; ---------------------------------------------------------------------------
;;; donkey--platform-info
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-platform-info-returns-expected-keys ()
  "Returns a plist with all documented diagnostic keys."
  (let ((info (donkey--platform-info)))
    (should (plist-member info :system-type))
    (should (plist-member info :display-type))
    (should (plist-member info :tty-type))
    (should (plist-member info :term-env))
    (should (plist-member info :clipboard-tools-available))
    (should (plist-member info :native-comp))
    (should (plist-member info :emacs-version))))

(ert-deftest donkey-platform-info-display-type-gui ()
  "Reports 'gui when display-graphic-p is non-nil."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () t)))
    (should (eq (plist-get (donkey--platform-info) :display-type) 'gui))))

(ert-deftest donkey-platform-info-display-type-terminal ()
  "Reports 'terminal when display-graphic-p is nil."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
    (should (eq (plist-get (donkey--platform-info) :display-type) 'terminal))))

(ert-deftest donkey-platform-info-reflects-clipboard-tools-available ()
  "Reflects the current value of donkey--clipboard-tools-available."
  (let ((donkey--clipboard-tools-available 'sentinel-value))
    (should (eq (plist-get (donkey--platform-info) :clipboard-tools-available)
                'sentinel-value))))

;;; ---------------------------------------------------------------------------
;;; donkey-debug-platform
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-debug-platform-creates-buffer-without-error ()
  "Creates the *DONKEY Platform Debug* buffer without signaling an error."
  (when (get-buffer "*DONKEY Platform Debug*")
    (kill-buffer "*DONKEY Platform Debug*"))
  (unwind-protect
      (progn
        (donkey-debug-platform)
        (should (get-buffer "*DONKEY Platform Debug*")))
    (when (get-buffer "*DONKEY Platform Debug*")
      (kill-buffer "*DONKEY Platform Debug*"))))

(ert-deftest donkey-debug-platform-buffer-is-read-only-special-mode ()
  "The debug buffer is read-only and uses special-mode with a 'q' binding."
  (when (get-buffer "*DONKEY Platform Debug*")
    (kill-buffer "*DONKEY Platform Debug*"))
  (unwind-protect
      (progn
        (donkey-debug-platform)
        (with-current-buffer "*DONKEY Platform Debug*"
          (should buffer-read-only)
          (should (eq major-mode 'special-mode))
          (should (eq (key-binding "q") #'quit-window))))
    (when (get-buffer "*DONKEY Platform Debug*")
      (kill-buffer "*DONKEY Platform Debug*"))))

(ert-deftest donkey-debug-platform-includes-system-info ()
  "The debug buffer mentions the running Emacs version."
  (when (get-buffer "*DONKEY Platform Debug*")
    (kill-buffer "*DONKEY Platform Debug*"))
  (unwind-protect
      (progn
        (donkey-debug-platform)
        (with-current-buffer "*DONKEY Platform Debug*"
          (goto-char (point-min))
          (should (search-forward emacs-version nil t))))
    (when (get-buffer "*DONKEY Platform Debug*")
      (kill-buffer "*DONKEY Platform Debug*"))))

(provide 'donkey-cursor-clipboard-test)

;;; donkey-cursor-clipboard-test.el ends here

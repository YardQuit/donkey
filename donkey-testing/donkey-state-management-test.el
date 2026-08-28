;;; donkey-state-management-test.el --- Tests for DONKEY Normal/Insert state management -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;; Declare donkey-specific variables so let-bindings are dynamic
(defvar donkey-normal-mode)
(defvar donkey-insert-mode)
(defvar donkey--saved-input-method)

(defvar-local donkey--just-exited-from-insert nil)
(defvar-local donkey--deferred-overlay-cleanup-timer nil)

(defvar this-command)
(defvar this-original-command)
(defvar last-command-event)

;;; ---------------------------------------------------------------------------
;;; Helper macros for integration-style C-g simulation
;;; ---------------------------------------------------------------------------

(defmacro donkey--with-test-buffer (&rest body)
  "Evaluate BODY in a fresh DONKEY-enabled buffer with point at the start.

The buffer is created in `fundamental-mode'.

`donkey-mode' is a GLOBAL minor mode: turning it on for the duration
of BODY and never back off would leak into every other test that
happens to run afterward in the same batch Emacs process, regardless
of file or test order -- e.g. a later test creating an unrelated
buffer would find `donkey-mode' already active and get an unexpected
Normal/Insert state applied to it.  Wrapped in `unwind-protect' so
`donkey-mode' is always turned back off once BODY completes, whether
normally or via a signalled error, leaving no trace for whichever test
runs next."
  (declare (indent 0))
  `(unwind-protect
       (with-temp-buffer
         (fundamental-mode)
         (donkey-mode -1)
         (donkey-mode 1)
         (donkey--ensure-default-state)
         (donkey-enter-insert)
         (insert "(defun foo ()\n  (let ((x 1))\n    (concat \"bar\" x)))")
         (goto-char (point-min))
         ,@body)
     (donkey-mode -1)))

(ert-deftest donkey--with-test-buffer-turns-donkey-mode-back-off ()
  "The test macro turns `donkey-mode' back off.

Regression test: `donkey--with-test-buffer' must leave `donkey-mode'
off afterward, whether BODY completes normally or signals an error.

Before this macro wrapped its body in `unwind-protect', `donkey-mode'
\(a GLOBAL minor mode\) stayed on for the rest of the batch Emacs
process once any of the 31+ tests using this macro ran -- confirmed
via a full suite run where `donkey-mode' was still non-nil after
`ert-run-tests-batch' completed, regardless of file or test order."
  (donkey-mode -1)
  (donkey--with-test-buffer nil)
  (should-not donkey-mode)
  (should-error
   (donkey--with-test-buffer (error "Deliberate test failure")))
  (should-not donkey-mode))

(defun donkey--simulate-cg ()
  "Simulate pressing \\=`C-g\\='.

Mocks `this-single-command-keys' so `pre-command-hook' functions can
see it."
  (let ((this-command nil)
        (this-original-command nil)
        (last-command-event 7))
    (cl-letf (((symbol-function 'this-single-command-keys)
               (lambda () [7])))
      (run-hooks 'pre-command-hook)
      (unless (eq this-command 'ignore)
        (when (and this-command (commandp this-command))
          (call-interactively this-command)))
      (run-hooks 'post-command-hook))))

(defun donkey--simulate-key (key)
  "Simulate pressing KEY for testing guard reset behavior.

`this-command' is set from KEY's own binding.  It used to be inherited
from whatever ran before, and the `call-interactively' below then ran
THAT command instead of this key's -- so the helper did not simulate
the key it was handed.  Harmless in ERT's fixed order, where the
previous command happened to be inert; running the suite shuffled
turned it into \"The mark is not set now, so there is no region\" from
a region command left behind by an unrelated test.

Bound rather than assigned, so the hooks below cannot leak a
`this-command' into the next test either."
  (let* ((last-command-event (aref key 0))
         (this-command (or (key-binding key) this-command))
         (this-original-command this-command))
    (cl-letf (((symbol-function 'this-single-command-keys)
               (lambda () key)))
      (run-hooks 'pre-command-hook)
      (unless (eq this-command 'ignore)
        (when (and this-command (commandp this-command))
          (call-interactively this-command)))
      (run-hooks 'post-command-hook))))

;;; ---------------------------------------------------------------------------
;;; donkey-indicator
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-state-indicator-normal-mode-active ()
  "When donkey-normal-mode is non-nil, returns \" DONKEY[N]\"."
  (with-temp-buffer
    (let ((donkey-normal-mode t)
          (donkey-insert-mode nil))
      (should (equal (donkey-indicator) " DONKEY[N]")))))

(ert-deftest donkey-state-indicator-insert-mode-active ()
  "The Insert-state indicator reads \" DONKEY[I]\".

This holds when donkey-insert-mode is non-nil and donkey-normal-mode is
nil."
  (with-temp-buffer
    (let ((donkey-normal-mode nil)
          (donkey-insert-mode t))
      (should (equal (donkey-indicator) " DONKEY[I]")))))

(ert-deftest donkey-state-indicator-neither-mode-active ()
  "When neither mode is active, returns empty string."
  (with-temp-buffer
    (let ((donkey-normal-mode nil)
          (donkey-insert-mode nil))
      (should (equal (donkey-indicator) "")))))

(ert-deftest donkey-state-indicator-normal-takes-precedence ()
  "Normal mode checked first in cond; if both somehow active, normal wins."
  (with-temp-buffer
    (let ((donkey-normal-mode t)
          (donkey-insert-mode t))
      (should (equal (donkey-indicator) " DONKEY[N]")))))

;;; ---------------------------------------------------------------------------
;;; donkey--minibuffer-current-state
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-minibuffer-current-state-normal ()
  "Returns 'normal when donkey-normal-mode is active."
  (with-temp-buffer
    (let ((donkey-normal-mode t)
          (donkey-insert-mode nil))
      (should (eq (donkey--minibuffer-current-state) 'normal)))))

(ert-deftest donkey-minibuffer-current-state-insert ()
  "Returns 'insert when donkey-insert-mode is active and normal is not."
  (with-temp-buffer
    (let ((donkey-normal-mode nil)
          (donkey-insert-mode t))
      (should (eq (donkey--minibuffer-current-state) 'insert)))))

(ert-deftest donkey-minibuffer-current-state-neither ()
  "Returns nil when neither mode is active."
  (with-temp-buffer
    (let ((donkey-normal-mode nil)
          (donkey-insert-mode nil))
      (should (null (donkey--minibuffer-current-state))))))

(ert-deftest donkey-minibuffer-current-state-normal-takes-precedence ()
  "Returns 'normal when both are somehow active, matching donkey-indicator."
  (with-temp-buffer
    (let ((donkey-normal-mode t)
          (donkey-insert-mode t))
      (should (eq (donkey--minibuffer-current-state) 'normal)))))

;;; ---------------------------------------------------------------------------
;;; donkey--minibuffer-pre-state-stack (nested minibuffer regression)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-minibuffer-nested-activation-restores-outer-state ()
  "Nested minibuffer activations each restore their own outer state.

Nested/recursive minibuffer activations must not clobber each other's
saved state.  Regression test: the pre-state used to be a single global
slot, so an inner minibuffer's setup/exit would overwrite and then clear
the outer minibuffer's saved state, leaving the original buffer's DONKEY
state unrestored once the outer minibuffer finally exited."
  (let ((buf (generate-new-buffer "donkey-nested-minibuf-test"))
        (donkey--minibuffer-pre-state-stack nil)
        (donkey-mode t))
    (unwind-protect
        (with-current-buffer buf
          (fundamental-mode)
          (donkey-normal-mode 1)
          (cl-letf (((symbol-function 'minibuffer-selected-window)
                     (lambda () 'donkey-fake-window))
                    ((symbol-function 'window-buffer)
                     (lambda (_win) buf)))
            ;; Outer minibuffer opens while buf is in Normal state.
            (donkey--minibuffer-setup)
            (should (equal donkey--minibuffer-pre-state-stack
                           (list (cons buf 'normal))))
            (should-not (bound-and-true-p donkey-normal-mode))
            ;; A nested/recursive minibuffer opens on top of the outer one.
            (donkey--minibuffer-setup)
            (should (equal donkey--minibuffer-pre-state-stack
                           (list (cons buf nil) (cons buf 'normal))))
            ;; Inner minibuffer exits first: pops its own entry only.
            (donkey--minibuffer-exit)
            (should (equal donkey--minibuffer-pre-state-stack
                           (list (cons buf 'normal))))
            (should-not (bound-and-true-p donkey-normal-mode))
            ;; Outer minibuffer exits: must still restore Normal for buf.
            (donkey--minibuffer-exit)
            (should (null donkey--minibuffer-pre-state-stack))
            (should (bound-and-true-p donkey-normal-mode))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest donkey-minibuffer-exit-restores-the-buffer-it-saved-from ()
  "Exit restores the buffer whose state was saved, not the window's current one.

Regression test: both hooks used to look the buffer up through
`minibuffer-selected-window' at the time they ran, so a command that
switched that window's buffer from inside the minibuffer had the saved
state applied to whatever buffer the window showed afterwards."
  (let ((orig (generate-new-buffer "donkey-minibuf-orig"))
        (other (generate-new-buffer "donkey-minibuf-other"))
        (donkey--minibuffer-pre-state-stack nil)
        (donkey-mode t)
        (shown nil))
    (unwind-protect
        (progn
          ;; Both start with no state, whatever a globally enabled
          ;; `donkey-mode' left behind through `fundamental-mode's hook.
          (dolist (b (list orig other))
            (with-current-buffer b
              (fundamental-mode)
              (donkey-normal-mode -1)
              (donkey-insert-mode -1)))
          (with-current-buffer orig (donkey-normal-mode 1))
          (setq shown orig)
          (cl-letf (((symbol-function 'minibuffer-selected-window)
                     (lambda () 'donkey-fake-window))
                    ((symbol-function 'window-buffer)
                     (lambda (_win) shown)))
            (donkey--minibuffer-setup)
            (with-current-buffer orig (donkey-normal-mode -1))
            ;; The command run from the minibuffer switched the window.
            (setq shown other)
            (donkey--minibuffer-exit)
            (should (buffer-local-value 'donkey-normal-mode orig))
            (should-not (buffer-local-value 'donkey-normal-mode other))))
      (dolist (b (list orig other))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest donkey-minibuffer-exit-skips-restore-when-donkey-mode-off ()
  "Minibuffer exit skips the restore when `donkey-mode' is off.

It does not resurrect Normal/Insert state on minibuffer exit when
donkey-mode has been globally disabled in the meantime, but still pops
the stack so it does not leak.  Regression test: `donkey-mode' being
disabled mid-minibuffer-session (e.g. via a keybinding, from a
recursive minibuffer) used to leave the exit hook unconditionally
calling `donkey-enter-normal'/`donkey-enter-insert' from the saved
state, resurrecting DONKEY in the originating buffer -- the same bug
class `donkey--exit-insert' has its own `donkey-mode' guard for."
  (let ((buf (generate-new-buffer "donkey-minibuf-mode-off-test"))
        (donkey--minibuffer-pre-state-stack nil)
        (donkey-mode t))
    (unwind-protect
        (with-current-buffer buf
          (fundamental-mode)
          (donkey-normal-mode 1)
          (cl-letf (((symbol-function 'minibuffer-selected-window)
                     (lambda () 'donkey-fake-window))
                    ((symbol-function 'window-buffer)
                     (lambda (_win) buf)))
            (donkey--minibuffer-setup)
            (should (equal donkey--minibuffer-pre-state-stack
                           (list (cons buf 'normal))))
            ;; User disables donkey-mode while the minibuffer is still open.
            (setq donkey-mode nil)
            (donkey--minibuffer-exit)
            (should (null donkey--minibuffer-pre-state-stack))
            (should-not (bound-and-true-p donkey-normal-mode))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; ---------------------------------------------------------------------------
;;; donkey--excluded-mode-p
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-excluded-mode-p-exact-match ()
  "An exactly listed major mode is excluded.

Returns non-nil for a major mode listed exactly in
`donkey-excluded-modes'."
  (with-temp-buffer
    (let ((major-mode 'comint-mode))
      (should (donkey--excluded-mode-p)))))

(ert-deftest donkey-excluded-mode-p-derived-mode ()
  "A mode derived from a listed mode is excluded too.

Returns non-nil for a major mode derived from one listed in
`donkey-excluded-modes' (e.g. `shell-mode' from `comint-mode'), even
though it is not an exact `member' match.  Regression test: two of the
three call sites used to check membership only and would miss this."
  (require 'shell)
  (with-temp-buffer
    (let ((major-mode 'shell-mode))
      (should (donkey--excluded-mode-p)))))

(ert-deftest donkey-excluded-mode-p-not-excluded ()
  "An unrelated major mode is not excluded.

Returns nil for a major mode neither listed in nor derived from one
in `donkey-excluded-modes'."
  (with-temp-buffer
    (let ((major-mode 'text-mode))
      (should-not (donkey--excluded-mode-p)))))

;;; ---------------------------------------------------------------------------
;;; donkey--mode-list (mis-set user options must not reach a command hook)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mode-list-coerces-without-signaling ()
  "`donkey--mode-list' returns a list for anything it is given.

A list passes through, a bare symbol becomes the one-element list it
was meant to be, and a value that can be neither reads as empty.  Nil
is a list already and must stay one, since an empty
`donkey-excluded-modes' is an ordinary configuration rather than a
mistake."
  (should (equal (donkey--mode-list '(dired-mode)) '(dired-mode)))
  (should (equal (donkey--mode-list 'dired-mode) '(dired-mode)))
  (should (equal (donkey--mode-list nil) nil))
  (should (equal (donkey--mode-list "dired-mode") nil))
  (should (equal (donkey--mode-list 42) nil)))

(ert-deftest donkey-excluded-mode-p-survives-a-mis-set-option ()
  "`donkey--excluded-mode-p' never signals, whatever the option holds.

`memq' and `derived-mode-p' both signal on a non-list, and this
predicate is reached from `post-command-hook'."
  (with-temp-buffer
    (let ((major-mode 'text-mode))
      (dolist (val '(dired-mode "dired-mode" 42 nil))
        (let ((donkey-excluded-modes val))
          (should-not (donkey--excluded-mode-p))))
      ;; The bare symbol still has to WORK, not merely fail to signal.
      (let ((donkey-excluded-modes 'text-mode))
        (should (donkey--excluded-mode-p))))))

(ert-deftest donkey-a-mis-set-mode-option-does-not-remove-the-post-command-guard ()
  "A wrong-typed mode option must not cost DONKEY its excluded-buffer guard.

`donkey--check-post-command-non-editing' is the catch-all that
guarantees Normal state can never be active in an excluded buffer, and
it runs on `post-command-hook' -- where Emacs REMOVES a function that
signals, silently, for the rest of the session.  Repairing the variable
afterwards does not bring it back; only toggling `donkey-mode' does.

Regression test: reading `donkey-excluded-modes' straight through
`memq' meant that

  (setq donkey-excluded-modes \\='dired-mode)

-- one missing pair of parentheses -- took the guarantee away on the
NEXT keypress.  Verified by re-introducing the unguarded read: the hook
is gone after a single key.

Driven with real keys through `execute-kbd-macro', because only the
real command loop removes a signaling hook function -- `run-hooks',
which the other integration helpers in this file use, propagates the
error instead and would never show the removal.  The buffer is
switched to rather than merely made current for the same reason
`donkey-tutor-test--live' switches: the command loop acts on the
SELECTED WINDOW's buffer, so keys sent to an undisplayed
`with-temp-buffer' land somewhere else entirely.

Both options that reach `donkey--major-mode-in-p' are covered."
  (dolist (var '(donkey-excluded-modes donkey-editing-modes))
    (let ((orig (symbol-value var))
          (buf (get-buffer-create "*donkey-mode-list-test*")))
      (unwind-protect
          (progn
            (donkey-mode 1)
            (switch-to-buffer buf)
            (fundamental-mode)
            (erase-buffer)
            (insert "alpha beta\n")
            (goto-char (point-min))
            (donkey-enter-normal)
            (should (memq #'donkey--check-post-command-non-editing
                          post-command-hook))
            (set var 'dired-mode)
            (execute-kbd-macro (kbd "l"))
            (should (memq #'donkey--check-post-command-non-editing
                          post-command-hook)))
        (set var orig)
        (when (buffer-live-p buf) (kill-buffer buf))
        (donkey-mode -1)))))

;;; ---------------------------------------------------------------------------
;;; donkey--handle-non-editing-buffer / donkey--check-post-command-non-editing
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-handle-non-editing-buffer-enters-insert-when-excluded ()
  "An excluded buffer in Normal state is forced into Insert state.

This is the `donkey--handle-non-editing-buffer' path."
  (let (entered)
    (with-temp-buffer
      (let ((major-mode 'comint-mode)
            (donkey-normal-mode t))
        (cl-letf (((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (donkey--handle-non-editing-buffer)))
      (should entered))))

(ert-deftest donkey-handle-non-editing-buffer-skips-when-not-excluded ()
  "When major-mode is not excluded, does nothing regardless of state."
  (let (entered)
    (with-temp-buffer
      (let ((major-mode 'fundamental-mode)
            (donkey-normal-mode t))
        (cl-letf (((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (donkey--handle-non-editing-buffer)))
      (should-not entered))))

(ert-deftest donkey-handle-non-editing-buffer-skips-when-normal-mode-inactive ()
  "An excluded buffer already out of Normal state is left alone.

There is nothing to correct."
  (let (entered)
    (with-temp-buffer
      (let ((major-mode 'comint-mode)
            (donkey-normal-mode nil))
        (cl-letf (((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (donkey--handle-non-editing-buffer)))
      (should-not entered))))

(ert-deftest donkey-check-post-command-non-editing-enters-insert-when-excluded ()
  "The post-command check forces Insert state in an excluded mode.

This fires when donkey-normal-mode is active in an excluded major mode."
  (let (entered)
    (with-temp-buffer
      (let ((major-mode 'term-mode)
            (donkey-normal-mode t))
        (cl-letf (((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (donkey--check-post-command-non-editing)))
      (should entered))))

(ert-deftest donkey-check-post-command-non-editing-skips-when-not-excluded ()
  "Does nothing when major-mode is not in the excluded list."
  (let (entered)
    (with-temp-buffer
      (let ((major-mode 'fundamental-mode)
            (donkey-normal-mode t))
        (cl-letf (((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (donkey--check-post-command-non-editing)))
      (should-not entered))))

(ert-deftest donkey-check-post-command-non-editing-skips-when-normal-mode-inactive ()
  "Does nothing when donkey-normal-mode is not active, even in an excluded mode."
  (let (entered)
    (with-temp-buffer
      (let ((major-mode 'term-mode)
            (donkey-normal-mode nil))
        (cl-letf (((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (donkey--check-post-command-non-editing)))
      (should-not entered))))

;;; ---------------------------------------------------------------------------
;;; donkey--exit-insert
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-state-exit-insert-deactivates-mark ()
  "The function `deactivate-mark' is called before entering Normal state."
  (let (deactivated)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (donkey-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () nil)))
          (donkey--exit-insert))))
    (should deactivated)))

(ert-deftest donkey-state-exit-insert-calls-donkey-enter-normal ()
  "Calls donkey-enter-normal to switch to normal mode."
  (let (called)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (donkey-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq called t))))
          (donkey--exit-insert))))
    (should called)))

(ert-deftest donkey-state-exit-insert-force-enables-normal-if-still-off ()
  "Normal state is force-enabled if it is still off.

If donkey-enter-normal doesn't activate normal mode, the fallback
\(donkey-normal-mode 1) runs."
  (let (force-arg)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (donkey-normal-mode nil))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () nil))
                  ((symbol-function 'donkey-normal-mode)
                   (lambda (&optional arg) (setq force-arg arg))))
          (donkey--exit-insert))))
    (should (eq force-arg 1))))

(ert-deftest donkey-state-exit-insert-skips-force-when-normal-active ()
  "The force-enable fallback is skipped when Normal state is active.

When donkey-enter-normal successfully enables normal mode, the
fallback \(donkey-normal-mode 1) is not called."
  (let (force-called)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (donkey-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () nil))
                  ((symbol-function 'donkey-normal-mode)
                   (lambda (&optional _arg) (setq force-called t))))
          (donkey--exit-insert))))
    (should-not force-called)))

(ert-deftest donkey-state-exit-insert-minibuffer-delegates-to-keyboard-quit ()
  "In the minibuffer, delegates to `keyboard-quit' and skips all other steps."
  (let (quit-called deactivated entered-normal)
    (cl-letf (((symbol-function 'minibufferp)
               (lambda () t))
              ((symbol-function 'keyboard-quit)
               (lambda () (setq quit-called t)))
              ((symbol-function 'deactivate-mark)
               (lambda () (setq deactivated t)))
              ((symbol-function 'donkey-enter-normal)
               (lambda () (setq entered-normal t))))
      (donkey--exit-insert))
    (should quit-called)
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest donkey-state-exit-insert-insert-mode-inactive-delegates-to-keyboard-quit ()
  "With Insert state inactive, the command delegates to `keyboard-quit'.

It skips all other steps.  Regression test:  Regression test:
`donkey-setup-smartparens' binds this command directly into
Smartparens' own keymaps (independent of DONKEY's lifecycle), so a
stray key press routed there after disabling `donkey-mode' (which
turns off `donkey-insert-mode' in every buffer) must not resurrect
`donkey-normal-mode'."
  (let (quit-called deactivated entered-normal)
    (with-temp-buffer
      (let ((donkey-mode nil)
            (donkey-insert-mode nil))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'keyboard-quit)
                   (lambda () (setq quit-called t)))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donkey--exit-insert))))
    (should quit-called)
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest donkey-state-exit-insert-standalone-usage-without-donkey-mode-still-transitions ()
  "The state commands work standalone, without the global mode.

donkey-insert-mode/donkey-normal-mode are usable without
ever enabling the global `donkey-mode'.  Regression test: checking
`donkey-mode' here (instead of the buffer-local `donkey-insert-mode')
would make `C-g' always fall through to `keyboard-quit' for that
usage, since `donkey-mode' would never be on -- never actually
transitioning to Normal state."
  (let (entered-normal)
    (with-temp-buffer
      (let ((donkey-mode nil)
            (donkey-insert-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donkey--exit-insert))))
    (should entered-normal)))

;;; ---------------------------------------------------------------------------
;;; donkey--intercept-quit-in-insert (unit level)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-state-intercept-quit-triggers-on-c-g-in-insert-mode ()
  "\\=`C-g\\=' in Insert state is intercepted.

When in insert mode (not minibuffer) and \\=`C-g\\=' ([7]) is pressed,
it intercepts: sets `this-command' to ignore, deactivates mark, enters
normal mode."
  (let (cmd-set deactivated entered-normal)
    (with-temp-buffer
      (let ((donkey-mode t)
            (donkey-insert-mode t)
            (donkey-normal-mode t)
            (donkey--just-exited-from-insert nil)
            (this-command 'original))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donkey--intercept-quit-in-insert)
          (setq cmd-set this-command))))
    (should (eq cmd-set 'ignore))
    (should deactivated)
    (should entered-normal)))

(ert-deftest donkey-state-intercept-quit-skips-when-not-insert-mode ()
  "When donkey-insert-mode is not active, does nothing."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((donkey-insert-mode nil)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donkey--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest donkey-state-intercept-quit-skips-in-minibuffer ()
  "Even in insert mode, minibuffer prevents interception."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () t))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donkey--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest donkey-state-intercept-quit-skips-in-excluded-mode ()
  "The interception is skipped in excluded-mode buffers.

Regression test: the
raw key must fall through to the direct `C-g' binding instead, so
`keyboard-quit' (called from `donkey--exit-insert' there) runs as an
ordinary command rather than signaling `quit' from inside this
`pre-command-hook' function — which Emacs's command loop treats as a
hook malfunction, reporting \"Error in pre-command-hook\" and
permanently removing this function from the hook after just one
`C-g' in any excluded-mode buffer, for the rest of the session."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (major-mode 'comint-mode)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donkey--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest donkey-state-intercept-quit-skips-non-c-g-key ()
  "Keys other than \\=`C-g\\=' ([7]) are not intercepted."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [8]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donkey--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest donkey-intercept-sp-cancel-command ()
  "Interception triggers when \\=`C-g\\=' resolves to sp-cancel.

This is the smartparens race-condition fix, and holds even without the
real raw-key check firing."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donkey--with-test-buffer
   (smartparens-mode 1)
   (donkey-enter-insert)
   (forward-char 1)
   (let ((this-command 'sp-cancel))
     (should-not (bound-and-true-p donkey-normal-mode))
     (donkey--intercept-quit-in-insert)
     (should (bound-and-true-p donkey-normal-mode)))))

(ert-deftest donkey-intercept-prevents-overlapping-handlers ()
  "When interceptor fires, it should set `this-command' to ignore."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((this-command 'sp-cancel))
     (donkey--intercept-quit-in-insert)
     (should (eq this-command 'ignore)))))

;;; ---------------------------------------------------------------------------
;;; donkey--on-normal-entry / donkey--on-insert-entry / donkey--on-input-method-activate
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-state-on-normal-entry-saves-and-deactivates-input-method ()
  "Entering Normal state saves and deactivates the input method.

This happens when donkey-normal-mode is active and an input method is
active."
  (let (deactivated)
    (with-temp-buffer
      (let ((donkey-normal-mode t)
            (current-input-method "swedish-postfix")
            (donkey--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donkey--on-normal-entry))
        (should (equal donkey--saved-input-method "swedish-postfix"))))
    (should deactivated)))

(ert-deftest donkey-state-on-normal-entry-skips-when-no-input-method ()
  "When no input method is active, does nothing."
  (let (deactivated)
    (with-temp-buffer
      (let ((donkey-normal-mode t)
            (current-input-method nil)
            (donkey--saved-input-method 'previous-val))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donkey--on-normal-entry))
        (should (eq donkey--saved-input-method 'previous-val))))
    (should-not deactivated)))

(ert-deftest donkey-state-on-normal-entry-skips-when-mode-disabled ()
  "When donkey-normal-mode is nil, does nothing."
  (let (deactivated)
    (with-temp-buffer
      (let ((donkey-normal-mode nil)
            (current-input-method "swedish-postfix")
            (donkey--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donkey--on-normal-entry))
        (should (eq donkey--saved-input-method nil))))
    (should-not deactivated)))

(ert-deftest donkey-state-on-insert-entry-restores-saved-input-method ()
  "Entering Insert state restores the saved input method.

This holds when there is a saved method and none currently active."
  (let (restored)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (donkey--saved-input-method "swedish-postfix")
            (current-input-method nil))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (method) (setq restored method))))
          (donkey--on-insert-entry))))
    (should (equal restored "swedish-postfix"))))

(ert-deftest donkey-state-on-insert-entry-skips-when-no-saved-method ()
  "When no saved input method, does nothing."
  (let (activated)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (donkey--saved-input-method nil)
            (current-input-method nil))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (_m) (setq activated t))))
          (donkey--on-insert-entry))))
    (should-not activated)))

(ert-deftest donkey-state-on-insert-entry-skips-when-method-already-active ()
  "When an input method is already active, does not restore."
  (let (activated)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
            (donkey--saved-input-method "swedish-postfix")
            (current-input-method "already-active"))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (_m) (setq activated t))))
          (donkey--on-insert-entry))))
    (should-not activated)))

(ert-deftest donkey-state-on-input-method-activate-blocks-in-normal-mode ()
  "Activating an input method in Normal state is blocked.

The method name is saved and the method deactivated."
  (let (deactivated)
    (with-temp-buffer
      (let ((donkey-normal-mode t)
            (current-input-method "blocked")
            (donkey--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donkey--on-input-method-activate))
        (should (equal donkey--saved-input-method "blocked"))))
    (should deactivated)))

(ert-deftest donkey-state-on-input-method-activate-allows-in-insert-mode ()
  "When not in normal mode, input method activation is allowed."
  (let (deactivated)
    (with-temp-buffer
      (let ((donkey-normal-mode nil)
            (current-input-method "allowed")
            (donkey--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donkey--on-input-method-activate))
        (should (eq donkey--saved-input-method nil))))
    (should-not deactivated)))

(ert-deftest donkey-state-on-input-method-activate-suppresses-recursion ()
  "The activate hook is suppressed during deactivation.

`input-method-activate-hook' is let-bound to nil, preventing recursive
hook invocation."
  (let (hook-during-deactivate)
    (with-temp-buffer
      (let ((donkey-normal-mode t)
            (current-input-method "test")
            (donkey--saved-input-method nil)
            (input-method-activate-hook '(some-function)))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda ()
                     (setq hook-during-deactivate
                           input-method-activate-hook))))
          (donkey--on-input-method-activate))))
    (should-not hook-during-deactivate)))

(ert-deftest donkey-state-on-input-method-deactivate-clears-saved-in-insert-mode ()
  "Deactivating in Insert state clears the saved input method.

The saved value is cleared so it won't be resurrected later."
  (with-temp-buffer
    (let ((donkey-insert-mode t)
          (donkey--saved-input-method "swedish-postfix"))
      (donkey--on-input-method-deactivate)
      (should-not donkey--saved-input-method))))

(ert-deftest donkey-state-on-input-method-deactivate-skips-when-not-insert-mode ()
  "Deactivating outside Insert state leaves the saved value alone.

When donkey-insert-mode is not active (e.g. this is
donkey--on-normal-entry's own save-and-deactivate step), the saved
value is untouched."
  (with-temp-buffer
    (let ((donkey-insert-mode nil)
          (donkey--saved-input-method "swedish-postfix"))
      (donkey--on-input-method-deactivate)
      (should (equal donkey--saved-input-method "swedish-postfix")))))

(ert-deftest donkey-input-method-manual-deactivation-is-not-resurrected ()
  "A manually deactivated input method is not resurrected.

Regression test: manually deactivating an input method while remaining
in Insert state used to leave the stale saved value in place, so the
next Normal-to-Insert cycle silently reactivated the very input method
the user had just turned off."
  (with-temp-buffer
    (fundamental-mode)
    (donkey-enter-insert)
    ;; These hooks are installed by `donkey-mode', which is not on
    ;; here; bind exactly what it would install.
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (name)
                 (setq current-input-method name)
                 (run-hooks 'input-method-activate-hook)))
              ((symbol-function 'deactivate-input-method)
               (lambda ()
                 (setq current-input-method nil)
                 (run-hooks 'input-method-deactivate-hook)))
              (input-method-activate-hook
               (list #'donkey--on-input-method-activate))
              (input-method-deactivate-hook
               (list #'donkey--on-input-method-deactivate)))
      (setq current-input-method "swedish-postfix")
      (donkey-enter-normal)
      (donkey-enter-insert)
      (should (equal current-input-method "swedish-postfix"))
      ;; User manually turns the input method off while still in Insert state.
      (deactivate-input-method)
      (should-not donkey--saved-input-method)
      ;; Cycling through Normal and back to Insert must NOT resurrect it.
      (donkey-enter-normal)
      (donkey-enter-insert)
      (should-not current-input-method))))

;;; ---------------------------------------------------------------------------
;;; donkey-disable-input-method
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-disable-input-method-clears-saved-value-in-normal-state ()
  "Disabling the input method clears the saved value in Normal state.

Regression test: `deactivate-input-method' alone is a no-op in Normal
state, since Donkey already deactivated the live input method on entry
and `current-input-method' is already nil -- so it never clears the
buffer-local `donkey--saved-input-method' stash, and the next
Insert-state entry resurrects the input method the user just tried to
turn off.  `donkey-disable-input-method' must clear the saved value
unconditionally, regardless of `current-input-method''s live state."
  (with-temp-buffer
    (let ((donkey--saved-input-method "swedish-postfix")
          (current-input-method nil)
          (deactivate-called nil))
      (cl-letf (((symbol-function 'deactivate-input-method)
                 (lambda () (setq deactivate-called t))))
        (donkey-disable-input-method))
      (should-not donkey--saved-input-method)
      (should-not deactivate-called))))

(ert-deftest donkey-disable-input-method-deactivates-live-method ()
  "Disabling the input method deactivates a live method too.

When an input method is actually active (e.g. called from Insert
state), it is deactivated, not just the saved stash."
  (with-temp-buffer
    (let ((donkey--saved-input-method nil)
          (current-input-method "swedish-postfix")
          (deactivate-called nil))
      (cl-letf (((symbol-function 'deactivate-input-method)
                 (lambda ()
                   (setq deactivate-called t
                         current-input-method nil))))
        (donkey-disable-input-method))
      (should deactivate-called)
      (should-not current-input-method)
      (should-not donkey--saved-input-method))))

(ert-deftest donkey-disable-input-method-prevents-resurrection-on-insert-entry ()
  "A disabled input method is not resurrected on Insert entry.

End-to-end regression: after `donkey-disable-input-method' in Normal
state, re-entering Insert state must not reactivate the input method."
  (with-temp-buffer
    (fundamental-mode)
    (donkey-enter-insert)
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (name)
                 (setq current-input-method name)
                 (run-hooks 'input-method-activate-hook)))
              ((symbol-function 'deactivate-input-method)
               (lambda ()
                 (setq current-input-method nil)
                 (run-hooks 'input-method-deactivate-hook))))
      (setq current-input-method "swedish-postfix")
      (donkey-enter-normal)
      ;; donkey--on-normal-entry already deactivated it and stashed it;
      ;; current-input-method is nil here, same as the bug's repro.
      (should-not current-input-method)
      (should (equal donkey--saved-input-method "swedish-postfix"))
      (donkey-disable-input-method)
      (should-not donkey--saved-input-method)
      (donkey-enter-insert)
      (should-not current-input-method))))

;;; ---------------------------------------------------------------------------
;;; Integration: Basic Mode Transition (C-g through full pipeline)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-exits-insert-to-normal ()
  "\\=`C-g\\=' in insert mode (no overlays, no mark) should enter normal mode."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (bound-and-true-p donkey-insert-mode))
   (should-not (bound-and-true-p donkey-normal-mode))
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should-not (bound-and-true-p donkey-insert-mode))))

(ert-deftest donkey-cg-normal-mode-lighter ()
  "Modeline lighter should show DONKEY[N] after \\=`C-g\\=' from insert."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (string-match-p "DONKEY\\[I\\]" (donkey-indicator)))
   (donkey--simulate-cg)
   (should (string-match-p "DONKEY\\[N\\]" (donkey-indicator)))
   (should-not (string-match-p "DONKEY\\[I\\]" (donkey-indicator)))))

(ert-deftest donkey-cg-cursor-shape ()
  "Cursor should change from bar to box after \\=`C-g\\=' from insert."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (eq cursor-type (default-value 'donkey-cursor-insert)))
   (donkey--simulate-cg)
   (should (eq cursor-type (default-value 'donkey-cursor-normal)))))

;;; ---------------------------------------------------------------------------
;;; Integration: Smartparens
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-inside-sp-pair ()
  "\\=`C-g\\=' inside a smartparens pair should enter normal mode on first press."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donkey--with-test-buffer
   (smartparens-mode 1)
   (donkey-enter-insert)
   (forward-char 1)
   (should (bound-and-true-p donkey-insert-mode))
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))))

(ert-deftest donkey-cg-no-sp-post-command-error ()
  "\\=`C-g\\=' with smartparens overlays signals no post-command error.

No error should be signaled in `post-command-hook'."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donkey--with-test-buffer
   (smartparens-mode 1)
   (donkey-enter-insert)
   (forward-char 1)
   (let ((errors nil))
     (condition-case err
         (donkey--simulate-cg)
       (error (push err errors)))
     (should (null errors)))))

(ert-deftest donkey-setup-smartparens-binds-c-g ()
  "Smartparens setup binds \\=`C-g\\=' in every keymap it finds.

It binds \\=`C-g\\=' to donkey--exit-insert in each smartparens keymap
found bound."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donkey-setup-smartparens)
  (should (eq (keymap-lookup smartparens-mode-map "C-g") #'donkey--exit-insert)))

(ert-deftest donkey-setup-smartparens-no-error-without-keymaps ()
  "Smartparens setup does not error without the keymaps.

It is a no-op when the relevant keymap variables are unbound."
  (cl-letf (((symbol-function 'boundp) (lambda (_sym) nil)))
    (should-not (condition-case nil
                    (progn (donkey-setup-smartparens) nil)
                  (error t)))))

;;; ---------------------------------------------------------------------------
;;; Integration: Active Region / Mark
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-with-active-region ()
  "\\=`C-g\\=' with active region should enter normal mode and deactivate mark."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (set-mark (point))
   (forward-word 1)
   (activate-mark)
   (should (region-active-p))
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should-not (region-active-p))))

;;; ---------------------------------------------------------------------------
;;; Integration: Minibuffer Safety
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-in-minibuffer-does-not-transition ()
  "\\=`C-g\\=' in the minibuffer should NOT trigger DONKEY state transition."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (bound-and-true-p donkey-insert-mode))
   (cl-letf (((symbol-function #'minibufferp) (lambda () t)))
     (donkey--simulate-cg))
   (should (bound-and-true-p donkey-insert-mode))))

;;; ---------------------------------------------------------------------------
;;; Integration: State Verification
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-normal-keymap-active ()
  "After \\=`C-g\\=' transition, normal-mode-map bindings should be active."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should (eq (keymap-lookup (current-active-maps) "h")
               #'backward-char))))

(ert-deftest donkey-cg-insert-keymap-disabled ()
  "After \\=`C-g\\=' transition, donkey-insert-mode-map should NOT be active."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should-not (memq donkey-insert-mode-map (current-active-maps)))))

(ert-deftest donkey-cg-normal-mode-hook-runs ()
  "`donkey-normal-mode-hook' should fire after \\=`C-g\\=' transition."
  (donkey--with-test-buffer
   (let ((hook-fired nil))
     (add-hook 'donkey-normal-mode-hook
               (lambda () (setq hook-fired t))
               nil t)
     (donkey-enter-insert)
     (should-not hook-fired)
     (donkey--simulate-cg)
     (should hook-fired))))

;;; ---------------------------------------------------------------------------
;;; Integration: Excluded Modes Safety
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-in-excluded-mode ()
  "\\=`C-g\\=' in an excluded mode delegates to `keyboard-quit'.

In excluded modes, DONKEY should start in insert state, and \\=`C-g\\='
should delegate rather than flip to normal mode, since a
flip would just get reverted immediately and silently swallow the quit.

Simulates the real command loop rather than using
`donkey--simulate-cg': in an excluded-mode buffer,
`donkey--intercept-quit-in-insert' must skip entirely (see its
docstring — signaling `quit' from inside a `pre-command-hook'
function gets the function permanently disabled), so `this-command'
must already resolve to `donkey--exit-insert' via the direct `C-g'
keymap binding before `pre-command-hook' runs, exactly as the real
command loop would set it."
  (donkey--with-test-buffer
   (let ((donkey-excluded-modes (cons 'fundamental-mode donkey-excluded-modes))
         (quit-called nil))
     (donkey-normal-mode -1)
     (donkey-insert-mode -1)
     (donkey--ensure-default-state)
     (should (bound-and-true-p donkey-insert-mode))
     (should-not (bound-and-true-p donkey-normal-mode))
     (cl-letf (((symbol-function 'keyboard-quit)
                (lambda () (setq quit-called t))))
       (let ((this-command 'donkey--exit-insert))
         (cl-letf (((symbol-function 'this-single-command-keys)
                    (lambda () [7])))
           (run-hooks 'pre-command-hook))
         (unless (eq this-command 'ignore)
           (call-interactively this-command))))
     (should quit-called)
     (should (bound-and-true-p donkey-insert-mode))
     (should-not (bound-and-true-p donkey-normal-mode)))))

;;; ---------------------------------------------------------------------------
;;; Integration: Input Method Preservation
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-input-method-saved-on-normal-entry ()
  "Entering normal mode should save and deactivate input method."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((donkey--saved-input-method nil))
     (setq current-input-method "TeX")
     (cl-letf (((symbol-function #'deactivate-input-method)
                (lambda () (setq current-input-method nil))))
       (donkey--simulate-cg))
     (should (equal donkey--saved-input-method "TeX"))
     (should (null current-input-method))
     (setq current-input-method nil))))

(ert-deftest donkey-cg-input-method-restored-on-insert-entry ()
  "Entering insert mode should restore previously saved input method."
  (donkey--with-test-buffer
   (donkey-enter-normal)
   (let ((donkey--saved-input-method "TeX"))
     (setq current-input-method nil)
     (cl-letf (((symbol-function #'activate-input-method)
                (lambda (method) (setq current-input-method method))))
       (donkey-enter-insert))
     (should (equal current-input-method "TeX"))
     (setq current-input-method nil))))

;;; ---------------------------------------------------------------------------
;;; Integration: Direct Function Call
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-exit-insert-direct-call ()
  "Calling donkey--exit-insert directly should enter normal mode."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (call-interactively #'donkey--exit-insert)
   (should (bound-and-true-p donkey-normal-mode))))

(ert-deftest donkey-cg-exit-insert-deactivates-mark ()
  "`donkey--exit-insert' should deactivate an active region."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (set-mark (point))
   (forward-word 1)
   (activate-mark)
   (call-interactively #'donkey--exit-insert)
   (should-not (region-active-p))))

;;; ---------------------------------------------------------------------------
;;; Integration: Repeated C-g Presses (Guard Race Condition)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-guard-prevents-double-execution ()
  "The guard should prevent double-execution when \\=`C-g\\=' is pressed rapidly."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should donkey--just-exited-from-insert)
   ;; `l' rather than `h': point is at `point-min' in this buffer, so
   ;; `h' (`backward-char') signals `beginning-of-buffer'.  It did not
   ;; show while `donkey--simulate-key' ran whatever `this-command'
   ;; happened to hold instead of the key's own binding -- the key was
   ;; never actually pressed, so it could not fail.
   (donkey--simulate-key [108])
   (should-not donkey--just-exited-from-insert)))

(ert-deftest donkey-cg-double-cg-stays-in-normal ()
  "Pressing \\=`C-g\\=' twice should remain in normal mode, not crash."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (let ((errors nil))
     (condition-case err
         (donkey--simulate-cg)
       (error (push err errors)))
     (should (null errors))
     (should (bound-and-true-p donkey-normal-mode)))))

(ert-deftest donkey-cg-then-insert-then-cg ()
  "\\=`C-g\\=' -> insert -> \\=`C-g\\=' cycle should work cleanly."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (donkey-enter-insert)
   (forward-word 1)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))))

(ert-deftest donkey-reset-exit-guard-is-buffer-local-to-originating-buffer ()
  "The exit-insert guard resets only in its originating buffer.

It must only reset once the ORIGINATING buffer is
current again for its next command, not whichever buffer happens to run
the next command.  Regression test: `donkey--reset-exit-guard' used to
be registered on the global `pre-command-hook', so if a different
buffer became current before the originating buffer's next real
command, the guard reset there instead, then self-removed — leaving
the originating buffer's guard stuck non-nil forever and silently
disabling the `C-g' interception fallback for it."
  (let ((buf-a (generate-new-buffer "donkey-guard-buf-a"))
        (buf-b (generate-new-buffer "donkey-guard-buf-b")))
    (unwind-protect
        (progn
          ;; Trigger the REAL interception path (not a hand-rolled
          ;; add-hook call) so this exercises whatever LOCAL-ness the
          ;; actual code uses.
          (with-current-buffer buf-a
            (fundamental-mode)
            (donkey-insert-mode 1)
            (let ((this-command 'self-insert-command))
              (cl-letf (((symbol-function 'this-single-command-keys)
                         (lambda () [7]))
                        ((symbol-function 'donkey--exit-insert)
                         (lambda () nil)))
                (donkey--intercept-quit-in-insert))))
          (should (buffer-local-value 'donkey--just-exited-from-insert buf-a))
          ;; Simulate the next command running in a DIFFERENT buffer.
          (with-current-buffer buf-b
            (run-hooks 'pre-command-hook))
          ;; buf-a hasn't had its own next command yet, so its guard
          ;; must still be set.
          (should (buffer-local-value 'donkey--just-exited-from-insert buf-a))
          ;; Now buf-a becomes current for its next real command.
          (with-current-buffer buf-a
            (run-hooks 'pre-command-hook))
          (should-not (buffer-local-value 'donkey--just-exited-from-insert buf-a)))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

;;; ---------------------------------------------------------------------------
;;; Overlay Cleanup (Transient Faces Variants)
;;;
;;; NOTE: delete-overlay removes the overlay from the buffer but the
;;; overlay object still exists. Check (overlay-start ov) returns nil to
;;; verify deletion, since overlayp still returns t for detached overlays.
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-clear-overlays-with-sp-show-pair-face ()
  "Deletes overlays with sp-show-pair-match-face."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'sp-show-pair-match-face)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-with-show-paren-match-face ()
  "Deletes overlays with show-paren-match face."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'show-paren-match)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-with-hl-paren-face ()
  "Deletes overlays with hl-paren-face."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'hl-paren-face)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-preserves-non-transient-faces ()
  "Does NOT delete overlays with non-transient faces."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'highlight)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-deletes-an-untracked-pair-overlay ()
  "Strategy 3 deletes a Smartparens overlay it is not tracking.

The other half of the strategy from
`donkey-clear-overlays-removes-tracked-pair-overlay-from-sp-list\=': an
overlay carrying the Smartparens keymap but absent from
`sp-pair-overlay-list\=' goes through a plain `delete-overlay\=', since
there is no Smartparens bookkeeping to unwind.

This used to gate on `sp-overlay-keymap\=', which does not exist -- not
in the version CI pins, and nowhere in Smartparens 1.11.0, where the
keymap is `sp-pair-overlay-keymap\='.  So it skipped unconditionally,
including in the job built to run exactly these tests, and the branch it
was written for went uncovered.  The source keeps a `boundp\='-guarded
arm for the other name in case some version has it; what it cannot do is
be tested through a symbol that is never bound."
  (skip-unless (boundp 'sp-pair-overlay-keymap))
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point))))
         ;; Deliberately NOT in `sp-pair-overlay-list': that is what
         ;; separates this from the tracked case.
         (sp-pair-overlay-list nil))
     (overlay-put ov 'keymap sp-pair-overlay-keymap)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-keeps-non-sp-keymap-overlays ()
  "Does NOT delete overlays carrying unrelated keymaps."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((dummy-map (make-sparse-keymap))
         (ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'keymap dummy-map)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-removes-tracked-pair-overlay-from-sp-list ()
  "A tracked pair overlay is removed through Smartparens itself.

Strategy 3 removes an overlay tracked in `sp-pair-overlay-list' via
`sp--remove-overlay' instead of a raw `delete-overlay'.

Regression test: raw `delete-overlay' on an overlay Smartparens still
has in `sp-pair-overlay-list' leaves a stale, deleted-overlay
reference there.  `overlay-start'/`overlay-end' on a deleted overlay
return nil, so the next command then crashed
`sp--pair-overlay-post-command-handler' (still buffer-locally
registered, since only `sp--remove-overlay' also unregisters it) with
\(wrong-type-argument `number-or-marker-p' nil) -- reproduced live in a
real `emacs -nw' session."
  (skip-unless (and (featurep 'smartparens)
                     (boundp 'sp-pair-overlay-keymap)
                     (fboundp 'sp--remove-overlay)))
  (require 'smartparens)
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let* ((ov (make-overlay (point) (1+ (point))))
          (sp-pair-overlay-list (list ov)))
     (overlay-put ov 'keymap sp-pair-overlay-keymap)
     (should (memq ov sp-pair-overlay-list))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov))
     (should-not (memq ov sp-pair-overlay-list)))))

;;; ---------------------------------------------------------------------------
;;; Deferred Cleanup Timer
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-schedule-overlay-cleanup-creates-timer ()
  "Creates a deferred timer."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should-not donkey--deferred-overlay-cleanup-timer)
   (donkey--schedule-overlay-cleanup)
   (should (timerp donkey--deferred-overlay-cleanup-timer))))

(ert-deftest donkey-schedule-overlay-cleanup-cancels-existing-timer ()
  "Cancels existing timer before creating new one."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--schedule-overlay-cleanup)
   (let ((old-timer donkey--deferred-overlay-cleanup-timer))
     (donkey--schedule-overlay-cleanup)
     (should (timerp donkey--deferred-overlay-cleanup-timer))
     (should (not (eq donkey--deferred-overlay-cleanup-timer old-timer))))))

;;; ---------------------------------------------------------------------------
;;; Graceful Degradation Without Smartparens
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-without-smartparens ()
  "\\=`C-g\\=' enters Normal state even without smartparens.

Only runs in environments where smartparens is absent."
  (skip-unless (not (featurep 'smartparens)))
  (donkey--with-test-buffer
   (should-not (featurep 'smartparens))
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))))

(ert-deftest donkey-cg-no-sp-functions-bound-check ()
  "Smartparens setup binds \\=`C-g\\=' without erroring.

This holds whether smartparens is absent or present."
  (should (fboundp 'donkey--exit-insert))
  (when (and (featurep 'smartparens)
             (boundp 'smartparens-mode-map))
    (require 'smartparens)
    (donkey-setup-smartparens)
    (should (eq (keymap-lookup smartparens-mode-map "C-g")
                #'donkey--exit-insert))))

;;; ---------------------------------------------------------------------------
;;; donkey-mode (global toggle)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mode-enable-registers-hooks ()
  "Enabling `donkey-mode' registers its hook functions.

It adds its `after-change-major-mode-hook' and `post-command-hook'
functions."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (should (memq #'donkey--ensure-default-state after-change-major-mode-hook))
        (should (memq #'donkey--track-position post-command-hook))
        (should (memq #'donkey--check-post-command-non-editing post-command-hook))
        (should (memq #'donkey--update-cursor-passive post-command-hook)))
    (donkey-mode -1)))

(ert-deftest donkey-mode-disable-removes-hooks ()
  "Disabling donkey-mode removes the hooks it registered."
  (donkey-mode 1)
  (donkey-mode -1)
  (should-not (memq #'donkey--ensure-default-state after-change-major-mode-hook))
  (should-not (memq #'donkey--track-position post-command-hook))
  (should-not (memq #'donkey--check-post-command-non-editing post-command-hook))
  (should-not (memq #'donkey--update-cursor-passive post-command-hook)))

(defun donkey-test--cancel-stray-resweep-timers ()
  "Cancel every pending `donkey--startup-resweep\\=' timer.

Calling the resweep DIRECTLY -- as several tests below do -- clears
the tracking variable while the enable-time timer object stays
pending, which the disable path then cannot see.  A test that does so
must sweep the orphan out by function identity afterward, or it leaks
into whichever test asserts `timer-idle-list\\=' contents next.  Found
by the shuffle runner, seeds 1 and 20260822: in alphabetical order the
asserting tests happen to run first, so only a shuffled order ever put
the leak in front of them."
  (dolist (tm (copy-sequence timer-idle-list))
    (when (eq (timer--function tm) #'donkey--startup-resweep)
      (cancel-timer tm)))
  (setq donkey--startup-resweep-timer nil))

;; The startup resweep -- see `donkey--startup-resweep'.  The buffer these
;; tests stand in for is the startup screen: created after every startup
;; hook and left in `fundamental-mode', the mode buffers are born in, so
;; neither the enable-time sweep nor `after-change-major-mode-hook' ever
;; reaches it.  `get-buffer-create' reproduces exactly that shape.

(ert-deftest donkey-mode-enable-schedules-the-startup-resweep ()
  "Enabling the mode schedules the one-shot idle resweep, unconditionally.

UNCONDITIONALLY is the load-bearing word, and a batch run is the
discriminating environment for it: `after-init-time' is long set here,
exactly as it is when the mode is enabled from a \"-l\" file or an
`after-init-hook' function -- enables whose startup screen still
arrives later.  Guarding the schedule on a nil `after-init-time' was
tried and rejected in the fix; reintroducing it fails this test before
it misses any splash."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (should after-init-time)
        (should donkey--startup-resweep-timer)
        (should (memq donkey--startup-resweep-timer timer-idle-list)))
    (donkey-mode -1)))

(ert-deftest donkey-mode-enable-twice-schedules-one-resweep ()
  "Re-enabling the mode does not stack a second resweep timer."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (let ((first donkey--startup-resweep-timer))
          (donkey-mode 1)
          (should (eq first donkey--startup-resweep-timer))
          (should (= 1 (seq-count
                        (lambda (tm)
                          (eq (timer--function tm) #'donkey--startup-resweep))
                        timer-idle-list)))))
    (donkey-mode -1)))

(ert-deftest donkey-mode-disable-cancels-the-startup-resweep ()
  "Disabling the mode cancels a pending resweep -- teardown mirrors setup.

The resweep would no-op behind its own `donkey-mode' guard, but a live
timer belonging to a switched-off mode is still DONKEY state, and the
teardown promises to clear all of it."
  (donkey-mode 1)
  (donkey-mode -1)
  (should-not donkey--startup-resweep-timer)
  (should-not (cl-some (lambda (tm)
                         (eq (timer--function tm) #'donkey--startup-resweep))
                       timer-idle-list)))

(ert-deftest donkey-startup-resweep-adopts-a-late-fundamental-buffer ()
  "The resweep gives Normal state to a buffer born after the enable sweep.

The reported defect, reduced to its mechanism: a buffer created after
`donkey-mode's own sweep, in `fundamental-mode' so no mode function
ever fires `after-change-major-mode-hook' for it, sat with the mode on
and no state -- on the real startup screen that meant every key dead."
  (unwind-protect
      (let (buf)
        (donkey-mode 1)
        (setq buf (get-buffer-create "*donkey-late-buffer*"))
        (with-current-buffer buf
          (should-not (bound-and-true-p donkey-normal-mode))
          (should-not (bound-and-true-p donkey-insert-mode)))
        (donkey--startup-resweep)
        (with-current-buffer buf
          (should (bound-and-true-p donkey-normal-mode))))
    (when (get-buffer "*donkey-late-buffer*")
      (kill-buffer "*donkey-late-buffer*"))
    (donkey-mode -1)
    (donkey-test--cancel-stray-resweep-timers)))

(ert-deftest donkey-startup-resweep-excluded-mode-gets-insert ()
  "A late buffer in an excluded mode gets passthrough Insert, not Normal.

The resweep routes through `donkey--ensure-default-state', so the
excluded-mode rule travels with it.  The mode is claimed by setting
`major-mode' directly rather than running `comint-mode', which would
want a subprocess; the variable is what `donkey--excluded-mode-p'
actually reads."
  (unwind-protect
      (let (buf)
        (donkey-mode 1)
        (setq buf (get-buffer-create "*donkey-late-excluded*"))
        (with-current-buffer buf
          (setq-local major-mode 'comint-mode))
        (donkey--startup-resweep)
        (with-current-buffer buf
          (should (bound-and-true-p donkey-insert-mode))
          (should-not (bound-and-true-p donkey-normal-mode))))
    (when (get-buffer "*donkey-late-excluded*")
      (kill-buffer "*donkey-late-excluded*"))
    (donkey-mode -1)
    (donkey-test--cancel-stray-resweep-timers)))

(ert-deftest donkey-startup-resweep-leaves-existing-state-alone ()
  "A buffer already holding a state keeps it through a resweep.

What makes the unconditional schedule safe to begin with: an extra
sweep only touches buffers holding no DONKEY state at all, so Insert
entered by hand survives it."
  (unwind-protect
      (let ((buf (get-buffer-create "*donkey-late-buffer*")))
        (donkey-mode 1)
        (with-current-buffer buf
          (donkey-enter-insert))
        (donkey--startup-resweep)
        (with-current-buffer buf
          (should (bound-and-true-p donkey-insert-mode))
          (should-not (bound-and-true-p donkey-normal-mode))))
    (when (get-buffer "*donkey-late-buffer*")
      (kill-buffer "*donkey-late-buffer*"))
    (donkey-mode -1)
    (donkey-test--cancel-stray-resweep-timers)))

(ert-deftest donkey-startup-resweep-respects-the-mode-being-off ()
  "With `donkey-mode' off, the resweep touches nothing.

The guard that makes a stale firing harmless: the timer is canceled on
disable as well, but a belt does not argue against suspenders here --
the timer fires code, and code that runs for a switched-off mode must
decline on its own evidence."
  (donkey-mode -1)
  (let ((buf (get-buffer-create "*donkey-late-buffer*")))
    (unwind-protect
        (progn
          (donkey--startup-resweep)
          (with-current-buffer buf
            (should-not (bound-and-true-p donkey-normal-mode))
            (should-not (bound-and-true-p donkey-insert-mode))))
      (kill-buffer buf))))

;; The minibuffer stays out of every sweep -- see the minibuffer guard
;; in `donkey--ensure-default-state'.

(ert-deftest donkey-ensure-default-state-leaves-minibuffers-alone ()
  "The state funnel gives a minibuffer no state at all, and says so.

Every sweep pours through `donkey--ensure-default-state' -- the
enable-time sweep, the startup resweep, and
`after-change-major-mode-hook' -- and before the guard this pins, a
sweep that reached an ACTIVE minibuffer put Normal state into it,
turning the letters being typed into commands.  Entry was protected
only by hook order; nothing protected a prompt after entry."
  (unwind-protect
      (with-current-buffer (window-buffer (minibuffer-window))
        (should (minibufferp))
        (should-not (donkey--ensure-default-state))
        (should-not (bound-and-true-p donkey-normal-mode))
        (should-not (bound-and-true-p donkey-insert-mode)))
    (donkey-mode -1)))

(ert-deftest donkey-startup-resweep-leaves-minibuffers-alone ()
  "The startup resweep passes minibuffers by, end to end.

The path that was probed live before the fix: a resweep firing while
a prompt was open put Normal state into the minibuffer.  This drives
the same sweep and asserts the minibuffer comes out stateless."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (donkey--startup-resweep)
        (with-current-buffer (window-buffer (minibuffer-window))
          (should-not (bound-and-true-p donkey-normal-mode))
          (should-not (bound-and-true-p donkey-insert-mode))))
    (donkey-mode -1)
    (donkey-test--cancel-stray-resweep-timers)))

;; donkey-version -- the loaded version, from the package header.

(ert-deftest donkey-version-matches-the-package-header ()
  "The captured version and the header's version are the same string.

Rule made literal: documentation stating an enumerable fact gets a
test that recounts it.  `lm-version' re-reads the header from the
source file on disk; in this suite the loaded library IS that file,
so the two must agree exactly -- a bump that edits the header without
a reload, or a capture that broke, shows up as a mismatch."
  (require 'lisp-mnt)
  (let ((source (locate-library "donkey.el")))
    (should source)
    (should (equal donkey-version (lm-version source)))))

(ert-deftest donkey-version-looks-like-a-version ()
  "The captured version has the MAJOR.MINOR.PATCH shape releases use."
  (should (stringp donkey-version))
  (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" donkey-version)))

(ert-deftest donkey-version-command-returns-the-string-from-lisp ()
  "Called from Lisp, the command returns the version, not a message.

The interactive branch -- echoing \"DONKEY <version>\" -- cannot be
tested here: `called-interactively-p' is nil under --batch whatever
the caller does, including `funcall-interactively', so a test of the
echo would really test the fallback.  That branch was verified in a
live -nw frame when the command shipped; this pins the Lisp contract."
  (should (equal (donkey-version) donkey-version)))

(ert-deftest donkey-version-command-returns-nil-when-unknown ()
  "An unreadable header at load time means nil from Lisp, not a sentence.

Callers can tell \"unknown\" from a real version without parsing
prose; the docstring promises exactly that."
  (let ((donkey-version nil))
    (should (null (donkey-version)))))

;; The quit-recovery guard -- see `donkey--recover-quit-in-insert'.  The
;; eaten keypress itself cannot be reproduced from ERT: keys driven
;; through `execute-kbd-macro' are injected above the interrupt layer,
;; so a macro C-g can never be eaten, at any timing -- a timing-sweep
;; test here would pass forever regardless of the code.  What CAN be
;; pinned deterministically is the handler's decision table, and the
;; live rig with real terminal bytes covers the timing dimension
;; outside the suite.

(ert-deftest donkey-recover-quit-in-insert-exits-insert ()
  "A quit unwinding with Insert state on becomes the exit it stood for."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (switch-to-buffer (get-buffer-create "*donkey-recover-test*"))
        (donkey-enter-insert)
        (let (orig-called)
          (donkey--recover-quit-in-insert
           (lambda (&rest _) (setq orig-called t)) '(quit) "" nil)
          (should-not orig-called)
          (should (bound-and-true-p donkey-normal-mode))
          (should-not (bound-and-true-p donkey-insert-mode))))
    (when (get-buffer "*donkey-recover-test*")
      (kill-buffer "*donkey-recover-test*"))
    (donkey-mode -1)))

(ert-deftest donkey-recover-quit-in-normal-passes-through ()
  "A quit with Normal state on is ordinary and reaches the wrapped handler."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (switch-to-buffer (get-buffer-create "*donkey-recover-test*"))
        (donkey-enter-normal)
        (let (orig-called)
          (donkey--recover-quit-in-insert
           (lambda (&rest _) (setq orig-called t)) '(quit) "" nil)
          (should orig-called)
          (should (bound-and-true-p donkey-normal-mode))))
    (when (get-buffer "*donkey-recover-test*")
      (kill-buffer "*donkey-recover-test*"))
    (donkey-mode -1)))

(ert-deftest donkey-recover-non-quit-passes-through ()
  "Real errors are not this handler's business, whatever the state."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (switch-to-buffer (get-buffer-create "*donkey-recover-test*"))
        (donkey-enter-insert)
        (let (orig-called)
          (donkey--recover-quit-in-insert
           (lambda (&rest _) (setq orig-called t))
           '(error "Boom") "" nil)
          (should orig-called)
          ;; and Insert survives: an error is not an exit request
          (should (bound-and-true-p donkey-insert-mode))))
    (when (get-buffer "*donkey-recover-test*")
      (kill-buffer "*donkey-recover-test*"))
    (donkey-mode -1)))

(ert-deftest donkey-recover-quit-in-excluded-mode-passes-through ()
  "Excluded modes keep their real quits, mirroring `donkey--exit-insert'."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (switch-to-buffer (get-buffer-create "*donkey-recover-test*"))
        (setq-local major-mode 'comint-mode)
        (donkey-enter-insert)
        (let (orig-called)
          (donkey--recover-quit-in-insert
           (lambda (&rest _) (setq orig-called t)) '(quit) "" nil)
          (should orig-called)
          (should (bound-and-true-p donkey-insert-mode))))
    (when (get-buffer "*donkey-recover-test*")
      (kill-buffer "*donkey-recover-test*"))
    (donkey-mode -1)))

(ert-deftest donkey-mode-enable-installs-the-quit-recovery ()
  "Enabling the mode wraps `command-error-function'; disabling unwraps.

Teardown mirrors setup: a handler left wrapped after the mode is off
would keep converting quits for a package the user turned off."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (should (advice-function-member-p #'donkey--recover-quit-in-insert
                                          command-error-function))
        (donkey-mode -1)
        (should-not (advice-function-member-p #'donkey--recover-quit-in-insert
                                              command-error-function)))
    (donkey-mode -1)))

(ert-deftest donkey-startup-resweep-forgets-its-timer ()
  "The resweep clears `donkey--startup-resweep-timer' when it runs.

The variable doubles as the only-schedule-one guard on the enable
path; a fired resweep that left it set would make every later enable
skip scheduling for the rest of the session."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (should donkey--startup-resweep-timer)
        (donkey--startup-resweep)
        (should-not donkey--startup-resweep-timer))
    (donkey-mode -1)
    (donkey-test--cancel-stray-resweep-timers)))

(ert-deftest donkey-mode-update-cursor-on-post-command-hook-resyncs-on-window-switch ()
  "The cursor resyncs on a window switch.

Regression test: `donkey--update-cursor-passive' must run on the
global `post-command-hook', not only on `donkey-normal-mode-hook'/
`donkey-insert-mode-hook'.

Those mode hooks only fire when a buffer's own DONKEY state actually
toggles.  Switching the selected window between two buffers that
already each have an established (but different) DONKEY state -- via
`other-window', `switch-to-buffer', etc. -- never toggles either
buffer's mode, so without this hook the terminal's cursor shape would
never resync to the newly-selected buffer's actual state.  Confirmed
live in `emacs -nw': `other-window' between a Normal-state buffer and
an Insert-state buffer sent no DECSCUSR sequence at all until this was
added."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (let ((call-count 0))
          (cl-letf (((symbol-function 'donkey--update-cursor-passive)
                     (lambda () (setq call-count (1+ call-count)))))
            (run-hooks 'post-command-hook))
          (should (= call-count 1))))
    (donkey-mode -1)))

(ert-deftest donkey-update-cursor-passive-skips-unmanaged-buffer ()
  "The passive cursor update skips an unmanaged buffer.

Regression test: `donkey--update-cursor-passive' must NOT reset
`cursor-type' in a buffer where neither `donkey-normal-mode' nor
`donkey-insert-mode' is active.

Confirmed live in `emacs -nw': a freshly `get-buffer-create'd buffer
that never runs any major-mode setup function never triggers
`after-change-major-mode-hook', so `donkey--ensure-default-state'
never applies DONKEY state to it -- yet the global `post-command-hook'
runs for EVERY buffer that becomes current, DONKEY-managed or not.
Before this fix, switching to such a buffer via `switch-to-buffer'
silently reset a `cursor-type' an unrelated package had set there on
purpose, via `donkey--update-cursor''s unconditional \"neither mode
active\" branch."
  (with-temp-buffer
    (setq-local cursor-type 'hbar)
    (donkey--update-cursor-passive)
    (should (local-variable-p 'cursor-type))
    (should (eq cursor-type 'hbar))))

(ert-deftest donkey-update-cursor-non-passive-still-resets-unmanaged-buffer ()
  "The non-passive cursor update still resets an unmanaged buffer.

Without PASSIVE, `donkey--update-cursor' still resets `cursor-type'
to the default when neither mode is active.

Regression guard for the opposite failure mode: a buffer's own
Normal/Insert -> disabled transition (e.g. standalone
`donkey-normal-mode' disabled without ever entering Insert, with no
global `donkey-mode' to fall back on its own explicit per-buffer
reset) relies on exactly this to restore the cursor -- `passive'
must only suppress the reset for the global `post-command-hook' poll,
never for the mode-hook-triggered call."
  (with-temp-buffer
    (donkey--apply-cursor-setting 'hbar)
    (donkey--update-cursor)
    (should-not (local-variable-p 'cursor-type))))

(ert-deftest donkey-mode-owns-every-global-hook ()
  "Every hook in `donkey--global-hooks' is on with the mode, gone without it.

Regression test: `pre-command-hook', the minibuffer hooks and the
input-method hooks used to be added at
load time by a bare `require' and never removed, so a session that had
only loaded the file ran DONKEY code on every command for good."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (pcase-dolist (`(,hook . ,fn) donkey--global-hooks)
          (should (memq fn (default-value hook))))
        (donkey-mode -1)
        (pcase-dolist (`(,hook . ,fn) donkey--global-hooks)
          (should-not (memq fn (default-value hook)))))
    (donkey-mode -1)))

(ert-deftest donkey-mode-check-post-command-non-editing-not-registered-before-enable ()
  "The non-editing post-command check is not registered before enable.

`donkey--check-post-command-non-editing' must not be a permanent,
unconditional global hook.  Regression test: it used to be added once
at load time regardless of whether `donkey-mode' was ever enabled, and
was never removed by `donkey-mode's disable branch — a hook lifecycle
leak that ran on every command in every buffer for the life of the
Emacs session."
  (donkey-mode -1)
  (should-not (memq #'donkey--check-post-command-non-editing post-command-hook)))

(ert-deftest donkey-mode-enable-activates-existing-buffers ()
  "Enabling donkey-mode activates normal state in existing editable buffers."
  (let ((buf (generate-new-buffer "*donkey-mode-test-buf*")))
    (unwind-protect
        (with-current-buffer buf
          (fundamental-mode)
          (donkey-normal-mode -1)
          (donkey-insert-mode -1)
          (donkey-mode 1)
          (should (or (bound-and-true-p donkey-normal-mode)
                      (bound-and-true-p donkey-insert-mode))))
      (donkey-mode -1)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest donkey-mode-disable-clears-existing-buffers ()
  "Disabling donkey-mode turns off normal/insert state in all buffers."
  (let ((buf (generate-new-buffer "*donkey-mode-test-buf2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (fundamental-mode)
            (donkey-mode 1)
            (should (or (bound-and-true-p donkey-normal-mode)
                        (bound-and-true-p donkey-insert-mode))))
          (donkey-mode -1)
          (with-current-buffer buf
            (should-not (bound-and-true-p donkey-normal-mode))
            (should-not (bound-and-true-p donkey-insert-mode))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; ---------------------------------------------------------------------------
;;; The one promise: C-g in Insert always reaches Normal
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-c-g-interception-survives-a-signaling-user-hook ()
  "A broken `donkey-normal-mode-hook' must not cost the user their escape key.

Regression, confirmed by driving a real key through `execute-kbd-macro':
`donkey--intercept-quit-in-insert' runs on `pre-command-hook', and Emacs
REMOVES a hook function that signals.  One error anywhere inside
`donkey--exit-insert' -- and `donkey-normal-mode-hook' is user-facing,
so anyone's cursor or modeline function can raise one -- silently
disabled the whole interception for the rest of the session, in every
buffer.

That mechanism is what keeps `C-g' working when something else has taken
the key: nested smartparens overlays, a package binding it in its own
map, a terminal where the direct binding is not reached.  Losing it
silently is the worst outcome DONKEY has."
  (let ((buf (get-buffer-create "*donkey-cg-test*"))
        (boom (lambda () (when (bound-and-true-p donkey-normal-mode)
                           (error "Boom from a user hook")))))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (text-mode)
          (donkey-mode 1)
          (insert "some text here\n")
          (goto-char (point-min))
          (donkey-enter-insert)
          (add-hook 'donkey-normal-mode-hook boom)
          (should (memq #'donkey--intercept-quit-in-insert
                        (default-value 'pre-command-hook)))
          (execute-kbd-macro (kbd "C-g"))
          ;; The interception is still installed ...
          (should (memq #'donkey--intercept-quit-in-insert
                        (default-value 'pre-command-hook)))
          ;; ... and the promise was kept on this press too.
          (should (bound-and-true-p donkey-normal-mode)))
      (remove-hook 'donkey-normal-mode-hook boom)
      ;; If the guard has regressed, Emacs has just removed the
      ;; interception -- put it back, so this test fails on its own
      ;; assertion instead of taking every later `donkey-cg-' test with
      ;; it.  Reverting the fix locally showed exactly that: 3 real
      ;; failures and 12 pieces of collateral damage.
      (add-hook 'pre-command-hook #'donkey--intercept-quit-in-insert)
      (donkey-mode -1)
      (kill-buffer buf))))

(ert-deftest donkey-c-g-still-reaches-normal-when-deactivate-mark-signals ()
  "A signaling `deactivate-mark-hook' must not strand the user in Insert.

It is the only step that runs BEFORE the state transition, and it runs
`deactivate-mark-hook', which is not DONKEY's -- so a stranger's broken
hook would otherwise abort the very keypress meant to leave Insert."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-enter-insert)
        (let ((transient-mark-mode t))
          (insert "hello world")
          (goto-char 1)
          (set-mark 5)
          (activate-mark)
          (let ((deactivate-mark-hook
                 (list (lambda () (error "Boom from deactivate-mark-hook")))))
            (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
              (donkey--exit-insert)))
          (should (bound-and-true-p donkey-normal-mode))
          (should-not (bound-and-true-p donkey-insert-mode))))
    (donkey-mode -1)))

(ert-deftest donkey-c-g-interception-survives-repeated-failures ()
  "Not just the first one: the hook is still there after several."
  (let ((buf (get-buffer-create "*donkey-cg-test-2*"))
        (boom (lambda () (when (bound-and-true-p donkey-normal-mode)
                           (error "Boom again")))))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (text-mode)
          (donkey-mode 1)
          (insert "text\n")
          (add-hook 'donkey-normal-mode-hook boom)
          (dotimes (_ 3)
            (donkey-enter-insert)
            (execute-kbd-macro (kbd "C-g"))
            (should (memq #'donkey--intercept-quit-in-insert
                          (default-value 'pre-command-hook)))
            (should (bound-and-true-p donkey-normal-mode))))
      (remove-hook 'donkey-normal-mode-hook boom)
      (add-hook 'pre-command-hook #'donkey--intercept-quit-in-insert)
      (donkey-mode -1)
      (kill-buffer buf))))

(ert-deftest donkey-c-g-in-insert-is-bound-on-both-paths ()
  "Both mechanisms behind the promise are in place.

The keymap binding is the primary route; the `pre-command-hook'
interception is the backup for when something else has taken the key."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (should (eq (lookup-key donkey-insert-mode-map (kbd "C-g"))
                    #'donkey--exit-insert))
        (should (memq #'donkey--intercept-quit-in-insert
                      (default-value 'pre-command-hook))))
    (donkey-mode -1)))

(ert-deftest donkey-indicator-says-E-in-an-excluded-mode ()
  "An excluded buffer reports DONKEY[E], not DONKEY[I].

Insert state is the truthful answer there -- keys really do pass
through -- but it is a misleading one, because NORMAL state cannot be
reached from those buffers by any key.  A lighter that says Insert
invites the reader to press `C-g\=' and conclude the key is broken."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (dolist (mode '(comint-mode term-mode vterm-mode eshell-mode))
          (with-temp-buffer
            (setq major-mode mode)
            (donkey--ensure-default-state)
            (should (bound-and-true-p donkey-insert-mode))
            (should (equal (donkey-indicator) " DONKEY[E]")))))
    (donkey-mode -1)))

(ert-deftest donkey-indicator-says-I-outside-excluded-modes ()
  "An ordinary buffer in Insert state still reports DONKEY[I]."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-enter-insert)
        (should (equal (donkey-indicator) " DONKEY[I]")))
    (donkey-mode -1)))

(ert-deftest donkey-insert-lighter-is-computed-not-stored ()
  "The lighter is an `:eval\=' form, so it follows a mode change.

A buffer can become excluded without any DONKEY transition firing --
`M-x `eshell-mode'\=' in an ordinary buffer does it -- so a lighter fixed
at the moment Insert state was entered would then be wrong with nothing
to correct it."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (let* ((entry (assq 'donkey-insert-mode minor-mode-alist))
               (form (cadr (cadr entry))))
          ;; it really is an :eval construct, not a constant string
          (should (eq (car (cadr entry)) :eval))
          (with-temp-buffer
            (text-mode)
            (donkey-enter-insert)
            (should (equal (eval form t) " DONKEY[I]"))
            ;; the major mode changes underneath the state
            (setq major-mode 'eshell-mode)
            (should (equal (eval form t) " DONKEY[E]")))))
    (donkey-mode -1)))

(ert-deftest donkey-indicator-says-E-for-a-derived-excluded-mode ()
  "Derivation counts: a mode derived from `comint-mode\=' reports E too.

`donkey--excluded-mode-p\=' uses `derived-mode-p\=', which is why listing
`comint-mode\=' alone covers its dozens of derivatives."
  (unwind-protect
      (progn
        (require 'comint)
        (eval '(define-derived-mode donkey-test--shellish-mode comint-mode "Shellish") t)
        (donkey-mode 1)
        (with-temp-buffer
          (donkey-test--shellish-mode)
          (donkey--ensure-default-state)
          (should (bound-and-true-p donkey-insert-mode))
          (should (equal (donkey-indicator) " DONKEY[E]"))))
    (donkey-mode -1)))

(ert-deftest donkey-c-g-in-insert-stops-a-recording-macro ()
  "Leaving Insert state aborts a keyboard macro, as `keyboard-quit\=' does.

Regression: `C-g\=' looked like it had abandoned the recording -- Normal
state, box cursor, nothing to suggest otherwise -- while every later
keystroke was still being recorded.  The only signal was
`kmacro-start-macro\=' refusing later with \"Already defining keyboard
macro\"."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-enter-insert)
        (setq defining-kbd-macro t)
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
          (donkey--exit-insert))
        (should-not defining-kbd-macro)
        (should (bound-and-true-p donkey-normal-mode)))
    (setq defining-kbd-macro nil)
    (donkey-mode -1)))

(ert-deftest donkey-macro-abort-cannot-block-the-state-transition ()
  "A signal from the macro cleanup must not strand the user in Insert.

It runs AFTER the transition and is caught, for the same reason the rest
of this path is: it is reached from `pre-command-hook\=', where a signal
costs the user the whole `C-g\=' interception for the session.  No macro
is worth that."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-enter-insert)
        (setq defining-kbd-macro t)
        (cl-letf (((symbol-function 'kmacro-keyboard-quit)
                   (lambda () (error "Boom from kmacro")))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (donkey--exit-insert))
        (should (bound-and-true-p donkey-normal-mode))
        (should-not (bound-and-true-p donkey-insert-mode)))
    (setq defining-kbd-macro nil)
    (donkey-mode -1)))

(ert-deftest donkey-macro-abort-does-nothing-when-no-macro-is-recording ()
  "The ordinary case is untouched: no recording, nothing to stop."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-enter-insert)
        (setq defining-kbd-macro nil)
        (donkey--exit-insert)
        (should-not defining-kbd-macro)
        (should (bound-and-true-p donkey-normal-mode)))
    (donkey-mode -1)))

(ert-deftest donkey-backspace-and-delete-are-blocked-under-every-key-name ()
  "All four names, not just the graphical pair.

BACKSPACE and DELETE arrive under different names depending on the
frame: a GUI sends <backspace> and <delete>, a terminal sends DEL
\(ASCII 127) and <deletechar>.  Only the first two were bound, so the
block worked on a GUI and did nothing in a terminal -- absent for the
users most likely to be running `emacs -nw\='."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-normal-mode 1)
        (dolist (key (list [backspace] [?\C-?] [delete] [deletechar]))
          (should (eq (key-binding key) #'ignore))))
    (donkey-mode -1)))

(ert-deftest donkey-blocking-DEL-leaves-the-bank-clear-binding-alone ()
  "`m DEL\=' still clears the bank under all three of its key names.

`m\=' is a prefix, so `m DEL\=' is a different key sequence from `DEL\=' --
but it is the obvious thing for this change to have broken."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-normal-mode 1)
        (dolist (key '("m DEL" "m <deletechar>" "m <delete>"))
          (should (eq (key-binding (kbd key)) #'donkey-clear-banked-selection))))
    (donkey-mode -1)))

(provide 'donkey-state-management-test)

;;; donkey-state-management-test.el ends here

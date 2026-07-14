;;; mule-state-management-test.el --- Tests for MULE state management -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; Declare mule-specific variables so let-bindings are dynamic
(defvar mule-normal-mode)
(defvar mule-insert-mode)
(defvar mule--saved-input-method)

(defvar-local mule--just-exited-from-insert nil)
(defvar-local mule--deferred-overlay-cleanup-timer nil)

(defvar this-single-command-keys)
(defvar this-command)

;; ===========================================================================
;; Section: mule-indicator
;; Selector: (ert "mule-state-indicator")
;; ===========================================================================

(ert-deftest mule-state-indicator-normal-mode-active ()
  "When mule-normal-mode is non-nil, returns \" MULE[N]\".
            Expected: \" MULE[N]\"."
  (with-temp-buffer
    (let ((mule-normal-mode t)
          (mule-insert-mode nil))
      (should (equal (mule-indicator) " MULE[N]")))))

(ert-deftest mule-state-indicator-insert-mode-active ()
  "When mule-insert-mode is non-nil and mule-normal-mode is nil,
            returns \" MULE[I]\".
            Expected: \" MULE[I]\"."
  (with-temp-buffer
    (let ((mule-normal-mode nil)
          (mule-insert-mode t))
      (should (equal (mule-indicator) " MULE[I]")))))

(ert-deftest mule-state-indicator-neither-mode-active ()
  "When neither mode is active, returns empty string.
            Expected: \"\"."
  (with-temp-buffer
    (let ((mule-normal-mode nil)
          (mule-insert-mode nil))
      (should (equal (mule-indicator) "")))))

(ert-deftest mule-state-indicator-normal-takes-precedence ()
  "Normal mode checked first in cond; if both somehow active,
            normal wins.
            Expected: \" MULE[N]\"."
  (with-temp-buffer
    (let ((mule-normal-mode t)
          (mule-insert-mode t))
      (should (equal (mule-indicator) " MULE[N]")))))

;; ===========================================================================
;; Section: mule--exit-insert
;; Selector: (ert "mule-state-exit-insert")
;; ===========================================================================

(ert-deftest mule-state-exit-insert-deactivates-mark ()
  "Calls deactivate-mark before entering normal mode.
            Expected: deactivate-mark invoked."
  (let (deactivated)
    (with-temp-buffer
      (let ((mule-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'mule-enter-normal)
                   (lambda () nil)))
          (mule--exit-insert))))
    (should deactivated)))

(ert-deftest mule-state-exit-insert-calls-mule-enter-normal ()
  "Calls mule-enter-normal to switch to normal mode.
            Expected: mule-enter-normal invoked."
  (let (called)
    (with-temp-buffer
      (let ((mule-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'mule-enter-normal)
                   (lambda () (setq called t))))
          (mule--exit-insert))))
    (should called)))

(ert-deftest mule-state-exit-insert-force-enables-normal-if-still-off ()
  "If mule-enter-normal doesn't activate normal mode, force-enables it
            via (mule-normal-mode 1).
            Expected: mule-normal-mode called with arg 1."
  (let (force-arg)
    (with-temp-buffer
      (let ((mule-normal-mode nil))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'mule-enter-normal)
                   (lambda () nil))
                  ((symbol-function 'mule-normal-mode)
                   (lambda (&optional arg) (setq force-arg arg))))
          (mule--exit-insert))))
    (should (eq force-arg 1))))

(ert-deftest mule-state-exit-insert-skips-force-when-normal-active ()
  "When mule-enter-normal successfully enables normal mode, the
            fallback (mule-normal-mode 1) is not called.
            Expected: mule-normal-mode not called directly."
  (let (force-called)
    (with-temp-buffer
      (let ((mule-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'mule-enter-normal)
                   (lambda () nil))
                  ((symbol-function 'mule-normal-mode)
                   (lambda (&optional _arg) (setq force-called t))))
          (mule--exit-insert))))
    (should-not force-called)))

(ert-deftest mule-state-exit-insert-minibuffer-delegates-to-keyboard-quit ()
  "In the minibuffer, delegates to keyboard-quit and skips all other steps.
            Expected: keyboard-quit called, deactivate-mark and mule-enter-normal skipped."
  (let (quit-called deactivated entered-normal)
    (cl-letf (((symbol-function 'minibufferp)
               (lambda () t))
              ((symbol-function 'keyboard-quit)
               (lambda () (setq quit-called t)))
              ((symbol-function 'deactivate-mark)
               (lambda () (setq deactivated t)))
              ((symbol-function 'mule-enter-normal)
               (lambda () (setq entered-normal t))))
      (mule--exit-insert))
    (should quit-called)
    (should-not deactivated)
    (should-not entered-normal)))

;; ===========================================================================
;; Section: mule--intercept-quit-in-insert
;; Selector: (ert "mule-state-intercept-quit")
;; ===========================================================================

(ert-deftest mule-state-intercept-quit-triggers-on-c-g-in-insert-mode ()
  "When in insert mode (not minibuffer) and C-g ([7]) is pressed,
    intercepts: sets this-command to ignore, deactivates mark,
    enters normal mode.
    Expected: all three side effects occur."
  (let (cmd-set deactivated entered-normal)
    (with-temp-buffer
      (let ((mule-insert-mode t)
            (mule-normal-mode t)
            (mule--just-exited-from-insert nil)
            (this-command 'original)
            (this-single-command-keys [7]))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'mule-enter-normal)
                   (lambda () (setq entered-normal t))))
          (mule--intercept-quit-in-insert)
          (setq cmd-set this-command))))
    (should (eq cmd-set 'ignore))
    (should deactivated)
    (should entered-normal)))

(ert-deftest mule-state-intercept-quit-skips-when-not-insert-mode ()
  "When mule-insert-mode is not active, does nothing.
            Expected: this-command unchanged, no side effects."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((mule-insert-mode nil)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'mule-enter-normal)
                   (lambda () (setq entered-normal t))))
          (mule--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest mule-state-intercept-quit-skips-in-minibuffer ()
  "Even in insert mode, minibuffer prevents interception.
            Expected: this-command unchanged, no side effects."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((mule-insert-mode t)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () t))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'mule-enter-normal)
                   (lambda () (setq entered-normal t))))
          (mule--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest mule-state-intercept-quit-skips-non-c-g-key ()
  "Keys other than C-g ([7]) are not intercepted.
            Expected: this-command unchanged, no side effects."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((mule-insert-mode t)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [8]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'mule-enter-normal)
                   (lambda () (setq entered-normal t))))
          (mule--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

;; ===========================================================================
;; Section: mule--on-normal-entry
;; Selector: (ert "mule-state-on-normal-entry")
;; ===========================================================================

(ert-deftest mule-state-on-normal-entry-saves-and-deactivates-input-method ()
  "When mule-normal-mode is active and an input method is active,
            saves the method name and deactivates it.
            Expected: mule--saved-input-method set, deactivate-input-method called."
  (let (deactivated)
    (with-temp-buffer
      (let ((mule-normal-mode t)
            (current-input-method "swedish-postfix")
            (mule--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (mule--on-normal-entry))
        (should (equal mule--saved-input-method "swedish-postfix"))))
    (should deactivated)))

(ert-deftest mule-state-on-normal-entry-skips-when-no-input-method ()
  "When no input method is active, does nothing.
            Expected: mule--saved-input-method unchanged, deactivate not called."
  (let (deactivated)
    (with-temp-buffer
      (let ((mule-normal-mode t)
            (current-input-method nil)
            (mule--saved-input-method 'previous-val))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (mule--on-normal-entry))
        (should (eq mule--saved-input-method 'previous-val))))
    (should-not deactivated)))

(ert-deftest mule-state-on-normal-entry-skips-when-mode-disabled ()
  "When mule-normal-mode is nil, does nothing.
            Expected: no action even if input method is active."
  (let (deactivated)
    (with-temp-buffer
      (let ((mule-normal-mode nil)
            (current-input-method "swedish-postfix")
            (mule--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (mule--on-normal-entry))
        (should (eq mule--saved-input-method nil))))
    (should-not deactivated)))

;; ===========================================================================
;; Section: mule--on-insert-entry
;; Selector: (ert "mule-state-on-insert-entry")
;; ===========================================================================

(ert-deftest mule-state-on-insert-entry-restores-saved-input-method ()
  "When entering insert mode with a saved method and none currently
            active, restores the saved method.
            Expected: activate-input-method called with saved value."
  (let (restored)
    (with-temp-buffer
      (let ((mule-insert-mode t)
            (mule--saved-input-method "swedish-postfix")
            (current-input-method nil))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (method) (setq restored method))))
          (mule--on-insert-entry))))
    (should (equal restored "swedish-postfix"))))

(ert-deftest mule-state-on-insert-entry-skips-when-no-saved-method ()
  "When no saved input method, does nothing.
            Expected: activate-input-method not called."
  (let (activated)
    (with-temp-buffer
      (let ((mule-insert-mode t)
            (mule--saved-input-method nil)
            (current-input-method nil))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (_m) (setq activated t))))
          (mule--on-insert-entry))))
    (should-not activated)))

(ert-deftest mule-state-on-insert-entry-skips-when-method-already-active ()
  "When an input method is already active, does not restore.
            Expected: activate-input-method not called."
  (let (activated)
    (with-temp-buffer
      (let ((mule-insert-mode t)
            (mule--saved-input-method "swedish-postfix")
            (current-input-method "already-active"))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (_m) (setq activated t))))
          (mule--on-insert-entry))))
    (should-not activated)))

(ert-deftest mule-state-on-insert-entry-skips-when-mode-disabled ()
  "When mule-insert-mode is nil, does nothing.
            Expected: activate-input-method not called."
  (let (activated)
    (with-temp-buffer
      (let ((mule-insert-mode nil)
            (mule--saved-input-method "swedish-postfix")
            (current-input-method nil))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (_m) (setq activated t))))
          (mule--on-insert-entry))))
    (should-not activated)))

;; ===========================================================================
;; Section: mule--on-input-method-activate
;; Selector: (ert "mule-state-on-input-method-activate")
;; ===========================================================================

(ert-deftest mule-state-on-input-method-activate-blocks-in-normal-mode ()
  "When input method activates while in normal mode, saves the method
            and deactivates it.
            Expected: mule--saved-input-method set, deactivate-input-method called."
  (let (deactivated)
    (with-temp-buffer
      (let ((mule-normal-mode t)
            (current-input-method "blocked")
            (mule--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (mule--on-input-method-activate))
        (should (equal mule--saved-input-method "blocked"))))
    (should deactivated)))

(ert-deftest mule-state-on-input-method-activate-allows-in-insert-mode ()
  "When not in normal mode, input method activation is allowed.
            Expected: no save, no deactivate."
  (let (deactivated)
    (with-temp-buffer
      (let ((mule-normal-mode nil)
            (current-input-method "allowed")
            (mule--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (mule--on-input-method-activate))
        (should (eq mule--saved-input-method nil))))
    (should-not deactivated)))

(ert-deftest mule-state-on-input-method-activate-skips-when-no-method ()
  "When current-input-method is nil, nothing to block.
            Expected: no action."
  (let (deactivated)
    (with-temp-buffer
      (let ((mule-normal-mode t)
            (current-input-method nil)
            (mule--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (mule--on-input-method-activate))
        (should (eq mule--saved-input-method nil))))
    (should-not deactivated)))

(ert-deftest mule-state-on-input-method-activate-suppresses-recursion ()
  "During deactivation, input-method-activate-hook is let-bound to nil
            by the source code, preventing recursive hook invocation.
            Expected: hook is nil when deactivate-input-method is called."
  (let (hook-during-deactivate)
    (with-temp-buffer
      (let ((mule-normal-mode t)
            (current-input-method "test")
            (mule--saved-input-method nil)
            (input-method-activate-hook '(some-function)))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda ()
                     (setq hook-during-deactivate
                           input-method-activate-hook))))
          (mule--on-input-method-activate))))
    (should-not hook-during-deactivate)))

            ;;; mule-state-management-test.el ends here

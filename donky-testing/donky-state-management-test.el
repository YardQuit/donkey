;;; donky-state-management-test.el --- Tests for DONKY state management -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donky)

;; Declare donky-specific variables so let-bindings are dynamic
(defvar donky-normal-mode)
(defvar donky-insert-mode)
(defvar donky--saved-input-method)

(defvar-local donky--just-exited-from-insert nil)
(defvar-local donky--deferred-overlay-cleanup-timer nil)

(defvar this-command)

;; ===========================================================================
;; Section: donky-indicator
;; Selector: (ert "donky-state-indicator")
;; ===========================================================================

(ert-deftest donky-state-indicator-normal-mode-active ()
  "When donky-normal-mode is non-nil, returns \" DONKY[N]\".
            Expected: \" DONKY[N]\"."
  (with-temp-buffer
    (let ((donky-normal-mode t)
          (donky-insert-mode nil))
      (should (equal (donky-indicator) " DONKY[N]")))))

(ert-deftest donky-state-indicator-insert-mode-active ()
  "When donky-insert-mode is non-nil and donky-normal-mode is nil,
            returns \" DONKY[I]\".
            Expected: \" DONKY[I]\"."
  (with-temp-buffer
    (let ((donky-normal-mode nil)
          (donky-insert-mode t))
      (should (equal (donky-indicator) " DONKY[I]")))))

(ert-deftest donky-state-indicator-neither-mode-active ()
  "When neither mode is active, returns empty string.
            Expected: \"\"."
  (with-temp-buffer
    (let ((donky-normal-mode nil)
          (donky-insert-mode nil))
      (should (equal (donky-indicator) "")))))

(ert-deftest donky-state-indicator-normal-takes-precedence ()
  "Normal mode checked first in cond; if both somehow active,
            normal wins.
            Expected: \" DONKY[N]\"."
  (with-temp-buffer
    (let ((donky-normal-mode t)
          (donky-insert-mode t))
      (should (equal (donky-indicator) " DONKY[N]")))))

;; ===========================================================================
;; Section: donky--exit-insert
;; Selector: (ert "donky-state-exit-insert")
;; ===========================================================================

(ert-deftest donky-state-exit-insert-deactivates-mark ()
  "Calls deactivate-mark before entering normal mode.
            Expected: deactivate-mark invoked."
  (let (deactivated)
    (with-temp-buffer
      (let ((donky-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donky-enter-normal)
                   (lambda () nil)))
          (donky--exit-insert))))
    (should deactivated)))

(ert-deftest donky-state-exit-insert-calls-donky-enter-normal ()
  "Calls donky-enter-normal to switch to normal mode.
            Expected: donky-enter-normal invoked."
  (let (called)
    (with-temp-buffer
      (let ((donky-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'donky-enter-normal)
                   (lambda () (setq called t))))
          (donky--exit-insert))))
    (should called)))

(ert-deftest donky-state-exit-insert-force-enables-normal-if-still-off ()
  "If donky-enter-normal doesn't activate normal mode, force-enables it
            via (donky-normal-mode 1).
            Expected: donky-normal-mode called with arg 1."
  (let (force-arg)
    (with-temp-buffer
      (let ((donky-normal-mode nil))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'donky-enter-normal)
                   (lambda () nil))
                  ((symbol-function 'donky-normal-mode)
                   (lambda (&optional arg) (setq force-arg arg))))
          (donky--exit-insert))))
    (should (eq force-arg 1))))

(ert-deftest donky-state-exit-insert-skips-force-when-normal-active ()
  "When donky-enter-normal successfully enables normal mode, the
            fallback (donky-normal-mode 1) is not called.
            Expected: donky-normal-mode not called directly."
  (let (force-called)
    (with-temp-buffer
      (let ((donky-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'donky-enter-normal)
                   (lambda () nil))
                  ((symbol-function 'donky-normal-mode)
                   (lambda (&optional _arg) (setq force-called t))))
          (donky--exit-insert))))
    (should-not force-called)))

(ert-deftest donky-state-exit-insert-minibuffer-delegates-to-keyboard-quit ()
  "In the minibuffer, delegates to keyboard-quit and skips all other steps.
            Expected: keyboard-quit called, deactivate-mark and donky-enter-normal skipped."
  (let (quit-called deactivated entered-normal)
    (cl-letf (((symbol-function 'minibufferp)
               (lambda () t))
              ((symbol-function 'keyboard-quit)
               (lambda () (setq quit-called t)))
              ((symbol-function 'deactivate-mark)
               (lambda () (setq deactivated t)))
              ((symbol-function 'donky-enter-normal)
               (lambda () (setq entered-normal t))))
      (donky--exit-insert))
    (should quit-called)
    (should-not deactivated)
    (should-not entered-normal)))

;; ===========================================================================
;; Section: donky--intercept-quit-in-insert
;; Selector: (ert "donky-state-intercept-quit")
;; ===========================================================================

(ert-deftest donky-state-intercept-quit-triggers-on-c-g-in-insert-mode ()
  "When in insert mode (not minibuffer) and C-g ([7]) is pressed,
    intercepts: sets this-command to ignore, deactivates mark,
    enters normal mode.
    Expected: all three side effects occur."
  (let (cmd-set deactivated entered-normal)
    (with-temp-buffer
      (let ((donky-insert-mode t)
            (donky-normal-mode t)
            (donky--just-exited-from-insert nil)
            (this-command 'original))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donky-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donky--intercept-quit-in-insert)
          (setq cmd-set this-command))))
    (should (eq cmd-set 'ignore))
    (should deactivated)
    (should entered-normal)))

(ert-deftest donky-state-intercept-quit-skips-when-not-insert-mode ()
  "When donky-insert-mode is not active, does nothing.
            Expected: this-command unchanged, no side effects."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((donky-insert-mode nil)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donky-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donky--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest donky-state-intercept-quit-skips-in-minibuffer ()
  "Even in insert mode, minibuffer prevents interception.
            Expected: this-command unchanged, no side effects."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((donky-insert-mode t)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () t))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [7]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donky-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donky--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

(ert-deftest donky-state-intercept-quit-skips-non-c-g-key ()
  "Keys other than C-g ([7]) are not intercepted.
            Expected: this-command unchanged, no side effects."
  (let (deactivated entered-normal)
    (with-temp-buffer
      (let ((donky-insert-mode t)
            (this-command 'some-command))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'this-single-command-keys)
                   (lambda () [8]))
                  ((symbol-function 'deactivate-mark)
                   (lambda () (setq deactivated t)))
                  ((symbol-function 'donky-enter-normal)
                   (lambda () (setq entered-normal t))))
          (donky--intercept-quit-in-insert)
          (should (eq this-command 'some-command)))))
    (should-not deactivated)
    (should-not entered-normal)))

;; ===========================================================================
;; Section: donky--on-normal-entry
;; Selector: (ert "donky-state-on-normal-entry")
;; ===========================================================================

(ert-deftest donky-state-on-normal-entry-saves-and-deactivates-input-method ()
  "When donky-normal-mode is active and an input method is active,
            saves the method name and deactivates it.
            Expected: donky--saved-input-method set, deactivate-input-method called."
  (let (deactivated)
    (with-temp-buffer
      (let ((donky-normal-mode t)
            (current-input-method "swedish-postfix")
            (donky--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donky--on-normal-entry))
        (should (equal donky--saved-input-method "swedish-postfix"))))
    (should deactivated)))

(ert-deftest donky-state-on-normal-entry-skips-when-no-input-method ()
  "When no input method is active, does nothing.
            Expected: donky--saved-input-method unchanged, deactivate not called."
  (let (deactivated)
    (with-temp-buffer
      (let ((donky-normal-mode t)
            (current-input-method nil)
            (donky--saved-input-method 'previous-val))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donky--on-normal-entry))
        (should (eq donky--saved-input-method 'previous-val))))
    (should-not deactivated)))

(ert-deftest donky-state-on-normal-entry-skips-when-mode-disabled ()
  "When donky-normal-mode is nil, does nothing.
            Expected: no action even if input method is active."
  (let (deactivated)
    (with-temp-buffer
      (let ((donky-normal-mode nil)
            (current-input-method "swedish-postfix")
            (donky--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donky--on-normal-entry))
        (should (eq donky--saved-input-method nil))))
    (should-not deactivated)))

;; ===========================================================================
;; Section: donky--on-insert-entry
;; Selector: (ert "donky-state-on-insert-entry")
;; ===========================================================================

(ert-deftest donky-state-on-insert-entry-restores-saved-input-method ()
  "When entering insert mode with a saved method and none currently
            active, restores the saved method.
            Expected: activate-input-method called with saved value."
  (let (restored)
    (with-temp-buffer
      (let ((donky-insert-mode t)
            (donky--saved-input-method "swedish-postfix")
            (current-input-method nil))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (method) (setq restored method))))
          (donky--on-insert-entry))))
    (should (equal restored "swedish-postfix"))))

(ert-deftest donky-state-on-insert-entry-skips-when-no-saved-method ()
  "When no saved input method, does nothing.
            Expected: activate-input-method not called."
  (let (activated)
    (with-temp-buffer
      (let ((donky-insert-mode t)
            (donky--saved-input-method nil)
            (current-input-method nil))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (_m) (setq activated t))))
          (donky--on-insert-entry))))
    (should-not activated)))

(ert-deftest donky-state-on-insert-entry-skips-when-method-already-active ()
  "When an input method is already active, does not restore.
            Expected: activate-input-method not called."
  (let (activated)
    (with-temp-buffer
      (let ((donky-insert-mode t)
            (donky--saved-input-method "swedish-postfix")
            (current-input-method "already-active"))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (_m) (setq activated t))))
          (donky--on-insert-entry))))
    (should-not activated)))

(ert-deftest donky-state-on-insert-entry-skips-when-mode-disabled ()
  "When donky-insert-mode is nil, does nothing.
            Expected: activate-input-method not called."
  (let (activated)
    (with-temp-buffer
      (let ((donky-insert-mode nil)
            (donky--saved-input-method "swedish-postfix")
            (current-input-method nil))
        (cl-letf (((symbol-function 'activate-input-method)
                   (lambda (_m) (setq activated t))))
          (donky--on-insert-entry))))
    (should-not activated)))

;; ===========================================================================
;; Section: donky--on-input-method-activate
;; Selector: (ert "donky-state-on-input-method-activate")
;; ===========================================================================

(ert-deftest donky-state-on-input-method-activate-blocks-in-normal-mode ()
  "When input method activates while in normal mode, saves the method
            and deactivates it.
            Expected: donky--saved-input-method set, deactivate-input-method called."
  (let (deactivated)
    (with-temp-buffer
      (let ((donky-normal-mode t)
            (current-input-method "blocked")
            (donky--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donky--on-input-method-activate))
        (should (equal donky--saved-input-method "blocked"))))
    (should deactivated)))

(ert-deftest donky-state-on-input-method-activate-allows-in-insert-mode ()
  "When not in normal mode, input method activation is allowed.
            Expected: no save, no deactivate."
  (let (deactivated)
    (with-temp-buffer
      (let ((donky-normal-mode nil)
            (current-input-method "allowed")
            (donky--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donky--on-input-method-activate))
        (should (eq donky--saved-input-method nil))))
    (should-not deactivated)))

(ert-deftest donky-state-on-input-method-activate-skips-when-no-method ()
  "When current-input-method is nil, nothing to block.
            Expected: no action."
  (let (deactivated)
    (with-temp-buffer
      (let ((donky-normal-mode t)
            (current-input-method nil)
            (donky--saved-input-method nil))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda () (setq deactivated t))))
          (donky--on-input-method-activate))
        (should (eq donky--saved-input-method nil))))
    (should-not deactivated)))

(ert-deftest donky-state-on-input-method-activate-suppresses-recursion ()
  "During deactivation, input-method-activate-hook is let-bound to nil
            by the source code, preventing recursive hook invocation.
            Expected: hook is nil when deactivate-input-method is called."
  (let (hook-during-deactivate)
    (with-temp-buffer
      (let ((donky-normal-mode t)
            (current-input-method "test")
            (donky--saved-input-method nil)
            (input-method-activate-hook '(some-function)))
        (cl-letf (((symbol-function 'deactivate-input-method)
                   (lambda ()
                     (setq hook-during-deactivate
                           input-method-activate-hook))))
          (donky--on-input-method-activate))))
    (should-not hook-during-deactivate)))

            ;;; donky-state-management-test.el ends here

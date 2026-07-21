;;; donkey-state-management-test.el --- Tests for DONKEY Normal/Insert state management -*- lexical-binding: t; -*-

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
  "Create a fresh buffer in `fundamental-mode', enable DONKEY, and
evaluate BODY with point at the start."
  (declare (indent 0))
  `(with-temp-buffer
     (fundamental-mode)
     (donkey-mode -1)
     (donkey-mode 1)
     (donkey--ensure-default-state)
     (donkey-enter-insert)
     (insert "(defun foo ()\n  (let ((x 1))\n    (concat \"bar\" x)))")
     (goto-char (point-min))
     ,@body))

(defun donkey--simulate-cg ()
  "Simulate pressing C-g. Mocks `this-single-command-keys' so
pre-command-hook functions can see it."
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
  "Simulate pressing KEY for testing guard reset behavior."
  (let ((last-command-event (aref key 0))
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
  "When donkey-insert-mode is non-nil and donkey-normal-mode is nil,
returns \" DONKEY[I]\"."
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
;;; donkey--handle-non-editing-buffer / donkey--check-post-command-non-editing
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-handle-non-editing-buffer-enters-insert-when-excluded ()
  "When major-mode is excluded and donkey-normal-mode is active, forces
insert mode."
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
  "When major-mode is excluded but donkey-normal-mode is not active, does
nothing (nothing to correct)."
  (let (entered)
    (with-temp-buffer
      (let ((major-mode 'comint-mode)
            (donkey-normal-mode nil))
        (cl-letf (((symbol-function 'donkey-enter-insert)
                   (lambda () (setq entered t))))
          (donkey--handle-non-editing-buffer)))
      (should-not entered))))

(ert-deftest donkey-check-post-command-non-editing-enters-insert-when-excluded ()
  "When donkey-normal-mode is active in an excluded major mode, forces
insert mode."
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
  "Calls deactivate-mark before entering normal mode."
  (let (deactivated)
    (with-temp-buffer
      (let ((donkey-normal-mode t))
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
      (let ((donkey-normal-mode t))
        (cl-letf (((symbol-function 'minibufferp)
                   (lambda () nil))
                  ((symbol-function 'deactivate-mark)
                   (lambda () nil))
                  ((symbol-function 'donkey-enter-normal)
                   (lambda () (setq called t))))
          (donkey--exit-insert))))
    (should called)))

(ert-deftest donkey-state-exit-insert-force-enables-normal-if-still-off ()
  "If donkey-enter-normal doesn't activate normal mode, force-enables it
via (donkey-normal-mode 1)."
  (let (force-arg)
    (with-temp-buffer
      (let ((donkey-normal-mode nil))
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
  "When donkey-enter-normal successfully enables normal mode, the
fallback (donkey-normal-mode 1) is not called."
  (let (force-called)
    (with-temp-buffer
      (let ((donkey-normal-mode t))
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
  "In the minibuffer, delegates to keyboard-quit and skips all other steps."
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

;;; ---------------------------------------------------------------------------
;;; donkey--intercept-quit-in-insert (unit level)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-state-intercept-quit-triggers-on-c-g-in-insert-mode ()
  "When in insert mode (not minibuffer) and C-g ([7]) is pressed,
intercepts: sets this-command to ignore, deactivates mark, enters
normal mode."
  (let (cmd-set deactivated entered-normal)
    (with-temp-buffer
      (let ((donkey-insert-mode t)
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

(ert-deftest donkey-state-intercept-quit-skips-non-c-g-key ()
  "Keys other than C-g ([7]) are not intercepted."
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
  "Triggers when C-g resolves to sp-cancel (smartparens race-condition fix),
even without the real raw-key check firing."
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
  "When interceptor fires, it should set this-command to ignore."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((this-command 'sp-cancel))
     (donkey--intercept-quit-in-insert)
     (should (eq this-command 'ignore)))))

;;; ---------------------------------------------------------------------------
;;; donkey--on-normal-entry / donkey--on-insert-entry / donkey--on-input-method-activate
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-state-on-normal-entry-saves-and-deactivates-input-method ()
  "When donkey-normal-mode is active and an input method is active, saves
the method name and deactivates it."
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
  "When entering insert mode with a saved method and none currently active,
restores the saved method."
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
  "When input method activates while in normal mode, saves the method and
deactivates it."
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
  "During deactivation, input-method-activate-hook is let-bound to nil,
preventing recursive hook invocation."
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

;;; ---------------------------------------------------------------------------
;;; Integration: Basic Mode Transition (C-g through full pipeline)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-exits-insert-to-normal ()
  "C-g in insert mode (no overlays, no mark) should enter normal mode."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (bound-and-true-p donkey-insert-mode))
   (should-not (bound-and-true-p donkey-normal-mode))
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should-not (bound-and-true-p donkey-insert-mode))))

(ert-deftest donkey-cg-normal-mode-lighter ()
  "Modeline lighter should show DONKEY[N] after C-g from insert."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (string-match-p "DONKEY\\[I\\]" (donkey-indicator)))
   (donkey--simulate-cg)
   (should (string-match-p "DONKEY\\[N\\]" (donkey-indicator)))
   (should-not (string-match-p "DONKEY\\[I\\]" (donkey-indicator)))))

(ert-deftest donkey-cg-cursor-shape ()
  "Cursor should change from bar to box after C-g from insert."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (eq cursor-type (default-value 'donkey-cursor-insert)))
   (donkey--simulate-cg)
   (should (eq cursor-type (default-value 'donkey-cursor-normal)))))

;;; ---------------------------------------------------------------------------
;;; Integration: Smartparens
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-inside-sp-pair ()
  "C-g inside a smartparens pair should enter normal mode on first press."
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
  "After C-g with smartparens overlays active, no error should be signaled
in post-command-hook."
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
  "donkey-setup-smartparens binds C-g to donkey--exit-insert in each
smartparens keymap it finds bound."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donkey-setup-smartparens)
  (should (eq (keymap-lookup smartparens-mode-map "C-g") #'donkey--exit-insert)))

(ert-deftest donkey-setup-smartparens-no-error-without-keymaps ()
  "donkey-setup-smartparens does not error when the relevant keymap
variables are unbound."
  (cl-letf (((symbol-function 'boundp) (lambda (_sym) nil)))
    (should-not (condition-case nil
                    (progn (donkey-setup-smartparens) nil)
                  (error t)))))

;;; ---------------------------------------------------------------------------
;;; Integration: Active Region / Mark
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-with-active-region ()
  "C-g with active region should enter normal mode and deactivate mark."
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
  "C-g in the minibuffer should NOT trigger DONKEY state transition."
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
  "After C-g transition, normal-mode-map bindings should be active."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should (eq (keymap-lookup (current-active-maps) "h")
               #'backward-char))))

(ert-deftest donkey-cg-insert-keymap-disabled ()
  "After C-g transition, donkey-insert-mode-map should NOT be active."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should-not (memq donkey-insert-mode-map (current-active-maps)))))

(ert-deftest donkey-cg-normal-mode-hook-runs ()
  "`donkey-normal-mode-hook' should fire after C-g transition."
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
  "In excluded modes, DONKEY should start in insert state. C-g should not crash."
  (donkey--with-test-buffer
   (let ((donkey-excluded-modes (cons 'fundamental-mode donkey-excluded-modes)))
     (donkey-normal-mode -1)
     (donkey-insert-mode -1)
     (donkey--ensure-default-state)
     (should (bound-and-true-p donkey-insert-mode))
     (should-not (bound-and-true-p donkey-normal-mode))
     (let ((errors nil))
       (condition-case err
           (donkey--simulate-cg)
         (error (push err errors)))
       (should (null errors))))))

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
  "The guard should prevent double-execution when C-g is pressed rapidly."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should donkey--just-exited-from-insert)
   (donkey--simulate-key [104])
   (should-not donkey--just-exited-from-insert)))

(ert-deftest donkey-cg-double-cg-stays-in-normal ()
  "Pressing C-g twice should remain in normal mode, not crash."
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
  "C-g -> insert -> C-g cycle should work cleanly."
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

(ert-deftest donkey-clear-overlays-with-smartparens-overlay-keymap ()
  "Strategy 3 deletes overlays carrying sp-overlay-keymap."
  (skip-unless (boundp 'sp-overlay-keymap))
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'keymap sp-overlay-keymap)
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
  "C-g should enter normal mode even when smartparens is not loaded. Only
runs in environments where smartparens is absent."
  (skip-unless (not (featurep 'smartparens)))
  (donkey--with-test-buffer
   (should-not (featurep 'smartparens))
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))))

(ert-deftest donkey-cg-no-sp-functions-bound-check ()
  "The with-eval-after-load block should not error when smartparens is
absent or present."
  (should (fboundp 'donkey--exit-insert))
  (when (and (featurep 'smartparens)
             (boundp 'smartparens-mode-map))
    (require 'smartparens)
    (should (eq (keymap-lookup smartparens-mode-map "C-g")
                #'donkey--exit-insert))))

;;; ---------------------------------------------------------------------------
;;; donkey-mode (global toggle)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mode-enable-registers-hooks ()
  "Enabling donkey-mode adds its after-change-major-mode-hook and
post-command-hook functions."
  (unwind-protect
      (progn
        (donkey-mode 1)
        (should (memq #'donkey--ensure-default-state after-change-major-mode-hook))
        (should (memq #'donkey--track-position post-command-hook)))
    (donkey-mode -1)))

(ert-deftest donkey-mode-disable-removes-hooks ()
  "Disabling donkey-mode removes the hooks it registered."
  (donkey-mode 1)
  (donkey-mode -1)
  (should-not (memq #'donkey--ensure-default-state after-change-major-mode-hook))
  (should-not (memq #'donkey--track-position post-command-hook)))

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

(provide 'donkey-state-management-test)

;;; donkey-state-management-test.el ends here

;;; donky-insert-to-normal-transition-test.el --- Comprehensive Tests for Insert→Normal Transition -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donky)
(defvar this-command)
(defvar this-original-command)
(defvar last-command-event)
  ;;; ---------------------------------------------------------------------------
  ;;; Helper Macros and Utilities
  ;;; ---------------------------------------------------------------------------

(defmacro donky--with-test-buffer (&rest body)
  "Create a fresh buffer in `fundamental-mode', enable DONKY, and
  evaluate BODY with point at the start."
  (declare (indent 0))
  `(with-temp-buffer
     (fundamental-mode)
     (donky -1)
     (donky 1)
     (donky--ensure-default-state)
     (donky-enter-insert)
     (insert "(defun foo ()\n  (let ((x 1))\n    (concat \"bar\" x)))")
     (goto-char (point-min))
     ,@body))

(defun donky--simulate-key (key)
  "Simulate pressing KEY by mocking `this-single-command-keys' and
  running `pre-command-hook', then executing the bound command.
  KEY should be a vector, e.g. [7] for C-g."
  (let ((last-command-event (aref key 0))
        (this-original-command this-command)
        (overriding-terminal-local-map nil))
    (cl-letf (((symbol-function 'this-single-command-keys)
               (lambda () key)))
      ;; Mimic command loop: run pre-command-hook
      (run-hooks 'pre-command-hook)
      ;; Execute this-command if it wasn't set to ignore
      (unless (eq this-command 'ignore)
        (when (commandp this-command)
          (call-interactively this-command)))
      ;; Run post-command-hook
      (run-hooks 'post-command-hook))))

(defun donky--simulate-cg ()
  "Simulate pressing C-g."
  (let ((this-command (or (keymap-lookup donky-insert-mode-map "C-g")
                          #'keyboard-quit))
        (this-original-command nil))
    (donky--simulate-key [7])))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 1: Basic Mode Transition
  ;;; ---------------------------------------------------------------------------

(ert-deftest donky-cg-exits-insert-to-normal ()
  "C-g in insert mode (no overlays, no mark) should enter normal mode."
  (donky--with-test-buffer
   (donky-enter-insert)
   (should (bound-and-true-p donky-insert-mode))
   (should-not (bound-and-true-p donky-normal-mode))
   (donky--simulate-cg)
   (should (bound-and-true-p donky-normal-mode))
   (should-not (bound-and-true-p donky-insert-mode))))

(ert-deftest donky-cg-normal-mode-lighter ()
  "Modeline lighter should show DONKY[N] after C-g from insert."
  (donky--with-test-buffer
   (donky-enter-insert)
   (should (string-match-p "DONKY\\[I\\]" (donky-indicator)))
   (donky--simulate-cg)
   (should (string-match-p "DONKY\\[N\\]" (donky-indicator)))
   (should-not (string-match-p "DONKY\\[I\\]" (donky-indicator)))))

(ert-deftest donky-cg-cursor-shape ()
  "Cursor should change from bar to box after C-g from insert."
  (donky--with-test-buffer
   (donky-enter-insert)
   (should (eq cursor-type (default-value 'donky-cursor-insert)))
   (donky--simulate-cg)
   (should (eq cursor-type (default-value 'donky-cursor-normal)))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 2: Smartparens Integration
  ;;; ---------------------------------------------------------------------------

(ert-deftest donky-cg-inside-sp-pair ()
  "C-g inside a smartparens pair should enter normal mode on first press.
  Requires smartparens to be loaded."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donky--with-test-buffer
   (smartparens-mode 1)
   (donky-enter-insert)
   (forward-char 1)
   (should (bound-and-true-p donky-insert-mode))
   (donky--simulate-cg)
   (should (bound-and-true-p donky-normal-mode))))

(ert-deftest donky-cg-inside-nested-sp-pairs ()
  "C-g inside deeply nested smartparens pairs should enter normal
  mode on first press regardless of nesting depth."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donky--with-test-buffer
   (smartparens-mode 1)
   (donky-enter-insert)
   (search-forward "bar")
   (backward-char 1)
   (should (bound-and-true-p donky-insert-mode))
   (donky--simulate-cg)
   (should (bound-and-true-p donky-normal-mode))))

(ert-deftest donky-cg-no-sp-post-command-error ()
  "After C-g with smartparens overlays active, no error should be
  signaled in `post-command-hook'."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donky--with-test-buffer
   (smartparens-mode 1)
   (donky-enter-insert)
   (forward-char 1)
   (let ((errors nil))
     (condition-case err
         (donky--simulate-cg)
       (error (push err errors)))
     (should (null errors)))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 3: Active Region / Mark
  ;;; ---------------------------------------------------------------------------

(ert-deftest donky-cg-with-active-region ()
  "C-g with an active region should enter normal mode and
  deactivate the mark in one press."
  (donky--with-test-buffer
   (donky-enter-insert)
   (set-mark (point))
   (forward-word 1)
   (activate-mark)
   (should (region-active-p))
   (donky--simulate-cg)
   (should (bound-and-true-p donky-normal-mode))
   (should-not (region-active-p))))

(ert-deftest donky-cg-with-region-and-sp-pair ()
  "C-g with both an active region and smartparens overlay should
  enter normal mode on one press."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donky--with-test-buffer
   (smartparens-mode 1)
   (donky-enter-insert)
   (forward-char 1)
   (set-mark (point))
   (forward-word 1)
   (activate-mark)
   (should (region-active-p))
   (donky--simulate-cg)
   (should (bound-and-true-p donky-normal-mode))
   (should-not (region-active-p))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 4: Minibuffer Safety
  ;;; ---------------------------------------------------------------------------

(ert-deftest donky-cg-in-minibuffer-does-not-transition ()
  "C-g in the minibuffer should NOT trigger DONKY state transition.
  The interceptor must check `(minibufferp)' and skip. The direct
  keymap binding (`donky--exit-insert') must also guard against
  minibuffer context."
  (donky--with-test-buffer
   (donky-enter-insert)
   (should (bound-and-true-p donky-insert-mode))
   ;; Mock minibufferp to return t, and keyboard-quit to be a no-op
   (cl-letf (((symbol-function #'minibufferp) (lambda () t))
             ((symbol-function #'keyboard-quit) (lambda () (interactive))))
     (donky--simulate-cg))
   (should (bound-and-true-p donky-insert-mode))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 5: State Verification
  ;;; ---------------------------------------------------------------------------

(ert-deftest donky-cg-normal-keymap-active ()
  "After C-g transition, `donky-normal-mode-map' bindings should be
  active (e.g., 'h' should be `backward-char')."
  (donky--with-test-buffer
   (donky-enter-insert)
   (donky--simulate-cg)
   (should (eq (keymap-lookup (current-active-maps) "h")
               #'backward-char))))

(ert-deftest donky-cg-insert-keymap-disabled ()
  "After C-g transition, `donky-insert-mode-map' should not be in
  the active keymaps."
  (donky--with-test-buffer
   (donky-enter-insert)
   (donky--simulate-cg)
   (should-not (memq donky-insert-mode-map (current-active-maps)))))

(ert-deftest donky-cg-normal-mode-hook-runs ()
  "`donky-normal-mode-hook' should fire after C-g transition."
  (donky--with-test-buffer
   (let ((hook-fired nil))
     (add-hook 'donky-normal-mode-hook
               (lambda () (setq hook-fired t))
               nil t)
     (donky-enter-insert)
     (should-not hook-fired)
     (donky--simulate-cg)
     (should hook-fired))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 6: Excluded Modes Safety
  ;;; ---------------------------------------------------------------------------

(ert-deftest donky-cg-in-excluded-mode ()
  "In excluded modes, DONKY should start in insert state.
  C-g should not crash."
  (donky--with-test-buffer
   ;; Temporarily treat fundamental-mode as excluded
   (let ((donky-excluded-modes (cons 'fundamental-mode donky-excluded-modes)))
     (donky-normal-mode -1)
     (donky-insert-mode -1)
     (donky--ensure-default-state)
     (should (bound-and-true-p donky-insert-mode))
     (should-not (bound-and-true-p donky-normal-mode))
     ;; C-g should not crash
     (let ((errors nil))
       (condition-case err
           (donky--simulate-cg)
         (error (push err errors)))
       (should (null errors))))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 7: Input Method Preservation
  ;;; ---------------------------------------------------------------------------

  (ert-deftest donky-cg-input-method-saved-on-normal-entry ()
    "Entering normal mode should save and deactivate input method."
    (donky--with-test-buffer
     (donky-enter-insert)
     (let ((donky--saved-input-method nil))
       (setq current-input-method "TeX")
       (cl-letf (((symbol-function #'deactivate-input-method)
                  (lambda () (setq current-input-method nil))))
         (donky--simulate-cg))
       (should (equal donky--saved-input-method "TeX"))
       (should (null current-input-method))
       (setq current-input-method nil))))

  (ert-deftest donky-cg-input-method-restored-on-insert-entry ()
    "Entering insert mode should restore previously saved input method."
    (donky--with-test-buffer
     (donky-enter-normal)
     (let ((donky--saved-input-method "TeX"))
       (setq current-input-method nil)
       (cl-letf (((symbol-function #'activate-input-method)
                  (lambda (method) (setq current-input-method method))))
         (donky-enter-insert))
       (should (equal current-input-method "TeX"))
       (setq current-input-method nil))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 8: Direct Function Call
  ;;; ---------------------------------------------------------------------------

  (ert-deftest donky-cg-exit-insert-direct-call ()
    "Calling `donky--exit-insert' directly should enter normal mode."
    (donky--with-test-buffer
     (donky-enter-insert)
     (call-interactively #'donky--exit-insert)
     (should (bound-and-true-p donky-normal-mode))))

  (ert-deftest donky-cg-exit-insert-deactivates-mark ()
    "`donky--exit-insert' should deactivate an active region."
    (donky--with-test-buffer
     (donky-enter-insert)
     (set-mark (point))
     (forward-word 1)
     (activate-mark)
     (call-interactively #'donky--exit-insert)
     (should-not (region-active-p))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 9: Repeated C-g Presses
  ;;; ---------------------------------------------------------------------------

  (ert-deftest donky-cg-double-cg-stays-in-normal ()
    "Pressing C-g twice should remain in normal mode, not crash."
    (donky--with-test-buffer
     (donky-enter-insert)
     (donky--simulate-cg)
     (should (bound-and-true-p donky-normal-mode))
     (let ((errors nil))
       (condition-case err
           (donky--simulate-cg)
         (error (push err errors)))
       (should (null errors))
       (should (bound-and-true-p donky-normal-mode)))))

  (ert-deftest donky-cg-then-insert-then-cg ()
    "C-g -> insert -> C-g cycle should work cleanly."
    (donky--with-test-buffer
     (donky-enter-insert)
     (donky--simulate-cg)
     (should (bound-and-true-p donky-normal-mode))
     (donky-enter-insert)
     (donky--simulate-cg)
     (should (bound-and-true-p donky-normal-mode))
     (donky-enter-insert)
     (forward-word 1)
     (donky--simulate-cg)
     (should (bound-and-true-p donky-normal-mode))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 10: Graceful Degradation Without Smartparens
  ;;; ---------------------------------------------------------------------------

  (ert-deftest donky-cg-without-smartparens ()
    "C-g should enter normal mode even when smartparens is not loaded.
  Only runs in environments where smartparens is absent."
    (skip-unless (not (featurep 'smartparens)))
    (donky--with-test-buffer
     (should-not (featurep 'smartparens))
     (donky-enter-insert)
     (donky--simulate-cg)
     (should (bound-and-true-p donky-normal-mode))))

  (ert-deftest donky-cg-no-sp-functions-bound-check ()
    "The `with-eval-after-load' block should not error when
  smartparens is absent or present."
    (should (fboundp 'donky--exit-insert))
    (when (and (featurep 'smartparens)
               (boundp 'smartparens-mode-map))
      (require 'smartparens)
      (should (eq (keymap-lookup smartparens-mode-map "C-g")
                  #'donky--exit-insert))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Runner
  ;;; ---------------------------------------------------------------------------

  (defun donky-run-all-tests ()
    "Run all DONKY transition tests interactively."
    (interactive)
    (ert "^donky-cg-" :result-buffer "*DONKY Test Results*"))

  (provide 'donky-insert-to-normal-transition-test)

  ;;; donky-insert-to-normal-transition-test.el ends here

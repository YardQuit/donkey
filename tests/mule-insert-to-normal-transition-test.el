;;; mule-insert-to-normal-transition-test.el --- Comprehensive Tests for Insert→Normal Transition -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)
(defvar this-single-command-keys)
(defvar this-command)
(defvar this-original-command)
(defvar last-command-event)
  ;;; ---------------------------------------------------------------------------
  ;;; Helper Macros and Utilities
  ;;; ---------------------------------------------------------------------------

(defmacro mule--with-test-buffer (&rest body)
  "Create a fresh buffer in `fundamental-mode', enable MULE, and
  evaluate BODY with point at the start."
  (declare (indent 0))
  `(with-temp-buffer
     (fundamental-mode)
     (mule-modal -1)
     (mule-modal 1)
     (mule--ensure-default-state)
     (mule-enter-insert)
     (insert "(defun foo ()\n  (let ((x 1))\n    (concat \"bar\" x)))")
     (goto-char (point-min))
     ,@body))

(defun mule--simulate-key (key)
  "Simulate pressing KEY by setting `this-single-command-keys' and
  running `pre-command-hook', then executing the bound command.
  KEY should be a vector, e.g. [7] for C-g."
  (let ((last-command-event (aref key 0))
        (this-single-command-keys key)
        (this-original-command this-command)
        (overriding-terminal-local-map nil))
    ;; Mimic command loop: run pre-command-hook
    (run-hooks 'pre-command-hook)
    ;; Execute this-command if it wasn't set to ignore
    (unless (eq this-command 'ignore)
      (when (commandp this-command)
        (call-interactively this-command)))
    ;; Run post-command-hook
    (run-hooks 'post-command-hook)))

(defun mule--simulate-cg ()
  "Simulate pressing C-g."
  (let ((this-command (or (keymap-lookup mule-insert-mode-map "C-g")
                          #'keyboard-quit))
        (this-original-command nil))
    (mule--simulate-key [7])))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 1: Basic Mode Transition
  ;;; ---------------------------------------------------------------------------

(ert-deftest mule-cg-exits-insert-to-normal ()
  "C-g in insert mode (no overlays, no mark) should enter normal mode."
  (mule--with-test-buffer
   (mule-enter-insert)
   (should (bound-and-true-p mule-insert-mode))
   (should-not (bound-and-true-p mule-normal-mode))
   (mule--simulate-cg)
   (should (bound-and-true-p mule-normal-mode))
   (should-not (bound-and-true-p mule-insert-mode))))

(ert-deftest mule-cg-normal-mode-lighter ()
  "Modeline lighter should show MULE[N] after C-g from insert."
  (mule--with-test-buffer
   (mule-enter-insert)
   (should (string-match-p "MULE\\[I\\]" (mule-indicator)))
   (mule--simulate-cg)
   (should (string-match-p "MULE\\[N\\]" (mule-indicator)))
   (should-not (string-match-p "MULE\\[I\\]" (mule-indicator)))))

(ert-deftest mule-cg-cursor-shape ()
  "Cursor should change from bar to box after C-g from insert."
  (mule--with-test-buffer
   (mule-enter-insert)
   (should (eq cursor-type (default-value 'mule-cursor-insert)))
   (mule--simulate-cg)
   (should (eq cursor-type (default-value 'mule-cursor-normal)))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 2: Smartparens Integration
  ;;; ---------------------------------------------------------------------------

(ert-deftest mule-cg-inside-sp-pair ()
  "C-g inside a smartparens pair should enter normal mode on first press.
  Requires smartparens to be loaded."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (mule--with-test-buffer
   (smartparens-mode 1)
   (mule-enter-insert)
   (forward-char 1)
   (should (bound-and-true-p mule-insert-mode))
   (mule--simulate-cg)
   (should (bound-and-true-p mule-normal-mode))))

(ert-deftest mule-cg-inside-nested-sp-pairs ()
  "C-g inside deeply nested smartparens pairs should enter normal
  mode on first press regardless of nesting depth."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (mule--with-test-buffer
   (smartparens-mode 1)
   (mule-enter-insert)
   (search-forward "bar")
   (backward-char 1)
   (should (bound-and-true-p mule-insert-mode))
   (mule--simulate-cg)
   (should (bound-and-true-p mule-normal-mode))))

(ert-deftest mule-cg-no-sp-post-command-error ()
  "After C-g with smartparens overlays active, no error should be
  signaled in `post-command-hook'."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (mule--with-test-buffer
   (smartparens-mode 1)
   (mule-enter-insert)
   (forward-char 1)
   (let ((errors nil))
     (condition-case err
         (mule--simulate-cg)
       (error (push err errors)))
     (should (null errors)))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 3: Active Region / Mark
  ;;; ---------------------------------------------------------------------------

(ert-deftest mule-cg-with-active-region ()
  "C-g with an active region should enter normal mode and
  deactivate the mark in one press."
  (mule--with-test-buffer
   (mule-enter-insert)
   (set-mark (point))
   (forward-word 1)
   (activate-mark)
   (should (region-active-p))
   (mule--simulate-cg)
   (should (bound-and-true-p mule-normal-mode))
   (should-not (region-active-p))))

(ert-deftest mule-cg-with-region-and-sp-pair ()
  "C-g with both an active region and smartparens overlay should
  enter normal mode on one press."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (mule--with-test-buffer
   (smartparens-mode 1)
   (mule-enter-insert)
   (forward-char 1)
   (set-mark (point))
   (forward-word 1)
   (activate-mark)
   (should (region-active-p))
   (mule--simulate-cg)
   (should (bound-and-true-p mule-normal-mode))
   (should-not (region-active-p))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 4: Minibuffer Safety
  ;;; ---------------------------------------------------------------------------

(ert-deftest mule-cg-in-minibuffer-does-not-transition ()
  "C-g in the minibuffer should NOT trigger MULE state transition.
  The interceptor must check `(minibufferp)' and skip. The direct
  keymap binding (`mule--exit-insert') must also guard against
  minibuffer context."
  (mule--with-test-buffer
   (mule-enter-insert)
   (should (bound-and-true-p mule-insert-mode))
   ;; Mock minibufferp to return t, and keyboard-quit to be a no-op
   (cl-letf (((symbol-function #'minibufferp) (lambda () t))
             ((symbol-function #'keyboard-quit) (lambda () (interactive))))
     (mule--simulate-cg))
   (should (bound-and-true-p mule-insert-mode))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 5: State Verification
  ;;; ---------------------------------------------------------------------------

(ert-deftest mule-cg-normal-keymap-active ()
  "After C-g transition, `mule-normal-mode-map' bindings should be
  active (e.g., 'h' should be `backward-char')."
  (mule--with-test-buffer
   (mule-enter-insert)
   (mule--simulate-cg)
   (should (eq (keymap-lookup (current-active-maps) "h")
               #'backward-char))))

(ert-deftest mule-cg-insert-keymap-disabled ()
  "After C-g transition, `mule-insert-mode-map' should not be in
  the active keymaps."
  (mule--with-test-buffer
   (mule-enter-insert)
   (mule--simulate-cg)
   (should-not (memq mule-insert-mode-map (current-active-maps)))))

(ert-deftest mule-cg-normal-mode-hook-runs ()
  "`mule-normal-mode-hook' should fire after C-g transition."
  (mule--with-test-buffer
   (let ((hook-fired nil))
     (add-hook 'mule-normal-mode-hook
               (lambda () (setq hook-fired t))
               nil t)
     (mule-enter-insert)
     (should-not hook-fired)
     (mule--simulate-cg)
     (should hook-fired))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 6: Excluded Modes Safety
  ;;; ---------------------------------------------------------------------------

(ert-deftest mule-cg-in-excluded-mode ()
  "In excluded modes, MULE should start in insert state.
  C-g should not crash."
  (mule--with-test-buffer
   ;; Temporarily treat fundamental-mode as excluded
   (let ((mule-excluded-modes (cons 'fundamental-mode mule-excluded-modes)))
     (mule-normal-mode -1)
     (mule-insert-mode -1)
     (mule--ensure-default-state)
     (should (bound-and-true-p mule-insert-mode))
     (should-not (bound-and-true-p mule-normal-mode))
     ;; C-g should not crash
     (let ((errors nil))
       (condition-case err
           (mule--simulate-cg)
         (error (push err errors)))
       (should (null errors))))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 7: Input Method Preservation
  ;;; ---------------------------------------------------------------------------

  (ert-deftest mule-cg-input-method-saved-on-normal-entry ()
    "Entering normal mode should save and deactivate input method."
    (mule--with-test-buffer
     (mule-enter-insert)
     (let ((mule--saved-input-method nil))
       (setq current-input-method "TeX")
       (cl-letf (((symbol-function #'deactivate-input-method)
                  (lambda () (setq current-input-method nil))))
         (mule--simulate-cg))
       (should (equal mule--saved-input-method "TeX"))
       (should (null current-input-method))
       (setq current-input-method nil))))

  (ert-deftest mule-cg-input-method-restored-on-insert-entry ()
    "Entering insert mode should restore previously saved input method."
    (mule--with-test-buffer
     (mule-enter-normal)
     (let ((mule--saved-input-method "TeX"))
       (setq current-input-method nil)
       (cl-letf (((symbol-function #'activate-input-method)
                  (lambda (method) (setq current-input-method method))))
         (mule-enter-insert))
       (should (equal current-input-method "TeX"))
       (setq current-input-method nil))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 8: Direct Function Call
  ;;; ---------------------------------------------------------------------------

  (ert-deftest mule-cg-exit-insert-direct-call ()
    "Calling `mule--exit-insert' directly should enter normal mode."
    (mule--with-test-buffer
     (mule-enter-insert)
     (call-interactively #'mule--exit-insert)
     (should (bound-and-true-p mule-normal-mode))))

  (ert-deftest mule-cg-exit-insert-deactivates-mark ()
    "`mule--exit-insert' should deactivate an active region."
    (mule--with-test-buffer
     (mule-enter-insert)
     (set-mark (point))
     (forward-word 1)
     (activate-mark)
     (call-interactively #'mule--exit-insert)
     (should-not (region-active-p))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 9: Repeated C-g Presses
  ;;; ---------------------------------------------------------------------------

  (ert-deftest mule-cg-double-cg-stays-in-normal ()
    "Pressing C-g twice should remain in normal mode, not crash."
    (mule--with-test-buffer
     (mule-enter-insert)
     (mule--simulate-cg)
     (should (bound-and-true-p mule-normal-mode))
     (let ((errors nil))
       (condition-case err
           (mule--simulate-cg)
         (error (push err errors)))
       (should (null errors))
       (should (bound-and-true-p mule-normal-mode)))))

  (ert-deftest mule-cg-then-insert-then-cg ()
    "C-g -> insert -> C-g cycle should work cleanly."
    (mule--with-test-buffer
     (mule-enter-insert)
     (mule--simulate-cg)
     (should (bound-and-true-p mule-normal-mode))
     (mule-enter-insert)
     (mule--simulate-cg)
     (should (bound-and-true-p mule-normal-mode))
     (mule-enter-insert)
     (forward-word 1)
     (mule--simulate-cg)
     (should (bound-and-true-p mule-normal-mode))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 10: Graceful Degradation Without Smartparens
  ;;; ---------------------------------------------------------------------------

  (ert-deftest mule-cg-without-smartparens ()
    "C-g should enter normal mode even when smartparens is not loaded.
  Only runs in environments where smartparens is absent."
    (skip-unless (not (featurep 'smartparens)))
    (mule--with-test-buffer
     (should-not (featurep 'smartparens))
     (mule-enter-insert)
     (mule--simulate-cg)
     (should (bound-and-true-p mule-normal-mode))))

  (ert-deftest mule-cg-no-sp-functions-bound-check ()
    "The `with-eval-after-load' block should not error when
  smartparens is absent or present."
    (should (fboundp 'mule--exit-insert))
    (when (and (featurep 'smartparens)
               (boundp 'smartparens-mode-map))
      (require 'smartparens)
      (should (eq (keymap-lookup smartparens-mode-map "C-g")
                  #'mule--exit-insert))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Runner
  ;;; ---------------------------------------------------------------------------

  (defun mule-run-all-tests ()
    "Run all MULE transition tests interactively."
    (interactive)
    (ert "^mule-cg-" :result-buffer "*MULE Test Results*"))

  (provide 'mule-insert-to-normal-transition-test)

  ;;; mule-insert-to-normal-transition-test.el ends here

;;; donkey-insert-to-normal-transition-test.el --- Comprehensive Tests for Insert→Normal Transition -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)
(defvar this-command)
(defvar this-original-command)
(defvar last-command-event)
  ;;; ---------------------------------------------------------------------------
  ;;; Helper Macros and Utilities
  ;;; ---------------------------------------------------------------------------

(defmacro donkey--with-test-buffer (&rest body)
  "Create a fresh buffer in `fundamental-mode', enable DONKEY, and
  evaluate BODY with point at the start."
  (declare (indent 0))
  `(with-temp-buffer
     (fundamental-mode)
     (donkey -1)
     (donkey 1)
     (donkey--ensure-default-state)
     (donkey-enter-insert)
     (insert "(defun foo ()\n  (let ((x 1))\n    (concat \"bar\" x)))")
     (goto-char (point-min))
     ,@body))

(defun donkey--simulate-key (key)
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

(defun donkey--simulate-cg ()
  "Simulate pressing C-g."
  (let ((this-command (or (keymap-lookup donkey-insert-mode-map "C-g")
                          #'keyboard-quit))
        (this-original-command nil))
    (donkey--simulate-key [7])))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 1: Basic Mode Transition
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
  ;;; Test Group 2: Smartparens Integration
  ;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-inside-sp-pair ()
  "C-g inside a smartparens pair should enter normal mode on first press.
  Requires smartparens to be loaded."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donkey--with-test-buffer
   (smartparens-mode 1)
   (donkey-enter-insert)
   (forward-char 1)
   (should (bound-and-true-p donkey-insert-mode))
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))))

(ert-deftest donkey-cg-inside-nested-sp-pairs ()
  "C-g inside deeply nested smartparens pairs should enter normal
  mode on first press regardless of nesting depth."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donkey--with-test-buffer
   (smartparens-mode 1)
   (donkey-enter-insert)
   (search-forward "bar")
   (backward-char 1)
   (should (bound-and-true-p donkey-insert-mode))
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))))

(ert-deftest donkey-cg-no-sp-post-command-error ()
  "After C-g with smartparens overlays active, no error should be
  signaled in `post-command-hook'."
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

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 3: Active Region / Mark
  ;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-with-active-region ()
  "C-g with an active region should enter normal mode and
  deactivate the mark in one press."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (set-mark (point))
   (forward-word 1)
   (activate-mark)
   (should (region-active-p))
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should-not (region-active-p))))

(ert-deftest donkey-cg-with-region-and-sp-pair ()
  "C-g with both an active region and smartparens overlay should
  enter normal mode on one press."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donkey--with-test-buffer
   (smartparens-mode 1)
   (donkey-enter-insert)
   (forward-char 1)
   (set-mark (point))
   (forward-word 1)
   (activate-mark)
   (should (region-active-p))
   (donkey--simulate-cg)
   (should (bound-and-true-p donkey-normal-mode))
   (should-not (region-active-p))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 4: Minibuffer Safety
  ;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-in-minibuffer-does-not-transition ()
  "C-g in the minibuffer should NOT trigger DONKEY state transition.
  The interceptor must check `(minibufferp)' and skip. The direct
  keymap binding (`donkey--exit-insert') must also guard against
  minibuffer context."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (bound-and-true-p donkey-insert-mode))
   ;; Mock minibufferp to return t, and keyboard-quit to be a no-op
   (cl-letf (((symbol-function #'minibufferp) (lambda () t))
             ((symbol-function #'keyboard-quit) (lambda () (interactive))))
     (donkey--simulate-cg))
   (should (bound-and-true-p donkey-insert-mode))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 5: State Verification
  ;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-normal-keymap-active ()
  "After C-g transition, `donkey-normal-mode-map' bindings should be
  active (e.g., 'h' should be `backward-char')."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
   (should (eq (keymap-lookup (current-active-maps) "h")
               #'backward-char))))

(ert-deftest donkey-cg-insert-keymap-disabled ()
  "After C-g transition, `donkey-insert-mode-map' should not be in
  the active keymaps."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--simulate-cg)
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
  ;;; Test Group 6: Excluded Modes Safety
  ;;; ---------------------------------------------------------------------------

(ert-deftest donkey-cg-in-excluded-mode ()
  "In excluded modes, DONKEY should start in insert state.
  C-g should not crash."
  (donkey--with-test-buffer
   ;; Temporarily treat fundamental-mode as excluded
   (let ((donkey-excluded-modes (cons 'fundamental-mode donkey-excluded-modes)))
     (donkey-normal-mode -1)
     (donkey-insert-mode -1)
     (donkey--ensure-default-state)
     (should (bound-and-true-p donkey-insert-mode))
     (should-not (bound-and-true-p donkey-normal-mode))
     ;; C-g should not crash
     (let ((errors nil))
       (condition-case err
           (donkey--simulate-cg)
         (error (push err errors)))
       (should (null errors))))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Group 7: Input Method Preservation
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
  ;;; Test Group 8: Direct Function Call
  ;;; ---------------------------------------------------------------------------

  (ert-deftest donkey-cg-exit-insert-direct-call ()
    "Calling `donkey--exit-insert' directly should enter normal mode."
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
  ;;; Test Group 9: Repeated C-g Presses
  ;;; ---------------------------------------------------------------------------

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
  ;;; Test Group 10: Graceful Degradation Without Smartparens
  ;;; ---------------------------------------------------------------------------

  (ert-deftest donkey-cg-without-smartparens ()
    "C-g should enter normal mode even when smartparens is not loaded.
  Only runs in environments where smartparens is absent."
    (skip-unless (not (featurep 'smartparens)))
    (donkey--with-test-buffer
     (should-not (featurep 'smartparens))
     (donkey-enter-insert)
     (donkey--simulate-cg)
     (should (bound-and-true-p donkey-normal-mode))))

  (ert-deftest donkey-cg-no-sp-functions-bound-check ()
    "The `with-eval-after-load' block should not error when
  smartparens is absent or present."
    (should (fboundp 'donkey--exit-insert))
    (when (and (featurep 'smartparens)
               (boundp 'smartparens-mode-map))
      (require 'smartparens)
      (should (eq (keymap-lookup smartparens-mode-map "C-g")
                  #'donkey--exit-insert))))

  ;;; ---------------------------------------------------------------------------
  ;;; Test Runner
  ;;; ---------------------------------------------------------------------------

  (defun donkey-run-all-tests ()
    "Run all DONKEY transition tests interactively."
    (interactive)
    (ert "^donkey-cg-" :result-buffer "*DONKEY Test Results*"))

  (provide 'donkey-insert-to-normal-transition-test)

  ;;; donkey-insert-to-normal-transition-test.el ends here

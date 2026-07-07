;;; mule-modal-test.el --- Comprehensive Tests for Insert→Normal Transition -*- lexical-binding: t; -*-

(require 'ert)
(require 'mule-modal)

;;; ---------------------------------------------------------------------------
;;; Helper Macros and Utilities
;;; ---------------------------------------------------------------------------

(defmacro mule--with-test-buffer (&rest body)
  "Create a fresh buffer in `fundamental-mode', enable MULE, and
evaluate BODY with point at the start."
  (declare (indent 0))
  `(with-temp-buffer
     (fundamental-mode)
     (mule-modal 1)
     (mule-enter-insert)
     (insert "(defun foo ()\n  (let ((x 1))\n    (concat \"bar\" x)))")
     (goto-char (point-min))
     ,@body))

(defun mule--simulate-key (key)
  "Simulate pressing KEY by setting `this-single-command-keys' and
running `pre-command-hook', then executing the bound command.
KEY should be a vector, e.g. [7] for C-g."
  (let ((last-command-event (aref key 0))
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

(ert-deftest mule-test-cg-exits-insert-to-normal ()
  "C-g in insert mode (no overlays, no mark) should enter normal mode."
  (mule--with-test-buffer
    (mule-enter-insert)
    (should (bound-and-true-p mule-insert-mode))
    (should-not (bound-and-true-p mule-normal-mode))
    (mule--simulate-cg)
    (should (bound-and-true-p mule-normal-mode))
    (should-not (bound-and-true-p mule-insert-mode))))

(ert-deftest mule-test-cg-normal-mode-lighter ()
  "Modeline lighter should show MULE[N] after C-g from insert."
  (mule--with-test-buffer
    (mule-enter-insert)
    (mule--simulate-cg)
    (should (string-match-p "MULE\\[N\\]" (format-mode-line mode-line)))
    (should-not (string-match-p "MULE\\[I\\]" (format-mode-line mode-line)))))

(ert-deftest mule-test-cg-cursor-shape ()
  "Cursor should change from bar to box after C-g from insert."
  (mule--with-test-buffer
    (mule-enter-insert)
    (should (eq cursor-type (default-value 'mule-cursor-insert)))
    (mule--simulate-cg)
    (should (eq cursor-type (default-value 'mule-cursor-normal)))))

;;; ---------------------------------------------------------------------------
;;; Test Group 2: Smartparens Integration
;;; ---------------------------------------------------------------------------

(ert-deftest mule-test-cg-inside-sp-pair ()
  "C-g inside a smartparens pair should enter normal mode on first press.
Requires smartparens to be loaded."
  (skip-unless (featurep 'smartparens))
  (mule--with-test-buffer
    (smartparens-mode 1)
    (mule-enter-insert)
    ;; Place point inside the outer parens of (defun ...)
    (forward-char 1) ;; After "("
    (should (bound-and-true-p mule-insert-mode))
    (mule--simulate-cg)
    (should (bound-and-true-p mule-normal-mode))))

(ert-deftest mule-test-cg-inside-nested-sp-pairs ()
  "C-g inside deeply nested smartparens pairs should enter normal
mode on first press regardless of nesting depth."
  (skip-unless (featurep 'smartparens))
  (mule--with-test-buffer
    (smartparens-mode 1)
    (mule-enter-insert)
    ;; Navigate to deeply nested position: inside (concat "bar" x)
    (search-forward "bar")
    (backward-char 1) ;; Inside the string, inside concat, inside let, inside defun
    (should (bound-and-true-p mule-insert-mode))
    (mule--simulate-cg)
    (should (bound-and-true-p mule-normal-mode))))

(ert-deftest mule-test-cg-no-sp-post-command-error ()
  "After C-g with smartparens overlays active, no error should be
signaled in `post-command-hook'."
  (skip-unless (featurep 'smartparens))
  (mule--with-test-buffer
    (smartparens-mode 1)
    (mule-enter-insert)
    (forward-char 1) ;; Inside first paren pair
    ;; Capture errors from post-command-hook
    (let ((errors nil))
      (condition-case err
          (mule--simulate-cg)
        (error (push err errors)))
      (should (null errors)))))

;;; ---------------------------------------------------------------------------
;;; Test Group 3: Active Region / Mark
;;; ---------------------------------------------------------------------------

(ert-deftest mule-test-cg-with-active-region ()
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

(ert-deftest mule-test-cg-with-region-and-sp-pair ()
  "C-g with both an active region and smartparens overlay should
enter normal mode on one press."
  (skip-unless (featurep 'smartparens))
  (mule--with-test-buffer
    (smartparens-mode 1)
    (mule-enter-insert)
    (forward-char 1) ;; Inside first paren pair
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

(ert-deftest mule-test-cg-in-minibuffer-does-not-transition ()
  "C-g in the minibuffer should NOT trigger MULE state transition.
The interceptor must check `(minibufferp)' and skip."
  (mule--with-test-buffer
    (mule-enter-insert)
    (should (bound-and-true-p mule-insert-mode))
    ;; Simulate minibuffer context
    (let ((minibufferp t))
      (mule--simulate-cg))
    ;; Should still be in insert mode
    (should (bound-and-true-p mule-insert-mode))))

;;; ---------------------------------------------------------------------------
;;; Test Group 5: State Verification
;;; ---------------------------------------------------------------------------

(ert-deftest mule-test-cg-normal-keymap-active ()
  "After C-g transition, `mule-normal-mode-map' bindings should be
active (e.g., 'h' should be `backward-char')."
  (mule--with-test-buffer
    (mule-enter-insert)
    (mule--simulate-cg)
    (should (eq (keymap-lookup (current-active-maps) "h")
                #'backward-char))))

(ert-deftest mule-test-cg-insert-keymap-disabled ()
  "After C-g transition, `mule-insert-mode-map' should not be in
the active keymaps."
  (mule--with-test-buffer
    (mule-enter-insert)
    (mule--simulate-cg)
    (should-not (memq mule-insert-mode-map (current-active-maps)))))

(ert-deftest mule-test-cg-normal-mode-hook-runs ()
  "`mule-normal-mode-hook' should fire after C-g transition."
  (mule--with-test-buffer
    (let ((hook-fired nil))
      (add-hook 'mule-normal-mode-hook
                (lambda () (setq hook-fired t))
                nil t) ;; buffer-local
      (mule-enter-insert)
      (should-not hook-fired)
      (mule--simulate-cg)
      (should hook-fired))))

;;; ---------------------------------------------------------------------------
;;; Test Group 6: Excluded Modes Safety
;;; ---------------------------------------------------------------------------

(ert-deftest mule-test-cg-in-excluded-mode ()
  "In excluded modes (e.g., `dired-mode'), MULE should use insert
state and C-g should not crash."
  (skip-unless (fboundp 'dired-mode))
  (let* ((dir (make-temp-file "mule-test-" :dir))
         (buf (find-file-noselect dir)))
    (unwind-protect
        (with-current-buffer buf
          (dired-mode)
          (mule-modal 1)
          ;; Should be in insert state (passthrough) in excluded modes
          (should (bound-and-true-p mule-insert-mode))
          (should-not (bound-and-true-p mule-normal-mode))
          ;; C-g should not crash even in excluded mode
          (mule--simulate-cg)
          ;; Should remain in insert state in excluded mode
          (should (bound-and-true-p mule-insert-mode)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory dir t))))

;;; ---------------------------------------------------------------------------
;;; Test Group 7: Input Method Preservation
;;; ---------------------------------------------------------------------------

(ert-deftest mule-test-input-method-saved-on-normal-entry ()
  "Entering normal mode should save and deactivate input method."
  (mule--with-test-buffer
    (mule-enter-insert)
    (let ((mule--saved-input-method nil)
          (current-input-method "TeX"))
      (mule--simulate-cg)
      (should (equal mule--saved-input-method "TeX"))
      (should (null current-input-method)))))

(ert-deftest mule-test-input-method-restored-on-insert-entry ()
  "Entering insert mode should restore previously saved input method."
  (mule--with-test-buffer
    (mule-enter-normal)
    (let ((mule--saved-input-method "TeX"))
      (mule-enter-insert)
      (should (equal current-input-method "TeX")))))

;;; ---------------------------------------------------------------------------
;;; Test Group 8: Direct Function Call
;;; ---------------------------------------------------------------------------

(ert-deftest mule-test-exit-insert-direct-call ()
  "Calling `mule--exit-insert' directly should enter normal mode."
  (mule--with-test-buffer
    (mule-enter-insert)
    (call-interactively #'mule--exit-insert)
    (should (bound-and-true-p mule-normal-mode))))

(ert-deftest mule-test-exit-insert-deactivates-mark ()
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

(ert-deftest mule-test-double-cg-stays-in-normal ()
  "Pressing C-g twice should remain in normal mode, not crash."
  (mule--with-test-buffer
    (mule-enter-insert)
    (mule--simulate-cg)
    (should (bound-and-true-p mule-normal-mode))
    ;; Second C-g — should run `keyboard-quit' (global) without error
    (let ((errors nil))
      (condition-case err
          (mule--simulate-cg)
        (error (push err errors)))
      (should (null errors))
      (should (bound-and-true-p mule-normal-mode)))))

(ert-deftest mule-test-cg-then-insert-then-cg ()
  "C-g → insert → C-g cycle should work cleanly."
  (mule--with-test-buffer
    ;; First cycle
    (mule-enter-insert)
    (mule--simulate-cg)
    (should (bound-and-true-p mule-normal-mode))
    ;; Second cycle
    (mule-enter-insert)
    (mule--simulate-cg)
    (should (bound-and-true-p mule-normal-mode))
    ;; Third cycle with movement
    (mule-enter-insert)
    (forward-word 1)
    (mule--simulate-cg)
    (should (bound-and-true-p mule-normal-mode))))

;;; ---------------------------------------------------------------------------
;;; Test Group 10: Graceful Degradation Without Smartparens
;;; ---------------------------------------------------------------------------

(ert-deftest mule-test-cg-without-smartparens ()
  "C-g should enter normal mode even when smartparens is not
loaded. The interceptor and keymap binding must handle this case."
  (let ((sp-loaded (featurep 'smartparens)))
    (when sp-loaded
      (unload-feature 'smartparens))
    (unwind-protect
        (mule--with-test-buffer
          (should-not (featurep 'smartparens))
          (mule-enter-insert)
          (mule--simulate-cg)
          (should (bound-and-true-p mule-normal-mode)))
      (when sp-loaded
        (require 'smartparens)))))

(ert-deftest mule-test-cg-no-sp-functions-bound-check ()
  "The `with-eval-after-load' block should not error when
smartparens is absent."
  (should-not (boundp 'sp-keymap))
  (should-not (boundp 'sp-overlay-map))
  ;; This should not signal any error
  (should (fboundp 'mule--exit-insert)))

;;; ---------------------------------------------------------------------------
;;; Test Runner
;;; ---------------------------------------------------------------------------

(defun mule-run-all-tests ()
  "Run all MULE transition tests interactively."
  (interactive)
  (ert '(tag mule) :result-buffer "*MULE Test Results*"))

(provide 'mule-modal-test)
;;; mule-modal-test.el ends here

(ert "mule-test")

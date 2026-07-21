;;; donky-insert-to-normal-transition-second-test.el --- Comprehensive Tests for Insert to Normal Transition -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donky)

;; Declare built-in dynamic variables so let-bindings are visible to hooks
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

(defun donky--simulate-cg ()
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

(defun donky--simulate-key (key)
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
  "C-g with active region should enter normal mode and deactivate mark."
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
  "C-g with region and smartparens overlay should enter normal mode."
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
  "C-g in the minibuffer should NOT trigger DONKY state transition."
  (donky--with-test-buffer
   (donky-enter-insert)
   (should (bound-and-true-p donky-insert-mode))
   (cl-letf (((symbol-function #'minibufferp) (lambda () t)))
     (donky--simulate-cg))
   (should (bound-and-true-p donky-insert-mode))))

;;; ---------------------------------------------------------------------------
;;; Test Group 5: State Verification
;;; ---------------------------------------------------------------------------

(ert-deftest donky-cg-normal-keymap-active ()
  "After C-g transition, normal-mode-map bindings should be active."
  (donky--with-test-buffer
   (donky-enter-insert)
   (donky--simulate-cg)
   (should (bound-and-true-p donky-normal-mode))
   (should (eq (keymap-lookup (current-active-maps) "h")
               #'backward-char))))

(ert-deftest donky-cg-insert-keymap-disabled ()
  "After C-g transition, donky-insert-mode-map should NOT be active."
  (donky--with-test-buffer
   (donky-enter-insert)
   (donky--simulate-cg)
   (should (bound-and-true-p donky-normal-mode))
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
  "In excluded modes, DONKY should start in insert state. C-g should not crash."
  (donky--with-test-buffer
   (let ((donky-excluded-modes (cons 'fundamental-mode donky-excluded-modes)))
     (donky-normal-mode -1)
     (donky-insert-mode -1)
     (donky--ensure-default-state)
     (should (bound-and-true-p donky-insert-mode))
     (should-not (bound-and-true-p donky-normal-mode))
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
  "Calling donky--exit-insert directly should enter normal mode."
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
;;; Test Group 9: Repeated C-g Presses (Guard Race Condition Tests)
;;; ---------------------------------------------------------------------------

(ert-deftest donky-cg-guard-prevents-double-execution ()
  "The guard should prevent double-execution when C-g is pressed rapidly."
  (donky--with-test-buffer
   (donky-enter-insert)
   (donky--simulate-cg)
   (should (bound-and-true-p donky-normal-mode))
   (should donky--just-exited-from-insert)
   (donky--simulate-key [104])
   (should-not donky--just-exited-from-insert)))

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
;;; Test Group 10: Overlay Cleanup (Transient Faces Variants)
;;; ---------------------------------------------------------------------------
;; NOTE: delete-overlay removes the overlay from the buffer but the overlay
;; object still exists. We check (overlay-start ov) returns nil to verify
;; deletion, since overlayp still returns t for detached overlay objects.

(ert-deftest donky-clear-overlays-with-sp-show-pair-face ()
  "`donky--clear-transient-overlays' should delete overlays with sp-show-pair-match-face."
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'sp-show-pair-match-face)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donky-clear-overlays-with-sp-mismatch-face ()
  "`donky--clear-transient-overlays' should delete overlays with sp-show-pair-mismatch-face."
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'sp-show-pair-mismatch-face)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donky-clear-overlays-with-show-paren-match-face ()
  "`donky--clear-transient-overlays' should delete overlays with show-paren-match face."
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'show-paren-match)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donky-clear-overlays-with-show-paren-mismatch-face ()
  "`donky--clear-transient-overlays' should delete overlays with show-paren-mismatch face."
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'show-paren-mismatch)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donky-clear-overlays-with-hl-paren-face ()
  "`donky--clear-transient-overlays' should delete overlays with hl-paren-face."
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'hl-paren-face)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donky-clear-overlays-preserves-non-transient-faces ()
  "`donky--clear-transient-overlays' should NOT delete overlays with non-transient faces."
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'highlight)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should (overlay-start ov)))))

;;; ---------------------------------------------------------------------------
;;; Test Group 11: Overlay Keymap Property Cleanup (Strategy 3)
;;; ---------------------------------------------------------------------------

(ert-deftest donky-clear-overlays-with-smartparens-overlay-keymap ()
  "`donky--clear-transient-overlays' Strategy 3 should delete overlays carrying sp-overlay-keymap."
  (skip-unless (boundp 'sp-overlay-keymap))
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'keymap sp-overlay-keymap)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donky-clear-overlays-with-sp-pair-overlay-keymap ()
  "`donky--clear-transient-overlays' Strategy 3 should delete overlays carrying sp-pair-overlay-keymap."
  (skip-unless (boundp 'sp-pair-overlay-keymap))
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'keymap sp-pair-overlay-keymap)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donky-clear-overlays-keeps-non-sp-keymap-overlays ()
  "`donky--clear-transient-overlays' should NOT delete overlays carrying unrelated keymaps."
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((dummy-map (make-sparse-keymap))
         (ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'keymap dummy-map)
     (overlay-put ov 'donky-test t)
     (should (overlay-start ov))
     (donky--clear-transient-overlays)
     (should (overlay-start ov)))))

;;; ---------------------------------------------------------------------------
;;; Test Group 12: Deferred Cleanup Timer
;;; ---------------------------------------------------------------------------

(ert-deftest donky-schedule-overlay-cleanup-creates-timer ()
  "`donky--schedule-overlay-cleanup' should create a deferred timer."
  (donky--with-test-buffer
   (donky-enter-insert)
   (should-not donky--deferred-overlay-cleanup-timer)
   (donky--schedule-overlay-cleanup)
   (should (timerp donky--deferred-overlay-cleanup-timer))))

(ert-deftest donky-schedule-overlay-cleanup-cancels-existing-timer ()
  "`donky--schedule-overlay-cleanup' should cancel existing timer before creating new one."
  (donky--with-test-buffer
   (donky-enter-insert)
   (donky--schedule-overlay-cleanup)
   (let ((old-timer donky--deferred-overlay-cleanup-timer))
     (donky--schedule-overlay-cleanup)
     (should (timerp donky--deferred-overlay-cleanup-timer))
     (should (not (eq donky--deferred-overlay-cleanup-timer old-timer))))))

;;; ---------------------------------------------------------------------------
;;; Test Group 13: sp-cancel Detection (Race Condition Fix)
;;; ---------------------------------------------------------------------------

(ert-deftest donky-intercept-sp-cancel-command ()
  "`donky--intercept-quit-in-insert' should trigger when C-g resolves to sp-cancel."
  (skip-unless (featurep 'smartparens))
  (require 'smartparens)
  (donky--with-test-buffer
   (smartparens-mode 1)
   (donky-enter-insert)
   (forward-char 1)
   (let ((this-command 'sp-cancel))
     (should-not (bound-and-true-p donky-normal-mode))
     (donky--intercept-quit-in-insert)
     (should (bound-and-true-p donky-normal-mode)))))

(ert-deftest donky-intercept-prevents-overlapping-handlers ()
  "When interceptor fires, it should set this-command to ignore."
  (donky--with-test-buffer
   (donky-enter-insert)
   (let ((this-command 'sp-cancel))
     (donky--intercept-quit-in-insert)
     (should (eq this-command 'ignore)))))

;;; ---------------------------------------------------------------------------
;;; Test Group 14: Graceful Degradation Without Smartparens
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
  "The with-eval-after-load block should not error when smartparens is absent or present."
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

    ;;; donky-insert-to-normal-transition-second-test.el ends here

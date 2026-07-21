;;; donkey-insert-to-normal-transition-second-test.el --- Comprehensive Tests for Insert to Normal Transition -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

;; Declare built-in dynamic variables so let-bindings are visible to hooks
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

(ert-deftest donkey-cg-with-region-and-sp-pair ()
  "C-g with region and smartparens overlay should enter normal mode."
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
  "C-g in the minibuffer should NOT trigger DONKEY state transition."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should (bound-and-true-p donkey-insert-mode))
   (cl-letf (((symbol-function #'minibufferp) (lambda () t)))
     (donkey--simulate-cg))
   (should (bound-and-true-p donkey-insert-mode))))

;;; ---------------------------------------------------------------------------
;;; Test Group 5: State Verification
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
;;; Test Group 6: Excluded Modes Safety
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
;;; Test Group 9: Repeated C-g Presses (Guard Race Condition Tests)
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
;;; Test Group 10: Overlay Cleanup (Transient Faces Variants)
;;; ---------------------------------------------------------------------------
;; NOTE: delete-overlay removes the overlay from the buffer but the overlay
;; object still exists. We check (overlay-start ov) returns nil to verify
;; deletion, since overlayp still returns t for detached overlay objects.

(ert-deftest donkey-clear-overlays-with-sp-show-pair-face ()
  "`donkey--clear-transient-overlays' should delete overlays with sp-show-pair-match-face."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'sp-show-pair-match-face)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-with-sp-mismatch-face ()
  "`donkey--clear-transient-overlays' should delete overlays with sp-show-pair-mismatch-face."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'sp-show-pair-mismatch-face)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-with-show-paren-match-face ()
  "`donkey--clear-transient-overlays' should delete overlays with show-paren-match face."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'show-paren-match)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-with-show-paren-mismatch-face ()
  "`donkey--clear-transient-overlays' should delete overlays with show-paren-mismatch face."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'show-paren-mismatch)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-with-hl-paren-face ()
  "`donkey--clear-transient-overlays' should delete overlays with hl-paren-face."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'hl-paren-face)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-preserves-non-transient-faces ()
  "`donkey--clear-transient-overlays' should NOT delete overlays with non-transient faces."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'face 'highlight)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should (overlay-start ov)))))

;;; ---------------------------------------------------------------------------
;;; Test Group 11: Overlay Keymap Property Cleanup (Strategy 3)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-clear-overlays-with-smartparens-overlay-keymap ()
  "`donkey--clear-transient-overlays' Strategy 3 should delete overlays carrying sp-overlay-keymap."
  (skip-unless (boundp 'sp-overlay-keymap))
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'keymap sp-overlay-keymap)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-with-sp-pair-overlay-keymap ()
  "`donkey--clear-transient-overlays' Strategy 3 should delete overlays carrying sp-pair-overlay-keymap."
  (skip-unless (boundp 'sp-pair-overlay-keymap))
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (let ((ov (make-overlay (point) (1+ (point)))))
     (overlay-put ov 'keymap sp-pair-overlay-keymap)
     (overlay-put ov 'donkey-test t)
     (should (overlay-start ov))
     (donkey--clear-transient-overlays)
     (should-not (overlay-start ov)))))

(ert-deftest donkey-clear-overlays-keeps-non-sp-keymap-overlays ()
  "`donkey--clear-transient-overlays' should NOT delete overlays carrying unrelated keymaps."
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
;;; Test Group 12: Deferred Cleanup Timer
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-schedule-overlay-cleanup-creates-timer ()
  "`donkey--schedule-overlay-cleanup' should create a deferred timer."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (should-not donkey--deferred-overlay-cleanup-timer)
   (donkey--schedule-overlay-cleanup)
   (should (timerp donkey--deferred-overlay-cleanup-timer))))

(ert-deftest donkey-schedule-overlay-cleanup-cancels-existing-timer ()
  "`donkey--schedule-overlay-cleanup' should cancel existing timer before creating new one."
  (donkey--with-test-buffer
   (donkey-enter-insert)
   (donkey--schedule-overlay-cleanup)
   (let ((old-timer donkey--deferred-overlay-cleanup-timer))
     (donkey--schedule-overlay-cleanup)
     (should (timerp donkey--deferred-overlay-cleanup-timer))
     (should (not (eq donkey--deferred-overlay-cleanup-timer old-timer))))))

;;; ---------------------------------------------------------------------------
;;; Test Group 13: sp-cancel Detection (Race Condition Fix)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-intercept-sp-cancel-command ()
  "`donkey--intercept-quit-in-insert' should trigger when C-g resolves to sp-cancel."
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
;;; Test Group 14: Graceful Degradation Without Smartparens
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
  "The with-eval-after-load block should not error when smartparens is absent or present."
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

(provide 'donkey-insert-to-normal-transition-second-test)

    ;;; donkey-insert-to-normal-transition-second-test.el ends here

;;; donkey-shuffle.el --- Run the ERT suite in a shuffled order -*- lexical-binding: t; -*-

;; This file is part of DONKEY's test tooling.  It is not a test file and
;; defines no tests; it runs the ones already loaded, in a different order.

;;; Commentary:

;; ERT runs tests in a fixed order, so a test that only passes because of
;; what ran before it stays green forever.  The isolation job catches state
;; leaking BETWEEN files; this catches it between tests.
;;
;; It is not a hypothetical concern.  Eight tests were relying on ambient
;; state when this was first run against a suite that had been green for
;; months: an unbound `kill-ring' (`donkey-yank' checks there is something
;; to paste before acting, so the tests only reached their own mocks
;; because an earlier test had stocked the ring), a `current-prefix-arg'
;; left set by `execute-kbd-macro' after a counted key, a
;; `temporary-goal-column' that walked a rectangle to the wrong column, a
;; `this-command' that made a key-simulation helper run some other
;; command entirely, and `donkey-mode' left on globally.
;;
;; To run it: start a batch Emacs from the repository root with "." and
;; "donkey-testing" on the load path, load every "*-test.el" file, load
;; this one, then call `donkey-shuffle-run' with a seed.  The "shuffled"
;; job in .github/workflows/ci.yml spells that out as a shell loop.
;;
;; Exits non-zero if any test fails.  A failing SEED reproduces exactly:
;; the shuffle is a small LCG rather than `random', so the same seed gives
;; the same order on any machine and any Emacs version.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defun donkey-shuffle--order (names seed)
  "Return NAMES permuted by a Fisher-Yates shuffle driven by SEED.

A self-contained linear congruential generator rather than `random',
so that a seed which fails in CI reproduces byte for byte locally."
  (let ((v (vconcat names))
        (state seed))
    (cl-loop for i from (1- (length v)) downto 1
             do (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
             (let ((j (mod state (1+ i)))
                   (tmp (aref v i)))
               (aset v i (aref v j))
               (aset v j tmp)))
    (append v nil)))

(defun donkey-shuffle-run (seed)
  "Run every loaded ERT test once, in the order given by SEED.

Exits with status 1 if any test fails, naming each one, and 0
otherwise.  Skipped tests are counted but are not failures."
  (let ((order (donkey-shuffle--order
                (mapcar #'ert-test-name (ert-select-tests t t))
                seed))
        (failed nil)
        (skipped 0)
        (ran 0))
    (dolist (name order)
      (let ((result (ert-run-test (ert-get-test name))))
        (setq ran (1+ ran))
        (cond ((ert-test-skipped-p result) (setq skipped (1+ skipped)))
              ((not (ert-test-passed-p result)) (push name failed)))))
    (setq failed (nreverse failed))
    (princ (format "\nseed %d: ran %d, %d skipped, %d failed\n"
                   seed ran skipped (length failed)))
    (dolist (name failed)
      (princ (format "  FAILED %s\n" name)))
    (kill-emacs (if failed 1 0))))

(provide 'donkey-shuffle)

;;; donkey-shuffle.el ends here

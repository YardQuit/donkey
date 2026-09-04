;;; donkey-test-keys.el --- Shared displayed-buffer key harness -*- lexical-binding: t -*-

;;; Commentary:

;; The one way to test a DONKEY key that means anything: real keys
;; through `execute-kbd-macro', in a buffer the selected window shows.
;; Three test files each grew their own copy of this harness, and each
;; copy re-learned the same lessons the hard way.  They are recorded
;; once, here, on the macro every copy now delegates to.

;;; Code:

(defvar donkey-test-keys--said nil
  "Last message emitted by the keys run through `donkey-test-keys--harness'.")

(defmacro donkey-test-keys--harness (name mode bindings text keys &rest body)
  "Type KEYS into a displayed DONKEY buffer of TEXT, then run BODY.

NAME is the scratch buffer's name, killed before and after so no state
survives between tests.  MODE is the major-mode function to enable.
BINDINGS is a `let' binding list spliced around the key run, for
whatever the calling file's tests additionally need pinned.

The shape below is load-bearing in four places, each learned from a
test that lied:

The buffer is SWITCHED TO, not merely current.  The command loop acts
on the selected window's buffer, so keys sent into `with-temp-buffer'
land in whatever buffer is showing instead -- early probes of this
package read entire result sets from the wrong buffer that way.

Keys go through `execute-kbd-macro', not direct calls.  Selection and
repeat state are settled by the command loop -- the variable
`deactivate-mark', `last-command' -- so a directly called command
always looks as though it kept the selection and never looks like a
repeat.  Tests written that way passed while the real key failed.

`prefix-arg' and `current-prefix-arg' are bound, because KEYS may carry
a \\[universal-argument] and `execute-kbd-macro' leaves the prefix set
GLOBALLY afterwards -- it once sent a later, unrelated test's
`set-mark-command' down its pop-the-mark-ring branch, failing only in
the full run.

The kill ring, `killed-rectangle', and the clipboard hooks are all
isolated, so a test neither reads the machine's clipboard nor writes
it, and \"saved nothing\" stays distinguishable from \"found something
already there\".

Messages are captured into `donkey-test-keys--said' (last one wins)
while still reaching the real `message', so a test can assert what the
user was told without silencing the run."
  (declare (indent 5))
  `(unwind-protect
       (progn
         (when (get-buffer ,name) (kill-buffer ,name))
         (switch-to-buffer (get-buffer-create ,name))
         (funcall ,mode)
         (donkey-mode 1)
         (let ((transient-mark-mode t)
               (prefix-arg nil) (current-prefix-arg nil)
               ;; No input from outside the test.  A terminal frame can
               ;; leave bytes in the queue before anything here runs --
               ;; `script', which the CI job uses to give Emacs a pty,
               ;; leaves a NUL -- and `execute-kbd-macro' spends what is
               ;; already pending BEFORE the keys it was given.  A NUL is
               ;; `C-@' is `set-mark-command', so the first test in the
               ;; run to type anything got a mark pushed at point and
               ;; activated, and then its own first key on top.  That is
               ;; how `M' came to adopt a selection its test never made.
               ;; Only the first test paid, the queue being empty after,
               ;; which is why it moved about and why no --batch job ever
               ;; saw it.
               (unread-command-events nil)
               (inhibit-message t)
               (kill-ring nil) (kill-ring-yank-pointer nil)
               (killed-rectangle nil)
               (select-enable-clipboard nil)
               (interprogram-cut-function nil)
               (interprogram-paste-function nil)
               (donkey-test-keys--said nil)
               ,@bindings)
           (insert ,text)
           (goto-char (point-min))
           (donkey-normal-mode 1)
           (cl-letf* ((orig (symbol-function 'message))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when fmt
                           (setq donkey-test-keys--said
                                 (apply #'format fmt args)))
                         (apply orig fmt args))))
             (execute-kbd-macro (kbd ,keys)))
           ,@body))
     (when (get-buffer ,name) (kill-buffer ,name))))

(provide 'donkey-test-keys)

;;; donkey-test-keys.el ends here

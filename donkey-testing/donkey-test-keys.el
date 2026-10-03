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

(defconst donkey-test-keys--clipboard-bindings
  '((select-enable-clipboard nil)
    (interprogram-cut-function nil)
    (interprogram-paste-function nil))
  "Bindings that keep a test off the machine\\='s clipboard.

One list, for every harness to splice, because naming the three in
each of them is how seven of them came to be missing all three.  A test
must neither read the clipboard nor write it: reading makes the test
depend on whatever the desktop is holding, and writing throws away
something the person running the suite meant to keep.

Neither shows under `--batch\\=' or in a terminal frame -- there is no
clipboard to reach -- so no CI job here can fail on it.  It took running
the suite in a GRAPHICAL frame to see either.  Reading:
`interprogram-paste-function\\=' is `gui-selection-value\\=' there, so
`yank\\=' takes the desktop selection over the test\\='s own kill ring, and
`donkey-p-and-P-keep-their-paste-jobs-inside-the-mode\\=' pasted
\"CLIPBOARD-CONTENT\" over its own \"XX\".  Writing: `kill-new\\=' reaches
`interprogram-cut-function\\=', and a run of the suite left \"ZZZ\" on the
clipboard of the machine it ran on.")

;; Declared here so the bindings below are dynamic in this file;
;; select.el defines all six and is preloaded.
(defvar gui--last-selected-text-clipboard)
(defvar gui--last-selected-text-primary)
(defvar gui--last-selection-timestamp-clipboard)
(defvar gui--last-selection-timestamp-primary)
(defvar gui-last-cut-in-clipboard)
(defvar gui-last-cut-in-primary)

(defvar donkey-test-keys--clipboard nil
  "What the fake system clipboard of `donkey-test-keys--with-clipboard' holds.")

(defvar donkey-test-keys--clipboard-reads 0
  "How often `donkey-test-keys--with-clipboard' was asked for the clipboard.")

(defmacro donkey-test-keys--with-clipboard (clipboard &rest body)
  "Run BODY with a fake system clipboard holding CLIPBOARD, a string or nil.

The fake sits under Emacs\\='s own selection code: `gui-get-selection'
and `gui-set-selection' read and write `donkey-test-keys--clipboard',
and everything above them -- `gui-select-text', `gui-selection-value'
with its test of whether the clipboard changed, `current-kill',
`kill-new' -- runs for real.  So a copy reaches the fake clipboard, a
paste reads it, and what a paste adds to the `kill-ring' is what a real
session gets.  Nothing reaches the machine\\='s clipboard.

`donkey-test-keys--harness' switches the clipboard off again with
`donkey-test-keys--clipboard-bindings'; `donkey-test-keys--clipboard-harness'
is the harness with it on.  Reads are counted in
`donkey-test-keys--clipboard-reads'."
  (declare (indent 1))
  `(let ((donkey-test-keys--clipboard ,clipboard)
         (donkey-test-keys--clipboard-reads 0)
         (gui--last-selected-text-clipboard nil)
         (gui--last-selected-text-primary nil)
         (gui--last-selection-timestamp-clipboard nil)
         (gui--last-selection-timestamp-primary nil)
         (gui-last-cut-in-clipboard nil)
         (gui-last-cut-in-primary nil)
         ;; `gui-select-text' leaves a copy here for `deactivate-mark'
         ;; to put in PRIMARY; bound so it cannot outlive the test.
         (saved-region-selection nil))
     (cl-letf (((symbol-function 'gui-get-selection)
                (lambda (&optional type _data-type)
                  (when (eq type 'CLIPBOARD)
                    (setq donkey-test-keys--clipboard-reads
                          (1+ donkey-test-keys--clipboard-reads))
                    (and donkey-test-keys--clipboard
                         (copy-sequence donkey-test-keys--clipboard)))))
               ((symbol-function 'gui-set-selection)
                (lambda (type data)
                  (when (and (eq type 'CLIPBOARD) (stringp data))
                    (setq donkey-test-keys--clipboard
                          (substring-no-properties data)))
                  data)))
       ,@body)))

(defmacro donkey-test-keys--clipboard-harness (name mode clipboard bindings text keys &rest body)
  "Run `donkey-test-keys--harness' with a fake system clipboard.

NAME, MODE, BINDINGS, TEXT, KEYS and BODY are the harness\\='s.
CLIPBOARD is what the clipboard holds as the keys start, a string or
nil; `donkey-test-keys--with-clipboard' says how the fake works.  The
clipboard is on, as it is in a graphical session: kills reach it
through `gui-select-text' and pastes read it through
`gui-selection-value'.  BINDINGS come after those and may change them,
`select-enable-clipboard' included."
  (declare (indent 6))
  `(donkey-test-keys--with-clipboard ,clipboard
     (donkey-test-keys--harness ,name ,mode
         ((interprogram-cut-function #'gui-select-text)
          (interprogram-paste-function #'gui-selection-value)
          (select-enable-clipboard t)
          (select-enable-primary nil)
          ,@bindings)
         ,text ,keys
       ,@body)))

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

`overriding-terminal-local-map' is saved and restored, so a transient
map armed while the keys run cannot outlive them.  The mark run's own
is taken down by `donkey--mark-run-exit' at the end; this covers every
other, `repeat' included.

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
               ;; Saved and restored, so a transient map armed during
               ;; the keys is gone when they are done -- whosever it
               ;; is.  `donkey--mark-run-exit' below takes down the
               ;; mark run's, and is no use against anybody else's:
               ;; `.' is `repeat', and `repeat' arms one of its own on
               ;; top, which two tests left standing terminal-wide for
               ;; whatever ran next.  It outranks `overriding-local-map'
               ;; and every emulation map, so the test it landed on
               ;; looked up its keys through a map it never set.
               (overriding-terminal-local-map overriding-terminal-local-map)
               ,@donkey-test-keys--clipboard-bindings
               (donkey-test-keys--said nil)
               ;; The macro's last command must not outlive the test:
               ;; a later direct call reads `this-command' as its own.
               (this-command nil) (last-command nil)
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
     ;; A macro that ends on one of the mark run's letters leaves its
     ;; map armed, terminal-wide, for whatever test runs next.
     (donkey--mark-run-exit)
     ;; So does a split left at its verb menu, and the next test's first
     ;; key would end it there -- reporting into that test's messages.
     ;; Taken down quietly while its buffer is still alive.
     (donkey--split-dissolve t)
     (when (get-buffer ,name) (kill-buffer ,name))))

(provide 'donkey-test-keys)

;;; donkey-test-keys.el ends here

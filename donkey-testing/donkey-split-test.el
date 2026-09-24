;;; donkey-split-test.el --- Tests for DONKEY Split mode -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'rect)
(require 'donkey)
(require 'donkey-test-keys)

(defmacro donkey-split-test--keys (name text keys &rest body)
  "Run KEYS over TEXT in a displayed buffer named NAME, then BODY.

`read-regexp' is stubbed to the last regexp a test asked for, so a
split can be driven by real keys without a minibuffer."
  (declare (indent 3))
  `(donkey-test-keys--harness ,name #'text-mode () ,text ,keys ,@body))

(defvar donkey-split-test--regexp "foo"
  "What the stubbed `read-regexp' answers for the test being run.")

(defmacro donkey-split-test--on (regexp &rest body)
  "Answer `read-regexp' with REGEXP inside BODY."
  (declare (indent 1))
  `(let ((donkey-split-test--regexp ,regexp))
     (cl-letf (((symbol-function 'read-regexp)
                (lambda (&rest _) donkey-split-test--regexp)))
       ,@body)))

;;; ---------------------------------------------------------------------------
;;; What the split itself does
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-split-holds-every-match-and-changes-nothing ()
  "The split selects: the buffer is untouched until a verb is pressed."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-hold*" "a foo b\nc foo d\n" "v G f"
      (should (equal (buffer-string) "a foo b\nc foo d\n"))
      (should (eq donkey--split-phase 'select))
      (should (= (length donkey--split-places) 2)))))

(ert-deftest donkey-split-refuses-matches-that-do-not-agree ()
  "A regexp matching different text at each place is refused.

Signals rather than reports, as `donkey-yank-rectangle' does over a row
mismatch: the reader asked for something the split cannot carry, and
what is typed at one place would replace the rest."
  (donkey-split-test--keys "*split-differ*" "alpha 1\nbeta 2\n" "v G"
    (should-error (donkey-split "^[a-z]+") :type 'user-error)
    (should (null donkey--split-places))
    (should (equal (buffer-string) "alpha 1\nbeta 2\n"))))

(ert-deftest donkey-split-says-nothing-matched-when-nothing-does ()
  "A regexp with no match leaves no split and says so."
  (donkey-split-test--on "zzz"
    (donkey-split-test--keys "*split-none*" "a foo b\n" "v G f"
      (should (null donkey--split-places))
      (should (string-match-p "Nothing matched" donkey-test-keys--said)))))

;;; ---------------------------------------------------------------------------
;;; The verbs
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-split-i-types-before-every-place ()
  "`i' puts point at each place's start, as `donkey-insert-here' does."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-i*" "a foo b\nc foo d\n" "v G f i X"
      (should (equal (buffer-string) "a Xfoo b\nc Xfoo d\n")))))

(ert-deftest donkey-split-a-types-after-every-place ()
  "`a' puts point at each place's end, as `donkey-insert-after' does."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-a*" "a foo b\nc foo d\n" "v G f a X"
      (should (equal (buffer-string) "a fooX b\nc fooX d\n")))))

(ert-deftest donkey-split-c-empties-every-place-before-typing ()
  "`c' clears the places, then what is typed appears at all of them."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-c*" "a foo b\nc foo d\n" "v G f c X"
      (should (equal (buffer-string) "a X b\nc X d\n")))))

(ert-deftest donkey-split-d-deletes-every-place-and-ends-the-split ()
  "`d' removes what the places hold and leaves no split behind."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-d*" "a foo b\nc foo d\n" "v G f d"
      (should (equal (buffer-string) "a  b\nc  d\n"))
      (should (null donkey--split-places))
      (should-not (memq #'donkey--split-flush kill-buffer-hook)))))

(ert-deftest donkey-split-c-and-d-save-one-copy-not-one-per-place ()
  "What a split replaces reaches the kill ring once, since the places agree."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-kill*" "a foo b\nc foo d\ne foo f\n"
        "v G f d"
      (should (equal (car kill-ring) "foo"))
      (should (= (length kill-ring) 1)))))

(ert-deftest donkey-split-i-saves-nothing-because-it-removes-nothing ()
  "`i' keeps what the places hold, so the kill ring is untouched."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-i-kill*" "a foo b\nc foo d\n" "v G f i X"
      (should (null kill-ring)))))

;;; ---------------------------------------------------------------------------
;;; Wrapping
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-split-a-delimiter-wraps-every-place-on-its-own-key ()
  "A delimiter pressed in Split mode wraps all the places at once."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-wrap*" "a foo b\nc foo d\n" "v G f ("
      (should (equal (buffer-string) "a (foo) b\nc (foo) d\n")))))

(ert-deftest donkey-split-the-closing-half-wraps-the-same-as-the-opening ()
  "Either half of a pair names it, as `donkey-wrap-region' allows."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-wrap-close*" "a foo b\nc foo d\n"
        "v G f )"
      (should (equal (buffer-string) "a (foo) b\nc (foo) d\n")))))

(ert-deftest donkey-split-wrapping-twice-in-one-pair-takes-it-off ()
  "The second press unwraps, the way `donkey-wrap-region' toggles."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-unwrap*" "a foo b\nc foo d\n" "v G f ( ("
      (should (equal (buffer-string) "a foo b\nc foo d\n")))))

(ert-deftest donkey-split-a-wrap-leaves-the-split-standing-for-a-verb ()
  "Wrapping does not end the split: another pair nests, and a verb follows."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-wrap-then*" "a foo b\nc foo d\n"
        "v G f ( [ a X"
      (should (equal (buffer-string) "a ([fooX]) b\nc ([fooX]) d\n")))))

;;; ---------------------------------------------------------------------------
;;; What is searched
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-split-with-no-selection-searches-this-line-only ()
  "No selection is the current line, so the whole buffer is never fallen into."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-line*" "a foo b\nc foo d\n" "f a X"
      (should (equal (buffer-string) "a fooX b\nc foo d\n")))))

(ert-deftest donkey-split-searches-only-inside-a-rectangle ()
  "A rectangle searches the block, not the rows it covers."
  (donkey-split-test--on "zz"
    (donkey-split-test--keys "*split-rect*" "ab foo zz\ncd foo zz\n"
        "m v j l l l l l l f"
      (should (null donkey--split-places)))))

(ert-deftest donkey-split-with-a-count-searches-each-rows-whole-line ()
  "A prefix argument widens a rectangle to the rows it covers."
  (donkey-split-test--on "zz"
    (donkey-split-test--keys "*split-rect-wide*" "ab foo zz\ncd foo zz\n"
        "m v j l l l l l l C-u f a !"
      (should (equal (buffer-string) "ab foo zz!\ncd foo zz!\n")))))

;;; ---------------------------------------------------------------------------
;;; Banked lines
;;; ---------------------------------------------------------------------------

(defconst donkey-split-test--four "a foo\nb foo\nc foo\nd foo\n"
  "Four lines that each hold one match.")

(ert-deftest donkey-split-searches-the-banked-lines-and-spends-the-bank ()
  "Banked lines are the scope, and opening the split spends them."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-bank*" donkey-split-test--four
        "V m l j j V m l f a X"
      (should (equal (buffer-string) "a fooX\nb foo\nc fooX\nd foo\n"))
      (should (null (donkey--banked-spans))))))

(ert-deftest donkey-split-a-line-start-stays-on-the-banked-lines ()
  "`^' holds the start of each banked line, not of the line after it."
  (donkey-split-test--on "^"
    (donkey-split-test--keys "*split-bank-bol*" donkey-split-test--four
        "V m l j j V m l f i >"
      (should (equal (buffer-string) ">a foo\nb foo\n>c foo\nd foo\n")))))

(ert-deftest donkey-split-a-region-joins-the-bank-exactly-as-selected ()
  "A live region is searched with the bank, and is not widened to its lines."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-bank-v*" donkey-split-test--four
        "V m l j j g h v l f"
      (should (= (length donkey--split-places) 1))
      (should (= (line-number-at-pos (overlay-start
                                      (car donkey--split-places)))
                 1))))
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-bank-v-in*" donkey-split-test--four
        "V m l j j g h v g l f a X"
      (should (equal (buffer-string) "a fooX\nb foo\nc fooX\nd foo\n")))))

(ert-deftest donkey-split-holds-a-match-once-where-region-and-bank-overlap ()
  "A region over a banked line does not hold its match twice."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-bank-overlap*" donkey-split-test--four
        "V m l g h v g l f"
      (should (= (length donkey--split-places) 1)))))

(ert-deftest donkey-split-with-a-bank-leaves-the-cursor-line-out ()
  "The bank is the selection, so the line under point is not added."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-bank-cursor*" donkey-split-test--four
        "V m l j j f a X"
      (should (equal (buffer-string) "a fooX\nb foo\nc foo\nd foo\n")))))

(ert-deftest donkey-split-that-does-not-open-keeps-the-bank ()
  "No match, and matches that differ, leave the bank standing."
  (donkey-split-test--on "zzz"
    (donkey-split-test--keys "*split-bank-none*" donkey-split-test--four
        "V m l f"
      (should (= (length (donkey--banked-spans)) 1))))
  (donkey-split-test--keys "*split-bank-differ*" "a foo\nb fox\n" "V j m l"
    (should-error (donkey-split "fo.") :type 'user-error)
    (should (= (donkey--banked-line-count) 2))))

(ert-deftest donkey-split-a-live-rectangle-wins-over-the-bank ()
  "A rectangle is searched instead of the bank, which stays banked."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-bank-rect*" donkey-split-test--four
        "j j j V m l k k k g h m v j l l l l l f"
      (should (= (length donkey--split-places) 2))
      (should (= (length (donkey--banked-spans)) 1)))))

;;; ---------------------------------------------------------------------------
;;; The anchors, and the ends of things
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-split-on-a-line-end-anchor-appends-to-every-line ()
  "`$' holds a zero-width place at each line end, whatever the line holds."
  (donkey-split-test--on "$"
    (donkey-split-test--keys "*split-eol*" "alpha\nbe\ngamma\n" "v G f a ;"
      (should (equal (buffer-string) "alpha;\nbe;\ngamma;\n")))))

(ert-deftest donkey-split-passes-over-the-line-after-a-final-newline ()
  "The empty line a trailing newline leaves is not one of the lines."
  (donkey-split-test--on "$"
    (donkey-split-test--keys "*split-phantom*" "alpha\nbeta\n" "v G f"
      (should (= (length donkey--split-places) 2)))))

(ert-deftest donkey-split-survives-a-selection-that-ends-at-a-line-end ()
  "A zero-width match at the bound must not step past it."
  (donkey-split-test--on "$"
    (donkey-split-test--keys "*split-bound*" "alpha\nbeta\n" "v j g l f a ;"
      (should (equal (buffer-string) "alpha;\nbeta;\n")))))

;;; ---------------------------------------------------------------------------
;;; What keeps Split mode, and what ends it
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-split-a-key-that-does-nothing-does-not-end-the-split ()
  "A mistyped key costs a bell, not the split, as in a mark run.

`ding' is stubbed because `execute-kbd-macro' stops at the bell in a
live frame, and the verb pressed AFTER the mistype is the whole point."
  (cl-letf (((symbol-function 'ding) #'ignore))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-typo*" "a foo b\nc foo d\n" "v G f q"
        (should (eq donkey--split-phase 'select))
        (should (= (length donkey--split-places) 2))))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-typo-then*" "a foo b\nc foo d\n"
          "v G f q a X"
        (should (equal (buffer-string) "a fooX b\nc fooX d\n"))))))

(ert-deftest donkey-split-a-key-of-its-own-ends-it-and-does-its-own-job ()
  "Any other key lapses the map and runs in the same press."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-lapse*" "a foo b\nc foo d\n" "v G f j"
      (should (null donkey--split-places))
      (should (equal (buffer-string) "a foo b\nc foo d\n")))))

(ert-deftest donkey-split-the-quit-key-ends-it-and-changes-nothing ()
  "`C-g' in Split mode leaves the buffer as the split found it."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-quit*" "a foo b\nc foo d\n" "v G f C-g"
      (should (null donkey--split-places))
      (should (equal (buffer-string) "a foo b\nc foo d\n")))))

(ert-deftest donkey-split-ends-when-insert-state-is-left ()
  "The quit key after a verb ends the split and unpaints the places.

The chooser's own map is gone by then, so the quit key reaches
`donkey--exit-insert' and nothing in Split mode sees it.  Without this
the mode changes under the reader while the places stay painted."
  (dolist (verb '("i" "a" "c"))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-cg-edit*" "a foo b\nc foo d\n"
          (concat "v G f " verb " X C-g")
        (should (null donkey--split-places))
        (should (null donkey--split-phase))
        (should (null (seq-filter (lambda (o) (overlay-get o 'donkey-split))
                                  (overlays-in (point-min) (point-max)))))
        (should (bound-and-true-p donkey-normal-mode))))))

(ert-deftest donkey-split-keeps-what-was-typed-when-insert-state-is-left ()
  "Ending the split keeps the writing, as the quit key does everywhere."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-cg-keep*" "a foo b\nc foo d\n"
        "v G f a X C-g"
      (should (equal (buffer-string) "a fooX b\nc fooX d\n")))))

(ert-deftest donkey-split-every-ending-says-what-it-did-to-how-many ()
  "However a split ends, it reports the same way: what happened, to how many.

One verb saying `Deleted 3 places' while another said only `Split
ended' left the reader guessing whether anything had happened at all."
  (dolist (case '(("i X C-g" "Split: typed before 3 places")
                  ("a X C-g" "Split: typed after 3 places")
                  ("c X C-g" "Split: changed 3 places")
                  ("d"       "Split: deleted 3 places")
                  ("( C-g"   "Split: wrapped 3 places")
                  ("( ( C-g" "Split: unwrapped 3 places")
                  ("( a X C-g" "Split: typed after 3 places")
                  ("C-g"     "Split ended -- 3 places left alone")))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-report*" "a foo b\nc foo d\ne foo f\n"
          (concat "v G f " (car case))
        (should (equal donkey-test-keys--said (cadr case)))))))

(ert-deftest donkey-split-counts-one-place-in-the-singular ()
  "A lone place is reported as one place, not one places."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-one-report*" "only foo here\n" "v G f a X C-g"
      (should (equal donkey-test-keys--said "Split: typed after 1 place")))))

(ert-deftest donkey-split-logs-its-report-once-after-its-opening ()
  "The report reaches *Messages* once, after the line that opened the split.

Asserted at the call, as `donkey-a-repainted-reminder-is-never-logged'
is, since a live-frame run of the suite has no log to count: what each
message owes is the binding of `message-log-max' it was made under."
  (dolist (keys '("d" "a X C-g" "( C-g" "C-g"))
    (let ((message-log-max 1000) logged)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (and fmt message-log-max)
                     (push (apply #'format fmt args) logged))
                   nil)))
        (donkey-split-test--on "foo"
          (donkey-split-test--keys "*split-log*" "a foo b\nc foo d\n"
              (concat "v G f " keys)
            nil)))
      (setq logged (seq-filter (lambda (m) (string-prefix-p "Split" m))
                               (nreverse logged)))
      (should (= (length logged) 2))
      (should (string-match-p "\\`Split: 2 places in " (car logged)))
      (should (string-match-p "2 places" (cadr logged))))))

(ert-deftest donkey-split-whose-buffer-is-killed-ends-there-with-its-count ()
  "Killing the buffer ends its split there, reporting what the split had.

Checked at the verb menu and while writing, with the buffer killed by
Lisp rather than by a command, so nothing but the kill can end it."
  (dolist (case '(("v G f"     "Split ended -- 2 places left alone")
                  ("v G f a X" "Split: typed after 2 places")))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-killed*" "a foo b\nc foo d\n"
          (car case)
        (let (said)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (when fmt (push (apply #'format fmt args) said))
                       nil)))
            (kill-buffer (current-buffer)))
          (should (equal said (list (cadr case)))))
        (should (null donkey--split-buffer))
        (should (null donkey--split-exit-function))
        (when (bound-and-true-p donkey-insert-mode)
          (donkey-normal-mode 1))))))

(ert-deftest donkey-split-whose-buffer-died-unseen-reports-no-count ()
  "Where the kill skipped its hook, the ending counts nothing it cannot see.

The next key ends the split from another buffer, which holds no places
of its own; a count read there would say none were left alone."
  (let ((other (get-buffer-create "*split-survivor*")))
    (unwind-protect
        (donkey-split-test--on "foo"
          (donkey-split-test--keys "*split-unseen*" "a foo b\nc foo d\n" "v G f"
            (let ((kill-buffer-hook nil))
              (kill-buffer (current-buffer)))
            (switch-to-buffer other)
            (text-mode)
            (insert "one\ntwo\n")
            (goto-char (point-min))
            (let (said)
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (when fmt (push (apply #'format fmt args) said))
                           nil)))
                (execute-kbd-macro (kbd "j")))
              (should-not (seq-find (lambda (m) (string-match-p "places" m))
                                    said)))
            (should (null donkey--split-exit-function))))
      (kill-buffer other))))

(ert-deftest donkey-split-can-never-stop-its-buffer-being-killed ()
  "An error while a split ends does not keep its buffer alive.

Run with `debug-on-error' on as well as off, since a guard that only
catches when the debugger is off lets the error through for anyone
who works with it on."
  (dolist (debug-on-error '(nil t))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-kill-error*" "a foo b\nc foo d\n" "v G f"
        (let ((buffer (current-buffer)))
          (cl-letf (((symbol-function 'donkey--split-report)
                     (lambda (_) (error "Report failed"))))
            (let ((inhibit-message t))
              (kill-buffer buffer)))
          (should-not (buffer-live-p buffer)))))))

(ert-deftest donkey-split-a-verb-key-in-another-buffer-does-its-own-job ()
  "A verb\'s key pressed in another buffer is that buffer\'s key.

The buffer is changed without a command, as a window selected by the
mouse or a frame switch does, so nothing has ended the split first."
  (let ((other (get-buffer-create "*split-elsewhere*")))
    (unwind-protect
        (donkey-split-test--on "foo"
          (donkey-split-test--keys "*split-home-key*" "a foo b\nc foo d\n"
              "v G f"
            (let ((home (current-buffer)))
              (switch-to-buffer other)
              (text-mode)
              (insert "zzz\n")
              (goto-char (point-min))
              (donkey--ensure-default-state)
              (execute-kbd-macro (kbd "i Q"))
              (should (equal (buffer-string) "Qzzz\n"))
              (should (null donkey--split-exit-function))
              (should (null (buffer-local-value 'donkey--split-places home))))))
      (kill-buffer other))))

(ert-deftest donkey-split-a-command-on-another-terminal-leaves-it-armed ()
  "Only a key on the terminal a split was armed on can end it."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-terminal*" "a foo b\nc foo d\n" "v G f"
      (let ((donkey--split-terminal 'elsewhere))
        (execute-kbd-macro (kbd "j")))
      (should donkey--split-exit-function)
      (execute-kbd-macro (kbd "d"))
      (should (equal (buffer-string) "a  b\nc  d\n")))))

(ert-deftest donkey-split-verbs-answer-only-on-its-own-terminal ()
  "Looked up from another terminal, a verb\'s key is the ordinary one."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-own-terminal*" "a foo b\nc foo d\n"
        "v G f"
      (should (eq (key-binding "d") #'donkey-split-delete))
      (let ((donkey--split-terminal 'elsewhere))
        (should-not (eq (key-binding "d") #'donkey-split-delete))))))

(ert-deftest donkey-split-a-map-left-behind-answers-nothing ()
  "A split ended from another terminal leaves its map there, inert.

Ending it with `overriding-terminal-local-map' bound to nil is what an
ending on another terminal does to this one: the map is popped from a
different value.  Arming the next split clears the map away."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-stranded*" "a foo b\nc foo d\n" "v G f"
      (let ((stranded (seq-find (lambda (map)
                                  (and (keymapp map)
                                       (lookup-key map [donkey-split-verbs])))
                                (cdr overriding-terminal-local-map))))
        (should stranded)
        (let ((overriding-terminal-local-map nil))
          (donkey--split-dissolve t))
        (should (memq stranded (cdr overriding-terminal-local-map)))
        (should-not (eq (key-binding "d") #'donkey-split-delete))
        (goto-char (point-min))
        (execute-kbd-macro (kbd "v G f"))
        (should-not (memq stranded (cdr overriding-terminal-local-map)))
        (should (eq (key-binding "d") #'donkey-split-delete))))))

(defvar donkey-split-test--shadow nil
  "The test buffer\'s text as `after-change-functions' alone reports it.")

(defun donkey-split-test--follow-change (beg end length)
  "Apply to the shadow the change from BEG to END that replaced LENGTH."
  (when (equal (buffer-name) "*split-hooks*")
    (setq donkey-split-test--shadow
          (concat (substring donkey-split-test--shadow 0 (1- beg))
                  (buffer-substring-no-properties beg end)
                  (substring donkey-split-test--shadow (+ (1- beg) length))))))

(ert-deftest donkey-split-change-hooks-are-told-of-every-edit ()
  "A copy kept from `after-change-functions' alone ends equal to the buffer.

What a language server client or a parser cache keeps: every edit at
every place has to reach the hooks, or the copy and the buffer part."
  (dolist (keys '("a X C-g" "i X C-g" "c X C-g" "d" "( [" "w (" "( ("))
    (let ((donkey-split-test--shadow ""))
      (donkey-split-test--on "foo"
        (donkey-test-keys--harness "*split-hooks*" #'text-mode
            ((after-change-functions (list #'donkey-split-test--follow-change)))
            "a foo b\nc foo d\ne foo f\n" (concat "v G f " keys)
          (should (equal (list keys donkey-split-test--shadow)
                         (list keys (buffer-string)))))))))

(ert-deftest donkey-split-belongs-to-the-buffer-it-was-made-in ()
  "A verb pressed in another buffer refuses, and leaves no places behind.

The map Split mode arms lives in `overriding-terminal-local-map', which
is terminal-wide, while the places are overlays in one buffer.  Without
this a verb elsewhere acts on nothing at all."
  (let ((other (get-buffer-create "*split-other*")))
    (unwind-protect
        (donkey-split-test--on "foo"
          (donkey-split-test--keys "*split-home*" "a foo b\nc foo d\n" "v G f"
            (should (= (length donkey--split-places) 2))
            (let ((home (current-buffer)))
              (with-current-buffer other
                (should-error (donkey-split-append) :type 'user-error))
              (with-current-buffer home
                (should (null donkey--split-places))
                (should (null (seq-filter
                               (lambda (o) (overlay-get o 'donkey-split))
                               (overlays-in (point-min) (point-max)))))))))
      (kill-buffer other))))

(ert-deftest donkey-split-is-bound-to-f-in-normal-state ()
  "The key the README and the tutor name reaches the command."
  (should (eq (keymap-lookup donkey-normal-mode-map "f") #'donkey-split)))

(provide 'donkey-split-test)

;;; donkey-split-test.el ends here

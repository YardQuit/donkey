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
      (should (null donkey--split-places)))))

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

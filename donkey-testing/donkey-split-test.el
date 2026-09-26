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

(ert-deftest donkey-split-holds-matches-that-differ ()
  "A regexp matching different text at each place still makes a split."
  (donkey-split-test--on "^[a-z]+"
    (donkey-split-test--keys "*split-differ*" "alpha 1\nbeta 2\n" "v G f a X C-g"
      (should (equal (buffer-string) "alphaX 1\nbetaX 2\n")))))

(ert-deftest donkey-split-types-only-at-the-edges-of-matches-that-differ ()
  "Where the matches differ, each verb leaves every match its own text."
  (dolist (case '(("i < C-g" "id=<1;\nid=<22;\nid=<333;\n")
                  ("a > C-g" "id=1>;\nid=22>;\nid=333>;\n")
                  ("c N C-g" "id=N;\nid=N;\nid=N;\n")
                  ("d"       "id=;\nid=;\nid=;\n")
                  ("( ["     "id=([1]);\nid=([22]);\nid=([333]);\n")))
    (donkey-split-test--on "[0-9]+"
      (donkey-split-test--keys "*split-edges*" "id=1;\nid=22;\nid=333;\n"
          (concat "v G f " (car case))
        (should (equal (list (car case) (buffer-string))
                       (list (car case) (cadr case))))))))

(ert-deftest donkey-split-edits-inside-matches-that-agree ()
  "Where the matches agree the place is the whole match, so an edit inside it is copied."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-inside*" "a foo b\nc foo d\n"
        "v G f i X <right> Y C-g"
      (should (equal (buffer-string) "a XfYoo b\nc XfYoo d\n")))))

(ert-deftest donkey-split-leaving-the-edge-of-matches-that-differ-ends-it ()
  "Where the matches differ, moving into one ends the split."
  (donkey-split-test--on "fo."
    (donkey-split-test--keys "*split-leave-edge*" "foo fox\n" "f i X <right>"
      (should (null donkey--split-phase))
      (should (equal (buffer-string) "Xfoo Xfox\n")))))

(ert-deftest donkey-split-kills-every-text-that-differs-one-per-line ()
  "`c' and `d' over places that differ put every text on the kill ring as one kill."
  (dolist (keys '("c N C-g" "d"))
    (donkey-split-test--on "[0-9]+"
      (donkey-split-test--keys "*split-kill-differ*" "id=1;\nid=22;\nid=333;\n"
          (concat "v G f " keys)
        (should (equal (list keys kill-ring) (list keys '("1\n22\n333"))))))))

(ert-deftest donkey-split-ignores-case-without-a-capital ()
  "Case is ignored as `replace-regexp' ignores it."
  (dolist (case '(("todo" t t 3) ("Todo" t t 1) ("todo" nil t 1)
                  ("Todo" t nil 3)))
    (let ((case-fold-search (nth 1 case))
          (search-upper-case (nth 2 case)))
      (donkey-split-test--on (car case)
        (donkey-split-test--keys "*split-case*" "Todo one\ntodo two\nTODO three\n"
            "v G f"
          (should (equal (list case (length donkey--split-places))
                         (list case (nth 3 case)))))))))

(ert-deftest donkey-split-refuses-matches-that-touch ()
  "Matches that share a boundary are refused, and nothing is painted."
  (dolist (case '(("aaa\n" "a") ("x  y\n" " ") ("xxb\n" "x*")))
    (donkey-split-test--on (cadr case)
      (donkey-split-test--keys "*split-touch*" (car case) ""
        (should-error (call-interactively #'donkey-split) :type 'user-error)
        (should (null donkey--split-places))
        (should (zerop (donkey-split-test--painted)))
        (should (equal (buffer-string) (car case)))))))

(ert-deftest donkey-split-types-at-the-edge-of-read-only-matches-that-differ ()
  "Where the matches differ only their edges are written, so read-only text in one is no obstacle."
  (donkey-split-test--on "[0-9]+"
    (donkey-split-test--keys "*split-ro-differ*" "id=1;\nid=22;\nid=333;\n" "v G f"
      (save-excursion
        (goto-char (point-max))
        (search-backward "333")
        (let ((inhibit-read-only t))
          (put-text-property (match-beginning 0) (match-end 0) 'read-only t)))
      (execute-kbd-macro (kbd "i < C-g"))
      (should (equal (buffer-string) "id=<1;\nid=<22;\nid=<333;\n"))
      (let ((inhibit-read-only t))
        (remove-text-properties (point-min) (point-max) '(read-only nil))))))

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

(ert-deftest donkey-split-wraps-an-empty-place-opener-first ()
  "A zero-width place takes its pair the right way round, and gives it back."
  (donkey-split-test--on "$"
    (donkey-split-test--keys "*split-wrap-empty*" "a\nbb\n" "v G f ("
      (should (equal (buffer-string) "a()\nbb()\n"))
      (should (equal (mapcar (lambda (place)
                               (cons (overlay-start place) (overlay-end place)))
                             donkey--split-places)
                     '((3 . 3) (8 . 8))))))
  (donkey-split-test--on "$"
    (donkey-split-test--keys "*split-wrap-empty-off*" "a\nbb\n" "v G f ( ("
      (should (equal (buffer-string) "a\nbb\n"))))
  (donkey-split-test--on "^"
    (donkey-split-test--keys "*split-wrap-empty-then*" "a\nbb\n" "v G f [ a X"
      (should (equal (buffer-string) "[X]a\n[X]bb\n")))))

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
  "No match, and matches that touch, leave the bank standing."
  (donkey-split-test--on "zzz"
    (donkey-split-test--keys "*split-bank-none*" donkey-split-test--four
        "V m l f"
      (should (= (length (donkey--banked-spans)) 1))))
  (donkey-split-test--keys "*split-bank-touch*" "aaa\nbbb\n" "V j m l"
    (should-error (donkey-split "a") :type 'user-error)
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

(defun donkey-split-test--painted ()
  "Return how many split places are painted in this buffer."
  (seq-count (lambda (o) (overlay-get o 'donkey-split))
             (save-restriction (widen) (overlays-in (point-min) (point-max)))))

(ert-deftest donkey-split-refuses-an-empty-regexp ()
  "An empty regexp is refused, and the selection it was given stays."
  (donkey-split-test--on ""
    (donkey-split-test--keys "*split-empty*" "a foo b\nc foo d\n" "v G"
      (should-error (call-interactively #'donkey-split) :type 'user-error)
      (should (region-active-p))
      (should (zerop (donkey-split-test--painted))))))

(ert-deftest donkey-split-reads-a-mis-set-pair-table-anyway ()
  "A pair table that is not all pairs, or not a list, still gives a split.

What it wraps in follows from what is left of the table: a pair that
survived is a pair, and a character the table no longer names wraps
in itself."
  (dolist (case '((((?\( . ?\)) ?x) "a (foo) b\nc (foo) d\n")
                  (parens "a (foo( b\nc (foo( d\n")))
    (let ((donkey-mark-pair-delimiters (car case)))
      (donkey-split-test--on "foo"
        (donkey-split-test--keys "*split-pairs*" "a foo b\nc foo d\n" "v G f w ("
          (should donkey--split-exit-function)
          (should (equal (buffer-string) (cadr case))))))))

(ert-deftest donkey-split-stands-though-letting-go-of-the-selection-fails ()
  "A `deactivate-mark-hook' that signals does not leave a split half made."
  (donkey-split-test--on "foo"
    (donkey-test-keys--harness "*split-hook-fails*" #'text-mode
        ((deactivate-mark-hook (list (lambda () (error "Hook failed")))))
        "a foo b\nc foo d\n" "v G f a X C-g"
      (should (equal (buffer-string) "a fooX b\nc fooX d\n"))
      (should (zerop (donkey-split-test--painted))))))

(ert-deftest donkey-split-a-search-that-fails-leaves-nothing-painted ()
  "A search that signals part way leaves no places and spends no bank."
  (let ((calls 0)
        (search (symbol-function 're-search-forward)))
    (cl-letf (((symbol-function 're-search-forward)
               (lambda (&rest args)
                 (when (= (cl-incf calls) 2) (error "Search failed"))
                 (apply search args))))
      (donkey-split-test--on "foo"
        (donkey-split-test--keys "*split-search-fails*" "a foo b\nc foo d\n"
            "V m l"
          (should-error (execute-kbd-macro (kbd "v G f")))
          (should (zerop (donkey-split-test--painted)))
          (should (null donkey--split-places))
          (should (= (length (donkey--banked-spans)) 1)))))))

(ert-deftest donkey-split-refuses-its-verbs-in-a-read-only-buffer ()
  "Every verb is refused in a read-only buffer, and nothing is written."
  (dolist (verb '("i" "a" "c" "d" "("))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-ro*" "a foo b\nc foo d\n" "v G f"
        (setq buffer-read-only t)
        (should-error (execute-kbd-macro (kbd verb)))
        (should (equal (list verb (buffer-string))
                       (list verb "a foo b\nc foo d\n")))
        (should (null kill-ring))
        (should donkey--split-exit-function)
        (setq buffer-read-only nil)))))

(defun donkey-split-test--read-only-last ()
  "Make the last match in the buffer read-only.
The last, so the places before it are written before it refuses."
  (save-excursion
    (goto-char (point-max))
    (search-backward "foo")
    (let ((inhibit-read-only t))
      (put-text-property (match-beginning 0) (match-end 0) 'read-only t))))

(ert-deftest donkey-split-writes-every-place-or-none ()
  "A place that cannot be written leaves every place as it was."
  (dolist (verb '("d" "c X" "(" "i X" "a X"))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-some-ro*" "a foo b\nc foo d\ne foo f\n"
          "v G f"
        (donkey-split-test--read-only-last)
        (ignore-errors (execute-kbd-macro (kbd verb)))
        (should (equal (list verb (buffer-string))
                       (list verb "a foo b\nc foo d\ne foo f\n")))
        (should (null kill-ring))
        (let ((inhibit-read-only t))
          (remove-text-properties (point-min) (point-max) '(read-only nil)))))))

(ert-deftest donkey-split-refuses-to-type-at-an-edge-that-cannot-take-text ()
  "A place whose edge refuses an insertion refuses `i', changing nothing.

The text of the place is writable; the character before it is
read-only, and an insertion takes that property from it."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-ro-edge*" "a foo b\nc foo d\ne foo f\n"
        "v G f"
      (save-excursion
        (goto-char (point-max))
        (search-backward "foo")
        (let ((inhibit-read-only t))
          (put-text-property (1- (point)) (point) 'read-only t)))
      (should-error (execute-kbd-macro (kbd "i")) :type 'user-error)
      (should (equal (buffer-string) "a foo b\nc foo d\ne foo f\n"))
      (should (eq donkey--split-phase 'select))
      (let ((inhibit-read-only t))
        (remove-text-properties (point-min) (point-max) '(read-only nil))))))

(ert-deftest donkey-split-a-place-that-refuses-the-edit-undoes-the-others ()
  "A place whose text will not change leaves every place as it was.

The refusal here comes from a `modification-hooks' property that
signals, which no read-only test sees in advance.  A wrap does not
touch the text it goes around, so it is not asked."
  (dolist (verb '("c X" "d"))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-refuses*" "a foo b\nc foo d\ne foo f\n"
          "v G f"
        (save-excursion
          (goto-char (point-max))
          (search-backward "foo")
          (put-text-property (match-beginning 0) (match-end 0)
                             'modification-hooks
                             (list (lambda (&rest _) (error "Refused")))))
        (ignore-errors (execute-kbd-macro (kbd verb)))
        (let ((inhibit-modification-hooks t))
          (remove-text-properties (point-min) (point-max)
                                  '(modification-hooks nil)))
        (should (equal (list verb (buffer-string))
                       (list verb "a foo b\nc foo d\ne foo f\n")))
        (should (null kill-ring))))))

(ert-deftest donkey-split-a-place-that-cannot-be-written-ends-the-split ()
  "A place turning read-only mid-writing ends the split, saying why."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-late-ro*" "a foo b\nc foo d\ne foo f\n"
        "v G f a"
      (donkey-split-test--read-only-last)
      (let ((said nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when fmt (push (apply #'format fmt args) said))
                     nil)))
          (execute-kbd-macro (kbd "X")))
        (should (member "Split ended -- Text is read-only" said)))
      (should (null donkey--split-phase))
      (should (zerop (donkey-split-test--painted)))
      (should (equal (buffer-string) "a fooX b\nc foo d\ne foo f\n"))
      (let ((inhibit-read-only t))
        (remove-text-properties (point-min) (point-max) '(read-only nil))))))

(ert-deftest donkey-split-that-fails-to-arm-spends-no-bank ()
  "The bank is spent only once the split stands."
  (cl-letf (((symbol-function 'set-transient-map)
             (lambda (&rest _) (error "Cannot arm"))))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-arm-fails*" "a foo b\nc foo d\n" "V m l"
        (should-error (execute-kbd-macro (kbd "v G f")))
        (should (= (length (donkey--banked-spans)) 1))))))

(ert-deftest donkey-split-copies-past-a-narrowing ()
  "Narrowing while writing does not stop the other places being copied."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-narrow*" "a foo b\nc foo d\n" "v G f a"
      (narrow-to-region (point-min) (line-end-position))
      (execute-kbd-macro (kbd "X C-g"))
      (widen)
      (should (equal (buffer-string) "a fooX b\nc fooX d\n")))))

(ert-deftest donkey-split-carries-on-past-a-place-deleted-by-something-else ()
  "A place whose overlay something else deleted is dropped, not tripped over."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-lost-place*" "a foo b\nc foo d\ne foo f\n"
        "v G f"
      (delete-overlay (car donkey--split-places))
      (execute-kbd-macro (kbd "a X C-g"))
      (should (equal (buffer-string) "a foo b\nc fooX d\ne fooX f\n")))))

(ert-deftest donkey-split-ends-when-the-major-mode-changes ()
  "A new major mode takes the split down: nothing stays painted."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-mode-change*" "a foo b\nc foo d\n" "v G f"
      (fundamental-mode)
      (should (zerop (donkey-split-test--painted)))
      (should (null donkey--split-buffer))
      (should (null donkey--split-exit-function)))))

(ert-deftest donkey-split-ends-when-donkey-mode-is-turned-off ()
  "Turning DONKEY off takes a split down with it."
  (unwind-protect
      (donkey-split-test--on "foo"
        (donkey-split-test--keys "*split-mode-off*" "a foo b\nc foo d\n" "v G f"
          (donkey-mode -1)
          (should (zerop (donkey-split-test--painted)))
          (should (null donkey--split-exit-function))
          (should-not (eq (key-binding "d") #'donkey-split-delete))))
    (donkey-mode 1)))

(ert-deftest donkey-split-says-left-alone-when-nothing-changed ()
  "Where a verb changed nothing, the report says the places were left alone."
  (dolist (case '(("foo" "a C-g") ("foo" "i C-g") ("$" "d") ("$" "c C-g")))
    (donkey-split-test--on (car case)
      (donkey-split-test--keys "*split-nothing*" "a foo b\nc foo d\n"
          (concat "v G f " (cadr case))
        (should (equal (list (cadr case) donkey-test-keys--said)
                       (list (cadr case)
                             "Split ended -- 2 places left alone")))))))

(ert-deftest donkey-split-repeats-an-edit-at-a-place-s-edge-everywhere ()
  "A deletion reaching just past the place is made past every place."
  (dolist (case '(("i DEL C-g" "afoo b\ncfoo d\n")
                  ("a C-d C-g" "a foob\nc food\n")
                  ("c DEL C-g" "a b\nc d\n")))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-edge*" "a foo b\nc foo d\n"
          (concat "v G f " (car case))
        (should (equal (list (car case) (buffer-string))
                       (list (car case) (cadr case))))))))

(ert-deftest donkey-split-keeps-every-place-alike-through-electric-indentation ()
  "What `electric-indent-mode' does beside the place after RET happens at every place."
  (let ((electric-indent-mode t))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-electric*" "a foo b\nc foo d\n"
          "v G f a RET C-g"
        (should (equal (buffer-string) "a foo\nb\nc foo\nd\n"))))))

(ert-deftest donkey-split-ends-where-an-edge-edit-cannot-be-made-alike ()
  "A deletion that would take a line break at one place and not another ends the split."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-edge-unlike*" "a foo b\nfoo d\n"
        "v G f i"
      (let ((said nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when fmt (push (apply #'format fmt args) said))
                     nil)))
          (execute-kbd-macro (kbd "DEL")))
        (should (member "Split ended -- an edit beside a place could not be made at every place"
                        said)))
      (should (null donkey--split-phase))
      (should (equal (buffer-string) "afoo b\nfoo d\n")))))

(ert-deftest donkey-split-ends-where-an-edge-edit-would-leave-places-touching ()
  "A deletion that leaves two places sharing a boundary ends the split."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-edge-touch*" "x foo foo y\n" "f a C-d"
      (should (null donkey--split-phase))
      (should (equal (buffer-string) "x foofoo y\n")))))

(defun donkey-split-test--append-z ()
  "Put a Z at the end of the buffer, away from every place."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "Z")))

(ert-deftest donkey-split-ends-at-an-edit-away-from-the-places ()
  "A command that changes text away from the places ends the split, saying so."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-stray*" "a foo b\nc foo d\n" "v G f a"
      (let ((said nil)
            (overriding-local-map (let ((map (make-sparse-keymap)))
                                    (define-key map [f7]
                                      #'donkey-split-test--append-z)
                                    map)))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when fmt (push (apply #'format fmt args) said))
                     nil)))
          (execute-kbd-macro [f7]))
        (should (member "Split ended -- an edit away from the places" said)))
      (should (null donkey--split-phase))
      (should (equal (buffer-string) "a foo b\nc foo d\nZ")))))

(ert-deftest donkey-split-survives-an-undo-while-writing ()
  "An undo while writing puts every place back together, and writing goes on."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-undo*" "a foo b\nc foo d\n"
        "v G f a X C-/ Y C-g"
      (should (equal (buffer-string) "a fooY b\nc fooY d\n")))))

(ert-deftest donkey-split-is-not-ended-by-a-property-change ()
  "A text property set away from the places while writing changes no text."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-property*" "a foo b\nc foo d\n" "v G f a"
      (put-text-property (1- (point-max)) (point-max) 'face 'bold)
      (execute-kbd-macro (kbd "X C-g"))
      (should (equal (buffer-string) "a fooX b\nc fooX d\n")))))

(defvar donkey-split-test--sizes nil
  "What each change on the second line inserted and removed, newest first.")

(defun donkey-split-test--note-size (beg end length)
  "Note what a change at BEG to END, replacing LENGTH, did on the second line."
  (when (and donkey--split-places (= (line-number-at-pos beg) 2))
    (push (list (- end beg) length) donkey-split-test--sizes)))

(ert-deftest donkey-split-rewrites-only-what-changed ()
  "A key typed into a split changes one character at every other place.

Not the whole place: what the undo record and the change hooks are
given is what was typed."
  (dolist (verb '("a" "i"))
    (let ((donkey-split-test--sizes nil))
      (donkey-split-test--on "fo+"
        (donkey-test-keys--harness "*split-only-changed*" #'text-mode
            ((after-change-functions (list #'donkey-split-test--note-size)))
            "a fooooooooo b\nc fooooooooo d\n" (concat "v G f " verb " X")
          (should (equal (list verb donkey-split-test--sizes)
                         (list verb '((1 0))))))))))

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

(defun donkey-split-test--marks ()
  "Return where the drawn cursors are, in buffer order."
  (sort (mapcar #'overlay-start donkey--split-cursor-marks) #'<))

(ert-deftest donkey-split-draws-a-cursor-at-every-match-but-the-real-one ()
  "Choosing a verb, a cursor is drawn at the start of every other match."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-drawn*" "a foo b\nc foo d\ne foo f\n"
        "v G f"
      (should (= (point) 3))
      (should (equal (donkey-split-test--marks) '(11 19))))))

(ert-deftest donkey-split-draws-each-cursor-where-typing-lands ()
  "While typing, each match shows a cursor as far in as the real one is."
  (dolist (case '(("v G f i X" (13 22)) ("v G f a X" (16 25))))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-drawn-typing*"
          "a foo b\nc foo d\ne foo f\n" (car case)
        (should (equal (list (car case) (donkey-split-test--marks))
                       case))))))

(ert-deftest donkey-split-draws-no-cursor-once-it-ends ()
  "Ending a split takes every drawn cursor with it."
  (dolist (keys '("v G f C-g" "v G f i X C-g" "v G f d"))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-drawn-end*" "a foo b\nc foo d\n" keys
        (should (null donkey--split-cursor-marks))
        (should (null (seq-filter
                       (lambda (o) (eq (overlay-get o 'face)
                                       'donkey-split-cursor-face))
                       (overlays-in (point-min) (point-max)))))))))

(ert-deftest donkey-split-is-bound-to-f-in-normal-state ()
  "The key the README and the tutor name reaches the command."
  (should (eq (keymap-lookup donkey-normal-mode-map "f") #'donkey-split)))

;;; ---------------------------------------------------------------------------
;;; Cursors in a column
;;; ---------------------------------------------------------------------------

(defconst donkey-split-test--column
  "alpha beta\ngamma delta\nepsilon zeta\nlast\n"
  "Four lines for the column-cursor tests.")

(defun donkey-split-test--cursors ()
  "Return where every cursor of the live split is, top first."
  (mapcar #'donkey--split-cursor donkey--split-places))

(ert-deftest donkey-split-t-adds-a-cursor-below-at-the-same-column ()
  "`donkey-split-add-cursor' makes two cursors a line apart, changing nothing."
  (donkey-split-test--keys "*cursors-T*" donkey-split-test--column "l l t"
    (should (equal (buffer-string) donkey-split-test--column))
    (should donkey--split-cursors)
    (should (equal (donkey-split-test--cursors) '(3 14)))
    (should (= (point) 3))))

(ert-deftest donkey-split-t-grows-by-one-or-by-a-count ()
  "Each `donkey-split-add-cursor' adds one below the last; a count, that many."
  (dolist (case '(("l l t t" (3 14 26)) ("l l C-u 3 t" (3 14 26 39))))
    (donkey-split-test--keys "*cursors-grow*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (donkey-split-test--cursors)) case)))))

(ert-deftest donkey-split-t-puts-a-cursor-at-a-short-line-s-end ()
  "A line shorter than the column gets its cursor at its end."
  (donkey-split-test--keys "*cursors-short*" "abcdef\nab\nabcdef\n"
      "l l l l t t"
    (should (equal (donkey-split-test--cursors) '(5 10 15)))))

(ert-deftest donkey-split-t-with-too-few-lines-below-changes-nothing ()
  "A count past the last line adds no cursor at all."
  (donkey-split-test--keys "*cursors-few*" donkey-split-test--column "j j"
    (should (equal (should-error (execute-kbd-macro (kbd "C-u 3 t"))
                                 :type 'user-error)
                   '(user-error "Only 1 line below")))
    (should (null donkey--split-places))
    (should (= (point) 24))))

(ert-deftest donkey-split-cursors-type-and-come-back ()
  "`i' types at every cursor, and `C-g' comes back to the cursors."
  (donkey-split-test--keys "*cursors-type*" donkey-split-test--column
      "l l t t i X C-g"
    (should (equal (buffer-string)
                   "alXpha beta\ngaXmma delta\nepXsilon zeta\nlast\n"))
    (should (eq donkey--split-phase 'select))
    (should (bound-and-true-p donkey-normal-mode))
    (should (equal (donkey-split-test--cursors) '(4 16 29)))))

(ert-deftest donkey-split-cursors-type-after-the-character-with-a ()
  "`a' types after the character under each cursor, `i' before it."
  (dolist (case '(("t t a X C-g"
                   "aXlpha beta\ngXamma delta\neXpsilon zeta\nlast\n")
                  ("t t i X C-g"
                   "Xalpha beta\nXgamma delta\nXepsilon zeta\nlast\n")
                  ("t t g l a X C-g"
                   "alpha betaX\ngamma deltaX\nepsilon zetaX\nlast\n")
                  ("t t v w a X C-g"
                   "alphaX beta\ngammaX delta\nepsilonX zeta\nlast\n")))
    (donkey-split-test--keys "*cursors-a*" donkey-split-test--column (car case)
      (should (equal (list (car case) (buffer-string)) case)))))

(ert-deftest donkey-split-cursors-type-at-line-ends-and-starts ()
  "`A' and `I' type at every cursor's line end and line start."
  (dolist (case '(("t t A ; C-g"
                   "alpha beta;\ngamma delta;\nepsilon zeta;\nlast\n")
                  ("l t t I - C-g"
                   "-alpha beta\n-gamma delta\n-epsilon zeta\nlast\n")))
    (donkey-split-test--keys "*cursors-line-ends*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (buffer-string)) case)))))

(ert-deftest donkey-split-cursors-g-l-puts-every-cursor-at-its-line-end ()
  "`g l' moves every cursor to its line's end, keeping an anchor or a whole line."
  (dolist (case '(("t t g l" (11 23 36) (nil nil nil))
                  ("t t C-u 3 g l" (11 23 36) (nil nil nil))
                  ("t t l v g l" (11 23 36) (2 13 25))
                  ("t t V g l" (11 23 36) (line line line))))
    (donkey-split-test--keys "*cursors-g-l*" donkey-split-test--column (car case)
      (should (equal (list (car case) (donkey-split-test--cursors))
                     (list (car case) (cadr case))))
      (should (equal (mapcar (lambda (place)
                               (if (overlay-get place 'donkey-line)
                                   'line
                                 (donkey--split-cursor-anchor place)))
                             donkey--split-places)
                     (nth 2 case)))
      (should (equal (buffer-string) donkey-split-test--column)))))

(ert-deftest donkey-split-cursors-g-l-ends-a-mark-run-like-any-motion ()
  "A mark key after `g l' marks afresh at every cursor, as after any other motion."
  (donkey-split-test--keys "*cursors-g-l-run*" donkey-split-test--column
      "t t m w g l m b"
    (should (equal (donkey-split-test--cursors) '(7 18 32)))
    (should (equal (mapcar #'donkey--split-cursor-anchor donkey--split-places)
                   '(11 23 36)))
    (should (equal (mapcar (lambda (place)
                             (buffer-substring-no-properties
                              (overlay-start place) (overlay-end place)))
                           donkey--split-places)
                   '("beta" "delta" "zeta")))))

(ert-deftest donkey-split-cursors-o-and-g-l-leave-the-display-engine-alone ()
  "Opening lines and moving to line ends at the cursors never call `vertical-motion'."
  (dolist (keys '("o" "O" "g l" "v g l"))
    (donkey-split-test--keys "*cursors-no-engine*" donkey-split-test--column "t t"
      (let ((calls 0))
        (cl-letf* ((motion (symbol-function 'vertical-motion))
                   ((symbol-function 'vertical-motion)
                    (lambda (&rest args) (setq calls (1+ calls)) (apply motion args))))
          (execute-kbd-macro (kbd keys)))
        (should (equal (list keys calls) (list keys 0)))))))

(ert-deftest donkey-split-cursors-replay-runs-without-line-numbers ()
  "While a command runs at every cursor `display-line-numbers' is nil, and back after."
  (donkey-split-test--keys "*cursors-no-numbers*" donkey-split-test--column "t t"
    (setq-local display-line-numbers 'visual)
    (let ((seen nil))
      (cl-letf (((symbol-function 'donkey-split-test--probe-command)
                 (lambda () (interactive) (push display-line-numbers seen))))
        (donkey--split-cursors-run 'donkey-split-test--probe-command nil))
      (should (equal seen '(nil nil nil)))
      (should (eq display-line-numbers 'visual)))))

(ert-deftest donkey-split-cursors-move-and-stop-at-their-line-s-end ()
  "A motion moves every cursor, and none crosses into the next line."
  (dolist (case '(("t t w" (6 17 31)) ("t t w w w w" (11 23 36))
                  ("t t C-u 2 w" (11 23 36)) ("t t g l h" (10 22 35))))
    (donkey-split-test--keys "*cursors-move*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (donkey-split-test--cursors)) case))
      (should (equal (buffer-string) donkey-split-test--column)))))

(ert-deftest donkey-split-cursors-grow-a-selection-on-a-second-press ()
  "`m w m w' selects two words at every cursor, each cursor growing its own."
  (donkey-split-test--keys "*cursors-grow-sel*" "a b c\nd e f\n" "t m w m w"
    (should (equal (mapcar #'donkey--split-place-text donkey--split-places)
                   '("a b" "d e")))))

(ert-deftest donkey-split-cursors-grow-each-its-own-pair ()
  "`m i ( m i (' goes one pair out at every cursor, from that cursor's own."
  (donkey-split-test--keys "*cursors-grow-pair*" "(a (b) c)\n(dd (e) f)\n"
      "l l l l t m i ( m i ("
    (should (equal (mapcar #'donkey--split-place-text donkey--split-places)
                   '("a (b) c" "dd (e) f")))))

(ert-deftest donkey-split-cursors-change-what-each-selects ()
  "`v w c' empties every cursor's selection and types there."
  (donkey-split-test--keys "*cursors-change*" donkey-split-test--column
      "l l t t v w c Z C-g"
    (should (equal (buffer-string) "alZ beta\ngaZ delta\nepZ zeta\nlast\n"))
    (should (equal (car kill-ring) "pha\nmma\nsilon"))))

(ert-deftest donkey-split-cursors-delete-characters-without-a-kill ()
  "`x' deletes the character at every cursor and saves nothing."
  (donkey-split-test--keys "*cursors-x*" donkey-split-test--column "l l t t x"
    (should (equal (buffer-string)
                   "alha beta\ngama delta\nepilon zeta\nlast\n"))
    (should (null kill-ring))
    (should (= (length donkey--split-places) 3))))

(ert-deftest donkey-split-cursors-that-meet-on-a-line-become-one ()
  "`V d' takes every cursor's line, and the cursors left on one line merge."
  (donkey-split-test--keys "*cursors-merge*" donkey-split-test--column "t t V d"
    (should (equal (buffer-string) "last\n"))
    (should (equal (car kill-ring) "alpha beta\ngamma delta\nepsilon zeta\n"))
    (should (equal (donkey-split-test--cursors) '(1)))))

(ert-deftest donkey-split-cursors-paste-each-its-own-line-back ()
  "What `y' took from the cursors, `p' gives back one line to each."
  (donkey-split-test--keys "*cursors-yank*" donkey-split-test--column
      "t t m w y g l p"
    (should (equal (car kill-ring) "alpha\ngamma\nepsilon"))
    (should (equal (buffer-string)
                   (concat "alpha betaalpha\ngamma deltagamma\n"
                           "epsilon zetaepsilon\nlast\n")))))

(ert-deftest donkey-split-cursors-kill-to-each-line-s-end ()
  "`D' kills from every cursor to its line's end, and no newline."
  (donkey-split-test--keys "*cursors-D*" donkey-split-test--column "l l t t D"
    (should (equal (buffer-string) "al\nga\nep\nlast\n"))
    (should (equal (car kill-ring) "pha beta\nmma delta\nsilon zeta"))))

(ert-deftest donkey-split-cursors-wrap-only-what-is-selected ()
  "A delimiter wraps each cursor's selection, and nothing where none is."
  (dolist (case '(("l l t t v l ("
                   "al(p)ha beta\nga(m)ma delta\nep(s)ilon zeta\nlast\n")
                  ("l l t t v l ( ("
                   "alpha beta\ngamma delta\nepsilon zeta\nlast\n")
                  ("t t V ("
                   "(alpha beta)\n(gamma delta)\n(epsilon zeta)\nlast\n")))
    (donkey-split-test--keys "*cursors-wrap*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (buffer-string)) case))
      (should (= (length donkey--split-places) 3))))
  (donkey-split-test--keys "*cursors-wrap-none*" donkey-split-test--column
      "l l t t"
    (condition-case nil (execute-kbd-macro (kbd "(")) (error nil))
    (should (equal (buffer-string) donkey-split-test--column))
    (should (= (length donkey--split-places) 3))))

(ert-deftest donkey-split-cursors-wrap-a-selecting-cursor-and-leave-the-rest ()
  "Among the cursors, only one holding a selection is wrapped."
  (donkey-split-test--keys "*cursors-wrap-some*" "ab\ncd\n" "t v l"
    (donkey--split-cursor-set (cadr donkey--split-places) 4 nil nil nil)
    (donkey--split-cursors-settle)
    (execute-kbd-macro (kbd "("))
    (should (equal (buffer-string) "(a)b\ncd\n"))))

(ert-deftest donkey-split-f-from-cursors-searches-their-lines ()
  "`f' searches the cursors' lines only, and finding nothing keeps them."
  (donkey-split-test--on "a"
    (donkey-split-test--keys "*cursors-f*" "ab\nab\nab\nab\n" "t f"
      (should-not donkey--split-cursors)
      (should (equal (mapcar #'overlay-start donkey--split-places) '(1 4)))))
  (donkey-split-test--on "q"
    (donkey-split-test--keys "*cursors-f-none*" "ab\nab\nab\nab\n" "t f"
      (should donkey--split-cursors)
      (should (equal (donkey-split-test--cursors) '(1 4))))))

(ert-deftest donkey-split-f-from-cursors-reads-its-regexp-through-the-prompt ()
  "Typing the regexp into a real prompt keeps the cursors, whatever the keys.

The letters and delimiters the cursors answer are typed into the
minibuffer as text, and every cursor's line is searched."
  (donkey-split-test--keys "*cursors-prompt*" "who (x)\nwho (x)\nwho (x)\nwho\n"
      "t t f w h o SPC ( RET"
    (should-not donkey--split-cursors)
    (should (equal (mapcar #'donkey--split-place-text donkey--split-places)
                   '("who (" "who (" "who (")))))

(ert-deftest donkey-split-quitting-the-prompt-leaves-the-cursors ()
  "`C-g' at the regexp prompt of `f' leaves every cursor standing."
  (donkey-split-test--keys "*cursors-prompt-quit*" donkey-split-test--column "t t"
    (condition-case nil (execute-kbd-macro (kbd "f w C-g")) (quit nil))
    (should donkey--split-cursors)
    (should (equal (donkey-split-test--cursors) '(1 12 24)))
    (should (equal (buffer-string) donkey-split-test--column))))

(ert-deftest donkey-split-cursors-undo-a-command-that-fails-at-one ()
  "A selection that fails at one cursor leaves every cursor where it was."
  (donkey-split-test--keys "*cursors-fail*" "(ab) x\nno pair\n" "l t"
    (should-error (execute-kbd-macro (kbd "m i (")) :type 'user-error)
    (should (equal (donkey-split-test--cursors) '(2 9)))
    (should-not (seq-some #'donkey--split-cursor-selecting-p
                          donkey--split-places))))

(ert-deftest donkey-split-cursors-ask-a-question-once ()
  "The delimiter `m i' asks for is read once for every cursor."
  (let ((reads 0))
    (cl-letf* ((real (symbol-function 'read-char))
               ((symbol-function 'read-char)
                (lambda (&rest args)
                  (setq reads (1+ reads))
                  (apply real args))))
      (donkey-split-test--keys "*cursors-ask*" "(ab) x\n(cd) y\n" "l t m i ("
        (should (= reads 1))
        (should (equal (mapcar #'donkey--split-place-text donkey--split-places)
                       '("ab" "cd")))))))

(ert-deftest donkey-split-cursors-let-go-of-selections-before-ending ()
  "`C-g' lets go of the selections first, and ends the split second."
  (donkey-split-test--keys "*cursors-quit*" donkey-split-test--column
      "t t v w C-g"
    (should donkey--split-cursors)
    (should-not (seq-some #'donkey--split-cursor-selecting-p
                          donkey--split-places)))
  (donkey-split-test--keys "*cursors-quit2*" donkey-split-test--column
      "t t v w C-g C-g"
    (should (null donkey--split-places))))

(ert-deftest donkey-split-cursors-end-on-a-key-they-do-not-answer ()
  "`j' ends the cursors and moves the real one, as it does anywhere."
  (donkey-split-test--keys "*cursors-j*" donkey-split-test--column "t t j"
    (should (null donkey--split-places))
    (should (= (point) 12))
    (should (null (seq-filter (lambda (o) (overlay-get o 'donkey-split))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest donkey-split-cursors-draw-every-cursor-but-the-real-one ()
  "Every cursor but the real one is drawn, and none once the split ends."
  (donkey-split-test--keys "*cursors-draw*" "ab\nab\n\n" "t t"
    (should (equal (sort (mapcar #'overlay-start donkey--split-cursor-marks)
                         #'<)
                   '(4 7)))
    (should (seq-find (lambda (o) (overlay-get o 'after-string))
                      donkey--split-cursor-marks))
    (donkey--split-dissolve t)
    (should (null (seq-filter (lambda (o) (eq (overlay-get o 'face)
                                              'donkey-split-cursor-face))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest donkey-split-cursors-tutor-exercises-do-what-lesson-16-says ()
  "The exercises of the tutor's cursor lesson give what it promises."
  (donkey-split-test--keys "*cursors-tutor-1*" "milk\neggs\nbread\n"
      "t t I - SPC C-g C-g"
    (should (equal (buffer-string) "- milk\n- eggs\n- bread\n"))
    (should (null donkey--split-places)))
  (donkey-split-test--keys "*cursors-tutor-2*" "ada\nalan\ngrace\n"
      "t t m w y A SPC = SPC C-g p C-g"
    (should (equal (buffer-string) "ada = ada\nalan = alan\ngrace = grace\n")))
  (donkey-split-test--keys "*cursors-tutor-V*" "mercury\nvenus\nearth\n"
      "V j j t i * SPC C-g C-g"
    (should (equal (buffer-string) "* mercury\n* venus\n* earth\n")))
  (donkey-split-test--keys "*cursors-tutor-T*"
      "jupiter\nsaturn\nuranus\nneptune\n" "j j j T T T DEL i > SPC C-g C-g"
    (should (equal (buffer-string)
                   "jupiter\n> saturn\n> uranus\n> neptune\n")))
  (donkey-split-test--keys "*cursors-tutor-align*"
      "one two three\nx yy zzz\nalpha beta\n" "t t w = i | C-g C-g"
    (should (equal (buffer-string) "one| two three\nx y|y zzz\nalp|ha beta\n")))
  (donkey-split-test--keys "*cursors-tutor-M*"
      "old red apple\nold green pear\nold blue plum\n"
      "t t M w c f r e s h C-g C-g"
    (should (equal (buffer-string) "fresh apple\nfresh pear\nfresh plum\n")))
  (donkey-split-test--on ","
    (donkey-split-test--keys "*cursors-tutor-3*" "1,2,3\n4,5,6\n7,8,9\n0,0,0\n"
        "t t f c ; C-g"
      (should (equal (buffer-string) "1;2;3\n4;5;6\n7;8;9\n0,0,0\n")))))

(defun donkey-split-test--drawn ()
  "Return the shape of every drawn cursor: `box', `bar', `hbar' or `hollow'."
  (delete-dups
   (mapcar (lambda (mark)
             (let ((face (or (overlay-get mark 'face)
                             (get-text-property
                              0 'face (or (overlay-get mark 'after-string) "")))))
               (cond
                ((overlay-get mark 'before-string) 'bar)
                ((eq face 'donkey-split-cursor-face) 'box)
                ((plist-get face :underline) 'hbar)
                ((plist-get face :box) 'hollow))))
           donkey--split-cursor-marks)))

(ert-deftest donkey-split-cursors-are-drawn-in-the-real-cursor-s-shape ()
  "The drawn cursors change shape with the real one, state by state."
  (dolist (case '(("t t" (box)) ("t t i" (bar)) ("t t i C-g" (box))))
    (donkey-split-test--keys "*cursors-shape*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (donkey-split-test--drawn)) case)))))

(ert-deftest donkey-split-cursors-follow-the-reader-s-cursor-setting ()
  "A shape set for Normal state is the shape every cursor is drawn in."
  (dolist (case '((hbar (hbar)) ((hbar . 3) (hbar)) (bar (bar)) (nil nil)))
    (let ((donkey-cursor-normal (car case))
          (cursor-type (car case)))
      (donkey-split-test--keys "*cursors-setting*" donkey-split-test--column
          "t t"
        (should (equal (list (car case) (donkey-split-test--drawn)) case))))))

(ert-deftest donkey-split-cursors-keep-their-own-color-in-every-shape ()
  "Every shape is drawn in the text's color, or the face's, never the cursor's."
  (let ((face-was (face-attribute 'donkey-split-cursor-face :background))
        (text-was (face-attribute 'default :foreground))
        (cursor-was (face-attribute 'cursor :background)))
    (unwind-protect
        (dolist (case '((unspecified "blue") ("gray50" "gray50")))
          (set-face-background 'donkey-split-cursor-face (car case))
          (set-face-foreground 'default "blue")
          (set-face-background 'cursor "red")
          (dolist (graphic '(t nil))
            (cl-letf (((symbol-function 'display-graphic-p)
                       (lambda (&rest _) graphic)))
              (donkey-split-test--keys "*cursors-color*"
                  donkey-split-test--column "t t i"
                (should (equal (list (car case) graphic
                                     (get-text-property
                                      0 'face
                                      (overlay-get (car donkey--split-cursor-marks)
                                                   'before-string)))
                               (list (car case) graphic
                                     (list (if graphic :background :foreground)
                                           (cadr case)))))))))
      (set-face-background 'donkey-split-cursor-face face-was)
      (set-face-foreground 'default text-was)
      (set-face-background 'cursor cursor-was))))

(ert-deftest donkey-split-types-after-a-painted-place-as-one-thing ()
  "`a' types after the whole place, where after `v' it types after the cursor."
  (donkey-split-test--keys "*split-a-v*" "alpha beta\n" "v w a X C-g"
    (should (equal (buffer-string) "alpha Xbeta\n")))
  (donkey-split-test--on "alpha"
    (donkey-split-test--keys "*split-a-place*" "alpha beta\n" "f a X C-g"
      (should (equal (buffer-string) "alphaX beta\n")))
    (donkey-split-test--keys "*split-i-place*" "alpha beta\n" "f i X C-g"
      (should (equal (buffer-string) "Xalpha beta\n")))))

(ert-deftest donkey-split-cursors-bank-every-cursor-s-line ()
  "`m l' banks every cursor's line and keeps the cursors; again, unbanks."
  (donkey-split-test--keys "*cursors-bank*" donkey-split-test--column "t t m l"
    (should (= (donkey--banked-line-count) 3))
    (should (= (length donkey--split-places) 3))
    (should (equal (buffer-string) donkey-split-test--column)))
  (donkey-split-test--keys "*cursors-bank2*" donkey-split-test--column
      "t t m l m l"
    (should (= (donkey--banked-line-count) 0)))
  (donkey-split-test--keys "*cursors-bank3*" donkey-split-test--column
      "t t m l m u"
    (should (= (donkey--banked-line-count) 0))
    (should (= (length donkey--split-places) 3))))

(ert-deftest donkey-split-cursors-bank-the-rest-of-a-partly-banked-column ()
  "Where some cursor lines are banked, `m l' banks the rest."
  (donkey-split-test--keys "*cursors-bank-part*" donkey-split-test--column
      "m l t t m l"
    (should (= (donkey--banked-line-count) 3))))

(ert-deftest donkey-split-cursors-leave-the-bank-for-after ()
  "Lines banked at the cursors are what `d' takes once the cursors end."
  (donkey-split-test--keys "*cursors-bank-d*" donkey-split-test--column
      "t m l C-g d"
    (should (equal (buffer-string) "epsilon zeta\nlast\n"))))

(ert-deftest donkey-split-cursors-draw-an-outline-as-a-box-in-a-terminal ()
  "A hollow cursor is outlined on a graphical frame and a box elsewhere."
  (dolist (graphic '(t nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) graphic)))
      (let ((donkey-cursor-normal 'hollow))
        (donkey-split-test--keys "*cursors-hollow*" donkey-split-test--column
            "t t"
          (should (equal (donkey-split-test--drawn)
                         (if graphic '(hollow) '(box)))))))))

(defconst donkey-split-test--words
  "one two three four\nfive six seven eight\nnine ten eleven\n"
  "Three lines of words for the mark run at the cursors.")

(defun donkey-split-test--texts ()
  "Return the text of every place of the live split, top first."
  (mapcar #'donkey--split-place-text donkey--split-places))

(ert-deftest donkey-split-cursors-M-selects-what-M-selects-at-one-cursor ()
  "A mark run at the cursors selects on the first line what one cursor would."
  (dolist (keys '("M" "M w" "M w w" "M w b" "M b" "M W" "M B" "M S" "M l l"
                  "M g l" "M g h g l" "M * w" "M w ." "M m w" "M w * l"))
    (let (single)
      (donkey-split-test--keys "*cursors-M-one*" donkey-split-test--words keys
        (setq single (buffer-substring (region-beginning) (region-end))))
      (donkey-split-test--keys "*cursors-M-many*" donkey-split-test--words
          (concat "t t " keys)
        (should donkey--split-running)
        (should (equal (list keys (car (donkey-split-test--texts)))
                       (list keys single)))))))

(ert-deftest donkey-split-cursors-M-steps-back-and-forward-as-one-cursor-does ()
  "`u' and `U' in the run step every cursor as they step one cursor."
  (dolist (keys '("M w u" "M w w u" "M w w u u" "M w u U" "M w w u u U U"
                  "M w u w" "M w b u" "M l u" "M g l u" "M * u" "M w . u"
                  "M m w u" "v w w M w u" "M w u b"))
    (let (single)
      (donkey-split-test--keys "*cursors-M-u-one*" donkey-split-test--words keys
        (setq single (buffer-substring (region-beginning) (region-end))))
      (donkey-split-test--keys "*cursors-M-u-many*" donkey-split-test--words
          (concat "t t " keys)
        (should donkey--split-running)
        (should (equal (list keys (car (donkey-split-test--texts)))
                       (list keys single)))
        (should (equal (buffer-string) donkey-split-test--words))))))

(ert-deftest donkey-split-cursors-M-refuses-a-step-that-is-not-there ()
  "Stepping past either end of the run refuses and keeps the run."
  (dolist (case '(("t t M" "u" "No earlier step in this run")
                  ("t t M w u" "u" "No earlier step in this run")
                  ("t t M w M M" "u" "No earlier step in this run")
                  ("t t M w u U" "U" "No later step in this run")
                  ("t t M w u w" "U" "No later step in this run")))
    (donkey-split-test--keys "*cursors-M-u-end*" donkey-split-test--words
        (car case)
      (should (equal (should-error (execute-kbd-macro (kbd (nth 1 case)))
                                   :type 'user-error)
                     (list 'user-error (nth 2 case))))
      (should donkey--split-running))))

(ert-deftest donkey-split-cursors-M-grows-every-cursor-s-own-selection ()
  "Each cursor's run grows from its own word."
  (donkey-split-test--keys "*cursors-M-grow*" donkey-split-test--words
      "l l l l t t M w b"
    (should (equal (donkey-split-test--texts)
                   '("one two three" "five six seven" "nine ten eleven")))))

(ert-deftest donkey-split-cursors-M-refuses-what-would-leave-the-line ()
  "Keys that would take a selection off its line beep and keep the run."
  (dolist (key '("j" "k" "J" "K" "g g" "g e" "G" "v" "V" "m p"))
    (donkey-split-test--keys "*cursors-M-refuse*" donkey-split-test--words
        "t t M w"
      (condition-case nil (execute-kbd-macro (kbd key)) (error nil))
      (should (equal (list key donkey--split-running (donkey-split-test--texts))
                     (list key t '("one two" "five six" "nine ten"))))
      (should (equal (buffer-string) donkey-split-test--words)))))

(ert-deftest donkey-split-cursors-M-ends-on-a-verb-that-keeps-the-cursors ()
  "`M w d' deletes at every cursor, ends the run and keeps the cursors."
  (donkey-split-test--keys "*cursors-M-d*" donkey-split-test--words "t t M w d"
    (should (equal (buffer-string) " three four\n seven eight\n eleven\n"))
    (should (equal (car kill-ring) "one two\nfive six\nnine ten"))
    (should-not donkey--split-running)
    (should (= (length donkey--split-places) 3))))

(ert-deftest donkey-split-cursors-M-and-C-g-end-the-run-one-level-at-a-time ()
  "`M' or `C-g' ends the run and keeps the cursors; `C-g' again ends them."
  (dolist (keys '("t t M w M" "t t M w C-g"))
    (donkey-split-test--keys "*cursors-M-end*" donkey-split-test--words keys
      (should-not donkey--split-running)
      (should (= (length donkey--split-places) 3))
      (should-not (seq-some #'donkey--split-cursor-selecting-p
                            donkey--split-places))))
  (donkey-split-test--keys "*cursors-M-end2*" donkey-split-test--words
      "t t M w C-g C-g"
    (should (null donkey--split-places))))

(ert-deftest donkey-split-cursors-M-starts-where-there-is-no-word ()
  "With no word to select, the run starts with nothing selected."
  (donkey-split-test--keys "*cursors-M-empty*" "\n\n\n" "t t M"
    (should donkey--split-running)
    (should (equal (donkey-split-test--texts) '("" "" ""))))
  (donkey-split-test--keys "*cursors-M-empty-quit*" "\n\n\n" "t t M C-g"
    (should-not donkey--split-running)
    (should (= (length donkey--split-places) 3))))

(ert-deftest donkey-split-cursors-M-takes-the-selections-it-finds ()
  "Selections the cursors hold are taken into the run and grow from there."
  (donkey-split-test--keys "*cursors-M-adopt*" donkey-split-test--words
      "t t v w w M w"
    (should (equal (donkey-split-test--texts)
                   '("one two three" "five six seven" "nine ten eleven"))))
  (donkey-split-test--keys "*cursors-M-adopt-V*" donkey-split-test--words
      "t t V M"
    (should donkey--split-running)
    (should (equal (donkey-split-test--texts)
                   '("one two three four" "five six seven eight"
                     "nine ten eleven")))))

(ert-deftest donkey-split-cursors-change-case-at-every-cursor ()
  "`M-u', `M-l' and `M-c' act on each selection, or the word at each cursor."
  (dolist (case '(("t t M-u" "ALPHA beta\nGAMMA delta\nEPSILON zeta\nlast\n")
                  ("t t M-c" "Alpha beta\nGamma delta\nEpsilon zeta\nlast\n")
                  ("t t C-u 2 M-u"
                   "ALPHA BETA\nGAMMA DELTA\nEPSILON ZETA\nlast\n")
                  ("t t v w M-u" "ALPHA beta\nGAMMA delta\nEPSILON zeta\nlast\n")
                  ("t t V M-u" "ALPHA BETA\nGAMMA DELTA\nEPSILON ZETA\nlast\n")
                  ("t t M-u M-b M-l"
                   "alpha beta\nGAMMA delta\nEPSILON zeta\nlast\n")))
    (donkey-split-test--keys "*cursors-case*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (buffer-string)) case)))))

(ert-deftest donkey-split-cursors-answer-a-case-key-the-reader-bound ()
  "A key the reader gave `upcase-region' in Normal state works at the cursors."
  (let ((was (keymap-lookup donkey-leader-map "U")))
    (unwind-protect
        (progn
          (keymap-set donkey-leader-map "U" #'upcase-region)
          (donkey-split-test--keys "*cursors-case-leader*"
              donkey-split-test--column "t t SPC U"
            (should (equal (buffer-string)
                           "ALPHA beta\nGAMMA delta\nEPSILON zeta\nlast\n"))
            (should (= (length donkey--split-places) 3))))
      (if was
          (keymap-set donkey-leader-map "U" was)
        (keymap-unset donkey-leader-map "U" t)))))

(ert-deftest donkey-split-cursors-change-case-without-leaving-the-line ()
  "At a line's end there is no word to change, and the next line is untouched."
  (donkey-split-test--keys "*cursors-case-eol*" donkey-split-test--column
      "t t g l M-u"
    (should (equal (buffer-string) donkey-split-test--column))))

(ert-deftest donkey-split-cursors-keep-their-place-through-a-line-edit ()
  "An edit over a whole-line selection leaves every cursor where it was."
  (donkey-split-test--keys "*cursors-case-V*" "ab cd\nef gh\n" "l l l t V M-u"
    (should (equal (buffer-string) "AB CD\nEF GH\n"))
    (should (equal (donkey-split-test--cursors) '(4 10)))))

(ert-deftest donkey-split-cursors-repeat-their-last-command ()
  "`.' runs the cursors' last command again at every cursor."
  (dolist (case '(("l t t x ." "aha beta\ngma delta\neilon zeta\nlast\n")
                  ("t t C-u 2 x ." "a beta\na delta\nlon zeta\nlast\n")
                  ("t t M-u ." "ALPHA BETA\nGAMMA DELTA\nEPSILON ZETA\nlast\n")))
    (donkey-split-test--keys "*cursors-repeat*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (buffer-string)) case))
      (should (= (length donkey--split-places) 3))))
  (donkey-split-test--keys "*cursors-repeat-w*" donkey-split-test--column
      "t t w ."
    (should (equal (donkey-split-test--cursors) '(11 23 36))))
  (donkey-split-test--keys "*cursors-repeat-none*" donkey-split-test--column
      "t t"
    (condition-case nil (execute-kbd-macro (kbd ".")) (error nil))
    (should (= (length donkey--split-places) 3))
    (should (equal (buffer-string) donkey-split-test--column))))

(ert-deftest donkey-split-cursors-undo-and-redo-keeping-the-cursors ()
  "`u' takes an edit back at every cursor at once, `U' puts it back."
  (dolist (case '(("l t t x u" nil) ("l t t x x u u" nil)
                  ("l t t x u U" "apha beta\ngmma delta\nesilon zeta\nlast\n")))
    (donkey-split-test--keys "*cursors-undo*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (buffer-string))
                     (list (car case)
                           (or (cadr case) donkey-split-test--column))))
      (should (= (length donkey--split-places) 3)))))

(ert-deftest donkey-split-cursors-stay-through-a-recenter ()
  "`z z' and `C-l' move the view, not the cursors, and keep them."
  (dolist (keys '("t t z z" "t t C-l"))
    (donkey-split-test--keys "*cursors-recenter*" donkey-split-test--column keys
      (should (equal (donkey-split-test--cursors) '(1 12 24))))))

(ert-deftest donkey-split-cursors-indent-and-comment-their-lines ()
  "`>' indents and `C' comments every cursor's line, `C' again uncomments."
  (dolist (case '(("j t >" "(a\nb\nc)\n" "(a\n b\n c)\n")
                  ("t t C" "(a)\n(b)\n(c)\n" ";; (a)\n;; (b)\n;; (c)\n")
                  ("t t C C" "(a)\n(b)\n(c)\n" "(a)\n(b)\n(c)\n")))
    (donkey-test-keys--harness "*cursors-lisp*" #'emacs-lisp-mode ()
        (nth 1 case) (car case)
      (should (equal (list (car case) (buffer-string))
                     (list (car case) (nth 2 case)))))))

(ert-deftest donkey-split-cursors-refuse-to-comment-in-an-org-source-block ()
  "`C' at the cursors inside an Org source block refuses and changes nothing."
  (let ((text "#+begin_src emacs-lisp\n(a)\n(b)\n#+end_src\n"))
    (donkey-test-keys--harness "*cursors-org*" #'org-mode () text "j t"
      (should (equal (should-error (execute-kbd-macro (kbd "C"))
                                   :type 'user-error)
                     '(user-error "Not at the cursors inside an Org source block")))
      (should (equal (buffer-string) text))
      (should (= (length donkey--split-places) 2)))))

(ert-deftest donkey-split-cursors-take-back-an-edit-that-fails-at-one ()
  "An edit that fails at one cursor is undone at the cursors before it."
  (donkey-split-test--keys "*cursors-rollback*" "ab\ncd\n" "t"
    (put-text-property 4 6 'read-only t)
    (should-error (execute-kbd-macro (kbd "M-u")))
    (should (equal (buffer-string) "ab\ncd\n"))
    (should (equal (donkey-split-test--cursors) '(1 4)))))

(ert-deftest donkey-split-cursors-open-lines-and-type-on-them ()
  "`o' and `O' open a line at every cursor and type there, then come back."
  (dolist (case '(("t t o X C-g"
                   "alpha beta\nX\ngamma delta\nX\nepsilon zeta\nX\nlast\n")
                  ("t t O X C-g"
                   "X\nalpha beta\nX\ngamma delta\nX\nepsilon zeta\nlast\n")))
    (donkey-split-test--keys "*cursors-open*" donkey-split-test--column
        (car case)
      (should (equal (list (car case) (buffer-string)) case))
      (should (= (length donkey--split-places) 3)))))

(defconst donkey-split-test--five
  "alpha beta\ngamma delta\nepsilon zeta\nlast one\nfinal\n"
  "Five lines for making cursors from a selection.")

(ert-deftest donkey-split-t-puts-a-cursor-on-every-selected-line ()
  "Over a selection of lines every line gets a cursor, the real one at point."
  (dolist (case '(("V j j t" (1 12 24) 24)
                  ("l l V j j t" (1 12 24) 24)
                  ("j j l l V k k t" (1 12 24) 1)
                  ("v j j l t" (2 13 25) 25)
                  ("l l m v j j t" (4 15 27) 27)
                  ("g g V G t" (1 12 24 37 46) 46)))
    (donkey-split-test--keys "*cursors-from-selection*" donkey-split-test--five
        (car case)
      (should (equal (list (car case) (donkey-split-test--cursors)
                           (donkey--split-cursor donkey--split-primary))
                     case))
      (should-not (region-active-p))
      (should (equal (buffer-string) donkey-split-test--five)))))

(ert-deftest donkey-split-t-on-a-selection-within-one-line-adds-below ()
  "A selection inside one line is let go of, and a cursor is added below."
  (donkey-split-test--keys "*cursors-one-line-sel*" donkey-split-test--five
      "v l l t"
    (should (equal (donkey-split-test--cursors) '(3 14)))))

(ert-deftest donkey-split-T-adds-a-cursor-above ()
  "`donkey-split-add-cursor-above' adds above the first; a count, that many."
  (dolist (case '(("j j T" (12 24)) ("j j T T" (1 12 24))
                  ("j j C-u 2 T" (1 12 24)) ("j j t T" (12 24 37))))
    (donkey-split-test--keys "*cursors-above*" donkey-split-test--five
        (car case)
      (should (equal (list (car case) (donkey-split-test--cursors)) case))
      (should (= (donkey--split-cursor donkey--split-primary) 24)))))

(ert-deftest donkey-split-T-with-no-line-above-changes-nothing ()
  "Adding above the first line refuses, and the cursors stay as they were."
  (donkey-split-test--keys "*cursors-above-none*" donkey-split-test--five "j T"
    (should (equal (should-error (execute-kbd-macro (kbd "T"))
                                 :type 'user-error)
                   '(user-error "No line above")))
    (should (equal (donkey-split-test--cursors) '(1 12)))))

(ert-deftest donkey-split-DEL-drops-the-cursor-added-last ()
  "`DEL' takes back the newest cursor and never the real one."
  (dolist (case '(("t t DEL" (1 12)) ("t t t DEL DEL" (1 12))
                  ("j j T t DEL" (12 24)) ("l l V j j t DEL DEL" (24))))
    (donkey-split-test--keys "*cursors-drop*" donkey-split-test--five
        (car case)
      (should (equal (list (car case) (donkey-split-test--cursors)) case))))
  (dolist (keys '("t DEL" "l l V j j t V d"))
    (donkey-split-test--keys "*cursors-drop-last*" donkey-split-test--five keys
      (should (= (length donkey--split-places) 1))
      (condition-case nil (execute-kbd-macro (kbd "DEL")) (error nil))
      (should (equal (list keys (length donkey--split-places)) (list keys 1)))
      (should donkey--split-cursors))))

(ert-deftest donkey-split-add-cursor-above-is-bound-to-T-in-normal-state ()
  "The key the README names reaches the command."
  (should (eq (keymap-lookup donkey-normal-mode-map "T")
              #'donkey-split-add-cursor-above)))

(ert-deftest donkey-split-rectangle-change-gives-what-the-prompt-gave ()
  "Typing at the rows of `m v c' leaves what `string-rectangle' left."
  (dolist (case '(("abcdef\ngh\nmnopqr\n" "l l l l m v j j l" "X Y"
                   "abcdXY\ngh  XY\nmnopXY\n" ("ef" "  " "qr"))
                  ("aaaaa\nbb\nccccccc\n" "g l m v j j" ";"
                   "aaaaa;\nbb   ;\nccccc;cc\n" ("" "" ""))
                  ("\tfoo\n\tbar\n" "m v j" "SPC SPC"
                   "  foo\n  bar\n" ("        " "        "))
                  ("漢字abc\n漢字abc\n" "l m v j" "X" "漢Xabc\n漢Xabc\n" ("字" "字"))
                  ("abcdef\nghijkl\nmnopqr\n" "j j l l m v k k l" "Z"
                   "abZef\nghZkl\nmnZqr\n" ("cd" "ij" "op"))))
    (cl-destructuring-bind (text keys typed result block) case
      (donkey-split-test--keys "*rect-change*" text
          (concat keys " c " typed " C-g C-g")
        (should (equal (list keys (buffer-string) killed-rectangle)
                       (list keys result block)))))))

(ert-deftest donkey-split-rectangle-change-puts-the-real-cursor-on-point-s-row ()
  "The real cursor of `m v c' is on the row point was on."
  (dolist (case '(("m v j j l c" 3) ("j j m v k k l c" 1)))
    (donkey-split-test--keys "*rect-change-primary*" "abc\ndef\nghi\n"
        (car case)
      (should (equal (list (car case)
                           (line-number-at-pos
                            (donkey--split-cursor donkey--split-primary)))
                     case)))))

(ert-deftest donkey-split-cursors-past-the-limit-are-refused ()
  "A press that would pass the limit makes nothing, and says why."
  (let ((donkey-split-cursor-limit 3)
        (donkey-split-cursor-limit-ask nil))
    (donkey-split-test--keys "*cursors-limit*" donkey-split-test--five "t t"
      (should (equal (cadr (should-error (execute-kbd-macro (kbd "t"))
                                         :type 'user-error))
                     (format-message "4 cursors would pass `%s' (3)"
                                     'donkey-split-cursor-limit)))
      (should (= (length donkey--split-places) 3)))
    (donkey-split-test--keys "*cursors-limit-sel*" donkey-split-test--five "% "
      (should-error (execute-kbd-macro (kbd "t")) :type 'user-error)
      (should (null donkey--split-places))
      (should (equal (buffer-string) donkey-split-test--five)))))

(ert-deftest donkey-split-cursors-past-the-limit-ask-when-told-to ()
  "With asking on, the answer decides; a no makes nothing."
  (dolist (answer '(t nil))
    (let ((donkey-split-cursor-limit 2)
          (donkey-split-cursor-limit-ask t))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) answer)))
        (donkey-split-test--keys "*cursors-limit-ask*" donkey-split-test--five
            ""
          (condition-case nil (execute-kbd-macro (kbd "t t")) (error nil))
          (should (equal (list answer (length donkey--split-places))
                         (list answer (if answer 3 2)))))))))

(ert-deftest donkey-split-cursor-limit-is-read-with-care ()
  "No limit at all when nil, and the default where the value is not a count."
  (dolist (case '((nil 5) ("many" 5) (4 4)))
    (let ((donkey-split-cursor-limit (car case))
          (donkey-split-cursor-limit-ask nil))
      (donkey-split-test--keys "*cursors-limit-read*" donkey-split-test--five
          ""
        (execute-kbd-macro (kbd "C-u 3 t"))
        (condition-case nil (execute-kbd-macro (kbd "t")) (error nil))
        (should (equal (list (car case) (length donkey--split-places))
                       case))))))

(ert-deftest donkey-split-rectangle-change-past-the-limit-asks-in-the-minibuffer ()
  "A block taller than the limit is changed through the one-pass prompt."
  (let ((donkey-split-cursor-limit 2)
        (donkey-split-cursor-limit-ask nil))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "XY")))
      (donkey-split-test--keys "*rect-change-limit*" "abcdef\nghijkl\nmnopqr\n"
          "l l m v j j l l c"
        (should (equal (buffer-string) "abXYf\nghXYl\nmnXYr\n"))
        (should (null donkey--split-places))
        (should (bound-and-true-p donkey-normal-mode))
        (should (equal killed-rectangle '("cde" "ijk" "opq")))))))

(ert-deftest donkey-split-cursors-hold-no-markers ()
  "A cursor is an end of its overlay, never a marker Emacs must move."
  (donkey-split-test--keys "*cursors-no-markers*" donkey-split-test--five
      "t t v w t"
    (dolist (place donkey--split-places)
      (should-not (seq-some #'markerp (overlay-properties place))))))

(ert-deftest donkey-split-cursors-are-drawn-only-where-a-window-can-show-them ()
  "Of many cursors, only those near what the window shows are drawn."
  (let ((donkey-split-cursor-limit nil)
        (text (mapconcat (lambda (i) (format "line %d" i))
                         (number-sequence 1 2000) "\n")))
    (donkey-split-test--keys "*cursors-drawn-visible*" (concat text "\n") "% t"
      (should (= (length donkey--split-places) 2000))
      (should (< 0 (length donkey--split-cursor-marks) 500)))))

(defconst donkey-split-test--ragged
  "alpha beta gamma\nx yy zzz wwww\nlonger words here\nab\n"
  "Lines whose words differ in length, for lining the cursors up.")

(defun donkey-split-test--columns ()
  "Return the column of every cursor, top first."
  (mapcar (lambda (place)
            (save-excursion
              (goto-char (donkey--split-cursor place))
              (current-column)))
          donkey--split-places))

(ert-deftest donkey-split-cursors-line-up-under-the-real-one ()
  "The equals key puts every cursor at the real one's column, or a line's end."
  (dolist (case '(("t t t w" (5 1 6 2)) ("t t t w =" (5 5 5 2))
                  ("t t t w w =" (10 10 10 2)) ("l l l t t t b =" (0 0 0 0))))
    (donkey-split-test--keys "*cursors-align*" donkey-split-test--ragged
        (car case)
      (should (equal (list (car case) (donkey-split-test--columns)) case))
      (should (equal (buffer-string) donkey-split-test--ragged)))))

(ert-deftest donkey-split-cursors-line-up-then-type-in-one-column ()
  "After lining the cursors up, typing lands in one column on every row."
  (donkey-split-test--keys "*cursors-align-type*" donkey-split-test--ragged
      "t t t w = i | C-g"
    (should (equal (buffer-string)
                   "alpha| beta gamma\nx yy |zzz wwww\nlonge|r words here\nab|\n"))))

(ert-deftest donkey-split-cursors-wrap-in-equals-when-selecting ()
  "With selections, the equals key wraps them as any delimiter does."
  (donkey-split-test--keys "*cursors-align-wrap*" donkey-split-test--ragged
      "t t t v w ="
    (should (equal (buffer-string)
                   "=alpha= beta gamma\n=x= yy zzz wwww\n=longer= words here\n=ab=\n"))))

(ert-deftest donkey-split-add-cursor-is-bound-to-t-in-normal-state ()
  "The key the README names reaches the command."
  (should (eq (keymap-lookup donkey-normal-mode-map "t")
              #'donkey-split-add-cursor)))

;;; ---------------------------------------------------------------------------
;;; Writing many places, and what the writing leaves for undo
;;; ---------------------------------------------------------------------------

(defun donkey-split-test--numbered-lines (n)
  "Return N lines reading foo 1 to foo N."
  (mapconcat (lambda (i) (format "foo %d\n" i)) (number-sequence 1 n) ""))

(defun donkey-split-test--last-line ()
  "Return the buffer's last line that is not empty."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (buffer-substring-no-properties (point) (line-end-position))))


(defmacro donkey-split-test--with-no-key-waiting (&rest body)
  "Run BODY with `input-pending-p' answering nil.

The sweep yields to a waiting key, and a terminal frame can report one
that is not a key at all; what these tests pin is the sweep, not the
frame."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) nil)))
     ,@body))

(ert-deftest donkey-split-finds-the-place-point-is-in-at-its-edges ()
  "The place point is in is found at its start, inside it, at its end, and empty."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-at-point*" "foo bar foo\n" "v G f"
      (let ((first (car donkey--split-places))
            (second (cadr donkey--split-places)))
        (dolist (case (list (cons (overlay-start first) first)
                            (cons (1+ (overlay-start first)) first)
                            (cons (overlay-end first) first)
                            (cons (1+ (overlay-end first)) nil)
                            (cons (overlay-start second) second)
                            (cons (overlay-end second) second)))
          (goto-char (car case))
          (should (eq (donkey--split-place-at-point) (cdr case)))))))
  (donkey-split-test--on "^"
    (donkey-split-test--keys "*split-at-point-empty*" "a\nb\n" "v G f"
      (goto-char (point-min))
      (should (eq (donkey--split-place-at-point) (car donkey--split-places)))
      (goto-char (point-max))
      (should (null (donkey--split-place-at-point))))))

(ert-deftest donkey-split-rewrites-a-place-whose-text-changed-under-it ()
  "A place no longer holding what the primary held is rewritten whole.

Even where what it holds is of the same length: the places are
compared by their text, not by their size."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-same-length*" "a foo b\nc foo d\n"
        "v G f a X"
      (let ((donkey--split-copying t)
            (buffer-undo-list t))
        (save-excursion
          (goto-char (point-max))
          (search-backward "X")
          (delete-char 1)
          (insert "Y")))
      (should (equal (buffer-string) "a fooX b\nc fooY d\n"))
      (execute-kbd-macro (kbd "Z C-g"))
      (should (equal (buffer-string) "a fooXZ b\nc fooXZ d\n")))))

(ert-deftest donkey-split-undo-after-the-split-takes-the-writing-back-everywhere ()
  "Once the split has ended, one undo takes what it wrote back at every place."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-undo-after*" "a foo b\nc foo d\ne foo f\n"
        "v G f a X Y C-g u"
      (should (equal (buffer-string) "a foo b\nc foo d\ne foo f\n"))
      (execute-kbd-macro (kbd "U"))
      (should (equal (buffer-string) "a fooXY b\nc fooXY d\ne fooXY f\n"))
      (execute-kbd-macro (kbd "u"))
      (should (equal (buffer-string) "a foo b\nc foo d\ne foo f\n")))))

(ert-deftest donkey-split-records-its-writing-as-one-undo-entry ()
  "The writing is one entry on the undo list, whatever the number of places."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-one-entry*"
        (donkey-split-test--numbered-lines 50) "v G f a X Y Z C-g"
      (should (= 1 (seq-count (lambda (entry) (eq (car-safe entry) 'apply))
                              buffer-undo-list)))
      ;; The entry, its boundaries, and what the harness recorded
      ;; putting the text in: nothing per place.
      (should (< (length buffer-undo-list) 8))
      (should (equal (donkey-split-test--last-line) "fooXYZ 50")))))

(ert-deftest donkey-split-c-and-the-writing-are-two-undo-steps ()
  "After c, one undo takes the typing back and a second puts the matches back."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-c-undo*" "a foo b\nc foo d\n"
        "v G f c X C-g u u"
      (should (equal (buffer-string) "a foo b\nc foo d\n"))
      (execute-kbd-macro (kbd "U U"))
      (should (equal (buffer-string) "a X b\nc X d\n")))))

(ert-deftest donkey-split-undo-puts-back-what-a-backspace-past-the-edges-took ()
  "Text a Backspace deleted just before every place comes back with one undo."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-edge-undo*" "a foo b\nc foo d\ne foo f\n"
        "v G f i DEL DEL C-g"
      (should (equal (buffer-string) "foo b\nfoo d\nfoo f\n"))
      (execute-kbd-macro (kbd "u"))
      (should (equal (buffer-string) "a foo b\nc foo d\ne foo f\n"))
      (execute-kbd-macro (kbd "U"))
      (should (equal (buffer-string) "foo b\nfoo d\nfoo f\n")))))

(ert-deftest donkey-split-joins-lines-at-every-place-and-undo-parts-them ()
  "A Backspace at every line-start place joins the lines, and undo parts them."
  (donkey-split-test--on "^"
    (donkey-split-test--keys "*split-join*" "one\ntwo\nthree\nfour\n"
        "j v j f i DEL C-g"
      (should (equal (buffer-string) "onetwothree\nfour\n"))
      (execute-kbd-macro (kbd "u"))
      (should (equal (buffer-string) "one\ntwo\nthree\nfour\n")))))

(ert-deftest donkey-split-ended-by-an-edit-away-keeps-undo-whole ()
  "After an edit away from the places ends the split, undo takes everything back."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-stray-undo*" "a foo b\nc foo d\n" "v G f a X"
      (local-set-key (kbd "<f9>")
                     (lambda ()
                       (interactive)
                       (save-excursion (goto-char (point-max)) (insert "Z"))))
      (execute-kbd-macro (kbd "<f9>"))
      (should (null donkey--split-phase))
      (should (equal (buffer-string) "a fooX b\nc fooX d\nZ"))
      (should (= 1 (seq-count (lambda (entry) (eq (car-safe entry) 'apply))
                              buffer-undo-list)))
      (execute-kbd-macro (kbd "C-g u"))
      (should (equal (buffer-string) "a foo b\nc foo d\n"))
      (execute-kbd-macro (kbd "U"))
      (should (equal (buffer-string) "a fooX b\nc fooX d\nZ")))))

(ert-deftest donkey-split-refuses-to-undo-over-text-that-changed-since ()
  "The writing's undo entry changes nothing where a place no longer holds what it wrote."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-undo-refuses*" "a foo b\nc foo d\n"
        "v G f a X C-g"
      (let ((buffer-undo-list t))
        (save-excursion
          (goto-char (point-max))
          (search-backward "X")
          (delete-char 1)))
      (should-error (execute-kbd-macro (kbd "u")))
      (should (equal (buffer-string) "a fooX b\nc foo d\n")))))

(ert-deftest donkey-split-undo-after-a-place-refused-takes-back-what-was-written ()
  "After a place turned read-only ended the split, undo takes the writing back."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-late-ro-undo*" "a foo b\nc foo d\ne foo f\n"
        "v G f a"
      (donkey-split-test--read-only-last)
      (execute-kbd-macro (kbd "X"))
      (should (null donkey--split-phase))
      (should (equal (buffer-string) "a fooX b\nc foo d\ne foo f\n"))
      (execute-kbd-macro (kbd "C-g u"))
      (should (equal (buffer-string) "a foo b\nc foo d\ne foo f\n"))
      (let ((inhibit-read-only t))
        (remove-text-properties (point-min) (point-max) '(read-only nil))))))

(ert-deftest donkey-split-writes-the-places-a-window-shows-first ()
  "Past the eager count, the places in view are written with the keystroke and the rest after."
  (let ((donkey--split-eager-places 3))
   (donkey-split-test--with-no-key-waiting
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-behind*"
          (donkey-split-test--numbered-lines 1000) "v G f a X"
        (should donkey--split-behind)
        (should donkey--split-sweep-timer)
        ;; The cursor's own line is in view whatever the frame.
        (should (equal (buffer-substring (point-min) (line-end-position))
                       "fooX 1"))
        ;; The places in view, then the eager count, and no more.
        (should (<= (- 1000 (length donkey--split-behind))
                    (+ (length (donkey--split-visible-places)) 3)))
        (donkey--split-sweep-run (current-buffer))
        (should (null donkey--split-behind))
        (should (null donkey--split-sweep-timer))
        (should (equal (donkey-split-test--last-line) "fooX 1000"))
        ;; Nothing of the sweep is recorded for undo.
        (should (< (length buffer-undo-list) 8))
        (execute-kbd-macro (kbd "Y C-g u"))
        (should (equal (donkey-split-test--last-line) "foo 1000"))
        (should (equal (buffer-substring (point-min) (line-end-position))
                       "foo 1")))))))

(ert-deftest donkey-split-a-waiting-key-leaves-the-rest-to-the-timer ()
  "The sweep stops for a key that is waiting, and the timer takes the rest."
  (let ((donkey--split-eager-places 3))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-key-waits*"
          (donkey-split-test--numbered-lines 1000) "v G f a X"
        (let ((behind (length donkey--split-behind)))
          (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) t)))
            (donkey--split-sweep-run (current-buffer)))
          (should (= (length donkey--split-behind) behind))
          (should donkey--split-sweep-timer))
        (donkey-split-test--with-no-key-waiting
          (donkey--split-sweep-run (current-buffer)))
        (should (null donkey--split-behind))
        (should (equal (donkey-split-test--last-line) "fooX 1000"))))))

(ert-deftest donkey-split-brings-the-places-newly-in-view-up-to-date-first ()
  "When the view moves onto places still behind, they are written with the next key."
  (let ((donkey--split-eager-places 3))
   (donkey-split-test--with-no-key-waiting
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-view*"
          (donkey-split-test--numbered-lines 1000) "v G f a X"
        (local-set-key (kbd "<f9>") #'ignore)
        (set-window-start (selected-window)
                          (save-excursion
                            (goto-char (point-max))
                            (forward-line -10)
                            (point)))
        (execute-kbd-macro (kbd "<f9>"))
        (should (equal (donkey-split-test--last-line) "fooX 1000"))
        (should donkey--split-behind))))))

(ert-deftest donkey-split-leaves-no-timer-behind-when-it-ends ()
  "Ending the split, or killing its buffer, with places behind cancels the sweep."
  (let ((donkey--split-eager-places 3))
   (donkey-split-test--with-no-key-waiting
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-timer-end*"
          (donkey-split-test--numbered-lines 1000) "v G f a X"
        (let ((timer donkey--split-sweep-timer))
          (should timer)
          (execute-kbd-macro (kbd "C-g"))
          (should-not (memq timer timer-list))
          (should (null donkey--split-sweep-timer))
          (should (equal (donkey-split-test--last-line) "fooX 1000")))))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-timer-kill*"
          (donkey-split-test--numbered-lines 1000) "v G f a X"
        (let ((timer donkey--split-sweep-timer))
          (should timer)
          (kill-buffer (current-buffer))
          (should-not (memq timer timer-list))))))))

(ert-deftest donkey-split-cursors-writing-is-one-undo-step-at-the-cursors ()
  "Back at the cursors, u takes what was typed at every cursor back in one step."
  (donkey-split-test--keys "*cursors-write-undo*" donkey-split-test--column
      "t t i X C-g u"
    (should (equal (buffer-string) donkey-split-test--column))
    (should (= (length donkey--split-places) 3))
    (execute-kbd-macro (kbd "U"))
    (should (equal (buffer-string)
                   "Xalpha beta\nXgamma delta\nXepsilon zeta\nlast\n"))))

;;; ---------------------------------------------------------------------------
;;; What a verb records for undo
;;; ---------------------------------------------------------------------------

(defun donkey-split-test--split-entries ()
  "Return how many entries of the split's own kind are on `buffer-undo-list'."
  (seq-count (lambda (entry)
               (and (eq (car-safe entry) 'apply)
                    (eq (nth 4 entry) #'donkey--split-put-back)))
             buffer-undo-list))

(ert-deftest donkey-split-d-is-one-undo-entry-whatever-the-number-of-places ()
  "Deleting fifty places records one entry, and undo puts every one back."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-d-entry*"
        (donkey-split-test--numbered-lines 50) "v G f d"
      (should (= 1 (donkey-split-test--split-entries)))
      (should (< (length buffer-undo-list) 8))
      (should (equal (donkey-split-test--last-line) " 50"))
      (execute-kbd-macro (kbd "u"))
      (should (equal (donkey-split-test--last-line) "foo 50"))
      (should (equal (buffer-substring (point-min) (line-end-position))
                     "foo 1"))
      (execute-kbd-macro (kbd "U"))
      (should (equal (donkey-split-test--last-line) " 50")))))

(ert-deftest donkey-split-d-over-places-that-differ-puts-each-text-back ()
  "Deleting places that differ is one entry too, and undo gives each its own."
  (donkey-split-test--on "[0-9]+"
    (donkey-split-test--keys "*split-d-differ*" "a 1 b\nc 22 d\ne 333 f\n"
        "v G f d"
      (should (equal (buffer-string) "a  b\nc  d\ne  f\n"))
      (should (= 1 (donkey-split-test--split-entries)))
      (execute-kbd-macro (kbd "u"))
      (should (equal (buffer-string) "a 1 b\nc 22 d\ne 333 f\n")))))

(ert-deftest donkey-split-c-records-its-emptying-as-one-entry ()
  "After c, the emptying and the writing are one entry each."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-c-entry*"
        (donkey-split-test--numbered-lines 50) "v G f c X C-g"
      (should (= 2 (donkey-split-test--split-entries)))
      (should (< (length buffer-undo-list) 10))
      (execute-kbd-macro (kbd "u u"))
      (should (equal (donkey-split-test--last-line) "foo 50"))
      (execute-kbd-macro (kbd "U U"))
      (should (equal (donkey-split-test--last-line) "X 50")))))

(ert-deftest donkey-split-a-wrap-and-its-removal-are-one-entry-each ()
  "Each wrap is one entry, taken back one pair at a time."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-wrap-entry*" "a foo b\nc foo d\n"
        "v G f ( [ C-g"
      (should (equal (buffer-string) "a ([foo]) b\nc ([foo]) d\n"))
      (should (= 2 (donkey-split-test--split-entries)))
      (execute-kbd-macro (kbd "u"))
      (should (equal (buffer-string) "a (foo) b\nc (foo) d\n"))
      (execute-kbd-macro (kbd "U"))
      (should (equal (buffer-string) "a ([foo]) b\nc ([foo]) d\n"))
      (execute-kbd-macro (kbd "u u"))
      (should (equal (buffer-string) "a foo b\nc foo d\n"))
      (execute-kbd-macro (kbd "U U"))
      (should (equal (buffer-string) "a ([foo]) b\nc ([foo]) d\n"))))
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-unwrap-entry*" "a foo b\nc foo d\n"
        "v G f ( ( C-g"
      (should (equal (buffer-string) "a foo b\nc foo d\n"))
      (should (= 2 (donkey-split-test--split-entries)))
      (execute-kbd-macro (kbd "u"))
      (should (equal (buffer-string) "a (foo) b\nc (foo) d\n"))
      (execute-kbd-macro (kbd "U"))
      (should (equal (buffer-string) "a foo b\nc foo d\n")))))

(ert-deftest donkey-split-a-wrap-s-record-holds-its-two-delimiters-once ()
  "A wrap over fifty places records the opener and the closer, not a hundred strings."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-wrap-size*"
        (donkey-split-test--numbered-lines 50) "v G f ("
      (let ((entry (seq-find (lambda (entry)
                               (and (eq (car-safe entry) 'apply)
                                    (eq (nth 4 entry) #'donkey--split-put-back)))
                             buffer-undo-list)))
        (should entry)
        ;; A hundred positions, two delimiters, one empty string.
        (should (< (donkey--split-entry-size entry) 1000))
        (execute-kbd-macro (kbd "C-g u"))
        (should (equal (donkey-split-test--last-line) "foo 50"))
        (execute-kbd-macro (kbd "U"))
        (should (equal (donkey-split-test--last-line) "(foo) 50"))))))

(ert-deftest donkey-split-wraps-empty-places-and-takes-the-pair-off-again ()
  "An empty place is wrapped opener first, and the pair comes off whole."
  (donkey-split-test--on "$"
    (donkey-split-test--keys "*split-wrap-empty*" "ab\ncd\n" "v G f ( ("
      (should (equal (buffer-string) "ab\ncd\n"))
      (execute-kbd-macro (kbd "("))
      (should (equal (buffer-string) "ab()\ncd()\n"))
      (execute-kbd-macro (kbd "C-g u u"))
      (should (equal (buffer-string) "ab()\ncd()\n")))))

(ert-deftest donkey-split-cursor-verbs-record-one-entry-each ()
  "Every verb at the cursors is one undo entry, taken back in order."
  (donkey-split-test--keys "*cursors-entries*" donkey-split-test--column
      "t t m w d"
    (should (equal (buffer-string) " beta\n delta\n zeta\nlast\n"))
    (should (= 1 (donkey-split-test--split-entries)))
    (execute-kbd-macro (kbd "p"))
    (should (equal (buffer-string) donkey-split-test--column))
    (should (= 2 (donkey-split-test--split-entries)))
    (execute-kbd-macro (kbd "D"))
    (should (equal (buffer-string) "alpha\ngamma\nepsilon\nlast\n"))
    (should (= 3 (donkey-split-test--split-entries)))
    (execute-kbd-macro (kbd "o Z C-g"))
    (should (equal (buffer-string)
                   "alpha\nZ\ngamma\nZ\nepsilon\nZ\nlast\n"))
    (should (= 5 (donkey-split-test--split-entries)))
    ;; The cursors stand after the Z: the case key wants the word ahead.
    (execute-kbd-macro (kbd "g h M-l"))
    (should (equal (buffer-string)
                   "alpha\nz\ngamma\nz\nepsilon\nz\nlast\n"))
    (should (= 6 (donkey-split-test--split-entries)))
    (should (< (length buffer-undo-list) 20))
    (execute-kbd-macro (kbd "u u u u u u"))
    (should (equal (buffer-string) donkey-split-test--column))
    (should (= (length donkey--split-places) 3))
    (execute-kbd-macro (kbd "U U U U U U"))
    (should (equal (buffer-string)
                   "alpha\nz\ngamma\nz\nepsilon\nz\nlast\n"))))

(ert-deftest donkey-split-opening-lines-at-many-cursors-is-not-quadratic ()
  "Opening lines at ten times the cursors costs about ten times, not a hundred.

In `emacs-lisp-mode', whose indentation asks the syntax cache: opened
from the bottom up, every line was parsed from far back."
  (let ((times nil))
    (dolist (n '(100 1000))
      (donkey-test-keys--harness "*cursors-open-scale*" #'emacs-lisp-mode
          ((donkey-split-cursor-limit nil))
          (mapconcat (lambda (i) (format "(setq variable-%d (list %d))" i i))
                     (number-sequence 1 (1+ n)) "\n")
          (format "C-u %d t" n)
        (garbage-collect)
        (let ((t0 (float-time))
              (gc0 gc-elapsed))
          (execute-kbd-macro (kbd "o"))
          (push (- (- (float-time) t0) (- gc-elapsed gc0)) times))))
    (let ((ratio (/ (car times) (max 1e-6 (cadr times)))))
      (should (< ratio 25)))))

(ert-deftest donkey-split-a-place-answers-for-its-face-and-its-mark ()
  "A place carries the split's face and its mark through its category."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-category*" "a foo b\nc foo d\n" "v G f"
      (dolist (place donkey--split-places)
        (should (eq (overlay-get place 'face) 'donkey-split-face))
        (should (overlay-get place 'donkey-split)))))
  (donkey-split-test--keys "*cursors-category*" donkey-split-test--column "t t"
    (dolist (place donkey--split-places)
      (should (eq (overlay-get place 'face) 'donkey-split-face))
      (should (overlay-get place 'donkey-split)))))

(ert-deftest donkey-split-cursors-settle-into-order-after-an-undo-moved-them ()
  "Cursors that came out of order are put back in order, and merged where they share a line."
  (donkey-split-test--keys "*cursors-order*" donkey-split-test--column "t t"
    ;; Move the first cursor below the second by hand, as an undo of
    ;; text before them could.
    (let ((first (car donkey--split-places))
          (third (nth 2 donkey--split-places)))
      (move-overlay first (overlay-start third) (overlay-start third))
      (donkey--split-cursors-settle)
      (should (= (length donkey--split-places) 2))
      (should (equal (donkey-split-test--cursors)
                     (sort (donkey-split-test--cursors) #'<))))))

(ert-deftest donkey-split-a-mark-a-replay-pushes-is-let-go-of ()
  "A marker a command pushed on a mark ring at a cursor points nowhere once the replay is done."
  (donkey-split-test--keys "*cursors-marks*" donkey-split-test--column "t t"
    (let ((pushed nil))
      (cl-letf* ((push (symbol-function 'push-mark))
                 ((symbol-function 'push-mark)
                  (lambda (&rest args)
                    (prog1 (apply push args)
                      (setq pushed (append mark-ring global-mark-ring pushed))))))
        (execute-kbd-macro (kbd "m w")))
      (should (>= (length pushed) 3))
      (should (seq-every-p (lambda (marker) (null (marker-buffer marker)))
                           pushed))
      ;; The cursors still selected their words: the mark itself was
      ;; kept, only the rings' copies were let go of.
      (should (seq-every-p #'donkey--split-cursor-selecting-p
                           donkey--split-places)))))

(ert-deftest donkey-split-i-asks-every-place-only-where-some-text-is-read-only ()
  "A buffer with no read-only text opens Insert state without asking each place."
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-i-fast*" "a foo b\nc foo d\n" "v G f"
      (let ((asked 0))
        (cl-letf (((symbol-function 'donkey--split-writable-p)
                   (lambda (&rest _) (setq asked (1+ asked)) t)))
          (execute-kbd-macro (kbd "i")))
        (should (= asked 0)))))
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-i-asks*" "a foo b\nc foo d\n" "v G f"
      (let ((inhibit-read-only t))
        (put-text-property 1 2 'read-only t))
      (let ((asked 0))
        (cl-letf (((symbol-function 'donkey--split-writable-p)
                   (lambda (&rest _) (setq asked (1+ asked)) t)))
          (execute-kbd-macro (kbd "i")))
        (should (= asked 2)))
      (let ((inhibit-read-only t))
        (remove-text-properties (point-min) (point-max) '(read-only nil))))))

(ert-deftest donkey-split-cursors-reminder-says-when-one-is-selecting ()
  "The reminder says selecting while a cursor holds a selection, and not after."
  (donkey-split-test--keys "*cursors-reminder*" donkey-split-test--column "t t m w"
    (should (string-match-p "selecting" (donkey--split-hint)))
    (execute-kbd-macro (kbd "C-g"))
    (should (= (length donkey--split-places) 3))
    (should-not (string-match-p "selecting" (donkey--split-hint)))))

(defvar donkey-split-test--changes nil
  "Every change seen by `donkey-split-test--note-change', newest last.")

(defun donkey-split-test--note-change (beg end _length)
  "Keep the bounds of a change, BEG to END, in `donkey-split-test--changes'."
  (setq donkey-split-test--changes
        (append donkey-split-test--changes (list (cons beg end)))))

(ert-deftest donkey-split-cursors-open-their-lines-top-down ()
  "The o and O keys open the cursors' lines from the first cursor to the last.

The mode's indentation parses on from the line before that way; from
the bottom up every line is parsed from far back."
  (dolist (key '("o" "O"))
    (let ((donkey-split-test--changes nil))
      (donkey-test-keys--harness "*cursors-open-order*" #'emacs-lisp-mode
          ((after-change-functions (list #'donkey-split-test--note-change)))
          "(a)\n(b)\n(c)\n(d)\n" (concat "t t t " key)
        (let ((starts (mapcar #'car donkey-split-test--changes)))
          (should (>= (length starts) 4))
          (should (equal (list key starts)
                         (list key (sort (copy-sequence starts) #'<)))))))))

(ert-deftest donkey-split-cursors-open-above-is-one-entry ()
  "O at the cursors is one entry, and undo closes every line it opened."
  (donkey-split-test--keys "*cursors-O-entry*" donkey-split-test--column
      "t t O Z C-g"
    (should (equal (buffer-string)
                   "Z\nalpha beta\nZ\ngamma delta\nZ\nepsilon zeta\nlast\n"))
    (should (= 2 (donkey-split-test--split-entries)))
    (execute-kbd-macro (kbd "u u"))
    (should (equal (buffer-string) donkey-split-test--column))))

(ert-deftest donkey-split-cursors-change-and-indent-are-one-entry-each ()
  "The c on characters and the > at the cursors each record one entry."
  (donkey-test-keys--harness "*cursors-c-entry*" #'emacs-lisp-mode ()
      "(a\nb\nc)\n" "j t >"
    (should (equal (buffer-string) "(a\n b\n c)\n"))
    (should (= 1 (donkey-split-test--split-entries)))
    (execute-kbd-macro (kbd "u"))
    (should (equal (buffer-string) "(a\nb\nc)\n")))
  (donkey-split-test--keys "*cursors-c-chars*" donkey-split-test--column
      "t t c X C-g"
    (should (equal (buffer-string) "Xlpha beta\nXamma delta\nXpsilon zeta\nlast\n"))
    (should (= 2 (donkey-split-test--split-entries)))
    (execute-kbd-macro (kbd "u u"))
    (should (equal (buffer-string) donkey-split-test--column))))

(ert-deftest donkey-split-rectangle-change-records-its-emptying-as-one-entry ()
  "The rectangle change empties the block as one entry, the writing is another."
  (donkey-split-test--keys "*rect-c-entry*" donkey-split-test--column
      "m v j j l c Q C-g C-g"
    (should (equal (buffer-string) "Qpha beta\nQmma delta\nQsilon zeta\nlast\n"))
    (should (= 2 (donkey-split-test--split-entries)))
    (execute-kbd-macro (kbd "u u"))
    (should (equal (buffer-string) donkey-split-test--column))))

(ert-deftest donkey-split-undo-record-keeps-to-undo-limit-by-what-it-holds ()
  "Past `undo-limit', counted by positions and texts, the older entries go."
  (let ((undo-limit 600)
        (undo-strong-limit 1200))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-undo-limit*"
          (donkey-split-test--numbered-lines 50) "v G f ( ["
        (should (= 1 (donkey-split-test--split-entries))))))
  (donkey-split-test--on "foo"
    (donkey-split-test--keys "*split-undo-limit-off*"
        (donkey-split-test--numbered-lines 50) "v G f ( ["
      (should (= 2 (donkey-split-test--split-entries))))))

(ert-deftest donkey-split-a-record-past-undo-outer-limit-is-not-made ()
  "A change too large for `undo-outer-limit' is made, said, and not recorded."
  (let ((undo-outer-limit 100)
        (said nil))
    (donkey-split-test--on "foo"
      (donkey-split-test--keys "*split-outer-limit*"
          (donkey-split-test--numbered-lines 50) "v G f"
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when fmt (push (apply #'format fmt args) said))
                     nil)))
          (execute-kbd-macro (kbd "d")))
        (should (equal (donkey-split-test--last-line) " 50"))
        (should (= 0 (donkey-split-test--split-entries)))
        (should (seq-find (lambda (line)
                            (string-match-p "too large to record" line))
                          said))))))

(ert-deftest donkey-split-cursor-limit-defaults-to-ten-thousand ()
  "The default limit is 10,000 cursors, and a value that is not a count reads as it."
  (should (= (default-value 'donkey-split-cursor-limit) 10000))
  (let ((donkey-split-cursor-limit "many")
        (donkey-split-cursor-limit-ask nil))
    (should (donkey--split-cursors-allowed-p 10000))
    (should-not (donkey--split-cursors-allowed-p 10001))))

(provide 'donkey-split-test)

;;; donkey-split-test.el ends here

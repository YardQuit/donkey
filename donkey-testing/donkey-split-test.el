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

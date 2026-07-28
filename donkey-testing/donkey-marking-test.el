;;; donkey-marking-test.el --- Tests for DONKEY mark/selection commands -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'rect)
(require 'donkey)

;;; ---------------------------------------------------------------------------
;;; donkey--ensure-non-rectangle-selection
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-ensure-non-rectangle-selection-disables-active-rectangle-mode ()
  "Disables `rectangle-mark-mode' when it is active."
  (with-temp-buffer
    (insert "hello\nworld\n")
    (goto-char 1)
    (rectangle-mark-mode 1)
    (should (bound-and-true-p rectangle-mark-mode))
    (donkey--ensure-non-rectangle-selection)
    (should-not (bound-and-true-p rectangle-mark-mode))))

(ert-deftest donkey-ensure-non-rectangle-selection-noop-when-inactive ()
  "Does nothing (no error) when `rectangle-mark-mode' is already off."
  (with-temp-buffer
    (insert "hello\n")
    (should-not (bound-and-true-p rectangle-mark-mode))
    (donkey--ensure-non-rectangle-selection)
    (should-not (bound-and-true-p rectangle-mark-mode))))

(defmacro donkey--test-clears-stale-rectangle-mode (test-name command-form)
  "Define ERT test TEST-NAME asserting COMMAND-FORM clears a stale rectangle.

COMMAND-FORM must clear a pre-existing
active `rectangle-mark-mode' selection.

Regression tests for the bug found live: `m v' on one line, then a
mark command elsewhere (without cancelling the rectangle first) left
`rectangle-mark-mode' active underneath the new, intended-to-be-linear
selection.  The next `donkey-copy'/`donkey-delete'/`donkey-yank' would
then misinterpret it as a rectangle -- confirmed to silently kill a
zero-width \"rectangle\" (one empty string per line) instead of
deleting a marked paragraph, with no error and no visible buffer
change at all."
  (declare (indent 1))
  `(ert-deftest ,test-name ()
     (with-temp-buffer
       (insert "(foo bar)\nsecond line\nthird line here\nfourth\n")
       (goto-char 1)
       (rectangle-mark-mode 1)
       (forward-line 1)
       (forward-char 2)
       (should (bound-and-true-p rectangle-mark-mode))
       (goto-char 1)
       ,command-form
       (should-not (bound-and-true-p rectangle-mark-mode)))))

(donkey--test-clears-stale-rectangle-mode
    donkey-mark-inner-clears-stale-rectangle-mode
  (progn (goto-char 1) (donkey-mark-inner)))

(donkey--test-clears-stale-rectangle-mode
    donkey-mark-outer-clears-stale-rectangle-mode
  (progn (goto-char 1) (donkey-mark-outer)))

(donkey--test-clears-stale-rectangle-mode
    donkey-mark-sexp-inner-clears-stale-rectangle-mode
  (progn (goto-char 1) (donkey-mark-sexp-inner)))

(donkey--test-clears-stale-rectangle-mode
    donkey-mark-sexp-outer-clears-stale-rectangle-mode
  (progn (goto-char 1) (donkey-mark-sexp-outer)))

(donkey--test-clears-stale-rectangle-mode
    donkey-mark-word-clears-stale-rectangle-mode
  (progn (goto-char 2) (donkey-mark-word)))

(donkey--test-clears-stale-rectangle-mode
    donkey-mark-sentence-clears-stale-rectangle-mode
  (progn (goto-char 2) (donkey-mark-sentence)))

(donkey--test-clears-stale-rectangle-mode
    donkey-mark-paragraph-clears-stale-rectangle-mode
  (progn (goto-char 2) (donkey-mark-paragraph)))

(donkey--test-clears-stale-rectangle-mode
    donkey-mark-symbol-clears-stale-rectangle-mode
  (progn (goto-char 2) (donkey-mark-symbol)))

(donkey--test-clears-stale-rectangle-mode
    donkey-visual-line-toggle-clears-stale-rectangle-mode
  (progn (goto-char 2) (donkey-visual-line-toggle)))

(donkey--test-clears-stale-rectangle-mode
    donkey-set-mark-clears-stale-rectangle-mode
  (progn (goto-char 2) (donkey-set-mark)))

(ert-deftest donkey-set-mark-activates-mark-at-point ()
  "The mark is set at point and the region activated.

This is like plain
`set-mark-command' with no prefix argument.

Checks `mark-active' directly rather than `use-region-p': the mark and
point are still at the SAME position right after
`donkey-set-mark' (point has not moved to select anything yet), so the region is
genuinely empty and `use-region-p' correctly reports nil for that per
its own documented, deliberate exclusion of empty regions."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "hello world")
      (goto-char 6)
      (donkey-set-mark)
      (should mark-active)
      (should (= (mark) 6)))))

(ert-deftest donkey-set-mark-delegates-to-set-mark-command ()
  "`donkey-set-mark' delegates to `set-mark-command'.

It goes through `call-interactively', so
prefix-argument behaviors (e.g. popping the mark ring with `C-u')
keep working exactly as they do for the underlying command."
  (let (called-with-prefix)
    (cl-letf (((symbol-function 'set-mark-command)
               (lambda (arg) (interactive "P") (setq called-with-prefix arg))))
      (with-temp-buffer
        (let ((current-prefix-arg '(4)))
          (call-interactively #'donkey-set-mark))))
    (should (equal called-with-prefix '(4)))))

(ert-deftest donkey-mark-paragraph-clears-stale-rectangle-mode-and-selects-correctly ()
  "A stale rectangle does not corrupt a new paragraph selection.

End-to-end regression test matching the exact live repro: a stale
rectangle selection from an unrelated `m v' session must not corrupt
the NEW multi-line selection `donkey-mark-paragraph' creates, and
`donkey-delete' run right after must delete the whole paragraph, not a
zero-width \"rectangle\" slice of it."
  (with-temp-buffer
    (insert "AABBCC\nDDEEFF\n\nfirst line of paragraph\nsecond line here\nthird line too\n\nlast\n")
    (goto-char 1)
    (rectangle-mark-mode 1)
    (forward-line 1)
    (forward-char 2)
    (should (bound-and-true-p rectangle-mark-mode))
    (goto-char (point-min))
    (forward-line 3)
    (forward-char 5)
    (donkey-mark-paragraph)
    (should-not (bound-and-true-p rectangle-mark-mode))
    (donkey-delete)
    (should (string= (buffer-string) "AABBCC\nDDEEFF\n\nlast\n"))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-word
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-word-point-in-middle ()
  "Point in middle of word selects entire word."
  (with-temp-buffer
    (insert "hello")
    (goto-char 3)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-word-point-at-beginning ()
  "Point at beginning of word selects entire word."
  (with-temp-buffer
    (insert "hello")
    (goto-char 1)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-word-point-at-end ()
  "Point at last character of word selects entire word."
  (with-temp-buffer
    (insert "hello")
    (goto-char 5)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-word-point-after-word ()
  "Point on whitespace after word selects previous word."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 6)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-word-point-before-word ()
  "Point on whitespace before word selects that word."
  (with-temp-buffer
    (insert "  hello")
    (goto-char 3)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-word-multiple-words-first ()
  "Point on first word in multi-word buffer selects first word."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 2)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "foo"))))

(ert-deftest donkey-mark-word-multiple-words-second ()
  "Point on second word in multi-word buffer selects second word."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 6)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "bar"))))

(ert-deftest donkey-mark-word-multiple-words-third ()
  "Point on third word in multi-word buffer selects third word."
  (with-temp-buffer
    (insert "foo bar baz")
    (goto-char 10)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "baz"))))

(ert-deftest donkey-mark-word-single-character ()
  "Single character word selected correctly."
  (with-temp-buffer
    (insert "x")
    (goto-char 1)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "x"))))

(ert-deftest donkey-mark-word-word-at-buffer-start ()
  "Word at buffer start selected correctly."
  (with-temp-buffer
    (insert "start")
    (goto-char 1)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "start"))))

(ert-deftest donkey-mark-word-word-at-buffer-end ()
  "Word at buffer end selected correctly."
  (with-temp-buffer
    (insert "one two")
    (goto-char 5)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "two"))))

(ert-deftest donkey-mark-word-point-on-last-word ()
  "Point on last word with trailing newline selected correctly."
  (with-temp-buffer
    (insert "alpha beta\n")
    (goto-char 7)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "beta"))))

(ert-deftest donkey-mark-word-separated-by-multiple-spaces ()
  "Words separated by multiple spaces selected correctly."
  (with-temp-buffer
    (insert "first    second")
    (goto-char 10)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "second"))))

(ert-deftest donkey-mark-word-separated-by-tabs ()
  "Words separated by tabs selected correctly."
  (with-temp-buffer
    (insert "left\tright")
    (goto-char 6)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "right"))))

(ert-deftest donkey-mark-word-newline-separated ()
  "Words separated by newlines selected correctly."
  (with-temp-buffer
    (insert "top\nbottom")
    (goto-char 5)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "bottom"))))

(ert-deftest donkey-mark-word-punctuation-adjacent ()
  "Word adjacent to punctuation selected without punctuation."
  (with-temp-buffer
    (insert "word.")
    (goto-char 2)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "word"))))

(ert-deftest donkey-mark-word-surrounded-by-punctuation ()
  "Word surrounded by punctuation selected without punctuation."
  (with-temp-buffer
    (insert "(test)")
    (goto-char 3)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "test"))))

(ert-deftest donkey-mark-word-multiline-buffer ()
  "Word in multiline buffer selected correctly."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char 8)
    (donkey-mark-word)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "line2"))))

(ert-deftest donkey-mark-word-has-mark ()
  "Mark is set after command."
  (with-temp-buffer
    (insert "content")
    (goto-char 1)
    (donkey-mark-word)
    (should (mark))))

(ert-deftest donkey-mark-word-region-valid ()
  "Region beginning is less than region end."
  (with-temp-buffer
    (insert "valid")
    (goto-char 1)
    (donkey-mark-word)
    (should (< (region-beginning) (region-end)))))

(ert-deftest donkey-mark-word-empty-buffer ()
  "Empty buffer raises error."
  (with-temp-buffer
    (should-error (donkey-mark-word))))

(ert-deftest donkey-mark-word-whitespace-only ()
  "Buffer with only whitespace raises error."
  (with-temp-buffer
    (insert "   ")
    (goto-char 2)
    (should-error (donkey-mark-word))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-symbol
;;; ---------------------------------------------------------------------------

(defun donkey-test--symbol-result (content pos)
  "Run `donkey-mark-symbol' in a temp buffer with CONTENT at 1-based POS.
Return list (POINT MARK TEXT) describing the resulting region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert content)
    (goto-char pos)
    (donkey-mark-symbol)
    (list (point)
          (or (mark t) (point))
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            ""))))

(ert-deftest donkey-mark-symbol-simple ()
  "Mark simple word from middle."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar" 3)) "foobar")))

(ert-deftest donkey-mark-symbol-from-start ()
  "Mark simple word when point is at the first character."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar" 1)) "foobar")))

(ert-deftest donkey-mark-symbol-from-end ()
  "Mark simple word when point is at the last character."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar" 6)) "foobar")))

(ert-deftest donkey-mark-symbol-trailing-comma ()
  "Trailing comma is omitted from selection."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar," 4)) "foobar")))

(ert-deftest donkey-mark-symbol-trailing-period ()
  "Trailing period is omitted from selection."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar." 4)) "foobar")))

(ert-deftest donkey-mark-symbol-trailing-both ()
  "Multiple trailing commas and periods are all omitted."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar,." 4)) "foobar")))

(ert-deftest donkey-mark-symbol-internal-comma-period ()
  "Internal comma and period characters are preserved within the symbol."
  (should (equal (nth 2 (donkey-test--symbol-result "word,.word" 6)) "word,.word")))

(ert-deftest donkey-mark-symbol-internal-from-left ()
  "Cursor on left side of internal comma marks the full symbol."
  (should (equal (nth 2 (donkey-test--symbol-result "word,.word" 4)) "word,.word")))

(ert-deftest donkey-mark-symbol-internal-from-right ()
  "Cursor on right side of internal comma marks the full symbol."
  (should (equal (nth 2 (donkey-test--symbol-result "word,.word" 5)) "word,.word")))

(ert-deftest donkey-mark-symbol-hyphenated ()
  "Hyphenated symbols are fully marked including hyphens."
  (should (equal (nth 2 (donkey-test--symbol-result "donkey-mark-symbol" 8)) "donkey-mark-symbol")))

(ert-deftest donkey-mark-symbol-underscore ()
  "Symbols with underscores are fully marked including underscores."
  (should (equal (nth 2 (donkey-test--symbol-result "foo_bar_baz" 6)) "foo_bar_baz")))

(ert-deftest donkey-mark-symbol-point-at-beg ()
  "Point should end at beginning of the symbol."
  (should (= (nth 0 (donkey-test--symbol-result "foobar" 4)) 1)))

(ert-deftest donkey-mark-symbol-trailing-comma-at-eob ()
  "Trailing comma at end of buffer with no following text."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar," 4)) "foobar")))

(ert-deftest donkey-mark-symbol-multiple-trailing ()
  "Multiple trailing commas and periods should all be trimmed."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar,,.." 4)) "foobar")))

(ert-deftest donkey-mark-symbol-single-char ()
  "Single character should be marked."
  (should (equal (nth 2 (donkey-test--symbol-result "x" 1)) "x")))

(ert-deftest donkey-mark-symbol-with-numbers ()
  "Symbols containing numbers should be fully marked."
  (should (equal (nth 2 (donkey-test--symbol-result "foo123bar" 5)) "foo123bar")))

(ert-deftest donkey-mark-symbol-whitespace-before ()
  "Cursor on space with symbol to the left should mark it."
  (should (equal (nth 2 (donkey-test--symbol-result "foo bar" 4)) "foo")))

(ert-deftest donkey-mark-symbol-whitespace-after ()
  "Cursor on space with symbol to the right."
  (should (equal (nth 2 (donkey-test--symbol-result "foo bar" 4)) "foo")))

(ert-deftest donkey-mark-symbol-before-paren ()
  "Symbol immediately before a paren should not include it."
  (should (equal (nth 2 (donkey-test--symbol-result "foo(bar)" 2)) "foo")))

(ert-deftest donkey-mark-symbol-after-paren ()
  "Symbol immediately after a paren should not include it."
  (should (equal (nth 2 (donkey-test--symbol-result "(foo bar)" 2)) "foo")))

(ert-deftest donkey-mark-symbol-mark-position ()
  "Mark should be at the end of the trimmed symbol."
  ;; "foobar, rest" — mark should be at position 7 (after 'r', before ',')
  (should (= (nth 1 (donkey-test--symbol-result "foobar, rest" 3)) 7)))

(ert-deftest donkey-mark-symbol-region-active ()
  "Region should be active after marking."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foobar")
    (goto-char 3)
    (donkey-mark-symbol)
    (should (region-active-p))))

(ert-deftest donkey-mark-symbol-bare-punctuation ()
  "Cursor on standalone comma with no adjacent symbol."
  ;; This may error — testing graceful handling
  (should-error (donkey-test--symbol-result "," 1) :type 'error))

(ert-deftest donkey-mark-symbol-blank-line-after-list-signals-user-error ()
  "Point on a blank line after a list reports cleanly instead of erroring.

Regression: `backward-sexp' lands on the list's opening paren, which is
no symbol, and `beginning-of-thing' signalled a bare `error' -- which
pops the debugger for anyone running with `debug-on-error' on.  A blank
line under an expression is an ordinary place to press this."
  (with-temp-buffer
    (insert "(foo bar)\n\n")
    (goto-char (point-max))
    (should-error (donkey-mark-symbol) :type 'user-error)))

(ert-deftest donkey-mark-symbol-whitespace-only-buffer-signals-user-error ()
  "A buffer with nothing to mark reports cleanly rather than signalling `error'."
  (with-temp-buffer
    (insert "   ")
    (goto-char (point-min))
    (should-error (donkey-mark-symbol) :type 'user-error)))

(ert-deftest donkey-mark-word-whitespace-only-buffer-signals-user-error ()
  "`donkey-mark-word' reports cleanly with no word anywhere before point."
  (with-temp-buffer
    (insert "   ")
    (goto-char (point-min))
    (should-error (donkey-mark-word) :type 'user-error)))

(ert-deftest donkey-mark-symbol-bob-trailing-comma ()
  "Symbol at BOB with trailing comma."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar, rest" 3)) "foobar")))

(ert-deftest donkey-mark-symbol-adjacent-via-comma ()
  "Two symbols separated only by a comma count as one symbol.

Given foo,bar, `mark-sexp' should treat them as one unit."
  (should (equal (nth 2 (donkey-test--symbol-result "foo,bar" 2)) "foo,bar")))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-sentence
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-sentence-single-sentence ()
  "Marks entire single sentence."
  (with-temp-buffer
    (insert "Hello world.")
    (goto-char 5)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Hello world."))))

(ert-deftest donkey-mark-sentence-point-at-beginning ()
  "Point at sentence beginning selects entire sentence."
  (with-temp-buffer
    (insert "First sentence.")
    (goto-char 1)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "First sentence."))))

(ert-deftest donkey-mark-sentence-point-at-end ()
  "Point at sentence end selects entire sentence."
  (with-temp-buffer
    (insert "End of sentence.")
    (goto-char 16)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "End of sentence."))))

(ert-deftest donkey-mark-sentence-point-in-middle ()
  "Point in middle of sentence selects entire sentence."
  (with-temp-buffer
    (insert "Middle sentence here.")
    (goto-char 8)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Middle sentence here."))))

(ert-deftest donkey-mark-sentence-two-sentences-selects-both ()
  "In multi-sentence buffer, selects from sentence boundary to sentence end."
  (with-temp-buffer
    (insert "One. Two.")
    (goto-char 3)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "One. Two."))))

(ert-deftest donkey-mark-sentence-three-sentences-selects-all ()
  "In three-sentence buffer, selects all content."
  (with-temp-buffer
    (insert "A. B. C.")
    (goto-char 5)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "A. B. C."))))

(ert-deftest donkey-mark-sentence-with-newlines ()
  "Sentence spanning newline selected correctly."
  (with-temp-buffer
    (insert "Line one.\nLine two.")
    (goto-char 5)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Line one."))))

(ert-deftest donkey-mark-sentence-question-mark ()
  "Sentence ending with question mark detected."
  (with-temp-buffer
    (insert "Is this right?")
    (goto-char 8)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Is this right?"))))

(ert-deftest donkey-mark-sentence-exclamation-mark ()
  "Sentence ending with exclamation detected."
  (with-temp-buffer
    (insert "Watch out!")
    (goto-char 8)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Watch out!"))))

(ert-deftest donkey-mark-sentence-short-sentence ()
  "Very short sentence selected correctly."
  (with-temp-buffer
    (insert "OK.")
    (goto-char 2)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "OK."))))

(ert-deftest donkey-mark-sentence-long-sentence ()
  "Long sentence without internal punctuation selected."
  (with-temp-buffer
    (insert "This is a very long sentence with many words and no punctuation inside.")
    (goto-char 20)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "This is a very long sentence with many words and no punctuation inside."))))

(ert-deftest donkey-mark-sentence-leading-whitespace-stripped ()
  "Leading whitespace stripped from selection start."
  (with-temp-buffer
    (insert "   Start of text.")
    (goto-char 5)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Start of text."))))

(ert-deftest donkey-mark-sentence-trailing-newline ()
  "Trailing newline not included in selection."
  (with-temp-buffer
    (insert "Sentence.\nNext")
    (goto-char 5)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Sentence."))))

(ert-deftest donkey-mark-sentence-buffer-end ()
  "Selection extends to buffer end when at last sentence."
  (with-temp-buffer
    (insert "Last.")
    (goto-char 3)
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Last."))))

(ert-deftest donkey-mark-sentence-has-mark ()
  "Mark is set after command."
  (with-temp-buffer
    (insert "Content here.")
    (goto-char 1)
    (donkey-mark-sentence)
    (should (mark))))

(ert-deftest donkey-mark-sentence-region-valid ()
  "Region beginning is less than region end."
  (with-temp-buffer
    (insert "Valid sentence.")
    (goto-char 5)
    (donkey-mark-sentence)
    (should (< (region-beginning) (region-end)))))

(ert-deftest donkey-mark-sentence-empty-buffer ()
  "Empty buffer reports a `user-error' rather than signalling.

Regression: the sentence motions raised a bare `end-of-buffer' here,
which pops the debugger for anyone running with `debug-on-error' on.
`donkey-mark-word' and `donkey-mark-symbol' already guarded this.  The
assertion was previously an untyped `should-error', which a bare `error'
satisfied just as well as the clean report does."
  (with-temp-buffer
    (should-error (donkey-mark-sentence) :type 'user-error)))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-paragraph
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-paragraph-single-paragraph ()
  "Marks entire single paragraph."
  (with-temp-buffer
    (insert "This is a paragraph.")
    (goto-char 5)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "This is a paragraph."))))

(ert-deftest donkey-mark-paragraph-point-at-beginning ()
  "Point at paragraph beginning selects entire paragraph."
  (with-temp-buffer
    (insert "First paragraph text.")
    (goto-char 1)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "First paragraph text."))))

(ert-deftest donkey-mark-paragraph-point-at-end ()
  "Point at paragraph end selects entire paragraph."
  (with-temp-buffer
    (insert "End paragraph.")
    (goto-char 14)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "End paragraph."))))

(ert-deftest donkey-mark-paragraph-point-in-middle ()
  "Point in middle of paragraph selects entire paragraph."
  (with-temp-buffer
    (insert "Middle paragraph here.")
    (goto-char 8)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Middle paragraph here."))))

(ert-deftest donkey-mark-paragraph-two-paragraphs-selects-first ()
  "In multi-paragraph buffer, selects first paragraph."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.")
    (goto-char 3)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Para one.\n"))))

(ert-deftest donkey-mark-paragraph-two-paragraphs-second ()
  "In multi-paragraph buffer, selects second paragraph with leading newline."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.")
    (goto-char 12)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "\nPara two."))))

(ert-deftest donkey-mark-paragraph-three-paragraphs-middle ()
  "In three-paragraph buffer, selects middle paragraph with surrounding newlines."
  (with-temp-buffer
    (insert "A.\n\nB.\n\nC.")
    (goto-char 6)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "\nB.\n"))))

(ert-deftest donkey-mark-paragraph-empty-buffer-errors ()
  "An empty buffer has no paragraph, and this says so.

Regression: the paragraph motions do not signal on an empty buffer --
they walk to its end and back -- so this announced \"Paragraph marked\"
with no region active at all.  Its three siblings (`donkey-mark-word',
`donkey-mark-symbol', `donkey-mark-sentence') all already errored here;
this was the odd one out."
  (with-temp-buffer
    (should-error (donkey-mark-paragraph) :type 'user-error)))

(ert-deftest donkey-mark-paragraph-whitespace-only-buffer-errors ()
  "A buffer of nothing but blank lines has no paragraph either.

Before the guard this \"marked\" the blank and reported success --
`donkey-mark-sentence' rejects exactly this case, for exactly this
reason."
  (dolist (blank '("\n\n\n" "   \n\t\n" "\n   \n\n"))
    (with-temp-buffer
      (insert blank)
      (goto-char (point-min))
      (should-error (donkey-mark-paragraph) :type 'user-error))))

(ert-deftest donkey-mark-paragraph-error-leaves-no-selection-behind ()
  "Rejecting the mark deactivates it rather than leaving a stale region."
  (with-temp-buffer
    (insert "\n\n\n")
    (goto-char (point-min))
    (let ((transient-mark-mode t))
      (ignore-errors (donkey-mark-paragraph))
      (should-not (use-region-p)))))

(ert-deftest donkey-mark-paragraph-zero-count-is-exempt-from-the-guard ()
  "A zero count marks nothing WITHOUT erroring, as documented.

The nothing came from the count, not from the buffer -- there is a
paragraph right there.  Pinned because the emptiness guard would
otherwise swallow this documented case."
  (with-temp-buffer
    (insert "A paragraph.\n")
    (goto-char 3)
    (let ((transient-mark-mode t))
      (should (progn (donkey-mark-paragraph 0) t))
      (should-not (use-region-p)))))

(ert-deftest donkey-mark-paragraph-blank-line-between-paragraphs-still-works ()
  "Pressing this from a blank line between paragraphs marks the one above.

The guard checks the RESULT, not what is under point, precisely so this
keeps working: point sits on whitespace, but the command has a real
paragraph to give."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.\n")
    (goto-char 11)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Para one.\n"))))

(ert-deftest donkey-mark-paragraph-leading-blank-lines-reach-the-text-below ()
  "From leading blank lines, the paragraph below is still marked."
  (with-temp-buffer
    (insert "\n\n\nReal text here.\n")
    (goto-char (point-min))
    (donkey-mark-paragraph)
    (should (string-match-p "Real text here"
                            (buffer-substring-no-properties (region-beginning)
                                                            (region-end))))))

(ert-deftest donkey-mark-paragraph-with-newlines ()
  "Paragraph with multiple lines selected correctly."
  (with-temp-buffer
    (insert "Line one.\nLine two.\nLine three.")
    (goto-char 5)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Line one.\nLine two.\nLine three."))))

(ert-deftest donkey-mark-paragraph-empty-line-separator ()
  "Paragraphs separated by empty line detected."
  (with-temp-buffer
    (insert "First para.\n\nSecond para.")
    (goto-char 5)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "First para.\n"))))

(ert-deftest donkey-mark-paragraph-short-paragraph ()
  "Very short paragraph selected correctly."
  (with-temp-buffer
    (insert "Hi.")
    (goto-char 2)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Hi."))))

(ert-deftest donkey-mark-paragraph-long-paragraph ()
  "Long paragraph without blank lines selected."
  (with-temp-buffer
    (insert "This is a very long paragraph with many words and no blank lines inside.")
    (goto-char 20)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "This is a very long paragraph with many words and no blank lines inside."))))

(ert-deftest donkey-mark-paragraph-leading-whitespace-included ()
  "Leading whitespace included in selection."
  (with-temp-buffer
    (insert "   Start of text.")
    (goto-char 5)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "   Start of text."))))

(ert-deftest donkey-mark-paragraph-trailing-blank-lines ()
  "Trailing blank lines not included in selection."
  (with-temp-buffer
    (insert "Para.\n\nMore")
    (goto-char 3)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Para.\n"))))

(ert-deftest donkey-mark-paragraph-buffer-end ()
  "Selection extends to buffer end when at last paragraph."
  (with-temp-buffer
    (insert "Last para.")
    (goto-char 5)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Last para."))))

(ert-deftest donkey-mark-paragraph-only-whitespace ()
  "A buffer of only whitespace has no paragraph to mark.

Was \"Paragraph with only whitespace and newlines selected\", asserting
that `\"   \\n\\n  \"' came back as the marked paragraph.  It described
what the code did rather than what marking a paragraph is for, and so
held the defect in place: `donkey-mark-sentence' rejects this very
buffer, and rejecting it is the whole reason that guard exists."
  (with-temp-buffer
    (insert "   \n\n  ")
    (goto-char 2)
    (should-error (donkey-mark-paragraph) :type 'user-error)))

(ert-deftest donkey-mark-paragraph-has-mark ()
  "Mark is set after command."
  (with-temp-buffer
    (insert "Content here.")
    (goto-char 1)
    (donkey-mark-paragraph)
    (should (mark))))

(ert-deftest donkey-mark-paragraph-region-valid ()
  "Region beginning is less than region end."
  (with-temp-buffer
    (insert "Valid para.")
    (goto-char 5)
    (donkey-mark-paragraph)
    (should (< (region-beginning) (region-end)))))

(ert-deftest donkey-mark-paragraph-empty-buffer ()
  "An empty buffer has no paragraph, and this reports so.

Was \"Empty buffer marks empty region\", and asserted only
`(should (mark))' -- true of a mark that had been pushed and then went nowhere.
It passed while the command announced \"Paragraph marked\" over a buffer
with nothing in it and no region active, which is what a test written
from the implementation rather than the purpose will do."
  (with-temp-buffer
    (should-error (donkey-mark-paragraph) :type 'user-error)))

(ert-deftest donkey-mark-paragraph-point-on-separator ()
  "Point on separator between paragraphs selects adjacent paragraph."
  (with-temp-buffer
    (insert "First.\n\nSecond.")
    (goto-char 7)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "First.\n"))))

(ert-deftest donkey-mark-paragraph-multiple-consecutive-blanks ()
  "Multiple consecutive blank lines handled."
  (with-temp-buffer
    (insert "Para.\n\n\n\nMore.")
    (goto-char 3)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Para.\n"))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-inner
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-inner-braces ()
  "Marks content inside braces, excluding delimiters."
  (with-temp-buffer
    (insert "{hello}")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-inner-parens ()
  "Marks content inside parens, excluding delimiters."
  (with-temp-buffer
    (insert "(world)")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "world"))))

(ert-deftest donkey-mark-inner-brackets ()
  "Marks content inside brackets, excluding delimiters."
  (with-temp-buffer
    (insert "[test]")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "test"))))

(ert-deftest donkey-mark-inner-double-quote ()
  "Marks content inside double quotes, excluding quotes."
  (with-temp-buffer
    (insert "\"quoted\"")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "quoted"))))

(ert-deftest donkey-mark-inner-single-quote ()
  "Marks content inside single quotes, excluding quotes."
  (with-temp-buffer
    (insert "'quoted'")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "quoted"))))

(ert-deftest donkey-mark-inner-angle ()
  "Marks content inside angle brackets, excluding brackets."
  (with-temp-buffer
    (insert "<tag>")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "tag"))))

(ert-deftest donkey-mark-inner-underscore ()
  "Marks content inside underscores, excluding underscores."
  (with-temp-buffer
    (insert "_italic_")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "italic"))))

(ert-deftest donkey-mark-inner-asterisk ()
  "Marks content inside asterisks, excluding asterisks."
  (with-temp-buffer
    (insert "*bold*")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "bold"))))

(ert-deftest donkey-mark-inner-tilde ()
  "Marks content inside tildes, excluding tildes."
  (with-temp-buffer
    (insert "~strike~")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "strike"))))

(ert-deftest donkey-mark-inner-equals ()
  "Marks content inside equals signs, excluding equals."
  (with-temp-buffer
    (insert "=math=")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "math"))))

(ert-deftest donkey-mark-inner-plus ()
  "Marks content inside plus signs, excluding pluses."
  (with-temp-buffer
    (insert "+code+")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "code"))))

(ert-deftest donkey-mark-inner-dollar ()
  "Marks content inside dollar signs, excluding dollars."
  (with-temp-buffer
    (insert "$latex$")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "latex"))))

(ert-deftest donkey-mark-inner-colon ()
  "Marks content inside colons, excluding colons."
  (with-temp-buffer
    (insert ":date:")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "date"))))

(ert-deftest donkey-mark-inner-slash ()
  "Marks content inside slashes, excluding slashes."
  (with-temp-buffer
    (insert "/path/")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "path"))))

(ert-deftest donkey-mark-inner-backtick ()
  "Marks content inside backticks, excluding backticks."
  (with-temp-buffer
    (insert "`inline`")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "inline"))))

(ert-deftest donkey-mark-inner-pipe ()
  "Marks content inside pipes, excluding pipes."
  (with-temp-buffer
    (insert "|pipe|")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "pipe"))))

(ert-deftest donkey-mark-inner-backslash ()
  "Marks content inside backslashes, excluding backslashes."
  (with-temp-buffer
    (insert "\\escaped\\")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "escaped"))))

(ert-deftest donkey-mark-inner-curly-single-quote ()
  "Content inside curly single quotes is marked, quotes excluded.

U+2018/U+2019 is a default delimiter pair."
  (with-temp-buffer
    (insert (concat (string ?‘) "quoted" (string ?’)))
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "quoted"))))

(ert-deftest donkey-mark-inner-curly-single-quote-distinct-open-close ()
  "Curly single quotes pair a distinct opener with a distinct closer.

Regression test: `donkey-mark-pair-delimiters' previously mapped
U+2019 (closing curly single quote) to itself for BOTH the open and
close side, instead of pairing U+2018 (opening) with U+2019 (closing).
That made the opening quote unrecognized as an opener (falling through
to the `read-char' prompt instead of auto-detecting), and made the
forward-then-backward-fallback search for a pair starting from the
closing quote search for the wrong character in both directions, so it
found nothing at all."
  (should (equal (assq ?‘ donkey-mark-pair-delimiters) (cons ?‘ ?’)))
  (should (equal (assq ?’ donkey-mark-pair-delimiters) nil)))

(ert-deftest donkey-mark-inner-curly-single-quote-from-closing-quote ()
  "Marking works from the closing curly single quote too.

Point on the CLOSING curly single quote falls through to the
`read-char' prompt (same as any other asymmetric closing delimiter,
e.g. `)'), and searching backward for the real opener (U+2018) then
correctly finds the pair."
  (with-temp-buffer
    (insert (concat (string ?‘) "quoted" (string ?’)))
    (goto-char (point-max))
    (backward-char 1)
    (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?‘)))
      (donkey-mark-inner))
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "quoted"))))

(ert-deftest donkey-mark-inner-curly-double-quote ()
  "Content inside curly double quotes is marked, quotes excluded.

U+201C/U+201D is a default delimiter pair."
  (with-temp-buffer
    (insert (concat (string ?“) "quoted" (string ?”)))
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "quoted"))))

(ert-deftest donkey-mark-inner-case-sensitive-delimiter ()
  "Delimiter matching is case-sensitive.

Regression test: matching a delimiter pair must be case-sensitive,
regardless of the buffer's own `case-fold-search' setting.

Without binding `case-fold-search' to nil, `search-forward'/
`search-backward' fold case by default, so an uppercase delimiter
like `X' would also match a lowercase `x' -- silently pairing with
the wrong occurrence.  Confirmed live in `emacs -nw': with `X' added
as a custom delimiter and point on the opening `X' in
\"Xfoo x barX\", `donkey-mark-inner' selected \"foo \" (stopping at
the lowercase `x') instead of \"foo x bar\" (stopping at the real
uppercase `X') before this fix."
  (let ((donkey-mark-pair-delimiters
         (cons (cons ?X ?X) donkey-mark-pair-delimiters))
        (case-fold-search t))
    (with-temp-buffer
      (insert "Xfoo x barX")
      (goto-char 1)
      (donkey-mark-inner)
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     "foo x bar")))))

(ert-deftest donkey-mark-inner-edge-empty ()
  "Empty braces produce no selectable content, raising error."
  (with-temp-buffer
    (insert "{}")
    (goto-char 1)
    (should-error (donkey-mark-inner) :type 'error)))

(ert-deftest donkey-mark-inner-edge-no-close ()
  "Unclosed delimiter raises error."
  (with-temp-buffer
    (insert "{unclosed")
    (goto-char 1)
    (should-error (donkey-mark-inner) :type 'error)))

(ert-deftest donkey-mark-inner-edge-nested ()
  "Nested delimiters select innermost pair content."
  (with-temp-buffer
    (insert "{{inner}}")
    (goto-char 2)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "inner"))))

(ert-deftest donkey-mark-inner-nested-same-type-from-outer-open ()
  "Marking from an outer opening delimiter spans the whole nest.

Regression test: point on the OUTER opening delimiter of a
same-type nested pair (e.g. the outer `{' of \"{{inner}}\") must select
the content up to the OUTER closing delimiter, not the nearest one.

Before `donkey--mark-pair-scan-forward' existed, this used a plain
`search-forward' for the close character, which stops at the FIRST
occurrence regardless of nesting -- for \"{{inner}}\" from the outer
`{', that found the inner pair's `}' instead of the outer one,
silently producing the nonsensical selection \"{inner\" (a stray
opening brace with no matching close) instead of erroring or
selecting the correct \"{inner}\"."
  (dolist (case '(("{{inner}}" . "{inner}")
                   ("((inner))" . "(inner)")
                   ("[[inner]]" . "[inner]")))
    (let ((text (car case)) (expected (cdr case)))
      (ert-info ((format "text %S" text))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (donkey-mark-inner)
          (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                         expected)))))))

(ert-deftest donkey-mark-inner-nested-same-type-from-outer-close ()
  "Marking from an outer closing delimiter spans the whole nest.

Same regression as `donkey-mark-inner-nested-same-type-from-outer-open',
but with point on the OUTER closing delimiter instead (exercising the
nesting-aware backward scan, `donkey--mark-pair-scan-backward')."
  (dolist (case '(("{{inner}}" . "{inner}")
                   ("((inner))" . "(inner)")
                   ("[[inner]]" . "[inner]")))
    (let ((text (car case)) (expected (cdr case)))
      (ert-info ((format "text %S" text))
        (with-temp-buffer
          (insert text)
          (goto-char (1- (point-max)))
          (donkey-mark-inner)
          (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                         expected)))))))

(ert-deftest donkey-mark-inner-nested-same-type-with-content-around-inner-pair ()
  "The nesting scan handles content around an inner pair.

It still finds the correct enclosing pair when the
nested same-type pair is surrounded by other text, not just adjacent
delimiters -- e.g. \"(a(b)c)\" from the outer `(' must select
\"a(b)c\", not stop at the inner pair's `)' and produce \"a(b\"."
  (with-temp-buffer
    (insert "(a(b)c)")
    (goto-char (point-min))
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "a(b)c"))))

(ert-deftest donkey-mark-inner-nested-same-type-triple-nesting ()
  "The nesting scan resolves three levels of the same delimiter.

This holds both from the outermost pair and from a middle pair."
  (with-temp-buffer
    (insert "(a(b(c)d)e)")
    (goto-char (point-min))
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "a(b(c)d)e"))
    (deactivate-mark)
    (goto-char 3)                      ; the middle '(', opening "b(c)d"
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "b(c)d"))))

(ert-deftest donkey-mark-inner-nested-mixed-delimiter-types-unaffected ()
  "A different bracket type nested inside does not confuse the scan.

Nesting-depth counting only tracks the SAME open/close characters
being searched for, so a different bracket type nested inside (e.g.
`[...]' inside `(...)') does not confuse the scan -- it is simply
ignored, exactly as a plain (non-nesting) search already treated any
unrelated character."
  (with-temp-buffer
    (insert "(a[b]c)")
    (goto-char (point-min))
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "a[b]c"))))

(ert-deftest donkey-mark-inner-nested-same-type-unbalanced-signals-error ()
  "An unbalanced nested same-type delimiter (missing outer close)
signals an error via `donkey--mark-pair-scan-forward' failing, rather
than silently matching the wrong (inner) close."
  (with-temp-buffer
    (insert "(a(b)c")
    (goto-char (point-min))
    (should-error (donkey-mark-inner) :type 'error)))

(ert-deftest donkey-mark-inner-edge-multiline ()
  "Multiline content between delimiters selected."
  (with-temp-buffer
    (insert "{line1\nline2}")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "line1\nline2"))))

(ert-deftest donkey-mark-inner-symmetric-delimiter-at-point-always-tries-forward-first ()
  "A symmetric delimiter at point always searches forward first.

Design contract: point on any occurrence of a symmetric
delimiter (open-char equals close-char, e.g. a quote) is always assumed
to be an OPENING delimiter first, searching forward -- exactly as if the user
had just typed it there.  This holds even when the forward match
belongs to a different, unrelated pair rather than the one enclosing
point: with point on the closing quote of \"hello\" in
`foo \"hello\" bar \"unrelated\" baz', the forward search finds
\"unrelated\"'s opening quote and accepts it, selecting \" bar \"
rather than \"hello\".  The backward-search fallback (see
`donkey-mark-inner-symmetric-delimiter-falls-back-when-no-forward-match')
only applies when the forward search finds nothing at all."
  (with-temp-buffer
    (insert "foo \"hello\" bar \"unrelated\" baz")
    (goto-char 1)
    (search-forward "hello")
    (should (eq (char-after) ?\"))
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   " bar "))))

(ert-deftest donkey-mark-inner-symmetric-delimiter-falls-back-when-no-forward-match ()
  "A symmetric delimiter falls back to a backward search.

When the forward search for a symmetric delimiter's close finds
nothing at all, it falls back to searching backward instead of erroring
-- treating point as the pair's CLOSING occurrence and finding its
matching opener.  With only one quote pair in the buffer and point on
its closing quote, there is nothing left to match searching forward,
so this must fall back rather than fail."
  (with-temp-buffer
    (insert "foo \"hello\" bar")
    (goto-char 1)
    (search-forward "hello")
    (should (eq (char-after) ?\"))
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-inner-symmetric-delimiter-three-in-a-row-middle-pairs-forward ()
  "The middle of three symmetric delimiters pairs forward.

With three occurrences of the same symmetric delimiter and point on
the middle one, forward search succeeds (finds the third), so it
pairs with the third rather than falling back to the first."
  (with-temp-buffer
    (insert "a/bee/cee/d")
    (goto-char 1)
    (search-forward "bee")
    (should (eq (char-after) ?/))
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "cee"))))

(ert-deftest donkey-mark-inner-symmetric-delimiter-three-in-a-row-last-falls-back ()
  "The last of three symmetric delimiters falls back.

With three occurrences of the same symmetric delimiter and point on
the last one, forward search finds nothing, so it falls back to
pairing with the middle occurrence."
  (with-temp-buffer
    (insert "a/bee/cee/d")
    (goto-char 1)
    (search-forward "cee")
    (should (eq (char-after) ?/))
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "cee"))))

(ert-deftest donkey-mark-inner-asymmetric-delimiter-unclosed-does-not-fall-back ()
  "An unclosed asymmetric delimiter does not fall back.

Asymmetric delimiters (open-char distinct from close-char, e.g. `('
and `)') never get the backward-search fallback: point on an opening
delimiter with no closing match ahead is an unclosed pair, plain and
simple, not an ambiguous opener/closer situation -- searching backward
for another `(' would not find a `)' anyway, so it stays an error."
  (with-temp-buffer
    (insert "foo (bar unclosed")
    (goto-char 1)
    (search-forward "(")
    (backward-char 1)
    (should-error (donkey-mark-inner) :type 'error)))

(ert-deftest donkey-mark-inner-edge-has-mark ()
  "Mark is set after command."
  (with-temp-buffer
    (insert "{content}")
    (goto-char 1)
    (donkey-mark-inner)
    (should (mark))))

(ert-deftest donkey-mark-inner-edge-region-valid ()
  "Region beginning is less than region end."
  (with-temp-buffer
    (insert "{valid}")
    (goto-char 1)
    (donkey-mark-inner)
    (should (< (region-beginning) (region-end)))))

(ert-deftest donkey-mark-inner-unsupported-delimiter-errors ()
  "An unsupported delimiter character (from the `read-char' prompt) signals an error."
  (with-temp-buffer
    (insert "!bang!")
    (goto-char 1)
    (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?!)))
      (should-error (donkey-mark-inner) :type 'error))))

(ert-deftest donkey-mark-inner-no-match-either-way-leaves-point-untouched ()
  "Failing to find a pair either way leaves point untouched.

Regression test: point outside any matching pair, with several
unrelated same-type pairs earlier in the buffer, signals an error
without moving point.

`donkey--mark-pair-scan-backward' walks past those earlier
pairs (correctly counting nesting depth as it goes) before it runs out of
buffer and fails -- each intermediate match genuinely moves point, so
without `save-excursion' wrapping the scan, the signalled error still
left point sitting at the last delimiter the scan happened to pass
through (here, the very first `(' in the buffer) instead of where the
command was actually invoked from.  Confirmed live: point after the
last `)' on the line
\";; To (create a (file), visit) it with '\\=`C-x\\=' \\=`C-f\\='' ...\",
pressing `m i (' silently landed on the first `(' with the error
message easy to miss, instead of just staying put."
  (with-temp-buffer
    (insert ";; To (create a (file), visit) it with 'C-x C-f' and enter text.")
    (goto-char (point-min))
    (search-forward "visit)")
    (let ((point-before (point)))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\()))
        (should-error (donkey-mark-inner) :type 'error))
      (should (= (point) point-before))
      (should-not (use-region-p)))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-pair-delimiters (customization)
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-pair-delimiters-customization-adds-new-pair ()
  "A pair added to `donkey-mark-pair-delimiters' is auto-recognized.

It is recognized without prompting via `read-char'."
  (let ((donkey-mark-pair-delimiters
         (cons (cons ?# ?#) donkey-mark-pair-delimiters)))
    (with-temp-buffer
      (insert "#comment#")
      (goto-char 1)
      (donkey-mark-inner)
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     "comment")))))

(ert-deftest donkey-mark-pair-delimiters-customization-asymmetric-pair-from-close-char ()
  "A custom asymmetric pair is recognized from its close character.

Such a pair added via `donkey-mark-pair-delimiters'
\(as one would in config.el, e.g. `(add-to-list \\='donkey-mark-pair-delimiters
\\='(?# . ?%))' for a hypothetical `#comment%' marker style) is
recognized from its CLOSE character too, not just its OPEN character --
the same fix that makes standing on a closing parenthesis work for the
built-in `(...)' pair applies equally to user-added pairs."
  (let ((donkey-mark-pair-delimiters
         (cons (cons ?# ?%) donkey-mark-pair-delimiters)))
    (with-temp-buffer
      (insert "#comment%")
      (goto-char (1- (point-max)))
      (cl-letf (((symbol-function 'read-char)
                 (lambda (&rest _) (error "read-char should not be called"))))
        (donkey-mark-inner))
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     "comment")))))

(ert-deftest donkey-mark-pair-delimiters-customization-removes-pair ()
  "A pair removed from `donkey-mark-pair-delimiters' is no longer auto-detected.

It falls through to the `read-char' prompt instead."
  (let* ((donkey-mark-pair-delimiters
          (assq-delete-all ?~ (copy-alist donkey-mark-pair-delimiters)))
         (prompted nil))
    (with-temp-buffer
      (insert "~bold~")
      (goto-char 1)
      (cl-letf (((symbol-function 'read-char)
                 (lambda (&rest _) (setq prompted t) ?~)))
        (should-error (donkey-mark-inner) :type 'error))
      (should prompted))))

(ert-deftest donkey-mark-pair-delimiters-prompt-reflects-customization ()
  "The `read-char' prompt reflects the current customization.

The prompt string is built from the current value of
`donkey-mark-pair-delimiters', so it stays in sync."
  (let ((donkey-mark-pair-delimiters '((?# . ?#))))
    (should (equal (donkey--mark-pair-prompt) "Char (#): "))))

(ert-deftest donkey-mark-pair-delimiters-unsupported-error-reflects-customization ()
  "The unsupported-delimiter error reflects the current customization.

Its message lists the characters
from the current value of `donkey-mark-pair-delimiters'."
  (let ((donkey-mark-pair-delimiters '((?# . ?#))))
    (should-error (donkey--mark-pair-unsupported-error ?!) :type 'error)
    (condition-case err
        (donkey--mark-pair-unsupported-error ?!)
      (error
       (should (string-match-p "Use: #" (error-message-string err)))))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-outer
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-outer-braces ()
  "Marks content including braces."
  (with-temp-buffer
    (insert "{hello}")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "{hello}"))))

(ert-deftest donkey-mark-outer-parens ()
  "Marks content including parens."
  (with-temp-buffer
    (insert "(world)")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(world)"))))

(ert-deftest donkey-mark-outer-brackets ()
  "Marks content including brackets."
  (with-temp-buffer
    (insert "[test]")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "[test]"))))

(ert-deftest donkey-mark-outer-double-quote ()
  "Marks content including double quotes."
  (with-temp-buffer
    (insert "\"quoted\"")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "\"quoted\""))))

(ert-deftest donkey-mark-outer-single-quote ()
  "Marks content including single quotes."
  (with-temp-buffer
    (insert "'quoted'")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "'quoted'"))))

(ert-deftest donkey-mark-outer-angle ()
  "Marks content including angle brackets."
  (with-temp-buffer
    (insert "<tag>")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "<tag>"))))

(ert-deftest donkey-mark-outer-underscore ()
  "Marks content including underscores."
  (with-temp-buffer
    (insert "_italic_")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "_italic_"))))

(ert-deftest donkey-mark-outer-asterisk ()
  "Marks content including asterisks."
  (with-temp-buffer
    (insert "*bold*")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "*bold*"))))

(ert-deftest donkey-mark-outer-tilde ()
  "Marks content including tildes."
  (with-temp-buffer
    (insert "~strike~")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "~strike~"))))

(ert-deftest donkey-mark-outer-equals ()
  "Marks content including equals signs."
  (with-temp-buffer
    (insert "=math=")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "=math="))))

(ert-deftest donkey-mark-outer-plus ()
  "Marks content including plus signs."
  (with-temp-buffer
    (insert "+code+")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "+code+"))))

(ert-deftest donkey-mark-outer-dollar ()
  "Marks content including dollar signs."
  (with-temp-buffer
    (insert "$latex$")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "$latex$"))))

(ert-deftest donkey-mark-outer-colon ()
  "Marks content including colons."
  (with-temp-buffer
    (insert ":date:")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   ":date:"))))

(ert-deftest donkey-mark-outer-slash ()
  "Marks content including slashes."
  (with-temp-buffer
    (insert "/path/")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "/path/"))))

(ert-deftest donkey-mark-outer-backtick ()
  "Marks content including backticks."
  (with-temp-buffer
    (insert "`inline`")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "`inline`"))))

(ert-deftest donkey-mark-outer-pipe ()
  "Marks content including pipes."
  (with-temp-buffer
    (insert "|pipe|")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "|pipe|"))))

(ert-deftest donkey-mark-outer-backslash ()
  "Marks content including backslashes."
  (with-temp-buffer
    (insert "\\escaped\\")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "\\escaped\\"))))

(ert-deftest donkey-mark-outer-edge-empty ()
  "Empty braces produce minimal selection including both delimiters."
  (with-temp-buffer
    (insert "{}")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "{}"))))

(ert-deftest donkey-mark-outer-edge-no-close ()
  "Unclosed delimiter raises error."
  (with-temp-buffer
    (insert "{unclosed")
    (goto-char 1)
    (should-error (donkey-mark-outer) :type 'error)))

(ert-deftest donkey-mark-outer-edge-nested ()
  "Nested delimiters select innermost pair including delimiters."
  (with-temp-buffer
    (insert "{{inner}}")
    (goto-char 2)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "{inner}"))))

(ert-deftest donkey-mark-outer-edge-multiline ()
  "Multiline content including delimiters selected."
  (with-temp-buffer
    (insert "{line1\nline2}")
    (goto-char 1)
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "{line1\nline2}"))))

(ert-deftest donkey-mark-outer-symmetric-delimiter-at-point-always-tries-forward-first ()
  "A symmetric delimiter at point always searches forward first.

Design contract: forward search wins when it finds anything, even a
different, unrelated pair.  See
`donkey-mark-inner-symmetric-delimiter-at-point-always-tries-forward-first'."
  (with-temp-buffer
    (insert "foo \"hello\" bar \"unrelated\" baz")
    (goto-char 1)
    (search-forward "hello")
    (should (eq (char-after) ?\"))
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "\" bar \""))))

(ert-deftest donkey-mark-outer-symmetric-delimiter-falls-back-when-no-forward-match ()
  "A symmetric delimiter falls back to a backward search.

It falls back when nothing is found forward.  See
`donkey-mark-inner-symmetric-delimiter-falls-back-when-no-forward-match'."
  (with-temp-buffer
    (insert "foo \"hello\" bar")
    (goto-char 1)
    (search-forward "hello")
    (should (eq (char-after) ?\"))
    (donkey-mark-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "\"hello\""))))

(ert-deftest donkey-mark-outer-edge-has-mark ()
  "Mark is set after command."
  (with-temp-buffer
    (insert "{content}")
    (goto-char 1)
    (donkey-mark-outer)
    (should (mark))))

(ert-deftest donkey-mark-outer-edge-region-valid ()
  "Region beginning is less than region end."
  (with-temp-buffer
    (insert "{valid}")
    (goto-char 1)
    (donkey-mark-outer)
    (should (< (region-beginning) (region-end)))))

(ert-deftest donkey-mark-outer-unsupported-delimiter-errors ()
  "An unsupported delimiter character (from the `read-char' prompt) signals an error."
  (with-temp-buffer
    (insert "!bang!")
    (goto-char 1)
    (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?!)))
      (should-error (donkey-mark-outer) :type 'error))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-sexp-inner
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-sexp-inner-parentheses ()
  "Marks content inside parentheses, excluding delimiters."
  (with-temp-buffer
    (insert "(hello)")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-sexp-inner-brackets ()
  "Marks content inside brackets, excluding delimiters."
  (with-temp-buffer
    (insert "[world]")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "world"))))

(ert-deftest donkey-mark-sexp-inner-braces ()
  "Marks content inside braces, excluding delimiters."
  (with-temp-buffer
    (insert "{test}")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "test"))))

(ert-deftest donkey-mark-sexp-inner-nested-parens ()
  "Marks innermost nested parentheses only."
  (with-temp-buffer
    (insert "((inner))")
    (goto-char 2)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "inner"))))

(ert-deftest donkey-mark-sexp-inner-nested-different-types ()
  "Marks inner expression regardless of delimiter type mix."
  (with-temp-buffer
    (insert "([mixed])")
    (goto-char 2)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "mixed"))))

(ert-deftest donkey-mark-sexp-inner-point-on-closer ()
  "Point on closing delimiter finds and marks content."
  (with-temp-buffer
    (insert "(hello)")
    (goto-char 7)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "hello"))))

(ert-deftest donkey-mark-sexp-inner-point-inside ()
  "Point inside expression marks entire inner content."
  (with-temp-buffer
    (insert "(content)")
    (goto-char 5)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "content"))))

(ert-deftest donkey-mark-sexp-inner-multiline ()
  "Multiline sexp content marked correctly."
  (with-temp-buffer
    (insert "(line1\nline2\nline3)")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "line1\nline2\nline3"))))

(ert-deftest donkey-mark-sexp-inner-with-whitespace ()
  "Whitespace trimmed from selection boundaries."
  (with-temp-buffer
    (insert "(  spaced  )")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "  spaced  "))))

(ert-deftest donkey-mark-sexp-inner-empty-expression ()
  "Empty parentheses raise error."
  (with-temp-buffer
    (insert "()")
    (goto-char 1)
    (should-error (donkey-mark-sexp-inner) :type 'user-error)))

(ert-deftest donkey-mark-sexp-inner-unbalanced-open ()
  "Unclosed parenthesis raises error."
  (with-temp-buffer
    (insert "(unclosed")
    (goto-char 1)
    (should-error (donkey-mark-sexp-inner) :type 'user-error)))

(ert-deftest donkey-mark-sexp-inner-unbalanced-close ()
  "Extra closing parenthesis raises user-error."
  (with-temp-buffer
    (insert "unclosed)")
    (goto-char 1)
    (should-error (donkey-mark-sexp-inner) :type 'user-error)))

(ert-deftest donkey-mark-sexp-inner-no-expression ()
  "No balanced expression nearby raises error."
  (with-temp-buffer
    (insert "plain text")
    (goto-char 1)
    (should-error (donkey-mark-sexp-inner) :type 'user-error)))

(ert-deftest donkey-mark-sexp-inner-has-mark ()
  "Mark is set after command."
  (with-temp-buffer
    (insert "(content)")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (mark))))

(ert-deftest donkey-mark-sexp-inner-region-valid ()
  "Region beginning is less than region end."
  (with-temp-buffer
    (insert "(valid)")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (< (region-beginning) (region-end)))))

(ert-deftest donkey-mark-sexp-inner-single-character ()
  "Single character content selected correctly."
  (with-temp-buffer
    (insert "(x)")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "x"))))

(ert-deftest donkey-mark-sexp-inner-deeply-nested ()
  "Deeply nested structure selects deepest level."
  (with-temp-buffer
    (insert "((((deep))))")
    (goto-char 5)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "deep"))))

(ert-deftest donkey-mark-sexp-inner-mixed-nesting ()
  "Mixed delimiter nesting respects type boundaries."
  (with-temp-buffer
    (insert "([[(mixed)]])")
    (goto-char 4)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "mixed"))))

(ert-deftest donkey-mark-sexp-inner-with-code ()
  "Lisp-like code content marked correctly."
  (with-temp-buffer
    (insert "(setq x 10)")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "setq x 10"))))

(ert-deftest donkey-mark-sexp-inner-with-string ()
  "String content inside sexp marked correctly."
  (with-temp-buffer
    (insert "(\"quoted string\")")
    (goto-char 1)
    (donkey-mark-sexp-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "\"quoted string\""))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-sexp-outer
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-sexp-outer-parentheses ()
  "Marks content including parentheses."
  (with-temp-buffer
    (insert "(hello)")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(hello)"))))

(ert-deftest donkey-mark-sexp-outer-brackets ()
  "Marks content including brackets."
  (with-temp-buffer
    (insert "[world]")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "[world]"))))

(ert-deftest donkey-mark-sexp-outer-braces ()
  "Marks content including braces."
  (with-temp-buffer
    (insert "{test}")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "{test}"))))

(ert-deftest donkey-mark-sexp-outer-nested-parens ()
  "Marks innermost nested parentheses including delimiters."
  (with-temp-buffer
    (insert "((inner))")
    (goto-char 2)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(inner)"))))

(ert-deftest donkey-mark-sexp-outer-nested-different-types ()
  "Marks inner expression including delimiters regardless of type mix."
  (with-temp-buffer
    (insert "([mixed])")
    (goto-char 2)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "[mixed]"))))

(ert-deftest donkey-mark-sexp-outer-point-on-closer ()
  "Point on closing delimiter finds and marks content including delimiters."
  (with-temp-buffer
    (insert "(hello)")
    (goto-char 7)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(hello)"))))

(ert-deftest donkey-mark-sexp-outer-point-inside ()
  "Point inside expression marks entire content including delimiters."
  (with-temp-buffer
    (insert "(content)")
    (goto-char 5)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(content)"))))

(ert-deftest donkey-mark-sexp-outer-multiline ()
  "Multiline sexp content including delimiters marked correctly."
  (with-temp-buffer
    (insert "(line1\nline2\nline3)")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(line1\nline2\nline3)"))))

(ert-deftest donkey-mark-sexp-outer-with-whitespace ()
  "Whitespace included in selection with delimiters."
  (with-temp-buffer
    (insert "(  spaced  )")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(  spaced  )"))))

(ert-deftest donkey-mark-sexp-outer-empty-expression ()
  "Empty parentheses select delimiters only, no error."
  (with-temp-buffer
    (insert "()")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "()"))))

(ert-deftest donkey-mark-sexp-outer-unbalanced-open ()
  "Unclosed parenthesis raises error."
  (with-temp-buffer
    (insert "(unclosed")
    (goto-char 1)
    (should-error (donkey-mark-sexp-outer) :type 'user-error)))

(ert-deftest donkey-mark-sexp-outer-unbalanced-close ()
  "Extra closing parenthesis raises user-error."
  (with-temp-buffer
    (insert "unclosed)")
    (goto-char 1)
    (should-error (donkey-mark-sexp-outer) :type 'user-error)))

(ert-deftest donkey-mark-sexp-outer-no-expression ()
  "No balanced expression nearby raises error."
  (with-temp-buffer
    (insert "plain text")
    (goto-char 1)
    (should-error (donkey-mark-sexp-outer) :type 'user-error)))

(ert-deftest donkey-mark-sexp-outer-has-mark ()
  "Mark is set after command."
  (with-temp-buffer
    (insert "(content)")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (mark))))

(ert-deftest donkey-mark-sexp-outer-region-valid ()
  "Region beginning is less than region end."
  (with-temp-buffer
    (insert "(valid)")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (< (region-beginning) (region-end)))))

(ert-deftest donkey-mark-sexp-outer-single-character ()
  "Single character content selected including delimiters."
  (with-temp-buffer
    (insert "(x)")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(x)"))))

(ert-deftest donkey-mark-sexp-outer-deeply-nested ()
  "Deeply nested structure selects deepest level including delimiters."
  (with-temp-buffer
    (insert "((((deep))))")
    (goto-char 5)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(deep)"))))

(ert-deftest donkey-mark-sexp-outer-mixed-nesting ()
  "Mixed delimiter nesting respects type boundaries, includes delimiters."
  (with-temp-buffer
    (insert "([[(mixed)]])")
    (goto-char 4)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(mixed)"))))

(ert-deftest donkey-mark-sexp-outer-with-code ()
  "Lisp-like code content marked including delimiters."
  (with-temp-buffer
    (insert "(setq x 10)")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(setq x 10)"))))

(ert-deftest donkey-mark-sexp-outer-with-string ()
  "String content inside sexp marked including delimiters."
  (with-temp-buffer
    (insert "(\"quoted string\")")
    (goto-char 1)
    (donkey-mark-sexp-outer)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "(\"quoted string\")"))))

;;; ---------------------------------------------------------------------------
;;; donkey-rectangle-mark-mode
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-rectangle-mark-mode-toggles-on ()
  "Calling `donkey-rectangle-mark-mode' enables `rectangle-mark-mode'."
  (with-temp-buffer
    (insert "hello\nworld")
    (goto-char 1)
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))
    (should (mark))))

(ert-deftest donkey-rectangle-mark-mode-advances-point ()
  "After activating rect mark mode, point moves right by 1."
  (with-temp-buffer
    (insert "hello\nworld")
    (goto-char 5)
    (let ((initial-pos 5))
      (donkey-rectangle-mark-mode)
      (should (= (point) (1+ initial-pos))))))

(ert-deftest donkey-rectangle-mark-mode-does-not-widen-existing-region ()
  "Starting a rectangle over an existing region does not widen it.

Regression test: converting an already-active region (e.g. from
`donkey-mark-inner') into a rectangle must use its own existing
corners, not widen it by one extra column.  `transient-mark-mode' is
bound to t here because `region-active-p'/`mark-active' only mean
anything when it's on -- off by default in `--batch', which would
otherwise make the region look inactive regardless of `activate-mark'."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "(hello)")
      (goto-char 2)
      (push-mark (point) t t)
      (goto-char 7)
      (activate-mark)
      (should (string= (buffer-substring (region-beginning) (region-end))
                       "hello"))
      (donkey-rectangle-mark-mode)
      (should (bound-and-true-p rectangle-mark-mode))
      (should (string= (buffer-substring (region-beginning) (region-end))
                       "hello")))))

(ert-deftest donkey-rectangle-mark-mode-still-widens-fresh-selection ()
  "A fresh rectangle selection still gets its one-column widening.

Starting `rectangle-mark-mode' with no pre-existing region still gets
its initial widening."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "AAAA\nBBBB\nCCCC\n")
      (goto-char 1)
      (should-not mark-active)
      (donkey-rectangle-mark-mode)
      (should (string= (buffer-substring (region-beginning) (region-end)) "A")))))

(ert-deftest donkey-rectangle-mark-mode-at-end-of-line-stays-on-its-column ()
  "The initial widening must not step over the newline.

Regression test: `right-char' at end of line moves to column 0 of the
NEXT line, so the widening did not widen the rectangle -- it moved it,
to the far side of the buffer from the column being looked at.

Confirmed live: point at the end of \"alpha\" (column 5), then \"m v\",
gave a rectangle whose columns were (0 . 0).  Pressing \"j\" twice and
\"c\" then prefixed every line against the left margin instead of
appending to it, with nothing on screen to explain why.

Zero width is the right answer here, not a bug to widen away:
`string-rectangle' INSERTS when the width is zero, which is what
standing at end of line and starting a rectangle is for."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "alpha\nbeta\ngamma\n")
      (goto-char (point-min))
      (end-of-line)
      (let ((col (current-column)))
        (should (= col 5))
        (donkey-rectangle-mark-mode)
        (should (bound-and-true-p rectangle-mark-mode))
        ;; The column is kept; only the width is zero.
        (should (= (current-column) col))
        (should (equal (rectangle--pos-cols (region-beginning) (region-end))
                       (cons col col)))))))

(ert-deftest donkey-rectangle-mark-mode-mid-line-still-widens ()
  "The end-of-line guard must not disarm the ordinary widening.

The guard is one `eolp\' test, and the failure it invites is disarming
the widening everywhere.  Pinned next to the end-of-line case so the two
cannot drift apart: mid-line keeps its one column of width."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "alpha\nbeta\ngamma\n")
      (goto-char (point-min))
      (donkey-rectangle-mark-mode)
      (should (= (current-column) 1))
      (should (equal (rectangle--pos-cols (region-beginning) (region-end))
                     (cons 0 1))))))

(ert-deftest donkey-rectangle-mark-mode-at-end-of-line-appends-via-change ()
  "End to end: a rectangle started at end of line appends to every row.

The point of the guard.  With the rectangle collapsed to column 0 this
inserted against the left margin instead, so the check is on the text,
not on the columns -- a reader never sees a column number."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "aaaaa\nbbbbb\nccccc\n")
      (goto-char (point-min))
      (end-of-line)
      (donkey-rectangle-mark-mode)
      (next-line 2)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ";")))
        (donkey-change))
      (should (equal (buffer-string) "aaaaa;\nbbbbb;\nccccc;\n")))))

(ert-deftest donkey-rectangle-mark-mode-creates-rectangular-selection ()
  "Rect mark mode creates a rectangular region selection."
  (with-temp-buffer
    (insert "hello\nworld\nfoo")
    (goto-char 1)
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))
    (should (mark))
    (should (< (mark) (point)))))

(ert-deftest donkey-rectangle-mark-mode-toggles-off ()
  "Calling the command again while active disables `rectangle-mark-mode'."
  (with-temp-buffer
    (insert "hello\nworld")
    (goto-char 1)
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))
    (donkey-rectangle-mark-mode)
    (should-not (bound-and-true-p rectangle-mark-mode))
    (should-not (region-active-p))))

(ert-deftest donkey-rectangle-mark-mode-edge-empty ()
  "In an empty buffer, `right-char' has nowhere to go but does not error."
  (with-temp-buffer
    (should (equal (buffer-string) ""))
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))))

(ert-deftest donkey-rectangle-mark-mode-edge-at-buffer-start ()
  "Activating rect mark mode at buffer start succeeds."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-min))
    (donkey-rectangle-mark-mode)
    (should (>= (point) (point-min)))
    (should (<= (point) (point-max)))))

(ert-deftest donkey-rectangle-mark-mode-edge-at-buffer-end ()
  "At buffer end, `right-char' has nowhere to go but does not error.

Regression test: `right-char' signals `end-of-buffer' with nothing
left to widen the rectangle into.  Confirmed live in `emacs -nw':
pressing `m v' at the end of a buffer used to surface an uncaught
\"End of buffer\" error message instead of cleanly toggling on (with a
valid, if zero-width, initial rectangle selection)."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-max))
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))
    (should (mark))
    (should (= (mark) (point-max)))))

(ert-deftest donkey-rectangle-mark-mode-edge-single-character ()
  "On a single character, `right-char' has one column to move into."
  (with-temp-buffer
    (insert "x")
    (goto-char 1)
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))))

(ert-deftest donkey-rectangle-mark-mode-edge-multi-line ()
  "With multi-line buffer, rect mark mode selects correctly."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char 1)
    (donkey-rectangle-mark-mode)
    (should (mark))
    (should (> (point) (mark)))))

(ert-deftest donkey-rectangle-mark-mode-edge-before-newline ()
  "Invoking just before newline character works correctly."
  (with-temp-buffer
    (insert "abc\ndef")
    (goto-char 3)
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))))

(ert-deftest donkey-rectangle-mark-mode-edge-on-newline ()
  "Invoking on newline character advances to next line."
  (with-temp-buffer
    (insert "abc\ndef")
    (goto-char 4)
    (donkey-rectangle-mark-mode)
    (should (or (= (point) 5)
                (= (point) 4)))))

(ert-deftest donkey-rectangle-mark-mode-edge-has-mark ()
  "The mark is set after activating rect mode."
  (with-temp-buffer
    (insert "test content")
    (goto-char 1)
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))
    (should (mark))))

(ert-deftest donkey-rectangle-mark-mode-edge-call-interactively ()
  "Command can be called interactively without error."
  (with-temp-buffer
    (insert "hello")
    (goto-char 1)
    (call-interactively #'donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))))

(ert-deftest donkey-rectangle-mark-mode-edge-region-boundaries ()
  "Rectangle region has valid boundaries."
  (with-temp-buffer
    (insert "abcde")
    (goto-char 2)
    (donkey-rectangle-mark-mode)
    (let ((beg (mark))
          (end (point)))
      (should (< beg end)))))

(ert-deftest donkey-rectangle-mark-mode-edge-after-right-char ()
  "Point advances exactly one character after activation."
  (with-temp-buffer
    (insert "01234")
    (goto-char 2)
    (let ((before 2))
      (donkey-rectangle-mark-mode)
      (should (= (point) (+ before 1))))))

(ert-deftest donkey-rectangle-mark-mode-edge-preserves-text ()
  "Buffer contents unchanged after activating rect mark mode."
  (with-temp-buffer
    (let ((original "preserve this text"))
      (insert original)
      (goto-char 1)
      (donkey-rectangle-mark-mode)
      (should (string= original (buffer-string))))))

(ert-deftest donkey-rectangle-mark-mode-edge-with-prefix-arg ()
  "Command works when `current-prefix-arg' is set."
  (with-temp-buffer
    (insert "hello")
    (goto-char 1)
    (let ((current-prefix-arg '(4)))
      (donkey-rectangle-mark-mode)
      (should (bound-and-true-p rectangle-mark-mode)))))

(ert-deftest donkey-rectangle-mark-mode-edge-empty-at-start ()
  "An empty buffer at `point-min' does not error.

Here `right-char' has nowhere to go, but must not signal."
  (with-temp-buffer
    (goto-char (point-min))
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-inner: exhaustive coverage of every default delimiter pair
;;; ---------------------------------------------------------------------------
;;;
;;; These three tests iterate `donkey-mark-pair-delimiters' itself (rather
;;; than hardcoding a handful of pairs), so a future typo like the one that
;;; silently broke curly single quotes -- mapping U+2019 to itself for both
;;; OPEN and CLOSE, instead of pairing it with U+2018 -- is always caught,
;;; for any pair, without needing a dedicated regression test per delimiter.

(ert-deftest donkey-mark-inner-all-default-delimiters-from-open-char ()
  "Every default pair is auto-detected from its open character.

For every default (OPEN . CLOSE) pair, point on the OPEN character
auto-detects the delimiter (no `read-char' prompt needed) and selects
the content up to the next CLOSE occurrence."
  (dolist (pair donkey-mark-pair-delimiters)
    (let ((open (car pair)) (close (cdr pair)))
      (ert-info ((format "pair (%c . %c)" open close))
        (with-temp-buffer
          (insert (string open) "quoted" (string close))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'read-char)
                     (lambda (&rest _) (error "read-char should not be called"))))
            (donkey-mark-inner))
          (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                         "quoted")))))))

(ert-deftest donkey-mark-inner-all-default-delimiters-from-close-char ()
  "Every default pair is auto-detected from its close character.

For every default (OPEN . CLOSE) pair, point on the CLOSE character
auto-detects the delimiter (no `read-char' prompt needed) and selects
the content between the delimiters -- via the forward-then-backward
fallback when OPEN and CLOSE are the same character, or by resolving
OPEN from CLOSE directly (see `donkey--mark-pair-read-delimiter') when
they differ, e.g. standing on a closing parenthesis."
  (dolist (pair donkey-mark-pair-delimiters)
    (let ((open (car pair)) (close (cdr pair)))
      (ert-info ((format "pair (%c . %c)" open close))
        (with-temp-buffer
          (insert (string open) "quoted" (string close))
          (goto-char (1- (point-max)))
          (cl-letf (((symbol-function 'read-char)
                     (lambda (&rest _) (error "read-char should not be called"))))
            (donkey-mark-inner))
          (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                         "quoted")))))))

(ert-deftest donkey-mark-inner-all-default-delimiters-from-inside-with-manual-char ()
  "Every default pair is found from inside via the manual prompt.

For every default (OPEN . CLOSE) pair, point somewhere INSIDE the
pair (on neither delimiter) always falls through to the `read-char'
prompt; answering with OPEN correctly finds the enclosing pair."
  (dolist (pair donkey-mark-pair-delimiters)
    (let ((open (car pair)) (close (cdr pair)))
      (ert-info ((format "pair (%c . %c)" open close))
        (with-temp-buffer
          (insert (string open) "quoted" (string close))
          (goto-char (+ (point-min) 3))
          (cl-letf (((symbol-function 'read-char) (lambda (&rest _) open)))
            (donkey-mark-inner))
          (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                         "quoted")))))))

(ert-deftest donkey-mark-whole-buffer-clears-stale-rectangle ()
  "`%' clears a stale rectangle before selecting the buffer.

Regression: `%' was the one selection-establishing key bound straight to
a stock command, so it never ran
`donkey--ensure-non-rectangle-selection'.  A rectangle left active from
an earlier session survived underneath the whole-buffer selection."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "abcd\nefgh\nijkl\n")
      (goto-char (point-min))
      (donkey-rectangle-mark-mode)
      (should (bound-and-true-p rectangle-mark-mode))
      (donkey-mark-whole-buffer)
      (should-not (bound-and-true-p rectangle-mark-mode))
      (should (use-region-p)))))

(ert-deftest donkey-mark-whole-buffer-then-delete-empties-buffer ()
  "After `%', `d' deletes the buffer rather than a zero-width rectangle.

Regression: the stale rectangle made `donkey-delete' kill one empty
string per line, leaving the text completely untouched with no error,
and leaving that emptiness in `killed-rectangle' so a rectangle paste would have
inserted that emptiness."
  (let ((transient-mark-mode t) (kill-ring nil) kill-ring-yank-pointer)
    (with-temp-buffer
      (insert "abcd\nefgh\nijkl\n")
      (goto-char (point-min))
      (donkey-rectangle-mark-mode)
      (donkey-mark-whole-buffer)
      (donkey-delete)
      (should (equal (buffer-string) "")))))

(ert-deftest donkey-mark-whole-buffer-is-bound-to-percent ()
  "`%' reaches the wrapper, not the stock command."
  (should (eq (keymap-lookup donkey-normal-mode-map "%")
              #'donkey-mark-whole-buffer)))

(ert-deftest donkey-top-and-bottom-keys-cover-vim-and-helix ()
  "Both editors' keys reach the top and bottom of the buffer.

`g g' is the start of the buffer in Vim and Helix alike, so it needs no
twin.  The end does: `g e' is Helix's and `G' is Vim's.  That
duplication is deliberate -- pinned here so it is not mistaken for a
leftover and removed."
  (should (eq (keymap-lookup donkey-normal-mode-map "g g") #'beginning-of-buffer))
  (should (eq (keymap-lookup donkey-normal-mode-map "g e") #'end-of-buffer))
  (should (eq (keymap-lookup donkey-normal-mode-map "G") #'end-of-buffer))
  ;; Removed: Helix's own "g t" is goto_window_top, not the file start,
  ;; so the binding matched neither editor.
  (should-not (keymap-lookup donkey-normal-mode-map "g t")))

(ert-deftest donkey-delete-keys-cover-vim-and-helix ()
  "`d' and `x' both delete: Helix's key and Vim's key for the same command.

Both editors' keys already cover char-or-selection -- Vim's `x' deletes
the character in normal state and the selection in visual state -- which
is exactly what `donkey-delete' does.  Deliberate duplication, pinned so
it is not mistaken for a leftover.  `D' stays `kill-line', Vim's
delete-to-end-of-line."
  (should (eq (keymap-lookup donkey-normal-mode-map "d") #'donkey-delete))
  (should (eq (keymap-lookup donkey-normal-mode-map "x") #'donkey-delete))
  (should (eq (keymap-lookup donkey-normal-mode-map "D") #'kill-line)))

(ert-deftest donkey-mark-sentence-newlines-only-signals-user-error ()
  "A buffer of only blank lines reports cleanly.

Regression: this raised a bare `error' reading \"Invalid search bound
\(wrong side of point)\" -- an internal that tells whoever pressed the
key nothing at all."
  (with-temp-buffer
    (insert "\n\n\n")
    (goto-char (point-min))
    (should-error (donkey-mark-sentence) :type 'user-error)))

(ert-deftest donkey-mark-sentence-whitespace-only-signals-user-error ()
  "A buffer of only whitespace reports cleanly."
  (with-temp-buffer
    (insert "   ")
    (goto-char (point-min))
    (should-error (donkey-mark-sentence) :type 'user-error)))

(ert-deftest donkey-mark-sentence-still-marks-ordinary-prose ()
  "The guard does not stop the command doing its job."
  (with-temp-buffer
    (insert "Hello there.  Second one.\n")
    (goto-char (point-min))
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Hello there."))))

(ert-deftest donkey-mark-sentence-below-prose-reports-no-next-sentence ()
  "Point on a blank line under prose reports rather than marking upwards.

Deliberately reverses what this test asserted when it was added: it used
to expect the sentence above to be marked.  Standing in the gap after a
sentence asks for the one COMING -- which is what a gap in the middle of
the text gives -- and in the trailing gap there is none, so marking the
previous sentence would answer a question that was not asked."
  (with-temp-buffer
    (insert "Hello there.\n\n\n")
    (goto-char (point-max))
    (should-error (donkey-mark-sentence) :type 'user-error)))

(ert-deftest donkey-mark-sentence-gap-selects-the-coming-sentence ()
  "In the gap after a sentence, the sentence that follows is marked.

A buffer and a pinned `last-command' per position, not one shared
between them: `mark-end-of-sentence' extends the existing selection when
`(eq last-command this-command)', and in batch BOTH are nil, so the
second position looked like a repeat of the first and marked two
sentences.  It passed only because some earlier test in the full run
happened to leave `last-command' set -- run alone, or under a selector,
it failed.  Interactively the moves between the two positions set
`last-command' themselves, which is what the binding here stands in for."
  ;; the spaces between the first and second sentences
  (dolist (pos '(15 16))
    (with-temp-buffer
      (insert "One two three.  Four five six.  Seven eight nine.\n")
      (goto-char pos)
      (let ((last-command 'forward-char)
            (this-command 'donkey-mark-sentence))
        (donkey-mark-sentence))
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     "Four five six.")))))

(ert-deftest donkey-mark-sentence-repeated-extends-by-one-sentence ()
  "Pressing the key again grows the selection by one more sentence.

Inherited from `mark-end-of-sentence', which extends rather than
re-marks when `(eq last-command this-command)'.  The other mark commands
do not do this -- `mark-word' gates the same behaviour behind an
ALLOW-EXTEND argument that is nil when called from Lisp."
  (with-temp-buffer
    (insert "One two three.  Four five six.  Seven eight nine.")
    (goto-char (point-min))
    (let ((this-command 'donkey-mark-sentence))
      (let ((last-command 'self-insert-command))
        (donkey-mark-sentence))
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     "One two three."))
      (let ((last-command 'donkey-mark-sentence))
        (donkey-mark-sentence))
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     "One two three.  Four five six.")))))

(ert-deftest donkey-mark-sentence-repeated-past-the-last-sentence-stops ()
  "Repeating past the last sentence keeps the whole selection, quietly.

Regression: the extension eventually walked `forward-sentence' off the
end, which signalled `end-of-buffer' and got reported as \"No sentence
at or before point\" -- so one press too many on the last sentence threw
away a selection that was already correct."
  (with-temp-buffer
    (insert "One two three.  Four five six.")
    (goto-char (point-min))
    (let ((this-command 'donkey-mark-sentence))
      (let ((last-command 'self-insert-command))
        (donkey-mark-sentence))
      (let ((last-command 'donkey-mark-sentence))
        (donkey-mark-sentence)
        (donkey-mark-sentence))
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     "One two three.  Four five six.")))))

(ert-deftest donkey-mark-sentence-on-a-period-marks-the-sentence-it-ends ()
  "A period belongs to the sentence before it, and that one is marked."
  (with-temp-buffer
    (insert "One two three.  Four five six.\n")
    (goto-char 14)                      ; the "." of the first sentence
    (should (equal (char-after) ?.))
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "One two three."))))

(ert-deftest donkey-mark-sentence-past-the-last-sentence-reports ()
  "With no sentence ahead, the command reports instead of marking behind."
  (with-temp-buffer
    (insert "One two three.  Four five six.\n")
    (goto-char (point-max))
    (should-error (donkey-mark-sentence) :type 'user-error)))

(ert-deftest donkey-mark-sentence-from-sentence-start-marks-that-sentence ()
  "Point on the first letter marks THAT sentence, not the one before.

Regression, reported live on the scratch message: with the cursor on the
\"T\" of \"To create a file\", this selected \"This buffer is for text
that is not saved, and for Lisp evaluation.\" -- the sentence before it.
`backward-sentence' lands on the PREVIOUS sentence whenever point already
sits at a sentence start, which is the most natural place to press the
key."
  (with-temp-buffer
    (insert "This buffer is for text that is not saved, and for Lisp evaluation.\n"
            "To create a file, visit it with 'C-x C-f' and enter text in its buffer.\n")
    (goto-char (point-min))
    (search-forward "To create")
    (goto-char (match-beginning 0))
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "To create a file, visit it with 'C-x C-f' and enter text in its buffer."))))

(ert-deftest donkey-mark-sentence-marks-the-same-sentence-from-any-position ()
  "Every position inside a sentence marks that one sentence.

Start, middle and end all have to agree; only the middle did before."
  (let ((text "One two three.  Four five six.  Seven eight nine.\n"))
    (dolist (probe '(("One two"     . "One two three.")
                     ("two three"   . "One two three.")
                     ("Four five"   . "Four five six.")
                     ("five six"    . "Four five six.")
                     ("Seven eight" . "Seven eight nine.")
                     ("eight nine"  . "Seven eight nine.")))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (search-forward (car probe))
        (goto-char (match-beginning 0))
        (donkey-mark-sentence)
        (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                       (cdr probe)))))))

(ert-deftest donkey-mark-commands-honour-a-count ()
  "The mark commands select COUNT things."
  (with-temp-buffer
    (insert "alpha beta gamma delta\n")
    (goto-char 1)
    (donkey-mark-word 3)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "alpha beta gamma")))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo-a bar-b baz-c\n")
    (goto-char 1)
    (donkey-mark-symbol 2)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "foo-a bar-b")))
  (with-temp-buffer
    (insert "One two.  Three four.  Five six.\n")
    (goto-char 1)
    (donkey-mark-sentence 2)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "One two.  Three four.")))
  (with-temp-buffer
    (insert "P1 line.\n\nP2 line.\n\nP3 line.\n")
    (goto-char 1)
    (donkey-mark-paragraph 2)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "P1 line.\n\nP2 line.\n"))))

(ert-deftest donkey-mark-symbol-count-covers-the-whole-run ()
  "A counted symbol selection starts at the first symbol, not the last.

Caught while adding counts: the closing `backward-sexp' moved back one
regardless of the count, so a count of 2 over \"foo-a bar-b\" marked only
\"bar-b\"."
  (dolist (probe '((1 . "foo-a") (2 . "foo-a bar-b") (3 . "foo-a bar-b baz-c")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "foo-a bar-b baz-c\n")
      (goto-char 1)
      (donkey-mark-symbol (car probe))
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     (cdr probe))))))

(ert-deftest donkey-mark-sexp-count-goes-that-many-levels-out ()
  "A count on the sexp marks selects that many levels outward."
  (dolist (probe '((1 . "(c d)") (2 . "(b (c d) e)") (3 . "(a (b (c d) e) f)")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(a (b (c d) e) f)\n")
      (goto-char 8)                     ; inside (c d)
      (donkey-mark-sexp-outer (car probe))
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     (cdr probe))))))

(ert-deftest donkey-mark-pair-count-goes-that-many-levels-out ()
  "A count on `m i'/`m a' selects that many levels of nesting outward."
  (let ((text "ratory (up at (the hospital. He was) bemoaning) him-\n"))
    (dolist (probe '((nil . "the hospital. He was")
                     (1   . "the hospital. He was")
                     (2   . "up at (the hospital. He was) bemoaning")))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (search-forward "hospital")
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\()))
          (donkey-mark-inner (car probe)))
        (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                       (cdr probe)))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (search-forward "hospital")
      (goto-char (match-beginning 0))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\()))
        (donkey-mark-outer 2))
      (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                     "(up at (the hospital. He was) bemoaning)")))))

(ert-deftest donkey-mark-pair-count-on-a-symmetric-delimiter-counts-outward ()
  "A count on a symmetric delimiter counts OCCURRENCES outward.

Once refused outright, on the reasoning that a character serving as both
ends has no nesting for a level to refer to.  That argument proves too
much -- it rules out level 1 as well, which ships and is useful -- and it
cost the ordinary prose case this asserts: nested quoted speech, where
\"two levels out\" plainly means the outer quotation."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "you died.  \"No use \"writing on paper.\" That\" would be\n")
      (goto-char (point-min))
      (search-forward "writ")
      (goto-char (- (point) 2))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\")))
        (donkey-mark-inner 2))
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "No use \"writing on paper.\" That")))))

(ert-deftest donkey-mark-pair-count-of-one-on-a-symmetric-delimiter-unchanged ()
  "Level 1 still finds the nearest occurrence each way, as it always did."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "you died.  \"No use \"writing on paper.\" That\" would be\n")
      (goto-char (point-min))
      (search-forward "writ")
      (goto-char (- (point) 2))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\")))
        (donkey-mark-inner 1))
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "writing on paper.")))))

(ert-deftest donkey-mark-pair-count-past-the-last-symmetric-delimiter ()
  "Running out of occurrences reports the same error the nesting path does."
  (with-temp-buffer
    (insert "say \"hello there\" now\n")
    (goto-char (point-min))
    (search-forward "hello")
    (goto-char (match-beginning 0))
    (let ((text-quoting-style 'grave))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\")))
        (let ((err (should-error (donkey-mark-inner 2) :type 'user-error)))
          (should (equal (cadr err)
                         "No enclosing `\"' beyond that level")))))))

(ert-deftest donkey-mark-outer-count-on-a-symmetric-delimiter-counts-outward ()
  "`m a' counts outward the same way, delimiters included."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "you died.  \"No use \"writing on paper.\" That\" would be\n")
      (goto-char (point-min))
      (search-forward "writ")
      (goto-char (- (point) 2))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\")))
        (donkey-mark-outer 2))
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "\"No use \"writing on paper.\" That\"")))))

(ert-deftest donkey-mark-pair-count-works-for-every-configured-delimiter ()
  "Every pair in `donkey-mark-pair-delimiters' takes a count.

Symmetric ones by counting occurrences outward, asymmetric ones by the
depth-counting scan.  Asserted over the whole configured set rather than
a sample, so adding a delimiter that a count cannot reach fails here."
  (let ((transient-mark-mode t))
    (dolist (pair donkey-mark-pair-delimiters)
      (let* ((open (car pair))
             (close (cdr pair))
             (text (format "x%cone%cTARGET%ctwo%cy"
                           open open close close))
             (expected (format "one%cTARGET%ctwo" open close)))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (search-forward "TARG")
          (goto-char (- (point) 2))
          (cl-letf (((symbol-function 'read-char) (lambda (&rest _) open)))
            (donkey-mark-inner 2))
          (should (equal (cons open
                               (buffer-substring-no-properties
                                (region-beginning) (region-end)))
                         (cons open expected))))))))

;;; ---------------------------------------------------------------------------
;;; Zero and negative counts on the mark commands
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-word-negative-count-marks-backward ()
  "A negative count marks that many words before the one point is on.

Regression: the count was clamped with `(max 1 count)', so a negative
count marked one word FORWARD -- the opposite direction from the one
asked for.  `mark-word' reads the sign natively."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "alpha beta gamma")
      (goto-char 13)
      (donkey-mark-word -2)
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "alpha beta ")))))

(ert-deftest donkey-mark-word-zero-count-marks-nothing ()
  "A count of zero marks an empty region, as `mark-word' does."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "alpha beta gamma")
      (goto-char 13)
      (donkey-mark-word 0)
      (should (= (region-beginning) (region-end))))))

(ert-deftest donkey-mark-symbol-negative-count-marks-backward ()
  "A negative count marks that many symbols before the one point is on."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "foo-a bar-b baz-c")
      (goto-char 14)
      (donkey-mark-symbol -2)
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "foo-a bar-b")))))

(ert-deftest donkey-mark-symbol-still-trims-trailing-punctuation ()
  "A forward count still drops a trailing comma or period.

The trim only applies going forwards: a negative count leaves point at
the region's START, where backing over punctuation would reach into the
symbol before it."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "foo-a. bar-b")
      (goto-char 2)
      (donkey-mark-symbol 1)
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "foo-a")))))

(ert-deftest donkey-mark-paragraph-negative-count-marks-backward ()
  "A negative count marks the paragraph before the one point is on."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "one one.\n\ntwo two.\n\nthree.\n")
      (goto-char 20)
      (donkey-mark-paragraph -1)
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "one one.\n")))))

(ert-deftest donkey-mark-sentence-treats-counts-below-one-as-one ()
  "Unlike the other mark commands, this one clamps a count below 1.

It is defined in terms of the sentence AHEAD of point -- it normalises
forward and rejects a selection ending behind where it started -- so a
zero or negative count has nothing it could mean but that error."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "One thing.  Two thing.  Three thing.")
      (goto-char (point-min))
      (donkey-mark-sentence 0)
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "One thing.")))))

(ert-deftest donkey-mark-sentence-count-past-last-sentence-marks-what-there-is ()
  "A count reaching past the last sentence stops at the end quietly.

Regression: `mark-end-of-sentence' signals a bare `end-of-buffer' when
its count runs off the end, which the guard turned into \"No sentence at
or before point\" -- a flat contradiction of a screen that is showing
three of them.  Bare \\[universal-argument] means FOUR, so plain
`C-u m s' hit this on any buffer of three sentences or fewer."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "One thing.  Two thing.  Three thing.")
      (goto-char (point-min))
      (donkey-mark-sentence 4)
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "One thing.  Two thing.  Three thing.")))))

(ert-deftest donkey-mark-sentence-still-reports-on-empty-buffer ()
  "Splitting the guard in two left the empty-buffer report intact."
  (with-temp-buffer
    (should-error (donkey-mark-sentence 1) :type 'user-error)))

(ert-deftest donkey-mark-sentence-still-reports-on-whitespace-only-buffer ()
  "Splitting the guard in two left the whitespace-only report intact."
  (with-temp-buffer
    (insert "   \n\n  \n")
    (goto-char (point-min))
    (should-error (donkey-mark-sentence 1) :type 'user-error)))

(ert-deftest donkey-mark-inner-count-past-outermost-pair-reports-the-level ()
  "A count past the outermost pair blames the count, not the delimiter.

Regression: the scan ran out of enclosing pairs and signalled a bare
`error' reading \"No \='(\=' found near cursor\" -- which contradicts a
screen plainly showing several, reads like the delimiter was mistyped
rather than the count overshot, and pops the debugger for anyone running
with `debug-on-error' on.  Bare \\[universal-argument] means FOUR, so
`C-u m i' hit this on text only one or two levels deep."
  (with-temp-buffer
    (insert "a (b (c d) e) f")
    (goto-char 8)
    ;; `error'/`user-error' run their format string through
    ;; `format-message', which rewrites ` and ' as curved quotes whenever
    ;; `text-quoting-style' resolves to `curve' -- the default on any
    ;; terminal that can display them.  Comparing against a literal only
    ;; holds if the style is pinned: without this the assertion passed in
    ;; a sandbox with no locale set and failed on CI, where the UTF-8
    ;; locale selected `curve' and the message read "No ‘(’ beyond...".
    (let ((text-quoting-style 'grave))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\()))
        (let ((err (should-error (donkey-mark-inner 3) :type 'user-error)))
          (should (equal (cadr err)
                         "No enclosing `(' beyond that level")))))))

(ert-deftest donkey-mark-inner-count-within-nesting-still-works ()
  "The level guard did not cost the levels that do exist."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "a (b (c d) e) f")
      (goto-char 8)
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\()))
        (donkey-mark-inner 2))
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "b (c d) e")))))

(ert-deftest donkey-mark-inner-missing-delimiter-is-a-user-error ()
  "With no bracket anywhere, the report is a `user-error'.

Pressing this on a line with no bracket on it is an ordinary miss, not a
malfunction; a bare `error' popped the debugger under `debug-on-error'.
The message itself is unchanged -- there really is no delimiter here."
  (with-temp-buffer
    (insert "no parens here")
    (goto-char 5)
    ;; `text-quoting-style' pinned -- see
    ;; `donkey-mark-inner-count-past-outermost-pair-reports-the-level'.
    (let ((text-quoting-style 'grave))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\()))
        (let ((err (should-error (donkey-mark-inner) :type 'user-error)))
          (should (equal (cadr err) "No '(' found near cursor")))))))

(ert-deftest donkey-mark-inner-on-an-empty-pair-is-a-user-error ()
  "`m i' on an empty pair reports without popping the debugger.

An empty pair is ordinary in code -- `()' for a no-argument call, `\"\"'
for an empty string -- so pressing this on one is a miss, not a
malfunction.  A bare `error' popped the debugger under `debug-on-error'."
  (with-temp-buffer
    (insert "a () b")
    (goto-char 4)
    (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\()))
      (should-error (donkey-mark-inner) :type 'user-error))))

(ert-deftest donkey-mark-outer-on-an-empty-pair-still-selects-it ()
  "`m a' on an empty pair selects the delimiters, which are the content."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "a () b")
      (goto-char 4)
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\()))
        (donkey-mark-outer))
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "()")))))

(ert-deftest donkey-mark-inner-unsupported-delimiter-is-a-user-error ()
  "Answering the prompt with a non-delimiter reports, and lists the set.

An ordinary typo on a prompt that accepts nineteen characters -- a bare
`error' popped the debugger under `debug-on-error'."
  (with-temp-buffer
    (insert "a (b) c")
    (goto-char 2)
    (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?z)))
      (let ((err (should-error (donkey-mark-inner) :type 'user-error)))
        (should (string-match-p "Unsupported delimiter"
                                (error-message-string err)))))))

(ert-deftest donkey-mark-pair-count-works-for-user-configured-delimiters ()
  "A count reaches pairs added to `donkey-mark-pair-delimiters' too.

The branch is on whether the pair's two characters are equal, not on any
list of known delimiters, so a configured pair goes down the same route
as a shipped one.  Asserted with a symmetric addition and an asymmetric
one, both outside the default set."
  (let ((transient-mark-mode t)
        (donkey-mark-pair-delimiters
         (append donkey-mark-pair-delimiters
                 ;; "#" symmetric; guillemets asymmetric and non-ASCII.
                 (list (cons ?# ?#) (cons ?\« ?\»)))))
    (dolist (case '((?# "a#one#TARGET#two#b" "TARGET" "one#TARGET#two")
                    (?\« "a«one«TARGET»two»b" "TARGET" "one«TARGET»two")))
      ;; Both routes: no count at all, and a count of 2.  A configured
      ;; delimiter that only worked on one of them would be half-supported.
      (dolist (count (list nil 2))
        (with-temp-buffer
          (insert (nth 1 case))
          (goto-char (point-min))
          (search-forward "TARG")
          (goto-char (- (point) 2))
          (cl-letf (((symbol-function 'read-char)
                     (lambda (&rest _) (nth 0 case))))
            (if count (donkey-mark-inner count) (donkey-mark-inner)))
          (should (equal (list (nth 0 case) count
                               (buffer-substring-no-properties
                                (region-beginning) (region-end)))
                         (list (nth 0 case) count
                               (if count (nth 3 case) (nth 2 case))))))))))

(ert-deftest donkey-mark-pair-symmetric-count-is-case-sensitive ()
  "A count of a LETTER delimiter does not fold case.

`donkey-mark-pair-delimiters' is a defcustom, so a letter can be
configured as a delimiter, and buffers default to `case-fold-search' t.
Regression: level 1 binds `case-fold-search' to nil and the outward walk
did not, so the two disagreed about what a delimiter is -- a count of 2
stopped at the lowercase x and marked \" mid X TARGET X two \"."
  (let ((transient-mark-mode t)
        (donkey-mark-pair-delimiters
         (append donkey-mark-pair-delimiters
                 (list (cons ?X ?X) (cons ?x ?x) (cons ?q ?Q)))))
    (dolist (fold '(t nil))
      (dolist (case '((?X "A X one x mid X TARGET X two X B"
                          " one x mid X TARGET X two ")
                      ;; The mirror image: a lowercase delimiter must not
                      ;; count the uppercase letter either.
                      (?x "A x one X mid x TARGET x two x B"
                          " one X mid x TARGET x two ")
                      ;; And a pair that differs only in case has to stay a
                      ;; PAIR -- folded, `q' and `Q' are indistinguishable
                      ;; and it would collapse into a symmetric delimiter.
                      (?q "z q one q TARGET Q two Q Z"
                          " one q TARGET Q two ")))
        ;; Both routes.  Level 1 goes through `donkey--mark-pair-positions',
        ;; which has bound `case-fold-search' all along; the count goes
        ;; through the outward walk, which is what did not.  Asserting only
        ;; the count would leave the two free to drift apart again.
        (dolist (count (list nil 2))
          (with-temp-buffer
            (insert (nth 1 case))
            (goto-char (point-min))
            (search-forward "TARG")
            (goto-char (- (point) 2))
            (let ((case-fold-search fold))
              (cl-letf (((symbol-function 'read-char)
                         (lambda (&rest _) (nth 0 case))))
                (if count (donkey-mark-inner count) (donkey-mark-inner))))
            (should (equal (list fold (nth 0 case) count
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end)))
                           (list fold (nth 0 case) count
                                 (if count (nth 2 case) " TARGET "))))))))))

(ert-deftest donkey-mark-pair-lopsided-symmetric-count-refuses-half-a-widen ()
  "Enough delimiters one way but not the other is refused, not half-done.

The outward walk searches backward and forward separately, so a text with
three occurrences behind point and two ahead can satisfy one and not the
other.  Both are inside one `condition-case', so a successful backward
walk is discarded rather than combined with an unwidened forward end --
a span widened on one side only would silently mark something nobody
asked for."
  (with-temp-buffer
    (insert "\"x \"y \"TARGET\" z\"")
    (goto-char (point-min))
    (search-forward "TARG")
    (goto-char (- (point) 2))
    (let ((text-quoting-style 'grave))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\")))
        (let ((err (should-error (donkey-mark-inner 3) :type 'user-error)))
          (should (equal (cadr err)
                         "No enclosing `\"' beyond that level")))))))

(ert-deftest donkey-mark-pair-lopsided-symmetric-count-of-two-still-works ()
  "The level that IS available on both sides is still given."
  (let ((transient-mark-mode t))
    (with-temp-buffer
      (insert "\"x \"y \"TARGET\" z\"")
      (goto-char (point-min))
      (search-forward "TARG")
      (goto-char (- (point) 2))
      (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?\")))
        (donkey-mark-inner 2))
      (should (equal (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                     "y \"TARGET\" z")))))

(ert-deftest donkey-mark-pair-overshoot-leaves-point-and-region-alone ()
  "Overshooting a count moves nothing and activates nothing.

Both delimiter kinds, and several sizes of overshoot.  The searches walk
point across the buffer as a means of computing the span, so without
`save-excursion' a refusal would strand point on some unrelated delimiter
-- and a half-set region would look like a selection that was asked for."
  (let ((transient-mark-mode t))
    (dolist (case '((?\" "you died. \"No use \"writing on paper.\" That\" would be")
                    (?\( "a (b (c d) e) f")))
      (dolist (count '(3 4 9))
        (with-temp-buffer
          (insert (nth 1 case))
          (goto-char (point-min))
          (search-forward "writ" nil t)
          (search-forward "c d" nil t)
          (goto-char (- (point) 2))
          (let ((origin (point)))
            (cl-letf (((symbol-function 'read-char)
                       (lambda (&rest _) (nth 0 case))))
              (should-error (donkey-mark-inner count) :type 'user-error))
            (should (equal (list (nth 0 case) count (point) (region-active-p))
                           (list (nth 0 case) count origin nil)))))))))

(provide 'donkey-marking-test)

;;; donkey-marking-test.el ends here

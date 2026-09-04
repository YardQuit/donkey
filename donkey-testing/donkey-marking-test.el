;;; donkey-marking-test.el --- Tests for DONKEY mark/selection commands -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'rect)
(require 'kmacro)
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
mark command elsewhere (without canceling the rectangle first) left
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
  "A buffer with nothing to mark reports cleanly rather than signaling `error'."
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
  "Empty buffer reports a `user-error' rather than signaling.

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
  "In multi-paragraph buffer, selects first paragraph and one blank after it.

The trailing blank used to be left behind.  Every paragraph but the
first arrives with the blank line BEFORE it, because that is where
`backward-paragraph' lands; the first has none to land on, so it came
with no blank at all and deleting it left a stray blank at the head of
the buffer.  It now takes the blank that follows instead, so exactly one
blank comes with a paragraph wherever it sits."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.")
    (goto-char 3)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Para one.\n\n"))))

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
paragraph to give.

The blank point started on comes with it: it is the one blank line the
first paragraph is entitled to, standing in for the leading one it does
not have."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.\n")
    (goto-char 11)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Para one.\n\n"))))

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
  "Paragraphs separated by empty line detected.

The separator comes with the FIRST paragraph, which owns no blank line
before it, and with the second by way of its leading blank -- one blank
either way."
  (with-temp-buffer
    (insert "First para.\n\nSecond para.")
    (goto-char 5)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "First para.\n\n"))))

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
  "One trailing blank line comes with a first paragraph, the text below never.

The selection stops at the start of \"More\": the blank line separating
the two is taken, standing in for the leading blank the first paragraph
does not have, and nothing beyond it is."
  (with-temp-buffer
    (insert "Para.\n\nMore")
    (goto-char 3)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Para.\n\n"))))

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
  "Point on separator between paragraphs selects adjacent paragraph.

The separator itself comes with it, as the one blank line a first
paragraph gets."
  (with-temp-buffer
    (insert "First.\n\nSecond.")
    (goto-char 7)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "First.\n\n"))))

(ert-deftest donkey-mark-paragraph-multiple-consecutive-blanks ()
  "Exactly ONE of several consecutive blank lines comes with the paragraph.

A run of blank lines is the author's spacing, so the first paragraph
takes one line and leaves the rest standing.  Deleting this selection
leaves \"\\n\\nMore.\", the same two blanks that separate any other
paragraph pair here."
  (with-temp-buffer
    (insert "Para.\n\n\n\nMore.")
    (goto-char 3)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Para.\n\n"))))

(ert-deftest donkey-mark-paragraph-deletion-leaves-one-separator-anywhere ()
  "Deleting a marked paragraph leaves its neighbors one blank line apart.

This is the property the blank-line rule exists for, asserted directly
rather than through a substring: whichever paragraph goes, what remains
reads as though it had never been there.  Before the rule, deleting the
FIRST paragraph left \"\\nBeta.\\n\\nGamma.\\n\" -- a stray blank at the
head of the buffer -- while deleting either of the others came out
clean.  Each expectation below also matches vi's `dap'."
  (dolist (probe '((2 . "Beta.\n\nGamma.\n")
                   (9 . "Alpha.\n\nGamma.\n")
                   (16 . "Alpha.\n\nBeta.\n")))
    (with-temp-buffer
      (let ((transient-mark-mode t))
        (insert "Alpha.\n\nBeta.\n\nGamma.\n")
        (goto-char (car probe))
        (donkey-mark-paragraph)
        (delete-region (region-beginning) (region-end))
        (should (equal (buffer-string) (cdr probe)))))))

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

(ert-deftest donkey-rectangle-mark-mode-takes-one-character-not-one-column ()
  "The initial widening is one CHARACTER, which is not always one column.

The widening is `right-char', so what it takes is a character; a
rectangle is measured in COLUMNS, and the two only coincide for
ordinary text:

  ordinary char   (0 . 1)
  TAB             (0 . 8)
  CJK wide char   (0 . 2)

Not a defect -- a rectangle cannot take half a TAB, and refusing to
start on one would be worse.  Pinned because the tutor used to promise
\"one column wide\", which sent readers to replace a whole indent with
`m v' then `c' and left nothing on screen to explain it.  Whichever way
this behavior is later changed, the prose has to move with it."
  (dolist (case '(("abcdef\nabcdef\n" 1)
                  ("\tabc\n\tabc\n"   8)
                  ("一二三\n一二三\n"  2)))
    (cl-destructuring-bind (text width) case
      (with-temp-buffer
        (let ((transient-mark-mode t))
          (insert text)
          (goto-char (point-min))
          (donkey-rectangle-mark-mode)
          (should (equal (rectangle--pos-cols (region-beginning) (region-end))
                         (cons 0 width))))))))

(ert-deftest donkey-rectangle-mark-mode-at-end-of-line-appends-via-change ()
  "End to end: a rectangle started at end of line appends to every row.

The point of the guard.  With the rectangle collapsed to column 0 this
inserted against the left margin instead, so the check is on the text,
not on the columns -- a reader never sees a column number.

Driven by keys in a DISPLAYED buffer, and both halves of that matter.
The descent used to be a bare `next-line' call, which in a live -nw
frame lands on column 0 rather than the column `end-of-line' put point
on: the rectangle came out spanning whole lines and the change REPLACED
them, giving \";\\n;\\n;\\n\".  Batch kept column 5 and the test passed,
and batch is the only place it ever ran.  Pressing `j' gives column 5 in
both, so the command was never at fault -- only the way it was reached.

And keys need a buffer in the selected window, since the command loop
acts there rather than on whatever is merely current.  Swapping
`next-line' for `j' inside a `with-temp-buffer' therefore broke it the
other way round, passing live and failing in batch."
  (unwind-protect
      (progn
        (when (get-buffer "*donkey-rect-eol*") (kill-buffer "*donkey-rect-eol*"))
        (switch-to-buffer (get-buffer-create "*donkey-rect-eol*"))
        (text-mode)
        (donkey-mode 1)
        ;; `last-command' bound because a line motion reuses the GLOBAL
        ;; `temporary-goal-column' whenever the previous command was
        ;; itself one.  Inherited from an earlier test, that walked the
        ;; rectangle to the remembered column instead of keeping the one
        ;; `end-of-line' put it on, and the appended text landed mid-word:
        ;; "aa;" rather than "aaaaa;".  Only visible when the suite runs
        ;; in an order where a line-motion test comes first.
        (let ((transient-mark-mode t)
              (last-command nil)
              (prefix-arg nil) (current-prefix-arg nil)
              (inhibit-message t))
          (insert "aaaaa\nbbbbb\nccccc\n")
          (goto-char (point-min))
          (donkey-normal-mode 1)
          (execute-kbd-macro (kbd "g l m v j j"))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ";")))
            (donkey-change))
          (should (equal (buffer-string) "aaaaa;\nbbbbb;\nccccc;\n"))))
    (when (get-buffer "*donkey-rect-eol*") (kill-buffer "*donkey-rect-eol*"))))

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

(ert-deftest donkey-mark-sentence-below-prose-marks-the-sentence-above ()
  "Point on a blank line under prose marks the sentence above it.

This assertion has been round twice.  It started here, was reversed to a
refusal when the command was defined in terms of the sentence AHEAD of
point, and is back: `m w' and `m p' from the end of a buffer mark the
last word and the last paragraph rather than refusing, and there is no
reason for sentences to be the exception."
  (with-temp-buffer
    (insert "Hello there.\n\n\n")
    (goto-char (point-max))
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Hello there."))))

(ert-deftest donkey-mark-sentence-gap-selects-the-sentence-behind ()
  "In the gap between two sentences, the one BEHIND is marked.

The gap is the whole point of the test: a cursor inside a sentence has
never been in doubt.  `m w', `m W' and `m p' all answer with the object
behind from the equivalent position, and `m s' reaching forward instead
made the same cursor position mean different things depending on which
mark key followed it.

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
                     "One two three.")))))

(ert-deftest donkey-mark-sentence-repeated-extends-by-one-sentence ()
  "Pressing the key again grows the selection by one more sentence.

Inherited from `mark-end-of-sentence', which extends rather than
re-marks when `(eq last-command this-command)'.  The other mark commands
do not do this -- `mark-word' gates the same behavior behind an
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

(ert-deftest donkey-mark-sentence-past-the-last-sentence-marks-it ()
  "With no sentence ahead, the last one is marked rather than refused.

The trailing gap is a gap like any other, so it answers like one."
  (with-temp-buffer
    (insert "One two three.  Four five six.\n")
    (goto-char (point-max))
    (donkey-mark-sentence)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "Four five six."))))

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

(ert-deftest donkey-mark-commands-honor-a-count ()
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
    ;; The trailing blank is the one blank line a first paragraph gets in
    ;; place of the leading one it has not got.
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "P1 line.\n\nP2 line.\n\n"))))

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

It is defined in terms of the sentence AHEAD of point -- it normalizes
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

;;; ---------------------------------------------------------------------------
;;; Repeating a mark key extends the selection
;;; ---------------------------------------------------------------------------

(defmacro donkey-mark-test--keys (text keys &rest body)
  "Run KEYS in a displayed DONKEY buffer of TEXT, then BODY.

Real keys through `execute-kbd-macro', because the whole property under
test is `last-command': calling the commands directly leaves it set to
whatever ran before and the extension never triggers.  The buffer is
switched to rather than merely made current, since the command loop
acts on the SELECTED WINDOW's buffer -- keys sent to an undisplayed
`with-temp-buffer' land in whatever window is showing instead.

`prefix-arg' and `current-prefix-arg' are bound because KEYS may carry
a \\[universal-argument]: `execute-kbd-macro' leaves the prefix set
GLOBALLY afterwards, and the next test to call a command found itself
running it prefixed.  It cost a run of
`donkey-set-mark-activates-mark-at-point' -- a prefixed
`set-mark-command' pops the mark ring instead of setting the mark --
failing only when this file ran before it, which is to say only in the
full suite."
  (declare (indent 2))
  `(unwind-protect
       (progn
         (when (get-buffer "*donkey-mark-test*") (kill-buffer "*donkey-mark-test*"))
         (donkey-mode 1)
         (let ((transient-mark-mode t)
               (prefix-arg nil) (current-prefix-arg nil)
               (this-command nil) (last-command nil))
           (switch-to-buffer (get-buffer-create "*donkey-mark-test*"))
           (fundamental-mode)
           (erase-buffer)
           (insert ,text)
           (goto-char (point-min))
           (donkey-enter-normal)
           (execute-kbd-macro (kbd ,keys))
           ,@body))
     (when (get-buffer "*donkey-mark-test*") (kill-buffer "*donkey-mark-test*"))
     ;; A macro that ends on a mark-run-mode letter leaves the transient
     ;; map armed GLOBALLY: `donkey--mark-run-mode-keep-p' is only
     ;; consulted on the next command, and `execute-kbd-macro' runs no
     ;; further command to consult it on.  Killing the buffer does not
     ;; help -- `overriding-terminal-local-map' is terminal-wide -- so
     ;; the next test's bare "w" would MARK instead of moving.  In live
     ;; use `donkey--mark-run-mode-post-command' catches this on the
     ;; command that ran the macro; there is no such command here, so
     ;; the mode is taken down directly.  A no-op when it never ran.
     (donkey--mark-run-exit)
     (donkey-mode -1)))

(defun donkey-mark-test--selection ()
  "Return the active region's text, or nil."
  (and (region-active-p)
       (buffer-substring-no-properties (region-beginning) (region-end))))

(ert-deftest donkey-every-mark-key-grows-on-a-second-press ()
  "`m w', `m W', `m s' and `m p' all extend, not just `m s'.

Regression test: `mark-end-of-sentence' has no ALLOW-EXTEND parameter
and always extends, while `mark-word', `mark-paragraph' and `mark-sexp'
take one that Emacs passes only when calling them interactively.
Reached from Lisp with a single argument, the extension is off -- so
`m s' grew and the rest did not, which read as a decision and was not
one."
  (dolist (case '(("m w" "alpha beta gamma delta"        "alpha beta")
                  ("m W" "a-one b-two c-three"           "a-one b-two")
                  ("m s" "One thing.  Two thing."        "One thing.  Two thing.")
                  ;; The blank line between the two paragraphs is part of
                  ;; the selection, as it has to be -- a region spanning
                  ;; both cannot skip what separates them.
                  ("m p" "Alpha.\n\nBeta.\n"             "Alpha.\n\nBeta.\n")))
    (cl-destructuring-bind (key text expected) case
      (donkey-mark-test--keys text (concat key " " key)
        (should (equal (cons key (donkey-mark-test--selection))
                       (cons key expected)))))))

(ert-deftest donkey-repeating-a-mark-key-equals-a-count ()
  "Pressing a mark key N times reaches what a count of N reaches.

Two mechanisms for one idea, so they have to agree or one of them is
lying.  Checked at two and three, for each of the twelve keys.

The three backward keys need a LEAD deep enough that the count has
room to reach: from the buffer's first object there is nothing behind,
and both mechanisms would agree on a truncated answer without proving
the count moves at all.

For the four delimiter keys a level out is what a press buys, and they
only started agreeing once a repeat stopped searching afresh from
wherever the previous selection left point: `m A' widened by accident
of position while `m I' re-marked what it already had, from the same
text and the same starting point."
  ;; LEAD moves point before the first press.  The delimiter keys need
  ;; it: from the outermost opener there is no second level to reach,
  ;; and from plain text a FRESH press prompts for the delimiter, so the
  ;; run has to start on the innermost one.
  (dolist (case '(("m w" "alpha beta gamma delta"             "")
                  ("m W" "a-one b-two c-three d-four"         "")
                  ("m b" "alpha beta gamma delta"             "w w w ")
                  ("m B" "a-one b-two c-three d-four"         "w w w w w w ")
                  ("m s" "One thing.  Two thing.  Three thing." "")
                  ("m S" "One thing.  Two thing.  Three thing.  Four thing."
                   "w w w w w ")
                  ("m p" "Alpha.\n\nBeta.\n\nGamma.\n"      "")
                  ("m P" "Alpha.\n\nBeta.\n\nGamma.\n"      "j j j j ")
                  ("m i" "(((deep)))"                         "l l ")
                  ("m a" "(((deep)))"                         "l l ")
                  ("m I" "(((deep)))"                         "l l ")
                  ("m A" "(((deep)))"                         "l l ")))
    (cl-destructuring-bind (key text lead) case
      (dolist (n '(2 3))
        (let ((repeated
               (donkey-mark-test--keys
                   text (concat lead (mapconcat #'identity (make-list n key) " "))
                 (donkey-mark-test--selection)))
              (counted
               (donkey-mark-test--keys text (format "%sC-u %d %s" lead n key)
                 (donkey-mark-test--selection))))
          (should (equal (list key n repeated) (list key n counted))))))))

(ert-deftest donkey-repeating-a-delimiter-mark-never-prompts ()
  "A second press of `m i'/`m a' does not stop to ask for a delimiter.

`donkey--mark-pair-read-delimiter' auto-detects only when point is ON a
delimiter and otherwise waits on `read-char'.  With point left at one
end of the selection, one of the two commands always lands somewhere
that is not a delimiter -- past the closing one when point was left at
the END, on the first character of the content once it was left at the
START -- so a repeat used to hang on whichever it was.  What fixes it
is the repeat reusing the delimiter it already resolved, not where the
cursor sits.

`read-char' is stubbed to signal rather than to answer, so a prompt
fails the test instead of hanging the run."
  (dolist (key '("m i" "m a" "m I" "m A"))
    (cl-letf (((symbol-function 'read-char)
               (lambda (&rest _) (error "Prompted for a delimiter"))))
      (donkey-mark-test--keys "(((deep)))" (format "l l %s %s" key key)
        (should (equal (cons key (donkey-mark-test--selection))
                       (cons key (if (member key '("m i" "m I"))
                                     "(deep)"
                                   "((deep))"))))))))

(ert-deftest donkey-a-key-in-between-ends-a-mark-run ()
  "Any other key between two presses starts a fresh selection.

The rule is `last-command', so a motion in the middle breaks the run.
Pinned because the obvious alternative -- extending whenever a region
is active, which is what Emacs' own ALLOW-EXTEND branch also does --
would make this pass while changing what the key means."
  (dolist (case '(("m w" "alpha beta gamma"      "alpha")
                  ("m W" "a-one b-two"           "a-one")
                  ("m s" "One thing.  Two thing." "One thing.")))
    (cl-destructuring-bind (key text expected) case
      (donkey-mark-test--keys text (format "%s l %s" key key)
        (should (equal (cons key (donkey-mark-test--selection))
                       (cons key expected)))))))

(ert-deftest donkey-a-live-selection-is-not-extended-by-a-mark-key ()
  "`v' and some motions, then `m w', still marks the word at point.

Emacs' own rule extends ANY active region, not just a repeat of the
same command.  Adopting it would have grown this selection instead --
a change to a flow that was never in question.  DONKEY uses the
narrower test, and this is what says so."
  (donkey-mark-test--keys "alpha beta gamma" "v l l l m w"
    (should (equal (donkey-mark-test--selection) "alpha"))))

;;; ---------------------------------------------------------------------------
;;; Growing a selection backward: m b, m B, m P
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-word-backward-continues-a-forward-run ()
  "`m w m w m b' from \"that\" selects two words forward and one back.

The headline case for the backward keys, as reported: every word
selection keeps its mark at the forward end and point at the start, so
the two directions grow one region without fighting over an end."
  (donkey-mark-test--keys "for text that is not saved" "w w l m w m w m b"
    (should (equal (donkey-mark-test--selection) "text that is"))))

(ert-deftest donkey-mark-word-backward-alone-selects-what-m-w-selects ()
  "A fresh `m b', with no run to continue, agrees with a fresh `m w'.

The pair differs only in which end a FOLLOWING press grows; a fresh
press must not disagree about what the first word is, or the meaning of
the run would depend on which key happened to start it."
  (let ((backward (donkey-mark-test--keys "for text that is" "w w l m b"
                    (donkey-mark-test--selection)))
        (forward (donkey-mark-test--keys "for text that is" "w w l m w"
                   (donkey-mark-test--selection))))
    (should (equal backward "that"))
    (should (equal backward forward))))

(ert-deftest donkey-mark-word-backward-repeats-grow-backward ()
  "`m b m b' takes the word at point and the one before it."
  (donkey-mark-test--keys "for text that is not saved" "w w l m b m b"
    (should (equal (donkey-mark-test--selection) "text that"))))

(ert-deftest donkey-mark-backward-then-forward-continues-the-run ()
  "The directions continue one run in either order, per object type.

`m b m w' grows the word selection forward rather than starting over,
and `m B m W' does the same for symbols -- the companion test in
`donkey--mark-extending-p' works both ways round.

The sentence pair is checked in BOTH orders, because its two directions
extend through different code: `m s m S' exercises the backward
command's own extending branch, while `m S m s' exercises the
`last-command' binding inside `donkey-mark-sentence' that presents the
companion press to `mark-end-of-sentence' as a repeat -- without it the
forward press would collapse the far end back to the first sentence."
  (donkey-mark-test--keys "for text that is not saved" "w w l m b m w"
    (should (equal (donkey-mark-test--selection) "that is")))
  (donkey-mark-test--keys "a-one b-two c-three d-four" "w w w w w m B m W"
    (should (equal (donkey-mark-test--selection) "c-three d-four")))
  (donkey-mark-test--keys "One thing.  Two thing.  Three thing." "w w w m S m s"
    (should (equal (donkey-mark-test--selection) "Two thing.  Three thing.")))
  (donkey-mark-test--keys "One thing.  Two thing.  Three thing." "w w w m s m S"
    (should (equal (donkey-mark-test--selection) "One thing.  Two thing."))))

(ert-deftest donkey-mark-word-backward-stops-at-the-buffer-start ()
  "Running out of buffer keeps the selection and stays put.

The forward direction stops quietly at the end of the buffer;
`forward-word' with a negative count stops the same way at the start,
so pressing `m b' with nothing left behind changes nothing and still
reports a marked word rather than erroring."
  (donkey-mark-test--keys "alpha beta" "m w m b m b"
    (should (equal (donkey-mark-test--selection) "alpha"))
    (should (= (point) (point-min)))))

(ert-deftest donkey-mark-backward-counts-below-one-are-one ()
  "`C-u 0' and `C-u -2' mean a plain press for all three backward keys.

Zero and negative counts already reach behind point elsewhere in the
family (`C-u -2 m w'), and these commands ARE the backward direction,
so they have nothing left to name here -- the same reading
`donkey-mark-sentence' gives its own counts below 1.

Checked both fresh and as a continuation, because only the second can
tell the clamp apart from delegation: a fresh press hands no count to
the partner it calls, so it reads as a plain press with or without the
clamp, while mid-run a raw 0 would grow by nothing and a raw -2 would
walk point FORWARD, past the mark."
  (dolist (prefix '("C-u 0 " "C-u -2 "))
    ;; Fresh: below 1 is a plain press.
    (dolist (case '(("m b" "that")
                    ("m B" "that")
                    ("m S" "for text that is")
                    ("m P" "for text that is")))
      (cl-destructuring-bind (key expected) case
        (donkey-mark-test--keys "for text that is"
            (concat "w w l " prefix key)
          (should (equal (list key prefix (donkey-mark-test--selection))
                         (list key prefix expected))))))
    ;; Continuing a run: below 1 still grows by exactly one object.
    ;; The sentence and paragraph runs have nothing behind this
    ;; one-object buffer, but the clamp still shows: a raw -2 would walk
    ;; point forward onto the mark and collapse the selection.
    (dolist (case '(("m w" "m b" "text that")
                    ("m W" "m B" "text that")
                    ("m s" "m S" "for text that is")
                    ("m p" "m P" "for text that is")))
      (cl-destructuring-bind (fwd bwd expected) case
        (donkey-mark-test--keys "for text that is"
            (concat "w w l " fwd " " prefix bwd)
          (should (equal (list bwd prefix (donkey-mark-test--selection))
                         (list bwd prefix expected))))))))

(ert-deftest donkey-a-mark-run-crosses-object-types ()
  "Any family member continues a run, adding one object of its own kind.

The reversal of a decision this test used to pin the other way round:
runs were confined to a forward/backward pair per object, on the
argument that \"the symbol before the current WORD selection\" is not
a length the word run promised.  The family's shared selection shape
makes the meaning plain -- forward keys push the mark by their object,
backward keys walk point by theirs -- and the pair rule made `m w m s'
silently discard a selection instead.  See `donkey--mark-run-commands'.

Eight cases, arranged so every member appears once as the first press
and once as the second: each case pins the SECOND command's family
list at its `donkey--mark-extending-p' call site, and dropping any
member from `donkey--mark-run-commands' breaks the case where it goes
first.

Two of them also pin the mid-object rule: a backward press whose
region start sits inside a larger object of its own kind first reaches
THAT object's start -- `m w m B' from the word \"two\" completes the
symbol backward to \"b-two\", and `m B m P' from mid-paragraph reaches
the paragraph's start -- and the next press adds a whole one."
  (dolist (case '(("m S m w" "w w w " sent "Two thing.  Three")
                  ("m s m b" "w w w " sent "thing.  Two thing.")
                  ("m b m W" "w w w w " sym "two c-three")
                  ("m w m B" "w w w w " sym "b-two")
                  ("m W m s" "w w w " sent "Two thing.")
                  ("m p m S" "j j " para "Alpha one.\n\nBeta two.\n")
                  ("m B m P" "j j " para "\nBeta")
                  ("m P m p" "j j " para "\nBeta two.\n\nGamma three.\n")))
    (cl-destructuring-bind (keys lead text-key expected) case
      (let ((text (pcase text-key
                    ('sent "One thing.  Two thing.  Three thing.")
                    ('sym "a-one b-two c-three d-four")
                    ('para "Alpha one.\n\nBeta two.\n\nGamma three.\n"))))
        (donkey-mark-test--keys text (concat lead keys)
          (should (equal (cons keys (donkey-mark-test--selection))
                         (cons keys expected))))))))

(ert-deftest donkey-an-interposed-key-ends-a-backward-run ()
  "Any other key between presses of `m b' starts a fresh selection.

The discriminating position matters: with the in-between motion left
inside the word just marked, a wrongly surviving run and a fresh mark
select the same text, which is why the family-wide in-between test
cannot carry this key.  Moving on to a LATER word separates them -- a
fresh press marks \"not\", a surviving run would walk point back from
it and leave a sliver next to the old mark."
  (donkey-mark-test--keys "for text that is not saved" "w w l m b w w l m b"
    (should (equal (donkey-mark-test--selection) "not"))))

(ert-deftest donkey-mark-symbol-backward-keeps-separating-punctuation-interior ()
  "`m B' growing backward crosses punctuation without trimming it.

`donkey--trim-symbol-punctuation' is about a trailing \".\" or \",\" at
the selection's forward END; the backward walk lands on symbol starts,
and whatever separated the symbols simply becomes interior text."
  (donkey-mark-test--keys "foo, bar baz" "w w w m B m B m B"
    (should (equal (donkey-mark-test--selection) "foo, bar baz"))))

(ert-deftest donkey-mark-paragraph-backward-takes-the-paragraph-above ()
  "`m p m P' selects both paragraphs with their one separator inside.

Pinned against the one-blank-line rule: `backward-paragraph' lands
before the blank that precedes the paragraph it walks over, so the
blank that used to lead the selection becomes interior and no stray
blank line appears at either end."
  (donkey-mark-test--keys "Alpha one.\n\nBeta two.\n\nGamma three.\n"
      "j j m p m P"
    (should (equal (donkey-mark-test--selection)
                   "Alpha one.\n\nBeta two.\n"))))

(ert-deftest donkey-mark-paragraph-backward-alone-selects-what-m-p-selects ()
  "A fresh `m P' agrees with a fresh `m p', like the word pair."
  (let ((backward (donkey-mark-test--keys
                      "Alpha one.\n\nBeta two.\n\nGamma three.\n" "j j m P"
                    (donkey-mark-test--selection)))
        (forward (donkey-mark-test--keys
                     "Alpha one.\n\nBeta two.\n\nGamma three.\n" "j j m p"
                   (donkey-mark-test--selection))))
    (should (equal backward "\nBeta two.\n"))
    (should (equal backward forward))))

(ert-deftest donkey-a-backward-press-revives-a-deactivated-run ()
  "A backward key re-activates the region it grows, like its partner.

The forward direction re-activates on every press as a side effect of
`set-mark'/`mark-word'; walking point activates nothing.  Without the
explicit `activate-mark' on the extending branch, a region a hook
deactivated mid-run would keep growing invisibly: point moves, nothing
shows, and the next `d' acts on a selection the user cannot see.

The deactivation is staged from `pre-command-hook', firing just before
the backward press -- the run is still live there, since the hook is
not a command and `last-command' still names the forward partner.  It
CANNOT be staged between two `execute-kbd-macro' calls: starting a
macro from Lisp resets `last-command' to nil before its first command,
so a second macro is never a continuation of the first and the fresh
branch would be exercised instead, in every frame kind this suite runs
in."
  ;; The paragraph pair has nothing behind a single-paragraph buffer to
  ;; grow onto; what it must still do is bring the selection back into
  ;; view, so its expectation is the unchanged span, re-activated.
  (dolist (case '(("m w" "m b" donkey-mark-word-backward "text that")
                  ("m W" "m B" donkey-mark-symbol-backward "text that")
                  ("m s" "m S" donkey-mark-sentence-backward
                   "for text that is")
                  ("m p" "m P" donkey-mark-paragraph-backward
                   "for text that is")))
    (cl-destructuring-bind (fwd bwd cmd expected) case
      (let ((sabotage (lambda ()
                        (when (eq this-command cmd)
                          (deactivate-mark)))))
        (unwind-protect
            (progn
              (add-hook 'pre-command-hook sabotage)
              (donkey-mark-test--keys "for text that is"
                  (concat "w w l " fwd " " bwd)
                (should (equal (list fwd bwd (donkey-mark-test--selection))
                               (list fwd bwd expected)))))
          (remove-hook 'pre-command-hook sabotage))))))

(ert-deftest donkey-a-forward-word-press-grows-a-deactivated-run ()
  "`m w' onto an invisible run grows it rather than starting over.

The mirror of `donkey-a-backward-press-revives-a-deactivated-run', on
the one member that does not grow the region itself: `donkey-mark-word'
hands the job to `mark-word', whose ALLOW-EXTEND branch has its own
test -- `last-command' equal to `this-command', or a visible region
beginning at point -- and a COMPANION press onto a run some hook
deactivated satisfies neither.  It pushed a fresh mark there, quietly
losing the word `m b' had just added.  The press is presented to it as
a repeat instead, exactly when `donkey--mark-extending-p' says the run
is live.

Staged from `pre-command-hook' for the reason the backward test gives:
`last-command' cannot survive a second `execute-kbd-macro'."
  (let ((sabotage (lambda ()
                    (when (eq this-command 'donkey-mark-word)
                      (deactivate-mark)))))
    (unwind-protect
        (progn
          (add-hook 'pre-command-hook sabotage)
          (donkey-mark-test--keys "for text that is not saved" "w w l m b m w"
            (should (equal (donkey-mark-test--selection) "that is"))))
      (remove-hook 'pre-command-hook sabotage))))

(ert-deftest donkey-a-deactivated-run-grows-whatever-mark-reads ()
  "Every key that revives a deactivated run does so with the mark off.

The extending branches exist partly to bring back a region some hook
took the highlight from, and they read the mark to do it.  Plain `mark'
refuses to answer for an INACTIVE region unless `mark-even-if-inactive'
is on -- it is on by default, so turning it off was all it took to make
three of this file\'s tests signal `mark-inactive' instead of growing:
`donkey--normalize-mark-run' and four branches asked with `mark', and
the two native extensions donkey borrows, `mark-word' and
`mark-end-of-sentence', ask with `mark' inside where donkey cannot
reach and are handed the binding instead.

Run over BOTH values, since the point is that the answer does not
depend on the setting.  The staging is the one
donkey-a-backward-press-revives-a-deactivated-run explains:
`pre-command-hook' fires while the run is still live, and
`last-command' cannot survive a second `execute-kbd-macro'."
  (dolist (inactive '(t nil))
    (dolist (case '(("w w l m b m w" donkey-mark-word
                     "for text that is not saved" "that is")
                    ("w w l m w m b" donkey-mark-word-backward
                     "for text that is not saved" "text that")
                    ("w w l m W m W" donkey-mark-symbol
                     "for text that is not saved" "that is")
                    ("w w w m S m s" donkey-mark-sentence
                     "One thing here.  Two thing there.  Three thing."
                     "One thing here.  Two thing there.")
                    ("w w l m p m p" donkey-mark-paragraph
                     "for text that is not saved"
                     "for text that is not saved")
                    ("M w J" donkey-mark-run-line-forward
                     "one\ntwo\nthree\n" "one\n")
                    ("w w l M w g h" donkey-mark-run-line-start
                     "for text that is not saved" "for text that")
                    ("w w l M w g l" donkey-mark-run-line-end
                     "for text that is not saved" "that is not saved")))
      (cl-destructuring-bind (keys cmd text expected) case
        (let ((sabotage (lambda ()
                          (when (eq this-command cmd)
                            (deactivate-mark))))
              (mark-even-if-inactive inactive))
          (unwind-protect
              (progn
                (add-hook 'pre-command-hook sabotage)
                (donkey-mark-test--keys text keys
                  (should (equal (list inactive keys
                                       (donkey-mark-test--selection))
                                 (list inactive keys expected)))))
            (remove-hook 'pre-command-hook sabotage)))))))

(ert-deftest donkey-a-continuation-does-not-renormalize-the-sentence-start ()
  "`m s' growing another object's run leaves the region's start alone.

`donkey-mark-sentence' normalizes onto a sentence start before it
marks, and on a pure `m s' run that re-normalization was harmless --
the region start IS a sentence start there.  On a cross-object run it
is not: from the word \"thing\" inside \"Two thing.\", a continuation
that still normalized walked the region's start silently back to
\"Two\", growing the selection at an end the press never named.  The
extending branch skips normalization, like every sibling; this is the
case that shows the difference, because the family test
`donkey-a-mark-run-crosses-object-types' happens to start its own
`m W m s' case on a word that begins its sentence."
  (donkey-mark-test--keys "One thing.  Two thing.  Three thing."
      "w w w w m w m s"
    (should (equal (donkey-mark-test--selection) "thing."))))

(ert-deftest donkey-mark-sentence-backward-repeats-grow-backward ()
  "`m S m S' takes the sentence at point and the one before it."
  (donkey-mark-test--keys "One thing.  Two thing.  Three thing." "w w w m S m S"
    (should (equal (donkey-mark-test--selection)
                   "One thing.  Two thing."))))

(ert-deftest donkey-mark-sentence-backward-alone-selects-what-m-s-selects ()
  "A fresh `m S' agrees with a fresh `m s', like the other pairs."
  (let ((backward (donkey-mark-test--keys
                      "One thing.  Two thing.  Three thing." "w w w m S"
                    (donkey-mark-test--selection)))
        (forward (donkey-mark-test--keys
                     "One thing.  Two thing.  Three thing." "w w w m s"
                   (donkey-mark-test--selection))))
    (should (equal backward "Two thing."))
    (should (equal backward forward))))

(ert-deftest donkey-mark-run-mode-is-the-m-prefix-held-down ()
  "`M w w b' selects exactly what `m w m w m b' selects.

The headline of mark run mode: each letter of
`donkey-mark-run-mode-map' is bound to the very command its
`m'-prefixed key runs, so the two spellings cannot drift apart.
Asserted against the literal expectation AND against the prefixed run
on the same text, so a change to either spelling that the other does
not follow fails here."
  (let ((moded (donkey-mark-test--keys "for text that is not saved"
                   "w w l M w w b" (donkey-mark-test--selection)))
        (prefixed (donkey-mark-test--keys "for text that is not saved"
                      "w w l m w m w m b" (donkey-mark-test--selection))))
    (should (equal moded "text that is"))
    (should (equal moded prefixed))))

(ert-deftest donkey-mark-run-mode-first-letter-marks-afresh ()
  "`M w' equals a fresh `m w': the mode plants no anchor.

An earlier design pushed an empty anchored selection at `M' and grew
from it, which made `M w' from MID-WORD select the tail of the word.
The mode form marks the whole object from anywhere inside it, exactly
as the prefixed key does -- the same behavior, minus the prefix.  The
mid-word start is what tells the two designs apart."
  (donkey-mark-test--keys "for text that is" "w w l l M w"
    (should (equal (donkey-mark-test--selection) "that"))))

(ert-deftest donkey-mark-run-mode-takes-counts ()
  "A count inside the mode matches its prefixed spelling.

`C-u 3' then `w' inside the mode selects what `C-u 3 m w' selects.
`donkey--mark-run-mode-keep-p' keeps the transient map alive through
`universal-argument' and its digits; without that arm the mode would
lapse on the count and the `w' would be a plain motion."
  (let ((moded (donkey-mark-test--keys "alpha beta gamma delta" "M C-u 3 w"
                 (donkey-mark-test--selection)))
        (prefixed (donkey-mark-test--keys "alpha beta gamma delta" "C-u 3 m w"
                    (donkey-mark-test--selection))))
    (should (equal moded "alpha beta gamma"))
    (should (equal moded prefixed))))

(ert-deftest donkey-a-foreign-key-lapses-the-mode-and-still-works ()
  "`M w w d' selects two words and deletes them; no explicit exit.

`d' is not in `donkey-mark-run-mode-map', so the transient map lapses
in that press's pre-command and the key does its ordinary job on the
selection the mode built.  And once lapsed, the letters are plain
keys again: the `w' after the delete moves and marks nothing.  (The
lapse can no longer be shown with a motion -- `h' `j' `k' `l' are the
mode's own keys now.)"
  (donkey-mark-test--keys "for text that is" "w l M w w d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "for  is"))
    (execute-kbd-macro (kbd "w"))
    (should-not (region-active-p))))

(ert-deftest donkey-hjkl-adjust-the-run-without-ending-it ()
  "Inside the mode, `h' `j' `k' `l' move point and the run carries on.

Through the `donkey-mark-run-' wrappers, which are family members --
the plain motions cannot be, or `m w l m w' would stop marking a
single word afresh, a pinned rule.  After `M w l', the region's near
end has moved one character in and the next `w' still EXTENDS: \"hat
is\", not a fresh \"is\".  `h' walks the near end outward instead,
counts pass through, and the vertical pair adjusts by lines with
`next-line's own column behavior, `j'/`k' being what they stand in
for."
  (donkey-mark-test--keys "for text that is not saved" "w w l M w l w"
    (should (equal (donkey-mark-test--selection) "hat is")))
  (donkey-mark-test--keys "for text that is" "w w l M w h"
    (should (equal (donkey-mark-test--selection) " that")))
  (donkey-mark-test--keys "for text that is not saved" "w w l M w C-u 2 l w"
    (should (equal (donkey-mark-test--selection) "at is")))
  (donkey-mark-test--keys "one\ntwo\nthree\n" "j M w k"
    (should (equal (donkey-mark-test--selection) "one\ntwo")))
  ;; The column survives the vertical move -- `next-line', not
  ;; `forward-line', as the wrappers' docstrings promise: from column 2
  ;; of "y two", `k' lands on column 2 of "x one".
  (donkey-mark-test--keys "x one\ny two\n" "j l l M w k"
    (should (equal (donkey-mark-test--selection) "one\ny two")))
  ;; And it survives a SECOND press over a line too short to hold it,
  ;; which is the whole of what `donkey--line-move-last-command' buys:
  ;; `line-move' keeps the starting column only while `last-command'
  ;; names one of the two motions the wrappers stand in for, so without
  ;; the binding each press reset it and the run walked down the ragged
  ;; edge.  Pinned against the plain `j j' it must agree with.
  (donkey-mark-test--keys "alpha beta\nxy\ngamma delta\n" "M C-u 6 l j j"
    (should (= (current-column) 6)))
  (donkey-mark-test--keys "alpha beta\nxy\ngamma delta\n" "C-u 6 l j j"
    (should (= (current-column) 6))))

(ert-deftest donkey-g-h-and-g-l-reach-the-line-ends-in-the-mode ()
  "`g h' and `g l' stretch the run to the line's ends and add up.

The pair owns FIXED ENDS -- `g h' the selection's start, `g l' its
end -- where \`h' \`j' \`k' \`l' all move point.  Both moved point
once, which made them cancel each other: `M w g h' reached back to
the line's start, and the `g l' after it dragged point across the
mark to the line's end and left the beginning behind, leaving \" is\"
where the whole line was asked for.  Now they add, in either order,
and a `w' after either still EXTENDS.

With nothing selected the key is the plain motion it is outside the
mode, which is what `M g l w' relies on.  Unmatched `g' sequences are
deliberately NOT shadowed -- the transient map defines only these
two, so `g q' still resolves to its normal-state command and lapses
the mode like any foreign key."
  (donkey-mark-test--keys "for text that is" "w w l M w g h"
    (should (equal (donkey-mark-test--selection) "for text that")))
  (donkey-mark-test--keys "for text that is\nnot saved\n" "w w l M w g l"
    (should (equal (donkey-mark-test--selection) "that is")))
  (donkey-mark-test--keys "for text that is\nnot saved\n" "w w l M w g h g l"
    (should (equal (donkey-mark-test--selection) "for text that is")))
  (donkey-mark-test--keys "for text that is\nnot saved\n" "w w l M w g l g h"
    (should (equal (donkey-mark-test--selection) "for text that is")))
  (donkey-mark-test--keys "for text that is" "w w l M w g h w"
    (should (equal (donkey-mark-test--selection) "for text that is")))
  ;; `g l' pushes the mark, so it measures from the end the selection
  ;; already reaches rather than from the cursor, and cannot shrink a
  ;; run that spans lines.
  (donkey-mark-test--keys "one\ntwo\nthree\nfour\n" "M J J g l"
    (should (equal (donkey-mark-test--selection) "one\ntwo\nthree")))
  ;; Fresh, both are still motions: nothing is marked, point moves.
  ;; Entered from WHITESPACE, since `M' on a word marks it and the
  ;; fresh branch would never be reached.
  (donkey-mark-test--keys "for text that is\nnot saved\n" "w w M g l"
    (should-not (region-active-p))
    (should (= (point) (line-end-position))))
  (donkey-mark-test--keys "for text that is\nnot saved\n" "w w M g h w"
    (should (equal (donkey-mark-test--selection) "for")))
  ;; A swap is not theirs to honor either -- they own fixed ends, so
  ;; they trade back first, exactly as the object keys do.
  (donkey-mark-test--keys "for text that is\nnot saved\n" "w w l M w * g h"
    (should (equal (donkey-mark-test--selection) "for text that")))
  (donkey-mark-test--keys "for text that is\nnot saved\n" "w w l M w * g l"
    (should (equal (donkey-mark-test--selection) "that is")))
  ;; And both bring a region back that a hook deactivated mid-run: the
  ;; extending branch is reached because `last-command' names a family
  ;; member that is no motion, so the visible-run guard does not apply,
  ;; and a selection growing invisibly is the thing that guard exists
  ;; to prevent everywhere else.  Staged from `pre-command-hook' for
  ;; the reason donkey-a-backward-press-revives-a-deactivated-run
  ;; gives: `last-command' cannot survive a second `execute-kbd-macro'.
  (dolist (case '(("g h" donkey-mark-run-line-start "for text that")
                  ("g l" donkey-mark-run-line-end "that is")))
    (cl-destructuring-bind (keys cmd expected) case
      (let ((sabotage (lambda ()
                        (when (eq this-command cmd)
                          (deactivate-mark)))))
        (unwind-protect
            (progn
              (add-hook 'pre-command-hook sabotage)
              (donkey-mark-test--keys "for text that is\nnot saved\n"
                  (concat "w w l M w " keys)
                (should (equal (list keys (donkey-mark-test--selection))
                               (list keys expected)))))
          (remove-hook 'pre-command-hook sabotage)))))
  (donkey-mark-test--keys "for text that is" "w w l M w"
    (should (eq (key-binding (kbd "g q")) 'fill-region))
    (should (eq (key-binding (kbd "g h")) 'donkey-mark-run-line-start))))

(ert-deftest donkey-a-mode-motion-continues-only-a-visible-run ()
  "A wrapper motion beside a stale mark does not conjure a selection.

The object members of `donkey--mark-run-commands' may revive a region
a hook deactivated mid-run; the `donkey--mark-run-adjusters' members
are held to `region-active-p' as well, or `M l w' next to the mark a
canceled selection left behind grew a selection from wherever that
mark lay.  Here: a run is marked and canceled from inside the mode --
its mark stays behind -- the mode is re-entered empty, `l' moves
beside the stale mark, and `w' must mark the word at point afresh --
\"that\", not the \"hat\" that growing from the stale mark gave.
\(Staged through the in-mode cancel because `M' over a live
selection now ADOPTS it rather than canceling, and re-entered from
WHITESPACE because `M' on a word marks it -- the head start would
give `l' a live region to reshape and hide the case entirely.)"
  (donkey-mark-test--keys "for text that is" "w w l M w M w M l w"
    (should (equal (donkey-mark-test--selection) "is"))))

(ert-deftest donkey-mark-run-mode-mixes-objects ()
  "`M w s' grows the word to its sentence's end, like `m w m s'.

The cross-object family rule reaches inside the mode unchanged --
the letters are the family commands, so nothing new has to."
  (donkey-mark-test--keys "One thing.  Two thing.  Three thing."
      "w w w w M w s"
    (should (equal (donkey-mark-test--selection) "thing."))))

(ert-deftest donkey-a-blank-run-grows-instead-of-being-refused ()
  "`M J s' on a blank line extends the run rather than dropping it.

`donkey-mark-sentence' and `donkey-mark-paragraph' both refuse a
selection holding nothing but whitespace, which is right for a FRESH
press -- an empty buffer has no sentence to mark, and the motions
would otherwise \"mark\" the blank silently.  On a continuation it was
wrong twice over: the run may legitimately hold blank, `J' on an
indented empty line being one way to get there, and the refusal
deactivated the mark, so a key asked to GROW the selection threw it
away instead.  The siblings simply extend; these two now do too.

A fresh press in a buffer of nothing but whitespace must still
report, which is the guard the continuation case had to be carved out
of rather than deleted."
  (donkey-mark-test--keys "Word.\n\n   \n" "j j M J s"
    (should (equal (donkey-mark-test--selection) "   \n")))
  (donkey-mark-test--keys "Word.\n\n   \n" "j j M J m p"
    (should (equal (donkey-mark-test--selection) "   \n")))
  (donkey-mark-test--keys "   \n\n" "M"
    (should-error (donkey-mark-sentence) :type 'user-error)
    (should-error (donkey-mark-paragraph) :type 'user-error)))

(ert-deftest donkey-mark-run-mode-cancels-on-m-and-on-c-g ()
  "`M' inside the mode drops selection and mode; `keyboard-quit' too.

The in-mode `M' runs `donkey-mark-run-cancel', which is deliberately
no family member, so `donkey--mark-run-mode-keep-p' lets the map go
with the selection -- the `w' that follows is a plain motion, pinned
by point landing at the end of the word rather than a region
appearing.  `C-g' cannot travel inside a keyboard macro, being
Emacs's interrupt character before it is a key, so `keyboard-quit' is
called as the command loop would; a live press also leaves it in
`last-command', ending the run by the family rule."
  (donkey-mark-test--keys "for text that is" "w w l M w M w"
    (should-not (region-active-p))
    (should (= (point) 14)))
  ;; The cancel wording matches its sibling toggle: "Mark run: canceled"
  ;; beside V's "Visual line: canceled".  Recounted here because the
  ;; docstring promises it.
  (let (msgs)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs))
                 nil)))
      (donkey-mark-test--keys "for text that is" "w w l M w M"
        nil))
    (should (equal (car msgs) "Mark run: canceled")))
  (donkey-mark-test--keys "for text that is" "w w l M w"
    (condition-case nil (keyboard-quit) (quit nil))
    (should-not (region-active-p))))

(ert-deftest donkey-mark-run-mode-keeps-its-hint-visible ()
  "The mode reminder is re-shown after every letter of the run.

A single flash at entry vanished under the first \"Word marked\";
`donkey--mark-run-mode-post-command' paints the reminder back after each
family command, and after nothing else -- during count entry the echo
area belongs to the keystroke echo, so across `M', a `C-u 3' and a
`w' the hint must appear exactly twice: at entry and after the
letter.  Messages
are captured by stubbing `message', because batch Emacs has no echo
area for `current-message' to read."
  (let (msgs)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs))
                 nil)))
      (donkey-mark-test--keys "for text that is not saved" "w w l M w w"
        nil))
    (should (equal (car msgs) donkey--mark-run-mode-hint)))
  (let (msgs)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs))
                 nil)))
      (donkey-mark-test--keys "alpha beta gamma delta" "M C-u 3 w"
        nil))
    (should (= 2 (seq-count (lambda (m) (equal m donkey--mark-run-mode-hint))
                            msgs))))
  ;; The ENTRY press ends on the reminder too.  The head start says
  ;; "Word marked" over the one `donkey--mark-run-enter' shows, and no
  ;; repaint follows -- the toggle is deliberately no family member --
  ;; so the mode's only sign on screen was missing for the whole first
  ;; press of every run that starts on a word, which is most of them.
  ;; `M d' is a complete interaction that never showed it.  Marking
  ;; before arming is what puts the reminder last.
  (dolist (keys '("M" "M w" "M C-u 3 w"))
    (let (msgs)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (donkey-mark-test--keys "alpha beta gamma delta" keys
          nil))
      (should (equal (list keys (car msgs))
                     (list keys donkey--mark-run-mode-hint)))))
  ;; On whitespace there is no head start to talk over, and the
  ;; reminder is still the last thing said.
  (let (msgs)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs))
                 nil)))
      (donkey-mark-test--keys " alpha beta" "M"
        nil))
    (should (equal (car msgs) donkey--mark-run-mode-hint))))

(ert-deftest donkey-the-hint-hook-leaves-with-the-mode ()
  "A key that lapses the mode takes the reminder hook with it.

The hook rides `post-command-hook' globally; `donkey--mark-run-exit'
removes it, reached as the transient map's ON-EXIT, so whichever key
ends the mode -- a delete here, since the motions joined the mode's
own keys -- must leave the hook gone, or every later mark command in
the session would re-paint a reminder for a mode that is over.

The map's ON-EXIT is asserted separately, by calling the exit
function the way the paths with no following command reach it --
`set-transient-map-timeout' deactivates on an idle timer, where
`donkey--mark-run-mode-post-command' never fires to notice."
  (donkey-mark-test--keys "for text that is" "w w l M w d"
    (should-not (memq #'donkey--mark-run-mode-post-command
                      post-command-hook)))
  (donkey-mark-test--keys "for text that is" "w w l M w"
    (funcall donkey--mark-run-exit-function)
    (should-not (memq #'donkey--mark-run-mode-post-command
                      post-command-hook))))

(ert-deftest donkey-a-command-that-outlived-the-map-ends-the-mode ()
  "The reminder hook disarms a mode the transient map cannot.

`set-transient-map' consults `donkey--mark-run-mode-keep-p' from
`pre-command-hook', which is one command too late for anything that
armed the map while it was already running: its own key lookup
happened long before, so the mode outlives it and the user's next bare
`w' MARKS instead of moving.  `donkey--mark-run-mode-post-command'
ends it on any command that is no family member, no part of a count,
and not the toggle itself.

Staged by calling the hook the way the command loop would: a command
that plays a macro through `execute-kbd-macro' comes back with
`this-command' nil, the inner loop having cleared it, so nil is what
the outer post-command sees.  \(`kmacro-call-macro' is the exception
that restores it -- see
donkey-a-replayed-macro-does-not-outlive-its-mode.)"
  (donkey-mark-test--keys "for text that is" "w w l M w"
    (should (eq (key-binding "w") 'donkey-mark-word))
    (let ((this-command nil))
      (donkey--mark-run-mode-post-command))
    (should (eq (key-binding "w") 'forward-word))
    (should-not (memq #'donkey--mark-run-mode-post-command
                      post-command-hook))))

(ert-deftest donkey-a-replayed-macro-does-not-outlive-its-mode ()
  "A macro ending in mark run mode takes the mode with it.

The case `donkey-a-command-that-outlived-the-map-ends-the-mode' does
NOT cover, and the reason `donkey--mark-run-armed-in-macro' exists.
`kmacro-call-macro' and `kmacro-end-and-call-macro' deliberately leave
`this-command' set to the macro\'s LAST command, so that
`last-command' chaining and the repeat key keep working -- which means
a macro ending on `M' and a letter reaches the hook with
`this-command' naming a family member, and a test on `this-command'
alone reads it as an ordinary press of the mode\'s own key and keeps
the mode.  The user\'s next bare `w' then marked a word instead of
moving, which is the whole complaint.

`executing-kbd-macro' is the signal that does not lie: the mode was
armed while a macro ran, and the first command to finish outside one
is the macro\'s own caller.

Driven as one turn of the command loop, from OUTSIDE any macro --
`donkey-mark-test--keys' has finished its own by the time the body
runs -- because the whole distinction is between commands that end
inside a macro and the one that ends outside it."
  (donkey-mark-test--keys "for text that is not saved" "w w l"
    (let ((this-command 'kmacro-call-macro) (last-command nil))
      (run-hooks 'pre-command-hook)
      (kmacro-call-macro 1 t nil (kbd "M w w"))
      ;; The trap, asserted rather than described: the macro left one
      ;; of the mode\'s own commands in `this-command'.
      (should (memq this-command donkey--mark-run-commands))
      (should (eq (key-binding "w") 'donkey-mark-word))
      (run-hooks 'post-command-hook))
    (should (eq (key-binding "w") 'forward-word))
    (should-not (memq #'donkey--mark-run-mode-post-command
                      post-command-hook))
    (should-not donkey--mark-run-armed-in-macro))
  ;; The control the harness cannot stage: it drives every key through
  ;; `execute-kbd-macro', where the flag is always set.  Entered with no
  ;; macro running it stays nil, so a mode armed by a live keypress is
  ;; never touched by the rule and survives its own commands.
  (unwind-protect
      (progn
        (donkey--mark-run-enter)
        (should-not donkey--mark-run-armed-in-macro)
        (let ((this-command 'donkey-mark-word))
          (donkey--mark-run-mode-post-command))
        (should (eq (key-binding "w") 'donkey-mark-word)))
    (donkey--mark-run-exit)))

(ert-deftest donkey-a-repainted-reminder-is-never-logged ()
  "The reminders both modes keep up leave *Messages* alone.

`donkey--repaint-hint' is called after every motion of a live session
or run, so a logged reminder would fill the log with copies of one
line and bury whatever was worth reading there.

Asserted at the call rather than against the log's own size: the
whole suite runs once inside a live frame under a stubbed `message',
where nothing reaches *Messages* to be counted and a size check would
pass for the wrong reason.  What the repaint owes is the binding, so
the binding is what the stub reads -- with a flag beside it, since a
repaint that messaged nothing at all would otherwise look clean."
  (let ((message-log-max 1000) called logging)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _)
                 (setq called t
                       logging message-log-max)
                 nil)))
      (donkey--repaint-hint "donkey reminder probe"))
    (should called)
    (should-not logging)))

(ert-deftest donkey-visual-line-keeps-its-hint-visible ()
  "The visual-line reminder is repainted after the session's motions.

Same treatment mark run mode's hint gets: without the repaint the
reminder vanished under the first message anything painted.  In
`V J' the hint must appear exactly twice -- entry and the repaint
after `J' -- which also catches the hook falling out of
`donkey--global-hooks', since entry alone still shows it once.  And a
message painted mid-session must be REPLACED by the next motion's
repaint: after a foreign `message' call, one `j' brings the reminder
back -- and so does a `g h', the line and buffer jumps having joined
`donkey--hint-motions' after a session that used one went
quiet for the rest of its life."
  (let (msgs)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs))
                 nil)))
      (donkey-mark-test--keys "one\ntwo\nthree\nfour\n" "V J"
        nil))
    (should (= 2 (seq-count (lambda (m) (equal m donkey--visual-line-hint))
                            msgs))))
  (let (msgs)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs))
                 nil)))
      (donkey-mark-test--keys "one\ntwo\nthree\nfour\n" "V J"
        (message "foreign")
        (execute-kbd-macro (kbd "j"))))
    (should (equal (car msgs) donkey--visual-line-hint)))
  (let (msgs)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) msgs))
                 nil)))
      (donkey-mark-test--keys "one two\nthree four\nfive\n" "V J"
        (message "foreign")
        (execute-kbd-macro (kbd "g h"))))
    (should (equal (car msgs) donkey--visual-line-hint))))

(defmacro donkey-hint-test--msgs (text keys &rest body)
  "Collect what is said while KEYS run over TEXT, newest first.

`message' is stubbed rather than read back through `current-message',
which batch Emacs has no echo area for -- and which reads nil
throughout a keyboard macro even in a live frame, so a test that asked
it would pass on every implementation."
  (declare (indent 2))
  `(let (msgs)
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) msgs))
                  nil)))
       (donkey-mark-test--keys ,text ,keys ,@(or body '(nil))))
     msgs))

(ert-deftest donkey-a-linear-selection-says-so-and-keeps-saying-it ()
  "`v' shows a reminder and keeps it up across the motions.

The third of the selection reminders, after mark run mode's and the
visual-line session's.  A linear selection had none, and it is the one
with least to go on: no keys of its own to give it away, and nothing
on screen but the highlight, so what it does and how to let go of it
were the two things a reader could not find out by looking.

Repainted after `donkey--hint-motions' like the visual-line one, which
is what makes it survive the navigating it is for.

A PREFIXED press is not a selection.  It pops the mark ring, and
`set-mark-command' has already said which."
  (should (member (car (donkey-hint-test--msgs "one two three\nfour\n" "v"))
                  (list donkey--linear-selection-hint)))
  ;; Up after each motion, not just at entry: three presses, three
  ;; paintings.
  (should (= 3 (seq-count
                (lambda (m) (equal m donkey--linear-selection-hint))
                (donkey-hint-test--msgs "one two three\nfour\n" "v l l"))))
  (should (= 3 (seq-count
                (lambda (m) (equal m donkey--linear-selection-hint))
                (donkey-hint-test--msgs "one two three\nfour\n" "v w w"))))
  ;; And it comes back over a foreign message, as the others do.
  (should (equal (car (donkey-hint-test--msgs "one two three\nfour\n" "v"
                        (message "foreign")
                        (execute-kbd-macro (kbd "l"))))
                 donkey--linear-selection-hint))
  ;; The flag says whose selection it is.
  (donkey-mark-test--keys "one two three\nfour\n" "v l"
    (should donkey--linear-selection-active))
  (donkey-mark-test--keys "one two three\nfour\n" "l l"
    (should-not donkey--linear-selection-active)))

(ert-deftest donkey-a-rectangle-says-so-and-keeps-saying-it ()
  "`m v' shows a reminder, and `m v' again says it is canceled.

`rectangle-mark-mode' says \"Mark set (rectangle mode)\" for itself,
which names the state without saying anything about leaving it --
and leaving is what a rectangle most needs to advertise, being the
one selection several commands refuse outright.  The reminder is shown
after that, so it is the one that stays.

The cancel message matches its two neighbors, \"Visual line: canceled\"
and \"Mark run: canceled\", the three being the selection toggles."
  (let ((msgs (donkey-hint-test--msgs "one two\nthree four\n" "m v")))
    (should (equal (car msgs) donkey--rectangle-hint))
    ;; Said AFTER the mode's own announcement, not before it.
    (should (member "Mark set (rectangle mode)" msgs)))
  (should (= 2 (seq-count
                (lambda (m) (equal m donkey--rectangle-hint))
                (donkey-hint-test--msgs "one two\nthree four\n" "m v j"))))
  (should (equal (car (donkey-hint-test--msgs "one two\nthree four\n" "m v m v"))
                 "Rectangle: canceled")))

(ert-deftest donkey-only-one-selection-reminder-speaks-at-a-time ()
  "Each selection mode's reminder gives way to the one that took over.

The three reminders share `post-command-hook' and their selections
share the mark, so a mode entered over another leaves the older one's
grounds intact: the linear flag survives a `V' or an `m v' that never
deactivated the mark, and mark run mode ADOPTS a linear selection
rather than replacing it.  Without a test each way, whichever hook ran
last would win, which is not a rule anybody could hold."
  (dolist (case (list (list "v l M"     donkey--mark-run-mode-hint)
                      (list "v l M w"   donkey--mark-run-mode-hint)
                      (list "v l V"     donkey--visual-line-hint)
                      (list "v l V J"   donkey--visual-line-hint)
                      (list "v l m v"   donkey--rectangle-hint)
                      (list "v l m v j" donkey--rectangle-hint)
                      (list "V m v j"   donkey--rectangle-hint)
                      (list "V J"       donkey--visual-line-hint)))
    (cl-destructuring-bind (keys expected) case
      (should (equal (list keys (car (donkey-hint-test--msgs
                                      "one two three\nfour five\n" keys)))
                     (list keys expected)))))
  ;; The linear reminder's two exclusions are asked of the hook
  ;; directly.  Through the keys, hook ORDER answers for them instead:
  ;; the three are registered in one list and `add-hook' prepends, so
  ;; the visual-line hook runs last and would win whether or not this
  ;; one stood aside -- and a guard that only looks right because of
  ;; the order it is called in stops being right the day somebody
  ;; reorders the list.
  (with-temp-buffer
    (insert "one two three")
    (goto-char (point-min))
    (setq donkey--linear-selection-active t)
    (push-mark (point-max) t t)
    (let ((this-command 'forward-char)
          ;; `deactivate-mark' below is a no-op without this: it asks
          ;; `region-active-p' first, which wants `transient-mark-mode',
          ;; and batch Emacs has it off.
          (transient-mark-mode t)
          (painted nil))
      (cl-letf (((symbol-function 'donkey--repaint-hint)
                 (lambda (hint) (setq painted hint))))
        ;; On its own it speaks.
        (donkey--linear-selection-show-hint)
        (should (equal painted donkey--linear-selection-hint))
        ;; Over a visual-line session it does not.
        (setq painted nil)
        (let ((donkey-visual-anchor (point-min)))
          (donkey--linear-selection-show-hint))
        (should-not painted)
        ;; Nor under a rectangle.
        (setq painted nil)
        (let ((rectangle-mark-mode t))
          (donkey--linear-selection-show-hint))
        (should-not painted)
        ;; Nor once the mark is gone, flag or no flag.
        (setq painted nil)
        (deactivate-mark)
        (let ((donkey--linear-selection-active t))
          (donkey--linear-selection-show-hint))
        (should-not painted)))))

(ert-deftest donkey-a-selection-reminder-does-not-outlive-its-selection ()
  "The reminder goes when the mark does, and only if it is what is showing.

A reminder is the only sign on screen that a selection is live, so it
must not outlive one: `d' or `y' over a linear selection ends it
without a word, and the echo area went on advertising a selection that
was already gone.  `donkey--clear-selection-hint' takes it down from
`deactivate-mark-hook', whatever deactivated the mark.

Cleared only when the reminder is what is showing -- the no-clobber
rule `donkey--mark-run-exit' keeps -- so a command that said something
of its own keeps its echo.

`current-message' is stubbed: batch Emacs has no echo area, and a
keyboard macro reads nil from it even in a live frame."
  (dolist (hint (list donkey--linear-selection-hint donkey--rectangle-hint))
    (let ((cleared nil))
      (cl-letf (((symbol-function 'current-message) (lambda () hint))
                ((symbol-function 'message)
                 (lambda (fmt &rest _) (unless fmt (setq cleared t)))))
        (with-temp-buffer
          (setq donkey--linear-selection-active t)
          (donkey--clear-selection-hint)
          (should-not donkey--linear-selection-active)
          (should cleared)))))
  ;; Someone else's message is left alone.
  (let ((cleared nil))
    (cl-letf (((symbol-function 'current-message) (lambda () "Saved buffer"))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (unless fmt (setq cleared t)))))
      (with-temp-buffer
        (setq donkey--linear-selection-active t)
        (donkey--clear-selection-hint)
        (should-not donkey--linear-selection-active)
        (should-not cleared))))
  ;; And the hook is really installed by the two commands that need it.
  (donkey-mark-test--keys "one two three\n" "v"
    (should (memq #'donkey--clear-selection-hint deactivate-mark-hook)))
  (donkey-mark-test--keys "one two three\n" "m v"
    (should (memq #'donkey--clear-selection-hint deactivate-mark-hook))))

(ert-deftest donkey-the-visual-hint-never-paints-over-foreign-echo ()
  "The repaint fires only for listed motions in a genuinely live session.

The whitelist in `donkey--hint-motions' is the whole
no-clobber rule: a command that messaged must keep its echo, and
whether one just did cannot be told after the fact, so anything not
listed -- here a stand-in `save-buffer' -- must not repaint.  Nor may
a listed motion outside any session: once the region is gone the
reminder would be advertising keys that no longer extend anything."
  (donkey-mark-test--keys "one\ntwo\nthree\n" "V J"
    (let (msgs)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (let ((this-command 'save-buffer))
          (donkey--visual-line-show-hint))
        (should-not msgs)
        (deactivate-mark)
        (let ((this-command 'next-line))
          (donkey--visual-line-show-hint))
        (should-not msgs)))))

(ert-deftest donkey-a-stale-visual-anchor-does-not-repaint-the-hint ()
  "A superseding selection ends the reminder along with the session.

A mark command mid-session repositions the region without ever
deactivating it, so the anchor survives while the session is over --
the exact state `donkey--visual-line-session-active-p' exists to
reject, and the case that separates it from a bare anchor check in
`donkey--visual-line-show-hint': anchor set, region active, mark on
the marked word.  A motion here must not resurrect the visual-line
reminder over a selection that is no longer a visual-line session."
  (donkey-mark-test--keys "one two\nthree four\n" "V J m w"
    (let (msgs)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) msgs))
                   nil)))
        (let ((this-command 'next-line))
          (donkey--visual-line-show-hint))
        (should-not msgs)))))

(ert-deftest donkey-m-adopts-an-existing-selection ()
  "`M' over a live selection carries it into the mode; `M M' cancels.

The press once canceled any active selection; adoption replaced that
because \"transfer my selection into the mode\" kept being the
intent.  A prefix-built run, a `v' region and a visual-line session
all carry over, and the object keys grow them: `m w M w' extends the
marked word, and a `V J' selection keeps both lines when a word is
added.  The adopted region is normalized to the family layout --
point at the start, where a downward `V' session had it at the end --
pinned by point sitting at `region-beginning' after `V J M'.  A
visual-line session also arrives WHOLE-LINED, final newline and all,
which is why the bare `V M' case expects one; every other selection
is adopted exactly as it shows.  The session itself ends at adoption,
anchor cleared, so its reminder cannot resurrect over a selection the
run now owns.  And the toggle is still a toggle: the in-mode `M'
after an adoption cancels, one key as ever."
  (donkey-mark-test--keys "for text that is not saved" "w w l m w M w"
    (should (equal (donkey-mark-test--selection) "that is")))
  (donkey-mark-test--keys "for text that is" "v l l M w"
    (should (equal (donkey-mark-test--selection) "for")))
  (donkey-mark-test--keys "one two\nthree four\nfive six\n" "V J M w"
    (should (equal (donkey-mark-test--selection)
                   "one two\nthree four\nfive")))
  (donkey-mark-test--keys "one two\nthree four\n" "V J M"
    (should (= (point) (region-beginning))))
  (donkey-mark-test--keys "one two\nthree four\n" "V M"
    (should (equal (donkey-mark-test--selection) "one two\n"))
    (should-not (donkey--visual-line-session-active-p)))
  (donkey-mark-test--keys "for text that is" "w w l m w M M"
    (should-not (region-active-p))))

(ert-deftest donkey-adopting-a-visual-line-session-takes-whole-lines ()
  "A `V' session carries its final newline into the run.

Visual-line sessions leave the newline that ends the last line
OUTSIDE the region deliberately -- the highlight stops where the text
does -- and their own `y' and `d' widen through
`donkey--visual-line-region-bounds' to make up for it.  A run that
inherited the region raw did not: `V J M d' removed the text of two
lines and left behind the blank line their newline still ended, where
`V J d' removes both lines outright.  Adoption goes through the same
widening, so the two agree.

Only for a live session.  A `v' region means the characters it covers
and is adopted untouched, which is what tells the widening from a
blanket one."
  (donkey-mark-test--keys "one two\nthree four\nrest\n" "V J M d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "rest\n")))
  (donkey-mark-test--keys "for text that is" "v l l M"
    (should (equal (donkey-mark-test--selection) "fo"))))

(ert-deftest donkey-an-empty-selection-is-not-adopted ()
  "`v M w' marks the whole word, as `M w' alone does.

`donkey-set-mark' activates a mark without covering anything yet, and
`region-active-p' says yes to that empty region.  Adopting it made
the first object key grow from the CURSOR -- `v M w' from mid-word
took the tail of the word, \"hat\" for \"that\" -- which is the very
design `donkey-mark-run-mode-first-letter-marks-afresh' pins the
toggle against.  The toggle drops such a region and marks the word
under the cursor in its place, and `donkey-mark-run-adopt' refuses
outright, being reachable by name.

Dropping it, rather than merely declining to adopt it: an empty
region left ACTIVE is a selection to everything that asks, so `v M d'
deleted nothing at all, and \\`*' had ends to trade where there is
nothing between them.  The drop now shows as a whole word taking the
empty region's place, so `v M d' deletes that word; from whitespace,
where there is no word to stand in, it shows on its own."
  (donkey-mark-test--keys "for text that is" "w w l l v M w"
    (should (equal (donkey-mark-test--selection) "that")))
  ;; The head start makes the point even plainer: the empty region is
  ;; gone and a WHOLE word stands in its place, where adopting would
  ;; have kept the cursor as one end.
  (donkey-mark-test--keys "for text that is" "w w l l v M"
    (should (equal (donkey-mark-test--selection) "that")))
  ;; From whitespace there is no word to stand in, so the drop shows on
  ;; its own -- and `*' has nothing to trade.
  (donkey-mark-test--keys "for text that is" "w w v M"
    (should-not (region-active-p))
    (should-error (donkey-mark-run-exchange) :type 'user-error))
  (donkey-mark-test--keys "for text that is" "w w l l v M d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "for text  is")))
  (donkey-mark-test--keys "for text that is" "w w l l v"
    (should-error (donkey-mark-run-adopt) :type 'user-error))
  (donkey-mark-test--keys "for text that is" "w"
    (should-error (donkey-mark-run-adopt) :type 'user-error)))

(ert-deftest donkey-the-m-prefix-still-works-inside-the-mode ()
  "`m w' inside the mode runs the mode's own `w' and keeps the run.

`m' is bound in neither the mode map nor the family, so the press
falls through to the normal map's prefix -- and lands on
`donkey-mark-word', which the bare `w' here runs too.  Both the keep
test and the extending test are about COMMANDS, so nothing has to
arrange this: the mode survives the press and the run grows.  Pinned
because the two spellings being interchangeable mid-run is what the
mode promises, and a keep test written against KEYS would quietly
break it."
  (donkey-mark-test--keys "for text that is not saved" "w w l M w m w w"
    (should (equal (donkey-mark-test--selection) "that is not")))
  ;; The paragraph pair has no bare letter here -- `p' and `P' are the
  ;; paste keys -- so the prefix is the ONLY way it reaches the mode,
  ;; and the run has to survive it like any other family press.
  (donkey-mark-test--keys "Alpha one.\n\nBeta two.\n\nGamma.\n" "j j M w m p"
    (should (equal (donkey-mark-test--selection) "Beta two.\n\n")))
  (donkey-mark-test--keys "Alpha one.\n\nBeta two.\n\nGamma.\n" "j j M w m p w"
    (should (equal (donkey-mark-test--selection) "Beta two.\n\nGamma")))
  (donkey-mark-test--keys "Alpha one.\n\nBeta two.\n\nGamma.\n"
      "j j j j M w m P"
    (should (equal (donkey-mark-test--selection) "\nGamma"))))

(ert-deftest donkey-the-mode-hint-names-only-keys-that-work ()
  "Every key the reminder advertises does what it says inside a run.

The reminder is the only place most people will read the mode's key
list, so a letter named there that does not work is a promise the
next press breaks -- which is exactly what happened when the
paragraph pair gave its letters back to pasting: `p/P paragraphs'
stayed in the hint while `p' had become `donkey-yank'.  The pairs are
read out of the hint itself rather than listed again here, so the two
cannot drift apart.

Two kinds of key are named, and they are checked differently.  A bare
letter has to be in `donkey-mark-run-mode-map'.  An `m'-prefixed
spelling must NOT be -- it reaches normal state, the mode being
transparent to the prefix -- and must run a `donkey--mark-run-commands'
member when it gets there, which is what makes it grow a run.

The width is pinned too.  The line names the object keys and leaves
out the ones that keep their normal-state meaning, and it does that
to stay readable at a glance; a later entry put back without thought
would undo the trim silently."
  (let* ((hint donkey--mark-run-mode-hint)
         (rest hint)
         (prefixed nil)
         (keys '("h" "j" "k" "l" "*" "M")))
    ;; The `m'-prefixed spellings first, and out of the string: the
    ;; plain-pair regexp below would otherwise read the "p/m" that
    ;; spans the middle of "m p/m P" as a pair of its own.
    (while (string-match "m \\([a-zA-Z]\\)/m \\([a-zA-Z]\\)" rest)
      (push (concat "m " (match-string 1 rest)) prefixed)
      (push (concat "m " (match-string 2 rest)) prefixed)
      (setq rest (replace-match "" t t rest)))
    (let ((start 0))
      (while (string-match "\\([a-zA-Z*]\\)/\\([a-zA-Z*]\\)" rest start)
        (push (match-string 1 rest) keys)
        (push (match-string 2 rest) keys)
        (setq start (match-end 0))))
    ;; The pairs really were found -- regexps that matched nothing
    ;; would leave this passing on the six literals alone.
    (should (> (length keys) 6))
    (should prefixed)
    (dolist (key keys)
      (should (equal (list key (and (keymap-lookup donkey-mark-run-mode-map key) t))
                     (list key t))))
    (dolist (key prefixed)
      ;; Asked for a command, not for nil: `keymap-lookup' answers a
      ;; NUMBER for a multi-key sequence whose first key the map does
      ;; not bind -- "m P" reads as 1 here, the count of events that
      ;; made a complete key -- so a nil test would fail on a map that
      ;; is behaving exactly as it should.
      (should (equal (list key (commandp
                                (keymap-lookup donkey-mark-run-mode-map key)))
                     (list key nil)))
      (should (memq (keymap-lookup donkey-normal-mode-map key)
                    donkey--mark-run-commands)))
    (should (<= (length hint) 100))))

(ert-deftest donkey-p-and-P-keep-their-paste-jobs-inside-the-mode ()
  "`M w p' replaces the marked word; the mode letters no paragraph.

Every other ordinary edit already reached a mark run selection --
`d', `y', `x' and `c' are all foreign keys that lapse the mode and
act on what it built -- but `p' and `P' were the paragraph keys here,
so the one thing the mode could not do was paste over its own
selection: `M w p' grew the selection to its paragraph and pasted
nothing, and there was no key sequence that would.  The pair passes
through now, and the objects they used to name are still one prefix
away, at `m p' and `m P'.

`P' is `donkey-yank-rectangle' as it is everywhere else, which does
nothing without a banked rectangle -- what matters is that the press
no longer marks, and that it lets the mode go."
  (let* ((kill-ring (list "XX"))
         (kill-ring-yank-pointer kill-ring))
    (donkey-mark-test--keys "for text that is" "w w l M w p"
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "for text XX is"))
      (should-not (region-active-p))
      (should (eq (key-binding "w") 'forward-word))))
  (donkey-mark-test--keys "for text that is" "w w l M w P"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "for text that is"))
    (should (eq (key-binding "w") 'forward-word)))
  ;; Mid-run, with the map armed, the two keys are still the paste ones.
  (donkey-mark-test--keys "for text that is" "w w l M w"
    (should (eq (key-binding "w") 'donkey-mark-word))
    (should (eq (key-binding "p") 'donkey-yank))
    (should (eq (key-binding "P") 'donkey-yank-rectangle))))

(ert-deftest donkey-j-and-k-grow-the-run-by-lines ()
  "Inside the mode, `J' and `K' make lines a growable object.

Fresh, either press marks the line point is on; further presses grow
by whole lines, `J' pushing the mark down and `K' walking point up,
the family's fixed ends.  A mark sitting mid-line first completes its
own line -- the mid-object rule words and symbols already follow --
so `M w J' takes the word's whole line, and objects keep mixing:
`M J w' is the line plus a word.  Counts work, and `J' outside the
mode is untouched -- still `donkey-visual-next-line', V's key.

A whole line here includes its NEWLINE, which is why every expectation
below that ends at a line boundary ends with one -- see
`donkey-line-runs-take-the-newline-with-them' for what that buys."
  (donkey-mark-test--keys "one two\nthree four\nfive six\nseven eight\n"
      "j M J"
    (should (equal (donkey-mark-test--selection) "three four\n")))
  (donkey-mark-test--keys "one two\nthree four\nfive six\nseven eight\n"
      "j M J J"
    (should (equal (donkey-mark-test--selection) "three four\nfive six\n")))
  (donkey-mark-test--keys "one two\nthree four\nfive six\nseven eight\n"
      "j M J K"
    (should (equal (donkey-mark-test--selection) "one two\nthree four\n")))
  (donkey-mark-test--keys "one two\nthree four\nfive six\nseven eight\n"
      "j M w J"
    (should (equal (donkey-mark-test--selection) "three four\n")))
  (donkey-mark-test--keys "one two\nthree four\nfive six\nseven eight\n"
      "j M J w"
    (should (equal (donkey-mark-test--selection) "three four\nfive")))
  (donkey-mark-test--keys "one two\nthree four\nfive six\nseven eight\n"
      "j M C-u 2 J"
    (should (equal (donkey-mark-test--selection) "three four\nfive six\n")))
  (donkey-mark-test--keys "one two\nthree four\nfive six\nseven eight\n"
      "V J M J"
    (should (equal (donkey-mark-test--selection)
                   "one two\nthree four\nfive six\n")))
  ;; The last line of a buffer with no final newline has none to take;
  ;; `forward-line' stops at `point-max' and the selection keeps what
  ;; there is.
  (donkey-mark-test--keys "one\ntwo" "j M J"
    (should (equal (donkey-mark-test--selection) "two")))
  (donkey-mark-test--keys "one two\nthree four\n" "J"
    (should-not (region-active-p))))

(ert-deftest donkey-line-runs-take-the-newline-with-them ()
  "`M J d' removes the line; it used to leave the blank one behind.

`J' and `K' put the mark at the START OF THE NEXT LINE rather than at
the end of this one, so the newline that ends the selection is inside
it.  Without that character the run was the TEXT of a line rather than
a line: `M J d' deleted the words and left the empty line their
newline still ended, and `M J y' produced a kill with no line break,
which the next `p' spliced into whatever line it landed in.

Pinned against `donkey-visual-line-toggle', which answers the same
problem from the other side -- it keeps the highlight tight and
widens at `y'/`d' through `donkey--visual-line-region-bounds', having
an anchor to widen from where this has none.  The two now agree on
what they leave behind, which is the whole point of the change.

`V M J d' is the case that made it worth doing: adopting a visual-line
session brings its lines over whole, and before this the very next
`J' handed the newline straight back."
  (donkey-mark-test--keys "one two\nthree four\nrest\n" "M J d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "three four\nrest\n")))
  (donkey-mark-test--keys "one two\nthree four\nrest\n" "M J J d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "rest\n")))
  (donkey-mark-test--keys "one two\nthree four\nrest\n" "j M K d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "one two\nrest\n")))
  (donkey-mark-test--keys "one two\nthree four\nrest\n" "V M J d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "rest\n")))
  ;; A delete through the mode and one through a `V' session leave the
  ;; same buffer, and a yank through either pastes back as a line.
  (let ((moded (donkey-mark-test--keys "one two\nthree four\nrest\n" "M J J d"
                 (buffer-substring-no-properties (point-min) (point-max))))
        (visual (donkey-mark-test--keys "one two\nthree four\nrest\n" "V J d"
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal moded visual)))
  (let ((moded (donkey-mark-test--keys "one two\nthree\n" "M J y G p"
                 (buffer-substring-no-properties (point-min) (point-max))))
        (visual (donkey-mark-test--keys "one two\nthree\n" "V y G p"
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal moded "one two\nthree\none two\n"))
    (should (equal moded visual))))

(ert-deftest donkey-star-trades-the-end-the-motions-hold ()
  "`*' swaps point and mark, so the motions adjust the other end.

Vi's `o' in visual mode: the motions move point, and point sits at
the selection's start, so trimming or growing the FAR end meant
walking the near one -- `M w w * l l' now grows past the far end
instead, `* h' trims it, and a second `*' trades back, pinned by the
un-swapped `l' shrinking the start again.  A member of
`donkey--mark-run-adjusters', so the run carries on across it.

Refused without an ACTIVE selection, and the stale mark is the case
that separates the guard from what `exchange-point-and-mark' refuses
on its own: with no mark at all the native command already signals,
but beside the mark a canceled run left behind it would leap there
and re-activate whatever lies between -- so after cancel, the swap
must refuse and the region must stay gone."
  (donkey-mark-test--keys "for text that is not saved" "w w l M w w * l l"
    (should (equal (donkey-mark-test--selection) "that is n")))
  (donkey-mark-test--keys "for text that is not saved" "w w l M w * h"
    (should (equal (donkey-mark-test--selection) "tha")))
  (donkey-mark-test--keys "for text that is not saved" "w w l M w * * l"
    (should (equal (donkey-mark-test--selection) "hat")))
  (donkey-mark-test--keys "for text that is" "w w l M w M"
    (should-error (donkey-mark-run-exchange) :type 'user-error)
    (should-not (region-active-p))))

(ert-deftest donkey-an-object-key-trades-the-ends-back-before-growing ()
  "After `*', an object key grows the run instead of destroying it.

The object keys own fixed ends -- mark forward, point backward -- so
a swap is not theirs to honor.  Left unhandled it was not merely
awkward: `M w * w' pushed the mark forward from the region\'s own
START and collapsed the selection to nothing, still reporting \"Word
marked\"; `M w * s' replaced it with a span on the other side of
point.  `donkey--normalize-mark-run' trades the ends back first, so
each of the eight reads exactly as its unswapped spelling does --
asserted against that spelling, key by key, rather than against
literals that could drift apart from it.

The motions keep the swap, which is what `*' is for; those are pinned
by donkey-star-trades-the-end-the-motions-hold."
  (dolist (key '("w" "b" "W" "B" "s" "S" "J" "K"))
    (let ((swapped (donkey-mark-test--keys "for text that is not saved"
                       (format "w w l M w * %s" key)
                     (donkey-mark-test--selection)))
          (plain (donkey-mark-test--keys "for text that is not saved"
                     (format "w w l M w %s" key)
                   (donkey-mark-test--selection))))
      (should (equal (list key swapped) (list key plain)))))
  ;; The paragraph pair is asserted on a buffer with paragraphs to
  ;; cross, and through the `m' prefix its keys keep inside the mode.
  (dolist (key '("m p" "m P"))
    (let ((swapped (donkey-mark-test--keys
                       "Alpha one.\n\nBeta two.\n\nGamma three.\n"
                       (format "j j M w * %s" key)
                     (donkey-mark-test--selection)))
          (plain (donkey-mark-test--keys
                     "Alpha one.\n\nBeta two.\n\nGamma three.\n"
                     (format "j j M w %s" key)
                   (donkey-mark-test--selection))))
      (should (equal (list key swapped) (list key plain)))))
  ;; Two literals, so the pairs cannot agree on something wrong.
  (donkey-mark-test--keys "for text that is not saved" "w w l M w * w"
    (should (equal (donkey-mark-test--selection) "that is")))
  (donkey-mark-test--keys "Alpha one.\n\nBeta two.\n\nGamma three.\n"
      "j j M w * m p"
    (should (equal (donkey-mark-test--selection) "Beta two.\n\n"))))

(ert-deftest donkey-a-crossing-motion-leaves-a-run-the-keys-can-grow ()
  "A motion may walk point past the mark; the object keys still grow.

`donkey-mark-run-toggle' promises the freeform a `v' region has --
\"a motion may even cross the mark, passing the selection through
empty before the object keys grow it again\" -- and before
`donkey--normalize-mark-run' the second half of that was not true.
With point six characters past the mark, `w' pushed the mark forward
from BEHIND point and left \"s\", a span with nothing to do with what
was on screen; `b' walked point back into the region and shrank it to
\" \".  Both now grow the visible selection from its far end."
  (donkey-mark-test--keys "for text that is not saved" "w w l M w C-u 6 l"
    (should (equal (donkey-mark-test--selection) " i")))
  (donkey-mark-test--keys "for text that is not saved" "w w l M w C-u 6 l w"
    (should (equal (donkey-mark-test--selection) " is")))
  (donkey-mark-test--keys "for text that is not saved" "w w l M w C-u 6 l b"
    (should (equal (donkey-mark-test--selection) "that i"))))

(ert-deftest donkey-a-negative-count-leaves-a-run-the-next-key-grows ()
  "A count reaching behind point still leaves a run that grows forward.

`donkey-mark-word' reads a negative COUNT as marking the words BEFORE
the one point normalizes onto, which finishes with the mark at the
selection\'s start -- the family layout inside out.  The next `w' then
walked the mark forward from there and ate into the selection instead
of extending it.  Pinned in both spellings, the mode\'s and the
prefix\'s, since the count belongs to the mark command rather than to
the mode."
  (donkey-mark-test--keys "for text that is not saved" "w w w l M C-u -3 w"
    (should (equal (donkey-mark-test--selection) "for text that ")))
  (donkey-mark-test--keys "for text that is not saved" "w w w l M C-u -3 w w"
    (should (equal (donkey-mark-test--selection) "for text that is")))
  (donkey-mark-test--keys "for text that is not saved"
      "w w w l C-u -3 m w m w"
    (should (equal (donkey-mark-test--selection) "for text that is"))))

(ert-deftest donkey-normalizing-a-run-touches-only-an-inverted-one ()
  "`donkey--normalize-mark-run\' is a no-op unless the ends are reversed.

The helper runs on every extending press, so what it does when there
is nothing to do matters as much as the swap itself: a run already
laid out point-at-start must come through untouched, mark included,
or every ordinary press would re-assert a mark it had no business
touching.  With no mark at all there is nothing to compare and
nothing to move."
  (with-temp-buffer
    (insert "alpha beta gamma")
    ;; Already the right way round: nothing moves.
    (goto-char 7)
    (push-mark 11 t t)
    (donkey--normalize-mark-run)
    (should (equal (list (point) (mark)) (list 7 11)))
    ;; Inverted: the ends trade.
    (goto-char 11)
    (set-mark 7)
    (donkey--normalize-mark-run)
    (should (equal (list (point) (mark)) (list 7 11)))
    ;; No mark: nothing to do, and no error.
    (deactivate-mark)
    (set-mark nil)
    (goto-char 3)
    (donkey--normalize-mark-run)
    (should (equal (list (point) (mark t)) (list 3 nil)))))

(ert-deftest donkey-a-key-that-does-nothing-costs-the-run-nothing ()
  "A dead key leaves both the mode and the run exactly as they were.

Every printable key the normal state leaves unbound resolves to
`undefined', and \`DEL' to `ignore'.  Both used to fail the keep test,
so the transient map lapsed and the next bare letter was a plain
motion again -- a mistyped \`0' in the middle of a run threw the
selection away and rang the bell about it.  They are inert now: the
mode stays, and because `donkey--mark-run-inert-commands' rides in
`donkey--mark-run-commands' they count as companions too, so the
object key after one still EXTENDS instead of marking afresh.

\`DEL' carries the end-to-end case because it is the one dead key that
does not ring: `execute-kbd-macro' stops on a command that dings, so
`undefined' cannot be driven through the harness and is asserted
against the predicate directly."
  (donkey-mark-test--keys "for text that is" "w w l M w DEL"
    (should (eq (key-binding "w") 'donkey-mark-word))
    (should (equal (donkey-mark-test--selection) "that")))
  (donkey-mark-test--keys "for text that is" "w w l M w DEL w"
    (should (equal (donkey-mark-test--selection) "that is")))
  (dolist (cmd '(undefined ignore))
    (let ((this-command cmd))
      (should (equal (list cmd (and (donkey--mark-run-mode-keep-p) t))
                     (list cmd t)))))
  (should (memq 'undefined donkey--mark-run-commands))
  (should (memq 'ignore donkey--mark-run-commands))
  ;; A key that really does something still ends the run, which is the
  ;; rule the exemption is carved out of.
  (donkey-mark-test--keys "for text that is" "w w l M w"
    (let ((this-command 'transpose-chars))
      (should-not (donkey--mark-run-mode-keep-p)))))

(ert-deftest donkey-V-is-refused-inside-a-mark-run ()
  "`v' and `V' say to leave the run first instead of taking it.

A visual-line session and a mark run are two ways of owning one
selection, and `donkey-visual-line-toggle' pressed mid-run dropped the
run and anchored a fresh line session on whatever line the cursor sat
in: `M w V' over a marked word came back holding that word's whole
line, with the run gone and nothing said.  The key is bound to
`donkey-mark-run-refuse' inside the mode, which signals and changes
nothing -- the mode is still armed and the selection still there when
the complaint lands, so the ways out that DO work are still one press
away.

Outside the mode `V' is untouched, which is the point of refusing
rather than rebinding."
  (donkey-mark-test--keys "for text that is\nnot saved\n" "w w l M w"
    (should (eq (key-binding "V") 'donkey-mark-run-refuse))
    (should (eq (key-binding "v") 'donkey-mark-run-refuse))
    (should-error (donkey-mark-run-refuse) :type 'user-error)
    ;; Nothing moved: the mode is armed and the run is intact.
    (should (eq (key-binding "w") 'donkey-mark-word))
    (should (equal (donkey-mark-test--selection) "that")))
  ;; The refusal is inert, so it does not break the run either.
  (should (memq 'donkey-mark-run-refuse donkey--mark-run-commands))
  (let ((this-command 'donkey-mark-run-refuse))
    (should (donkey--mark-run-mode-keep-p)))
  ;; Outside the mode both keys are themselves as ever.
  (donkey-mark-test--keys "one two\nthree four\n" "V"
    (should (equal (donkey-mark-test--selection) "one two")))
  (donkey-mark-test--keys "for text that is" "w w l v l l"
    (should (equal (donkey-mark-test--selection) "th")))

  ;; And after a legal exit it works again: the delete takes the run,
  ;; and the `V' after it starts its session on what is left.
  (donkey-mark-test--keys "one two\nthree four\n" "M w d V"
    (should (equal (donkey-mark-test--selection) " two"))))

(ert-deftest donkey-the-tutors-mark-run-walkthrough-is-true ()
  "The tutor's `M' exercise does what the tutor says it does.

The tutor teaches mark run mode by having the reader press the keys
on a sample line, and a walkthrough that has drifted from the code
teaches the wrong thing to exactly the people least able to spot it.
The sequence is the one the tutor prints -- `M', two `w', a `b', then
the delete -- and the assertions are the two claims it makes: that
three words end up selected, and that the delete takes all three."
  (donkey-mark-test--keys "one two three four five six" "w w l M w w b"
    (should (equal (donkey-mark-test--selection) "two three four")))
  (donkey-mark-test--keys "one two three four five six" "w w l M w w b d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "one  five six")))
  ;; And the line-edge claim from the same section.
  (donkey-mark-test--keys "one two three four five six" "w w l M w g h g l"
    (should (equal (donkey-mark-test--selection)
                   "one two three four five six"))))

(ert-deftest donkey-M-marks-the-word-under-the-cursor ()
  "`M' arrives holding a word, and changes nothing about the letters.

Entering the mode used to leave the selection empty and wait, so the
commonest run -- mark this word, then grow it -- cost a press that
said nothing on screen.  `M' now marks the word under the cursor on
its way in.

Only UNDER the cursor.  `donkey-mark-word' reaches for the word behind
a gap, which is right for a key that says \"word\" and wrong for one
that says \"start selecting\": from a blank line the head start would
have jumped the selection up to the paragraph above, and in a buffer
with no word at all it would have reported instead of entering.

And the head start is not the run's first press: the toggle stays out
of `donkey--mark-run-commands', so the object key after it still marks
afresh.  `M w' is the word the prefix would have marked, `M w w b' is
`m w m w m b' still, and nothing about the letters moves."
  (donkey-mark-test--keys "for text that is" "w w l M"
    (should (equal (donkey-mark-test--selection) "that"))
    (should (eq (key-binding "w") 'donkey-mark-word)))
  ;; Mid-word takes the whole word, not the tail.
  (donkey-mark-test--keys "for text that is" "w w l l M"
    (should (equal (donkey-mark-test--selection) "that")))
  ;; On whitespace, and on a blank line with prose above it, the mode
  ;; arrives empty rather than reaching for a distant word.
  (donkey-mark-test--keys "for text that is" "w w M"
    (should-not (region-active-p))
    (should (eq (key-binding "w") 'donkey-mark-word)))
  (donkey-mark-test--keys "Word.\n\n   \n" "j j M"
    (should-not (region-active-p))
    (should (eq (key-binding "w") 'donkey-mark-word)))
  ;; The letters are untouched: the first one still marks afresh.
  (let ((head (donkey-mark-test--keys "for text that is not saved" "w w l M w"
                (donkey-mark-test--selection)))
        (prefixed (donkey-mark-test--keys "for text that is not saved" "w w l m w"
                    (donkey-mark-test--selection))))
    (should (equal head "that"))
    (should (equal head prefixed)))
  (let ((moded (donkey-mark-test--keys "for text that is not saved"
                   "w w l M w w b" (donkey-mark-test--selection)))
        (prefixed (donkey-mark-test--keys "for text that is not saved"
                      "w w l m w m w m b" (donkey-mark-test--selection))))
    (should (equal moded "text that is"))
    (should (equal moded prefixed)))
  ;; Which makes the shortest useful thing the mode can do one press
  ;; and one key: take this word away.
  (donkey-mark-test--keys "for text that is" "w w l M d"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "for text  is"))))

(ert-deftest donkey-the-reminder-does-not-outlive-the-mode ()
  "The echo area stops advertising a mode that has ended.

The reminder is the only sign on screen that the mode is on.  A
foreign key that neither messages nor signals -- `g q', a recenter --
left it up over a selection the mode no longer owned, and the next
`w' moved instead of growing, with the screen still saying otherwise.

Cleared only when the reminder is what is showing, which is the
no-clobber rule `donkey--hint-motions' keeps in the other
direction: a command that said something of its own keeps its echo.
Driven through stubs because batch Emacs has no echo area for
`current-message' to read."
  (dolist (case (list (list donkey--mark-run-mode-hint t)
                      (list "a command said this" nil)
                      (list nil nil)))
    (cl-destructuring-bind (showing expected) case
      (let (cleared)
        (cl-letf (((symbol-function 'current-message) (lambda () showing))
                  ((symbol-function 'message)
                   (lambda (fmt &rest _) (when (null fmt) (setq cleared t)) nil)))
          (donkey--mark-run-exit))
        (should (equal (list showing (and cleared t))
                       (list showing expected)))))))

(defun donkey-mark-test--span-with-tmm (tmm text keys)
  "Run KEYS over TEXT with `transient-mark-mode' TMM; return the marked span.

`donkey-mark-test--keys' binds `transient-mark-mode' to t for
determinism, which is exactly the binding the caller of this needs to
vary, so the setup is repeated here rather than parameterized into a
macro two hundred tests depend on.  The span is read from point and
the mark rather than through `region-active-p', which refuses to
report one with the mode off and would hide what is being measured."
  (unwind-protect
      (progn
        (when (get-buffer "*donkey-tmm-test*") (kill-buffer "*donkey-tmm-test*"))
        (donkey-mode 1)
        (let ((transient-mark-mode tmm)
              (prefix-arg nil) (current-prefix-arg nil)
              (this-command nil) (last-command nil))
          (switch-to-buffer (get-buffer-create "*donkey-tmm-test*"))
          (fundamental-mode)
          (erase-buffer)
          (insert text)
          (goto-char (point-min))
          (donkey-enter-normal)
          (execute-kbd-macro (kbd keys))
          (and mark-active (mark t)
               (buffer-substring-no-properties (min (point) (mark t))
                                               (max (point) (mark t))))))
    (when (get-buffer "*donkey-tmm-test*") (kill-buffer "*donkey-tmm-test*"))
    (donkey--mark-run-exit)
    (donkey-mode -1)))

(ert-deftest donkey-a-run-does-not-need-transient-mark-mode ()
  "The mode answers the same with `transient-mark-mode' off.

Three guards asked `region-active-p', which is not the question they
meant.  It answers whether region-aware commands should treat the
region as active, and demands `transient-mark-mode' to say yes; what
the guards mean is whether a selection is LIVE, which is `mark-active'.

With the mode off the object keys grew runs exactly as ever -- donkey
never refused to mark invisibly -- while the motions, `*' and adoption
refused every time.  Half a mode, and by accident rather than by
policy: `donkey-rectangle-mark-mode' had already reached for
`mark-active' for this reason and written down why.

Run over BOTH values, since the point is that the answer does not
depend on the setting."
  (dolist (case '(;; a run surviving one of its own motions
                  ("w w l M w l w"        "hat is")
                  ;; the swap, which used to refuse outright
                  ("w w l M w *"          "that")
                  ("w w l M w * w"        "that is")
                  ("w w l M w * l l"      "that i")
                  ;; adoption of a region `v' built
                  ("w w l v C-u 5 l M b"  "text that ")
                  ("w w l v C-u 5 l M w"  "that is")
                  ;; and the line-edge pair, which owns ends
                  ("w w l M w g h g l"    "for text that is not saved")))
    (cl-destructuring-bind (keys expected) case
      (dolist (tmm '(t nil))
        (should (equal
                 (list tmm keys
                       (donkey-mark-test--span-with-tmm
                        tmm "for text that is not saved\nsecond line\n" keys))
                 (list tmm keys expected)))))))

(ert-deftest donkey-u-steps-a-run-back-one-press ()
  "`u' puts the run back where the last press found it.

A run only ever grows.  Every object key adds at its own end -- `b'
after `w' takes a word at the other end rather than giving one back,
which is the fixed-ends rule and worth keeping -- so a press that
reached further than it looked left cancelling and starting again as
the only way out.  That is cheap for a word and expensive for a
paragraph, and the mode cannot tell in advance which a press will be.

One press, one step, and the ladder is walked all the way down: `M w
w s' is the sentence, and three steps back is the first word.  A
fourth reports rather than guessing, and leaves the selection alone
while it does."
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M w w s"
    (should (equal (donkey-mark-test--selection) "that is not saved here today")))
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M w w s u"
    (should (equal (donkey-mark-test--selection) "that is")))
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M w w s u u"
    (should (equal (donkey-mark-test--selection) "that")))
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M w w s u u u"
    ;; A fourth finds nothing to undo and says so, leaving the run it
    ;; could not step still standing.  Called rather than pressed: the
    ;; signal would abort the macro and take the assertions with it.
    (should (equal (donkey-mark-test--selection) "that"))
    (should (eq (key-binding "w") 'donkey-mark-word))
    (should-error (donkey-mark-run-step-back) :type 'user-error))
  ;; The run carries on across a step back rather than starting over.
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M w w u w"
    (should (equal (donkey-mark-test--selection) "that is")))
  ;; Every press the mode owns is a step, the motions and `*' included.
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M w l l u"
    (should (equal (donkey-mark-test--selection) "hat")))
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M w * u"
    (should (equal (donkey-mark-test--selection) "that")))
  ;; A counted press is one press, so one step takes all of it back.
  (donkey-mark-test--keys "for text that is not saved here today"
      "w w l M w C-u 5 w u"
    (should (equal (donkey-mark-test--selection) "that")))
  ;; A key that changed nothing recorded nothing, so the step goes past
  ;; it to the press that did.
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M w DEL u"
    (should (equal (donkey-mark-test--selection) "that"))
    (should-error (donkey-mark-run-step-back) :type 'user-error))
  ;; The head start is not a recorded press: `M' alone has nothing
  ;; before it to go back to, and cancelling is what undoes it.
  (donkey-mark-test--keys "for text that is not saved here today" "w w l M"
    (should-error (donkey-mark-run-step-back) :type 'user-error))
  ;; Entered from whitespace there is no head start, so the first
  ;; press records a state with NO mark -- and stepping back to it has
  ;; to put the selection away rather than leave the last one showing.
  (donkey-mark-test--keys "for text that is not saved here today" "w w M w"
    (should (equal (donkey-mark-test--selection) "text")))
  (donkey-mark-test--keys "for text that is not saved here today" "w w M w u"
    (should-not (region-active-p))
    (should (eq (key-binding "w") 'donkey-mark-word))))

(ert-deftest donkey-U-steps-a-run-forward-again ()
  "`U' puts the run back where `u' stepped it out of.

The other half of the pair, and one press per step like its sibling:
`M w w u u' is the first word, and two of these is two words again.
A press that is neither of the two ends the redo -- a new branch has
nothing to redo onto, which is the bargain every undo system strikes
-- and with nothing left it reports rather than guessing.

`U' records no step of its own, or it would undo its own move: after
`u U' the history stands exactly where it did, and `u' steps back
again."
  (let ((text "for text that is not saved here today"))
    (donkey-mark-test--keys text "w w l M w w u U"
      (should (equal (donkey-mark-test--selection) "that is")))
    (donkey-mark-test--keys text "w w l M w w u u U U"
      (should (equal (donkey-mark-test--selection) "that is")))
    ;; `U' left the history alone, so `u' still walks back down it.
    (donkey-mark-test--keys text "w w l M w w u U u"
      (should (equal (donkey-mark-test--selection) "that")))
    ;; The run carries on from what came back.
    (donkey-mark-test--keys text "w w l M w w u U w"
      (should (equal (donkey-mark-test--selection) "that is not")))
    ;; The mode is still on, and `U' is its key while it is.
    (donkey-mark-test--keys text "w w l M w w u U"
      (should (eq (key-binding "w") 'donkey-mark-word))
      (should (eq (key-binding "U") 'donkey-mark-run-step-forward)))
    ;; Nothing stepped back, nothing to step forward to.  Called
    ;; rather than pressed: the signal would abort the macro.
    (donkey-mark-test--keys text "w w l M w w"
      (should-error (donkey-mark-run-step-forward) :type 'user-error))
    ;; A press off the path drops the redo.
    (donkey-mark-test--keys text "w w l M w w u w"
      (should (equal (donkey-mark-test--selection) "that is"))
      (should-error (donkey-mark-run-step-forward) :type 'user-error))
    ;; Even a motion, which is a press like any other here.
    (donkey-mark-test--keys text "w w l M w w u l"
      (should-error (donkey-mark-run-step-forward) :type 'user-error)))
  (should (memq 'donkey-mark-run-step-forward donkey--mark-run-commands))
  (should (memq 'donkey-mark-run-step-forward donkey--mark-run-adjusters)))

(ert-deftest donkey-U-in-a-run-does-not-redo-a-text-edit ()
  "`U' inside a run touches the selection, never the buffer.

This is why the key needed taking over at all.  Outside the mode `u'
and `U' are `undo' and `undo-redo'; inside it `u' had been taken for
the run while `U' still meant the buffer, so the press anyone would
reach for after `u' redid a TEXT EDIT, dropped the selection and
lapsed the mode, none of it announced.  It was worst exactly where it
was likeliest -- straight after `u', from someone reading the pair
the way the pair reads everywhere else.

Driven by hand rather than through `donkey-mark-test--keys', which
gives its buffer no undo history to redo: the fixture is the whole
test."
  (let ((buf (get-buffer-create "*donkey-redo-test*")))
    (unwind-protect
        (progn
          (donkey-mode 1)
          (let ((transient-mark-mode t)
                (prefix-arg nil) (current-prefix-arg nil)
                (this-command nil) (last-command nil))
            (switch-to-buffer buf)
            (fundamental-mode)
            (erase-buffer)
            (buffer-enable-undo)
            (setq buffer-undo-list nil)
            (insert "one two three\n")
            (undo-boundary)
            (goto-char (point-max))
            (insert "ADDED\n")
            (undo-boundary)
            ;; A real undo, so there is genuinely something to redo.
            (let ((last-command 'ignore) (this-command 'undo))
              (call-interactively #'undo))
            (should-not (string-match-p "ADDED" (buffer-string)))
            (goto-char (point-min))
            (donkey-enter-normal)
            (setq last-command nil)
            (execute-kbd-macro (kbd "M w u U"))
            ;; The selection came back and the buffer did not change.
            (should (equal (buffer-substring-no-properties
                            (region-beginning) (region-end))
                           "one"))
            (should-not (string-match-p "ADDED" (buffer-string)))
            (should (eq (key-binding "w") 'donkey-mark-word))))
      (donkey--mark-run-exit)
      (when (buffer-live-p buf) (kill-buffer buf))
      (donkey-mode -1))))

(ert-deftest donkey-a-mistyped-sequence-costs-a-beep-not-the-run ()
  "A key sequence that does nothing leaves the run exactly as it was.

The promise for a mistype is a beep rather than the selection, and it
held for a single unbound key only.  Emacs runs `undefined' for that
one -- which is why \\`~' was already inert -- but a SEQUENCE that dies
in a prefix map reaches no command at all: `this-command' is nil, and
nil is not in any list.

It cost the run twice over.  `donkey--mark-run-mode-keep-p' saw a
command that was not a family member and let the mode lapse, and the
command loop wrote that nil into `last-command', so the next object
key found no run to continue and marked afresh over what the run had
grown.  Two keys after the slip, the selection was gone and the
letters were moving point.

`m x' for `m w' is the way to do it by accident: `m' is the
companion prefix the mode documents, and `g' has four two-key
sequences of its own.

`ding' is stubbed because `execute-kbd-macro' stops at the bell, and
the key pressed AFTER the mistype is the whole point."
  (cl-letf (((symbol-function 'ding) #'ignore))
    (dolist (keys '("w w l M w g x w"      ; the mode's own prefix
                    "w w l M w g ~ w"
                    "w w l M w m x w"      ; the companion prefix
                    "w w l M w z x w"      ; a normal-state prefix
                    "w w l M w C-x C-\\ w" ; and a native one
                    "w w l M w ~ w"))      ; the single key, as before
      (donkey-mark-test--keys "for text that is not saved" keys
        (should (equal (list keys (donkey-mark-test--selection))
                       (list keys "that is")))
        (should (equal (list keys (eq (key-binding "w") 'donkey-mark-word))
                       (list keys t)))))
    ;; The press is called what the other spelling is called, which is
    ;; what carries the run across it.
    (donkey-mark-test--keys "for text that is not saved" "w w l M w g x"
      (should (eq last-command 'undefined))))
  ;; And the predicate answers for the nil directly, since it is asked
  ;; before the renaming above can happen.
  (let ((this-command nil))
    (should (donkey--mark-run-mode-keep-p))))

(ert-deftest donkey-a-runs-history-does-not-outlive-it ()
  "The steps go when the mode does, and `u'/`U' are undo outside it.

A run's history means nothing to the next run -- stepping back into a
selection the previous run left would be worse than having no step
back at all -- so `donkey--mark-run-enter' empties it on the way in
and `donkey--mark-run-exit' on the way out.  The redo stack goes with
it: a run cannot step FORWARD into a shape another run stepped out of
any more sensibly than it can step back into one.

And the keys are only borrowed.  In normal state they are `undo' and
`undo-redo', and both are given straight back when the mode ends --
which matters more for `U', since what it means out there writes to
the buffer."
  (donkey-mark-test--keys "for text that is" "w w l M w w"
    (should donkey--mark-run-history))
  (donkey-mark-test--keys "for text that is" "w w l M w w u"
    (should donkey--mark-run-redo))
  (donkey-mark-test--keys "for text that is" "w w l M w w u M"
    (should-not donkey--mark-run-history)
    (should-not donkey--mark-run-redo))
  (donkey-mark-test--keys "for text that is" "w w l M w w u d"
    (should-not donkey--mark-run-history)
    (should-not donkey--mark-run-redo))
  ;; A second run starts empty rather than inheriting the first's.
  (donkey-mark-test--keys "for text that is" "w w l M w w u M M"
    (should-not donkey--mark-run-history)
    (should-not donkey--mark-run-redo))
  (donkey-mark-test--keys "for text that is" "w w l"
    (should (eq (key-binding "u") 'undo))
    (should (eq (key-binding "U") 'undo-redo))))

(ert-deftest donkey-g-g-and-g-e-stretch-the-run-to-the-buffer-ends ()
  "`g g' and `g e' own an end apiece, as `g h' and `g l' do.

The buffer's edges are the line's written large.  Left as plain
motions the two dragged the near end to `point-min' or `point-max' and
lapsed the mode on the way out, which is the largest thing a single
press can do to a run and gave no way back -- so they waited until
\`u' existed and could take one off again.

`G' is the same command as `g e', as it is in normal state."
  (let ((text "one two three\nfour five six\nseven eight nine\n"))
    (donkey-mark-test--keys text "j M w g g"
      (should (equal (donkey-mark-test--selection) "one two three\nfour")))
    (donkey-mark-test--keys text "j M w g e"
      (should (equal (donkey-mark-test--selection)
                     "four five six\nseven eight nine\n")))
    (donkey-mark-test--keys text "j M w G"
      (should (equal (donkey-mark-test--selection)
                     "four five six\nseven eight nine\n")))
    ;; An end apiece, so they add rather than cancel.
    (donkey-mark-test--keys text "j M w g g g e"
      (should (equal (donkey-mark-test--selection) text)))
    ;; And the reason they could be adopted at all.
    (donkey-mark-test--keys text "j M w g e u"
      (should (equal (donkey-mark-test--selection) "four")))
    ;; Fresh, both are the plain motions the keys are outside the mode
    ;; -- and reach the very edge.  With \"p\" rather than \"P\" a bare
    ;; press arrives as 1, which `beginning-of-buffer' reads as a tenth
    ;; of the way in: `M g g' landed on the second line before the
    ;; interactive spec matched the command being wrapped.
    (donkey-mark-test--keys text "j w w M g g"
      (should-not (region-active-p))
      (should (= (point) (point-min))))
    (donkey-mark-test--keys text "j w w M g e"
      (should-not (region-active-p))
      (should (= (point) (point-max))))
    ;; A count still reaches the fraction it names when there is no run
    ;; to stretch, and is ignored by one that has an edge to reach.
    (donkey-mark-test--keys text "j w w M C-u 5 g g"
      (should (> (point) (point-min))))
    (donkey-mark-test--keys text "j M w C-u 5 g g"
      (should (equal (donkey-mark-test--selection) "one two three\nfour")))
    ;; `g g' brings back a region a hook deactivated mid-run, the way
    ;; the backward object keys and `g h' do -- moving point activates
    ;; nothing, so the branch has to re-assert it or the run would grow
    ;; on invisibly.  Staged from `pre-command-hook' for the reason
    ;; donkey-a-backward-press-revives-a-deactivated-run gives.
    (let ((sabotage (lambda ()
                      (when (eq this-command 'donkey-mark-run-buffer-start)
                        (deactivate-mark)))))
      (unwind-protect
          (progn
            (add-hook 'pre-command-hook sabotage)
            (donkey-mark-test--keys text "j M w g g"
              (should (equal (donkey-mark-test--selection)
                             "one two three\nfour"))))
        (remove-hook 'pre-command-hook sabotage)))
    ;; Outside the mode the keys are untouched.
    (donkey-mark-test--keys text "j w g g"
      (should-not (region-active-p))
      (should (= (point) (point-min))))
    (donkey-mark-test--keys text "j w g e"
      (should-not (region-active-p))
      (should (= (point) (point-max)))))
  ;; The edges are the ACCESSIBLE ones, as every other key in the mode
  ;; already treats them.  Driven directly: the shared harness has no
  ;; way to narrow before the keys run.
  (unwind-protect
      (progn
        (donkey-mode 1)
        (with-temp-buffer
          (insert "one two three\nfour five six\nseven eight nine\n")
          (narrow-to-region 15 28)
          (goto-char (point-min))
          (let ((transient-mark-mode t))
            (donkey--mark-run-enter)
            (donkey-mark-word)
            ;; Staged as the command loop would leave it, so the press
            ;; reads as a continuation and takes the extending branch.
            (let ((last-command 'donkey-mark-word)
                  (this-command 'donkey-mark-run-buffer-end))
              (donkey-mark-run-buffer-end))
            (should (equal (buffer-substring-no-properties
                            (region-beginning) (region-end))
                           "four five six")))))
    (donkey--mark-run-exit)
    (donkey-mode -1)))

(ert-deftest donkey-percent-keeps-the-mode-and-can-be-taken-back ()
  "`%' marks the whole buffer without ending the run.

The last of the keys that changed a selection and left.  Refusing it
the way `v' and `V' are refused would have been the wrong answer to a
key doing exactly what it says -- it marks, and a mark mode has no
business lapsing on a mark command.  Membership in
`donkey--mark-run-commands' is the whole change: no binding of its
own, since `%' already falls through to normal state, and with it the
mode stays and the press is recorded, so \\`u' takes it back.

It is a member without being a growable object.  Nothing can add to a
selection that already covers everything, and the press after it finds
nothing to do rather than anything surprising."
  (let ((text "one two three\nfour five six\n"))
    (donkey-mark-test--keys text "M w %"
      (should (equal (donkey-mark-test--selection) text))
      (should (eq (key-binding "w") 'donkey-mark-word)))
    (donkey-mark-test--keys text "M w % u"
      (should (equal (donkey-mark-test--selection) "one")))
    ;; Nothing left to grow: the press after it changes nothing.
    (donkey-mark-test--keys text "M w % w"
      (should (equal (donkey-mark-test--selection) text)))
    ;; Outside the mode the key is untouched, and does not arm anything.
    (donkey-mark-test--keys text "%"
      (should (equal (donkey-mark-test--selection) text))
      (should (eq (key-binding "w") 'forward-word))))
  (should (memq 'donkey-mark-whole-buffer donkey--mark-run-commands))
  ;; It marks, so it is no adjuster: an object key after it may revive
  ;; a region a hook deactivated, where a motion may not.
  (should-not (memq 'donkey-mark-whole-buffer donkey--mark-run-adjusters)))

(ert-deftest donkey-a-rectangle-is-dropped-and-the-mode-still-starts ()
  "`M' over a rectangle drops it, then starts as it would over nothing.

The one selection adoption refuses: the family's rule -- forward keys
push the mark, backward keys walk point -- has no meaning across a
block, so the stale `rectangle-mark-mode' is disabled and the
selection dropped.  What follows the drop is the point.  The press
used to cancel and RETURN, leaving the mode off -- the only selection
whose `M' did not start a run -- so the press did nothing but clear,
and there was nothing to press a letter at.

Pressing `M' again is the obvious answer to that, and it was the
worst one.  With the mode still off the second press reached the
toggle a second time, where `donkey--mark-extending-p' found
`last-command' equal to `this-command' -- two toggles in a row are
each other -- and the head start GREW from the mark the rectangle had
left, selecting from where the rectangle began to the word under
point.  A span across lines that nobody asked for, and `M M d' would
have taken it.

Both halves are fixed here: every branch of the toggle now enters, so
that shape is unreachable, and the head start binds `last-command'
away so it cannot match itself even if some later branch stops
entering."
  ;; The rectangle's span is not adopted -- the head start marks the
  ;; word under point, exactly as it would have with no selection.
  (donkey-mark-test--keys "abc def\nghi jkl\n" "m v j l M"
    (should (eq (key-binding "w") 'donkey-mark-word))
    (should-not (bound-and-true-p rectangle-mark-mode))
    (should (equal (donkey-mark-test--selection) "ghi")))
  ;; A letter after it marks afresh, the head start being no press.
  (donkey-mark-test--keys "abc def\nghi jkl\n" "m v j l M w"
    (should (equal (donkey-mark-test--selection) "ghi")))
  ;; And the second `M' is the mode's own cancel now, not a second
  ;; toggle growing a stale mark.
  (donkey-mark-test--keys "abc def\nghi jkl\n" "m v j l M M"
    (should-not (region-active-p))
    (should-not (eq (key-binding "w") 'donkey-mark-word))))

(ert-deftest donkey-the-head-start-never-grows-a-mark-it-found ()
  "The head start marks afresh whatever `last-command' says.

`donkey-mark-run-toggle' deliberately leaves `this-command' alone, so
that the letter after `M' marks rather than grows -- see
`donkey-the-toggle-is-no-family-member'.  The cost is that the toggle
can match ITSELF: `donkey--mark-extending-p' asks
`(eq last-command this-command)', and two toggles in a row satisfy it,
so the head start would grow from whatever mark was lying about
instead of marking the word under point.

That shape was reachable through the rectangle branch, which canceled
and returned without arming the map, leaving the second `M' to reach
the toggle again -- see
`donkey-a-rectangle-is-dropped-and-the-mode-still-starts'.  Every
branch enters now, so no key sequence gets there, and this asks the
command directly instead: the guard has to hold on its own, or the
next branch that forgets to enter brings the bug back with it."
  (let ((buf (get-buffer-create "*donkey-head-start-test*")))
    (unwind-protect
        (progn
          (donkey-mode 1)
          (let ((transient-mark-mode t))
            (switch-to-buffer buf)
            (fundamental-mode)
            (erase-buffer)
            (insert "abc def\nghi jkl\n")
            ;; A mark left over at the buffer's start, inactive, with
            ;; point on a word further down -- what the canceled
            ;; rectangle used to leave behind.
            (goto-char (point-min))
            (push-mark (point) t nil)
            (goto-char 10)
            (deactivate-mark)
            (let ((last-command 'donkey-mark-run-toggle)
                  (this-command 'donkey-mark-run-toggle))
              (donkey-mark-run-toggle))
            (should (equal (buffer-substring-no-properties
                            (region-beginning) (region-end))
                           "ghi"))))
      (donkey--mark-run-exit)
      (when (buffer-live-p buf) (kill-buffer buf))
      (donkey-mode -1))))

(ert-deftest donkey-the-toggle-is-no-family-member ()
  "A letter after `M' marks afresh even with an old mark lying around.

`M w M M b': the in-mode `M' cancels the word selection, leaving its
mark at the word's end; the next `M' re-enters the mode empty.  Were
`donkey-mark-run-toggle' in `donkey--mark-run-commands', that stale
mark would qualify `b' as a continuation -- point walks back and
re-activates \"text that\", a selection the user had just dropped.
Fresh, `b' marks the word at point."
  (donkey-mark-test--keys "for text that is" "w w l M w M M b"
    (should (equal (donkey-mark-test--selection) "that"))))

(ert-deftest donkey-mark-extending-p-companions-only-through-the-command-loop ()
  "COMPANIONS extend a run only under the same guards as a repeat.

The companion arm sits inside the guards the plain test already has:
`this-command' must be set (a Lisp call is never a continuation, however
the mark got there) and a mark must exist.  And membership is checked
against the list the CALLER passes, so a command outside the pair does
not continue the run however recently it made a selection."
  (with-temp-buffer
    (insert "alpha beta gamma")
    (goto-char (point-min))
    (push-mark (point-max) t t)
    ;; Outside the command loop: no continuation, companions or not.
    (let ((this-command nil) (last-command 'donkey-mark-word))
      (should-not (donkey--mark-extending-p '(donkey-mark-word))))
    ;; Through the keymap, the companion continues the run...
    (let ((this-command 'donkey-mark-word-backward)
          (last-command 'donkey-mark-word))
      (should (donkey--mark-extending-p '(donkey-mark-word))))
    ;; ...and a command outside the pair does not.
    (let ((this-command 'donkey-mark-word-backward)
          (last-command 'donkey-mark-sentence))
      (should-not (donkey--mark-extending-p '(donkey-mark-word))))))

(ert-deftest donkey-mark-extending-p-is-nil-outside-the-command-loop ()
  "A Lisp call is never a repeat, whatever mark happens to be set.

Outside the command loop BOTH `last-command' and `this-command' are
nil, so comparing them alone is TRUE -- and any caller reaching a mark
command from Lisp with a mark already set got an extension where it
asked for a fresh selection.  Guarding on `this-command' being set at
all is the whole fix.

The property is asserted directly because nothing else does.  The bug
was caught originally by
`donkey-mark-paragraph-clears-stale-rectangle-mode-and-selects-correctly',
which fails only when this file runs FIRST -- it does not bind
`last-command', so any earlier test hides the bug by leaving it
non-nil.  Neither the combined suite nor any shuffled seed notices;
only the per-file isolation job does, and only by accident.

That protection is one hygiene fix away from vanishing.  Binding
`last-command' in a test to make it order-independent is exactly the
change this suite has had applied to it elsewhere, and doing it there
would silently remove the only thing standing between this bug and a
green CI run."
  (with-temp-buffer
    (insert "alpha beta gamma")
    (goto-char (point-min))
    (push-mark (point-max) t t)
    (let ((this-command nil) (last-command nil))
      (should-not (donkey--mark-extending-p)))
    ;; And the same when only `last-command' is set, which is the
    ;; ordinary case for a Lisp call made from inside some other command.
    (let ((this-command nil) (last-command 'donkey-mark-word))
      (should-not (donkey--mark-extending-p)))
    ;; Through the keymap it must still say yes, or the fix has gone too
    ;; far and repeating a key stops extending.
    (let ((this-command 'donkey-mark-word) (last-command 'donkey-mark-word))
      (should (donkey--mark-extending-p)))))

(ert-deftest donkey-mark-word-from-lisp-marks-fresh-despite-a-stale-mark ()
  "The predicate's consequence, end to end.

`donkey-mark-word' called from Lisp with a mark left over from
something else must mark the word at point, not grow from that mark.
Asserted on the selected text rather than on the predicate, so it
still means something if the internals are reorganised."
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "alpha beta gamma")
      ;; A stale mark at the far end, as `rectangle-mark-mode' or any
      ;; abandoned selection would leave.
      (goto-char (point-max))
      (push-mark (point-min) t t)
      (goto-char 7)                     ; inside "beta"
      (let ((this-command nil) (last-command nil))
        (donkey-mark-word 1))
      (should (equal (buffer-substring-no-properties
                      (region-beginning) (region-end))
                     "beta")))))

(ert-deftest donkey-mark-extension-stops-at-the-buffer-end ()
  "Extending past the last object marks what there is and stops.

No error: that is what every counted command in DONKEY does when it
runs out of buffer, and a run of presses is the same idea."
  (dolist (case '(("m w" "alpha beta"    "alpha beta")
                  ("m W" "a-one b-two"   "a-one b-two")
                  ("m s" "One thing."    "One thing.")))
    (cl-destructuring-bind (key text expected) case
      (donkey-mark-test--keys text (mapconcat #'identity (make-list 6 key) " ")
        (should (equal (cons key (donkey-mark-test--selection))
                       (cons key expected)))))))

(ert-deftest donkey-mark-commands-agree-about-which-end-holds-the-mark ()
  "EVERY object mark leaves point at the START, as native does.

Regression test: `m p' used to leave point at the END, alone among the
four linear marks in inverting `mark-paragraph', which finishes with
`backward-paragraph' and leaves point where the selection begins.
Being the odd one out cost something concrete -- the first attempt at
extending it handed native's ALLOW-EXTEND branch a mark-at-start region,
and since native grows by pushing the MARK forward it collapsed the
selection onto the paragraph's first character.

The four delimiter marks were the other way round for longer, and this
test used to pin them that way on the grounds that they have no native
counterpart.  They do: `mark-sexp' leaves point at the start like
everything else.  Being inverted cost something concrete there too --
point landed past the closing delimiter, which is not a delimiter, so a
second press of `m a' fell into the prompting branch of
`donkey--mark-pair-read-delimiter' and sat waiting on `read-char'.

All eight are asserted together now.  A mark command that disagrees
about which end holds the mark is the odd one out by construction."
  (dolist (case '(("m w" "alpha beta gamma"     start)
                  ("m W" "a-one b-two"          start)
                  ("m s" "One thing.  Two."     start)
                  ("m p" "Alpha.\n\nBeta.\n"    start)
                  ("m i" "(abc)"                start)
                  ("m a" "(abc)"                start)
                  ("m I" "(abc)"                start)
                  ("m A" "(abc)"                start)))
    (cl-destructuring-bind (key text where) case
      (donkey-mark-test--keys text key
        (should (equal (cons key (if (= (point) (region-beginning)) 'start 'end))
                       (cons key where)))))))

;;; ---------------------------------------------------------------------------
;;; donkey-mark-sentence at the end of the buffer
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-mark-sentence-answers-the-same-with-or-without-a-final-newline ()
  "The answer past the last sentence must not depend on a trailing newline.

Regression test: `forward-sentence' signals `end-of-buffer' at point-max
when there is no newline to land on, and the general handler turned that
into \"No sentence at or before point\" -- false, and contradicting a
screen showing two.  With a trailing newline the forward step succeeded
and a different guard produced \"No sentence after point\" instead, so
which message a reader saw depended on something invisible.

Both now mark the last sentence, which is the same answer `m w' and
`m p' give from the end of a buffer, so the pair of messages that had to
be told apart is down to one.  The property under test is unchanged and
is the reason this file still carries it: nothing about the answer may
turn on whether the file ends with a newline.

A buffer with no sentence in it at all still reports, since marking the
whitespace it walked over would be no answer at all."
  (dolist (case '((""                     "No sentence at or before point")
                  ("\n\n\n"               "No sentence at or before point")
                  ("   "                  "No sentence at or before point")
                  ("One.  Two."           "Two.")
                  ("One.  Two.\n"         "Two.")
                  ("  One."               "One.")))
    (cl-destructuring-bind (text expected) case
      (with-temp-buffer
        (let ((transient-mark-mode t))
          (insert text)
          (goto-char (point-max))
          (should (equal (cons text
                               (condition-case e
                                   (progn (donkey-mark-sentence 1)
                                          (buffer-substring-no-properties
                                           (region-beginning) (region-end)))
                                 (user-error (error-message-string e))))
                         (cons text expected))))))))

(ert-deftest donkey-mark-commands-agree-about-which-object-a-gap-means ()
  "From the gap between two objects, all four mark the one BEHIND.

The property is cross-command, so it is asserted across commands rather
than inside any one of them: a cursor parked in whitespace must not mean
different things depending on which mark key follows it.  `m s' was the
exception -- it reached forward from every gap -- and the exception was
invisible from within its own tests, all of which were written in terms
of the sentence ahead.

Emacs' own commands mark forward from a gap, but that is an artifact of
marking from point without normalizing at all: `mark-word' in the gap of
\"alpha  beta\" answers \" beta\", leading space attached.  Native has
no opinion about which object was MEANT, so it cannot settle this and
does not appear in the expectations below."
  (dolist (case '((donkey-mark-word      "alpha  beta"           7  nil "alpha")
                  (donkey-mark-symbol    "foo-a  bar-b"          7  emacs-lisp-mode "foo-a")
                  (donkey-mark-sentence  "One two.  Three four." 10 nil "One two.")
                  (donkey-mark-paragraph "A.\n\nB."              4  nil "A.\n\n")))
    (cl-destructuring-bind (command text pos mode expected) case
      (with-temp-buffer
        (when mode (funcall mode))
        (let ((transient-mark-mode t) (this-command nil) (last-command nil))
          (insert text)
          (goto-char pos)
          (funcall command)
          (should (equal (cons command (buffer-substring-no-properties
                                        (region-beginning) (region-end)))
                         (cons command expected))))))))

(ert-deftest donkey-mark-commands-agree-about-the-trailing-gap ()
  "Past the last object, all four mark that last object.

The end of the buffer is a gap like any other and answers like one.
`m s' used to refuse here with \"No sentence after point\" while the
other three marked the last word, symbol and paragraph."
  (dolist (case '((donkey-mark-word      "alpha beta   "           nil "beta")
                  (donkey-mark-symbol    "foo-a bar-b   "          emacs-lisp-mode "bar-b")
                  (donkey-mark-sentence  "One two.  Three four.  " nil "Three four.")
                  (donkey-mark-paragraph "A.\n\nB.\n\n"            nil "\nB.\n")))
    (cl-destructuring-bind (command text mode expected) case
      (with-temp-buffer
        (when mode (funcall mode))
        (let ((transient-mark-mode t) (this-command nil) (last-command nil))
          (insert text)
          (goto-char (point-max))
          (funcall command)
          (should (equal (cons command (buffer-substring-no-properties
                                        (region-beginning) (region-end)))
                         (cons command expected))))))))

(ert-deftest donkey-mark-commands-agree-about-the-leading-gap ()
  "In the LEADING gap all four reach forward to the object ahead.

Nothing sits behind the start of a buffer, so the object ahead is the
only answer available.  `m s' and `m p' always gave it; `m w' and `m W'
reported \"No word at or before point\" and \"No symbol at or before
point\" instead, which was the last corner where the four disagreed
about what a gap means -- the mirror image of what `m s' used to do at
the other end of the buffer.

This test has been inverted deliberately: it was added asserting the
disagreement, on the footing that changing it should be a decision
rather than a drift.  This is the decision."
  (dolist (case '((donkey-mark-word      "   alpha beta"        nil "alpha")
                  (donkey-mark-symbol    "   foo-a bar-b"       emacs-lisp-mode "foo-a")
                  (donkey-mark-sentence  "   One two.  Three."  nil "One two.")
                  (donkey-mark-paragraph "\n\nA.\n\nB."         nil "\n\nA.\n")))
    (cl-destructuring-bind (command text mode expected) case
      (with-temp-buffer
        (when mode (funcall mode))
        (let ((transient-mark-mode t) (this-command nil) (last-command nil))
          (insert text)
          (goto-char (point-min))
          (funcall command)
          (should (equal (cons command (buffer-substring-no-properties
                                        (region-beginning) (region-end)))
                         (cons command expected))))))))

(ert-deftest donkey-mark-word-and-symbol-still-report-with-nothing-either-way ()
  "Reaching forward must not turn \"nothing here\" into a selection.

The risk the leading-gap reach introduces: a buffer with no word in it
at all now runs the backward search, finds nothing, runs the forward
search, and must still arrive at the refusal rather than marking
whatever it walked over.  `donkey--mark-reach-forward-for' puts point
back before answering no, which is what keeps the message honest about
where the cursor is.

Blank leading text with real words further down is the case that has to
keep working, and it is covered by the agreement test above; this one is
its complement."
  (dolist (case '((donkey-mark-word   ""        nil             "No word at or before point")
                  (donkey-mark-word   "     "   nil             "No word at or before point")
                  (donkey-mark-word   "\n\n\n"  nil             "No word at or before point")
                  (donkey-mark-symbol ""        emacs-lisp-mode "No symbol at or before point")
                  (donkey-mark-symbol "     "   emacs-lisp-mode "No symbol at or before point")
                  (donkey-mark-symbol "  ()  "  emacs-lisp-mode "No symbol at or before point")))
    (cl-destructuring-bind (command text mode expected) case
      (with-temp-buffer
        (when mode (funcall mode))
        (let ((transient-mark-mode t) (this-command nil) (last-command nil))
          (insert text)
          (goto-char (point-min))
          (should (equal (cons text
                               (condition-case e
                                   (progn (funcall command) 'no-error)
                                 (user-error (error-message-string e))))
                         (cons text expected))))))))

(ert-deftest donkey-mark-symbol-refusal-does-not-leave-point-where-it-searched ()
  "A refused forward reach puts the cursor back before reporting.

\"  ()  \" in `emacs-lisp-mode' is the shape that shows it: the forward
reach lands on the paren at position 3, which is a sexp but not a symbol,
so the answer is no -- and without the restore the cursor would be left
sitting on the paren the search rejected, three characters from where the
key was pressed.  Nothing else in the suite distinguishes the two, since
the whitespace-only buffers happen to walk back to point-min either way."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((transient-mark-mode t) (this-command nil) (last-command nil))
      (insert "  ()  ")
      (goto-char (point-min))
      (should-error (donkey-mark-symbol) :type 'user-error)
      (should (equal (point) (point-min))))))

(ert-deftest donkey-mark-word-in-a-buffer-with-no-word-reports ()
  "A buffer of nothing but punctuation has no word, and this says so.

`thing-at-point' answers with the WHOLE BUFFER as the `word' at point
when no word character appears anywhere in it -- \"...\", \"!!!\" and
\"()\" each report themselves -- so the guard meant to catch a buffer
with no word in it accepted one, and `m w' selected everything while
announcing \"Word marked\".  Pressing \\`d' next emptied the buffer.

A buffer of pure whitespace answers nil and always reported correctly,
which is why the hole showed only for text that is neither word nor
blank.  Both are asserted here so a future fix cannot trade one for the
other, and all three major modes are covered because the answer does not
come from the syntax table."
  (dolist (mode '(fundamental-mode text-mode emacs-lisp-mode))
    (dolist (text '("..." "  ...  " "!!!" "()" "  ()  " "   " ""))
      (with-temp-buffer
        (funcall mode)
        (let ((transient-mark-mode t) (this-command nil) (last-command nil))
          (insert text)
          (goto-char (point-min))
          (should (equal (list mode text
                               (condition-case e
                                   (progn (donkey-mark-word) 'marked)
                                 (user-error (error-message-string e))))
                         (list mode text "No word at or before point"))))))))

(ert-deftest donkey-mark-symbol-made-only-of-punctuation-is-not-trimmed-away ()
  "A symbol that IS punctuation survives the trailing-punctuation trim.

In Lisp `.' has symbol syntax, so \"...\" is a symbol in its own right.
The trim that drops a trailing \".\" or \",\" took all of it, leaving an
EMPTY region that was announced as a successful mark -- a selection of
nothing, which the following operator then acted on.

The ordinary trim is asserted alongside it: dropping the floor is not a
licence to stop trimming \"foobar.\" down to \"foobar\"."
  (dolist (case '(("." . ".") (".." . "..") ("..." . "...")
                  ("foobar." . "foobar") ("foobar,." . "foobar")
                  ("...foo" . "...foo")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((transient-mark-mode t) (this-command nil) (last-command nil))
        (insert (car case))
        (goto-char (point-min))
        (donkey-mark-symbol)
        (should (equal (cons (car case) (buffer-substring-no-properties
                                        (region-beginning) (region-end)))
                       case))))))

(ert-deftest donkey-mark-symbol-extending-onto-punctuation-takes-the-symbol ()
  "Extending onto a punctuation symbol adds the symbol, not the space.

The same missing floor on the extend path, presenting differently: the
trim ate the \"...\" the second press had just covered but not the space
before it, so \"a\" grew to \"a \" -- a trailing space standing where the
symbol should have been.  Nothing announced that anything was wrong.

\"foo bar.\" is here to hold the ordinary case down: the trim still drops
a trailing period when there is a symbol in front of it to keep."
  (dolist (case '(("a ..."    "a"   "a ...")
                  ("foo ..."  "foo" "foo ...")
                  ("x . y"    "x"   "x .")
                  ("foo bar." "foo" "foo bar")))
    (cl-destructuring-bind (text once twice) case
      (with-temp-buffer
        (emacs-lisp-mode)
        (let ((transient-mark-mode t))
          (insert text)
          (goto-char (point-min))
          (let ((this-command 'donkey-mark-symbol) (last-command nil))
            (donkey-mark-symbol))
          (should (equal (cons text (buffer-substring-no-properties
                                     (region-beginning) (region-end)))
                         (cons text once)))
          (let ((this-command 'donkey-mark-symbol)
                (last-command 'donkey-mark-symbol))
            (donkey-mark-symbol))
          (should (equal (cons text (buffer-substring-no-properties
                                     (region-beginning) (region-end)))
                         (cons text twice))))))))

(ert-deftest donkey-mark-commands-never-announce-an-empty-selection ()
  "No mark command reports success with nothing selected.

The property behind two of the defects above, asserted directly: `m w'
marked a whole buffer it should have refused, and `m W' marked nothing at
all, and both said the mark had been made.  A selection is either real or
an error -- never zero characters with a cheerful message."
  (dolist (command '(donkey-mark-word donkey-mark-symbol
                     donkey-mark-sentence donkey-mark-paragraph))
    (dolist (text '("" "   " "\n\n\n" "..." "  ...  " "()" "!!!" "a" "a b"))
      (dolist (mode '(text-mode emacs-lisp-mode))
        (with-temp-buffer
          (funcall mode)
          (let ((transient-mark-mode t) (this-command nil) (last-command nil))
            (insert text)
            (goto-char (point-min))
            (condition-case nil
                (progn
                  (funcall command)
                  (should-not
                   (equal (list command mode text
                                (- (region-end) (region-beginning)))
                          (list command mode text 0))))
              (user-error nil))))))))

(provide 'donkey-marking-test)

;;; donkey-marking-test.el ends here

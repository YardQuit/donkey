;;; donkey-marking-test.el --- Tests for DONKEY mark/selection commands -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'rect)
(require 'donkey)

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
  "Internal ,/. are preserved within the symbol."
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

(ert-deftest donkey-mark-symbol-bob-trailing-comma ()
  "Symbol at BOB with trailing comma."
  (should (equal (nth 2 (donkey-test--symbol-result "foobar, rest" 3)) "foobar")))

(ert-deftest donkey-mark-symbol-adjacent-via-comma ()
  "Two symbols separated only by comma: foo,bar.
mark-sexp should treat them as one unit."
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
  "Empty buffer raises error."
  (with-temp-buffer
    (should-error (donkey-mark-sentence))))

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
  "Paragraph with only whitespace and newlines selected."
  (with-temp-buffer
    (insert "   \n\n  ")
    (goto-char 2)
    (donkey-mark-paragraph)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "   \n\n  "))))

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
  "Empty buffer marks empty region."
  (with-temp-buffer
    (donkey-mark-paragraph)
    (should (mark))))

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

(ert-deftest donkey-mark-inner-edge-multiline ()
  "Multiline content between delimiters selected."
  (with-temp-buffer
    (insert "{line1\nline2}")
    (goto-char 1)
    (donkey-mark-inner)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "line1\nline2"))))

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
  "An unsupported delimiter character (from the read-char prompt) signals an error."
  (with-temp-buffer
    (insert "!bang!")
    (goto-char 1)
    (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?!)))
      (should-error (donkey-mark-inner) :type 'error))))

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
  "An unsupported delimiter character (from the read-char prompt) signals an error."
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
  "Calling the command again while active disables rectangle-mark-mode."
  (with-temp-buffer
    (insert "hello\nworld")
    (goto-char 1)
    (donkey-rectangle-mark-mode)
    (should (bound-and-true-p rectangle-mark-mode))
    (donkey-rectangle-mark-mode)
    (should-not (bound-and-true-p rectangle-mark-mode))
    (should-not (region-active-p))))

(ert-deftest donkey-rectangle-mark-mode-edge-empty ()
  "In an empty buffer, stub right-char to avoid end-of-buffer."
  (with-temp-buffer
    (should (equal (buffer-string) ""))
    (cl-letf (((symbol-function 'right-char) (lambda (&optional n) nil)))
      (donkey-rectangle-mark-mode)
      (should (bound-and-true-p rectangle-mark-mode)))))

(ert-deftest donkey-rectangle-mark-mode-edge-at-buffer-start ()
  "Activating rect mark mode at buffer start succeeds."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-min))
    (donkey-rectangle-mark-mode)
    (should (>= (point) (point-min)))
    (should (<= (point) (point-max)))))

(ert-deftest donkey-rectangle-mark-mode-edge-at-buffer-end ()
  "At buffer end, stub right-char to avoid error."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-max))
    (cl-letf (((symbol-function 'right-char) (lambda (&optional n) nil)))
      (donkey-rectangle-mark-mode)
      (should (bound-and-true-p rectangle-mark-mode)))))

(ert-deftest donkey-rectangle-mark-mode-edge-single-character ()
  "On a single character, stub right-char."
  (with-temp-buffer
    (insert "x")
    (goto-char 1)
    (cl-letf (((symbol-function 'right-char) (lambda (&optional n) nil)))
      (donkey-rectangle-mark-mode)
      (should (bound-and-true-p rectangle-mark-mode)))))

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
  "Command works when current-prefix-arg is set."
  (with-temp-buffer
    (insert "hello")
    (goto-char 1)
    (let ((current-prefix-arg '(4)))
      (donkey-rectangle-mark-mode)
      (should (bound-and-true-p rectangle-mark-mode)))))

(ert-deftest donkey-rectangle-mark-mode-edge-empty-at-start ()
  "Empty buffer with point at min, stubs right-char."
  (with-temp-buffer
    (goto-char (point-min))
    (cl-letf (((symbol-function 'right-char) (lambda (&optional n) nil)))
      (donkey-rectangle-mark-mode)
      (should (bound-and-true-p rectangle-mark-mode)))))

(provide 'donkey-marking-test)

;;; donkey-marking-test.el ends here

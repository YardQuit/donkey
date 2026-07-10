;;; mule-mark-symbol-test.el --- Tests for mule-mark-symbol -*- lexical-binding: t; -*-

(require 'ert)
(require 'mule-modal)

(declare-function mule-mark-symbol nil)

(defun mule-test--symbol-result (content pos)
  "Run mule-mark-symbol in temp buffer with CONTENT at 1-based POS.
Return list (POINT MARK TEXT) describing the resulting region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert content)
    (goto-char pos)
    (mule-mark-symbol)
    (list (point)
          (or (mark t) (point))
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            ""))))

(ert-deftest mule-mark-symbol-simple ()
  "Mark simple word from middle."
  (should (equal (nth 2 (mule-test--symbol-result "foobar" 3)) "foobar")))

(ert-deftest mule-mark-symbol-from-start ()
  "Mark simple word when point is at the first character."
  (should (equal (nth 2 (mule-test--symbol-result "foobar" 1)) "foobar")))

(ert-deftest mule-mark-symbol-from-end ()
  "Mark simple word when point is at the last character."
  (should (equal (nth 2 (mule-test--symbol-result "foobar" 6)) "foobar")))

(ert-deftest mule-mark-symbol-trailing-comma ()
  "Trailing comma is omitted from selection."
  (should (equal (nth 2 (mule-test--symbol-result "foobar," 4)) "foobar")))

(ert-deftest mule-mark-symbol-trailing-period ()
  "Trailing period is omitted from selection."
  (should (equal (nth 2 (mule-test--symbol-result "foobar." 4)) "foobar")))

(ert-deftest mule-mark-symbol-trailing-both ()
  "Multiple trailing commas and periods are all omitted."
  (should (equal (nth 2 (mule-test--symbol-result "foobar,." 4)) "foobar")))

(ert-deftest mule-mark-symbol-internal-comma-period ()
  "Internal ,/. are preserved within the symbol."
  (should (equal (nth 2 (mule-test--symbol-result "word,.word" 6)) "word,.word")))

(ert-deftest mule-mark-symbol-internal-from-left ()
  "Cursor on left side of internal comma marks the full symbol."
  (should (equal (nth 2 (mule-test--symbol-result "word,.word" 4)) "word,.word")))

(ert-deftest mule-mark-symbol-internal-from-right ()
  "Cursor on right side of internal comma marks the full symbol."
  (should (equal (nth 2 (mule-test--symbol-result "word,.word" 5)) "word,.word")))

(ert-deftest mule-mark-symbol-hyphenated ()
  "Hyphenated symbols are fully marked including hyphens."
  (should (equal (nth 2 (mule-test--symbol-result "mule-mark-symbol" 8)) "mule-mark-symbol")))

(ert-deftest mule-mark-symbol-underscore ()
  "Symbols with underscores are fully marked including underscores."
  (should (equal (nth 2 (mule-test--symbol-result "foo_bar_baz" 6)) "foo_bar_baz")))

(ert-deftest mule-mark-symbol-point-at-beg ()
  "Point should end at beginning of the symbol."
  (should (= (nth 0 (mule-test--symbol-result "foobar" 4)) 1)))

(ert-deftest mule-mark-symbol-trailing-comma-at-eob ()
  "Trailing comma at end of buffer with no following text."
  (should (equal (nth 2 (mule-test--symbol-result "foobar," 4)) "foobar")))

(ert-deftest mule-mark-symbol-multiple-trailing ()
  "Multiple trailing commas and periods should all be trimmed."
  (should (equal (nth 2 (mule-test--symbol-result "foobar,,.." 4)) "foobar")))

(ert-deftest mule-mark-symbol-single-char ()
  "Single character should be marked."
  (should (equal (nth 2 (mule-test--symbol-result "x" 1)) "x")))

(ert-deftest mule-mark-symbol-with-numbers ()
  "Symbols containing numbers should be fully marked."
  (should (equal (nth 2 (mule-test--symbol-result "foo123bar" 5)) "foo123bar")))

(ert-deftest mule-mark-symbol-whitespace-before ()
  "Cursor on space with symbol to the left should mark it."
  (should (equal (nth 2 (mule-test--symbol-result "foo bar" 4)) "foo")))

(ert-deftest mule-mark-symbol-whitespace-after ()
  "Cursor on space with symbol to the right."
  (should (equal (nth 2 (mule-test--symbol-result "foo bar" 4)) "foo")))

(ert-deftest mule-mark-symbol-before-paren ()
  "Symbol immediately before a paren should not include it."
  (should (equal (nth 2 (mule-test--symbol-result "foo(bar)" 2)) "foo")))

(ert-deftest mule-mark-symbol-after-paren ()
  "Symbol immediately after a paren should not include it."
  (should (equal (nth 2 (mule-test--symbol-result "(foo bar)" 2)) "foo")))

(ert-deftest mule-mark-symbol-mark-position ()
  "Mark should be at the end of the trimmed symbol."
  ;; "foobar, rest" — mark should be at position 7 (after 'r', before ',')
  (should (= (nth 1 (mule-test--symbol-result "foobar, rest" 3)) 7)))

(ert-deftest mule-mark-symbol-region-active ()
  "Region should be active after marking."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foobar")
    (goto-char 3)
    (mule-mark-symbol)
    (should (region-active-p))))

(ert-deftest mule-mark-symbol-bare-punctuation ()
  "Cursor on standalone comma with no adjacent symbol."
  ;; This may error — testing graceful handling
  (should-error (mule-test--symbol-result "," 1) :type 'error))

(ert-deftest mule-mark-symbol-bob-trailing-comma ()
  "Symbol at BOB with trailing comma."
  (should (equal (nth 2 (mule-test--symbol-result "foobar, rest" 3)) "foobar")))

(ert-deftest mule-mark-symbol-adjacent-via-comma ()
  "Two symbols separated only by comma: foo,bar.
mark-sexp should treat them as one unit."
  (should (equal (nth 2 (mule-test--symbol-result "foo,bar" 2)) "foo,bar")))

;;; mule-mark-symbol-test.el ends here

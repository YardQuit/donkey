;;; donkey-mark-word-test.el --- Tests for donkey-mark-word -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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

;;; donkey-mark-word-test.el ends here

;;; donkey-mark-paragraph-test.el --- Tests for donkey-mark-paragraph -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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

;;; donkey-mark-paragraph-test.el ends here

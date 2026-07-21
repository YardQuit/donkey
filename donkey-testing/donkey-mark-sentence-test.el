;;; donkey-mark-sentence-test.el --- Tests for donkey-mark-sentence -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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

;;; donkey-mark-sentence-test.el ends here

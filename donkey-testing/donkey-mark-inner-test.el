;;; donkey-mark-inner-test.el --- Tests for donkey-mark-inner -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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

;;; donkey-mark-inner-test.el ends here

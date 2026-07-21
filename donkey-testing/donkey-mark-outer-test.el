;;; donkey-mark-outer-test.el --- Tests for donkey-mark-outer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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

;;; donkey-mark-outer-test.el ends here

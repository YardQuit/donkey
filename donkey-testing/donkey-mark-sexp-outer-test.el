;;; donkey-mark-sexp-outer-test.el --- Tests for donkey-mark-sexp-outer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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

;;; donkey-mark-sexp-outer-test.el ends here

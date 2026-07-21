;;; donkey-mark-sexp-inner-test.el --- Tests for donkey-mark-sexp-inner -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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

;;; donkey-mark-sexp-inner-test.el ends here

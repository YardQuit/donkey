;;; donkey-pair-test.el --- Tests for DONKEY pairing while typing -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'donkey)
(require 'donkey-test-keys)
(require 'sgml-mode)   ; html-mode, the worked case for inclusions

;; Declared so a `let' over them in a test is DYNAMIC.  Neither package
;; is loaded in the suite, and in a lexical-binding file a `let' over an
;; undeclared symbol binds lexically, where `bound-and-true-p' inside
;; `donkey--wrap-pairing-package-here' cannot see it.
(defvar electric-pair-mode)
(defvar smartparens-mode)

(defconst donkey-pair-test--source-dir
  (or (and load-file-name (file-name-directory (directory-file-name
                                                (file-name-directory load-file-name))))
      default-directory)
  "Directory holding donkey.el and README.org, for the test that reads them.

Defined here rather than taken from another test file: the suite runs
each file on its own as well as together, and a test file is not a
library for other test files to require.")

;; A test that says a pairing package is live binds `electric-pair-mode'
;; to t, and if the REAL `electric-pair-post-self-insert-function' is on
;; `post-self-insert-hook' it then does its work for real, on top of
;; whatever the test was simulating.  It gets on to that hook easily:
;; turning `electric-pair-local-mode' on and off again leaves the
;; function there with the mode variable back at nil, so whether it is
;; there depends on which other tests have run.  Every such test below
;; therefore WRITES the hook rather than adding to it.

(defmacro donkey-pair-test--typing (name mode bindings text keys &rest body)
  "Type KEYS into a displayed buffer of TEXT with `donkey-pair-mode' on.

NAME, MODE, BINDINGS, TEXT, KEYS and BODY are
`donkey-test-keys--harness' arguments.  The mode is global and hangs
two hooks, so it is turned off again whatever BODY does."
  (declare (indent 5))
  `(unwind-protect
       (progn
         (donkey-pair-mode 1)
         (donkey-test-keys--harness ,name ,mode ,bindings ,text ,keys ,@body))
     (donkey-pair-mode -1)))

;;; ---------------------------------------------------------------------------
;;; What a delimiter does while you type
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-an-opener-writes-its-closer-and-sits-between ()
  "An opening delimiter pairs, with point left between the halves."
  (dolist (case '(("(" "()") ("[" "[]") ("{" "{}")))
    (donkey-pair-test--typing "*donkey-pair-open*" #'text-mode () ""
        (concat "i " (car case))
      (should (equal (buffer-string) (cadr case)))
      (should (= (point) 2)))))

(ert-deftest donkey-pair-a-symmetric-delimiter-closes-itself ()
  "A delimiter that opens and closes with one character still pairs."
  (donkey-pair-test--typing "*donkey-pair-sym*" #'text-mode () "" "i \""
    (should (equal (buffer-string) "\"\""))
    (should (= (point) 2))))

(ert-deftest donkey-pair-a-closer-steps-over-the-one-already-there ()
  "Typing the closing half where it stands moves point past it."
  (donkey-pair-test--typing "*donkey-pair-skip*" #'text-mode () "" "i ( )"
    (should (equal (buffer-string) "()"))
    (should (= (point) 3))))

(ert-deftest donkey-pair-a-symmetric-closer-steps-over-too ()
  "The second press of a symmetric delimiter steps over the first."
  (donkey-pair-test--typing "*donkey-pair-skip-sym*" #'text-mode () "" "i \" a \""
    (should (equal (buffer-string) "\"a\""))
    (should (= (point) 4))))

(ert-deftest donkey-pair-text-typed-inside-stays-inside ()
  "The closing half stays put while you type between the halves."
  (donkey-pair-test--typing "*donkey-pair-inside*" #'text-mode () "" "i ( a b c"
    (should (equal (buffer-string) "(abc)"))
    (should (= (point) 5))))

(ert-deftest donkey-pair-nests ()
  "A pair opened inside a pair closes inside it."
  (donkey-pair-test--typing "*donkey-pair-nest*" #'text-mode () "" "i ( [ x"
    (should (equal (buffer-string) "([x])"))))

(ert-deftest donkey-pair-leaves-a-delimiter-it-was-not-given ()
  "A character outside `donkey-pair-delimiters' types as itself."
  (donkey-pair-test--typing "*donkey-pair-other*" #'text-mode () "" "i < : _ *"
    (should (equal (buffer-string) "<:_*"))))

(ert-deftest donkey-pair-an-empty-delimiter-list-pairs-nothing ()
  "An empty `donkey-pair-delimiters' leaves the mode on and pairing off."
  (donkey-pair-test--typing "*donkey-pair-none*" #'text-mode
      ((donkey-pair-delimiters nil)) "" "i ( \""
    (should (equal (buffer-string) "(\""))))

(ert-deftest donkey-pair-all-means-every-pair-the-table-knows ()
  "`all' pairs the ordinary punctuation too, which is why it is not the default."
  (donkey-pair-test--typing "*donkey-pair-all*" #'text-mode
      ((donkey-pair-delimiters 'all)) "" "i :"
    (should (equal (buffer-string) "::")))
  (donkey-pair-test--typing "*donkey-pair-all-2*" #'text-mode
      ((donkey-pair-delimiters 'all)) "" "i <"
    (should (equal (buffer-string) "<>"))))

(ert-deftest donkey-pair-safe-leaves-the-ordinary-punctuation-alone ()
  "`safe', the default, drops what `donkey-pair-safe-exclusions' names.

Every one of those characters is text far more often than it is a
delimiter, and each of them IS in the table, so `all' would pair it."
  (dolist (char donkey-pair-safe-exclusions)
    (should (assq char donkey-mark-pair-delimiters))
    (should-not (memq char (donkey--pair-characters)))))

(ert-deftest donkey-readme-safe-table-says-what-safe-does ()
  "The README table of what `safe' types matches the code.

It is the table a reader checks before wondering why a key did
nothing, so every cell is read back out of README.org and asked of
`donkey--pair-characters' and `donkey-pair-safe-exclusions',
rule 30.

The vertical bar cannot sit in an Org table cell, so the README
spells it with the \\vert entity and it is put back before the row is
scanned -- the same trick, and the same reason, as the binding
tables."
  (let (wrong (rows 0))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "README.org" donkey-pair-test--source-dir))
      (goto-char (point-min))
      (while (re-search-forward "\\\\vert" nil t) (replace-match "=|=" t t))
      (goto-char (point-min))
      (should (re-search-forward "^| Opener +| =safe= +| Why" nil t))
      (forward-line 2)
      (while (looking-at "^| \\(.+?\\) +| \\(yes\\|no\\) +|")
        ;; Both captures read BEFORE the helper runs: it matches, and
        ;; matching clobbers the match data the second one needs.
        (let* ((openers (match-string 1))
               (want (equal (match-string 2) "yes"))
               (chars (donkey-pair-test--chars-named openers)))
          (should chars)
          (setq rows (1+ rows))
          (dolist (char chars)
            ;; Every character the table names ships in the pair table.
            (unless (assq char donkey-mark-pair-delimiters)
              (push (format "%c is in the README table but not in the pair table" char)
                    wrong))
            (let ((typed (and (memq char (donkey--pair-characters)) t)))
              (unless (eq typed want)
                (push (format "%c: README says %s, safe says %s"
                              char (if want "yes" "no") (if typed "yes" "no"))
                      wrong)))
            ;; And "no" must mean it is on the punctuation list, not
            ;; merely absent for some other reason.
            (when (and (not want)
                       (not (memq char donkey-pair-safe-exclusions)))
              (push (format "%c is marked no but is not on the punctuation list" char)
                    wrong))))
        (forward-line 1)))
    (should (> rows 5))
    (should (equal wrong nil))))

(ert-deftest donkey-pair-never-pairs-the-angle-bracket ()
  "DONKEY\\='s own pairing leaves `<' alone in every mode.

The README says so, and says that where `<' does close itself -- Org
and HTML -- it is Emacs doing it and not DONKEY.  Less-than is
less-than in most buffers, so `<' is on
`donkey-pair-safe-exclusions'."
  (dolist (mode '(org-mode text-mode emacs-lisp-mode python-mode html-mode))
    (donkey-pair-test--typing "*donkey-pair-angle*" mode () "" "i <"
      (should (equal (buffer-string) "<")))))

(ert-deftest donkey-readme-table-of-what-emacs-pairs-is-true ()
  "Every cell of the README\\='s `electric-pair-mode' table is what Emacs does.

The table tells a reader what they already have before turning
anything on, and what DONKEY therefore adds.  It states an enumerable
fact about another package, so it is read back out of the README and
checked against the live behaviour rather than repeated here (rule
30).  It has been the same on Emacs 29.1, 30.1, 30.2 and 31.1.

A mode that will not load is skipped rather than failed."
  (let (wrong (rows 0))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "README.org" donkey-pair-test--source-dir))
      (goto-char (point-min))
      (should (re-search-forward "^| Mode +| =(=" nil t))
      (let ((columns '(?\( ?\[ ?{ ?\" ?< ?\')))
        (forward-line 2)
        (while (looking-at "^| \\([a-z, -]+?\\) +|\\(.*\\)|[ \t]*$")
          ;; Both taken before either split: `split-string' matches,
          ;; and matching clobbers the match data the second one needs.
          (let* ((names (match-string 1))
                 (row (match-string 2))
                 (modes (split-string names ", " t " "))
                 (cells (mapcar #'string-trim (split-string row "|" t))))
            (setq rows (1+ rows))
            (dolist (name modes)
              (let ((mode (intern (concat name "-mode"))))
                (when (fboundp mode)
                  (cl-loop for char in columns
                           for want in cells
                           do (let ((got (donkey-pair-test--emacs-pairs-p mode char)))
                                (unless (eq got (equal want "yes"))
                                  (push (format "%s %c: README says %s, Emacs says %s"
                                                mode char want (if got "yes" "no"))
                                        wrong))))))))
          (forward-line 1))))
    (should (> rows 4))
    (should (equal wrong nil))))

(ert-deftest donkey-readme-configuration-defaults-are-the-real-ones ()
  "Every value the worked configuration marks \"; default\" is one.

The section shows twenty options set to the value they already have,
so a reader can see the shipped answer beside the knob.  A default
that moves leaves those lines quietly wrong, which is the worst kind
of wrong in a configuration people copy (rule 30).

Read out of README.org rather than repeated here, so the two cannot
drift."
  (let (wrong (checked 0))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "README.org" donkey-pair-test--source-dir))
      (emacs-lisp-mode)
      (goto-char (point-min))
      ;; Read the whole `setopt' form with `forward-sexp' rather than
      ;; matching one line: a value long enough to wrap would otherwise
      ;; be skipped in silence, which is the failure this test exists
      ;; to prevent.
      (while (re-search-forward "^ *(setopt \\(donkey-[a-z0-9-]+\\)\\_>" nil t)
        (let ((symbol (intern (match-string 1)))
              (start (progn (goto-char (match-beginning 0))
                            (skip-chars-forward " ")
                            (point))))
          (forward-sexp)
          (when (looking-at " *; *default *$")
            (let* ((form (car (read-from-string
                               (buffer-substring start (point)))))
                   (said (nth 2 form))
                   (said (if (and (consp said) (eq (car said) 'quote))
                             (cadr said)
                           said)))
              (setq checked (1+ checked))
              (cond
               ((not (boundp symbol))
                (push (format "%s is named but does not exist" symbol) wrong))
               ((not (equal said (default-value symbol)))
                (push (format "%s: README says %S, the default is %S"
                              symbol said (default-value symbol))
                      wrong))))))))
    ;; An exact count, not a floor: dropping the "; default" marker
    ;; from a line takes that line out of the scan, and a floor cannot
    ;; tell that from a section that never had it.
    (should (= checked 20))
    (should (equal wrong nil))))

(ert-deftest donkey-readme-configuration-names-every-option ()
  "The worked configuration names every user option the package has.

An option nobody documents is one nobody finds.  The section is the
one place a reader meets them all together, so it is checked against
the file rather than against a list kept here."
  (let ((section
         (with-temp-buffer
           (insert-file-contents
            (expand-file-name "README.org" donkey-pair-test--source-dir))
           (goto-char (point-min))
           (let ((start (progn (re-search-forward "^\\* A Complete Configuration")
                               (point)))
                 (end (progn (re-search-forward "^\\* When Something Is Not Right")
                             (point))))
             (buffer-substring start end))))
        missing)
    (mapatoms
     (lambda (symbol)
       (when (and (custom-variable-p symbol)
                  (string-prefix-p "donkey-" (symbol-name symbol))
                  (not (string-match-p (regexp-quote (symbol-name symbol)) section)))
         (push (symbol-name symbol) missing))))
    (should (equal (sort missing #'string<) nil))))

(ert-deftest donkey-readme-option-table-counts-are-the-real-ones ()
  "Every option the table sizes by a number is that size.

Four cells say how big a default is rather than printing it, as 21
pairs or 13 characters, and a default that grows leaves the number
behind.  Read out of README.org so the two cannot drift."
  (let (wrong (checked 0))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "README.org" donkey-pair-test--source-dir))
      (goto-char (point-min))
      (while (re-search-forward
              "^| =\\(donkey-[a-z0-9-]+\\)= *| *\\([0-9]+\\) +[a-z]+ *|" nil t)
        ;; Both groups before anything else matches: `string-to-number'
        ;; is safe, but a later search would clobber the match data.
        (let ((symbol (intern (match-string 1)))
              (said (string-to-number (match-string 2))))
          (setq checked (1+ checked))
          (cond
           ((not (boundp symbol))
            (push (format "%s is named but does not exist" symbol) wrong))
           ((not (listp (default-value symbol)))
            (push (format "%s is sized by a number but is not a list" symbol)
                  wrong))
           ((not (= said (length (default-value symbol))))
            (push (format "%s: README says %d, the default holds %d"
                          symbol said (length (default-value symbol)))
                  wrong))))))
    (should (= checked 4))
    (should (equal wrong nil))))

(ert-deftest donkey-readme-syntax-class-table-is-true ()
  "Every cell of the README table of syntax classes is what Emacs says.

That table is what makes the rest of the section make sense -- why a
brace does not pair in Lisp, why an angle bracket pairs in Org -- so a
stale cell would leave a reader with an explanation of the wrong
thing.  Read back out of the README rather than repeated here (rule
30); identical on Emacs 29.1, 30.1, 30.2 and 31.1.

A mode that will not load is skipped rather than failed."
  (let ((modes '(text-mode org-mode emacs-lisp-mode
                 python-mode html-mode latex-mode))
        (rows 0)
        wrong)
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "README.org" donkey-pair-test--source-dir))
      (goto-char (point-min))
      (should (re-search-forward "^| Character +| text +|" nil t))
      (forward-line 2)
      (while (looking-at "^| \\(.+?\\) +|\\(.*\\)|[ \t]*$")
        ;; Both captures taken before either split: `split-string'
        ;; matches, and matching clobbers the match data.
        (let* ((keys (match-string 1))
               (row (match-string 2))
               (chars (donkey-pair-test--chars-named keys))
               (cells (mapcar #'string-trim (split-string row "|" t))))
          (should chars)
          (setq rows (1+ rows))
          (cl-loop for mode in modes
                   for want in cells
                   do (when (fboundp mode)
                        (dolist (char chars)
                          (let ((said (string-trim want "\\*" "\\*"))
                                (got (donkey-pair-test--syntax-word mode char)))
                            (unless (equal said got)
                              (push (format "%s %c: README says %s, Emacs says %s"
                                            mode char said got)
                                    wrong)))))))
        (forward-line 1)))
    (should (> rows 3))
    (should (equal wrong nil))))

(defun donkey-pair-test--chars-named (cell)
  "Return the characters a README table CELL names between equals signs."
  (let (chars (start 0))
    (while (string-match "=\\(.\\)=" cell start)
      (push (aref (match-string 1 cell) 0) chars)
      (setq start (match-end 0)))
    (nreverse chars)))

(defun donkey-pair-test--syntax-word (mode char)
  "Return the README's word for the syntax class of CHAR in MODE."
  (with-temp-buffer
    (funcall mode)
    (pcase (char-syntax char)
      (?\( "open")
      (?\" "string")
      (?_  "symbol")
      (?w  "word")
      (?\' "quote")
      (?.  "punct")
      (other (char-to-string other)))))

(defun donkey-pair-test--emacs-pairs-p (mode char)
  "Return non-nil if `electric-pair-mode' closes CHAR in MODE."
  (let ((buffer (get-buffer-create " *donkey-pair-emacs*")))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (funcall mode)
          (electric-pair-local-mode 1)
          (let ((last-command-event char))
            (self-insert-command 1 char))
          (> (buffer-size) 1))
      (kill-buffer buffer))))

(ert-deftest donkey-a-delimiter-can-be-kept-out-of-the-typing-set ()
  "`donkey-pair-safe-exclusions' drops one delimiter from `safe'.

A pair added to the table so that \\\\[donkey-mark-inner] can select
between two of them is a pair `safe' will also type, and for a letter
that is never wanted: typing it would give you two.  Naming it here
keeps it markable and wrappable and stops it pairing."
  (donkey-pair-test--typing "*donkey-pair-excl-x*" #'text-mode
      ((donkey-mark-pair-delimiters (cons '(?X . ?X) donkey-mark-pair-delimiters)))
      "" "i f a X"
    (should (equal (buffer-string) "faXX")))
  (donkey-pair-test--typing "*donkey-pair-excl-x2*" #'text-mode
      ((donkey-mark-pair-delimiters (cons '(?X . ?X) donkey-mark-pair-delimiters))
       (donkey-pair-safe-exclusions (cons ?X donkey-pair-safe-exclusions)))
      "" "i f a X"
    (should (equal (buffer-string) "faX")))
  ;; and it is still a pair for everything else the table feeds
  (let ((donkey-mark-pair-delimiters (cons '(?X . ?X) donkey-mark-pair-delimiters))
        (donkey-pair-safe-exclusions (cons ?X donkey-pair-safe-exclusions)))
    (should (assq ?X donkey-mark-pair-delimiters))
    (should (memq ?X (donkey--wrap-delimiter-characters)))
    (should-not (memq ?X (donkey--pair-characters)))))

(ert-deftest donkey-the-exclusion-list-is-read-and-not-trusted ()
  "No value of `donkey-pair-safe-exclusions' makes typing signal.

It is a user option now rather than a constant, and it is read from
`post-self-insert-hook', so it is coerced like the rest: a value that
is not a list reads as the empty list, and anything in it that is not
a character is ignored (rules 3 and 81)."
  (dolist (value '(42 "nonsense" nope (?\( "x" nil 3.5)))
    (donkey-pair-test--typing "*donkey-pair-excl-junk*" #'text-mode
        ((donkey-pair-safe-exclusions value)) "" ""
      (execute-kbd-macro (kbd "i ( a"))
      (should (stringp (buffer-string)))))
  ;; a non-list means nothing is excluded, so `safe' is the whole table
  (let ((donkey-pair-safe-exclusions 42))
    (should (equal (donkey--pair-characters)
                   (mapcar #'car (seq-filter #'consp (donkey--pair-table)))))))

(ert-deftest donkey-pair-safe-takes-a-pair-you-added-to-the-table ()
  "A pair added to the table is one you can type, with nothing said twice.

That is what `safe' is for: it reads the table rather than a second
list, so `(?# . ?#)' put there for \\=`m i\\=' is typed as well."
  (donkey-pair-test--typing "*donkey-pair-added*" #'text-mode
      ((donkey-mark-pair-delimiters (cons '(?# . ?#) donkey-mark-pair-delimiters)))
      "" "i #"
    (should (equal (buffer-string) "##")))
  ;; And named explicitly, the same.
  (donkey-pair-test--typing "*donkey-pair-added-named*" #'text-mode
      ((donkey-mark-pair-delimiters (cons '(?# . ?#) donkey-mark-pair-delimiters))
       (donkey-pair-delimiters '(?#)))
      "" "i #"
    (should (equal (buffer-string) "##"))))

;;; ---------------------------------------------------------------------------
;;; The count
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-ignores-a-count ()
  "A count types that many delimiters and pairs none of them."
  (donkey-pair-test--typing "*donkey-pair-count*" #'text-mode () "" "i C-u 3 ("
    (should (equal (buffer-string) "((("))))

;;; ---------------------------------------------------------------------------
;;; DEL
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-del-between-the-halves-takes-both ()
  "One press of DEL empties an empty pair."
  (donkey-pair-test--typing "*donkey-pair-del*" #'text-mode () "" "i ( DEL"
    (should (equal (buffer-string) ""))))

(ert-deftest donkey-pair-del-elsewhere-is-the-major-modes ()
  "DEL away from an empty pair deletes one character, as it always did."
  (donkey-pair-test--typing "*donkey-pair-del-plain*" #'text-mode () "" "i a b DEL"
    (should (equal (buffer-string) "a")))
  (donkey-pair-test--typing "*donkey-pair-del-inside*" #'text-mode () "" "i ( a DEL DEL"
    (should (equal (buffer-string) ""))))

(ert-deftest donkey-pair-del-leaves-a-pair-it-was-not-given ()
  "DEL between two characters DONKEY does not pair is the major mode\\='s.

The halves match each other in the table, which is not enough: the
delimiter has to be one `donkey-pair-delimiters' asked for."
  (donkey-pair-test--typing "*donkey-pair-del-other*" #'text-mode () ""
      "i < > C-b DEL"
    (should (equal (buffer-string) ">"))))

(ert-deftest donkey-pair-del-under-a-count-deletes-characters ()
  "A count on DEL means that many characters, pair or no pair."
  (donkey-pair-test--typing "*donkey-pair-del-count*" #'text-mode () "" "i a ( C-u 2 DEL"
    (should (equal (buffer-string) ")"))))

(ert-deftest donkey-pair-del-is-handed-back-where-no-pair-stands ()
  "The filter answers with nothing unless point is between the halves.

Nil is what hands DEL to the major mode, so a buffer with no pair at
point keeps its own binding -- and so does \\[describe-bindings],
which runs the filter in a buffer of its own."
  (unwind-protect
      (progn
        (donkey-pair-mode 1)
        (with-temp-buffer
          (text-mode)
          (insert "ab")
          (should-not (donkey--pair-delete-filter 'donkey-pair-delete-pair))
          (erase-buffer)
          (insert "()")
          (goto-char 2)
          (should (eq (donkey--pair-delete-filter 'donkey-pair-delete-pair)
                      'donkey-pair-delete-pair))))
    (donkey-pair-mode -1)))

;;; ---------------------------------------------------------------------------
;;; Where it does not pair
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-refuses-a-read-only-buffer ()
  "Neither half of a pair reaches a buffer that cannot be written to.

`self-insert-command' signals before `post-self-insert-hook' runs, so
the pairer never sees the press, and `donkey-pair-delete-pair' signals
the same way and leaves the pair standing.  The refusal is Emacs\\='s
own in both cases, and nothing is modified first."
  (unwind-protect
      (progn
        (donkey-pair-mode 1)
        (with-temp-buffer
          (text-mode)
          (insert "x")
          (goto-char (point-min))
          (setq buffer-read-only t)
          (should-error (let ((last-command-event ?\()) (self-insert-command 1))
                        :type 'buffer-read-only)
          (should (equal (buffer-string) "x"))
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "()")
          (goto-char 2)
          (setq buffer-read-only t)
          (should-error (donkey-pair-delete-pair) :type 'buffer-read-only)
          (should (equal (buffer-string) "()"))))
    (donkey-pair-mode -1)))

(ert-deftest donkey-pair-normal-state-does-not-pair ()
  "Normal state suppresses typing, so no delimiter pairs there.

The press is wrapped because a wrap key with nothing selected rings
the bell, and a bell ends a keyboard macro in a terminal frame though
not in batch.  What is pinned either way is that nothing was typed and
the state did not change."
  (donkey-pair-test--typing "*donkey-pair-normal*" #'text-mode () "" ""
    (condition-case nil (execute-kbd-macro (kbd "(")) (error nil) (quit nil))
    (should (equal (buffer-string) ""))
    (should donkey-normal-mode)
    (should-not (bound-and-true-p donkey-insert-mode))))

(ert-deftest donkey-pair-stays-out-of-smartparens-way ()
  "DONKEY writes nothing of its own where Smartparens is on."
  (donkey-pair-test--typing "*donkey-pair-sp*" #'text-mode
      ((smartparens-mode t)
       (post-self-insert-hook '(donkey--pair-post-self-insert)))
      "" "i ("
    (should (equal (buffer-string) "("))))

(ert-deftest donkey-pair-stand-down-can-be-turned-off ()
  "With `donkey-pair-stand-down' nil, DONKEY pairs beside Smartparens."
  (donkey-pair-test--typing "*donkey-pair-insist*" #'text-mode
      ((smartparens-mode t) (donkey-pair-stand-down nil)
       (post-self-insert-hook '(donkey--pair-post-self-insert)))
      "" "i ("
    (should (equal (buffer-string) "()"))))

(ert-deftest donkey-pair-writes-nothing-itself-where-emacs-is-pairing ()
  "Emacs keeps the job wherever `electric-pair-mode' is on.

DONKEY hands it the delimiters instead of competing, so its own hook
writes nothing even with `donkey-pair-stand-down' nil -- that option
is about Smartparens and does not reach this."
  (dolist (stand-down '(t nil))
    (donkey-pair-test--typing "*donkey-pair-elec*" #'text-mode
        ((electric-pair-mode t) (donkey-pair-stand-down stand-down)
         (post-self-insert-hook '(donkey--pair-post-self-insert)))
        "" "i ("
      (should (equal (buffer-string) "(")))))

(ert-deftest donkey-pair-leaves-its-own-excluded-modes-alone ()
  "`donkey-pair-excluded-modes' stops the pairing and not the state."
  (donkey-pair-test--typing "*donkey-pair-excl*" #'text-mode
      ((donkey-pair-excluded-modes '(text-mode))) "" "i ("
    (should (equal (buffer-string) "("))))

(ert-deftest donkey-pair-leaves-donkeys-excluded-modes-alone ()
  "Nothing pairs where DONKEY itself stays out of the way."
  (let ((donkey-excluded-modes '(text-mode)))
    (with-temp-buffer
      (text-mode)
      (should (donkey--pair-off-here-p)))))

(ert-deftest donkey-pair-leaves-the-minibuffer-alone ()
  "The minibuffer is not a buffer DONKEY pairs in."
  (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
    (with-temp-buffer
      (text-mode)
      (should (donkey--pair-off-here-p)))))

(ert-deftest donkey-pair-honors-a-delimiter-exception-for-the-mode ()
  "A delimiter named in `donkey-pair-delimiter-exceptions' types as itself."
  (donkey-pair-test--typing "*donkey-pair-except*" #'text-mode
      ((donkey-pair-delimiters '(?\( ?\"))
       (donkey-pair-delimiter-exceptions '((text-mode ?\"))))
      "" "i ( \""
    (should (equal (buffer-string) "(\")"))))

(ert-deftest donkey-pair-includes-a-delimiter-in-one-mode-only ()
  "A delimiter named in `donkey-pair-delimiter-inclusions' pairs there.

The list in force leaves the less-than sign out, and the mode
`html-mode' asks for it back; no other buffer gets it."
  (donkey-pair-test--typing "*donkey-pair-incl*" #'html-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
      "" "i < s c r i p t"
    (should (equal (buffer-string) "<script>")))
  (donkey-pair-test--typing "*donkey-pair-incl-other*" #'text-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
      "" "i <"
    (should (equal (buffer-string) "<"))))

(ert-deftest donkey-pair-an-inclusion-reaches-a-derived-mode ()
  "A mode deriving from the one named gets the inclusion too.

`mhtml-mode' derives from `html-mode' and is not named itself."
  (skip-unless (fboundp 'mhtml-mode))
  (donkey-pair-test--typing "*donkey-pair-incl-derived*" #'mhtml-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
      "" "i <"
    (should (equal (buffer-string) "<>"))))

(ert-deftest donkey-pair-an-inclusion-adds-rather-than-replaces ()
  "The rest of the list goes on pairing in the mode that names one."
  (donkey-pair-test--typing "*donkey-pair-incl-adds*" #'html-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
      "" "i ( ["
    (should (equal (buffer-string) "([])"))))

(ert-deftest donkey-pair-an-inclusion-steps-over-its-own-closer ()
  "Typing the closing half where it already stands steps over it."
  (donkey-pair-test--typing "*donkey-pair-incl-skip*" #'html-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
      "" "i < >"
    (should (equal (buffer-string) "<>"))
    (should (= (point) 3))))

(ert-deftest donkey-pair-an-exception-beats-an-inclusion ()
  "Named in both for one mode, the character types as itself.

The exception is asked first and answers for the press, so the two
options cannot disagree about a buffer."
  (donkey-pair-test--typing "*donkey-pair-incl-vs*" #'html-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<)))
       (donkey-pair-delimiter-exceptions '((html-mode ?<))))
      "" "i <"
    (should (equal (buffer-string) "<"))))

(ert-deftest donkey-pair-an-inclusion-does-not-reach-an-excluded-mode ()
  "A mode where pairing is off pairs nothing, inclusion or not."
  (donkey-pair-test--typing "*donkey-pair-incl-off*" #'html-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<)))
       (donkey-pair-excluded-modes '(html-mode)))
      "" "i <"
    (should (equal (buffer-string) "<"))))

(ert-deftest donkey-pair-an-inclusion-names-only-what-the-table-carries ()
  "A character the pair table has no closing half for is ignored.

`donkey--pair-close-for' would answer nil and the closing half is
written into the buffer, so this is the guard that keeps a signal out
of `post-self-insert-hook'."
  (donkey-pair-test--typing "*donkey-pair-incl-nocloser*" #'html-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?Z))))
      "" "i Z"
    (should (equal (buffer-string) "Z")))
  (with-temp-buffer
    (html-mode)
    (let ((donkey-pair-delimiter-inclusions '((html-mode ?Z ?< ?\s))))
      (should (equal (donkey--pair-inclusions-here) '(?<))))))

(ert-deftest donkey-pair-del-takes-both-halves-of-an-included-pair ()
  "\\[donkey-pair-delete-pair] knows the pair the mode asked for.

The typing hook and \\`DEL' read one list between them, so a pair one
of them writes is a pair the other takes back.  Read the global list
instead and \\`DEL' leaves the closing half standing."
  (donkey-pair-test--typing "*donkey-pair-incl-del*" #'html-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
      "" "i < DEL"
    (should (equal (buffer-string) "")))
  ;; Not in a mode that never asked: there DEL is the major mode's.
  (donkey-pair-test--typing "*donkey-pair-incl-del-other*" #'text-mode
      ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
      "" "i < >"
    (should (equal (buffer-string) "<>"))))

(ert-deftest donkey-pair-the-platform-line-counts-this-buffer ()
  "The report says how many delimiters pair HERE, not everywhere.

Its own docstring promises the answer for THIS buffer, so a mode that
adds one is a mode the number follows."
  (unwind-protect
      (progn
        (donkey-pair-mode 1)
        (let ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
          (with-temp-buffer
            (html-mode)
            (should (equal (donkey--debug-pair-line)
                           "on, DONKEY pairs (9 delimiters)")))
          (with-temp-buffer
            (text-mode)
            (should (equal (donkey--debug-pair-line)
                           "on, DONKEY pairs (8 delimiters)")))))
    (donkey-pair-mode -1)))

(ert-deftest donkey-pair-the-platform-line-counts-only-what-pairs ()
  "The report never claims a delimiter the buffer will not pair.

Three ways it did.  A per-mode EXCEPTION is taken off the press and
not off the list, so a Lisp buffer that pairs eight said nine while
the hash sat in the table.  An INCLUSION naming a character the list
already had counted it twice.  An inclusion and an exception naming
one character counted it and then refused it."
  (unwind-protect
      (progn
        (donkey-pair-mode 1)
        ;; An exception in force: the shipped Lisp rows take the hash.
        (let ((donkey-mark-pair-delimiters
               (cons '(?# . ?#) donkey-mark-pair-delimiters)))
          (with-temp-buffer
            (text-mode)
            (should (equal (donkey--debug-pair-line)
                           "on, DONKEY pairs (9 delimiters)")))
          (with-temp-buffer
            (emacs-lisp-mode)
            (should (equal (donkey--debug-pair-line)
                           "on, DONKEY pairs (8 delimiters)"))))
        ;; An inclusion of a character the list already carries.
        (let ((donkey-pair-delimiter-inclusions '((text-mode ?\())))
          (with-temp-buffer
            (text-mode)
            (should (equal (donkey--debug-pair-line)
                           "on, DONKEY pairs (8 delimiters)"))))
        ;; Included and excepted at once: the exception wins, and counts.
        (let ((donkey-pair-delimiter-inclusions '((html-mode ?<)))
              (donkey-pair-delimiter-exceptions '((html-mode ?<))))
          (with-temp-buffer
            (html-mode)
            (should (equal (donkey--debug-pair-line)
                           "on, DONKEY pairs (8 delimiters)"))))
        ;; And an inclusion that really does add one still says nine.
        (let ((donkey-pair-delimiter-inclusions '((html-mode ?<))))
          (with-temp-buffer
            (html-mode)
            (should (equal (donkey--debug-pair-line)
                           "on, DONKEY pairs (9 delimiters)")))))
    (donkey-pair-mode -1)))

(ert-deftest donkey-the-inclusion-list-is-read-and-not-trusted ()
  "No value of `donkey-pair-delimiter-inclusions' makes typing signal."
  (dolist (value (list 'junk 42 "text" '(bad) '((html-mode . 5))
                       '(("string" ?<)) '((nil ?<)) '((html-mode "x" nil ?<))))
    (donkey-pair-test--typing "*donkey-pair-incl-junk*" #'html-mode
        ((donkey-pair-delimiter-inclusions value))
        "" "i ("
      (should (equal (buffer-string) "()")))))

;;; ---------------------------------------------------------------------------
;;; The guard against a package's second turn
;;; ---------------------------------------------------------------------------

(defun donkey-pair-test--mimic-a-pairing-package ()
  "Write a closing half the way `electric-pair-mode' writes one.

`electric-pair--insert' binds its own mode variable to nil and calls
`self-insert-command', inside a `save-excursion', which runs
`post-self-insert-hook' a second time for one press -- with a
character the reader never typed and with the package reporting
itself as off."
  (let ((char last-command-event))
    ;; Guarded on its own mode variable, which is what the real function
    ;; does -- and which is the whole point: a package stops ITSELF from
    ;; running twice and stops nobody else.
    (when (and (bound-and-true-p electric-pair-mode)
               (memq char '(?\( ?\")))
      (save-excursion
        (let ((last-command-event (if (eq char ?\() ?\) char))
              (electric-pair-mode nil))
          (self-insert-command 1))))))

(ert-deftest donkey-pair-takes-one-turn-per-press ()
  "A package writing its own closing half does not get DONKEY a second turn.

A symmetric delimiter is the case that bites: the character the
package writes is the character the reader typed, so nothing in
`last-command-event' or `this-command-keys-vector' tells the second
run of the hook from the first."
  ;; The hook is written out rather than added to, for the reason
  ;; `donkey-pair-test--only-donkey-on-the-hook' gives, and in the order
  ;; the depths produce: DONKEY first, the package after it.
  (let ((hook '(donkey--pair-post-self-insert
                donkey-pair-test--mimic-a-pairing-package)))
    (donkey-pair-test--typing "*donkey-pair-reenter*" #'text-mode
        ((electric-pair-mode t) (post-self-insert-hook hook)) "" "i ("
      (should (equal (buffer-string) "()")))
    (donkey-pair-test--typing "*donkey-pair-reenter-sym*" #'text-mode
        ((electric-pair-mode t) (post-self-insert-hook hook)) "" "i \""
      (should (equal (buffer-string) "\"\"")))))

;;; ---------------------------------------------------------------------------
;;; The options are read, not trusted
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-survives-a-mistyped-pair-table ()
  "A table row that is not a pair is skipped rather than signaled over."
  (donkey-pair-test--typing "*donkey-pair-bad-table*" #'text-mode
      ((donkey-mark-pair-delimiters
        (append '(?x ("nonsense") (?\( . ?\))) donkey-mark-pair-delimiters)))
      "" "i ("
    (should (equal (buffer-string) "()"))))

(ert-deftest donkey-pair-refuses-a-pair-whose-closer-is-not-a-character ()
  "A table row holding something other than a character pairs nothing.

`insert' takes a string as happily as a character, so without the
coercion a row like (?# . \"hash\") would type the word."
  (donkey-pair-test--typing "*donkey-pair-bad-closer*" #'text-mode
      ((donkey-mark-pair-delimiters (cons '(?# . "hash") donkey-mark-pair-delimiters))
       (donkey-pair-delimiters '(?#)))
      "" "i #"
    (should (equal (buffer-string) "#")))
  (donkey-pair-test--typing "*donkey-pair-bad-closer-all*" #'text-mode
      ((donkey-mark-pair-delimiters (cons '(?# . "hash") donkey-mark-pair-delimiters))
       (donkey-pair-delimiters 'all))
      "" "i #"
    (should (equal (buffer-string) "#"))))

(ert-deftest donkey-pair-survives-a-mistyped-delimiter-list ()
  "Anything in `donkey-pair-delimiters' that is not a pair is dropped."
  (donkey-pair-test--typing "*donkey-pair-bad-list*" #'text-mode
      ((donkey-pair-delimiters (list "(" 'nope ?\( ?%)))
      "" "i ( %"
    (should (equal (buffer-string) "(%)"))))

(ert-deftest donkey-pair-survives-a-mistyped-exception-list ()
  "A malformed exceptions row is skipped rather than signaled over."
  (donkey-pair-test--typing "*donkey-pair-bad-except*" #'text-mode
      ((donkey-pair-delimiter-exceptions
        (list "nonsense" nil '(text-mode "x" ?\())))
      "" "i ("
    (should (equal (buffer-string) "("))))

(ert-deftest donkey-pair-reads-its-options-at-each-press ()
  "A delimiter list changed between presses takes effect on the next one."
  (donkey-pair-test--typing "*donkey-pair-reread*" #'text-mode () "" "i ("
    (should (equal (buffer-string) "()"))
    (let ((donkey-pair-delimiters nil))
      (execute-kbd-macro (kbd "(")))
    (should (equal (buffer-string) "(()"))))

;;; ---------------------------------------------------------------------------
;;; A mis-typed option never signals from the hook
;;; ---------------------------------------------------------------------------

(defconst donkey-pair-test--mistyped-options
  '(("the table is not a list"        donkey-mark-pair-delimiters 42)
    ("the table is a string"          donkey-mark-pair-delimiters "nonsense")
    ("the table is a symbol"          donkey-mark-pair-delimiters nope)
    ("the table has junk rows"        donkey-mark-pair-delimiters ((?a . ?b) 7 "x"))
    ("the delimiters are a symbol"    donkey-pair-delimiters nonsense)
    ("the delimiters are a number"    donkey-pair-delimiters 7)
    ("the delimiters are a junk list" donkey-pair-delimiters ("(" 3.5 nil x))
    ("the exceptions are not a list"  donkey-pair-delimiter-exceptions 9)
    ("an exception row is dotted"     donkey-pair-delimiter-exceptions ((text-mode . 5)))
    ("the exception rows are junk"    donkey-pair-delimiter-exceptions (1 "x" (nil ?a) (text-mode . 5)))
    ("the excluded list is junk"      donkey-pair-excluded-modes (1 "x" nil))
    ("the excluded list is a number"  donkey-pair-excluded-modes 7)
    ("the inclusions are not a list"  donkey-pair-delimiter-inclusions 9)
    ("an inclusion row is dotted"     donkey-pair-delimiter-inclusions ((text-mode . 5)))
    ("the inclusion rows are junk"    donkey-pair-delimiter-inclusions (1 "x" (nil ?a) (text-mode . 5))))
  "Ways a reader can mis-type one of the pairing options.

Every one is a shape a reader reaches by hand: a dot too many, a
bracket too few, a symbol where a list belongs.")

(ert-deftest donkey-pair-a-mistyped-option-never-signals-from-the-hook ()
  "No value of any pairing option makes typing signal.

These options are read from `post-self-insert-hook'.  A signal there
aborts the reader\\='s own typing -- every delimiter they press, until
they find the mistake -- so each option is coerced at every level it
is walked (rules 3 and 81).  The dotted row is the case rule 81 was
written around: it passes `consp' and its tail is not a list."
  (dolist (case donkey-pair-test--mistyped-options)
    (let ((label (car case))
          (symbol (cadr case))
          (value (nth 2 case)))
      (donkey-pair-test--typing "*donkey-pair-junk*" #'text-mode
          ;; One binding per option the table can name: `set' below
          ;; writes the value for real, so an option missing from here
          ;; keeps its rubbish for the rest of the run (rule 15).
          ((donkey-mark-pair-delimiters donkey-mark-pair-delimiters)
           (donkey-pair-delimiters donkey-pair-delimiters)
           (donkey-pair-safe-exclusions donkey-pair-safe-exclusions)
           (donkey-pair-delimiter-exceptions donkey-pair-delimiter-exceptions)
           (donkey-pair-delimiter-inclusions donkey-pair-delimiter-inclusions)
           (donkey-pair-excluded-modes donkey-pair-excluded-modes))
          "" ""
        (set symbol value)
        ;; No `should-not-error': what is asserted is that the keys
        ;; run at all, so an escaping signal fails the test by itself.
        (execute-kbd-macro (kbd "i ( a"))
        (should (stringp (buffer-string)))
        (ignore label)))))

(ert-deftest donkey-every-reader-of-the-pair-table-survives-a-mistyped-one ()
  "No subsystem signals on a `donkey-mark-pair-delimiters' that is not a list.

The option is a defcustom and holds whatever it was given.  `assq',
`rassq', `seq-filter' and `length' all signal on a non-list, and five
subsystems read it: the mark commands, the wrap engine, the chart of
which keys wrap, the platform report, and the typing hook.  Each used
to signal in its own way at a reader who typed one bracket too few,
which is rules 3 and 10.

Driven through the functions rather than through keys because `m i'
prompts, and a batch run that reaches `read-char' hangs rather than
fails."
  (dolist (value '(42 "nonsense" nope))
    (let ((donkey-mark-pair-delimiters value))
      (should (equal (donkey--pair-table) nil))
      ;; the mark commands
      (should (eq (donkey--mark-pair-open-for ?\() ?\())
      ;; the wrap engine, which answers a symmetric delimiter with
      ;; itself by design rather than with nil
      (should (characterp (donkey--wrap-close-char ?\()))
      ;; the chart, and which keys wrap
      (should (equal (donkey--wrap-delimiter-characters) nil))
      ;; the platform report
      (should (donkey--debug-donkey-lines))
      ;; the typing hook
      (should (equal (donkey--pair-characters) nil))
      (should-not (donkey--pair-close-for ?\())
      (should-not (donkey--pair-open-for ?\)))
      (should-not (donkey--pair-exception-p ?\()))))

(ert-deftest donkey-the-delimiter-prompt-reads-the-table-after-the-wait ()
  "A pair added while `m i' is waiting is one `m i' can use.

`donkey--mark-pair-read-delimiter' resolves the opening half through
`donkey--mark-pair-open-for', which reads the table when the key is
answered.  The closing half has to be read then too: a snapshot taken
before the prompt let the two disagree, so a pair added during the
wait was accepted as an opener and refused as unsupported in the same
breath (rule 19).

The prompt is stubbed with the change it is racing, because the race
is the thing under test and a batch run that reaches `read-char'
hangs rather than fails."
  (let ((table (default-value 'donkey-mark-pair-delimiters)))
    (unwind-protect
        (cl-letf (((symbol-function 'donkey--mark-pair-read-delimiter-char)
                   (lambda (&rest _)
                     (setq donkey-mark-pair-delimiters
                           (cons '(?@ . ?%) donkey-mark-pair-delimiters))
                     ?@)))
          (with-temp-buffer
            (text-mode)
            (insert "alpha")
            (goto-char 2)
            (should (equal (donkey--mark-pair-read-delimiter)
                           (list ?@ ?% nil nil)))))
      (set-default 'donkey-mark-pair-delimiters table))))

(ert-deftest donkey-the-delimiter-prompt-refuses-a-mistyped-table ()
  "`m i' answers a mistyped pair table with its own refusal.

`donkey--mark-pair-read-delimiter' resolves the character at point,
or the one it reads, against the table.  With the option set to
something that is not a list it used to signal `wrong-type-argument'
from `assq' before reaching any refusal of its own.

The read is stubbed rather than driven, because a batch run that
reaches `read-char' hangs rather than fails -- and it is stubbed on a
function this file has already loaded (rule 35).  What is asserted is
the KIND of error: a `user-error' naming the option is the refusal
working, and anything else is the crash coming back."
  (cl-letf (((symbol-function 'donkey--mark-pair-read-delimiter-char)
             (lambda (&rest _) ?\()))
    (dolist (value '(42 "nonsense"))
      (let ((donkey-mark-pair-delimiters value))
        (with-temp-buffer
          (text-mode)
          (insert "alpha")
          (goto-char 2)
          (should-error (donkey--mark-pair-read-delimiter) :type 'user-error))))))

(ert-deftest donkey-a-mistyped-pair-table-refuses-rather-than-crashes ()
  "With no usable table, the delimiter prompt refuses by name.

`m i' and `m a' answer a delimiter they cannot place with a
`user-error' naming the option (rule 5).  An empty table makes every
delimiter one of those, which is the refusal working rather than a
new failure."
  (let ((donkey-mark-pair-delimiters 42))
    (should-error (donkey--mark-pair-unsupported-error ?\() :type 'user-error)
    (with-temp-buffer
      (text-mode)
      (insert "(alpha)")
      (goto-char 3)
      ;; What the prompt would do with the character it read.
      (should (eq (donkey--mark-pair-open-for ?\() ?\()))))

(ert-deftest donkey-pair-table-coercion-has-one-address ()
  "Every reader of the pair table goes through `donkey--pair-table'.

A second reader that walks the option itself is the way this defect
comes back: it was the fast path in `donkey--pair-post-self-insert'
that still signalled after the first three sites were fixed."
  (dolist (value '(42 "nonsense" nope))
    (let ((donkey-mark-pair-delimiters value))
      (should (equal (donkey--pair-table) nil))
      (should (equal (donkey--pair-characters) nil))
      (should-not (donkey--pair-close-for ?\())
      (should-not (donkey--pair-open-for ?\)))
      (should-not (donkey--pair-exception-p ?\()))))

;;; ---------------------------------------------------------------------------
;;; setopt on the table reaches Emacs
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-setopt-on-the-table-reaches-electric-pair ()
  "A pair added with `setopt' is handed to Emacs there and then.

The README teaches adding a pair that way, and Emacs has to be TOLD
the set rather than reading it, so the table\\='s own `:set' hands it
over -- the same reason that `:set' already claims the wrap keys."
  (require 'elec-pair)
  (let ((before (copy-sequence (default-value 'electric-pair-pairs)))
        (table (default-value 'donkey-mark-pair-delimiters)))
    (unwind-protect
        (progn
          (donkey-pair-mode 1)
          (should-not (member '(?@ . ?@) (default-value 'electric-pair-pairs)))
          (setopt donkey-mark-pair-delimiters
                  (cons '(?@ . ?@) donkey-mark-pair-delimiters))
          (should (member '(?@ . ?@) (default-value 'electric-pair-pairs))))
      (donkey-pair-mode -1)
      (set-default 'donkey-mark-pair-delimiters table)
      (set-default 'electric-pair-pairs before))))

(ert-deftest donkey-pair-setopt-on-the-exclusions-reaches-electric-pair ()
  "A delimiter excluded with `setopt' stops pairing there and then.

Emacs has to be TOLD the set rather than reading it, so excluding a
character has to hand the smaller set over at once.  Without the
option\\='s own `:set' the delimiter would go on pairing until something
else refreshed, which is the whole point of excluding it."
  (require 'elec-pair)
  (let ((before (copy-sequence (default-value 'electric-pair-pairs)))
        (table (default-value 'donkey-mark-pair-delimiters))
        (out (default-value 'donkey-pair-safe-exclusions)))
    (unwind-protect
        (progn
          (donkey-pair-mode 1)
          (setopt donkey-mark-pair-delimiters
                  (cons '(?@ . ?@) donkey-mark-pair-delimiters))
          (should (member '(?@ . ?@) (default-value 'electric-pair-pairs)))
          (setopt donkey-pair-safe-exclusions
                  (cons ?@ donkey-pair-safe-exclusions))
          (should-not (member '(?@ . ?@) (default-value 'electric-pair-pairs)))
          ;; And back again: the option is a knob, not a one-way door.
          (setopt donkey-pair-safe-exclusions
                  (delq ?@ (copy-sequence donkey-pair-safe-exclusions)))
          (should (member '(?@ . ?@) (default-value 'electric-pair-pairs))))
      (donkey-pair-mode -1)
      (set-default 'donkey-pair-safe-exclusions out)
      (set-default 'donkey-mark-pair-delimiters table)
      (set-default 'electric-pair-pairs before))))

;;; ---------------------------------------------------------------------------
;;; The delete command refuses rather than eating two characters
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-delete-pair-refuses-where-there-is-no-pair ()
  "\\\\[donkey-pair-delete-pair] takes a pair or nothing.

The `:filter' guards the KEY.  The command is interactive, so a
reader can reach it by name anywhere, and it used to delete the two
characters it happened to be standing between (rule 5)."
  (unwind-protect
      (progn
        (donkey-pair-mode 1)
        (with-temp-buffer
          (text-mode)
          (insert "alpha")
          (goto-char 3)
          (should-error (donkey-pair-delete-pair) :type 'user-error)
          (should (equal (buffer-string) "alpha"))
          (erase-buffer)
          (should-error (donkey-pair-delete-pair) :type 'user-error)
          (insert "()")
          (goto-char 2)
          (donkey-pair-delete-pair)
          (should (equal (buffer-string) ""))))
    (donkey-pair-mode -1)))

;;; ---------------------------------------------------------------------------
;;; The library is loaded only where something is written
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-does-not-load-elec-pair-with-the-mode-off ()
  "Setting an option with the mode off pulls in no library.

`elec-pair' is loaded where `electric-pair-pairs' is about to be
written, and nowhere else: a reader who has not asked for the pairing
has not asked for the library either."
  ;; The second call SUPPLIES for real, so what it wrote is taken back
  ;; again: a test that leaves rows in `electric-pair-pairs' makes the
  ;; next reader of that variable fail for reasons of its own.
  (require 'elec-pair)
  (let ((loaded nil)
        (before (copy-sequence (default-value 'electric-pair-pairs)))
        (supplied donkey--pair-supplied))
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &rest _) (push feature loaded) nil)))
          (let ((donkey-pair-mode nil))
            (donkey--pair-supply-electric-pair))
          (should-not (memq 'elec-pair loaded))
          (let ((donkey-pair-mode t))
            (donkey--pair-supply-electric-pair))
          (should (memq 'elec-pair loaded)))
      (setq donkey--pair-supplied supplied)
      (set-default 'electric-pair-pairs before))))

;;; ---------------------------------------------------------------------------
;;; Setup and teardown
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-mode-is-off-until-it-is-asked-for ()
  "The mode ships off, so an upgrade changes nothing for anybody."
  (should-not (default-value 'donkey-pair-mode)))

(ert-deftest donkey-pair-mode-takes-its-hooks-back-down ()
  "Turning the mode off leaves neither of its hooks behind."
  ;; Cleared first, then captured: a teardown that left one behind would
  ;; otherwise have put it into this test's own baseline, and the
  ;; comparison would pass on the leak it is here to catch.
  (remove-hook 'post-self-insert-hook #'donkey--pair-post-self-insert)
  (remove-hook 'pre-command-hook #'donkey--pair-reset)
  (let ((post (copy-sequence (default-value 'post-self-insert-hook)))
        (pre (copy-sequence (default-value 'pre-command-hook))))
    (unwind-protect
        (progn
          (donkey-pair-mode 1)
          (should (memq 'donkey--pair-post-self-insert
                        (default-value 'post-self-insert-hook)))
          (should (memq 'donkey--pair-reset (default-value 'pre-command-hook)))
          (donkey-pair-mode -1)
          (should (equal (default-value 'post-self-insert-hook) post))
          (should (equal (default-value 'pre-command-hook) pre)))
      (donkey-pair-mode -1))))

(ert-deftest donkey-pair-mode-runs-ahead-of-the-pairing-packages ()
  "DONKEY takes its turn before a package can report itself as off."
  (unwind-protect
      (progn
        (donkey-pair-mode 1)
        (add-hook 'post-self-insert-hook
                  #'electric-pair-post-self-insert-function 50)
        (should (memq 'donkey--pair-post-self-insert
                      (memq 'donkey--pair-post-self-insert
                            (default-value 'post-self-insert-hook))))
        (let ((hook (default-value 'post-self-insert-hook)))
          (should (< (seq-position hook 'donkey--pair-post-self-insert)
                     (seq-position hook 'electric-pair-post-self-insert-function)))))
    (remove-hook 'post-self-insert-hook
                 #'electric-pair-post-self-insert-function)
    (donkey-pair-mode -1)))

;;; ---------------------------------------------------------------------------
;;; What the platform report says
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-platform-line-says-who-is-pairing ()
  "The report names whoever has the job, which is not always DONKEY."
  (should (equal (donkey--debug-pair-line) "off"))
  (unwind-protect
      (progn
        (donkey-pair-mode 1)
        (with-temp-buffer
          (text-mode)
          (should (string-match-p "\\`on, DONKEY pairs (8 delimiters)\\'"
                                  (donkey--debug-pair-line)))
          ;; The count is of what SURVIVES the coercion, which is the
          ;; one place a mis-typed option is refused.
          (let ((donkey-pair-delimiters (list "(" 'nope ?\( ?%)))
            (should (equal (donkey--debug-pair-line)
                           "on, DONKEY pairs (1 delimiter)")))
          (let ((electric-pair-mode t))
            (should (string-match-p "electric-pair-mode pairs"
                                    (donkey--debug-pair-line))))
          (let ((smartparens-mode t))
            (should (string-match-p "standing down"
                                    (donkey--debug-pair-line))))))
    (donkey-pair-mode -1)))

;;; ---------------------------------------------------------------------------
;;; Handing the delimiters to Emacs
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-pair-hands-its-delimiters-to-electric-pair ()
  "Turning the mode on puts the typing set into `electric-pair-pairs'."
  (require 'elec-pair)
  (let ((before (copy-sequence (default-value 'electric-pair-pairs))))
    (unwind-protect
        (let ((donkey-mark-pair-delimiters
               (cons '(?# . ?#) donkey-mark-pair-delimiters)))
          (donkey-pair-mode 1)
          (should (member '(?# . ?#) (default-value 'electric-pair-pairs)))
          (should (member '(?\( . ?\)) (default-value 'electric-pair-pairs)))
          ;; Emacs keeps what it shipped with.
          (dolist (row before)
            (should (member row (default-value 'electric-pair-pairs)))))
      (donkey-pair-mode -1)
      (set-default 'electric-pair-pairs before))))

(ert-deftest donkey-pair-gives-back-exactly-what-it-took ()
  "Turning the mode off removes DONKEY's rows and nobody else's."
  (require 'elec-pair)
  (let ((before (copy-sequence (default-value 'electric-pair-pairs))))
    (unwind-protect
        (progn
          ;; A row of the reader's own, and one they share with DONKEY.
          (set-default 'electric-pair-pairs
                       (append '((?% . ?%) (?\{ . ?\})) before))
          (let ((mine (copy-sequence (default-value 'electric-pair-pairs))))
            (donkey-pair-mode 1)
            (donkey-pair-mode -1)
            (should (equal (default-value 'electric-pair-pairs) mine))))
      (donkey-pair-mode -1)
      (set-default 'electric-pair-pairs before))))

;; The `require' in `donkey--pair-supply-electric-pair' has no test, and
;; cannot have one here: this suite loads `elec-pair' itself, and in a
;; process where the library is already loaded, removing that line
;; changes nothing.  Verified by hand instead, in two fresh batch
;; Emacsen that never load it any other way: as shipped, enabling the
;; mode loads `elec-pair', adds DONKEY's pairs and leaves Emacs's own
;; three standing; with the line removed, the library never loads, the
;; variable stays unbound, and the reader silently gets no pairing at
;; all.  The guard below covers the other half -- that nothing is
;; written while the variable is unbound -- which is what stops the
;; second failure being data loss rather than a no-op.

(ert-deftest donkey-pair-does-not-pre-empt-the-elec-pair-defcustom ()
  "Nothing is written while `electric-pair-pairs' is unbound.

`defcustom' leaves a variable that is already bound alone, so writing
this one before `elec-pair' has defined it would throw Emacs\\='s own
three pairs away and leave the reader with DONKEY\\='s and nothing else.

The variable is unbound for the length of the test rather than mocked,
so what is asserted is that nothing bound it back.  The `require' that
normally makes it bound is stubbed out for the same reason -- with it
running, there is no unbound state left to test."
  (require 'elec-pair)
  (let ((had (default-value 'electric-pair-pairs))
        (donkey--pair-supplied nil))
    (unwind-protect
        (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
          (makunbound 'electric-pair-pairs)
          (let ((donkey-pair-mode t))
            (donkey--pair-supply-electric-pair))
          (should-not (boundp 'electric-pair-pairs))
          (should (equal donkey--pair-supplied nil)))
      (set-default 'electric-pair-pairs had))))

(ert-deftest donkey-pair-refresh-hands-over-a-table-changed-by-setq ()
  "\\[donkey-pair-refresh] reaches a change `setq' told nobody about."
  (require 'elec-pair)
  (let ((before (copy-sequence (default-value 'electric-pair-pairs)))
        (table donkey-mark-pair-delimiters))
    (unwind-protect
        (progn
          (donkey-pair-mode 1)
          (should-not (member '(?@ . ?@) (default-value 'electric-pair-pairs)))
          (setq donkey-mark-pair-delimiters
                (cons '(?@ . ?@) donkey-mark-pair-delimiters))
          (should-not (member '(?@ . ?@) (default-value 'electric-pair-pairs)))
          (donkey-pair-refresh)
          (should (member '(?@ . ?@) (default-value 'electric-pair-pairs))))
      (donkey-pair-mode -1)
      (setq donkey-mark-pair-delimiters table)
      (set-default 'electric-pair-pairs before))))

(provide 'donkey-pair-test)

;;; donkey-pair-test.el ends here

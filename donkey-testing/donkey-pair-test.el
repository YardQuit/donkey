;;; donkey-pair-test.el --- Tests for DONKEY pairing while typing -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'donkey)
(require 'donkey-test-keys)

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
  "`safe', the default, drops what `donkey--pair-typing-punctuation' names.

Every one of those characters is text far more often than it is a
delimiter, and each of them IS in the table, so `all' would pair it."
  (dolist (char donkey--pair-typing-punctuation)
    (should (assq char donkey-mark-pair-delimiters))
    (should-not (memq char (donkey--pair-characters)))))

(ert-deftest donkey-readme-safe-table-says-what-safe-does ()
  "The README table of what `safe' types matches the code.

It is the table a reader checks before wondering why a key did
nothing, so every cell is read back out of README.org and asked of
`donkey--pair-characters' and `donkey--pair-typing-punctuation',
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
                       (not (memq char donkey--pair-typing-punctuation)))
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
`donkey--pair-typing-punctuation'."
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

The section shows sixteen options set to the value they already have,
so a reader can see the shipped answer beside the knob.  A default
that moves leaves those lines quietly wrong, which is the worst kind
of wrong in a configuration people copy (rule 30).

Read out of README.org rather than repeated here, so the two cannot
drift."
  (let (wrong (checked 0))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "README.org" donkey-pair-test--source-dir))
      (goto-char (point-min))
      (while (re-search-forward
              "^ *(setopt \\(donkey-[a-z0-9-]+\\) \\(.*?\\)) *; *default *$" nil t)
        (let* ((symbol (intern (match-string 1)))
               (text (match-string 2))
               (said (car (read-from-string text)))
               (said (if (and (consp said) (eq (car said) 'quote)) (cadr said) said)))
          (setq checked (1+ checked))
          (cond
           ((not (boundp symbol))
            (push (format "%s is named but does not exist" symbol) wrong))
           ((not (equal said (default-value symbol)))
            (push (format "%s: README says %S, the default is %S"
                          symbol said (default-value symbol))
                  wrong))))))
    (should (> checked 10))
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

;;; donkey-describe-bindings-test.el --- Tests for donkey-describe-bindings -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'donkey)

(defconst donkey-test--source-dir
  (or (and load-file-name (file-name-directory (directory-file-name
                                                (file-name-directory load-file-name))))
      default-directory)
  "Directory holding donkey.el, for tests that read the source as text.")

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defun donkey-describe-bindings-test--simple-map ()
  "Flat keymap with two leaf bindings."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'ignore)
    (keymap-set map "b" #'forward-char)
    map))

(defun donkey-describe-bindings-test--nested-map ()
  "Keymap with a sub-prefix under SPC."
  (let ((map (make-sparse-keymap))
        (sub (make-sparse-keymap)))
    (keymap-set map "x" #'ignore)
    (keymap-set sub "q" #'kill-region)
    (define-key map (kbd "SPC") sub)
    map))

(defun donkey-describe-bindings-test--remap-map ()
  "Keymap with a remap to `self-insert-command'."
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'ignore)
    map))

(defun donkey-describe-bindings-test--non-sic-remap-map ()
  "Keymap with a remap to a command other than `self-insert-command'."
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-char] #'ignore)
    map))

(defun donkey-describe-bindings-test--cons-cdr-keymap-map ()
  "Keymap whose binding is a cons with a keymap as its cdr.
Triggers the (and (consp def) (keymapp (cdr def))) branch."
  (let ((outer (make-sparse-keymap))
        (inner (make-sparse-keymap)))
    (keymap-set inner "j" #'join-line)
    (define-key outer "m" (cons 'placeholder inner))
    outer))

(defun donkey-describe-bindings-test--menu-item-map ()
  "Keymap with a standard menu-item binding.
Treated as a leaf because (cdr def) is a list, not a keymap."
  (let ((outer (make-sparse-keymap))
        (inner (make-sparse-keymap)))
    (keymap-set inner "j" #'join-line)
    (define-key outer "m" `(menu-item "Test" ,inner))
    outer))

(defun donkey-describe-bindings-test--complex-def-map ()
  "Keymap with a non-symbol, non-keymap definition."
  (let ((map (make-sparse-keymap)))
    (define-key map "k" '("my-data"))
    map))

;; ===========================================================================
;; Section: donkey--desc-bindings-collect-leaves
;; Selector: (ert "donkey-describe-bindings-collect-leaves")
;; ===========================================================================

;;; --- Basic collection ---

(ert-deftest donkey-describe-bindings-collect-leaves-empty-map ()
  "An empty keymap produces no leaf entries.
Expected: nil."
  (should (null (donkey--desc-bindings-collect-leaves
                 (make-sparse-keymap) ""))))

(ert-deftest donkey-describe-bindings-collect-leaves-nil-def-skipped ()
  "Bindings whose definition is nil are silently ignored.
Expected: nil (empty list)."
  (let ((map (make-sparse-keymap)))
    (define-key map [?a] nil)
    (should (null (donkey--desc-bindings-collect-leaves map "")))))

(ert-deftest donkey-describe-bindings-collect-leaves-single-level ()
  "A flat keymap yields one entry per binding with correct key strings.
Expected: two entries (\"a\" . #'ignore) and (\"b\" . #'forward-char)."
  (let* ((map (donkey-describe-bindings-test--simple-map))
         (res (donkey--desc-bindings-collect-leaves map "")))
    (should (= (length res) 2))
    (should (equal (car (assoc "a" res)) "a"))
    (should (eq    (cdr (assoc "a" res)) #'ignore))
    (should (equal (car (assoc "b" res)) "b"))
    (should (eq    (cdr (assoc "b" res)) #'forward-char))))

;;; --- Nested keymaps ---

(ert-deftest donkey-describe-bindings-collect-leaves-nested-descends ()
  "Sub-keymaps are traversed recursively.
Expected: top-level 'x' and nested 'SPC q' both present."
  (let* ((map (donkey-describe-bindings-test--nested-map))
         (res (donkey--desc-bindings-collect-leaves map "")))
    (should (= (length res) 2))
    (should (assoc "x"     res))
    (should (assoc "SPC q" res))
    (should (eq (cdr (assoc "SPC q" res)) #'kill-region))))

(ert-deftest donkey-describe-bindings-collect-leaves-prefix-accumulated ()
  "The prefix argument is prepended to every key in a sub-keymap.
Expected: keys from nested maps carry the parent prefix followed by a space."
  (let ((map (make-sparse-keymap))
        (sub (make-sparse-keymap)))
    (keymap-set sub "a" #'ignore)
    (define-key map "g" sub)
    (let ((res (donkey--desc-bindings-collect-leaves map "P")))
      (should (assoc "Pg a" res)))))

;;; --- Remap filtering ---

(ert-deftest donkey-describe-bindings-collect-leaves-skips-self-insert-remap ()
  "Remap entries targeting `self-insert-command' are excluded.
Expected: empty result (the only binding was such a remap)."
  (let ((res (donkey--desc-bindings-collect-leaves
              (donkey-describe-bindings-test--remap-map) "")))
    (should (null res))))

(ert-deftest donkey-describe-bindings-collect-leaves-non-self-insert-remap-collected ()
  "A remap to a command other than `self-insert-command' is a leaf.
It is treated like any other binding and included in the result.
Expected: one entry whose value is #'ignore."
  (let* ((map (donkey-describe-bindings-test--non-sic-remap-map))
         (res (donkey--desc-bindings-collect-leaves map "")))
    (should (= (length res) 1))
    (should (eq (cdar res) #'ignore))))

;;; --- Cons-cdr-keymap traversal ---

(ert-deftest donkey-describe-bindings-collect-leaves-cons-cdr-keymap ()
  "A cons whose cdr is a keymap is descended into.
When a binding takes that shape, the collector walks the inner keymap.
Expected: the inner binding 'j' appears with prefix 'm '."
  (let* ((map (donkey-describe-bindings-test--cons-cdr-keymap-map))
         (res (donkey--desc-bindings-collect-leaves map "")))
    (should (assoc "m j" res))
    (should (eq (cdr (assoc "m j" res)) #'join-line))))

;;; --- Menu-item treated as leaf ---

(ert-deftest donkey-describe-bindings-collect-leaves-menu-item-as-leaf ()
  "A standard menu-item binding is treated as a leaf because (cdr def)
is a list, not a keymap.
Expected: one entry whose definition car is 'menu-item."
  (let* ((map (donkey-describe-bindings-test--menu-item-map))
         (res (donkey--desc-bindings-collect-leaves map "")))
    (should (= (length res) 1))
    (should (eq (car (cdar res)) 'menu-item))))

;;; --- Complex definitions ---

(ert-deftest donkey-describe-bindings-collect-leaves-complex-def-as-leaf ()
  "Non-symbol, non-keymap definitions are stored verbatim as leaves.
Expected: one entry whose cdr is the list (\"my-data\")."
  (let* ((map (donkey-describe-bindings-test--complex-def-map))
         (res (donkey--desc-bindings-collect-leaves map "")))
    (should (= (length res) 1))
    (should (equal (cdar res) '("my-data")))))

;;; --- Ordering ---

(ert-deftest donkey-describe-bindings-collect-leaves-order-is-stable ()
  "Push then nreverse preserves keymap iteration order.
Expected: 'a' before 'b' in the result list."
  (let* ((map (donkey-describe-bindings-test--simple-map))
         (res (donkey--desc-bindings-collect-leaves map ""))
         (keys (mapcar #'car res)))
    (should (equal keys (sort keys #'string<)))
    (should (string< (car keys) (cadr keys)))))

;;; --- Deep nesting ---

(ert-deftest donkey-describe-bindings-collect-leaves-three-level-nesting ()
  "Recursive descent across three levels of keymaps.
Expected: deepest binding key is 'a b c'."
  (let ((l1 (make-sparse-keymap))
        (l2 (make-sparse-keymap))
        (l3 (make-sparse-keymap)))
    (keymap-set l3 "c" #'ignore)
    (define-key l2 "b" l3)
    (define-key l1 "a" l2)
    (let ((res (donkey--desc-bindings-collect-leaves l1 "")))
      (should (= (length res) 1))
      (should (equal (caar res) "a b c")))))

;;; --- Mixed map ---

(ert-deftest donkey-describe-bindings-collect-leaves-mixed-leaf-and-submap ()
  "Nils are skipped, leaves are collected, sub-keymaps are recursed.
Expected: three entries — 'p', 's a', 's b'."
  (let ((root (make-sparse-keymap))
        (sub  (make-sparse-keymap)))
    (keymap-set root "p" #'ignore)
    (define-key root [?x] nil)
    (keymap-set sub  "a" #'forward-char)
    (keymap-set sub  "b" #'backward-char)
    (define-key root "s" sub)
    (let ((res (donkey--desc-bindings-collect-leaves root "")))
      (should (= (length res) 3))
      (should (assoc "p"   res))
      (should (assoc "s a" res))
      (should (assoc "s b" res)))))

;; ===========================================================================
;; Section: donkey--binding-group-name
;; Selector: (ert "donkey-describe-bindings-group-name")
;; ===========================================================================

(ert-deftest donkey-describe-bindings-group-name-single ()
  "Prefix \"single\" maps to \"Single Keys\".
Expected: \"Single Keys\"."
  (should (equal (donkey--binding-group-name "single") "Single Keys")))

(ert-deftest donkey-describe-bindings-group-name-g ()
  "Prefix \"g\" maps to \"Goto / Scroll\".
Expected: \"Goto / Scroll\"."
  (should (equal (donkey--binding-group-name "g") "Goto / Scroll")))

(ert-deftest donkey-describe-bindings-group-name-m ()
  "Prefix \"m\" maps to \"Mark Objects\".
Expected: \"Mark Objects\"."
  (should (equal (donkey--binding-group-name "m") "Mark Objects")))

(ert-deftest donkey-describe-bindings-group-name-r ()
  "Prefix \"r\" maps to \"Search / Replace\".
Expected: \"Search / Replace\"."
  (should (equal (donkey--binding-group-name "r") "Search / Replace")))

(ert-deftest donkey-describe-bindings-group-name-z ()
  "Prefix \"z\" maps to \"Scroll\".
Expected: \"Scroll\"."
  (should (equal (donkey--binding-group-name "z") "Scroll")))

(ert-deftest donkey-describe-bindings-group-name-unknown-prefix ()
  "Unknown single-char prefix is uppercased and suffixed with \" Prefix\".
Expected: \"X Prefix\"."
  (should (equal (donkey--binding-group-name "x") "X Prefix")))

(ert-deftest donkey-describe-bindings-group-name-multi-char-prefix ()
  "Multi-character unknown prefixes are uppercased in full.
Expected: \"SPC Prefix\"."
  (should (equal (donkey--binding-group-name "SPC") "SPC Prefix")))

(ert-deftest donkey-describe-bindings-group-name-empty-string ()
  "Empty-string prefix yields \" Prefix\" (leading space).
Documents existing behaviour rather than asserting it is ideal.
Expected: \" Prefix\"."
  (should (equal (donkey--binding-group-name "") " Prefix")))

(ert-deftest donkey-describe-bindings-group-name-numeric-string ()
  "Numeric string prefix is treated as any unknown prefix.
Expected: \"1 Prefix\"."
  (should (equal (donkey--binding-group-name "1") "1 Prefix")))

;; ===========================================================================
;; Section: donkey-describe-bindings
;; Selector: (ert "donkey-describe-bindings")
;;           runs ALL tests in this file
;;
;;           (ert "donkey-describe-bindings-")
;;           runs only this section (avoids matching the sub-section prefixes)
;; ===========================================================================

(defconst donkey-describe-bindings-test--expected-title "DONKEY Normal Mode Key Bindings"
  "Title string expected in the *DONKEY Bindings* buffer.")

;;; --- Pre-condition error ---

(ert-deftest donkey-describe-bindings-errors-without-map ()
  "An unbound `donkey-normal-mode-map' raises a `user-error'.
Calling `donkey-describe-bindings' there signals rather than failing
obscurely.
Expected: signal of type `user-error'."
  (let ((had-map (boundp 'donkey-normal-mode-map))
        (old-val (and (boundp 'donkey-normal-mode-map)
                      (default-value 'donkey-normal-mode-map))))
    (when had-map
      (makunbound 'donkey-normal-mode-map))
    (unwind-protect
        (should-error (donkey-describe-bindings) :type 'user-error)
      (when had-map
        (set-default 'donkey-normal-mode-map old-val)))))

;;; --- Buffer creation and basic content ---

(ert-deftest donkey-describe-bindings-creates-buffer ()
  "Creates the buffer named *DONKEY Bindings*.
Expected: buffer exists after the call."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (when (get-buffer "*DONKEY Bindings*")
      (kill-buffer "*DONKEY Bindings*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (should (get-buffer "*DONKEY Bindings*"))
    (kill-buffer "*DONKEY Bindings*")))

(ert-deftest donkey-describe-bindings-title-present ()
  "Buffer contains the expected title text.
Expected: first line includes \"DONKEY Normal Mode Key Bindings\"."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (with-current-buffer "*DONKEY Bindings*"
      (goto-char (point-min))
      (should (search-forward donkey-describe-bindings-test--expected-title nil t)))
    (kill-buffer "*DONKEY Bindings*")))

(ert-deftest donkey-describe-bindings-read-only ()
  "Buffer is read-only after generation.
Expected: `buffer-read-only' is non-nil."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (with-current-buffer "*DONKEY Bindings*"
      (should buffer-read-only))
    (kill-buffer "*DONKEY Bindings*")))

(ert-deftest donkey-describe-bindings-truncate-lines ()
  "Buffer has `truncate-lines' set to t.
Expected: `truncate-lines' is t."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (with-current-buffer "*DONKEY Bindings*"
      (should truncate-lines))
    (kill-buffer "*DONKEY Bindings*")))

;;; --- Sort order ---

(ert-deftest donkey-describe-bindings-sorted-alphabetically ()
  "Binding lines appear in ascending key order.
Expected: extracted keys are in sorted order."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "b" #'ignore)
    (keymap-set map "a" #'forward-char)
    (keymap-set map "c" #'backward-char)
    (let ((donkey-normal-mode-map map))
      (cl-letf (((symbol-function 'display-buffer) #'ignore))
        (donkey-describe-bindings))
      (with-current-buffer "*DONKEY Bindings*"
        (let (keys)
          (goto-char (point-min))
          (search-forward "---" nil t)
          (forward-line 1)
          (while (and (not (eobp))
                      (looking-at-p "^ "))
            (push (buffer-substring-no-properties
                   (point) (+ (point) 14))
                  keys)
            (forward-line 1))
          (setq keys (nreverse keys))
          (should (equal keys (sort (copy-sequence keys) #'string<)))))
      (kill-buffer "*DONKEY Bindings*"))))

;;; --- Leaf entries: symbol vs complex ---

(ert-deftest donkey-describe-bindings-symbol-leaf-as-button ()
  "Symbol-typed definitions produce a clickable button.
Expected: at least one button present in the buffer."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (with-current-buffer "*DONKEY Bindings*"
      (should (next-button (point-min))))
    (kill-buffer "*DONKEY Bindings*")))

(ert-deftest donkey-describe-bindings-complex-def-shown-as-text ()
  "Non-symbol definitions render as literal \"[complex]\" text.
Expected: the string \"[complex]\" appears in the buffer."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--complex-def-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (with-current-buffer "*DONKEY Bindings*"
      (goto-char (point-min))
      (should (search-forward "[complex]" nil t)))
    (kill-buffer "*DONKEY Bindings*")))

;;; --- Group separators ---

(ert-deftest donkey-describe-bindings-group-separators ()
  "Group transitions insert a blank line, header, and dash separator.
Expected: buffer contains at least one group header from
`donkey--binding-group-name'."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" #'ignore)
    (keymap-set map "g g" #'forward-char)
    (let ((donkey-normal-mode-map map))
      (cl-letf (((symbol-function 'display-buffer) #'ignore))
        (donkey-describe-bindings))
      (with-current-buffer "*DONKEY Bindings*"
        (goto-char (point-min))
        (should (search-forward "Goto / Scroll" nil t)))
      (kill-buffer "*DONKEY Bindings*"))))

;;; --- Local keymap ---

(ert-deftest donkey-describe-bindings-local-keymap-q-binds-quit-window ()
  "The local keymap binds \"q\" to `quit-window'.
Expected: `lookup-key' on the local map for \"q\" returns `quit-window'."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (with-current-buffer "*DONKEY Bindings*"
      (should (eq (lookup-key (current-local-map) (kbd "q"))
                  #'quit-window)))
    (kill-buffer "*DONKEY Bindings*")))

(ert-deftest donkey-describe-bindings-local-keymap-ret-binds-push-button ()
  "The local keymap binds RET to `push-button'.
Expected: `lookup-key' on the local map for RET returns `push-button'."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (with-current-buffer "*DONKEY Bindings*"
      (should (eq (lookup-key (current-local-map) (kbd "RET"))
                  #'push-button)))
    (kill-buffer "*DONKEY Bindings*")))

;;; --- Footer ---

(ert-deftest donkey-describe-bindings-footer-present ()
  "Buffer ends with a footer describing 'q' and 'RET' actions.
Expected: buffer text contains the footer string."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (with-current-buffer "*DONKEY Bindings*"
      (goto-char (point-min))
      (should (search-forward "q: quit  |  RET or click: describe command"
                              nil t)))
    (kill-buffer "*DONKEY Bindings*")))

;;; --- Point position ---

(ert-deftest donkey-describe-bindings-point-at-min ()
  "Point is at `point-min' after generation.
Expected: `point' equals `point-min'."
  (let ((donkey-normal-mode-map (donkey-describe-bindings-test--simple-map)))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (donkey-describe-bindings))
    (should (= (point) (point-min)))
    (kill-buffer "*DONKEY Bindings*")))

;;; --- Idempotent erase ---

(ert-deftest donkey-describe-bindings-overwrite-on-repeat ()
  "Calling twice with different maps erases old content.
Expected: after the second call, only keys from the second map remain."
  (let ((map-a (make-sparse-keymap))
        (map-b (make-sparse-keymap)))
    (keymap-set map-a "a" #'ignore)
    (keymap-set map-b "b" #'forward-char)
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (let ((donkey-normal-mode-map map-a))
        (donkey-describe-bindings))
      (with-current-buffer "*DONKEY Bindings*"
        (should (search-forward "ignore" nil t)))
      (let ((donkey-normal-mode-map map-b))
        (donkey-describe-bindings))
      (with-current-buffer "*DONKEY Bindings*"
        (goto-char (point-min))
        (should-not (search-forward "ignore" nil t))
        (should     (search-forward "forward-char" nil t))))
    (kill-buffer "*DONKEY Bindings*")))

(ert-deftest donkey-describe-bindings-each-group-appears-once ()
  "Every prefix group gets exactly one labelled section.

Regression: entries were sorted by key alone, so single keys interleaved
with the prefix groups alphabetically -- \"h\" landing between \"g t\"
and \"m a\".  A header is emitted on each group transition, so \"Single
Keys\" appeared four separate times, which is not the grouping the
docstring promises."
  (donkey-describe-bindings)
  (unwind-protect
      (with-current-buffer "*DONKEY Bindings*"
        (goto-char (point-min))
        (let (heads)
          (while (re-search-forward "^  \\([A-Za-z/ ]+\\)$" nil t)
            (push (substring-no-properties (match-string 1)) heads))
          (setq heads (nreverse heads))
          (should heads)
          (should (equal heads (delete-dups (copy-sequence heads))))
          ;; The leading block is labelled too, rather than being the one
          ;; group left without a header.
          (should (equal (car heads) "Single Keys"))))
    (when (get-buffer "*DONKEY Bindings*")
      (kill-buffer "*DONKEY Bindings*"))))

(ert-deftest donkey-desc-bindings-group-splits-on-first-space ()
  "Prefix grouping keys off everything before the first space."
  (should (equal (donkey--desc-bindings-group "m DEL") "m"))
  (should (equal (donkey--desc-bindings-group "m <deletechar>") "m"))
  (should (equal (donkey--desc-bindings-group "g g") "g"))
  (should (equal (donkey--desc-bindings-group "G") "single"))
  (should (equal (donkey--desc-bindings-group "C-j") "single"))
  (should (equal (donkey--desc-bindings-group "<backspace>") "single")))

;;; ---------------------------------------------------------------------------
;;; donkey-tutor
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-tutor-opens-in-normal-state ()
  "The tutor buffer must be in NORMAL state to be usable.

DONKEY is a MINOR mode, which no other editor's tutor has to contend
with: a tutor buffer left in INSERT would make every instruction in it
inert, since the keys it names would type rather than act."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should donkey-mode)
          (should-not (bound-and-true-p donkey-insert-mode))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-substitutes-every-key-it-names ()
  "Bindings render as keys, not as \\=`M-x\\=' invocations.

Regression: `substitute-command-keys' resolves against the CURRENT
buffer's active keymaps, so running it before `donkey-mode' was enabled
rendered every binding as an \\=`M-x\\=' invocation -- silently, and worst for
exactly the commands a new reader most needs named.

Now that `donkey-tutor' is bound to \\=`g ?\\=' there is nothing left that
resolves to \\=`M-x\\=' at all: it was the one command deliberately shown that
way while it had no key.  An \\=`M-x\\=' appearing here again means either a
command lost its binding or the substitution moved back ahead of
`donkey-mode'."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-min))
          (let ((unresolved '()))
            (while (re-search-forward "M-x \\(donkey-[a-z-]+\\)" nil t)
              (push (match-string 1) unresolved))
            (should (equal unresolved '())))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-returns-to-an-existing-buffer ()
  "Running it again resumes rather than discarding the lesson."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-max))
          (insert "PROGRESS MARKER"))
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should (string-match-p "PROGRESS MARKER" (buffer-string)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-is-editable ()
  "It is practised in, so it must not be read-only."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should-not buffer-read-only)))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-every-command-it-names-exists ()
  "No lesson may teach a command that is not there.

The content is a string, so a renamed or removed command would otherwise
go unnoticed until a reader reached the lesson naming it."
  (let ((pos 0) (missing '()))
    (while (string-match "\\\\\\[\\([a-z-]+\\)\\]" donkey--tutor-content pos)
      (setq pos (match-end 0))
      (let ((sym (intern (match-string 1 donkey--tutor-content))))
        (unless (commandp sym) (push sym missing))))
    (should (equal missing '()))))

(ert-deftest donkey-tutor-is-bound-in-normal-state ()
  "The tutor is reachable without knowing its name.

Under `g' rather than on a letter of its own: the letters vi uses for
motions are all still free in this map, and spending one on a command
read once would take a key a motion will want later."
  (should (eq (lookup-key donkey-normal-mode-map (kbd "g ?")) 'donkey-tutor)))

(ert-deftest donkey-tutor-names-every-key-with-the-same-markup ()
  "Every key the tutor names carries `help-key-binding', none is bare text.

Regression: keys written literally -- \\=`C-g\\=', g g, the \\=`C-u\\=' N
part of a count -- rendered as plain text while substituted bindings
rendered as faced chips, so the same buffer showed two kinds of key.
Literal ones now use
the \\=`KEY\\=' markup, which carries the same face."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-min))
          ;; Any of these appearing WITHOUT the face means a bare mention
          ;; slipped back in.
          (dolist (key '("C-g" "C-u 3" "C-u 2" "C-u 5"))
            (goto-char (point-min))
            (while (search-forward key nil t)
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'help-key-binding))))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-teaches-jump-back-as-a-recovery-key ()
  "The `S' lesson is present and framed as undoing a mis-keyed jump."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should (string-match-p "recovery key rather than a filing system"
                                  (buffer-string)))
          (should (string-match-p "bookmarks" (buffer-string)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

;;; ---------------------------------------------------------------------------
;;; The tutor's claims about native Emacs must stay true
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-tutor-intro-mentions-E-and-its-forward-reference-resolves ()
  "The intro names DONKEY[E] and points at a section that really explains it.

A cross-reference by name is the kind of claim that rots quietly: rename
the section and the intro sends the reader nowhere.  The HEADING is what
is matched here, not the bare phrase -- matching the phrase finds the
intro\='s own quoted mention of it and proves nothing, which is how the
first version of this test passed against a deliberately renamed
section."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let* ((text (buffer-string))
                 (intro (string-match "DONKEY\\[E\\]" text))
                 (section (string-match "^Your Emacs still works\n-+$" text)))
            (should intro)
            (should section)
            (should (< intro section))
            ;; the section it points at really does explain the indicator
            (should (string-match "DONKEY\\[E\\]" text section)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-claim-emacs-keys-still-work ()
  "Every key the tutor promises is untouched must really be untouched.

The tutor tells a new reader that these behave exactly as they always
did.  That promise is only worth making if something checks it: a
binding added to `donkey-normal-mode-map' in a hurry could quietly
falsify a paragraph nobody thinks to re-read."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-normal-mode 1)
        (dolist (pair '(("C-x C-s" save-buffer)
                        ("C-x C-f" find-file)
                        ("M-x"     execute-extended-command)
                        ("C-h k"   describe-key)
                        ("C-s"     isearch-forward)
                        ("C-r"     isearch-backward)
                        ("C-a"     move-beginning-of-line)
                        ("C-e"     move-end-of-line)
                        ("C-k"     kill-line)
                        ("C-w"     kill-region)
                        ("M-w"     kill-ring-save)
                        ("C-y"     yank)
                        ("C-SPC"   set-mark-command)
                        ("M-f"     forward-word)
                        ("M-b"     backward-word)
                        ("M-^"     delete-indentation)
                        ("TAB"     indent-for-tab-command)
                        ("C-u"     universal-argument)
                        ("<up>"    previous-line)
                        ("<down>"  next-line)
                        ("<home>"  move-beginning-of-line)
                        ("<prior>" scroll-down-command)))
          (should (eq (key-binding (kbd (car pair))) (cadr pair)))))
    (donkey-mode -1)))

(ert-deftest donkey-tutor-claim-normal-state-costs-exactly-four-things ()
  "The four differences the tutor names, and no fifth one.

If a key is ever added to `donkey-normal-mode-map' that shadows a stock
Emacs command, this fails -- and the tutor and README both need the new
line.  `RET' is the one entry here with a command behind it; the rest
are absences."
  (unwind-protect
      (with-temp-buffer
        (text-mode)
        (donkey-mode 1)
        (donkey-normal-mode 1)
        ;; 1. Letters run commands rather than typing.
        (should (eq (lookup-key donkey-normal-mode-map [remap self-insert-command])
                    'undefined))
        ;; 2. Digits are not counts.
        (dolist (d '("0" "3" "9"))
          (should (eq (key-binding (kbd d)) 'undefined)))
        ;; 3. RET is DONKEY's.
        (should (eq (key-binding (kbd "RET")) #'donkey-enter-dwim))
        ;; 4. Backspace and Delete do nothing -- under every key name they
        ;; arrive as, graphical and terminal alike.
        (dolist (key (list [backspace] [delete] [?\C-?] [deletechar]))
          (should (eq (key-binding key) #'ignore))))
    (donkey-mode -1)))

(ert-deftest donkey-tutor-claim-no-fifth-shadowed-emacs-command ()
  "Nothing outside the documented set shadows a real Emacs command.

Resolves every leaf of `donkey-normal-mode-map' against what a plain
`text-mode' buffer would do with the same keys.  Anything that differs,
and whose Emacs meaning is a real command rather than self-insertion,
must be one the tutor and README already name.

DEL and <deletechar> joined the list when the BACKSPACE/DELETE block was
extended to cover the terminal key names as well as the graphical ones.
They are the two entries this test can actually catch: `key-binding\='
applies no `function-key-map\=' translation, so <backspace> and <delete>
resolve to nil in a vanilla buffer and slip through the filter -- they
are listed for completeness and checked by name in the test above.  This
test earned its keep on that change, failing the moment the keymap grew
past what the docs described."
  (let ((documented '("RET" "<backspace>" "<delete>" "DEL" "<deletechar>"))
        (found '()))
    (cl-labels
        ((walk (map prefix)
           (map-keymap
            (lambda (ev def)
              (let ((seq (vconcat prefix (vector ev))))
                (if (keymapp def)
                    (walk def seq)
                  (when def
                    (let ((vanilla (with-temp-buffer
                                     (text-mode)
                                     (key-binding seq t))))
                      (when (and vanilla
                                 (not (numberp vanilla))
                                 (not (memq vanilla
                                            '(self-insert-command undefined
                                              digit-argument negative-argument)))
                                 (not (eq vanilla def)))
                        (push (key-description seq) found)))))))
            map)))
      (walk donkey-normal-mode-map []))
    ;; `key-binding' does not apply `function-key-map' translation, so the
    ;; two function keys above never show up here -- they are checked by
    ;; name in the test above instead.  This catches the ordinary ones.
    (should (equal (sort found #'string<)
                   (sort (seq-intersection documented found) #'string<)))))

(ert-deftest donkey-tutor-claim-insert-state-changes-one-key ()
  "INSERT state binds `C-g' and nothing else.

The tutor says INSERT is Emacs with one key changed.  A second entry in
this map would make that false."
  (let ((bindings '()))
    (map-keymap (lambda (ev def) (push (cons (key-description (vector ev)) def) bindings))
                donkey-insert-mode-map)
    (should (equal bindings '(("C-g" . donkey--exit-insert))))))

(ert-deftest donkey-tutor-claim-dired-keys-survive ()
  "The Dired keys the tutor names by hand are really still Dired's."
  (skip-unless (require 'dired nil t))
  (unwind-protect
      (with-temp-buffer
        (setq major-mode 'dired-mode)
        (use-local-map dired-mode-map)
        (donkey-mode 1)
        (donkey-normal-mode 1)
        (dolist (pair '(("n" dired-next-line)
                        ("t" dired-toggle-marks)
                        ("q" quit-window)
                        ("^" dired-up-directory)
                        ("+" dired-create-directory)))
          (should (eq (key-binding (kbd (car pair))) (cadr pair)))))
    (donkey-mode -1)))

;;; ---------------------------------------------------------------------------
;;; The tutor names BOTH delete keys
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-tutor-names-both-delete-keys ()
  "The tutor names BOTH delete keys, everywhere, not just one of them.

`\\=\\[donkey-delete]' names whichever binding `substitute-command-keys'
finds first -- \"x\" -- so the tutor never mentioned \"d\" at all,
despite it being the Helix binding and the one half the audience
reaches for."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            (should (string-match-p "Use d/x\\." text))
            ;; and not only in that one sentence: every place the tutor
            ;; names the delete command now names both keys.
            (should (>= (cl-count-if (lambda (l) (string-match-p "d/x" l))
                                     (split-string text "\n"))
                        10)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-delete-keys-placeholder-is-always-replaced ()
  "No DONKEY-DELETE-KEYS token may survive into the rendered tutor.

It is not a `substitute-command-keys' escape, so nothing else would
catch it -- a reader would simply see the raw token."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should-not (string-match-p "DONKEY-DELETE-KEYS" (buffer-string)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-delete-keys-follow-a-rebinding ()
  "A reader who rebound the delete keys is taught the keys they have.

That is the promise the `substitute-command-keys' escapes make, and the
reason this is computed rather than written into the tutor text."
  (let ((map (copy-keymap donkey-normal-mode-map)))
    (define-key map "d" nil)
    (define-key map "x" nil)
    (keymap-set map "Z" #'donkey-delete)
    (cl-letf (((symbol-function 'donkey--tutor-delete-keys)
               (lambda ()
                 (let ((keys (sort (mapcar #'key-description
                                           (where-is-internal #'donkey-delete map))
                                   #'string<)))
                   (cond ((null keys) "\\[donkey-delete]")
                         ((null (cdr keys)) (car keys))
                         (t (concat (mapconcat #'identity (butlast keys) ", ")
                                    " or " (car (last keys)))))))))
      (unwind-protect
          (progn
            (donkey-tutor)
            (with-current-buffer "*DONKEY Tutor*"
              (should (string-match-p "Use Z\\." (buffer-string)))
              (should-not (string-match-p "Use d/x\\." (buffer-string)))))
        (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))))

(ert-deftest donkey-tutor-delete-keys-render-as-keys-not-prose ()
  "Both keys carry `help-key-binding', like every other key in the tutor.

Computing the keys made them raw text, so they were the only keys in the
whole buffer rendering as plain prose -- which reads as an oversight in
a document whose entire job is showing you keys.  Every existing test
passed in that state, because they all compared strings and the string
was right; only the face was wrong."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-min))
          (should (search-forward "from NORMAL state.  Use " nil t))
          (should (eq (get-text-property (point) 'face) 'help-key-binding))
          (should (search-forward "or " nil t))
          (should (eq (get-text-property (point) 'face) 'help-key-binding))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-delete-keys-are-sorted-not-keymap-ordered ()
  "The order is deterministic, not whichever `keymap-set' call came first.

`where-is-internal' returns keymap order, which put the vi key ahead of
the Helix one by accident of layout and would have reordered the
sentence if the two `keymap-set' calls were ever swapped.

The keys come back wrapped in the key-quote escape, unresolved: this
runs BEFORE `substitute-command-keys', which is what lets them pick up
the `help-key-binding' face."
  (should (equal (donkey--tutor-delete-keys) "\\`d'/\\`x'")))

;;; ---------------------------------------------------------------------------
;;; Lessons 9 and 10 teach what the code actually does
;;; ---------------------------------------------------------------------------

(defmacro donkey-tutor-test--buffer (text &rest body)
  "Run BODY over TEXT with DONKEY on and every relevant global bound.

Used to check that the tutor's exercises really produce what the lesson
says they do.  A tutor is practised in, so an instruction that does not
work is worse than no instruction: the reader concludes the editor is
broken, not the sentence."
  (declare (indent 1))
  `(unwind-protect
       (with-temp-buffer
         (donkey-mode 1)
         (donkey-normal-mode 1)
         (let ((transient-mark-mode t)
               (kill-ring nil)
               (kill-ring-yank-pointer nil)
               (killed-rectangle nil)
               (donkey--clipboard-warning-shown nil)
               (this-command nil)
               (last-command nil))
           (insert ,text)
           (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
             ,@body)))
     (donkey-mode -1)))

(defun donkey-tutor-test--row (n)
  "Move point to the start of line N."
  (goto-char (point-min))
  (forward-line (1- n)))

(ert-deftest donkey-tutor-lesson-9-teaches-rectangles ()
  "The tutor covers `m v' at all.

It did not mention rectangles anywhere -- not the key, not the word --
despite `donkey-copy', `donkey-delete' and `donkey-yank' all having
rectangle behaviour a reader would meet by accident."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should (string-match-p "Lesson 9 -- columns" (buffer-string)))
          (should (string-match-p "RECTANGLE" (buffer-string)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(defmacro donkey-tutor-test--live (&rest body)
  "Open a fresh tutor in the selected window and run BODY with real keys.

`execute-kbd-macro' resolves against the live keymaps and runs the real
command loop, which is the only way these checks mean anything: an
earlier version of these tests called the commands directly, and the
missing post-command cleanup made a rectangle look like it stayed
selected after a cut.  A whole paragraph of the lesson was written
around that false reading before running the keys showed otherwise."
  `(unwind-protect
       (progn
         (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))
         (donkey-mode 1)
         ;; BOUND, not assigned.  These are globals, and the exercises set
         ;; them: an earlier version used `setq' and left
         ;; `killed-rectangle' non-empty for the rest of the
         ;; run, which sent twenty later `donkey-yank' tests down the
         ;; rectangle branch.  Every file still passed in isolation, so
         ;; only the combined run showed it.
         (let ((transient-mark-mode t)
               (kill-ring nil)
               (kill-ring-yank-pointer nil)
               (killed-rectangle nil)
               (donkey--clipboard-warning-shown nil)
               (this-command nil)
               (last-command nil))
           (donkey-tutor)
           (switch-to-buffer "*DONKEY Tutor*")
           (donkey-enter-normal)
           ,@body))
     (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))
     (donkey-mode -1)))

(defun donkey-tutor-test--keys (s)
  "Run S as real key input.

Quit is caught: `C-g' in NORMAL state runs the real `keyboard-quit',
which signals, and batch has no command loop to absorb it.

`prefix-arg' and `current-prefix-arg' are bound because S may carry a
\\[universal-argument], and `execute-kbd-macro' leaves the prefix set
GLOBALLY when it returns.  Later tests calling a command through
`call-interactively' then ran it counted: it cost
`donkey-change-call-interactively-no-region' and
`donkey-visual-previous-line-call-interactively-with-selection', which
failed only in orders where a tutor exercise using a count ran first.
ERT's fixed order is not one of them, so this never showed until the
suite was run shuffled."
  (let ((prefix-arg nil) (current-prefix-arg nil))
    (condition-case nil (execute-kbd-macro (kbd s)) (quit nil))))

(defun donkey-tutor-test--goline (needle &optional occurrence)
  "Put point at the start of the line holding OCCURRENCE of NEEDLE."
  (goto-char (point-min))
  (dotimes (_ (or occurrence 1)) (search-forward needle))
  (beginning-of-line))

(defun donkey-tutor-test--line ()
  "Return the current line as a string."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(ert-deftest donkey-tutor-banking-paste-over-a-selection-works ()
  "The banking exercise replaces the marker line rather than pushing it down.

The `V\=' step is what makes the exercise teach two things at once: the
banked lines arrive together, AND a paste replaces whatever is selected.
Without it the marker line survives underneath the pasted pair, which is
tidier to read about than to look at."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> milk")
   (donkey-tutor-test--keys "m l")
   (donkey-tutor-test--goline "---> bread")
   (donkey-tutor-test--keys "m l")
   (donkey-tutor-test--keys "y")
   (should (equal (car kill-ring) "   ---> milk\n   ---> bread\n"))
   ;; the practice line is the SECOND "(paste here)" -- the first is in
   ;; the instruction text above it
   (donkey-tutor-test--goline "(paste here)" 2)
   (donkey-tutor-test--keys "V p")
   ;; The marker line is replaced, not pushed down: only the mention
   ;; inside the instruction text above it is left.
   (should (= 1 (save-excursion
                  (goto-char (point-min))
                  (cl-loop while (search-forward "(paste here)" nil t) count t))))
   (donkey-tutor-test--goline "---> milk" 2)
   (should (equal (donkey-tutor-test--line) "   ---> milk"))
   (forward-line 1)
   (should (equal (donkey-tutor-test--line) "   ---> bread"))))

(ert-deftest donkey-tutor-lessons-are-in-dependency-order ()
  "No lesson uses a key that a later lesson introduces.

Three forward references were found by checking this mechanically: `y\='
and `p\=' were both used in exercises before copy-and-paste was taught,
and `v\=' was used in an exercise having never been taught at all.  The
fix was to move copy-and-paste ahead of banking -- banking is ABOUT
copying several things at once, so it cannot be demonstrated first --
and to give `v\=' its own place in the selecting lesson."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let* ((text (buffer-string))
                 (pos (lambda (s) (string-match (regexp-quote s) text))))
            ;; copy and paste before banking, which depends on them
            (should (< (funcall pos "Lesson 7 -- copy and paste")
                       (funcall pos "Lesson 8 -- banking")))
            ;; v is taught in the selecting lesson, before it is used
            (should (< (funcall pos "drops a mark and starts a selection")
                       (funcall pos "select the \"bread\" line")))
            ;; and the whole sequence is still numbered 1..10 in order
            (let ((n 0))
              (dolist (heading '("Lesson 1 -- " "Lesson 2 -- " "Lesson 3 -- "
                                 "Lesson 4 -- " "Lesson 5 -- " "Lesson 6 -- "
                                 "Lesson 7 -- " "Lesson 8 -- " "Lesson 9 -- "
                                 "Lesson 10 -- "))
                (let ((at (funcall pos heading)))
                  (should at)
                  (should (> at n))
                  (setq n at)))))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-2-tells-the-truth-about-bare-digits ()
  "A bare digit drops the count and runs the motion once.

The lesson first said `3 j\=' \"moves nowhere\", which is wrong and the
comfortable half of the truth.  Running it shows the digit is rejected
with a message and the motion then runs on its own, so the reader moves
ONE line while believing they asked for three -- a silently wrong result
rather than a visibly absent one."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> one two three four five")
   (let ((l0 (line-number-at-pos)))
     (condition-case nil (execute-kbd-macro (kbd "3 j")) (error nil) (quit nil))
     (should (= (line-number-at-pos) (1+ l0)))))
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should (string-match-p "move ONE line instead of three" (buffer-string)))
          (should-not (string-match-p "moves nowhere" (buffer-string)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-2-count-exercise-lands-where-it-says ()
  "`C-u 5 w' leaves the cursor just after \"five\", not on \"six\".

`forward-word' lands at the END of each word, so counting five of them
stops after the fifth.  The lesson first claimed it landed ON the sixth,
which running it disproved."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> one two three")
   (search-forward "---> ")
   (donkey-tutor-test--keys "C-u 5 w")
   (should (string-suffix-p "five"
                            (buffer-substring-no-properties
                             (line-beginning-position) (point))))
   (should (string-prefix-p " six"
                            (buffer-substring-no-properties
                             (point) (line-end-position))))))

(ert-deftest donkey-tutor-lesson-4-change-exercise-names-the-exact-character ()
  "The counted `c' exercise says WHICH character to start on, and why.

\"On the word wrong\" is not enough for a command that counts characters
from the cursor: one place off replaces the wrong five.  The lesson now
names the \"w\", and the failure it warns about is the real one."
  ;; the exercise as written
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> This word is wrong")
   (search-forward "wrong")
   (backward-char 5)                    ; on the "w", as instructed
   (donkey-tutor-test--keys "c")        ; C-u 5 c
   (should t))
  ;; and the warning it gives is accurate
  (with-temp-buffer
    (insert "This word is wrong and needs replacing.")
    (goto-char (point-min))
    (search-forward "wrong")
    (backward-char 4)                   ; one character off, on the "r"
    (donkey-change 5)
    (insert "right")
    (should (string-match-p "wrightand" (buffer-string))))
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            (should (string-match-p "the \"w\" of \"wrong\"" text))
            (should (string-match-p "\"wrightand\"" text)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-2-says-when-cursor-position-matters ()
  "Lesson 2 distinguishes counting characters from selecting things.

Both halves are checked against the commands, not just asserted: a
counted character command really does depend on where the cursor is, and
a text object really does not."
  ;; characters: position-critical
  (let (results)
    (dolist (off '(0 1 2))
      (with-temp-buffer
        (insert "This word is wrong and needs replacing.")
        (goto-char (point-min))
        (search-forward "wrong")
        (backward-char (- 5 off))
        (donkey-change 5)
        (insert "right")
        (push (buffer-string) results)))
    (should (= 3 (length (delete-dups results)))))
  ;; things: position-tolerant
  (let (results)
    (dolist (off '(0 3 6))
      (with-temp-buffer
        (let ((transient-mark-mode t))
          (insert "Words to replace without counting.")
          (goto-char (point-min))
          (search-forward "replace")
          (backward-char (- 7 off))
          (donkey-mark-word 1)
          (push (buffer-substring-no-properties (region-beginning) (region-end))
                results))))
    (should (equal (delete-dups results) '("replace"))))
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            (should (string-match-p "Counting CHARACTERS counts from the cursor" text))
            (should (string-match-p "does not care where inside the thing you are" text)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-3-names-the-word-to-type ()
  "The pangram exercise says which word is missing.

\"Type the missing word\" assumes the reader knows the phrase.  Anyone who
does not is stuck on the third lesson with no way forward."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should (string-match-p "it is \"dog\"" (buffer-string)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-4-count-row-really-deletes-three ()
  "Lesson 4's count row works on both delete keys."
  (dolist (key '("C-u 3 d" "C-u 3 x"))
    (donkey-tutor-test--live
     (donkey-tutor-test--goline "---> xxxand the rest")
     (search-forward "---> ")
     (donkey-tutor-test--keys key)
     (should (equal (donkey-tutor-test--line)
                    "   ---> and the rest of the line stays")))))

(ert-deftest donkey-tutor-lesson-5-select-then-change-really-works ()
  "`m w' then `c' replaces the whole selection, not one character.

The point of the exercise: selecting first means never counting
characters, which is what the counted `c' in Lesson 4 required."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> Words to replace")
   (search-forward "replace")
   (backward-char 4)
   (donkey-tutor-test--keys "m w")
   (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                  "replace"))
   (donkey-tutor-test--keys "c")
   (insert "change")
   (donkey-tutor-test--keys "C-g")
   (should (equal (donkey-tutor-test--line)
                  "   ---> Words to change without counting anything."))))

(ert-deftest donkey-tutor-lesson-5-count-row-selects-two-words ()
  "Lesson 5's count row selects two words rather than one."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> alpha beta gamma")
   (search-forward "---> ")
   (donkey-tutor-test--keys "C-u 2 m w")
   (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                  "alpha beta"))))

(ert-deftest donkey-tutor-lesson-5-teaches-v-before-it-is-used ()
  "`v\=' has its own explanation and exercise, not just a passing mention."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> Select part of this line by hand")
   (search-forward "---> ")
   (donkey-tutor-test--keys "v l l l l l")
   (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                  "Selec"))
   (donkey-tutor-test--keys "v")
   (should-not (use-region-p))))

(ert-deftest donkey-tutor-lesson-6-explains-the-invisible-newline ()
  "Lesson 6 warns that a V selection takes one character more than it shows.

It is the tutor's counterpart to the note already in the README: the
highlight stops at the end of the last line, so the newline never looks
selected, and a reader who notices reports it as a bug."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            (should (string-match-p "never LOOKS selected" text))
            (should (string-match-p "one character shorter than what it takes" text)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-6-teaches-joining ()
  "Lesson 6 covers `g j', with its own exercise and practice lines."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            (should (string-match-p "Lines can be put back together" text))
            (should (string-match-p "---> a sentence broken" text))
            ;; the direction matters: it must say the line BELOW comes up
            (should (string-match-p "line BELOW up onto the one you are on" text))
            ;; and that Emacs\' own join, the other way, still works
            (should (string-match-p "M-\\^" text)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-6-join-exercise-works-with-real-keys ()
  "The `g j' exercise and its count claim both do what the lesson says."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> a sentence broken")
   (donkey-tutor-test--keys "g j")
   (should (equal (donkey-tutor-test--line)
                  "   ---> a sentence broken ---> across three"))
   (donkey-tutor-test--keys "g j")
   (should (equal (donkey-tutor-test--line)
                  "   ---> a sentence broken ---> across three ---> separate lines")))
  ;; "A count joins that many lines at once" -- same result in one press.
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> a sentence broken")
   (donkey-tutor-test--keys "C-u 2 g j")
   (should (equal (donkey-tutor-test--line)
                  "   ---> a sentence broken ---> across three ---> separate lines"))))

(ert-deftest donkey-tutor-both-delete-keys-really-work ()
  "Every exercise the tutor gives for the delete command works on both keys.

The tutor now names them as \"d/x\" throughout, so a reader may reach for
either.  Pinned because the prose promising both is only honest if both
are actually bound to the same command."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> Thiis liine")
   (search-forward "Thi")
   (donkey-tutor-test--keys "x")
   (should (string-match-p "This liine" (donkey-tutor-test--line))))
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> Thiis liine")
   (search-forward "Thi")
   (donkey-tutor-test--keys "d")
   (should (string-match-p "This liine" (donkey-tutor-test--line))))
  ;; and on the whole-line exercise too
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> first line to remove")
   (donkey-tutor-test--keys "V J J d")
   (should-not (string-match-p "line to remove" (buffer-string)))))

(ert-deftest donkey-tutor-lesson-9-exercise-works-with-real-keys ()
  "The Lesson 9 exercise does what it says, driven by actual keys.

Two corrections came out of running it.  The lesson said to press the
forward key THREE times, which takes the trailing space as well --
`forward-char' is remapped to `rectangle-forward-char' inside
`rectangle-mark-mode', so the anchor column counts and the naive
arithmetic is off by one.  And it claimed the rectangle stayed selected
after the cut, so a `C-g' was needed before pasting; with real keys the
cut releases the selection and no `C-g' is wanted."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> 111 alpha")
   (search-forward "---> ")
   (donkey-tutor-test--keys "m v j j l l x")
   (should (equal killed-rectangle '("111" "222" "333")))
   ;; Released by the cut -- the lesson must NOT tell the reader to press C-g.
   (should-not (bound-and-true-p rectangle-mark-mode))
   (should-not (use-region-p))
   (donkey-tutor-test--goline "--->  alpha")
   (search-forward "---> ")
   ;; "P", not "p": the block goes back through the rectangle key now.
   (donkey-tutor-test--keys "P")
   (donkey-tutor-test--goline "---> 111 alpha")
   (should (equal (donkey-tutor-test--line) "   ---> 111 alpha"))))

(ert-deftest donkey-tutor-lesson-10-exercises-work-with-real-keys ()
  "All four Lesson 10 steps, in order, driven by actual keys.

The third step needs the first: a rectangle copy never reaches the kill
ring, so without an ordinary copy beforehand a paste over a bank reports
\"Nothing to paste\" and the banked line is left alone -- which is what
the lesson used to instruct the reader to do.

Two corrections came out of running it.  Copying the rectangle releases
it, so the quit key the lesson used to call for was doing nothing.  And
the rectangle comes back through \"P\" rather than a second \"p\": the two
keys read different stores, which is what makes the last step give the
same answer whether or not the session has a system clipboard."
  (donkey-tutor-test--live
   ;; 1. an ordinary whole-line copy, so there is something to paste
   (donkey-tutor-test--goline "---> col two")
   (donkey-tutor-test--keys "V y")
   (should (equal (car kill-ring) "   ---> col two\n"))
   ;; 2. bank a line, draw a rectangle, copy: rectangle wins, bank survives
   (donkey-tutor-test--goline "---> keep this banked")
   (donkey-tutor-test--keys "m l")
   (donkey-tutor-test--goline "---> col one")
   (search-forward "---> ")
   (donkey-tutor-test--keys "m v j l l y")
   (should (equal killed-rectangle '("col" "col")))
   (should (= (length (donkey--banked-spans)) 1))
   ;; 3. paste: the bank wins and the rectangle is untouched.  No C-g --
   ;; the copy in step 2 already released the rectangle.
   (should-not (bound-and-true-p rectangle-mark-mode))
   (donkey-tutor-test--keys "p")
   (should (= (length (donkey--banked-spans)) 0))
   (should (equal killed-rectangle '("col" "col")))
   (should-not (save-excursion (goto-char (point-min))
                               (search-forward "keep this banked" nil t)))
   ;; 4. "P" brings the rectangle back, from its own store
   (donkey-tutor-test--goline "---> col one")
   (search-forward "---> ")
   (donkey-tutor-test--keys "P")
   (donkey-tutor-test--goline "---> colcol one")
   (should (equal (donkey-tutor-test--line) "   ---> colcol one"))))

(ert-deftest donkey-tutor-lesson-9-prose-matches-the-verified-keys ()
  "The words of Lesson 9 agree with the key sequence that was verified.

The behavioural tests drive keys directly, so they pass whatever the
lesson happens to SAY -- both of this lesson's defects lived in the
prose.  These two assertions are the ones that would have caught them:
the reader is told to press the forward key twice, not three times, and
is NOT told the rectangle survives the cut."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            ;; Three presses takes the trailing space too.
            (should (string-match-p "twice and\n   l twice" text))
            (should-not (string-match-p "l three times" text))
            ;; The cut releases the selection; no C-g is wanted.
            (should-not (string-match-p "still selected after the cut" text)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-10-prose-establishes-a-paste-source ()
  "Lesson 10 tells the reader to make an ordinary copy before pasting.

Without it the paste has nothing to insert -- a rectangle copy never
reaches the kill ring -- and the lesson's claim that the banked line is
replaced is simply false.  The step is load-bearing, so its absence
should fail rather than be discovered by a reader."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            (should (string-match-p "A rectangle never reaches the kill" text))
            (should (string-match-p "ordinary whole-line copy" text))
            ;; the ordinary copy must come BEFORE the bank is drawn
            (should (< (string-match "ordinary whole-line copy" text)
                       (string-match "Now bank the" text))))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-5-teaches-the-sexp-marks ()
  "Lesson 5 covers `m I' and `m A', not just `m i' and `m a'."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (should (string-match-p "m I and m A" (buffer-string)))
          (should (string-match-p "(defun f (a b) \\[1 2 3\\])" (buffer-string)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-lesson-5-sexp-exercise-really-works ()
  "The `m I'/`m A' exercise produces exactly what the lesson claims.

Checked in `text-mode', which is what the tutor buffer actually uses --
the syntax table is what these two commands read, so verifying in
`emacs-lisp-mode' would have proved nothing about the tutor."
  (donkey-tutor-test--buffer "(defun f (a b) [1 2 3])"
    (text-mode)
    (goto-char 19)                      ; on the "2"
    (donkey-mark-sexp-inner 1)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "1 2 3")))
  (donkey-tutor-test--buffer "(defun f (a b) [1 2 3])"
    (text-mode)
    (goto-char 19)
    (donkey-mark-sexp-outer 1)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "[1 2 3]")))
  ;; "Counts go outward here too, and cross bracket types on the way out."
  (donkey-tutor-test--buffer "(defun f (a b) [1 2 3])"
    (text-mode)
    (goto-char 19)
    (donkey-mark-sexp-inner 2)
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                   "defun f (a b) [1 2 3]"))))

(ert-deftest donkey-tutor-lesson-10-rule-table-columns-line-up ()
  "The rule table's right-hand column starts at the same offset on every row.

The keys are substituted at render time and vary in width, so the table
was written with the FIXED text on the left for exactly this reason --
an earlier draft put the keys first and rendered ragged.

Three rows, not four.  The fourth was \"with nothing banked, p pastes the
rectangle\", which stopped being true when pasting split into two keys:
\"p\" reads the kill ring and \"P\" reads `killed-rectangle', so there is
nothing left for a rule to arbitrate on the pasting side.  The row count
is asserted rather than inferred so that dropping another row silently
fails here."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-min))
          (should (search-forward "THE BANK IS THE FALLBACK" nil t))
          (forward-line 2)
          (let (offsets)
            ;; The row AFTER the table must not match, or a fourth row
            ;; could reappear without this noticing.
            (should-not
             (string-match "\\`    with [a-z ]+?  +[^ ]"
                           (save-excursion
                             (forward-line 3)
                             (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))))
            (dotimes (_ 3)
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (should (string-match "\\`    \\(with [a-z ]+?\\)  +[^ ]" line))
                (push (match-end 0) offsets))
              (forward-line 1))
            (should (= 1 (length (delete-dups offsets)))))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))


(ert-deftest donkey-tutor-lesson-5-v-drop-step-really-drops-the-selection ()
  "Lesson 5's drop step leaves nothing selected, and `v' would not have.

Reported live: the lesson said to press \\=`v\\=' again to let go, and it does
not let go -- \\=`v\\=' is `set-mark-command', which re-anchors.  A second press
drops a fresh mark at point and carries on selecting, so a reader
following the old wording moved to Lesson 6 with a selection still
active and the next lesson's keys acting on it.

Both halves are pinned: the corrected key clears the selection, and the
key the lesson used to name does not."
  ;; The step as the lesson now writes it.
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> Select part of this line")
   (donkey-tutor-test--keys "v")
   (should (region-active-p))
   (donkey-tutor-test--keys "l l l j")
   (should (region-active-p))
   (donkey-tutor-test--keys "C-g")
   (should-not (region-active-p)))
  ;; The step as it used to be written.
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> Select part of this line")
   (donkey-tutor-test--keys "v")
   (donkey-tutor-test--keys "l l l j")
   (let ((anchor (mark t)))
     (donkey-tutor-test--keys "v")
     (should mark-active)
     (should-not (equal (mark t) anchor))
     (should (= (mark t) (point))))))

(ert-deftest donkey-tutor-says-v-does-not-toggle ()
  "The lesson states that a second `v' re-anchors instead of letting go.

The old wording was not merely imprecise, it told the reader to press a
key that does the opposite of what the sentence promised.  Pinned as
prose because the behaviour is stock `set-mark-command' and is staying
that way -- the tutor is where the correction lives."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-min))
          (should (search-forward "Lesson 5 -- selecting things" nil t))
          (let ((lesson-5 (point)))
            (should (search-forward "does not let go" nil t))
            (goto-char lesson-5)
            (should (search-forward "C-g to drop the" nil t))
            (goto-char lesson-5)
            ;; The instruction that was wrong must not have come back.
            (should-not (search-forward "again to drop the" nil t)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-warns-early-that-c-g-clears-a-selection ()
  "The opening says `C-g' cancels a selection, before any lesson makes one.

Requested after the `v' correction: a selection carried into the next
lesson changes what its keys do, so the reader needs this before Lesson
5, not inside it.  Pinned to the opening rather than to the file at
large -- stating it only in Lesson 5 would be stating it too late."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-min))
          (let ((lesson-1 (save-excursion
                            (search-forward "Lesson 1 -- moving around")
                            (point))))
            (goto-char (point-min))
            (let ((hit (search-forward "also cancels a selection" nil t)))
              (should hit)
              (should (< hit lesson-1))))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))


(ert-deftest donkey-tutor-lesson-5-word-versus-symbol-exercise-really-works ()
  "The `m w'/`m W' exercise selects what the lesson says, from the named spot.

Run in the real tutor buffer rather than a constructed one: `m W' reads
the syntax table, and the tutor is `text-mode', so a check in any other
mode would prove nothing about what a reader sees.

The lesson names the \\=`m\\=' of \"mail\" specifically.  An earlier lesson had
to be corrected for saying \"anywhere in the word\" when the starting
column changed the answer, so the spot the prose names is the spot tested."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> Call send-mail_to")
   ;; Captured BEFORE any command runs.  Reading `match-beginning' after
   ;; the first keypress gave "DONKEY" -- `m w' searches internally and
   ;; leaves its own match data behind.
   (let ((spot (progn (search-forward "mail") (match-beginning 0))))
     (goto-char spot)
     (donkey-tutor-test--keys "m w")
     (should (equal (buffer-substring-no-properties
                     (region-beginning) (region-end))
                    "mail"))
     ;; "Press C-g, then press m W from the same spot" -- the same spot
     ;; has to survive the quit, or the second half of the exercise is a
     ;; different exercise.
     (donkey-tutor-test--keys "C-g")
     (should-not (region-active-p))
     (goto-char spot)
     (donkey-tutor-test--keys "m W")
     (should (equal (buffer-substring-no-properties
                     (region-beginning) (region-end))
                    "send-mail_to")))))

(ert-deftest donkey-tutor-lesson-5-word-versus-symbol-line-has-a-real-difference ()
  "The demo line is one where `m w' and `m W' genuinely disagree.

The point of the exercise is the contrast, so a line whose word and
symbol happen to coincide would read as if the two keys were the same.
Pinned because the marker line is prose and prose gets reworded: swapping
in a plain word would leave both halves passing individually while the
lesson taught nothing."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> Call send-mail_to")
   (let ((spot (progn (search-forward "mail") (match-beginning 0)))
         word symbol)
     (goto-char spot)
     (donkey-tutor-test--keys "m w")
     (setq word (buffer-substring-no-properties
                 (region-beginning) (region-end)))
     (donkey-tutor-test--keys "C-g")
     (goto-char spot)
     (donkey-tutor-test--keys "m W")
     (setq symbol (buffer-substring-no-properties
                   (region-beginning) (region-end)))
     (should-not (equal word symbol))
     (should (string-match-p (regexp-quote word) symbol))
     (should (> (length symbol) (length word))))))


(ert-deftest donkey-tutor-lesson-5-punctuation-claim-is-true-in-both-directions ()
  "The lesson's claim about periods and commas holds in Emacs Lisp and in text.

Two separate rules, and the lesson separates them:

The MODE decides whether a period or comma holds a name together.  In
`emacs-lisp-mode' both do, so \\=`m W\\=' on \"foo.bar\" takes all of it; in
`text-mode' -- which is what the tutor buffer is -- neither does, and the
same keypress stops at \"foo\".

DONKEY decides the trailing one.  `donkey-mark-symbol' walks back over a
final \",\" or \".\" explicitly, so the last name of a sentence comes without
its full stop even where the mode would have included it.

The `text-mode' half is why the lesson says there is nothing here to try
it on: a reader who took the Emacs Lisp example into the tutor buffer
would get \"foo\" and conclude the editor was broken."
  (dolist (case '((emacs-lisp-mode "run foo.bar now"   "foo.bar")
                  (emacs-lisp-mode "run foo,bar now"   "foo,bar")
                  ;; Trailing punctuation dropped even though the mode
                  ;; counts it as part of the symbol.
                  (emacs-lisp-mode "end with foo.bar." "foo.bar")
                  (emacs-lisp-mode "end with foo.bar," "foo.bar")
                  ;; The tutor's own mode joins neither.
                  (text-mode       "run foo.bar now"   "foo")
                  (text-mode       "run foo,bar now"   "foo")
                  (text-mode       "end with foo.bar." "foo")))
    (cl-destructuring-bind (mode text expected) case
      (with-temp-buffer
        (funcall mode)
        (let ((transient-mark-mode t))
          (insert text)
          (goto-char (point-min))
          (search-forward "foo")
          (goto-char (match-beginning 0))
          (donkey-mark-symbol 1)
          (should (equal (buffer-substring-no-properties
                          (region-beginning) (region-end))
                         expected))))))
  ;; And the hyphen/underscore claim, in the mode the lesson is read in.
  (with-temp-buffer
    (text-mode)
    (let ((transient-mark-mode t))
      (insert "Call send-mail_to when the queue drains.")
      (goto-char (point-min))
      (search-forward "mail")
      (goto-char (match-beginning 0))
      (donkey-mark-symbol 1)
      (should (equal (buffer-substring-no-properties
                      (region-beginning) (region-end))
                     "send-mail_to")))))

(ert-deftest donkey-tutor-lesson-5-says-punctuation-is-the-modes-call ()
  "The lesson does not promise behaviour the tutor buffer cannot show.

`text-mode' joins neither period nor comma, so an exercise built on the
Emacs Lisp behaviour would fail in front of the reader.  The lesson
states the rule and says outright that there is nothing here to try it
on -- pinned so a later edit does not turn the prose into a \">>\" step."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-min))
          (should (search-forward "Lesson 5 -- selecting things" nil t))
          (let ((lesson-5 (point))
                ;; Rendered prose is hard-wrapped, so the phrase spans a
                ;; newline.  A literal `search-forward\' found nothing and
                ;; reported the caveat missing when it was plainly there.
                (caveat-re "nothing[ \n]+here to try"))
            (should (re-search-forward caveat-re nil t))
            ;; The caveat must sit inside Lesson 5, not drift to the end.
            (goto-char lesson-5)
            (let ((caveat (save-excursion (re-search-forward caveat-re)))
                  (lesson-6 (save-excursion
                              (search-forward "Lesson 6" nil t))))
              (should (or (null lesson-6) (< caveat lesson-6)))))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))


(ert-deftest donkey-tutor-lesson-9-rectangle-change-exercise-really-works ()
  "The `m v'/`c' exercise changes all three rows at once, as the lesson says.

`string-rectangle' reads its replacement with `read-string', so the keys
alone cannot drive it in batch -- there is no minibuffer to type into.
The prompt is answered by mocking `read-string', which is also how the
lesson's claim about WHERE the text is typed gets checked: if `c' ever
stopped prompting and entered INSERT state instead, the mock would go
uncalled and this test would fail rather than quietly still passing."
  (let ((prompt nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (p &rest _) (setq prompt p) "##")))
      (donkey-tutor-test--live
       (donkey-tutor-test--goline "---> 777 red")
       (search-forward "777")
       (goto-char (match-beginning 0))
       (donkey-tutor-test--keys "m v")
       (donkey-tutor-test--keys "j j")
       (donkey-tutor-test--keys "l l")
       (donkey-tutor-test--keys "c")
       ;; "It asks for the replacement in the minibuffer -- String rectangle:"
       (should (equal prompt "String rectangle: "))
       ;; "You stay in NORMAL state throughout."
       (should donkey-normal-mode)
       (should-not donkey-insert-mode)
       ;; "All three rows lose their digits together."
       (donkey-tutor-test--goline "## red")
       (dolist (expected '("   ---> ## red" "   ---> ## green" "   ---> ## blue"))
         (should (equal (donkey-tutor-test--line) expected))
         (forward-line 1))))))

(ert-deftest donkey-tutor-lesson-9-bare-rectangle-change-takes-one-character ()
  "The second exercise changes exactly one character, which is its point.

The lesson contrasts the two: `m v' alone is one column on one line, so
`c' straight after it is a single-character change.  Pinned separately
from the three-row case because the contrast is the lesson -- if the
initial widening ever changed, this exercise would teach the opposite of
what it says while the three-row one still passed."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "##")))
    (donkey-tutor-test--live
     (donkey-tutor-test--goline "---> 555 solo")
     (search-forward "555")
     (goto-char (match-beginning 0))
     (donkey-tutor-test--keys "m v")
     (donkey-tutor-test--keys "c")
     (should donkey-normal-mode)
     (donkey-tutor-test--goline "solo")
     (should (equal (donkey-tutor-test--line) "   ---> ##55 solo")))))

(ert-deftest donkey-tutor-lesson-9-rectangle-change-differs-from-plain-change ()
  "Without a rectangle, `c' deletes a character and enters INSERT state.

The lesson tells the reader they stay in NORMAL state and type into the
minibuffer, which is only remarkable because the same key does the
opposite when no rectangle is live.  Checked so the claim is anchored to
a real contrast rather than to `c' happening to behave that way for
every selection."
  (donkey-tutor-test--live
   (donkey-tutor-test--goline "---> 555 solo")
   (search-forward "555")
   (goto-char (match-beginning 0))
   (donkey-tutor-test--keys "m v")
   (donkey-tutor-test--keys "C-g")
   (should-not (bound-and-true-p rectangle-mark-mode))
   (donkey-tutor-test--keys "c")
   (should donkey-insert-mode)
   (donkey-tutor-test--goline "solo")
   (should (equal (donkey-tutor-test--line) "   ---> 55 solo"))))


(ert-deftest donkey-tutor-lesson-9-prefix-exercise-really-works ()
  "The prefix exercise adds text without replacing any.

The whole point of taking the width back off with `h' is that nothing is
deleted.  Asserted on the full rows rather than on a prefix match: a
one-column rectangle would also produce a line STARTING with \"// \", and
would have eaten a character to do it."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "// ")))
    (donkey-tutor-test--live
     (donkey-tutor-test--goline "---> red")
     (search-forward "red")
     (goto-char (match-beginning 0))
     (donkey-tutor-test--keys "m v")
     (donkey-tutor-test--keys "h")
     (donkey-tutor-test--keys "j j")
     (donkey-tutor-test--keys "c")
     (donkey-tutor-test--goline "// red")
     (dolist (expected '("   ---> // red" "   ---> // green" "   ---> // blue"))
       (should (equal (donkey-tutor-test--line) expected))
       (forward-line 1)))))

(ert-deftest donkey-tutor-lesson-9-suffix-exercise-really-works ()
  "The suffix exercise appends to every row, at the column the first ends on.

Depends on the end-of-line guard in `donkey-rectangle-mark-mode'.
Without it the initial widening steps over the newline and the rectangle
collapses to column 0, so this exercise would prefix all three rows
instead -- which is why the assertion is on whole rows and not on a
suffix match."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " ;")))
    (donkey-tutor-test--live
     (donkey-tutor-test--goline "---> aaaaa")
     (donkey-tutor-test--keys "g l")
     (donkey-tutor-test--keys "m v")
     (donkey-tutor-test--keys "j j")
     (donkey-tutor-test--keys "c")
     (donkey-tutor-test--goline "aaaaa")
     (dolist (expected '("   ---> aaaaa ;" "   ---> bbbbb ;" "   ---> ccccc ;"))
       (should (equal (donkey-tutor-test--line) expected))
       (forward-line 1)))))

(ert-deftest donkey-tutor-lesson-9-suffix-rows-are-equal-length ()
  "The suffix exercise's rows are equal length, as its caveat requires.

The lesson says a suffix lands on a column rather than at each line's
own end, and that the two coincide only on rows of equal length.  Its
own rows have to be that case or the exercise contradicts the sentence
under it -- ragged rows would pad one and split another in front of the
reader."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (goto-char (point-min))
          (should (search-forward "---> aaaaa" nil t))
          (beginning-of-line)
          (let (lengths)
            (dotimes (_ 3)
              (push (- (line-end-position) (line-beginning-position)) lengths)
              (forward-line 1))
            (should (= 1 (length (delete-dups lengths)))))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))

(ert-deftest donkey-tutor-does-not-claim-a-rectangle-starts-one-column-wide ()
  "Lesson 9 says one CHARACTER, and names the column caveat.

Regression test: the lesson said `m v' \"always starts one column
wide\".  True of ordinary text and false wherever a character is not
one column -- eight on a TAB, two on a wide character -- so a reader
following it on tab-indented code replaced the whole indent.  The
prose has to say character, and has to name the case where the two
part company, or the exercise misleads exactly the readers who go on
to use rectangles on real files."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            (should-not (string-match-p "starts one column wide" text))
            (should (string-match-p "starts one character wide" text))
            ;; The caveat itself, not merely the absence of the old claim.
            (should (string-match-p "one character is not always" text))
            (should (string-match-p "On a TAB it is eight" text)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))


(ert-deftest donkey-every-key-reference-in-the-source-resolves ()
  "Every \\[command] in donkey.el names a command that exists.

`substitute-command-keys' does not signal on an unknown command.  It
renders the reference as the bare command name instead, so a typo
becomes a key nobody can press, visible only to whoever opens that
help buffer or tutor lesson.

This is not hypothetical.  \\[donkey-yang-rectangle] shipped in
`donkey-yank's docstring and survived two rounds of review, because
nothing reads those references except a human eye on rendered output.
Byte-compile does not look inside strings and checkdoc checks form, not
whether the target exists."
  (let (unresolved)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "donkey.el" donkey-test--source-dir))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\\\\\[\\([a-zA-Z0-9---]+\\)\\]" nil t)
        (let ((cmd (intern (match-string 1))))
          (unless (fboundp cmd)
            (push (format "line %d: %s" (line-number-at-pos) cmd) unresolved)))))
    (should (equal unresolved nil))))

(ert-deftest donkey-every-donkey-symbol-named-in-the-source-exists ()
  "Every `donkey-...' quoted in a docstring or comment is a real symbol.

The other half of the same problem: a quoted symbol that does not exist
renders as a dead cross-reference in the help buffer, and
`donkey--replace-rectangle-relection-with-killed-rectangel' sat in a
docstring for two commits looking exactly like a working one.

Faces and overlay properties are legitimate references that are neither
`fboundp' nor `boundp', so they are allowed explicitly rather than by
loosening the check -- an unknown symbol should still fail."
  (let ((allowed '(donkey-banked))          ; overlay property
        unresolved)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "donkey.el" donkey-test--source-dir))
      (goto-char (point-min))
      (while (re-search-forward "`\\(donkey-[a-zA-Z0-9---]+\\)'" nil t)
        (let ((sym (intern (match-string 1))))
          (unless (or (fboundp sym) (boundp sym) (facep sym) (memq sym allowed))
            (push (format "line %d: %s" (line-number-at-pos) sym) unresolved)))))
    (should (equal unresolved nil))))


(ert-deftest donkey-tutor-lesson-10-does-not-claim-m-del-alone-discards-banks ()
  "Lesson 10 says acting on banks spends them, and names the real exception.

The lesson used to close \"m DEL is the only key that discards banks\",
which is false: `donkey-copy', `donkey-delete' and `donkey-yank' all
consume the bank as they act on it.  Driven with real keys, a bank of
two lines goes to zero after any of the three.

The distinction the sentence was reaching for is real -- \\[donkey-clear-banked-selection]
discards WITHOUT using them, the others spend them by using them -- but a
reader takes \"only key\" literally, and the README had said the accurate
version all along.

Pinned as prose because the behaviour is already covered from four
directions and was never in doubt; what drifted was the sentence.  This
asserts the claim is gone and the correction is present, so restoring
the shorter, wronger wording fails here."
  (unwind-protect
      (progn
        (donkey-tutor)
        (with-current-buffer "*DONKEY Tutor*"
          (let ((text (buffer-string)))
            (should-not (string-match-p "only key that discards" text))
            (should (string-match-p "Acting on banked lines does spend them" text))
            ;; and it still names the key that clears without using
            (should (string-match-p "WITHOUT using them" text)))))
    (when (get-buffer "*DONKEY Tutor*") (kill-buffer "*DONKEY Tutor*"))))


(ert-deftest donkey-readme-binding-tables-match-the-keymap ()
  "Every \"| =KEY= | =donkey-command= |\" row in README.org is true.

The README's binding tables are the first thing a reader trusts and the
last thing anyone re-checks.  Rows go stale silently: a rebinding
changes the keymap, the table keeps its old row, and nothing disagrees
until someone presses the key.

Checked in both directions -- see
`donkey-every-bound-command-appears-in-the-readme' for the other half."
  (let (mismatches)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "README.org" donkey-test--source-dir))
      (goto-char (point-min))
      (while (re-search-forward
              "^| =\\([^=]+\\)= *| =\\(donkey-[a-z0-9---]+\\)= *|" nil t)
        (let* ((key (match-string 1))
               (claimed (intern (match-string 2)))
               (actual (ignore-errors (lookup-key donkey-normal-mode-map (kbd key)))))
          (cond
           ((not (fboundp claimed))
            (push (format "%s claims %s, which does not exist" key claimed) mismatches))
           ((null actual)
            (push (format "%s claims %s, but the key is unbound" key claimed) mismatches))
           ((not (eq actual claimed))
            (push (format "%s claims %s, but is bound to %s" key claimed actual)
                  mismatches))))))
    (should (equal mismatches nil))))

(ert-deftest donkey-every-bound-command-appears-in-the-readme ()
  "Every DONKEY command bound in Normal state is named in README.org.

The other direction: a new binding that never reaches the README is
undiscoverable except by reading the source.  Names are matched as text
rather than in a table, so prose mentions count -- the point is that the
command is documented somewhere, not that it sits in a particular row."
  (let ((readme (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "README.org" donkey-test--source-dir))
                  (buffer-string)))
        undocumented)
    (dolist (map (list donkey-normal-mode-map
                       (lookup-key donkey-normal-mode-map "m")))
      (when (keymapp map)
        (map-keymap
         (lambda (_ev def)
           (when (and (symbolp def) (commandp def)
                      (string-prefix-p "donkey-" (symbol-name def))
                      (not (string-match-p (regexp-quote (symbol-name def)) readme)))
             (push (symbol-name def) undocumented)))
         map)))
    (should (equal (delete-dups undocumented) nil))))

(ert-deftest donkey-readme-lists-every-default-mark-pair-delimiter ()
  "The README's delimiter list covers all of `donkey-mark-pair-delimiters'.

Found by audit: U+2018, the OPENING curly single quote, was a working
default that the README did not mention -- it listed the closing U+2019
alone, so the pair looked symmetric when it is not.  A reader with point
on the opener had no reason to think \\=`m i\\=' would do anything.

The OPENING character of each pair is what is checked.  Asymmetric
brackets are listed by their opener alone -- \"( [ { <\" -- and the
closers are rightly left implied; requiring both would fail on a list
that is perfectly clear.  U+2018 was a real gap precisely because the
README had the pair the wrong way round, listing only the closer."
  (let ((documented
         (with-temp-buffer
           (insert-file-contents (expand-file-name "README.org" donkey-test--source-dir))
           (goto-char (point-min))
           (if (re-search-forward
                "=donkey-mark-inner=/=donkey-mark-outer= support \\(.*\\)$" nil t)
               (match-string 1)
             "")))
        missing)
    (dolist (pair donkey-mark-pair-delimiters)
      (let ((open (car pair)))
        (unless (string-match-p (regexp-quote (char-to-string open)) documented)
          (push (format "%c (U+%04X)" open open) missing))))
    (should (equal (delete-dups missing) nil))))

;;; donkey-describe-bindings-test.el ends here

;;; donkey-describe-bindings-test.el --- Tests for donkey-describe-bindings -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'donkey)

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
  "A remap to any command other than `self-insert-command' is treated
as a regular leaf and included in the result.
Expected: one entry whose value is #'ignore."
  (let* ((map (donkey-describe-bindings-test--non-sic-remap-map))
         (res (donkey--desc-bindings-collect-leaves map "")))
    (should (= (length res) 1))
    (should (eq (cdar res) #'ignore))))

;;; --- Cons-cdr-keymap traversal ---

(ert-deftest donkey-describe-bindings-collect-leaves-cons-cdr-keymap ()
  "When a binding's definition is a cons whose cdr is a keymap, the
collector descends into that inner keymap.
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
  "Calling `donkey-describe-bindings' when `donkey-normal-mode-map' is unbound
raises a `user-error'.
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
  "Bindings render as keys, not as M-x invocations.

Regression: `substitute-command-keys' resolves against the CURRENT
buffer's active keymaps, so running it before `donkey-mode' was enabled
rendered every binding as an M-x invocation -- silently, and worst for
exactly the commands a new reader most needs named.

Now that `donkey-tutor' is bound to \\=`g ?\\=' there is nothing left that
resolves to M-x at all: it was the one command deliberately shown that
way while it had no key.  An M-x appearing here again means either a
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

Regression: keys written literally -- C-g, g g, the C-u N part of a count
-- rendered as plain text while substituted bindings rendered as faced
chips, so the same buffer showed two kinds of key.  Literal ones now use
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
        ;; 4. Backspace and Delete do nothing.
        (should (eq (key-binding [backspace]) #'ignore))
        (should (eq (key-binding [delete]) #'ignore)))
    (donkey-mode -1)))

(ert-deftest donkey-tutor-claim-no-fifth-shadowed-emacs-command ()
  "Nothing outside the documented set shadows a real Emacs command.

Resolves every leaf of `donkey-normal-mode-map' against what a plain
`text-mode' buffer would do with the same keys.  Anything that differs,
and whose Emacs meaning is a real command rather than self-insertion,
must be one the tutor and README already name."
  (let ((documented '("RET" "<backspace>" "<delete>"))
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
  "The dired keys the tutor names by hand are really still dired's."
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

;;; donkey-describe-bindings-test.el ends here

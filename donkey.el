;;; donkey.el --- Opinionated Modal Editing -*- lexical-binding: t -*-

;; Copyright (C) 2026 Michael Jones
;; Author: Michael Jones <yardquit@pm.me>
;; Maintainer: Michael Jones
;; Assisted-by: Lumo:2.0 Max
;; Assisted-by: Claude:claude-opus-5
;; Assisted-by: Claude:claude-sonnet-5
;; Assisted-by: Claude:claude-fable-5
;; Assisted-by: Claude:claude-fable-5-1
;; URL: https://github.com/yardquit/donkey
;; Version: 1.7.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; Homepage: https://github.com/yardquit/donkey

;; This file is not part of GNU Emacs.
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Philosophy: Leverage Emacs Native Commands and built-in functions
;; wherever possible.  Custom commands only where beneficial.
;;
;; DONKEY is an addition to Emacs, not a replacement, and it takes as
;; little as a modal editor can -- measured, not promised.  Every `C-x'
;; and `C-c' sequence, `M-x', `C-h', isearch, the arrow keys, every Meta
;; binding and every key your packages bind work exactly as they always
;; did, in both states; no DONKEY keymap binds a Meta key or the ESC
;; prefix at all.  INSERT state is Emacs with one key changed: `C-g'
;; returns to NORMAL state.  NORMAL state differs in four things, all
;; of them named: letters run commands instead of typing, digits are
;; not counts (`C-u 3' is), RET does nothing in a buffer you are
;; editing and is handed back to Dired, Magit and the like, and
;; BACKSPACE and DELETE do nothing.  Nothing else is shadowed, and the
;; test suite walks the keymaps against a plain Emacs buffer to keep
;; each of these sentences true.
;;
;; Optional Smartparens Integration:
;; If you use smartparens, call `(donkey-setup-smartparens)' in
;; your config after loading smartparens to bind C-g in smartparens
;; overlay keymaps.  This improves reliability of C-g escape in terminal
;; mode when inside nested smartparens overlays.

;; Usage:
;; - Press C-g to enter DONKEY-NORMAL state.
;; - In NORMAL: h,j,k,l navigate; i,I,a,A,o,O,c enter INSERT state.
;; - In INSERT: Standard Emacs behavior, press C-g to return to NORMAL.
;; - State indicators show in modeline: DONKEY[N] = Normal, DONKEY[I] = Insert.

;;; Code:

(require 'thingatpt) ;(donkey-mark-word)
(require 'cl-lib)    ; Explicitly load cl-lib for cl-some
(require 'rect)      ; killed-rectangle, extract-rectangle-bounds, etc.
(require 'seq)       ; seq-find
(eval-and-compile
  (declare-function org-open-at-point "org")     ;(donkey-enter-dwim)
  (declare-function org-element-at-point "org")  ;(donkey-enter-dwim)
  (declare-function org-edit-src-exit "org")     ;(donkey-comment-dwim)
  (declare-function org-edit-special "org")      ;(donkey-comment-dwim)
  (declare-function markdown-link-p "markdown-mode")              ;(donkey--markdown-enter-handler)
  (declare-function markdown-wiki-link-p "markdown-mode")         ;(donkey--markdown-enter-handler)
  (declare-function markdown-follow-thing-at-point "markdown-mode") ;(donkey--markdown-enter-handler)
  (defvar donkey-normal-mode-map nil)
  (defvar donkey-insert-mode-map nil))

(defvar this-command)                          ;(donkey--intercept-quit-in-insert)

;;; ---------------------------------------------------------------------------
;;; Donkey Excluded-modes
;;; ---------------------------------------------------------------------------

(defcustom donkey-excluded-modes
  '(comint-mode term-mode vterm-mode eshell-mode)
  "Major modes where DONKEY Normal state should be permanently disabled.

These modes manage subprocess interaction or terminal emulation
where suppressing keys via `suppress-keymap' would break
functionality.  Derived modes (e.g. `shell-mode' from
`comint-mode') are caught by `derived-mode-p' in
`donkey--ensure-default-state'.

For modes like `dired-mode' or `magit-status-mode' where normal
mode is a preference rather than a necessity, add them here
explicitly if desired."
  :type '(repeat symbol)
  :group 'donkey)

;; Every read of a mode-list option goes through `donkey--mode-list':
;; the readers sit on hooks, and a hook function that signals is
;; removed for the session.
(defun donkey--mode-list (value)
  "Return VALUE as a list of major modes, never signaling.

A list is returned with any non-symbol dropped, a bare symbol is taken
as a one-element list, and anything else reads as the empty list."
  (cond ((listp value) (seq-filter #'symbolp value))
        ((symbolp value) (list value))
        (t nil)))

(defun donkey--major-mode-in-p (mode-list)
  "Return non-nil if the current major mode is in MODE-LIST.

Checks both exact membership and derivation via `derived-mode-p', so a
concrete mode (e.g. `shell-mode', derived from `comint-mode') is
caught even when only its parent mode is listed.

MODE-LIST is read through `donkey--mode-list', so a mis-set user
option cannot signal from here."
  (let ((modes (donkey--mode-list mode-list)))
    (or (memq major-mode modes)
        (apply #'derived-mode-p modes))))

(defun donkey--memo-major-mode-in-p (cache-var mode-list)
  "Return `donkey--major-mode-in-p' of MODE-LIST, memoized in CACHE-VAR.

CACHE-VAR names a buffer-local variable holding a cons of the key
\(MAJOR-MODE . SNAPSHOT) and the RESULT, SNAPSHOT being a copy of
MODE-LIST as it was when the entry was computed.  The entry is reused
only while the buffer's `major-mode' is `eq' and MODE-LIST is `equal'
to that snapshot, so a mode change or any change to the user option,
in place or not, recomputes on the next call."
  (let ((cache (symbol-value cache-var)))
    (if (and cache
             (eq (car (car cache)) major-mode)
             (equal (cdr (car cache)) mode-list))
        (cdr cache)
      (let ((result (donkey--major-mode-in-p mode-list)))
        (set cache-var (cons (cons major-mode
                                   (if (listp mode-list)
                                       (copy-sequence mode-list)
                                     mode-list))
                             result))
        result))))

(defvar-local donkey--excluded-mode-cache nil
  "Memo for `donkey--excluded-mode-p'; see `donkey--memo-major-mode-in-p'.")

(defun donkey--excluded-mode-p ()
  "Return non-nil if the current major mode is in `donkey-excluded-modes'."
  (donkey--memo-major-mode-in-p 'donkey--excluded-mode-cache
                                donkey-excluded-modes))

(defun donkey--insert-state-lighter ()
  "Return the mode-line text for Insert state in the current buffer.

\" DONKEY[E]\" in a `donkey-excluded-modes' buffer, where Normal state
cannot be reached, \" DONKEY[I]\" everywhere else.  Shared by the
`donkey-insert-mode' lighter and `donkey-indicator'."
  (if (donkey--excluded-mode-p) " DONKEY[E]" " DONKEY[I]"))

(defun donkey--handle-non-editing-buffer ()
  "Bounce straight back to Insert state in an excluded major mode.

On `donkey-normal-mode-hook', so Normal state entered directly -- `M-x
donkey-normal-mode', a key binding -- in an excluded buffer is undone
within the same toggle.  `donkey--ensure-default-state' covers a
buffer's first activation, and `donkey--check-post-command-non-editing'
covers anything that sets the variable without the toggle."
  (when (donkey--excluded-mode-p)
    (when (bound-and-true-p donkey-normal-mode)
      (donkey-enter-insert))))

(add-hook 'donkey-normal-mode-hook #'donkey--handle-non-editing-buffer)

(defun donkey--check-post-command-non-editing ()
  "Force Insert state if Normal state is somehow active in an excluded mode.

On the global `post-command-hook', after every command in every
buffer, reading the `donkey-normal-mode' variable directly: the
catch-all behind `donkey--handle-non-editing-buffer' and
`donkey--ensure-default-state' for anything that sets the variable
without going through the minor-mode toggle."
  (when (and (bound-and-true-p donkey-normal-mode)
             (donkey--excluded-mode-p))
    (donkey-enter-insert)))

;;; ---------------------------------------------------------------------------
;;; Org-Scratch Buffer Creation
;;; ---------------------------------------------------------------------------

(defun donkey-insert-org-scratch-message ()
  "Insert the *org-scratch* header naming the `save-some-buffers' key.

The key is rendered through `substitute-command-keys', so the header
shows the binding in force."
  (insert
   (substitute-command-keys
    (concat "# This buffer is for scribbling in org-mode.\n"
            "# Start your scribble here and save to file with `"
            "\\[save-some-buffers]"
            "' for persistence.\n\n")))
  (goto-char (point-max)))

(defun donkey-create-org-scratch ()
  "Create an _org-scratch_ buffer."
  (let ((buffer (get-buffer-create "*org-scratch*")))
    (switch-to-buffer buffer)
    (org-mode)
    (donkey-insert-org-scratch-message)))

(defun donkey-org-scratch ()
  "Create or switch to _org-scratch_."
  (interactive)
  (let ((org-scratch-buffer (get-buffer "*org-scratch*")))
    (if org-scratch-buffer
        (progn
          (switch-to-buffer org-scratch-buffer)
          (message "*org-scratch* buffer already exists, switching."))
      (donkey-create-org-scratch)
      (message "*org-scratch* buffer doesn't exist, creating."))))

;;; ---------------------------------------------------------------------------
;;; Line and Buffer Navigation Commands
;;; ---------------------------------------------------------------------------

(defcustom donkey-position-ring-max 10
  "Number of position markers retained in the ring.

Zero switches position tracking off: nothing is retained, so
`donkey-jump-back' has nowhere to go and says so.  Anything that is not
a number is read the same way rather than signaling."
  :type 'integer
  :group 'donkey)

(defun donkey--position-ring-limit ()
  "Return `donkey-position-ring-max' as a usable count, never signaling.

A non-number or a negative value reads as 0, tracking off; a float is
truncated."
  (if (numberp donkey-position-ring-max)
      (max 0 (truncate donkey-position-ring-max))
    0))

(defvar-local donkey--position-ring nil
  "List of markers recording previous cursor positions, most recent first.")

(defvar-local donkey--position-index 0
  "Current rotation offset into `donkey--position-ring'.

0 = most recent entry.  Reset to 0 whenever a new position is
recorded.")

(defvar-local donkey--last-tracked-state nil
  "Where point stood after the previous command, or nil for not yet.")

(defun donkey--track-position ()
  "Record the previous cursor position.

Runs on `post-command-hook', recording point whenever it has moved
since the last command.  Independent of the mark ring and region.

Must not signal, being on `post-command-hook'; the one value a user
can get wrong is made safe by `donkey--position-ring-limit'."
  (unless (minibufferp)
    (let ((now-pt (point)))
      (when (and donkey--last-tracked-state
                 (/= donkey--last-tracked-state now-pt))
        (let ((m (make-marker))
              (limit (donkey--position-ring-limit)))
          (set-marker m donkey--last-tracked-state)
          (push m donkey--position-ring)
          ;; Trimmed down to the limit, not by one, so a lowered limit
          ;; takes effect; `butlast' rather than `nbutlast', which
          ;; cannot empty a one-element list.
          (when (> (length donkey--position-ring) limit)
            (dolist (stale (nthcdr limit donkey--position-ring))
              (set-marker stale nil))
            (setq donkey--position-ring
                  (butlast donkey--position-ring
                           (- (length donkey--position-ring) limit)))))
        (setq donkey--position-index 0))
      (setq donkey--last-tracked-state now-pt))))

(defun donkey-jump-back (&optional count)
  "Rotate to the next stored position in the ring and jump there.

Press repeatedly to cycle through the last `donkey-position-ring-max'
recorded positions in this buffer.

COUNT jumps back that many recorded positions at once, reaching the same
place a run of COUNT presses reaches.  A COUNT below 1 is treated as 1.

Meant for taking back a mis-keyed jump: reaching for `g l' and
slipping to `g e' lands you at the end of the buffer, and one press
puts it right.  Positions are recorded automatically as you move, so
it is a recovery key rather than a bookmark.

Positions outside the accessible portion are skipped rather than jumped
to, and the count in the message is of the visible entries, so it matches
what pressing again will cycle through."
  (interactive "p")
  (let ((visible (seq-filter (lambda (m)
                               (let ((pos (marker-position m)))
                                 (and pos
                                      (<= (point-min) pos)
                                      (<= pos (point-max)))))
                             donkey--position-ring)))
    (cond
     ((null donkey--position-ring)
      (user-error "No positions recorded yet"))
     ((null visible)
      (user-error "No recorded position in the visible portion"))
     (t
      ;; The counter is used before it is advanced.
      (let* ((ring-len (length visible))
             (idx (if (>= donkey--position-index ring-len)
                      0
                    donkey--position-index)))
        ;; A COUNT is N presses in one, wrapping as N presses would.
        (setq idx (mod (+ idx (1- (max 1 (or count 1)))) ring-len))
        (goto-char (nth idx visible))
        (setq donkey--position-index (1+ idx))
        (setq donkey--last-tracked-state (point))
        (message "Position %d/%d" (1+ idx) ring-len))))))

(defun donkey-goto-line ()
  "Prompt for a line number and move point to the start of that line.

Out-of-range input is clamped by `forward-line' itself: past the end
of the buffer moves to the last line, zero or below to the first.
Fractional input is rounded to the nearest line.  Input that is not a
finite number, such as \"1e999\", is refused with a `user-error' naming
it, with point left where it was."
  (interactive)
  (let* ((input (read-number "Line: "))
         (target-line (condition-case nil
                          (round input)
                        (overflow-error
                         (user-error "Not a line number: %s" input)))))
    (goto-char (point-min))
    (forward-line (1- target-line))))

(defun donkey-switch-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

;;; ---------------------------------------------------------------------------
;;; Indentation Commands
;;; ---------------------------------------------------------------------------

(defun donkey-indent-region-or-line ()
  "Indent active region or current line."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (indent-region (line-beginning-position) (line-end-position))))

;;; ---------------------------------------------------------------------------
;;; Insert Entry Commands
;;; ---------------------------------------------------------------------------

(defun donkey-enter-insert ()
  "Switch to INSERT state."
  (donkey-insert-mode 1))

(defun donkey--deactivate-region-if-active ()
  "Deactivate the mark if there is an active, non-empty region."
  (when (use-region-p)
    (deactivate-mark)))

(defun donkey-insert-here ()
  "Insert at point, and enter INSERT state.

Any active selection is dropped, a rectangle included, and banked
lines are left standing; `donkey-change' is the insert-entry key that
acts on them instead.

Takes no COUNT."
  (interactive)
  (donkey--deactivate-region-if-active)
  (donkey-enter-insert))

(defun donkey-insert-after ()
  "Insert after the character at point, and enter INSERT state.

At the very end of the buffer point stays put.

Any active selection is dropped, a rectangle included, and banked
lines are left standing; `donkey-change' is the insert-entry key that
acts on them instead.

Takes no COUNT."
  (interactive)
  (donkey--deactivate-region-if-active)
  (condition-case _err
      (forward-char 1)
    (end-of-buffer nil))
  (donkey-enter-insert))

(defun donkey-insert-beginning-of-line ()
  "Move to the beginning of the line, and enter INSERT state.

Any active selection is dropped, a rectangle included, and banked
lines are left standing; `donkey-change' is the insert-entry key that
acts on them instead.

Takes no COUNT."
  (interactive)
  (donkey--deactivate-region-if-active)
  (beginning-of-line)
  (donkey-enter-insert))

(defun donkey-insert-end-of-line ()
  "Move to the end of the line, and enter INSERT state.

Any active selection is dropped, a rectangle included, and banked
lines are left standing; `donkey-change' is the insert-entry key that
acts on them instead.

Takes no COUNT."
  (interactive)
  (donkey--deactivate-region-if-active)
  (move-end-of-line 1)
  (donkey-enter-insert))

(defun donkey-open-below (&optional count)
  "Open COUNT new lines below the current one, and enter INSERT state.

Any active selection is dropped, a rectangle included, and banked
lines are left standing; `donkey-change' is the insert-entry key that
acts on them instead.

COUNT opens that many lines.  The first is the line a bare press opens,
indented by the mode with the cursor on it; the rest are empty lines
below it, so \\[universal-argument] 3 \\[donkey-open-below] is the
usual new line with two blank ones under it.  Only the cursor's line is
indented: the blanks are plain, and leave no whitespace behind when
nothing is typed on them.  A COUNT below 1 opens one line, as a bare
press does -- there is no meaning for zero lines on a key that exists
to open one.

`donkey-open-above' reads its count the same way, the blanks above
the cursor's line."
  (interactive "p")
  (donkey--deactivate-region-if-active)
  (move-end-of-line 1)
  (newline-and-indent)
  ;; The extra lines go beyond the cursor's line, after the
  ;; indentation, so only that line carries any.
  (let ((extra (1- (or count 1))))
    (when (> extra 0)
      (save-excursion (insert (make-string extra ?\n)))))
  (donkey-enter-insert))

(defun donkey-open-above (&optional count)
  "Open COUNT new lines above the current one, and enter INSERT state.

Any active selection is dropped, a rectangle included, and banked
lines are left standing; `donkey-change' is the insert-entry key that
acts on them instead.

COUNT opens that many lines: the line a bare press opens, directly
above the one the cursor came from and indented by the mode with the
cursor on it, and COUNT - 1 empty lines above that.  A COUNT below 1
opens one line, as a bare press does."
  (interactive "p")
  (donkey--deactivate-region-if-active)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  ;; Inserted at the line's start, so the line is pushed down and the
  ;; cursor stays beside the line it came from.
  (let ((extra (1- (or count 1))))
    (when (> extra 0)
      (beginning-of-line)
      (insert (make-string extra ?\n))
      (end-of-line)))
  (donkey-enter-insert))

(defun donkey-change (&optional count)
  "Delete the active region (or the character at point) and enter INSERT state.

Under `rectangle-mark-mode' the region is replaced via
`string-rectangle', which prompts for the replacement text and applies
it to every covered line, and DONKEY stays in NORMAL state.

A visual-line selection made with `V' is NOT widened to whole lines
here, unlike `donkey-copy', `donkey-delete' and `donkey-yank'.  The
newline ending the last line is kept, so `V c' empties the line and
leaves point on it ready to type, and `V J c' collapses the span to a
single empty line.  `V d' takes the newline; `V c' keeps it.

An EMPTY line under `V' is changed the same way: it stays, empty, with
INSERT state on it.

Banked lines are not honored either.  With lines banked via
`donkey-bank-selection' and no active region, this changes the character
at point and leaves the banks standing -- `y', `d' and `p' all act on
the bank instead.

What a SELECTION replaces goes on the `kill-ring', so
\\[donkey-yank] brings it back, as it does after `donkey-delete'.  A
rectangle goes to `killed-rectangle' instead, where
\\[donkey-yank-rectangle] pastes it from.

With NO selection nothing is saved: a character changed under the
cursor is a typo being fixed, not a cut.  A COUNT does not change that
-- \\`C-u 3 c' is still no selection -- so the text it removes is gone
except through `undo'.  `donkey-delete' draws the same line.

INSERT state is entered even when there is nothing to delete, such as at
the very end of the buffer.

COUNT changes that many characters when no selection is active.  A
negative COUNT changes that many characters before point, and a COUNT of
zero changes none while still entering INSERT state, the same reading
`donkey-delete' gives its own argument."
  (interactive "p")
  (if (donkey--selection-to-act-on-p)
      (if (bound-and-true-p rectangle-mark-mode)
          (progn
            ;; Saved to `killed-rectangle' before the prompt, as
            ;; `donkey-delete' saves it; aborting the prompt leaves it
            ;; there.
            (call-interactively #'copy-rectangle-as-kill)
            (call-interactively #'string-rectangle)
            ;; Explicit: the command may be reached from Insert state
            ;; through \\[execute-extended-command].
            (donkey-enter-normal))
        ;; `kill-region', not `delete-region': what a selection replaces
        ;; is recoverable.  Not over an empty span, which would push ""
        ;; onto the ring.
        (if (= (mark) (point))
            (deactivate-mark)
          (kill-region (mark) (point)))
        (donkey-enter-insert))
    ;; Not killed: no selection was made, so there is nothing to put
    ;; back.
    (delete-region (point)
                   (max (point-min)
                        (min (point-max) (+ (point) (or count 1)))))
    (donkey-enter-insert)))

;;; ---------------------------------------------------------------------------
;;; Enter DWIM
;;; ---------------------------------------------------------------------------

(defvar donkey--enter-rules nil
  "List of (ELEMENT-TYPE PROPERTY COMMAND1 COMMAND2 ...) for ENTER DWIM dispatch.")

(defvar-local donkey--saved-ret-binding nil
  "Saved RET binding from the buffer's local map when entering Normal state.

Captured, by a `donkey-normal-mode-hook' function in this section,
for a buffer whose major mode is not in `donkey-editing-modes', and
read back by `donkey--non-editing-enter-handler', so RET still does
what that mode expects.")

(defcustom donkey-editing-modes
  '(prog-mode text-mode org-mode fundamental-mode conf-mode markdown-mode gfm-mode)
  "Major modes where RET/<enter> does nothing in DONKEY Normal state.

Derived modes (e.g. `python-mode' from `prog-mode') are caught by
`derived-mode-p' in `donkey--editing-mode-p', so listing a handful of
broad parent modes here covers the vast majority of buffers actually
being edited as code or plain text -- inserting a literal newline via
Enter in Normal state is rarely what's wanted there.  See
`donkey-enter-dwim' for what happens instead in modes NOT in this
list: it falls through to Org/markdown-aware dispatch, or to
whatever RET was originally bound to before Normal state's keymap
took over (e.g. `dired-find-file' in `dired-mode').

Add a major mode here if Enter should also be a no-op for it; remove
one if you'd rather it fall through to its own original RET binding."
  :type '(repeat symbol)
  :group 'donkey)

(defvar-local donkey--editing-mode-cache nil
  "Memo for `donkey--editing-mode-p'; see `donkey--memo-major-mode-in-p'.")

(defun donkey--editing-mode-p ()
  "Return non-nil if current major mode is in `donkey-editing-modes'."
  (donkey--memo-major-mode-in-p 'donkey--editing-mode-cache
                                donkey-editing-modes))

(defun donkey--register-enter-rule (rule)
  "Register RULE for ENTER DWIM dispatch.

Prepended to the front of `donkey--enter-rules', so the most recently
added rule is tried first — letting a rule added later (e.g. from
`config.el' via `with-eval-after-load') take priority over an earlier,
same element-type/property default rule."
  (add-to-list 'donkey--enter-rules rule))

(defmacro donkey-add-enter-rule (element-type property &rest commands)
  "Add an ENTER rule with element type, property, and command fallback.

ELEMENT-TYPE specifies the org element type
\(e.g. `:todo-type', `:checkbox', or nil).
PROPERTY is the attribute to check on the element.
COMMANDS is a list of functions tried sequentially until one succeeds.

See `donkey-enter-dwim' for how these rules are evaluated."
  (declare (indent 2))
  `(donkey--register-enter-rule '(,element-type ,property ,@commands)))

(defcustom donkey-default-enter-rules-enabled t
  "If non-nil, install default ENTER rules on load.

Set to nil in `config.el' if you want to define rules manually."
  :type 'boolean
  :group 'donkey)

(defun donkey--callable-command-p (cmd)
  "Return non-nil if CMD is a bound, callable interactive command."
  (and cmd (fboundp cmd) (commandp cmd)))

(defun donkey--find-enter-handler ()
  "Find command for Enter key based on element at point.

Checks context first, then parent, then ancestors — always trying all rules
against more specific elements before broader ancestors.
Returns command symbol or nil if no handler matches.

Nil outside a buffer derived from `org-mode', without asking Org's
parser anything; Markdown has `donkey--markdown-enter-handler'.  This
is the one place the mode is tested for the Org rules."
  (let* ((in-org (derived-mode-p 'org-mode))
         (parent (and in-org
                      (fboundp 'org-element-at-point)
                      (org-element-at-point)))
         (ctx (and in-org
                   (fboundp 'org-element-context)
                   (org-element-context)))
         (ancestors (and parent
                         (fboundp 'org-element-lineage)
                         (org-element-lineage parent)))
         ;; At a line start `org-element-at-point' resolves to the
         ;; enclosing container; one character forward gives the element
         ;; there.  Tried before the ancestors.
         (fallback-parent (and in-org
                               (fboundp 'org-element-at-point)
                               (= (point) (line-beginning-position))
                               (< (point) (point-max))
                               (org-element-at-point (1+ (point)))))
         (result nil))
    ;; Context FIRST (inline elements like links within tables/headlines)
    (dolist (rule donkey--enter-rules)
      (when (null result)
        (let ((rule-type (nth 0 rule))
              (rule-cmds (nthcdr 2 rule)))
          (when (and ctx
                     (eq (car ctx) rule-type)
                     (null (nth 1 rule)))
            (setq result (seq-find #'donkey--callable-command-p rule-cmds))))))
    ;; Parent, then its line-start fallback, then ancestors — ALL rules
    ;; checked per element level, most specific first
    (dolist (elem (append (list parent fallback-parent) ancestors))
      (when (null result)
        (dolist (rule donkey--enter-rules)
          (when (null result)
            (let ((rule-type (nth 0 rule))
                  (rule-prop (nth 1 rule))
                  (rule-cmds (nthcdr 2 rule)))
              (when (and elem
                         (eq (car elem) rule-type)
                         (or (null rule-prop)
                             (and (fboundp 'org-element-property)
                                  (org-element-property rule-prop elem))))
                (setq result (seq-find #'donkey--callable-command-p rule-cmds))))))))
    result))

(defun donkey--execute-handler (cmd)
  "Execute CMD if it exists and is callable."
  (when (donkey--callable-command-p cmd)
    (call-interactively cmd)))

(defun donkey--org-agenda-enter-handler ()
  "Handle Enter in `org-agenda' mode.  Return t if handled, otherwise nil."
  (when (and (boundp 'org-agenda-mode-map)
             (derived-mode-p 'org-agenda-mode))
    (let ((ret-cmd (lookup-key org-agenda-mode-map (kbd "RET"))))
      (when (and ret-cmd
                 (not (eq ret-cmd 'undefined))
                 (commandp ret-cmd))
        (call-interactively ret-cmd)
        t))))

(defun donkey--org-mode-enter-handler ()
  "Handle Enter in `org-mode'.  Return t if handled.

Derivation counts: a mode built on `org-mode', such as
`org-journal-mode', dispatches the same.  The mode test lives in
`donkey--find-enter-handler'."
  (let ((handler (donkey--find-enter-handler)))
    (when handler
      (donkey--execute-handler handler)
      t)))

(defun donkey--markdown-enter-handler ()
  "Follow the link at point in a Markdown buffer.  Return t if handled.

Markdown has no element tree for `donkey--enter-rules' to match
against, so the rules are not consulted: `markdown-mode' itself says
whether point is on a link -- `markdown-link-p' for inline, reference,
bare and angle-bracket URLs, `markdown-wiki-link-p' for wiki links --
and `markdown-follow-thing-at-point', the command behind its own
follow-link key, follows it.
Anywhere else on the page RET stays inert, as in every editing mode.
Derivation counts, so `gfm-mode' is covered.

The functions are looked up rather than required: a buffer in a mode
derived from `markdown-mode' has the library loaded, and the guards
are for the tests, which stand in for it."
  (when (and (derived-mode-p 'markdown-mode)
             (fboundp 'markdown-link-p)
             (fboundp 'markdown-wiki-link-p)
             (fboundp 'markdown-follow-thing-at-point)
             (or (markdown-link-p) (markdown-wiki-link-p)))
    (call-interactively #'markdown-follow-thing-at-point)
    t))

(defun donkey--non-editing-enter-handler ()
  "Handle Enter in non-editing modes.  Return t if handled."
  (unless (donkey--editing-mode-p)
    (when (and donkey--saved-ret-binding
               (not (eq donkey--saved-ret-binding 'undefined))
               (not (keymapp donkey--saved-ret-binding))
               (commandp donkey--saved-ret-binding))
      (call-interactively donkey--saved-ret-binding)
      t)))

(defun donkey-org-todo ()
  "Toggle headline TODO state between TODO and DONE.

Uses `org-element-at-point' to detect the :todo-type property and
dispatches `org-todo' accordingly.  No keyword string parsing needed.

A headline with no keyword is left alone."
  (interactive)
  (when (and (fboundp 'org-element-at-point)
             (fboundp 'org-element-property)
             (fboundp 'org-todo))
    (let* ((elem (org-element-at-point))
           (todo-type (and (consp elem)
                           (eq (car elem) 'headline)
                           (org-element-property :todo-type elem))))
      (cond
       ((eq todo-type 'todo)
        (org-todo 'done))
       ((eq todo-type 'done)
        (org-todo 'todo))))))

(when donkey-default-enter-rules-enabled
  (donkey-add-enter-rule item :checkbox org-toggle-checkbox)
  (donkey-add-enter-rule headline :todo-type donkey-org-todo)
  ;; Org's command alone; Markdown links are
  ;; `donkey--markdown-enter-handler's.
  (donkey-add-enter-rule link nil org-open-at-point))

(defun donkey-enter-dwim ()
  "Smart Return handler for DONKEY Normal state.

Bound to both RET and <enter> in `donkey-normal-mode-map'.  Tries, in
order, stopping at the first one that reports it handled the key:

1. `donkey--org-agenda-enter-handler' -- delegates to whatever
   `org-agenda-mode-map' itself binds RET to (open item, visit entry).
2. `donkey--org-mode-enter-handler' -- in `org-mode' buffers, derived
   modes such as `org-journal-mode' included, dispatches via
   `donkey--find-enter-handler' against the element at point (see
   `donkey-add-enter-rule' to register more element-type/command
   rules, e.g. from `config.el').
3. `donkey--markdown-enter-handler' -- in `markdown-mode' and
   `gfm-mode' buffers, follows the link at point through
   `markdown-follow-thing-at-point', Markdown's own key for it.
4. `donkey--non-editing-enter-handler' -- outside `donkey-editing-modes'
   (`dired-mode', `magit-status-mode', etc.), falls through to
   whatever RET was ORIGINALLY bound to before Normal state's keymap
   took over, via `donkey--saved-ret-binding'.

If none of these handle it -- ordinary `prog-mode'/`text-mode' buffers
being edited as code or plain text -- RET does nothing at all, on
purpose."
  (interactive)
  (cond
   ((donkey--org-agenda-enter-handler))
   ((donkey--org-mode-enter-handler))
   ((donkey--markdown-enter-handler))
   ((donkey--non-editing-enter-handler))))

(add-hook 'donkey-normal-mode-hook
          (lambda ()
            (unless (donkey--editing-mode-p)
              ;; `current-local-map' is nil for a mode without one, and
              ;; `lookup-key' signals on nil.
              (setq donkey--saved-ret-binding
                    (let ((map (current-local-map)))
                      (and map (lookup-key map (kbd "RET")))))))
          t)

;;; ---------------------------------------------------------------------------
;;; Comment DWIM
;;; ---------------------------------------------------------------------------

(defun donkey--in-org-src-block-p ()
  "Return non-nil if point is inside an Org source block."
  (and (eq major-mode 'org-mode)
       (fboundp 'org-element-at-point)
       (let ((elem (org-element-at-point)))
         (and (consp elem) (eq (car elem) 'src-block)))))

(defun donkey-comment-dwim ()
  "Comment/uncomment whole lines in region, or current line if no region.

When inside an Org source block, delegates to the block's native
major mode via `org-edit-special' for language-aware commenting,
then returns to the Org buffer."
  (interactive)
  (cond
   ((donkey--in-org-src-block-p)
    (let ((has-region (use-region-p))
          (cur-line (line-number-at-pos))
          (reg-beg-line (when (use-region-p)
                          (line-number-at-pos (region-beginning))))
          (reg-end-line (when (use-region-p)
                          (line-number-at-pos (region-end)))))
      (condition-case err
          (progn
            (org-edit-special)
            ;; `org-edit-special' stays outside the `unwind-protect':
            ;; with no edit buffer there is nothing to exit from.
            (unwind-protect
                (if has-region
                    (let* ((cur-line-in-edit (line-number-at-pos))
                           (diff (- cur-line-in-edit cur-line))
                           (last-line (line-number-at-pos (point-max)))
                           ;; Clamp to the edit buffer's own line range; the
                           ;; region may reach past either end of the block.
                           (edit-beg-line (max 1 (+ reg-beg-line diff)))
                           (edit-end-line (min last-line (+ reg-end-line diff))))
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- edit-beg-line))
                        (let ((beg (line-beginning-position)))
                          (forward-line (- edit-end-line edit-beg-line))
                          (comment-or-uncomment-region
                           beg (line-beginning-position 2)))))
                  (comment-or-uncomment-region
                   (line-beginning-position)
                   (line-beginning-position 2)))
              (org-edit-src-exit))
            (when has-region (deactivate-mark)))
        (error
         (message "donkey-comment-dwim (org-src): %s"
                  (error-message-string err))))))
   (t
    (if (use-region-p)
        (let ((beg (save-excursion
                     (goto-char (region-beginning))
                     (line-beginning-position)))
              (end (save-excursion
                     (goto-char (region-end))
                     (if (bolp) (point) (line-beginning-position 2)))))
          (comment-or-uncomment-region beg end))
      (comment-or-uncomment-region
       (line-beginning-position)
       (line-beginning-position 2)))
    (deactivate-mark))))

;;; ---------------------------------------------------------------------------
;;; Clipboard Tools Detection
;;; ---------------------------------------------------------------------------

(defvar donkey--clipboard-warning-shown nil
  "Non-nil once the clipboard warning has been shown this session.")

(defvar donkey--clipboard-executables 'unknown
  "Memoized answer to \"is a clipboard tool on PATH?\": t, nil, or `unknown'.

Filled by `donkey--detect-clipboard-tools' on its first call and kept
for the session.")

(defun donkey--detect-clipboard-tools ()
  "Detect available system clipboard tools.

Checks for wl-clipboard (Wayland), xclip/xsel (X11), and
pbcopy/pbpaste (macOS).  On Windows, native clipboard integration
is assumed, and a graphical frame counts as having a clipboard on
every platform.  Returns non-nil if any tool or native support is
found.  The PATH walk is done once per session; the frame check is
made on every call, so the answer is right for the selected frame."
  (cond
   ;; macOS: always has pbcopy/pbpaste
   ((eq system-type 'darwin) t)
   ;; Windows: native clipboard integration, no external tools needed
   ((eq system-type 'windows-nt) t)
   ;; Linux/BSD: check for Wayland and X11 clipboard tools
   ((progn
      (when (eq donkey--clipboard-executables 'unknown)
        (setq donkey--clipboard-executables
              (and (or (executable-find "wl-copy")
                       (executable-find "xclip")
                       (executable-find "xsel"))
                   t)))
      donkey--clipboard-executables)
    t)
   ;; GUI Emacs has its own clipboard bridge on all platforms
   ((display-graphic-p) t)
   (t nil)))

(unless (or (donkey--detect-clipboard-tools) noninteractive)
  (message "Warning (donkey): No system clipboard tools detected.
    Yank will fall back to the kill-ring. Install wl-clipboard
    (Wayland), xclip or xsel (X11) for system clipboard integration."))

;;; ---------------------------------------------------------------------------
;;; Clipboard Platform Diagnostics and Debugging
;;; ---------------------------------------------------------------------------

(defun donkey--platform-info ()
  "Return a plist describing the current execution environment.

Includes system type, display backend, terminal type, and clipboard
availability.  Useful for debugging platform-specific issues."
  (list :system-type system-type
        :display-type (if (display-graphic-p) 'gui 'terminal)
        :tty-type (tty-type)
        :term-env (getenv "TERM")
        :clipboard-tools-available (donkey--detect-clipboard-tools)
        ;; Whether native compilation is available, not whether the
        ;; predicate exists.
        :native-comp (native-comp-available-p)
        :emacs-version emacs-version))

(defun donkey-debug-platform ()
  "Display detailed platform information for troubleshooting.

Shows system type, display backend, terminal configuration,
and clipboard tool availability.  Useful when reporting bugs
or debugging platform-specific issues.

Output goes to a temporary buffer named '*DONKEY Platform Debug*'."
  (interactive)
  (let ((info (donkey--platform-info)))
    (with-output-to-temp-buffer "*DONKEY Platform Debug*"
      (princ "=== DONKEY Modal Platform Diagnostics ===\n\n")

      (princ "--- System Information ---\n")
      (princ (format "Emacs Version: %s\n" (plist-get info :emacs-version)))
      (princ (format "System Type:   %s\n" (plist-get info :system-type)))
      (princ (format "Native Comp:   %s\n"
                     (if (plist-get info :native-comp) "yes" "no")))
      (princ "\n")

      (princ "--- Display Backend ---\n")
      (let ((dtype (plist-get info :display-type)))
        (princ (format "Display Mode:  %s\n" dtype))
        (when (eq dtype 'gui)
          (princ (format "Window System: %s\n" (window-system)))))
      (princ (format "TTY Type:      %s\n" (plist-get info :tty-type)))
      (princ (format "TERM Env:      %s\n" (or (plist-get info :term-env)
                                               "(not set)")))
      (princ "\n")

      (princ "--- Clipboard Status ---\n")
      (princ (format "Tools Available: %s\n"
                     (if (plist-get info :clipboard-tools-available)
                         "yes" "no")))
      (unless (plist-get info :clipboard-tools-available)
        (princ "\nRecommended Actions:\n")
        (cond
         ((eq system-type 'darwin)
          (princ "  macOS: pbcopy/pbpaste should be available by default.\n")
          (princ "  If missing, check your PATH or reinstall Xcode CLI tools.\n"))
         ((eq system-type 'windows-nt)
          (princ "  Windows: Native clipboard support is built-in.\n")
          (princ "  Verify you're not running in pure terminal mode without\n")
          (princ "  Windows Terminal or ConEmu with VT support.\n"))
         (t
          (princ "  Linux/Other: Install one of the following:\n")
          (princ "    - wl-clipboard (Wayland): sudo apt install wl-clipboard\n")
          (princ "    - xclip (X11):            sudo apt install xclip\n")
          (princ "    - xsel (X11):             sudo apt install xsel\n")))
        (princ "\n"))

      (princ "--- Platform-Specific Checks ---\n")
      (cond
       ((eq system-type 'darwin)
        (princ "macOS Detected:\n")
        (princ "  • DECSCUSR cursor sequences may not work in Terminal.app\n")
        (princ "  • iTerm2 and Alacritty have better terminal support\n")
        (princ "  • GUI mode bypasses terminal limitations entirely\n"))
       ((eq system-type 'windows-nt)
        (princ "Windows Detected:\n")
        (princ "  • Ensure Windows 10+ for VT sequence support in -nw mode\n")
        (princ "  • Use Windows Terminal or ConEmu for best compatibility\n")
        (princ "  • PowerShell/CMD without VT may break cursor shapes\n"))
       ((eq system-type 'gnu/linux)
        (princ "Linux Detected:\n")
        (princ "  • Check DISPLAY/WAYLAND_DISPLAY environment variables\n")
        (princ "  • Verify your display server (X11 vs Wayland)\n")
        (princ "  • Terminal emulator capability varies significantly\n")))

      (princ "\n=== End of Diagnostics ===\n")
      (princ "\nPress 'q' to close this buffer.\n"))

    (with-current-buffer "*DONKEY Platform Debug*"
      (special-mode))))

;;; ---------------------------------------------------------------------------
;;; Yank, Copy, and Delete Commands
;;; ---------------------------------------------------------------------------

(defun donkey--clipboard-yank ()
  "Yank from the system clipboard with `kill-ring' fallback.

If `clipboard-yank' signals an error (empty or inaccessible clipboard),
falls back to `yank' from the kill ring and emits an informative message
with platform context.  Shows platform-appropriate installation tips
only once per session.

`clipboard-yank' is called without a `fboundp' guard, and the docstring
used to describe a fallback for when it \"is available; otherwise\" --
there is no otherwise.  The function is preloaded from menu-bar.el in
every Emacs this package runs on, so the guard could never be false and
the fallback it selected could never run.  The same discovery retired
`kill-active-region' from `donkey--delete-active-region-safe': a branch
that cannot run is documentation that cannot be true."
  (condition-case err
      (clipboard-yank)
    (error
     (yank)
     (message "Clipboard unavailable on %s; yanked from kill ring (%s)."
              (cond
               ((eq system-type 'darwin) "macOS")
               ((eq system-type 'windows-nt) "Windows")
               (t "Linux/BSD"))
              (error-message-string err))))
  ;; Show tip only once, and only for platforms that actually need
  ;; external tools.  The flag is set when the tip FIRES, not on the
  ;; first paste whatever the answer: `display-graphic-p' is
  ;; frame-dependent, and a daemon session whose first paste happened
  ;; in a GUI frame -- where the tip is never eligible -- must still
  ;; show it on a later `emacsclient -t' paste with the tools missing.
  ;; The per-paste cost that once justified first-paste latching is
  ;; gone a different way: the PATH walk inside
  ;; `donkey--detect-clipboard-tools' is memoized in
  ;; `donkey--clipboard-executables'.
  (when (and (not donkey--clipboard-warning-shown)
             (not (display-graphic-p))
             (not (eq system-type 'darwin))
             (not (eq system-type 'windows-nt))
             (not (donkey--detect-clipboard-tools)))
    (setq donkey--clipboard-warning-shown t)
    (message "Tip: Install wl-clipboard (Wayland) or xclip/xsel (X11) for system clipboard.")))

(defun donkey--delete-active-region-safe ()
  "Delete the active region, if there is one, to make room for a paste.

DELETED rather than killed, deliberately.  The function
`delete-active-region' takes
a KILLP argument that would push the replaced text onto the kill ring,
and every caller here is about to paste: killing first would make the
`yank' that follows pull back the text just removed instead of what the
user asked to paste.  The replaced text stays recoverable through
\\[undo], which is where a paste-over normally leaves it.

Previously this called `kill-active-region' as \"available (Emacs 29+)\",
falling back to the function `delete-active-region'.  There is no such
function --
not in Emacs 29, 30, 31 or 32, and not anywhere in the Emacs Lisp tree --
so the fallback was the only branch that ever ran.  Removed rather than
fixed, since killing is the wrong thing here for the reason above."
  (when (use-region-p)
    (delete-active-region)))

(defun donkey--nothing-to-paste-p ()
  "Return non-nil when there is nothing for a paste to insert.

`current-kill' is the same source `yank' reads, so this also picks up
the system clipboard through `interprogram-paste-function' rather than
looking at `kill-ring' alone -- a clipboard with content in it is
something to paste even when the kill ring is empty.  DO-NOT-MOVE keeps
the probe from rotating `kill-ring-yank-pointer' underneath the paste
that follows.

Checked BEFORE anything is removed.  `donkey-yank' used to delete the
selection and only then discover it had nothing to insert, which left
the selected text gone, nothing pasted, and a bare \"Kill ring is empty\"
on screen -- and gone for real, since the region is deleted rather than
killed.  Confirmed live: two selected lines vanished with the kill ring
still empty afterwards."
  (condition-case nil
      (progn (current-kill 0 t) nil)
    (error t)))

(defun donkey--rectangle-top-left (start end)
  "Return the buffer position of the top-left corner of the rectangle.

START and END are the rectangle's corners.  `extract-rectangle-bounds'
returns one (START . END) cons per row of the rectangle, top row
first; that first row's START column position IS the rectangle's
top-left corner, computed the exact same way `rect.el' itself computes
it for every other rectangle operation."
  (caar (extract-rectangle-bounds start end)))

(defun donkey--replace-rectangle-selection-with-killed-rectangle ()
  "Replace the active `rectangle-mark-mode' selection with `killed-rectangle'.

Refuses via `user-error', without touching the buffer at all, when the
selection's row count doesn't match `killed-rectangle's row count --
silently replacing a differently-sized selection would either lose
rows of the pasted content or leave rows of the selection only
partially overwritten, either way not what \"replace this rectangle
with that one\" should ever silently do.

Uses `delete-rectangle', not `killed-rectangle', to clear the
destination: `killed-rectangle' would ALSO save what it deletes into the
very same `killed-rectangle' slot we're about to read from, clobbering
the source rectangle before it's ever pasted back.  The top-left
corner is captured before deleting -- deleting the rectangle only
ever removes text at or after that position on its own row, never
before it, so the captured position stays valid afterward without
needing any adjustment."
  (let* ((start (region-beginning))
         (end (region-end))
         (source killed-rectangle)
         (dest-row-count (length (extract-rectangle start end))))
    (unless (= dest-row-count (length source))
      (user-error
       "Rectangle row mismatch: selection has %d row%s, copied rectangle has %d -- paste refused"
       dest-row-count (if (= dest-row-count 1) "" "s") (length source)))
    (let ((top-left (donkey--rectangle-top-left start end)))
      (delete-rectangle start end)
      (goto-char top-left)
      (insert-rectangle source))))

(defun donkey--yank-rectangle-times (n)
  "Paste `killed-rectangle' with each of its rows repeated N times.

Sideways, not stacked.  A rectangle is a block of columns, so repeating
it means a wider block -- which is what a count on a blockwise paste
does in vi, and what `donkey--paste-times' cannot express: calling
`yank-rectangle' N times pastes the second block wherever the first one
left point, which is partway down and across the first, so two copies of
a three-row block came out as a staircase rather than as anything a user
asked for.

N below 1 pastes nothing, matching `donkey--paste-times'."
  (when (> n 0)
    (let ((killed-rectangle
           (mapcar (lambda (row) (mapconcat #'identity (make-list n row) ""))
                   killed-rectangle)))
      (yank-rectangle))))

(defun donkey--paste-times (n inserter)
  "Call INSERTER N times, or not at all when N is below 1.

The whole of what a count means for a paste.  INSERTER is called
repeatedly rather than its text being fetched once and inserted N times,
so the clipboard fallback and the rectangle path each keep their own
behavior instead of being re-implemented here.

A count below 1 inserts nothing, the way `donkey-copy' copies nothing and
`donkey-delete' deletes nothing at zero.  Negative gets the same answer
rather than a separate one: a paste has no backward direction for a
negative count to mean, so there is nothing for it to do but nothing."
  (dotimes (_ (max 0 n))
    (funcall inserter)))

(defun donkey--paste-restoring-line-ending (n took-newline)
  "Paste N times at point, giving back a line ending the delete took.

The tail end of pasting over a line selection.  Both of DONKEY's line
selections -- a \"V\" session and banked lines -- are removed whole,
final newline included, before the paste lands.  TOOK-NEWLINE says
whether the deleted span really did end in one; the buffer's last line
has none to take.  When it did, and the pasted text does not end in a
newline of its own, the taken one is inserted back -- under
`save-excursion', because it is restored structure rather than pasted
content, so point stays where the paste left it.

The rule this enforces: replacing a line selection preserves the
buffer's line structure.  A kill taken with \"V y\" carries its final
newline and slots in as the complete line it is; a fragment killed
mid-line becomes the line's new content instead of splicing onto the
line below.  Each selection used to get one of those wrong, in opposite
directions: \"V p\" left the target's newline standing, so a whole-line
kill brought a second one and opened an empty line under every replaced
line, while the bank always took the newline and never gave it back, so
pasting a fragment over a banked line glued it to the line after.

Whether anything was pasted is measured by point, not by N: N above
zero still inserts nothing when the newest kill is empty, and restoring
a newline behind a paste of nothing would conjure a blank line for a
press that visibly did nothing -- at the top of the buffer, where no
earlier line ending covers for it, dropping the point check does
exactly that."
  (let ((before (point)))
    (donkey--paste-times n #'donkey--clipboard-yank)
    (when (and took-newline
               (> (point) before)
               (not (eq (char-before) ?\n)))
      (save-excursion (insert "\n")))))

(defun donkey--replace-visual-lines-with-paste (n)
  "Replace the visual-line selection's whole lines with N pastes.

The \"V\" counterpart of `donkey--replace-banked-selection-with-paste':
the session's lines are deleted whole -- widened exactly as `y' and `d'
widen them, see `donkey--visual-line-region-bounds' -- and the paste
lands where they began.  Deleted rather than killed, for the reason
`donkey--delete-active-region-safe' gives: the yank that follows must
pull what is being pasted, not what was just removed.

The final newline traveling with the lines is the point of this
function.  It used to stay behind, because the paste deleted the raw
region the highlight shows -- one character short of the lines it
presents as, see `donkey--visual-line-region-bounds' -- so pasting the
complete line \"V y\" kills put its newline next to the survivor and
opened an empty line under every replaced line.  A kill that brings no
newline gets the deleted one restored after it;
`donkey--paste-restoring-line-ending' states the whole rule.

An N below 1 pastes nothing, and the lines are still removed: asking to
replace them with nothing is a delete, the same reading the banked
counterpart gives its own count of zero."
  (let* ((span (donkey--visual-line-region-bounds))
         (took-newline (eq (char-before (cdr span)) ?\n)))
    (delete-region (car span) (cdr span))
    (deactivate-mark)
    (goto-char (car span))
    (donkey--paste-restoring-line-ending n took-newline)))

;; Why pasting takes two keys rather than one:
;;
;; "p" used to decide for you, by tracking which of the two stores had been
;; written more recently -- but Emacs gives a rectangle no way to say so.
;; The kill ring is untyped text and `killed-rectangle' is a separate
;; variable that is only ever written, never cleared, so the answer had to
;; be carried in a flag alongside them, maintained by advice on `kill-new'.
;;
;; That flag went stale in ways no reader could predict.  `current-kill'
;; calls `kill-new' to import the system clipboard, so a PASTE reached the
;; advice and retired the pending rectangle -- in graphical sessions only.
;; The same keys gave different buffers on a GUI and in a terminal, and the
;; tutor could not state which.  Two keys need no such bookkeeping.
(defun donkey-yank (&optional count)
  "Paste clipboard content, replacing the active region if present.

Linear text only.  A rectangle is a block of columns and lives in its
own store, `killed-rectangle'; \\[donkey-yank-rectangle] is the key that
pastes it.

Falls back to the kill ring when the system clipboard is inaccessible,
so behavior is the same across GUI and terminal Emacs on Linux
\(X11/Wayland), macOS, and Windows.

Banked lines are a selection, and a paste replaces a selection: with
lines banked, they are replaced by what is pasted rather than the paste
landing at point.  Checked before the lines are removed, so a paste with
nothing to paste does not eat them.

A visual-line selection made with `V' is replaced as whole lines,
widened exactly as `y' and `d' widen it, final newline included.  A
kill taken with \"V y\" carries its own newline and slots in as the
complete line it is, instead of opening an empty line under every line
it replaces; a kill without one gets the taken newline restored behind
it and becomes the line's new content.  See
`donkey--replace-visual-lines-with-paste'.

With `rectangle-mark-mode' active, falls through to `undefined' --
there is no rectangular shape to give linear clipboard text, and
pasting it anyway would delete every row of the selection and replace
only one.  \\[donkey-yank-rectangle] is what pastes over a rectangle
selection.

COUNT inserts that many copies, so \\[universal-argument] 3 p pastes
three.  That is what a count on a paste means in vi, and it is the
reading this keymap wants: `C-y' is untouched in INSERT state, so
anyone reaching for Emacs\\=' own meaning -- a prefix argument
selecting WHICH `kill-ring' entry to pull -- still has it here, on the
key it belongs to.

A COUNT below 1 inserts nothing, matching what zero and negative
counts do for the other editing commands.  Any selection is still
replaced first: \\[universal-argument] 0 p over a region is a delete,
which is what asking to replace it with nothing means."
  (interactive "p")
  (let ((n (or count 1)))
    (cond
     ((bound-and-true-p rectangle-mark-mode)
      (call-interactively #'undefined))
     ((donkey--banked-selection-p)
      (if (donkey--nothing-to-paste-p)
          (message "Nothing to paste")
        (donkey--replace-banked-selection-with-paste n)))
     ((donkey--nothing-to-paste-p)
      (message "Nothing to paste"))
     ((donkey--visual-line-session-active-p)
      (donkey--replace-visual-lines-with-paste n))
     (t
      (donkey--delete-active-region-safe)
      (donkey--paste-times n #'donkey--clipboard-yank)))))

(defun donkey-yank-rectangle (&optional count)
  "Paste `killed-rectangle' as a block of columns.

The rectangle counterpart of \\[donkey-yank], which pastes linear
text.  Nothing is guessed: this key always means the rectangle store,
and \\[donkey-yank] always means the kill ring or system clipboard.

With `rectangle-mark-mode' active, replaces the selected rectangle via
`donkey--replace-rectangle-selection-with-killed-rectangle', which
refuses the paste when the row counts differ rather than risk a
silent, lossy, mismatched replace.  Otherwise the block lands at point,
deleting an ordinary active region first the way any paste over a
selection does.

Reports rather than signals when `killed-rectangle' is empty, the way
\\[donkey-yank] does when there is nothing on the kill ring.

Banked lines are not a selection here.  \\[donkey-yank] replaces them,
because linear text can stand in for whole lines; a block of columns
cannot, so this key leaves the bank alone and lands at point.

COUNT repeats each ROW sideways rather than stacking copies -- see
`donkey--yank-rectangle-times' for why a blockwise count has to mean a
wider block.  A COUNT below 1 inserts nothing, as it does for
\\[donkey-yank]."
  (interactive "p")
  (cond
   ;; A rectangle of nothing but empty rows counts as nothing to paste,
   ;; not as a paste of nothing.  It is one press away -- copying a
   ;; zero-width rectangle, which `m v' draws from an existing empty
   ;; region, stores ("" "") -- and pasting it used to change no text and
   ;; say nothing at all, leaving a reader who believed the store was
   ;; loaded with no clue why the key did nothing.  Two routes to the
   ;; same situation now reach the same message.
   ((or (null killed-rectangle)
        (seq-every-p #'string-empty-p killed-rectangle))
    (message "No rectangle to paste"))
   ((bound-and-true-p rectangle-mark-mode)
    (donkey--replace-rectangle-selection-with-killed-rectangle))
   (t
    (donkey--delete-active-region-safe)
    (donkey--yank-rectangle-times (or count 1)))))

(defun donkey--visual-line-region-bounds ()
  "Return the active region as (BEG . END), whole-lined for a `V' session.

`donkey-visual-line-toggle' and its `J'/`K' motions leave point at the
END of the last selected line, so the newline that ends it falls outside
the region.  That geometry is deliberate -- the highlight stops where the
text does, and the motion logic counts lines from where point sits -- but
it means a selection presented as whole lines is one character short of
being them.

The consequences landed on `y' and `d' rather than on the selection: `d'
removed the text and left an empty line behind, so `V d' had to be
followed by another `d' to clear up after it, and `y' produced a kill
with no final newline, which the next `p' spliced onto whatever line it
landed in.  `donkey-bank-selection' has always spanned whole lines, via
`donkey--whole-line-span'; routing a visual-line session through the same
helper makes donkey's two line selections finally agree.

`p' joined `y' and `d' later, for the same reason: pasting over a \"V\"
selection deleted the raw region and left the un-shown newline standing,
so the complete line `y' now kills arrived with one newline too many and
opened an empty line under every line it replaced.

Widened here rather than in the motions: moving point past the line
instead would make `forward-line' count from one line further along than
the user is on, so a single `J' grew the selection by two lines.  Tried
and rejected -- the geometry the motions rely on is load-bearing.

Only for a live visual-line session.  A character-wise region made with
`v' means the characters it covers, and is returned untouched."
  (if (donkey--visual-line-session-active-p)
      (donkey--whole-line-span (region-beginning) (region-end))
    (cons (region-beginning) (region-end))))

(defun donkey--selection-to-act-on-p ()
  "Return non-nil when an action key has a linear selection to take.

The test `donkey-copy', `donkey-delete' and `donkey-change' share for
the linear case, in place of a bare `use-region-p'.  That one is nil
for an EMPTY active region -- `use-empty-active-region' is nil by
default, and this package leaves it so -- which is right for a `v'
selection that has not moved yet, and wrong for a `V' session on an
empty line.  The session presents the line as selected, and the line
has a newline to take, but its region runs from the line's start to
the line's end, which on an empty line is no distance at all.  All
three keys therefore fell through to their no-selection branches and
acted on the character at point as though nothing had been selected.
Confirmed live, on \"a\", an empty line and \"b\" with `V' pressed on
the empty one: `d' removed the newline with `delete-region', off the
kill ring, so `p' then had nothing to put back; `c' removed the
newline and joined \"b\" up onto the empty line, where changing a line
leaves an empty line to type on; and `y' saved the newline only
because the character at point happened to be it, so that a count
reached past the line the session showed.  `donkey-yank' asked
`donkey--visual-line-session-active-p' itself all along, which is why
`V p' on the same line already worked.

A session whose widened span is EMPTY -- `V' on the buffer's last
line, when that line is empty and ends without a newline -- is still
no selection.  There is nothing to take, and `kill-region' over
nothing would push \"\" onto the ring; the count branches report
\"nothing to delete\" there, as they did."
  (or (use-region-p)
      (and (donkey--visual-line-session-active-p)
           (let ((bounds (donkey--visual-line-region-bounds)))
             (< (car bounds) (cdr bounds))))))

;; Why a rectangle copy never reaches the clipboard, and why `y' does not
;; use `kill-ring-save':
;;
;; `copy-rectangle-as-kill' and `kill-rectangle' both leave the kill ring
;; and the system clipboard alone, and a rectangle has no meaning outside a
;; buffer that could survive the trip through a flat clipboard.  It reads
;; like an oversight every time someone looks -- it has been raised,
;; investigated and set aside more than once, and a test pins it.  Note also
;; that a rectangle is pasted by its own key, "P", so pushing the text onto
;; the kill ring as well would put one copy in two stores that are emptied
;; independently.
;;
;; The rectangle branch used to go the other way: "y" over a rectangle you
;; had just drawn copied whole banked lines and left `killed-rectangle'
;; empty, so the rectangle was not there to paste afterwards either.
;;
;; `kill-ring-save' is not called directly because its interactive spec
;; reads `region-beginning'/`region-end', which use wherever the mark last
;; happened to be regardless of whether the region is ACTIVE.  A mark left
;; over from an earlier command (a stale `donkey-mark-inner' selection, say)
;; would then be copied instead of the single character at point.
;;
;; Copying nothing at `point-max', and at a count of zero, is for one
;; reason: copying the empty range would push an empty string and displace
;; the kill ring's newest entry, so one stray "y" past the last character
;; would make the next paste insert nothing with no error to explain it.
;; Confirmed live -- with "IMPORTANT" freshly copied, "y" at `point-max'
;; left the newest entry as "".
(defun donkey--kill-rectangle-guarded (kill-command empty-message)
  "Run KILL-COMMAND, keeping `killed-rectangle' safe from a no-width take.

Interactively invokes KILL-COMMAND -- `copy-rectangle-as-kill' or
`kill-rectangle' -- with its result taken aside, and commits it to
`killed-rectangle' only when it holds any text.  Returns non-nil exactly
when it does.

A rectangle with no width takes nothing but empty rows, and both
commands overwrite `killed-rectangle' before anyone can look at what
they took -- so a stored rectangle waiting to be pasted was replaced by
emptiness on a press that visibly did nothing.  When that happens the
store is left alone, EMPTY-MESSAGE is shown, and the variable
`deactivate-mark' is cleared so the rectangle stays on screen: both
commands set that variable themselves, and the command loop acts on it
after the calling command returns, so declining to call the function
`deactivate-mark' is not on its own enough.

One function rather than the same guard written into `donkey-copy' and
`donkey-delete' separately, which is how it started: an invariant
maintained by copy-paste is an invariant one future edit can break in
one place and not the other, and this package has repaired exactly that
drift twice before -- the trailing-punctuation trim and the
paragraph blank-line rule each had to be re-taught to a branch that
missed the lesson."
  (let ((taken (let ((killed-rectangle nil))
                 (call-interactively kill-command)
                 killed-rectangle)))
    (if (seq-every-p #'string-empty-p taken)
        (progn
          (setq deactivate-mark nil)
          (message "%s" empty-message)
          nil)
      (setq killed-rectangle taken)
      t)))

(defun donkey-copy (&optional count)
  "Copy the active region, or the character at point if no region is active.

With lines banked via `donkey-bank-selection', copies all of them
\\(plus any active region's lines) as a single kill instead.

A visual-line selection made with `V' is widened to whole lines before
being copied.  The highlight stops at the end of the last line, so the
newline ending it never looks selected -- but it IS copied, and the kill
pastes back as a complete line instead of splicing onto whatever line
\"p\" lands in.  See `donkey--visual-line-region-bounds'.  An empty line
is a line too: `V y' on one copies its newline, and a count typed with
it does not reach past the line -- see `donkey--selection-to-act-on-p'
for the empty region that used to fall through to the count.

With `rectangle-mark-mode' active, copies the rectangle instead of a
linear region -- and does so even when lines are banked, leaving every
bank standing.  The live selection wins and the bank is the fallback;
see `donkey--live-rectangle-p'.

A rectangle goes to `killed-rectangle' ONLY.  The kill ring and the
system clipboard are left alone, so a rectangle copied here cannot be
pasted into another application: \\[donkey-yank-rectangle] pastes it back
within Emacs and nothing else will.

At the very end of the buffer there is no character to copy, so nothing
is pushed onto the `kill-ring' at all.

COUNT copies that many characters when no region is active.  A negative
COUNT copies that many characters before point, matching how
`delete-char' and friends read a negative argument.  A COUNT of zero
copies nothing at all."
  (interactive "p")
  (let* ((n (or count 1))
         (target (max (point-min) (min (point-max) (+ (point) n)))))
   ;; Each branch answers whether it copied anything, because only a copy
   ;; that happened should clear the selection.  The `deactivate-mark'
   ;; used to sit outside this `cond' and fire on every branch, including
   ;; the two that report having nothing to take -- so a rectangle drawn
   ;; where there was nothing to copy vanished on the press that told you
   ;; so, and had to be drawn again.  `donkey-delete' cannot get this
   ;; wrong: it never deactivates explicitly, and a deletion that deletes
   ;; nothing leaves the selection standing by doing nothing at all.
   ;;
   ;; A successful copy still clears it.  Nothing else would -- the buffer
   ;; is untouched, so there is no edit for the command loop to notice --
   ;; and a selection surviving the key that consumed it is the surprise
   ;; running the other way.
   (let ((copied
          (cond
           ;; Before the bank: a rectangle on screen is the live
           ;; selection, and the live selection wins.  See
           ;; `donkey--live-rectangle-p'.
           ((donkey--live-rectangle-p)
            ;; Guarded, and the answer matters here: a no-width copy is
            ;; not a copy, so it must not clear the selection -- see
            ;; `donkey--kill-rectangle-guarded' for the store it is
            ;; also protecting.
            (donkey--kill-rectangle-guarded
             #'copy-rectangle-as-kill
             "Nothing to copy -- the rectangle has no width"))
           ((donkey--banked-selection-p)
            (donkey--copy-banked-selection) t)
           ((donkey--selection-to-act-on-p)
            (let ((bounds (donkey--visual-line-region-bounds)))
              (kill-ring-save (car bounds) (cdr bounds)))
            t)
           ((zerop n) nil)
           ((/= target (point))
            (kill-ring-save (point) target)
            t)
           ((< n 0)
            (message "Beginning of buffer -- nothing to copy")
            nil)
           (t
            (message "End of buffer -- nothing to copy")
            nil))))
     (when copied
       (deactivate-mark)))))

;; Two things this got wrong before:
;;
;; The bank used to outrank a drawn rectangle here, which was worse than the
;; same mistake in `donkey-copy': drawing a rectangle over two rows and
;; pressing "d" deleted three whole banked lines instead, taking text the
;; rectangle never covered.
;;
;; Counts used to clamp up to 1, so "delete zero characters" removed one and
;; "delete two backwards" removed one forwards.
(defun donkey-delete (&optional count)
  "Delete character or region.

With lines banked via `donkey-bank-selection', kills all of them (plus
any active region's lines) as a single kill instead.

A visual-line selection made with `V' is widened to whole lines before
being killed.  The highlight stops at the end of the last line, so the
newline ending it never looks selected -- but it IS deleted, so `V d'
removes those lines outright rather than emptying them and leaving the
blanks behind.  Taking one character more than was highlighted is
deliberate, not an off-by-one: see `donkey--visual-line-region-bounds'.
An empty line goes the same way, its newline on the kill ring for
\\[donkey-yank] to put back.  It used to go through `delete-region'
instead, off the ring, its region being empty and `use-region-p'
therefore nil -- see `donkey--selection-to-act-on-p'.

With `rectangle-mark-mode' active, kills the rectangle via
`kill-rectangle', which fills `killed-rectangle' -- the store
\\[donkey-yank-rectangle] pastes from.  Like `donkey-copy', that
reaches `killed-rectangle' only and never the system clipboard; see
there for why that is deliberate.

Banked lines do not override that: the rectangle is the live selection
and wins, and the banks survive untouched.  See
`donkey--live-rectangle-p'.

COUNT deletes that many characters when no region is active.
A count larger than the text remaining stops at the end rather than
signaling.  A negative COUNT deletes that many characters before point
and a COUNT of zero deletes none, matching `delete-char'.

Those characters are NOT put on the `kill-ring', and neither is a
counted run of them: only a selection is saved.  A character deleted
under the cursor is a typo being fixed rather than a cut, and filling the
ring with single characters would push out what was put there
deliberately -- which is why `delete-char' does not save either, while
`kill-region' does.  The consequence is worth stating plainly, since
nothing on screen shows it: after \\`C-u 3 d' a \\[donkey-yank] pastes
whatever was already on the ring, not the three characters just removed.
`undo' is what brings those back.  `donkey-change' draws the same line in
the same place."
  (interactive "p")
  (let* ((n (or count 1))
         (target (max (point-min) (min (point-max) (+ (point) n)))))
   (cond
    ;; Before the bank, for the reason `donkey-copy' gives: see
    ;; `donkey--live-rectangle-p'.
    ((donkey--live-rectangle-p)
     ;; The return value is unused: a kill that took text has already
     ;; changed the buffer, which is all this branch owes anyone.  The
     ;; guard is for the no-width press -- see
     ;; `donkey--kill-rectangle-guarded'.
     (donkey--kill-rectangle-guarded
      #'kill-rectangle
      "Nothing to delete -- the rectangle has no width"))
    ((donkey--banked-selection-p)
     (donkey--delete-banked-selection))
    ((donkey--selection-to-act-on-p)
     (let ((bounds (donkey--visual-line-region-bounds)))
       (kill-region (car bounds) (cdr bounds))))
    ((zerop n) nil)
    ((/= target (point))
     (delete-region (point) target))
   ((< n 0)
    (message "Beginning of buffer -- nothing to delete"))
   (t
    ;; At `point-max' there is no character to delete and `delete-char'
    ;; signals a bare `end-of-buffer', which pops the debugger for anyone
    ;; running with `debug-on-error' on.  `donkey-copy' and `donkey-change'
    ;; both already guard this exact position; this one was missed.
    ;; Reached by pressing "x" or "d" once too often at the end of a
    ;; buffer, which \\[end-of-buffer] lands on directly.
    (message "End of buffer -- nothing to delete")))))

(defun donkey-join-line (&optional count)
  "Pull the following line up onto this one, or join the selected lines.

The direction every modal editor uses: vi's `J' and Helix's `J' both
absorb the line below the one point is on, which is the direction you
want when you are sitting on a line deciding to take in what comes
next.  Emacs\\='s own `\\[delete-indentation]' goes the other way,
pulling the CURRENT line up onto the previous one, and is untouched --
every `M-' key falls through in Normal state, so both directions are
available.

This is a fix as well as a move.  Joining was on `C-j' and ran
`join-line' with no argument -- the Emacs direction -- while the README
had always described it as \"Join line with next\".  The documentation
promised the modal reading and the key delivered the Emacs one.

Off `C-j' because that key is not free in Emacs the way it looks: it is
globally `electric-newline-and-maybe-indent', and in `*scratch*' and
any `lisp-interaction-mode' buffer it is `eval-print-last-sexp'.  A
minor-mode map outranks the major mode, so binding it here cost the
scratch buffer its evaluate-and-print key -- the only stock Emacs
command Normal state took away that a user would actually miss.

COUNT joins that many following lines, so `C-u 3 g j' collapses three
lines into this one.  A COUNT below 1 joins nothing:
`join-line' reads any nil-or-non-positive argument as \"join to the
PREVIOUS line\" instead, which would silently reverse the direction of
a command the user asked to do less of.

On the last line there is nothing to pull up and nothing happens.  Left
to `join-line' this quietly ate the buffer's final newline instead: the
\"line\" below the last one is the empty position after it, and joining
that removes the newline separating them.  Nothing visible changes --
the screen looks identical and point sits at `point-max' either way --
so the first sign is a diff reporting \"\\\\ No newline at end of file\"
later on.  A count that overshoots hit the same thing on its last
iteration.

vi's `J' stops at the last line for the same reason, and every other
whole-line command here already leaves the final newline alone: `V d',
`V y', a banked copy and `D' were all checked.

With a selection, the lines it touches become ONE line and the
selection is spent -- vi's `J' reading of a visual selection: `V J J
g j' on four lines makes the three selected into one and leaves the
fourth.  Which lines a selection touches is `donkey--region-line-count'
-- one that ends at the start of a line leaves that line out.  A
selection inside a single line joins that line with the next, vi's
minimum of two, so the key does not sit idle on a selection that could
not mean anything else.  The rows of a rectangle are lines like any
other selection's, and every kind is spent the same way: the function
`deactivate-mark' takes the visual-line anchor and `rectangle-mark-mode'
with it.  COUNT is not read while a selection is -- the selection says
how many.  Banked lines are not consulted; a bank is spent by `y', `d'
and `p' only.

The selection used to be ignored -- `V J J g j' joined the third line
with the fourth and dropped the selection, recorded at the time as a
difference from vi worth knowing about rather than changed.  Changed on
request: lines you can see selected are the natural way to say which
lines, and a count a poorer one.  A selection nothing can be done with
-- one line, the last in the buffer -- is kept, the way a refused wrap
key keeps its selection; the message is the same as without one."
  (interactive "p")
  (let ((n (if (use-region-p)
               (max 1 (1- (donkey--region-line-count)))
             (max 0 (or count 1))))
        (joined 0))
    ;; The join starts on the selection's first line, and the selection
    ;; is spent there -- unless nothing is below that line, in which case
    ;; nothing is done and the selection stays where it was.
    (when (and (use-region-p)
               (not (save-excursion (goto-char (region-beginning))
                                    (donkey--no-line-below-p))))
      (goto-char (region-beginning))
      (deactivate-mark))
    (while (and (> n 0) (not (donkey--no-line-below-p)))
      (join-line 1)
      (setq joined (1+ joined)
            n (1- n)))
    ;; N is untouched when nothing joined, so it still says whether a
    ;; join was asked for: a COUNT below 1 asks for none.
    (when (and (zerop joined) (> n 0))
      (message "No line below to join"))))

(defun donkey--region-line-count ()
  "Return how many lines the active region touches.

A region that ends at the start of a line does not touch that line: the
highlight shows nothing of it, and `donkey-comment-dwim' reads a region
stopping at a line's beginning the same way.  That is exactly the count
`count-lines' gives for the region's two ends -- the newlines between
them, plus one when the end is not at a line's beginning -- and where
the region starts within its first line makes no difference to it, so
neither end needs widening first.  A `V' session ends at the END of its
last line -- see `donkey--visual-line-region-bounds' -- so all of its
lines count, and a region on the last line of a buffer with no newline
after it counts that line too."
  (count-lines (region-beginning) (region-end)))

(defun donkey--no-line-below-p ()
  "Return non-nil when no line follows the one point is on.

True on the last line whether or not the buffer ends in a newline: for
`\"a\\n\"' the position after `forward-line' is `point-max' because the
final newline ends the only line, and for `\"a\"' because there is no
newline at all.  On an empty line BETWEEN lines it is nil, so joining a
blank line away still works."
  (save-excursion
    (forward-line 1)
    (eobp)))

;;; ---------------------------------------------------------------------------
;;; Wrap Region Commands
;;; ---------------------------------------------------------------------------

(defcustom donkey-wrap-delimiters '(?\( ?\[ ?\{ ?\" ?\' ?\`)
  "Characters that trigger `donkey-wrap-region' in Normal state.

Bound in `donkey-normal-mode-map'; only takes effect while a
region is active (see `donkey-wrap-region').  Changing this after
`donkey.el' has loaded has no effect on already-bound keys -- set
it before loading, or re-run the `dolist' near
`donkey-normal-mode-map's definition."
  :type '(repeat character)
  :group 'donkey)

(defvar donkey-mark-pair-delimiters) ;(donkey--wrap-close-char); defined below, in "Mark and Text Object Selection Commands"

(defun donkey--wrap-close-char (open-char)
  "Return the character that closes OPEN-CHAR for `donkey-wrap-region'.

Looked up in `donkey-mark-pair-delimiters' when OPEN-CHAR is a
recognized pair there, so bracket-type wrap delimiters (e.g. `(') close
with their real counterpart (`)') instead of themselves; otherwise
OPEN-CHAR is symmetric (e.g. `\"') and closes with itself."
  (or (cdr (assq open-char donkey-mark-pair-delimiters)) open-char))

(defun donkey--insertion-read-only-p (pos)
  "Return non-nil when inserting text at POS would be refused as read-only.

The buffer's own flag is `barf-if-buffer-read-only's business; this is
the `read-only' TEXT PROPERTY, which refuses an insertion only through
stickiness: the character before POS refuses when its `read-only' is
rear-sticky, which it is unless that character's `rear-nonsticky'
covers it (or `text-property-default-nonsticky' does), and the
character after POS refuses when its `read-only' is front-sticky, which
it is only when that character's `front-sticky' covers it.  That is
the rule Emacs applies inside `insert'.

Asked by `donkey-wrap-region' of the places a wrap inserts at, so the
refusal comes before Insert state is entered."
  (or (and (> pos (point-min))
           (get-text-property (1- pos) 'read-only)
           (let ((nonsticky (get-text-property (1- pos) 'rear-nonsticky)))
             (not (or (eq nonsticky t)
                      (and (listp nonsticky) (memq 'read-only nonsticky))
                      (cdr (assq 'read-only text-property-default-nonsticky))))))
      (and (< pos (point-max))
           (get-text-property pos 'read-only)
           (let ((sticky (get-text-property pos 'front-sticky)))
             (or (eq sticky t)
                 (and (listp sticky) (memq 'read-only sticky)))))))

(defun donkey--wrap-refused-by-read-only-text-p ()
  "Return non-nil when the selection's wrap would land beside read-only text.

The places asked are the ones a wrap inserts at: point and both ends
of a linear region -- a pairing package puts the closer at the far
end -- and both column edges of every row of a rectangle.  See
`donkey--insertion-read-only-p' for the rule at each.  A missing mark
reads as point."
  (let* ((mark (or (mark t) (point)))
         (beg (min mark (point)))
         (end (max mark (point))))
    (if (bound-and-true-p rectangle-mark-mode)
        (seq-some (lambda (row)
                    (or (donkey--insertion-read-only-p (car row))
                        (donkey--insertion-read-only-p (cdr row))))
                  (extract-rectangle-bounds beg end))
      (or (donkey--insertion-read-only-p (point))
          (donkey--insertion-read-only-p beg)
          (donkey--insertion-read-only-p end)))))

(defun donkey--wrap-rectangle-region (open-char)
  "Wrap each line of the active rectangle selection with OPEN-CHAR.

Also inserts OPEN-CHAR's matching close character (see
`donkey--wrap-close-char'), each at that line's own rectangle
start/end column.  Uses `move-to-column' with FORCE non-nil, same as
`string-rectangle-line' and other rectangle commands, so lines
shorter than the rectangle are padded with spaces up to each column
instead of bunching both characters together at end of line."
  (let ((close-char (donkey--wrap-close-char open-char)))
    (apply-on-rectangle
     (lambda (startcol endcol)
       (move-to-column endcol t)
       (insert (string close-char))
       (move-to-column startcol t)
       (insert (string open-char)))
     (region-beginning) (region-end))))

(defun donkey-wrap-region ()
  "Insert the pressed delimiter into the active region without deselecting.

Bound to each of `donkey-wrap-delimiters' in Normal state.  With no
active region, falls through to `undefined', same as any other
suppressed key.

With `rectangle-mark-mode' active, wraps each line of the rectangle at
its own start/end column instead; see `donkey--wrap-rectangle-region'.

With an ordinary active region, enters Insert state without
deactivating the mark and inserts the pressed character via
`self-insert-command', letting whatever pairing package is active see
the still-active region and wrap it.  With none enabled the character
is simply inserted at point, since nothing is listening.  Then returns
to Normal state, even if the insertion signals.

A read-only buffer is refused before any of that, with the selection
left standing, and read-only text the same way and at the same
moment, through `donkey--wrap-refused-by-read-only-text-p'.  A
refusal that still comes from inside the insertion is signaled again
from Normal state, with the mark kept.

Which delimiters wrap is the pairing package's decision, not this
command's.  `electric-pair-mode' wraps `(', `[', `{' and `\"' and
inserts `\\='' and `\\=`' at point with the region unwrapped, unless they
are added to `electric-pair-pairs'; Smartparens wraps all six unless a
pair is excluded.  The rectangle path does its own insertion and wraps
every line whatever the package.

A count is ignored: one delimiter press, one wrap.  A wrap does not
stop a keyboard macro that is being recorded."
  (interactive)
  (cond
   ((not (use-region-p))
    (call-interactively #'undefined))
   ;; Only a character event names a delimiter; \\[execute-extended-command]
   ;; or a function key does not.
   ((not (characterp last-command-event))
    (call-interactively #'undefined))
   (t
    ;; Refused here, before any state changes, so the selection
    ;; outlives the refusal.
    (barf-if-buffer-read-only)
    ;; The text-property twin of the check above, at the same moment
    ;; and for the same reason; signaled as Emacs itself signals it.
    (when (donkey--wrap-refused-by-read-only-text-p)
      (signal 'text-read-only nil))
    (if (bound-and-true-p rectangle-mark-mode)
        (donkey--wrap-rectangle-region last-command-event)
      (donkey-insert-mode 1)
      ;; A refusal from the insertion is held until Insert state has
      ;; been left with the mark kept, then signaled again;
      ;; `buffer-read-only' is the parent of `text-read-only'.
      (let (refusal)
        (unwind-protect
            (condition-case err
                ;; The count is DONKEY's, not the pairing package's.
                (let ((current-prefix-arg nil))
                  (self-insert-command 1))
              (buffer-read-only (setq refusal err)))
          ;; The state change alone, not the `C-g' key's errands.
          (donkey--leave-insert (and refusal t)))
        (when refusal
          (signal (car refusal) (cdr refusal))))))))

;;; ---------------------------------------------------------------------------
;;; Mark and Text Object Selection Commands
;;; ---------------------------------------------------------------------------

(defvar-local donkey-visual-anchor nil
  "Anchor position for visual line selection.")

(defun donkey--ensure-non-rectangle-selection ()
  "Clear the selection state an earlier selection may have left behind.

Disables `rectangle-mark-mode' if it is active, and forgets any
`donkey-visual-anchor'.  Each is the state of one KIND of selection --
a rectangle, a visual-line session -- and each is only ever taken down
by `deactivate-mark-hook', which fires on the mark's active -> inactive
transition and not when a command simply repositions an ALREADY-active
mark, which is exactly what `push-mark'/`set-mark'/`activate-mark' do
for every Donkey selection command (`donkey-mark-inner',
`donkey-mark-paragraph', `donkey-set-mark', etc.).  So a selection of
one kind started over a live selection of another kept the old kind's
state underneath it, and the action keys read the new selection as the
old kind.

The rectangle: one left active from an earlier, unrelated
`donkey-rectangle-mark-mode' session silently persisted underneath a
brand new, intended-to-be-linear selection, and the next
`donkey-copy'/`donkey-delete'/`donkey-yank' took the new selection for
a rectangle instead of the intended linear span.  Confirmed live: after
`m v' (rectangle-mark) on one line, then `m p' (mark-paragraph)
elsewhere without canceling the rectangle first, pressing `d' silently
killed a zero-width \"rectangle\" (one empty string per line) instead of
deleting the paragraph, with no error and no visible change to the
buffer at all.

The anchor, for the same reason.  `donkey--visual-line-session-active-p'
knows a session by where the mark sits -- at the anchor, or at the
anchor line's end -- and a fresh selection can land its mark on either:
with point at the end of the line `V' selected, `m w' marks the last
word and leaves the mark at that line's end, and `v' plants one at
point.  Confirmed live: `V m w' highlighted \"beta\" alone and `d' then
removed the whole line; `V v h h' highlighted two characters, `y'
copied the whole line, and the reminder after each `h' read \"Visual
line\" over a selection `v' had just started; `V m w V' said \"Visual
line: canceled\" where a fresh session should have started.  Clearing
the anchor here is what makes the highlight and the action agree.  The
predicate's positional check stays for selections made by commands
outside this package, which never come through here.

Called by every Donkey command that establishes a new selection,
before that command's own `push-mark'/`set-mark' call.  The four
delimiter commands call it only once their search has FOUND a pair or
an expression, so a press that finds none leaves the old selection
standing whole, kind and all; the object commands call it before
their search, whose refusals put point back and mark nothing, so the
old selection's highlight is gone with its kind.
`donkey-mark-run-toggle' does not come
through here itself: adopting a live session needs the anchor, which
`donkey-mark-run-adopt' widens the session through before clearing it,
and the branch that starts a run afresh marks its word with
`donkey-mark-word', which does."
  (when (bound-and-true-p rectangle-mark-mode)
    (rectangle-mark-mode -1))
  (setq donkey-visual-anchor nil))

(defun donkey--clear-visual-anchor ()
  "Clear `donkey-visual-anchor' whenever the mark is deactivated.

Runs on `deactivate-mark-hook', so the anchor never survives past its
region regardless of what deactivated the mark — this command,
`keyboard-quit', or anything else.  Without this, a stale anchor left
over from an abandoned visual-line selection could hijack a later,
unrelated region activation (e.g. via `set-mark-command') in the same
buffer.

Installed buffer-locally by `donkey-visual-line-toggle' at the moment
it sets the anchor, rather than globally at load: an anchor is the
only thing this has to clear, and the buffers holding one are exactly
the buffers where the hook needs to exist.  Adding a function that is
already present is a no-op, so repeated sessions do not grow the hook."
  (setq donkey-visual-anchor nil))


(defun donkey--visual-line-session-active-p ()
  "Return non-nil if point is continuing an active visual-line selection.

Requires an active region, a recorded `donkey-visual-anchor', and that
the mark still sits where a visual-line command would have left it --
either exactly AT the anchor (a line beginning) or at that anchor
line's end.  Those are the only two values `donkey-visual-line-toggle',
`donkey-visual-next-line' and `donkey-visual-previous-line' ever set
the mark to, depending on which side of the anchor point is on.

Checking the mark rather than `last-command' is what lets whole-line
and character-wise motion be mixed freely within one session: `j'/`k'
\(plain `next-line'/`previous-line') move point without touching the
mark, so a following `J'/`K' still recognizes the session and
re-anchors to whole lines, instead of falling through to plain
`forward-line' as a `last-command'-based check would.

The mark test also stands between a STALE anchor and a session it was
never part of.  Every selection command of this package clears the
anchor through `donkey--ensure-non-rectangle-selection' before it sets
its mark, because a new mark can land exactly where a session would
have left it -- see there.  Commands from outside the package
\(\\[set-mark-command], `mark-word', `mark-sexp') reposition an active
mark without passing through it, and `deactivate-mark-hook' does not
fire for a mark that merely moves, so an anchor can outlive its session
that way.  Those commands land the mark somewhere unrelated to the
anchor's line, and the check below fails for them.  Confirmed live:
`V' on a second line and then `mark-sexp', which grows an active region
at the mark's side and so took the word before the line, left the
anchor behind, and `J' pressed on that selection moved down one line as
plain `forward-line' does instead of snapping the region back to the
anchor line."
  (and (region-active-p)
       donkey-visual-anchor
       ;; An anchor outside the accessible portion is not a session this
       ;; can continue.  Buffer positions are absolute and narrowing does
       ;; not move them, so `V' followed by \\[narrow-to-region] (or
       ;; `org-narrow-to-subtree') leaves the anchor pointing at text the
       ;; buffer is no longer showing.  Both `goto-char' below and the
       ;; `set-mark' the J/K commands do afterwards silently CLAMP there
       ;; rather than signaling, so the selection quietly re-anchored on
       ;; the narrowing edge while still presenting itself as the session
       ;; started higher up.  Rejecting it here makes `J'/`K' fall back to
       ;; the plain `forward-line' their docstrings describe.
       (<= (point-min) donkey-visual-anchor)
       (<= donkey-visual-anchor (point-max))
       (mark)
       (or (= (mark) donkey-visual-anchor)
           (= (mark) (save-excursion
                       (goto-char donkey-visual-anchor)
                       (line-end-position))))))

;; Canceling only a genuine session avoids reporting a misleading "Visual
;; line: canceled" for a selection that was never a visual-line session.

(defvar donkey--visual-line-hint
  "Visual line: J/K whole lines, j/k by char, V to cancel"
  "The echo-area reminder shown while a visual-line session is active.

Shown by `donkey-visual-line-toggle' at entry and kept VISIBLE across
the session's motions by `donkey--show-selection-hint', the way
`donkey--mark-run-mode-hint' stays up for mark run mode.")

(defconst donkey--hint-motions
  '(donkey-visual-next-line donkey-visual-previous-line
    next-line previous-line forward-char backward-char
    forward-word backward-word forward-sexp backward-sexp
    beginning-of-line move-end-of-line
    beginning-of-buffer end-of-buffer
    rectangle-forward-char rectangle-backward-char
    rectangle-right-char rectangle-left-char
    rectangle-next-line rectangle-previous-line
    rectangle-exchange-point-and-mark)
  "The commands after which a selection reminder is repainted.

Shared by the three reminders that survive their own motions: a
visual-line session, a linear selection and a rectangle.  Mark run
mode does not use it -- its reminder follows family membership, every
key of the mode being one of its own.

Motions only, and only motions that never message: a command that DID
message must keep its echo, and whether one just did cannot be told
after the fact -- a stale message and a fresh one read the same from
`current-message' -- so the rule is a whitelist of the session's own
silent motions rather than a guess.

The last four are the jumps behind `g h', `g l', `g g' and `g e': they
move point without touching the mark, so the session survives them and
the reminder should too -- it used to go quiet for the rest of a
session that had used one.  The buffer pair messages \"Mark set\" only
when there is no active region to push a mark for, which a live
session always has.

The `rectangle-' seven are the same motions again under another name.
`rectangle-mark-mode' REMAPS the motion keys to its own commands, so a
rectangle moving by `j' and `l' arrives here as `rectangle-next-line'
and `rectangle-forward-char' -- and the reminder, matching on the
names above, went quiet after the first press of a selection that had
not moved yet.  They can only run inside a rectangle, so listing them
costs the other reminders nothing.

This is where the coverage stops: after any command not listed here,
the reminder waits for the next listed motion instead of repainting,
and a count's keystroke echo is never painted over because the prefix
commands are not listed either.")

(defun donkey--repaint-hint (hint)
  "Show HINT in the echo area without logging it.

Both selection modes keep a reminder up across their own motions --
`donkey--visual-line-hint' for a visual-line session,
`donkey--mark-run-mode-hint' for mark run mode -- and both repaint it
from `post-command-hook'.  Unlogged, because a dozen copies of one
reminder is what *Messages* would otherwise keep."
  (let ((message-log-max nil))
    (message "%s" hint)))

(defvar donkey--linear-selection-hint
  "Linear selection: any motion extends, v re-anchors, C-g to cancel"
  "The echo-area reminder shown while a `donkey-set-mark' selection lives.

Shown by `donkey-set-mark' at entry and kept VISIBLE across the
selection's motions by `donkey--show-selection-hint', the way
`donkey--visual-line-hint' and `donkey--mark-run-mode-hint' stay up
for their own modes.

The wording says what the other two do not have to.  A visual-line
session and a mark run each own their keys, so their reminders can
list them; this one owns none -- every motion extends it, which is the
whole of what it offers -- and the two things worth saying are that a
second `v' re-anchors rather than cancels, and that
\\[keyboard-quit] is what lets go.  Both are the questions the
command's own docstring exists to answer.")

(defvar donkey--rectangle-hint
  "Rectangle: any motion sizes the block, m v or C-g to cancel"
  "The echo-area reminder shown while `rectangle-mark-mode' is on.

Shown by `donkey-rectangle-mark-mode' at entry and kept VISIBLE across
the block's motions by `donkey--show-selection-hint'.

It replaces the \"Mark set (rectangle mode)\" that
`rectangle-mark-mode' says for itself, which names the state without
saying anything about leaving it -- and leaving is the part a
rectangle most needs to advertise, being the one selection that
several commands refuse outright.")

(defvar-local donkey--linear-selection-active nil
  "Non-nil while a selection `donkey-set-mark' started is still live.

A visual-line session has `donkey-visual-anchor' and a rectangle has
`rectangle-mark-mode'; a linear selection is just an active mark, and
nothing about the mark says which key made it.  So the key says so
itself, and `donkey--clear-selection-hint' takes it back on
`deactivate-mark-hook' -- whatever deactivated the mark, this command,
\\[keyboard-quit], or an edit that consumed the selection.

Buffer-local, because a selection is: two buffers can each hold one,
and neither should speak for the other.")

(defun donkey--clear-selection-hint ()
  "Forget a linear selection, and take its reminder off the screen.

On `deactivate-mark-hook', installed buffer-locally by
`donkey-set-mark' and `donkey-rectangle-mark-mode' at the moment each
starts a selection -- the same bargain `donkey--clear-visual-anchor'
makes for the anchor, and for the same reason: the buffers holding a
selection are exactly the buffers where the hook needs to exist.
Adding a function already present is a no-op, so repeated selections
do not grow the hook.

A reminder is the only sign on screen that a selection is live, so it
must not outlive one.  `d' or `y' over a linear selection ends it
without a word, and the echo area went on advertising a selection that
was already gone.  Cleared only when the reminder is what is showing:
the same no-clobber rule `donkey--mark-run-exit' keeps, so a command
that said something of its own keeps its echo.

Both reminders are checked because both selections end here.  A
rectangle canceled with `m v' says so for itself, but one consumed by
an action key goes the same quiet way a linear selection does."
  (setq donkey--linear-selection-active nil)
  (when (member (current-message)
                (list donkey--linear-selection-hint donkey--rectangle-hint))
    (message nil)))

(defun donkey--show-selection-hint ()
  "Keep the live selection's reminder visible across its own motions.

On `post-command-hook' for the life of `donkey-mode' -- registered in
`donkey--global-hooks' rather than per session, so two selections in
two buffers cannot strand or double it.

One function for all three selections, deciding once.  They were three
hooks with a set of exclusions each, and the rule they add up to --
that only one reminder is ever on screen -- was written nowhere and
true only by arithmetic: every hook ran, and whichever painted last
won.  Hook ORDER settled two of the cases, which is not a rule anybody
can hold, and a guard removed from one of them went on looking right
until somebody reordered the list.  Here the precedence IS the `cond',
in order, and a case that cannot arise is absent rather than guarded.

A rectangle comes first: it can be entered over either of the others
without deactivating the mark, so their grounds survive underneath it
and it has the better claim.  Then a visual-line session, which a
linear selection likewise survives into.  A linear selection last,
being the one the other two get built on top of.

Mark run mode is not here at all.  It repaints from its own
`donkey--mark-run-mode-post-command', by family membership rather than
by motion, and nothing in `donkey--hint-motions' can run while it is
armed -- the mode rebinds every motion key it owns.

The selection tests come before the motion test because most commands
are neither: three variable reads answer for every keystroke in a
buffer with nothing selected, and the list is only walked once
something is.  Repaints after the motions in `donkey--hint-motions'
and after nothing else, so a command that said something of its own
keeps its echo and a count's keystroke echo is never painted over.

Guarded, not signaling -- a function that errors on
`post-command-hook' is silently removed for the session."
  (when (and (or (bound-and-true-p rectangle-mark-mode)
                 donkey-visual-anchor
                 donkey--linear-selection-active)
             (memq this-command donkey--hint-motions))
    (cond
     ((and (bound-and-true-p rectangle-mark-mode) mark-active)
      (donkey--repaint-hint donkey--rectangle-hint))
     ((and donkey-visual-anchor (donkey--visual-line-session-active-p))
      (donkey--repaint-hint donkey--visual-line-hint))
     ((and donkey--linear-selection-active mark-active)
      (donkey--repaint-hint donkey--linear-selection-hint)))))

(defun donkey-visual-line-toggle (&optional arg)
  "Start/cancel visual line selection; with a count, select that many rows.

Only cancels when a visual-line session is genuinely active (see
`donkey--visual-line-session-active-p').  Pressing this with some OTHER
active region -- a `donkey-mark-inner' selection, say -- starts a fresh
visual-line session anchored at the current line instead.

With a count, selects that many rows at once, anchored on the cursor's
line: \\[universal-argument] 3 \\[donkey-visual-line-toggle] is this key
followed by \\[donkey-visual-next-line] twice, and `J'/`K' afterwards
grow or shrink it from the same anchor.  A negative count selects
UPWARD, the way \\[universal-argument] -3 \\[donkey-visual-next-line]
moves.  Running out of buffer stops where `J' stops.  A counted press
always starts a fresh selection, even over a live session -- a count is
an instruction about size, and cancelling is what the bare press is
for.  Zero is a bare press, as it is on the mark keys.

ARG is the raw prefix argument, because a bare press and
\\[universal-argument] 1 must be told apart: only the bare press
toggles.  Took no count for a long time, as the other selection
toggles still do not; changed on request, since every modal editor's
line-select key takes one and \\[universal-argument] 3 \\[donkey-visual-next-line],
\\[universal-argument] 3 \\[donkey-bank-selection] already read a count as rows.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

What is highlighted is one character short of what `y', `d' and `p'
take.  The selection stops at the end of the last line, so the newline
ending it is NOT shown as selected -- but `donkey-copy', `donkey-delete'
and `donkey-yank' widen a live session to whole lines before acting, so
the line break goes with it: `d' removes the line outright rather than
emptying it, `y' gives a kill that pastes back as a complete line, and
`p' replaces the line instead of opening an empty one under it."
  (interactive "P")
  (let ((n (prefix-numeric-value arg)))
    (cond
     ((and arg (not (zerop n)))
      (donkey--visual-line-start n))
     ((donkey--visual-line-session-active-p)
      ;; `deactivate-mark' clears the anchor through the buffer-local
      ;; `deactivate-mark-hook'.  That hook is guaranteed present: an
      ;; active session requires a non-nil anchor, and the only code
      ;; that sets one is `donkey--visual-line-start', one line after
      ;; installing the hook.
      (deactivate-mark)
      (message "Visual line: canceled"))
     (t
      (donkey--visual-line-start 1)))))

(defun donkey--visual-line-start (n)
  "Start a visual-line session of N rows, anchored on the cursor's line.

The start branch of `donkey-visual-line-toggle', for a bare press (N
of 1) and a counted one alike.  The shape is the one the session's own
motions leave, so `J' and `K' pick it up as theirs: rows downward keep
the mark at the anchor with point at the last row's end, rows upward
put the mark at the anchor line's end with point at the first row's
start -- the two layouts `donkey--visual-line-session-active-p' knows.
N below zero counts upward; the motion is `forward-line', which stops
at the buffer's edge the way the session's `J' and `K' do."
  (donkey--ensure-non-rectangle-selection)
  (add-hook 'deactivate-mark-hook #'donkey--clear-visual-anchor nil t)
  (setq donkey-visual-anchor (line-beginning-position))
  (if (> n 0)
      (progn
        (set-mark (line-beginning-position))
        (forward-line (1- n))
        (end-of-line))
    (set-mark (line-end-position))
    (forward-line (1+ n))
    (beginning-of-line))
  (activate-mark)
  (message "%s" donkey--visual-line-hint))

(defun donkey-visual-next-line (&optional count)
  "Move down COUNT lines, extending the visual-line selection if active.

See `donkey--visual-line-session-active-p' for what \"active\" means
here; otherwise this is a plain `forward-line'.

The selection always spans whole lines from `donkey-visual-anchor' to
point, on whichever side of the anchor point currently is: moving down
while already below the anchor keeps growing downward (mark pinned to
the anchor's line start, point at the new line's end); moving down
while still above the anchor instead shrinks the selection from the
top (mark moves to the anchor's line end, point to the new line's
start) -- covering the case where `J' first moves point back up TO,
and then past, the anchor line itself.

COUNT defaults to 1, and a negative COUNT moves up instead.  The
selection is re-derived from the anchor and wherever point lands, not
accumulated as it goes, so a count needs no special handling: the
branch below is the same one a run of single presses would end on.
Counts are what `j' and `k' already do -- they are bound straight to
`next-line' and `previous-line' -- so leaving them off here made
\\[universal-argument] 5 J move a single line while
\\[universal-argument] 5 j moved five.

Inside a rectangle this moves as `j' does there: `rectangle-mark-mode'
remaps `next-line' to `rectangle-next-line', which keeps the column,
so the block grows by a row.  A rectangle is not a visual-line session,
and the plain `forward-line' this fell through to lands on column 0,
which collapsed the block to nothing on the very key a reader who
grows selections with `J' reaches for.  Confirmed live: `m v l l J'
left a zero-width block and `d' then said there was nothing to delete.
`donkey-visual-previous-line' mirrors it."
  (interactive "p")
  (cond
   ((donkey--visual-line-session-active-p)
    (forward-line (or count 1))
    (if (> (line-beginning-position) donkey-visual-anchor)
        (progn
          (set-mark donkey-visual-anchor)
          (end-of-line))
      (progn
        (set-mark (save-excursion
                    (goto-char donkey-visual-anchor)
                    (line-end-position)))
        (beginning-of-line)))
    (activate-mark))
   ((bound-and-true-p rectangle-mark-mode)
    (rectangle-next-line (or count 1)))
   (t
    (forward-line (or count 1)))))

(defun donkey-visual-previous-line (&optional count)
  "Move up COUNT lines, extending the visual-line selection if active.

See `donkey--visual-line-session-active-p' for what \"active\" means
here; otherwise this is a plain `forward-line' with a negative count.

Mirrors `donkey-visual-next-line': moving up while already above the
anchor keeps growing upward; moving up while still below the anchor
shrinks the selection back down toward it instead, covering the case
where `K' moves point up past the anchor line.

COUNT defaults to 1, and a negative COUNT moves down instead; see
`donkey-visual-next-line' for why no accumulation is needed.

Inside a rectangle this moves as `k' does there, through
`rectangle-previous-line', for the reason `donkey-visual-next-line'
gives."
  (interactive "p")
  (cond
   ((donkey--visual-line-session-active-p)
    (forward-line (- (or count 1)))
    (if (< (line-beginning-position) donkey-visual-anchor)
        (progn
          (set-mark (save-excursion
                      (goto-char donkey-visual-anchor)
                      (line-end-position)))
          (beginning-of-line))
      (progn
        (set-mark donkey-visual-anchor)
        (end-of-line)))
    (activate-mark))
   ((bound-and-true-p rectangle-mark-mode)
    (rectangle-previous-line (or count 1)))
   (t
    (forward-line (- (or count 1))))))

(defun donkey-rectangle-mark-mode ()
  "Toggle rectangle mark mode.

If a region is already active (e.g. from `donkey-mark-inner') when
enabling, `rectangle-mark-mode' reinterprets that EXISTING region as a
rectangle using its own corners, rather than starting a fresh
single-column selection at point -- matching stock Emacs's own
documented behavior for entering `rectangle-mark-mode' with an active
region.  Only widen the initial selection by one column when there was
no region to begin with: doing it unconditionally would otherwise
silently extend an existing selection being converted to a rectangle
by one extra character past its real, intended boundary.  Checks
`mark-active' directly rather than `region-active-p', since the latter
also requires `transient-mark-mode', which is off by default in
`--batch' Emacs and would always read as nil there regardless of
whether a region was genuinely active."
  (interactive)
  (if (bound-and-true-p rectangle-mark-mode)
      (progn
        (rectangle-mark-mode -1)
        (deactivate-mark)
        (message "Rectangle: canceled"))
    (let ((had-active-region mark-active))
      (rectangle-mark-mode 1)
      (add-hook 'deactivate-mark-hook #'donkey--clear-selection-hint nil t)
      ;; Give the rectangle some initial width beyond the single starting
      ;; column, but only for a genuinely fresh selection.  At the very
      ;; end of the buffer there's nothing to widen into, and `right-char'
      ;; signals `end-of-buffer' -- harmless to skip, since
      ;; rectangle-mark-mode is already correctly enabled with a (valid,
      ;; if zero-width) selection at that point.
      ;;
      ;; End of LINE is skipped for a different reason: `right-char' there
      ;; steps over the newline onto the next line at column 0, which does
      ;; not widen the rectangle -- it MOVES it, to a column at the far
      ;; side of the buffer from the one being looked at.  Confirmed live:
      ;; point at the end of "alpha" (column 5), then "m v", left a
      ;; rectangle whose columns were (0 . 0).  Anything done to it landed
      ;; against the left margin, and appending to a block of lines --
      ;; which is what standing at end of line and pressing "m v" means --
      ;; was unreachable.  A zero-width rectangle at the column point is
      ;; actually on is both correct and the useful thing there, since
      ;; `string-rectangle' inserts rather than replaces when the width is
      ;; zero.
      (unless (or had-active-region (eolp))
        (condition-case nil
            (right-char 1)
          (end-of-buffer nil)))
      ;; Last, so it is what stays: `rectangle-mark-mode' says "Mark set
      ;; (rectangle mode)" on the way in, and the widening above can
      ;; speak too.
      (message "%s" donkey--rectangle-hint))))

(defcustom donkey-mark-pair-delimiters
  '((?\{ . ?\}) (?\[ . ?\]) (?\( . ?\)) (?\< . ?>)
    (?\" . ?\") (?\' . ?\') (?\` . ?\`) (?‘ . ?’) (?“ . ?”)
    (?= . ?=) (?* . ?*) (?~ . ?~) (?\| . ?\|) (?\\ . ?\\)
    (?/ . ?/) (?: . ?:) (?+ . ?+) (?_ . ?_) (?$ . ?$))
  "Delimiter pairs (OPEN . CLOSE) for `donkey-mark-inner'/`donkey-mark-outer'.

For symmetric delimiters (e.g. quotes, where the same character both
opens and closes a pair), OPEN and CLOSE are identical.

Customize this to add or remove supported delimiters -- e.g. add
`(?# . ?#)' for a language that uses # as an inline marker, or remove
pairs you never use.  Order does not matter: the prompt names this
variable rather than listing it, and either half of a pair answers
it."
  :type '(alist :key-type (character :tag "Open")
                :value-type (character :tag "Close"))
  :group 'donkey)

(defun donkey--mark-pair-prompt ()
  "Return the `read-char' prompt for `m i' and `m a'.

It NAMES the delimiters rather than listing them.  Listing meant
nineteen characters across the echo area, most of a line spent on
something nobody reads twice, and the list grows with every pair a
reader adds to `donkey-mark-pair-delimiters'.  Naming the variable
covers the built-in pairs and any customized ones at once, and \\[describe-variable]
on it shows both, marked as customized where they are.

The README lists the defaults in full, and a test keeps that list
honest."
  "Delimiter (see donkey-mark-pair-delimiters): ")

(defun donkey--mark-pair-unsupported-error (char)
  "Signal a `user-error' for CHAR not in `donkey-mark-pair-delimiters'.

A `user-error' rather than a bare `error': this is reached by answering
the `m i'/`m a' prompt with a character that is not a delimiter, which is
an ordinary typo -- not a malfunction.  A bare `error' pops the debugger
for anyone running with `debug-on-error' on.

The message names `donkey-mark-pair-delimiters' rather than reciting it,
for the reason `donkey--mark-pair-prompt' gives: the list is nineteen
characters long, it grows with customization, and \\[describe-variable] on the
variable shows the reader their own.

CHAR is spelled the way a key binding is -- `single-key-description'
-- rather than inserted as itself.  It is whatever `read-char' handed
back, and the keys a reader is likeliest to hit by accident at a
prompt do not print: \\`RET' made the message wrap onto a second
line, \\`SPC' and \\`TAB' showed an empty pair of quotes, \\`DEL' and
the control characters showed nothing at all, and a META key, which
`read-char' returns with the modifier bit set, was not a character to
`format' at all and turned the typo into \"Wrong type argument:
characterp, 134217848\" -- a bare error, popping the debugger this
function exists to keep shut.  Confirmed live for all of them.  A
printing character comes out as itself either way."
  (user-error "Unsupported delimiter `%s'; see donkey-mark-pair-delimiters"
              (single-key-description char)))

(defun donkey--mark-pair-open-for (char)
  "Return the OPEN character of the pair CHAR belongs to.

CHAR itself when it opens a pair, and the opener when it CLOSES one, so
the prompt takes \\=`)\\=' for \\=`(\\=', and \\=`]\\=' for \\=`[\\='.  Point
sitting on a closer has always resolved this way -- `rassq' against
`donkey-mark-pair-delimiters' -- and there is no reason a reader who
TYPES the closer should be told it is unsupported when the same
character under the cursor is understood.

A symmetric delimiter opens and closes with itself, so `assq' answers
first and the second lookup never sees it.

Anything else comes back unchanged, for the close lookup in the caller
to reject by name."
  (cond ((assq char donkey-mark-pair-delimiters) char)
        ((rassq char donkey-mark-pair-delimiters)
         (car (rassq char donkey-mark-pair-delimiters)))
        (t char)))

(defun donkey--pair-delimiter-already-taken ()
  "Do nothing, silently, and keep the mark command's repeat alive.

What a delimiter key runs for one press after `m i' or `m a' resolved
its delimiter without reading a key.  Silent on purpose: the selection
message is the answer the press was after, and overwriting it with an
explanation would take away the one thing worth reading.

`this-command' is set to `last-command' -- the `m i' or `m a' whose
press this was -- so the command loop hands the next key the same
`last-command' it would have seen had the delimiter never been typed.
Without that the swallowed press itself became `last-command', and a
following `m i' was a fresh search instead of a repeat: from inside
\"((a (b c) d) e)\", `m i ( m i ( m i (' stalled at \"a (b c) d\", since
a fresh search from where the selection left point finds the pair it
is already inside.  `donkey--mark-extending-p' compares the two, and
says why."
  (interactive)
  (setq this-command last-command))

(defun donkey--suppress-one-pair-delimiter ()
  "Make the NEXT key harmless if it names a delimiter.

`m i' and `m a' read their delimiter from the character at point when
point is on one, and then they are finished -- so the delimiter the
reader types as the third key of \\=`m i (\\=' never reaches `read-char'
and runs as a command in its own right.  Every delimiter this package
knows is bound to `donkey-wrap-region', which INSERTS: \\=`m i (\\=' with
point on the paren turned \"call(alpha) end\" into \"call((alpha) end\".
A selection key had edited the buffer, silently, from the sequence the
reader meant to select with.

The press is swallowed rather than the auto-detect being taken away, so
both spellings work and neither edits: \\=`m i\\=' alone still selects,
\\=`m i (\\=' selects and the paren does nothing.  One press only -- a
second \\=`(\\=' wraps the selection, which is how somebody who wanted the
wrap gets it.

A repeat arms it too.  A second \\=`m i\\=' never reads a delimiter --
`donkey--mark-pair-select' reuses the one it resolved -- so the paren a
reader types after it out of habit is as loose as after the first
press.  Unprotected, \\=`m i ( m i (\\=' from inside \"((a (b c) d) e)\"
grew the selection a level and then ran the paren: \"(((a (b c) d) e)\"
with nothing pairing, a wrap under `electric-pair-mode', and a `d'
pressed next deleted one character, the insertion having ended the
selection.  Confirmed live in `emacs -nw'.  So every press of the two
commands that does not stop at the prompt, fresh or repeat, protects
exactly one delimiter press -- once it has FOUND its pair.

A press that finds none arms nothing.  The swallow used to be armed
where the delimiter was read from the buffer, before the search, so a
refused press left it standing: with the cursor on the paren of an
unbalanced \"(abc\", \\=`m i\\=' said \"No matching )\" and then ate the
\\=`(\\=' typed next, silently -- the one key a reader reaches for to wrap
the selection the refusal had just left standing.  A refused key
changes nothing, and that includes the key after it.

Closing characters are bound too, point being able to sit on either
end of a pair.

Nothing is armed for a call from Lisp.  There is no next keystroke to
protect there, and the map would be left standing: `set-transient-map'
puts it in `overriding-terminal-local-map', which is terminal-wide and
comes down on the next COMMAND, so a caller outside the command loop
leaves it up for whatever runs next.  The suite showed this the moment
it was written -- the marking tests call the command directly with
point on a delimiter, and the check that no key of this package
shadows an Emacs command outside the documented few then resolved
\\=`(\\=' through the leftover map and reported one.  Three shuffled
orders caught it; the fixed order did not.

The test is that `this-command' IS one of the two commands, not merely
that it is set.  The command loop sets it before each command and
nothing resets it afterwards, so outside the loop it holds whatever
ran last: an earlier test had left `kill-region' there, and a later
Lisp call with point on a brace found it non-nil and armed the map all
the same -- the one shuffled order that survived the first guard.
Naming the commands also fixes what a wrapper gets: a command
of the reader\\='s own that calls this one is not `m i', so nobody typed
a delimiter after it, and its next key must not be eaten.
`donkey--mark-extending-p' guards on `this-command' for the same
reason, and says so."
  (when (memq this-command '(donkey-mark-inner donkey-mark-outer))
    (let ((map (make-sparse-keymap)))
      (dolist (pair donkey-mark-pair-delimiters)
        (define-key map (vector (car pair))
                    #'donkey--pair-delimiter-already-taken)
        (define-key map (vector (cdr pair))
                    #'donkey--pair-delimiter-already-taken))
      (set-transient-map map))))

(defun donkey--mark-pair-read-delimiter ()
  "Return (OPEN-CHAR CLOSE-CHAR ON-OPENER AUTO) for the char pair to mark.

Uses the character at point when it is a recognized OPEN or CLOSE
delimiter (see `donkey-mark-pair-delimiters'); otherwise prompts via
`read-char'.  ON-OPENER is non-nil only when point sits on the OPEN
side -- when it sits on the CLOSE side of an asymmetric pair (e.g. `)'
for `(', where OPEN and CLOSE differ), OPEN-CHAR is still resolved
automatically here, but ON-OPENER comes back nil so
`donkey--mark-pair-positions' takes its search-backward-then-forward
path instead of assuming point is the opener.

AUTO is non-nil when the delimiter was read from the buffer rather
than from a key, so the delimiter the reader types next is loose.
Whether to swallow that press is the caller's decision, made once the
pair has been found -- see `donkey--suppress-one-pair-delimiter' for
why it is not made here."
  (let* ((default-char (char-after))
         (on-opener (and default-char (assq default-char donkey-mark-pair-delimiters)))
         (on-closer (and default-char (not on-opener)
                          (rassq default-char donkey-mark-pair-delimiters)))
         (open-char (cond
                     (on-opener default-char)
                     (on-closer (car on-closer))
                     (t (donkey--mark-pair-open-for
                         (read-char (donkey--mark-pair-prompt))))))
         (close-char (or (cdr (assq open-char donkey-mark-pair-delimiters))
                         (donkey--mark-pair-unsupported-error open-char))))
    (list open-char close-char on-opener (and (or on-opener on-closer) t))))

(defun donkey--mark-pair-scan-forward (open-char close-char)
  "Scan forward for the CLOSE-CHAR balancing one already-open OPEN-CHAR.

Counts nested OPEN-CHAR/CLOSE-CHAR occurrences of the SAME type along
the way, so a nested pair of the same delimiter (e.g. the inner
`(...)' in \"(a(b)c)\") does not get mistaken for the enclosing one's
close.  Returns the position immediately after the matching CLOSE-CHAR.
Signals `search-failed' if the nesting never closes before the end of
the buffer.  Only valid when OPEN-CHAR and CLOSE-CHAR differ --
nesting is meaningless for a symmetric delimiter, where the same
character both opens and closes."
  (let ((regexp (concat (regexp-quote (string open-char))
                         "\\|" (regexp-quote (string close-char))))
        (depth 1))
    (while (> depth 0)
      (unless (re-search-forward regexp nil t)
        (signal 'search-failed (list (string close-char))))
      (setq depth (if (eq (char-before) open-char) (1+ depth) (1- depth))))
    (point)))

(defun donkey--mark-pair-scan-backward (open-char close-char)
  "Scan backward for the OPEN-CHAR balancing one already-closed CLOSE-CHAR.

Counts nested OPEN-CHAR/CLOSE-CHAR occurrences of the SAME type along
the way, mirroring `donkey--mark-pair-scan-forward'.  Returns the
position of the matching OPEN-CHAR.  Signals `search-failed' if the
nesting never opens before the start of the buffer.  Only valid when
OPEN-CHAR and CLOSE-CHAR differ."
  (let ((regexp (concat (regexp-quote (string open-char))
                         "\\|" (regexp-quote (string close-char))))
        (depth 1))
    (while (> depth 0)
      (unless (re-search-backward regexp nil t)
        (signal 'search-failed (list (string open-char))))
      (setq depth (if (eq (char-after) close-char) (1+ depth) (1- depth))))
    (point)))

(defun donkey--mark-pair-positions (open-char close-char on-opener)
  "Return (START-POS . END-POS) for the delimiter pair around point.

OPEN-CHAR/CLOSE-CHAR are the pair's delimiters.  ON-OPENER is non-nil
when the character at point already matched OPEN-CHAR (i.e. no
`read-char' prompt was needed to pick a delimiter).

When ON-OPENER, point is always assumed to be the OPENING delimiter
first, and the search goes forward for its close -- same as if the
user had just typed it.  For symmetric delimiters (OPEN-CHAR equals
CLOSE-CHAR, e.g. `\"', `|', `~'), that assumption can be wrong: point
may actually be sitting on the pair's CLOSING occurrence instead (e.g.
the closing quote of \"hello\"), which looks identical to an opening
one.  If the forward search fails to find a close, this falls back to
treating point as the closer instead and searches backward for the
matching opener.  Only symmetric delimiters get this fallback:
asymmetric ones (e.g. `(' and `)') can never have this ambiguity,
since the closing character is never itself a member of the
recognized-opener set, so point being ON-OPENER there always
genuinely means the opening delimiter.

For asymmetric delimiters, forward/backward searches go through
`donkey--mark-pair-scan-forward'/`donkey--mark-pair-scan-backward'
instead of a plain `search-forward'/`search-backward', so nested
occurrences of the SAME delimiter (e.g. `(a(b)c)') resolve to the
correct enclosing pair rather than the nearest occurrence of the
character regardless of nesting.  Symmetric delimiters keep using a
plain search: nesting has no well-defined meaning when the same
character serves as both open and close.

START-POS is the position of the opening delimiter; END-POS is the
position immediately after the closing delimiter.

Searches are always case-sensitive (`case-fold-search' bound to nil),
regardless of the buffer's own `case-fold-search' setting -- otherwise
a delimiter like an uppercase `X' would also match a lowercase `x' in
the buffer, silently pairing with the wrong occurrence.

Wrapped in `save-excursion': every search above moves point as a means
to compute START-POS/END-POS, not as a side effect callers should see.
That matters most when no pair is found at all -- e.g. point sitting
well outside any bracket on a line with several unrelated pairs, like
after the last `)' on \";; To (create a (file), visit) it with...\".
The nesting-aware backward scan there walks past several real `('/`)'
occurrences (correctly counting depth as it goes) before ultimately
running out of buffer and signaling `search-failed', converted to the
error below -- but each of those intermediate matches really did move
point, so without `save-excursion' the error would still leave point
sitting at the last successfully-found delimiter (confusingly, on some
unrelated `(' elsewhere in the buffer) instead of exactly where the
user invoked the command from.

Those conversions are to `user-error', not `error'.  Pressing this on a
line with no bracket on it is an ordinary miss, not a malfunction, and a
bare `error' pops the debugger for anyone running with `debug-on-error'
on.  `donkey-mark-word', `donkey-mark-symbol' and `donkey-mark-sentence'
all guard their own \"nothing there\" cases the same way."
  (let ((symmetric (= open-char close-char))
        start-pos end-pos (case-fold-search nil))
    (save-excursion
      (if on-opener
          (progn
            (setq start-pos (point))
            (goto-char (1+ start-pos))
            (condition-case nil
                (setq end-pos (if symmetric
                                   (search-forward (string close-char) nil nil)
                                 (donkey--mark-pair-scan-forward open-char close-char)))
              (search-failed
               (unless symmetric
                 (user-error "No matching '%c' found after cursor" close-char))
               (goto-char start-pos)
               (setq end-pos (1+ start-pos))
               (condition-case nil
                   (setq start-pos (search-backward (string open-char) nil nil))
                 (search-failed
                  (user-error "No matching '%c' found before cursor" open-char))))))
        (if (and (char-after) (= (char-after) open-char))
            (setq start-pos (point))
          (condition-case nil
              (setq start-pos (if symmetric
                                   (search-backward (string open-char) nil nil)
                                 (donkey--mark-pair-scan-backward open-char close-char)))
            (search-failed
             (user-error "No '%c' found near cursor" open-char))))
        (goto-char (1+ start-pos))
        (condition-case nil
            (setq end-pos (if symmetric
                               (search-forward (string close-char) nil nil)
                             (donkey--mark-pair-scan-forward open-char close-char)))
          (search-failed
           (user-error "No matching '%c' found after cursor" close-char))))
      (cons start-pos end-pos))))

(defun donkey--mark-pair-widen-symmetric (char span levels)
  "Widen SPAN outward by LEVELS-1 more occurrences of CHAR each way.

How a count works for a delimiter that opens and closes alike.  There is
no nesting to step out of -- CHAR gives no way to tell an opener from a
closer -- so a level counts OCCURRENCES instead: level 2 is the second
CHAR back and the second CHAR forward, and so on.

Once refused outright, on the reasoning that a character serving as both
ends has no nesting for a level to refer to.  That argument proves too
much: it rules out level 1 as well, which ships and is useful.  Marking a
symmetric pair is already a nearest-one-each-way heuristic -- in
\"say `alpha' beta `gamma' done\" with point in \"beta\", level 1 selects
the GAP between two quoted strings rather than a quoted string, because
nothing there says which quote opens.  A count inherits that heuristic
rather than introducing a new one, and the case it makes possible is
ordinary prose: with point in \"writing\" in

    \"No use \"writing on paper.\" That\"

level 1 gives \"writing on paper.\" and level 2 gives the whole of the
outer quotation, which is what asking for two levels plainly means.

Signals a `user-error' when the text runs out of delimiters before the
count does, the same one the nesting-aware path signals."
  (let ((extra (1- levels))
        ;; Case-sensitive, like `donkey--mark-pair-positions' and for the
        ;; same reason: `donkey-mark-pair-delimiters' is a defcustom, so a
        ;; LETTER can be configured as a delimiter, and a case-folded
        ;; search would count a lowercase `x' toward a count of uppercase
        ;; `X'.  Buffers default to `case-fold-search' t, so leaving it
        ;; alone here meant level 1 (which binds it) and level 2 (which
        ;; did not) disagreed about what a delimiter even is: on
        ;; "A X one x mid X TARGET X two X B" a count of 2 stopped at the
        ;; lowercase x and marked " mid X TARGET X two ".
        (case-fold-search nil))
    (if (<= extra 0)
        span
      (save-excursion
        (condition-case nil
            (let ((start (progn (goto-char (car span))
                                (search-backward (string char) nil nil extra)))
                  (end (progn (goto-char (cdr span))
                              (search-forward (string char) nil nil extra))))
              (cons start end))
          (search-failed
           (user-error "No enclosing `%c' beyond that level" char)))))))

(defun donkey--mark-pair-positions-nth (open-char close-char on-opener levels)
  "Return (START . END) for the LEVELS-th enclosing OPEN-CHAR/CLOSE-CHAR pair.

ON-OPENER is passed through to `donkey--mark-pair-positions' for the
first level; see there for what it means.  LEVELS of 1 is the pair that
function finds on its own.
Each level beyond that steps just outside the pair already found and
searches again, so from inside the inner parentheses of
\"(up at (the hospital) bemoaning)\" a LEVELS of 2 gives the outer pair.
The forward and backward scans count depth, so the pair already stepped
out of is skipped rather than re-matched.

A symmetric delimiter has no depth to count, so it goes through
`donkey--mark-pair-widen-symmetric' instead, which counts occurrences
outward.  Every delimiter in `donkey-mark-pair-delimiters' therefore
takes a count, by one route or the other.

Signals a `user-error' when there is no enclosing pair left."
  (let ((span (donkey--mark-pair-positions open-char close-char on-opener)))
    (when (= open-char close-char)
      ;; Widen FIRST, with the real count, then flatten LEVELS so the
      ;; depth-counting loop below is a no-op -- `setq' assigns left to
      ;; right, so the other order would hand the widener a count of 1.
      (setq span (donkey--mark-pair-widen-symmetric open-char span levels)
            levels 1))
    (dotimes (_ (1- levels))
      (when (<= (car span) (point-min))
        (user-error "No enclosing `%c' beyond that level" open-char))
      (setq span (save-excursion
                   (goto-char (1- (car span)))
                   ;; Running out of enclosing pairs is what a count too
                   ;; large FOR THIS TEXT looks like, and it is ordinary
                   ;; rather than exceptional: bare
                   ;; \\[universal-argument] means FOUR, so `C-u m i' asks
                   ;; for four levels on text that is usually one or two
                   ;; deep.  Left to itself the scan reports "No `(' found
                   ;; near cursor" -- which contradicts a screen plainly
                   ;; showing one, reads like the delimiter was mistyped
                   ;; rather than the count overshot, and being a bare
                   ;; `error' pops the debugger for anyone running with
                   ;; `debug-on-error' on.  The `point-min' check above
                   ;; only catches the case where the pair found last
                   ;; started at the very first position.
                   (condition-case nil
                       (donkey--mark-pair-positions open-char close-char nil)
                     (error
                      (user-error "No enclosing `%c' beyond that level"
                                  open-char))))))
    span))

(defvar-local donkey--mark-pair-state nil
  "How the last `m i'/`m a' selection was arrived at, for growing it.
A list (ANCHOR OPEN-CHAR CLOSE-CHAR ON-OPENER LEVEL): where the
delimiter search ran from, what it resolved to, and how many levels out
it went.  Read only by `donkey--mark-pair-select' and only when the same
command repeats, so a stale entry is never consulted.  A plain position
rather than a marker: nothing can edit the buffer between two presses of
the same key, since an editing command in between is exactly what stops
the second press counting as a repeat.")

(defun donkey--mark-pair-select (inner-p &optional count)
  "Shared implementation for `donkey-mark-inner'/`donkey-mark-outer'.

With INNER-P non-nil, selects the content between the delimiters,
excluding them; otherwise selects the delimiters too.

COUNT selects how many levels out to go -- see
`donkey--mark-pair-positions-nth'.  Repeating the command goes one level
further out per press, so `m i m i' reaches what `C-u 2 m i' reaches.

Point is left at the START of the selection and the mark at its end,
the same way round as every other DONKEY mark command and as
`mark-sexp'.

The selection standing before the press is cleared only once the pair
has been FOUND -- see the comment at the marking below."
  ;; A repeat re-runs the ORIGINAL search one level wider rather than
  ;; searching afresh from wherever the last selection left point.  Two
  ;; things fall out of that, and neither is available to a fresh search:
  ;;
  ;; Repeating never prompts.  The delimiter is remembered, so the second
  ;; press does not go back through
  ;; `donkey--mark-pair-read-delimiter' -- which auto-detects only when
  ;; point is ON a delimiter, and would otherwise sit waiting on
  ;; `read-char'.  Whichever end of the selection point is left at, one
  ;; of `m i' and `m a' lands somewhere that is not a delimiter, so this
  ;; is not something the cursor position alone can fix.  Not prompting
  ;; leaves the delimiter the reader types next loose, exactly as the
  ;; auto-detect does, so the repeat gets the same one-press protection
  ;; once it has marked; see `donkey--suppress-one-pair-delimiter'.
  ;;
  ;; And repeating agrees with counting, the way it does for the other
  ;; mark commands: both walk outward from the same anchor.
  (let* ((state (and (donkey--mark-extending-p) donkey--mark-pair-state))
         (anchor (if state (nth 0 state) (point)))
         (spec (if state
                   (cdr state)
                 (donkey--mark-pair-read-delimiter)))
         (open-char (nth 0 spec))
         (close-char (nth 1 spec))
         (on-opener (nth 2 spec))
         ;; Whether the delimiter typed next is loose: always on a
         ;; repeat, which reads none, and on a fresh press that read it
         ;; from the buffer.  Acted on only once the pair is found.
         (auto (if state t (nth 3 spec)))
         (level (+ (if state (nth 3 spec) 0) (max 1 (or count 1)))))
    (pcase-let* ((`(,start-pos . ,end-pos)
                  (save-excursion
                    (goto-char anchor)
                    (donkey--mark-pair-positions-nth open-char close-char
                                                     on-opener level)))
                 (start (if inner-p (1+ start-pos) start-pos))
                 (end (if inner-p (1- end-pos) end-pos)))
      ;; Refused BEFORE anything is marked, so that a refusal changes
      ;; nothing: point is where the key was pressed, and whatever was
      ;; selected before the press is still selected -- see below.  The
      ;; anchor is not recorded either, a press that marked nothing being
      ;; no press to widen from.
      ;;
      ;; A `user-error': an empty pair is ordinary in code -- `()' for a
      ;; no-argument call, `""' for an empty string -- so pressing `m i'
      ;; on one is a miss, not a malfunction, and a bare `error' popped
      ;; the debugger under `debug-on-error'.  `m a' on the same pair
      ;; still works, since there the delimiters themselves are content.
      (when (>= start end)
        (user-error "Empty selection between %c and %c" open-char close-char))
      ;; Only now, with a pair in hand, is the selection that stood before
      ;; the press let go of.  Every selection command clears the previous
      ;; selection's KIND -- the visual-line anchor, `rectangle-mark-mode'
      ;; -- through `donkey--ensure-non-rectangle-selection' before it
      ;; marks, and this one used to do so before its SEARCH: a `V'
      ;; session or a rectangle followed by an `m i' that found no pair
      ;; kept its highlight and lost its kind, so the `d' that followed
      ;; took the highlighted text as a plain region -- the line's text
      ;; without its newline, the block as one linear span.  A refused
      ;; key changes nothing, the way a refused wrap key keeps its
      ;; selection.  Confirmed live in a terminal frame and a graphical
      ;; one.  `donkey--mark-sexp-select' clears at the same moment.
      (donkey--ensure-non-rectangle-selection)
      ;; Mark at the end, point at the start.  It used to be the other way
      ;; round, which made these the only mark commands to invert the rest;
      ;; `mark-sexp' and DONKEY's own four linear mark commands all finish
      ;; with point at the start of what they selected.
      (push-mark end)
      (goto-char start)
      (activate-mark)
      (setq donkey--mark-pair-state
            (list anchor open-char close-char on-opener level))
      ;; Only a press that marked has a next key to protect -- see
      ;; `donkey--suppress-one-pair-delimiter' for the refused press that
      ;; used to eat one.
      (when auto
        (donkey--suppress-one-pair-delimiter))
      (message (if inner-p
                   "Selected content for '%c'"
                 "Selected OUTER content including '%c'")
               open-char))))

(defun donkey-mark-inner (&optional count)
  "Mark text INSIDE CHAR pairs (excluding delimiters).

The delimiters are `donkey-mark-pair-delimiters', which is where to look
for the pairs in force -- the built-in ones and anything added by
customizing it.  The README lists the defaults in full.

Auto-detects the delimiter when point is on a recognized OPEN or CLOSE
character; otherwise prompts via `read-char'.  EITHER half of a pair
answers that prompt: \\=`m i )\\=' means what \\=`m i (\\=' means, the closer
resolving to its opener the same way point sitting on one always has.

The delimiter key is harmless when the prompt was skipped -- see
`donkey--suppress-one-pair-delimiter', which is there because it was
not.  For asymmetric pairs (e.g. `(' and `)'), nested
occurrences of the SAME pair resolve to the correctly balanced match
-- e.g. the outer `(' of \"(a(b)c)\" selects \"a(b)c\", not just up to
the first `)' found.

This is a plain character scan, not syntax-table aware: unlike
`donkey-mark-sexp-inner', it does not know about strings or comments,
so the delimiter character appearing inside one can still throw off
the match.  Use `donkey-mark-sexp-inner' for balanced expressions in
real code instead.

With no matching pair either way (point outside any delimiter, and no
enclosing pair to fall back on), signals an error and leaves point
exactly where it was -- it never lands somewhere else in the buffer as
a side effect of the failed search.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

Point is left at the START of the selection and the mark at its end,
which is where `mark-sexp' leaves them and where the other DONKEY
mark commands leave them.

COUNT selects how many levels out to go, so a count of 2 from inside a
nested pair marks the pair enclosing it.  Every pair in
`donkey-mark-pair-delimiters' takes one.  For a symmetric delimiter --
one that opens and closes alike, such as a quote -- there is no depth to
count, so a level counts OCCURRENCES outward instead: with point in
\"writing\" in

    \"No use \"writing on paper.\" That\"

a COUNT of 1 marks \"writing on paper.\" and a COUNT of 2 marks the whole
of the outer quotation.  See `donkey--mark-pair-widen-symmetric'."
  (interactive "p")
  (donkey--mark-pair-select t count))

(defun donkey-mark-outer (&optional count)
  "Mark text INCLUDING CHAR pairs (delimiters included).

See `donkey-mark-inner' for delimiter auto-detection, nested-pair
matching, and its syntax-awareness caveat versus `donkey-mark-sexp-outer'
-- all of it applies here identically, just with the delimiters
themselves included in the selection.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

Point is left at the START of the selection and the mark at its end,
which is where `mark-sexp' leaves them and where the other DONKEY
mark commands leave them.

COUNT selects how many levels out to go, so a count of 2 from inside a
nested pair marks the pair enclosing it -- including for symmetric
delimiters, where a level counts occurrences outward.  See
`donkey-mark-inner'."
  (interactive "p")
  (donkey--mark-pair-select nil count))

(defvar-local donkey--mark-sexp-state nil
  "How the last `m I'/`m A' selection was arrived at, for growing it.
A cons (ANCHOR . LEVEL): where the search ran from and how many levels
out it went.  Read only by `donkey--mark-sexp-select' and only when the
same command repeats; see `donkey--mark-pair-state' for why a plain
position is enough.")

(defun donkey--mark-sexp-select (inner-p &optional count)
  "Shared implementation for `donkey-mark-sexp-inner'/`donkey-mark-sexp-outer'.

Uses the syntax table to identify delimiters (parentheses, brackets,
braces).  If point is on an opening or closing delimiter, uses that
pair; if point is inside a pair, finds the enclosing delimiters.

COUNT selects how many levels out to go, so a count of 2 marks the pair
enclosing the one that would be marked without it.  Point already on an
opening delimiter counts as being at that pair, so a count of 1 there
uses it rather than its parent.  Repeating the command goes one level
further out per press, so `m I m I' reaches what `C-u 2 m I' reaches.

With INNER-P non-nil, selects the expression's content, excluding its
delimiters, and errors if that content is empty (e.g. \"()\");
otherwise selects the delimiters too.

Point is left at the START of the selection and the mark at its end,
which is where `mark-sexp' leaves them and where every other DONKEY
mark command does.

The selection standing before the press is cleared only once the
expression has been found, as `donkey--mark-pair-select' does and for
the reason given there: a refused key changes nothing."
  ;; A repeat widens the ORIGINAL search rather than searching afresh
  ;; from where the last one left point -- see `donkey--mark-pair-select'
  ;; for why the two are not the same thing.  `m A' used to appear to
  ;; widen on a second press, but only because point had been left past
  ;; the closing delimiter where a fresh scan happens to find the
  ;; enclosing pair; it was an accident of position, and `m I' -- left
  ;; inside its own content -- re-marked the same expression instead.
  (let* ((state (and (donkey--mark-extending-p) donkey--mark-sexp-state))
         (anchor (if state (car state) (point)))
         (levels (+ (if state (cdr state) 0) (max 1 (or count 1)))))
    ;; Everything up to the last moment happens under `save-excursion',
    ;; so a selection that cannot be made leaves point where it was --
    ;; including on the repeat path, where ANCHOR is somewhere point had
    ;; already moved away from.
    (pcase-let
        ((`(,start . ,end)
          (save-excursion
            (goto-char anchor)
            (condition-case nil
                (backward-up-list (if (looking-at "\\s(") (1- levels) levels))
              (scan-error
               (user-error "Not inside a balanced expression")))
            (let ((start (if inner-p (1+ (point)) (point))) end)
              (condition-case nil
                  (setq end (progn (forward-list 1)
                                   (if inner-p (1- (point)) (point))))
                (scan-error
                 (user-error "Unbalanced expression")))
              (cons start end)))))
      (when (and inner-p (>= start end))
        (user-error "Empty expression"))
      ;; With the expression in hand, and not before -- see
      ;; `donkey--mark-pair-select'.
      (donkey--ensure-non-rectangle-selection)
      ;; Mark at the end, point at the start -- the same reversal as in
      ;; `donkey--mark-pair-select', and for the same reason.
      (push-mark end t)
      (goto-char start)
      (activate-mark)
      (setq donkey--mark-sexp-state (cons anchor levels))
      (message (if inner-p "Marked inner expression" "Marked outer expression")))))

(defun donkey-mark-sexp-inner (&optional count)
  "Mark content inside the balanced expression at point.

Uses the syntax table to identify delimiters (parentheses,
brackets, braces).  If point is on an opening or closing
delimiter, marks content within that pair.  If point is inside
a pair, finds the enclosing delimiters and marks everything
within, excluding the delimiters themselves.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

Point is left at the START of the selection and the mark at its end,
which is where `mark-sexp' leaves them and where the other DONKEY
mark commands leave them.

COUNT selects how many levels out to go."
  (interactive "p")
  (donkey--mark-sexp-select t count))

(defun donkey-mark-sexp-outer (&optional count)
  "Mark the balanced expression at point, including delimiters.

Uses the syntax table to identify delimiters (parentheses,
brackets, braces).  If point is on a delimiter, marks that
pair.  If point is inside a pair, finds the enclosing pair
and marks it including delimiters.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

Point is left at the START of the selection and the mark at its end,
which is where `mark-sexp' leaves them and where the other DONKEY
mark commands leave them.

COUNT selects how many levels out to go."
  (interactive "p")
  (donkey--mark-sexp-select nil count))

(defun donkey--back-to-symbol-char ()
  "Move point back onto the last symbol character before it.

`donkey-mark-symbol' used `backward-sexp' to normalize point onto the
symbol behind, and landed wherever a SEXP starts, which is not where a
symbol starts.  Reported from a key sequence quoted in prose, of the
shape `substitute-command-keys' renders a binding as -- curly quotes and
all.  With point in the gap between its two halves that landing was the
opening quote, punctuation with no symbol at point, and the mark was
REFUSED although a symbol sat directly behind.  The same landing sent a
press behind #\\='bar back to the symbol before it.

Punctuation, quotes, and brackets are all crossed to reach the symbol,
because none of them can be part of one; the search is for a character
of word or symbol syntax and stops on the first.

`skip-syntax-backward' stops in exactly two places here, at the first
such character or at the start of the buffer, so a landing past
`point-min' IS the character and needs no second test for it.  One was
written and mutation testing showed it changed nothing.

Nothing moves when there is no such character behind point at all, and
the return value says which: non-nil when point moved onto a symbol
character, nil when it stayed.  `donkey--mark-reach-from-gap' reads it
as the answer to whether the gap has a symbol behind it, and reaches
FORWARD instead when it has not -- the leading gap of a buffer is that
case.

The guard is a rule about this function rather than about what the user
sees.  `donkey-mark-symbol' puts point back before it reports a refusal,
so leaving the cursor at `point-min' here would be invisible through the
command -- checked, across five modes and every position of fourteen
buffers, and nothing differed.  It is written anyway, because the restore
belongs to the refusal and this belongs to the search: a step that only
moves when it has found something cannot strand the cursor even if the
restore is one day taken out.  The suite holds that contract by calling
this function directly, for the same reason."
  (let ((landing (save-excursion (skip-syntax-backward "^w_") (point))))
    (when (> landing (point-min))
      (goto-char (1- landing)))))

(defun donkey--trim-symbol-prefix ()
  "Move point forward over punctuation stuck to the front of the symbol ahead.

The mirror of `donkey--trim-symbol-punctuation', called with point at
the START of a backward symbol run.  `backward-sexp' stops where a sexp
starts rather than where a symbol does, and scanning sweeps adjacent
punctuation into the sexp, so a mark of a name quoted in prose came out
holding the opening quote as well.

Only PUNCTUATION syntax is shed.  An expression prefix -- \\=' and \\=`
and # in Lisp -- introduces the form after it and belongs with it, so
marking \\='bar still gives \\='bar; punctuation belongs to no symbol in
any mode.  The distinction is the mode's own syntax table rather than a
list of characters here, which is why the same rule serves prose and
code: the curly quotes are punctuation in `text-mode' and
`emacs-lisp-mode' alike, and in `help-mode', where they are brackets
instead, sexp scanning already stopped in the right place and this finds
nothing to do.

The ceiling is the end of the sexp just traversed, so the rule reads the
same as at the other end: never trim away the thing that was just added.
A symbol that IS punctuation keeps itself that way."
  (let ((start (point))
        (sexp-end (save-excursion
                    (condition-case nil
                        (forward-sexp 1)
                      (scan-error nil))
                    (point))))
    (while (and (char-after)
                (eq (char-syntax (char-after)) ?.))
      (forward-char 1))
    (when (>= (point) sexp-end)
      (goto-char start))))

(defun donkey--trim-symbol-punctuation ()
  "Move point back over trailing punctuation on the symbol just passed.

Called with point at the END of a forward symbol run, where a trailing
\".\" or \",\" is prose punctuation rather than part of the name --
`donkey-mark-symbol' drops it so that marking the symbol in \"see
\\=`foo\\=', bar.\" gives \"bar\" rather than \"bar.\".

Anything of punctuation SYNTAX goes the same way, which is what carries
the rule into modes whose punctuation is not those two characters.  The
closing curly quote around a name quoted in prose is punctuation in
`text-mode' and `emacs-lisp-mode' alike, and marking such a name
gave it with that quote still attached until this counted it.  The two
literal characters stay named because Lisp gives \".\" symbol syntax and
\",\" the syntax of an expression prefix, so neither is punctuation in
the mode this package is written in.

`donkey--trim-symbol-prefix' is the mirror at the other end, and the
pair of them is why the command can promise a symbol rather than a sexp.

Stops short when the symbol IS that punctuation.  In Lisp `.' has
symbol syntax, so \"...\" is a symbol in its own right, and trimming it
left nothing: `m W' on a buffer of \"...\" produced an EMPTY region
and announced a successful mark.  On the extend path the same trim ate
the symbol but not the space before it, so a second press over
\"a ...\" grew the selection from \"a\" to \"a \" -- a trailing
space where the symbol should have been.  Both leave the whole symbol
alone now.

The floor is the start of the sexp just traversed rather than the start
of the whole selection, so the rule reads the same on a fresh mark and on
an extension: never trim away the thing that was just added."
  (let ((end (point))
        (sexp-start (save-excursion
                      (condition-case nil
                          (backward-sexp 1)
                        (scan-error nil))
                      (point))))
    (while (and (char-before)
                (or (memq (char-before) '(?, ?.))
                    (eq (char-syntax (char-before)) ?.)))
      (backward-char 1))
    (when (<= (point) sexp-start)
      (goto-char end))))

(defun donkey--real-thing-at-point (thing)
  "Return the THING at point, unless nothing in it is really a THING.

`thing-at-point' reports the WHOLE BUFFER as the `word' at point when
the buffer holds no word character anywhere: \"...\", \"!!!\" and
\"()\" each answer with themselves.  So the guard meant to reject a
buffer with no word in it accepted one instead, and `donkey-mark-word'
selected the entire buffer while reporting \"Word marked\" -- after
which \\[donkey-delete] emptied it.  Confirmed in `fundamental-mode',
`text-mode' and `emacs-lisp-mode' alike, so it is not the major
mode\\='s syntax table deciding.  A buffer of pure whitespace answers nil,
which is why the hole shows only with non-word text that is not blank
either.

Checked by looking for a character of the right syntax inside what came
back, so a real word or symbol passes through unchanged.  `symbol'
does not need the treatment -- it answers nil unless the characters
really do have symbol syntax, which is why \"+++\" is a symbol and
marking it is correct -- but it is asked the same way here so the two
commands cannot drift apart."
  (let ((found (thing-at-point thing)))
    (and found
         (string-match-p (if (eq thing 'word) "\\w" "\\w\\|\\s_") found)
         found)))

(defun donkey--object-count (count)
  "Return COUNT as the number of objects for a forward mark key.

Nil and zero are one; anything else is itself.  `donkey-mark-word',
`donkey-mark-symbol' and `donkey-mark-paragraph' read their COUNT
through this, so the three agree, and `donkey-mark-sentence' reads
every count below one as one for a reason of its own -- see there.

Zero used to mark NOTHING, on the argument that `mark-word' and
`forward-paragraph' read zero that way.  What that gave the user was
an empty active region under \"Word marked\": the one press of a key
that says \"word\" which left no word selected, and a selection with
nothing in it for the action keys to refuse.  Changed on request: a
count of zero is a bare press.  A NEGATIVE count keeps its meaning,
the objects behind the one point normalizes onto."
  (let ((n (or count 1)))
    (if (zerop n) 1 n)))

(defvar donkey--mark-reach 'ahead
  "Which neighbor a fresh mark press takes from a gap: `ahead' or `behind'.

Point ON an object marks that object, whichever mark key is pressed.
Point in the gap between two -- on whitespace or punctuation, or on a
blank line between paragraphs -- has a choice, and the choice is the
key's: `donkey-mark-word', `donkey-mark-symbol', `donkey-mark-sentence'
and `donkey-mark-paragraph' take the object AHEAD, and their backward
partners the one BEHIND, so that from the space between two words
`m w' and `m b' are the two words on either side of it, and `M' answers
as `m w' does.

The eight used to agree on the object behind.  The report that changed
it was `m w' and `m b' pressed from the same space and marking the same
word, with `M' marking it too -- a forward key reaching backward.  A key
that names a direction reaches that way.

Where the preferred side has nothing, the other answers: the trailing
gap of a buffer gives a forward key the last object, and the leading gap
gives a backward key the first, so no gap in a buffer that holds an
object at all is refused.  A buffer with no object anywhere still is.

Bound to `behind' by `donkey--mark-backward' around the DELEGATE call a
fresh backward press makes; the forward commands read it, and it is
`ahead' everywhere else.  A variable rather than a parameter because the
four forward commands are the user's keys, whose one argument is the
COUNT, and this is not something a keypress can ask for.")

(defun donkey--text-before-p (position)
  "Return non-nil if anything but whitespace lies before POSITION.

Whether a gap has an object behind it at all.  `donkey-mark-sentence'
and `donkey-mark-paragraph' ask it before stepping back from a gap for
a backward press: in the LEADING gap of a buffer there is nothing
behind, and the step back would land on the buffer's start and drag the
leading whitespace into the selection, so the object ahead is taken
instead -- see `donkey--mark-reach'.  The newline is named because
`[:space:]' is whitespace SYNTAX, which a newline has not got in every
major mode."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "[^[:space:]\n]" position t)))

(defun donkey--mark-reach-from-gap (ahead behind)
  "Move point from a gap onto the object a fresh press takes there.

AHEAD and BEHIND each move point onto the nearest object on their own
side and return non-nil when there is one; where there is none they may
leave point anywhere, since it is put back here.  The side
`donkey--mark-reach' names is tried first and the other when that side
is empty.  Returns nil with point where it started when both are, so the
caller can report without having moved the cursor -- see
`donkey-mark-word' for the press that made that matter.

The helper this replaces reached forward only, and only as the
fallback: every gap gave the object BEHIND then, and the leading gap of
a buffer, with nothing behind, was the one place a reach forward was
needed.  Reaching forward is the rule now and the step back the
fallback, for the forward keys; for the backward keys the two trade
places."
  (let ((origin (point))
        (behind-first (eq donkey--mark-reach 'behind)))
    (or (funcall (if behind-first behind ahead))
        (progn (goto-char origin)
               (funcall (if behind-first ahead behind)))
        (progn (goto-char origin)
               nil))))

(defun donkey--point-on-word-or-symbol-char-p ()
  "Return non-nil if the character after point has word or symbol syntax.

Used by `donkey-mark-word'/`donkey-mark-symbol' to decide whether
point already sits inside a word/symbol -- in which case there is
nothing to reach for -- or is in a gap and needs to move onto the one
ahead or behind first, see `donkey--mark-reach-from-gap'."
  (and (char-after)
       (member (char-syntax (char-after)) '(?\w ?_))))

;; One rule for the whole mark family, rather than four.
;;
;; `m s' grew on a second press and the others did not, which read as a
;; decision and was not one.  `mark-end-of-sentence' has no ALLOW-EXTEND
;; parameter -- it always extends -- while `mark-word', `mark-paragraph'
;; and `mark-sexp' take one that is non-nil only when Emacs calls them
;; interactively.  Reached from Lisp with a single argument, as these
;; commands did, the extension is simply switched off.  So DONKEY was
;; not adding the behavior to `m s'; it was removing it from the rest.
;;
;; Native's own test is wider than this one:
;;
;;   (or (and (eq last-command this-command) (mark t))
;;       (and transient-mark-mode mark-active))
;;
;; -- the second arm extends ANY active region, so `v' and a few motions
;; followed by `m w' would grow that selection instead of marking a
;; word.  That is a change to a flow nobody asked about, and it is not
;; what `m s' does today: `m s l m s' starts over.  Matching `m s'
;; exactly keeps the family uniform without disturbing `v'.
(defconst donkey--mark-run-adjusters
  '(donkey-mark-run-left donkey-mark-run-right
    donkey-mark-run-down donkey-mark-run-up
    donkey-mark-run-line-start donkey-mark-run-line-end
    donkey-mark-run-buffer-start donkey-mark-run-buffer-end
    donkey-mark-run-exchange
    donkey-mark-run-step-back donkey-mark-run-step-forward)
  "The mark run keys that adjust a selection instead of marking one.

\`h' \`j' \`k' \`l' move point, `g h' and `g l' stretch an end to the
line's edge, `g g' and `g e' stretch one to the buffer's, \`*' trades
which end is which, and \`u' and \`U' walk the run back and forward
through its own steps.  None of them names an object, and
that is the whole of what they have in common -- the
list was called the mode's \"motions\" while the line-edge pair still
was one, and had to explain itself once the pair grew fixed ends.

Members of `donkey--mark-run-commands', so a run carries on across
them -- but `donkey--mark-extending-p' holds them to a stricter test
than the object commands: a motion continues only a VISIBLE run.  An
object key may revive a region some hook deactivated mid-run; a
motion member with no active region is just the cursor having moved,
and the mark it finds next to `last-command' could be anything --
without the distinction, `M l w' beside a stale mark grew a surprise
selection from wherever that mark lay.")

(defconst donkey--mark-run-inert-commands
  '(undefined ignore donkey-mark-run-refuse)
  "The commands that change nothing, so a mark run survives them.

Every printable key the normal state leaves unbound resolves to
`undefined' through `suppress-keymap's remap of `self-insert-command',
and \`DEL' to `ignore'.  Both used to end a run and lapse the mode, so
a mistyped \`0' mid-run threw the selection away and rang the bell
about it.  A press that changes nothing should change nothing: listed
here, they satisfy `donkey--mark-run-mode-keep-p' so the mode stays,
and they count as companions in `donkey--mark-extending-p' so the next
object key still EXTENDS rather than marking afresh -- the typo costs
a beep and nothing else.

`donkey-mark-run-refuse' is here for the same reason from the other
direction: it exists to leave a run standing, so it must not be the
thing that breaks it.

Membership is why they are appended to `donkey--mark-run-commands'
rather than tested separately: that list IS the companion set, and a
command transparent to the mode has to be transparent to the run.")

(defconst donkey--mark-run-commands
  (append
   '(donkey-mark-word donkey-mark-word-backward
     donkey-mark-symbol donkey-mark-symbol-backward
     donkey-mark-sentence donkey-mark-sentence-backward
     donkey-mark-paragraph donkey-mark-paragraph-backward
     donkey-mark-run-line-forward donkey-mark-run-line-backward
     donkey-mark-whole-buffer donkey-mark-run-adopt)
   donkey--mark-run-adjusters
   donkey--mark-run-inert-commands)
  "The commands that grow ONE selection between them -- the mark run family.

Membership is what makes a press CONTINUE the current mark run rather
than start a fresh selection -- see `donkey--mark-extending-p'.  The
eight share one selection shape, mark at the forward end and point at
the start, so any member can grow either end of any run: a forward key
pushes the mark ahead by its own object, a backward key walks point
back by its own, and no two ever contend over an end.

The eight object commands are also exactly what
`donkey-mark-run-mode-map' offers without their `m' prefix, and the
whole list is what `donkey--mark-run-mode-keep-p' holds mark run mode
open for.  The `donkey--mark-run-adjusters' tail is included so that
adjusting point mid-run -- possible only inside the mode, where alone
those commands are bound -- reads as the run continuing, the way
`j'/`k' keep a visual-line session; the PLAIN motions stay out, so
any of them still ends a run, prefix spelling and mode alike.
`donkey-mark-run-toggle' itself is NOT a member, and needs no
membership: a press that marked a word or adopted a selection renames
itself in `this-command' to the member that did it, so the next key
grows what it left, and the press that found neither keeps its own
name, so that a mark left over from an older selection cannot qualify
the first letter after `M' as a continuation -- see its docstring.
`donkey-mark-run-adopt' IS one: an adopting press hands the next key a
live selection, and membership is what makes that key grow it.

`donkey-mark-whole-buffer' is a member without being a growable
object: \`%' replaces the selection rather than adding to it, and no
press after it can grow what already covers everything.  It is here
because it MARKS -- a mark mode has no business lapsing on a mark
command -- and membership is also what gets the press recorded, so \`u'
takes it back.  It was the last of the keys that changed a run and
left, and refusing it would have been the wrong answer to a key doing
exactly what it says.

The delimiter marks (`donkey-mark-inner' and friends) are not members:
they count LEVELS, not objects, and one level out is not one more of
anything a run could add.  `donkey-rectangle-mark-mode' is not a member
because a rectangle has no forward end in this sense.  And a `v'
selection is not grown by any of these: the test suite pins that a
region made some other way is left alone, so `v' keeps working the way
it reads.")

(defun donkey--mark-extending-p (&optional companions)
  "Return non-nil when a mark command should grow its selection.

True when the command now running is the one that ran last and it left
a mark behind -- the same test `mark-end-of-sentence' applies, which is
why `m s' has grown on a second press since before there was a rule.
Any other key in between ends the run.

COMPANIONS names the commands that continue this command's run without
being it; the eight object mark commands each pass
`donkey--mark-run-commands', so a run crosses OBJECT TYPES freely --
`m w m s' is the word grown forward to the end of its sentence, `m s
m b' the sentence plus the word before it.  Each press adds one object
of its own kind at its own end, which is well defined because every
member leaves the same selection shape behind.  Runs were once
confined to a forward/backward pair per object, on the argument that
\"the symbol before the current WORD selection\" is not a length the
word run promised -- but the family shape makes the meaning plain, and
the pair rule made `m w m s' silently discard a selection instead.

`this-command' is checked for being set at all before it is compared.
Outside the command loop BOTH it and `last-command' are nil, so the
comparison alone is true, and any Lisp caller with a mark already set
got an extension where it asked for a fresh selection.  Reached by
`donkey-mark-paragraph' with a stale `rectangle-mark-mode' mark: the
extension grew from that mark instead of marking the paragraph under
point, so a following `donkey-delete' took the wrong text.  It hid
behind test order -- any earlier test leaves `last-command' non-nil,
which makes the comparison false again, so it only showed when the
marking tests ran first."
  (and this-command
       (or (eq last-command this-command)
           (memq last-command companions))
       (mark t)
       ;; A motion member continues only a LIVE run.  The object
       ;; members may revive a region a hook deactivated mid-run -- the
       ;; run was theirs -- but a motion with no live mark is just the
       ;; cursor having moved, and the mark next to it could be
       ;; anything: without this, `M l w' beside a stale mark grew a
       ;; selection from wherever that mark lay instead of marking the
       ;; word at point.
       ;;
       ;; `mark-active', not `region-active-p'.  The question is whether
       ;; a selection is live, and `region-active-p' answers a different
       ;; one -- it also demands `transient-mark-mode', which is on by
       ;; default but need not be.  With it off the object keys grew
       ;; runs as they always do while this test refused every one that
       ;; had passed through a motion, so `M l w' re-marked one word
       ;; where `M l w' with the mode on takes \"hat is\".  Half a mode,
       ;; and by accident: `donkey-rectangle-mark-mode' asks
       ;; `mark-active' for the same reason and says so.  Deactivating
       ;; clears it either way, so the stale-mark protection stands.
       (or (not (memq last-command donkey--mark-run-adjusters))
           mark-active)
       t))

(defun donkey--normalize-mark-run ()
  "Put point back at the selection's start and the mark at its far end.

The layout every object mark command relies on: point at the start,
mark at the forward end, so the forward keys grow a run by pushing the
mark and the backward keys by walking point.  Three things break it,
all of them deliberately, and all of them leaving the next object key
holding the selection by the wrong ends:

\`*' trades the ends so the motions adjust the other one, which is
what it is for.  A motion may walk point PAST the mark -- the freeform
a `v' region has always had.  And a negative count reaches behind
point, so `C-u -3 m w' finishes with the mark at the START.

In all three the object key that followed did not grow the selection,
it destroyed it: `M * w' pushed the mark forward from the region's
own start and collapsed the selection to nothing, while still
reporting \"Word marked\"; `M * s' replaced it with a span on the
other side of point.  Swapping back first makes every object key mean
the one thing it means everywhere else, and costs the swap only when
there is one to undo -- this is a no-op on a run already laid out the
right way round, which is nearly every press.

Called from the EXTENDING branches only.  A fresh press builds its own
layout, and the motions and \`*' must keep theirs, or trading ends
would trade them straight back.

Every read here is `(mark t)', never `(mark)'.  A run reaches this
with its region DEACTIVATED whenever a hook took the highlight away
mid-way -- the case the extending branches re-assert the mark for --
and plain `mark' refuses to answer for an inactive region unless
`mark-even-if-inactive' is on.  It is on by default, which is why
turning it off was all it took to make three of this file's own tests
signal `mark-inactive' instead of growing the selection."
  (when (and (mark t) (> (point) (mark t)))
    (let ((start (mark t)))
      (set-mark (point))
      (goto-char start))))

(defun donkey--mark-run-continuing-p ()
  "Return non-nil when this press continues a run, squaring it up first.

Every mark run key asks the same two things in the same order: is this
a continuation, and if so are the ends the right way round?  They were
two calls at each of nine sites, and nothing but habit kept them
together -- the `*' bug got in exactly because an object key grew a
run whose ends had been traded and nobody had put them back.  One call
makes the pairing structural: a key that continues a run cannot see it
sideways, and the next key added to the mode inherits that without
being told.

`donkey--mark-extending-p' stays separate and stays pure.
`donkey-mark-inner' and `donkey-mark-sexp-inner' ask it with no
companions and must NOT be squared up: they count levels rather than
objects, and their state is their own."
  (when (donkey--mark-extending-p donkey--mark-run-commands)
    (donkey--normalize-mark-run)
    t))

(defun donkey-mark-run-cancel ()
  "Drop the active selection and let mark run mode lapse.

Bound to \`M' inside `donkey-mark-run-mode-map'.  It is deliberately
no member of
`donkey--mark-run-commands' and no key of the mode map's family row,
so running it fails `donkey--mark-run-mode-keep-p' and the transient
map is gone by the next key.

The message is worded like `donkey-visual-line-toggle's \"Visual
line: canceled\", the pair being the two selection toggles.  Since
adoption arrived, the selection it drops is always a run this mode
grew -- adopted or marked -- so the wording is also simply accurate."
  (interactive)
  (deactivate-mark)
  (message "Mark run: canceled"))

(defun donkey-mark-run-refuse ()
  "Refuse a key that would silently throw the mark run away.

Bound to \`V' and \`v' inside `donkey-mark-run-mode-map'.  Both start
a selection of their own, and neither has an honest reading over a
run.  Pressed mid-run, `donkey-visual-line-toggle' dropped the run and
anchored a fresh line session on whatever line the cursor sat in --
`M V' over a marked word came back holding that word's whole line,
with the run gone and nothing said.  `donkey-set-mark' re-anchors,
which is what \`v' means everywhere else and reads as \"start again
from here\" -- but mid-run it left an empty region at point where a
selection had been, the run gone as quietly.  Rather than guess
between adopting a run and discarding it, the key says which presses
do end one and leaves everything else standing.

Those presses are unchanged: \`M' or \`C-g' drops the selection, and
any key that USES it -- `d', `y', `p', `c', `x' -- takes it and goes.
Only a key that would discard the run without using it is refused.

Allowed by `donkey--mark-run-mode-keep-p', so the refusal leaves the
mode exactly as it found it.  The `user-error' also keeps its own
echo, `post-command-hook' not running after a signal, so the reminder
cannot paint over the complaint."
  (interactive)
  ;; Named after the key that reached it, so a second key bound here
  ;; later reports itself.  Called from Lisp there is no such key --
  ;; `this-command-keys' answers with whatever ran last -- so the
  ;; sentence starts with \"That\" rather than a lie.
  (let ((key (key-description (this-command-keys))))
    (user-error "%s would drop the mark run: leave with M, C-g or an action key"
                (if (or (string-empty-p key) (not (called-interactively-p 'any)))
                    "That"
                  key))))

(defun donkey-mark-run-left (&optional count)
  "Move point back COUNT characters without ending the mark run.

The `donkey--mark-run-adjusters' wrappers exist because the PLAIN
motions must keep ending runs: `m w l m w' marking a single word afresh is
pinned behavior, so `forward-char' and friends cannot join
`donkey--mark-run-commands'.  Bound only inside
`donkey-mark-run-mode-map', these wrappers give the mode what `j'/`k'
give a visual-line session -- point adjusts the selection's near end
freely, and the run carries on -- without changing what any key means
outside the mode."
  (interactive "p")
  (backward-char count))

(defun donkey-mark-run-right (&optional count)
  "Move point forward COUNT characters without ending the mark run.
See `donkey-mark-run-left' for why the mode wraps its motions."
  (interactive "p")
  (forward-char count))

(defun donkey--line-move-last-command ()
  "Return the `last-command' a vertical move needs to keep its column.

`line-move' remembers the column a run of vertical motion started from
only while `last-command' is `next-line' or `previous-line'.  The test
is by NAME, and `donkey-mark-run-down' and `donkey-mark-run-up' are
not those names, so each press through the wrappers reset the memory:
`M j j' down a ragged edge of text held the short line's column
instead of returning to the one it set out from, where a plain `j j'
returns.

Presenting a wrapper press as the motion it stands in for is the whole
fix.  Anything else is passed through untouched, so a first press
after some other command still starts a fresh column -- and the
binding must be a binding, not a `setq': the real `last-command' is
what tells `donkey--mark-extending-p' the run is still live."
  (if (memq last-command '(donkey-mark-run-down donkey-mark-run-up))
      'next-line
    last-command))

(defun donkey-mark-run-down (&optional count)
  "Move point down COUNT lines without ending the mark run.
See `donkey-mark-run-left' for why the mode wraps its motions."
  (interactive "p")
  ;; `next-line' deliberately, not the `forward-line' the byte compiler
  ;; suggests: this wrapper stands in for `j', which IS `next-line', and
  ;; the two must move identically -- goal column, screen lines and all.
  (let ((last-command (donkey--line-move-last-command)))
    (with-suppressed-warnings ((interactive-only next-line))
      (next-line count))))

(defun donkey-mark-run-up (&optional count)
  "Move point up COUNT lines without ending the mark run.
See `donkey-mark-run-left' for why the mode wraps its motions."
  (interactive "p")
  ;; See `donkey-mark-run-down' for why not `forward-line'.
  (let ((last-command (donkey--line-move-last-command)))
    (with-suppressed-warnings ((interactive-only previous-line))
      (previous-line count))))

(defun donkey-mark-run-line-start (&optional count)
  "Stretch the run back to the line start, or move there.
With COUNT, the start of the line COUNT - 1 lines down.  Stands in
for `g h'; see `donkey-mark-run-left' for why the mode wraps its
motions.

The pair owns FIXED ENDS, the way the object keys do rather than the
way \`h' \`j' \`k' \`l' do: this one takes the selection's start,
`donkey-mark-run-line-end' its end.  They were plain motions once,
and both moved POINT, which made them cancel each other -- `M g h'
reached back to the line's start, and the `g l' after it dragged
point across the mark to the line's end and left the beginning
behind, so the pair could never build the whole line.  Owning an end
apiece, they add: `M g h g l' is the line's text from one edge to
the other, in either order.

With no run in progress the key is still just a motion.  Nothing is
lost by that -- `v g l' has always been the way to select to the
line's end from scratch -- and it keeps the pair usable for placing
the cursor before marking whenever the mode holds no run."
  (interactive "p")
  (let ((extending (donkey--mark-run-continuing-p)))
    (beginning-of-line count)
    ;; Moving point activates nothing; the same re-assertion the
    ;; backward object keys make, for the same reason.
    (when extending
      (activate-mark))))

(defun donkey-mark-run-line-end (&optional count)
  "Stretch the run forward to the line end, or move there.
With COUNT, the end of the line COUNT - 1 lines down.  Stands in for
`g l'; see `donkey-mark-run-left' for why the mode wraps its motions,
and `donkey-mark-run-line-start' for why the pair owns fixed ends.

This one pushes the MARK, the forward end, so it cannot shrink what
is selected: the end of a line is never behind the position it is
measured from.  Measured from the MARK rather than from point, which
is what makes it the forward end's key -- on a run already spanning
lines it reaches the end of the line the selection stops on, not the
end of the line the cursor happens to sit in."
  (interactive "p")
  (if (donkey--mark-run-continuing-p)
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (move-end-of-line count)
                  (point)))
    (move-end-of-line count)))


(defun donkey-mark-run-buffer-start (&optional arg)
  "Stretch the run back to the buffer's start, or jump there.

Stands in for `g g'.  The buffer's edges are the line's edges written
large, so the pair works the way `donkey-mark-run-line-start' and
`donkey-mark-run-line-end' do: an end apiece, this one the start, and
they add -- `M g g g e' is the whole buffer from a word in the
middle of it.

A jump is the largest thing a single press can do to a run, which is
why the mode had to grow \`u' before it could offer one.  Left as a
plain motion the press dragged the near end to `point-max' or
`point-min' and lapsed the mode on its way, giving no way back at all;
adopted, it is a press like any other and one \`u' takes it off again.

ARG is passed on when there is no run to stretch, where the key is the
ordinary `beginning-of-buffer' and reads it as that command does --
raw, and so with the interactive spec that command uses.  \"p\" would
turn a bare press into 1, which `beginning-of-buffer' reads as a
tenth of the way in rather than as the start: `M g g' landed on the
second line of a short buffer before the spec was fixed.  A run
reaches the edge, ARG or no ARG: there is no useful sense in which a
continuation goes a tenth of the way there."
  (interactive "P")
  (if (donkey--mark-run-continuing-p)
      (progn
        ;; `goto-char', not the `beginning-of-buffer' the fresh branch
        ;; uses: that command pushes a mark whenever no region is
        ;; active, and a run whose region a hook deactivated mid-way
        ;; arrives here exactly so.  The push put the mark at point and
        ;; the extension came back holding the wrong end -- "one two
        ;; three\n" where the run had reached "one two three\nfour".
        ;; Caught by the test that deactivates the region on purpose.
        (goto-char (point-min))
        ;; Moving point activates nothing; the same re-assertion the
        ;; backward object keys make, for the same reason.
        (activate-mark))
    ;; `beginning-of-buffer' deliberately here: a bare press must stand
    ;; in for `g g' exactly, mark push, screen position and all.
    (with-suppressed-warnings ((interactive-only beginning-of-buffer))
      (beginning-of-buffer arg))))

(defun donkey-mark-run-buffer-end (&optional arg)
  "Stretch the run forward to the buffer's end, or jump there.

Stands in for `g e' and \`G'; see `donkey-mark-run-buffer-start' for
why the pair is adopted rather than left to lapse the mode.

This one pushes the MARK, the forward end, so it cannot shrink what is
selected -- `point-max' is never behind the position it is measured
from.  Unlike `donkey-mark-run-line-end' it needs no measuring at all:
a buffer has one end, wherever the mark happens to sit.

ARG is passed on when there is no run to stretch, raw, for the reason
`donkey-mark-run-buffer-start' gives."
  (interactive "P")
  (if (donkey--mark-run-continuing-p)
      ;; `point-max', not `(point-max)' of the whole buffer: a narrowed
      ;; buffer's end is the end of what is accessible, which is what
      ;; every other key in the mode already respects.
      (set-mark (point-max))
    (with-suppressed-warnings ((interactive-only end-of-buffer))
      (end-of-buffer arg))))

(defun donkey-mark-run-line-forward (&optional count)
  "Mark the current line, or grow the run's forward end by whole lines.

Bound to \`J' inside `donkey-mark-run-mode-map', making lines a
growable object of the mode the way words and sentences are: a fresh
press selects the line point is on, and each further press pushes the
MARK down a line -- the forward end, per the family's rule.  A mark
sitting mid-line -- a word selection's end, say -- first completes its
own line, the way a backward symbol press from mid-symbol first
reaches that symbol's start; the next press adds a whole one.

\`M' arrives holding a word, so inside the mode nearly every press is
a further one: `M J' is the word grown to the end of its line, which
from the line's first word is the whole line and from any other word
is the word onward -- point stays at the word's start, that end being
`donkey-mark-run-line-backward's.  `M J K' is the whole line from
anywhere, in either order, and \`V' then \`M' adopts one whole.  A
fresh press comes only where \`M' found no word to mark.

The mark lands at the START OF THE NEXT LINE rather than at the end of
this one, so the newline that ends the selection is inside it.  That
one character is the difference between a line selection and the text
of a line: without it `M J d' deleted the words and left the blank
line their newline still ended, where `V d' removes the line outright,
and `M J y' produced a kill that the next `p' spliced into whatever
line it landed in.  `donkey-visual-line-toggle' answers the same
problem from the other side -- it keeps the highlight tight and widens
at `y'/`d' through `donkey--visual-line-region-bounds' -- which it can
do because it has an anchor to widen from and this has none.  The
visible difference is that the highlight here reaches the next line's
first column; the deletions and the kills now agree.

COUNT lines; a COUNT below 1 is treated as 1, the reading the
backward keys give theirs -- the other direction is \`K'."
  (interactive "p")
  (let ((n (max 1 (or count 1))))
    (if (donkey--mark-run-continuing-p)
        ;; `forward-line' from the mark does both jobs the old
        ;; end-of-line dance did: from a mark at a line start it adds
        ;; N whole lines, and from one sitting mid-line it completes
        ;; that line first, landing on the next line's start either
        ;; way.  At the end of a buffer with no final newline it
        ;; stops at `point-max', keeping what is selected.
        (set-mark (save-excursion
                    (goto-char (mark t))
                    (forward-line n)
                    (point)))
      (donkey--ensure-non-rectangle-selection)
      (beginning-of-line)
      (push-mark (save-excursion (forward-line n) (point)) t t))
    (message "Line marked")))

(defun donkey-mark-run-line-backward (&optional count)
  "Mark the current line, or grow the run's backward end by whole lines.

Bound to \`K' inside `donkey-mark-run-mode-map'; the other end of
`donkey-mark-run-line-forward's pair.  A fresh press selects the line
point is on; each further press walks POINT up a line, first
completing a partial line the way its partner does at the mark end.
From a word, then, `M K' reaches back to the start of the word's line
and no further -- the completing step -- while from a line's first
word, point sitting at a line start already, it takes the line above
as well; the word's own end stays where it is, that end being the
partner's, so `M K J' is the whole line as `M J K' is.

A fresh press marks the whole line, newline included -- see its
partner for why the character matters.  The extending branch needs no
counterpart: it walks POINT, which sits at a line START already, so
the end the newline belongs to is the one the partner owns.

COUNT lines; a COUNT below 1 is treated as 1."
  (interactive "p")
  (let ((n (max 1 (or count 1))))
    (if (donkey--mark-run-continuing-p)
        (progn
          (if (bolp)
              (forward-line (- n))
            (beginning-of-line)
            (forward-line (- (1- n))))
          (activate-mark))
      (donkey--ensure-non-rectangle-selection)
      (beginning-of-line)
      (push-mark (save-excursion (forward-line 1) (point)) t t)
      (when (> n 1)
        (forward-line (- (1- n)))))
    (message "Line marked")))

(defun donkey-mark-run-exchange ()
  "Swap point and mark, so the motions adjust the run's other end.

Bound to \`*' inside `donkey-mark-run-mode-map', the mode's version
of vi's \`o' in visual mode: growing with `w w w' and trimming with
`l l l' both work the START of the selection, because the motions
move point and point sits there -- this press trades ends, putting
point (and so \`h' \`j' \`k' \`l', `g h', `g l') at the other one.
Press again to trade back.

The OBJECT keys own fixed ends -- mark forward, point backward -- so
a swap is not theirs to honor: they call `donkey--normalize-mark-run'
and trade back before they grow.  `M * w' therefore selects what
`M w' selects.  It used to push the mark forward from the region's
own start and collapse the selection to nothing while still reporting
\"Word marked\", which was the swap's one sharp edge; growing by
objects now reads the same whichever way round the ends are.

A member of `donkey--mark-run-adjusters', so the run carries on and the
visible-run guard applies to what follows.  Refuses without an active
selection: `exchange-point-and-mark' would leap to some stale mark
and re-activate whatever lies between, which is not what a key for
trading the ends of a VISIBLE selection can mean."
  (interactive)
  ;; `mark-active' rather than `region-active-p', so the key works with
  ;; `transient-mark-mode' off -- see `donkey--mark-extending-p'.
  (if (and mark-active (mark t))
      (exchange-point-and-mark)
    (user-error "No selection to swap ends of")))

(defvar donkey-mark-run-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "w" #'donkey-mark-word)
    (keymap-set map "W" #'donkey-mark-symbol)
    (keymap-set map "b" #'donkey-mark-word-backward)
    (keymap-set map "B" #'donkey-mark-symbol-backward)
    (keymap-set map "s" #'donkey-mark-sentence)
    (keymap-set map "S" #'donkey-mark-sentence-backward)
    (keymap-set map "M" #'donkey-mark-run-cancel)
    (keymap-set map "h" #'donkey-mark-run-left)
    (keymap-set map "l" #'donkey-mark-run-right)
    (keymap-set map "j" #'donkey-mark-run-down)
    (keymap-set map "k" #'donkey-mark-run-up)
    (keymap-set map "g h" #'donkey-mark-run-line-start)
    (keymap-set map "g l" #'donkey-mark-run-line-end)
    (keymap-set map "g g" #'donkey-mark-run-buffer-start)
    (keymap-set map "g e" #'donkey-mark-run-buffer-end)
    (keymap-set map "G" #'donkey-mark-run-buffer-end)
    (keymap-set map "J" #'donkey-mark-run-line-forward)
    (keymap-set map "K" #'donkey-mark-run-line-backward)
    (keymap-set map "*" #'donkey-mark-run-exchange)
    (keymap-set map "u" #'donkey-mark-run-step-back)
    (keymap-set map "U" #'donkey-mark-run-step-forward)
    (keymap-set map "." #'repeat)
    (keymap-set map "v" #'donkey-mark-run-refuse)
    (keymap-set map "V" #'donkey-mark-run-refuse)
    map)
  "The keys live during mark run mode -- see `donkey-mark-run-toggle'.

Each letter is bound to the VERY COMMAND its `m'-prefixed key runs,
not to a re-implementation, so the two spellings cannot drift apart:
`M w b' selects exactly what `m w m w m b' selects, the press itself
being `m w'.  \\`h' \\`j' \\`k' \\`l' move point without ending the
run, adjusting the selection's near end the way `j'/`k' adjust a
visual-line session -- through the `donkey--mark-run-adjusters'
wrappers, since the plain motions must keep ending runs everywhere
else.  \`M' inside the mode cancels.

Every other key is missing on purpose.  Pressing one fails
`donkey--mark-run-mode-keep-p', so the transient map lapses and the
key does its ordinary job in the same press -- `M w d' selects two
words and deletes them, with no explicit exit.  Two kinds of press are
exempt.  A key that does nothing -- unbound, or \`DEL' -- leaves the
run alone rather than throwing it away over a typo, which
`donkey--mark-run-mode-keep-p' arranges without a binding here.  And
\`V' IS bound here, to `donkey-mark-run-refuse', because it is the one
key whose ordinary job would discard the run silently.

\`.' is `repeat', as it is in normal state, so `M w .' is three words
and `M w . .' four, a count carrying over -- `M \\[universal-argument] 3 w .'
is seven.  The key always did grow the selection; what it also did
was END THE MODE, because `repeat' runs the previous command by name
and the keep test judged the key by its own name, `repeat', rather
than by the command it stood for.  The run then stood with nothing
behind it: the next \`w' MOVED, dragging the highlight along, and
\`u' undid a text edit inside the region.
`donkey--mark-run-press-command' is what sees through the key now, for
`donkey--mark-run-mode-keep-p' and for the history alike, so each
\`.' is one more press and one more step for \`u' to take back.
Bound here rather than left to the normal map so that the key is the
mode's own -- listed with the rest, and not dependent on what \`.'
means outside.  `M .' repeats the toggle itself, which takes the
selection up again as if freshly pressed, with no steps behind it:
`M' is not `w', and the key that ran was `M'.

\`p' and \`P' are missing DELIBERATELY, though their objects belong
to the family.  Holding them here shadowed the two paste keys, and
the mode has no other way to reach them: `d', `y', `x' and `c' all
act on a mark run selection, so replacing one with the kill ring was
the single ordinary edit the mode made unreachable -- `M p' grew
the selection to its paragraph and pasted nothing.  Paragraphs are
one keystroke away either way, and pasting was none.

\`m' is missing too, and that one is what makes the trade cheap: it
still reaches the normal map's prefix, so `m w' inside the mode runs
`donkey-mark-word' -- the same command the bare `w' here runs -- and
both the keep test and the family test are about COMMANDS, so the
mode survives the press and the run grows.  `M m w w' selects three
words, and `m p' and `m P' grow a run by paragraphs from inside the
mode exactly as the bare letters used to.")

(defvar donkey--mark-run-mode-hint
  "Mark run: w/b words, W/B symbols, s/S sentences, m p/m P paragraphs, * other end, M to cancel"
  "The echo-area reminder shown while mark run mode is active.

Styled after `donkey-visual-line-toggle's message, and kept VISIBLE
for the whole mode by `donkey--mark-run-mode-post-command' -- a single
flash at entry disappeared under the first \"Word marked\".

It names the keys whose SUBJECT the mode changes, and no others.
\`w' moves by a word in normal state and marks one here, and nobody
could guess that from the key -- so the object keys are spelled out.
A key that keeps its subject is not: \`h' \`j' \`k' \`l' still move,
\`J' and \`K' still work on lines, \`.' still repeats, \`u' and \`U'
still step back and forward, of the run rather than the buffer, which
is the same idea one level down.  Naming those spent the line on the
keys least in need of it, and the line is what a reader has to take in
at a glance.

Paragraphs keep their \`m' prefix -- \`p' and \`P' pass through to the
paste commands here -- so they are the one entry the reminder has to
spell in full.  The complete list, the motions and the jumps and the
step keys included, is in the README and the tutor.")

(defvar donkey--mark-run-history nil
  "The run's earlier shapes, newest first, for \`u' to step back to.

Each entry is (POINT MARK ACTIVE), the selection as it stood BEFORE
one press changed it.  Pushed by `donkey--mark-run-mode-pre-command',
popped by `donkey-mark-run-step-back', and emptied whenever the mode is
disarmed, a run's history meaning nothing to the next run.

A run is otherwise one-way.  Every object key GROWS -- `b' after `w'
adds a word at the other end rather than taking one back, which is the
fixed-ends rule and worth keeping -- so before this there was no way
to take a press back at all: `m p' over a long paragraph, a count with
a digit too many, or a reach that simply went further than it looked,
left cancelling and starting again as the only way out.  Small steps
are cheap to redo and large ones are not, and the mode cannot tell in
advance which a press will be.")

(defvar donkey--mark-run-redo nil
  "The shapes \`u' has stepped back out of, newest first, for \`U'.

The other half of `donkey--mark-run-history', and the ordinary redo
bargain: `donkey-mark-run-step-back' pushes what it is leaving here
before it restores, `donkey-mark-run-step-forward' pops it and hands
it back, and any OTHER press in the run empties it, a new branch
having nothing to redo onto.  Emptied with the history whenever the
mode is disarmed.

\`U' is `undo-redo' in normal state, and inside a run it did exactly
that: with a selection live and \`u' meaning the run rather than the
buffer, the natural next press REDID A TEXT EDIT, dropped the
selection and lapsed the mode, all without saying so.  Whatever the
mode did with the key, it could not keep meaning that.")

(defvar donkey--mark-run-armed-in-macro nil
  "Non-nil when mark run mode was armed from inside a keyboard macro.

Read by `donkey--mark-run-mode-post-command' to end the mode when the
macro that armed it is over -- see there for why `this-command' cannot
be asked instead.  Set at entry from `executing-kbd-macro' and cleared
by `donkey--mark-run-exit', so a mode armed by a live keypress carries
nil and is never touched by the rule.")

(defun donkey--mark-run-press-command ()
  "Return the command the press now starting stands for.

`this-command', except for \`.': that key runs `repeat', and `repeat'
runs whatever command came before it -- `last-repeatable-command',
which the command loop takes from `real-this-command' -- so the command
the press stands for is that one.  `donkey--mark-run-mode-keep-p' and
`donkey--mark-run-mode-pre-command' both ask this rather than
`this-command', so that `M w .' is judged and recorded as the \`w' it
repeats: the mode stays, and the press is one more step for \`u'.

Both hooks run BEFORE `repeat' does.  Once it runs, `repeat' sets
`this-command' to the command it repeats, which is why
`donkey--mark-run-mode-post-command' needs no help; but the keep test
and the history are asked on `pre-command-hook', where `this-command'
still names the key's own binding.

A second \`.' arrives with `last-repeatable-command' set to `repeat'
itself -- the previous press's `real-this-command' was the repeat, not
the command it ran -- and `repeat' keeps the command it last repeated
in `repeat-previous-repeated-command' for exactly this, reading it back
on the way in.  The same substitution here is what makes `M w . .'
four words with three steps behind it.  `bound-and-true-p', because
the variable belongs to repeat.el, which is loaded the first time the
key runs and not before; it is only ever consulted after that.

Nil when there is nothing to repeat, which the keep test treats as
inert: `repeat' will say \"There is nothing to repeat\" and change
nothing, so the run is left standing as over any other key that does
nothing.

One case is not seen through.  With `repeat-message-function' set,
`repeat' binds the key to a closure of its own for the presses that
follow the first, and a closure has no name to look up: the second
\`.' then ends the mode, as every \`.' did before.  The variable is
nil by default and nothing in this package sets it."
  (if (eq this-command 'repeat)
      (if (eq last-repeatable-command 'repeat)
          (bound-and-true-p repeat-previous-repeated-command)
        last-repeatable-command)
    this-command))

(defun donkey--mark-run-mode-pre-command ()
  "Record the run's shape before a press that is about to change it.

On `pre-command-hook' for the life of the mode, that being the only
moment at which the state before a press is still the state.  Records
for the family presses alone: the inert commands change nothing, so
stepping back to what they left would spend a press on nothing, and
neither `donkey-mark-run-step-back' nor `donkey-mark-run-step-forward'
may record, each keeping the other's stack and neither able to make
progress against its own.

A \`.' is recorded as the command it repeats, which
`donkey--mark-run-press-command' names: the press still reads
`repeat' here, `repeat' renaming `this-command' only once it runs, so
recording by `this-command' skipped every \`.', and \`u' after
`M w . .' stepped back past all three words at once.

It also names the nameless press -- see the comment below -- which is
the one thing here that is not about the history.

Guarded, not signaling: a function that errors on `pre-command-hook'
is silently removed for the session."
  ;; Name the nameless press.  A key sequence that resolves to nothing
  ;; runs no command at all and arrives with `this-command' nil, where
  ;; a single unbound KEY runs `undefined' -- one accident with two
  ;; spellings, and the difference showed twice.  The mode ended, which
  ;; `donkey--mark-run-mode-keep-p' now answers for; and the command
  ;; loop wrote that nil into `last-command', so the next object key
  ;; found no run to continue and marked afresh, quietly throwing away
  ;; what the run had grown.  Calling it what the other spelling is
  ;; called settles both: `undefined' is already a family member, so
  ;; the run carries across the beep exactly as it does across \`~'.
  ;;
  ;; Done here rather than in the keep predicate because that one runs
  ;; FIRST -- `set-transient-map' adds its hook after this one, and
  ;; `add-hook' prepends -- so it sees the nil and needs its own answer
  ;; to it.  This is about what the press leaves behind.
  (when (null this-command)
    (setq this-command 'undefined))
  (let ((command (donkey--mark-run-press-command)))
    (when (and (memq command donkey--mark-run-commands)
               (not (memq command donkey--mark-run-inert-commands))
               (not (memq command '(donkey-mark-run-step-back
                                    donkey-mark-run-step-forward))))
      (push (list (point) (mark t) (and mark-active t))
            donkey--mark-run-history)
      ;; A step off the path is a new branch, and there is nothing to
      ;; redo onto it -- the bargain every undo system strikes.
      (setq donkey--mark-run-redo nil))))

(defun donkey-mark-run-step-back ()
  "Put the run back where the last press found it.

Bound to \`u' inside `donkey-mark-run-mode-map'.  One press, one step:
`M w w s' and three of these is the first word again, a fourth
reporting rather than guessing.  Every press the mode counts as its
own steps back this way, the motions and \`*' included -- a simpler
rule to hold than one that undid the object keys only.

The key is free.  Inside a run \`u' otherwise reaches `undo', which
sees an active region, tries a region undo and reports \"No further
undo information for region\": one of the three keys in normal state
that fail against a live run and leave it standing.  Nothing anyone
uses is displaced.

What it leaves is kept, so `donkey-mark-run-step-forward' on \`U' can
hand it back -- until any other press in the run drops the redo, a new
branch having nothing to redo onto.

Point, the mark and whether the mark was active are all restored by
`donkey--mark-run-restore'.  A member of `donkey--mark-run-commands',
so the run carries on: `M w w u w' grows from the restored selection
instead of marking afresh."
  (interactive)
  (unless donkey--mark-run-history
    (user-error "No earlier step in this run"))
  (push (list (point) (mark t) (and mark-active t)) donkey--mark-run-redo)
  (donkey--mark-run-restore (pop donkey--mark-run-history)))

(defun donkey-mark-run-step-forward ()
  "Put the run back where \\`u' stepped it out of.

Bound to \\`U' inside `donkey-mark-run-mode-map', where it is the
other half of `donkey-mark-run-step-back': one press, one step, and
\\`M' \\`w' \\`w' \\`u' \\`u' \\`U' \\`U' is the three words again.  A press
that is not one of the two ends the redo, a new branch having nothing
to redo onto, and this reports rather than guessing when there is
nothing left.

The pair matters more than the redo does.  Outside the mode \\`u' and
\\`U' are `undo' and `undo-redo'; inside it \\`u' had been taken for
the run while \\`U' still meant the buffer, so the press anyone would
reach for after \\`u' redid a TEXT EDIT, dropped the selection and
lapsed the mode without a word.  Keeping the two keys on one subject
is what fixes that; that the subject is now the run rather than the
buffer is the same idea one level down.

A member of `donkey--mark-run-commands' like its sibling, so the run
carries on and the next object key grows what came back."
  (interactive)
  (unless donkey--mark-run-redo
    (user-error "No later step in this run"))
  (push (list (point) (mark t) (and mark-active t)) donkey--mark-run-history)
  (donkey--mark-run-restore (pop donkey--mark-run-redo)))

(defun donkey--mark-run-restore (state)
  "Put point, the mark and the mark's activation back to STATE.

STATE is one (POINT MARK ACTIVE) entry as
`donkey--mark-run-mode-pre-command' records them.  Shared by
`donkey-mark-run-step-back' and `donkey-mark-run-step-forward', which
differ only in the stack they take it from and the stack they leave
the current shape on.

Whether the mark was ACTIVE is restored along with the rest, so the
shape before the first recorded press -- the word
`donkey-mark-run-toggle' marks on the way in, or nothing at all --
comes back as it was rather than as a selection it never was."
  (cl-destructuring-bind (pt mk active) state
    (goto-char pt)
    (if (and mk active)
        (set-mark mk)
      (deactivate-mark))))

(defun donkey--mark-run-mode-post-command ()
  "Repaint the mark run reminder, or end a mode that outlived its map.

On `post-command-hook' from mode entry until `donkey--mark-run-exit',
with two jobs.

The reminder is repainted after the mode's family commands and after
nothing else: their own messages \(\"Word marked\" and kin) repeat what
the visible selection already shows, so replacing them costs nothing,
while during count entry the echo area belongs to the keystroke echo.
Count entry is told apart by `prefix-arg' rather than by name, because
the keys of a count carry no name of their own here:
`universal-argument' and `digit-argument' copy `last-command' into
`this-command' \(`prefix-command-preserve-state'), so that the command
they prefix still reads as a repeat of the one before it, and mid-run
each key of \`C-u 3' therefore arrives naming the family member it
followed.  `prefix-arg' is non-nil after exactly those keys and nil
again after the command they were for.  Without the test the reminder
painted over the \`C-u 3-' echo, and the count being typed could not
be seen.

The exit is the transient map's backstop.  `set-transient-map' asks
`donkey--mark-run-mode-keep-p' from `pre-command-hook', which is one
command too late for anything that armed the map while it was already
running: a command whose own key lookup happened long before leaves
the mode armed behind it, and the next bare `w' the user typed marked
a word instead of moving.  A command that is neither a family member,
nor part of entering a count, nor `donkey-mark-run-toggle' itself --
the arming press when it found nothing to mark or adopt, the one case
in which it keeps its own name -- ends the mode here.

A REPLAYED MACRO needs the extra arm above, because
`this-command' cannot see it.  `kmacro-call-macro' and
`kmacro-end-and-call-macro' deliberately leave `this-command' set to
the macro's LAST command so that `last-command' chaining and the
repeat key keep working; a macro ending on \`M' and a letter therefore
reaches this hook with `this-command' naming a family member, and the
test below reads it as an ordinary press of the mode's own key.
`executing-kbd-macro' is the honest signal: the mode was armed while a
macro ran, and the first command to finish outside one is the macro's
own caller.  Noted at entry in `donkey--mark-run-armed-in-macro'.
\(A command that merely calls `execute-kbd-macro' comes back with
`this-command' nil and would have been caught anyway; kmacro is the
one that restores it.)

Guarded, not signaling: a function that errors on `post-command-hook'
is silently removed for the session."
  (cond
   ((and donkey--mark-run-armed-in-macro (not executing-kbd-macro))
    (donkey--mark-run-exit))
   ((memq this-command donkey--mark-run-commands)
    ;; A count's keys arrive under the family member's name -- see
    ;; the docstring -- and the echo area is theirs while one is
    ;; being typed.
    (unless prefix-arg
      (donkey--repaint-hint donkey--mark-run-mode-hint)))
   ((or (donkey--mark-run-mode-keep-p)
        (eq this-command 'donkey-mark-run-toggle))
    nil)
   (t
    (donkey--mark-run-exit))))

(defun donkey--mark-run-mode-keep-p ()
  "Return non-nil while mark run mode should stay active.

The mode lives while the command about to run is a mark run family
member -- which is what the letters of `donkey-mark-run-mode-map'
resolve to -- or part of entering a count, which must not end the mode
or \`C-u 3 w' inside it would fall apart between the \`C-u' and the
\`w'.

A key that DOES NOTHING does not end it either -- see
`donkey--mark-run-inert-commands' for which those are and why a
mistyped key should cost a beep rather than the selection.  A mistyped
SEQUENCE reaches no command at all and is checked separately below,
the two spellings of one accident having arrived here differently.

`donkey-mark-run-refuse' is allowed for the same reason from the other
direction: it exists to leave the run standing, so it must not be the
thing that ends it.  Both arrive through
`donkey--mark-run-inert-commands', which the family list already
appends, so the first test below covers them.

\`.' is judged by the command it repeats, not by `repeat' -- see
`donkey--mark-run-press-command'.  Judged by name it was a foreign
key, and the mode ended on a press that grew the selection."
  (or (memq (donkey--mark-run-press-command) donkey--mark-run-commands)
      ;; A key sequence that resolves to NOTHING never reaches a
      ;; command, and arrives here as nil.  Emacs runs `undefined' for
      ;; a single unbound key -- which is why \`~' was already inert --
      ;; but a sequence that dies in a prefix map is just a beep, and
      ;; `this-command' keeps whatever it held before.  Both are the
      ;; same mistype, and the difference cost the run: \`m x' for
      ;; \`m w' ended the mode silently, and the next letter then moved
      ;; point and left an empty highlight behind it.
      (null this-command)
      (memq this-command '(universal-argument universal-argument-more
                           digit-argument negative-argument))))

(defvar donkey--mark-run-exit-function nil
  "What disarms mark run mode, or nil when the mode is not armed.

`set-transient-map' returns it and `donkey--mark-run-exit' is the only
caller.  Global rather than buffer-local, because the map it takes
down lives in `overriding-terminal-local-map', which is terminal-wide:
a mode entered in one buffer is armed for every buffer on the
terminal until something disarms it.")

(defun donkey--mark-run-exit ()
  "Disarm mark run mode: the transient map and its reminder hook.

One address for leaving, the way `donkey--mark-run-enter' is one
address for arriving.  Reached as the transient map's own ON-EXIT
whichever key lapses it, from `donkey--mark-run-mode-post-command'
when a command outlived the map, and from the test suite between
cases, where a keyboard macro that ends on one of the mode's letters
leaves the map armed with no further command to lapse it.

`donkey--mark-run-exit-function' is cleared BEFORE it is called: the
call runs the map's ON-EXIT, which is this function again, and the nil
is what stops the second pass.  Harmless when the mode is not armed.

Reached on both sides of the foreign command that ends the mode: the
map's ON-EXIT fires from `pre-command-hook', before that command can
message, and the reminder hook fires after, when anything the command
said is already in the echo area.  The clearing test reads correctly
either way."
  (remove-hook 'pre-command-hook #'donkey--mark-run-mode-pre-command)
  (remove-hook 'post-command-hook #'donkey--mark-run-mode-post-command)
  (setq donkey--mark-run-history nil)
  (setq donkey--mark-run-redo nil)
  ;; The reminder is the only sign on screen that the mode is on, so it
  ;; must not outlive it.  A foreign key that neither messages nor
  ;; signals -- `g q', `z z', a recenter -- left the echo area still
  ;; advertising the mark run keys over a selection the mode no longer
  ;; owned, and the next `w' moved instead of growing.  Cleared only
  ;; when the reminder is what is showing: the same no-clobber rule
  ;; `donkey--hint-motions' keeps in the other direction,
  ;; so a command that said something of its own keeps its echo.
  (when (equal (current-message) donkey--mark-run-mode-hint)
    (message nil))
  (setq donkey--mark-run-armed-in-macro nil)
  (let ((exit donkey--mark-run-exit-function))
    (setq donkey--mark-run-exit-function nil)
    (when exit
      (funcall exit))))

(defun donkey--mark-run-enter ()
  "Arm mark run mode: the transient map, the hint hook, the reminder.

One address for entering, whether `donkey-mark-run-toggle' starts from
nothing or `donkey-mark-run-adopt' brings a selection along.  Teardown
is `donkey--mark-run-exit', which the transient map calls as its
ON-EXIT and which the reminder hook calls for the map that outlives
its command.

Re-entry cannot double anything: any previous arming is taken down
first, so one exit function and one hook are all there ever are.

Whether a keyboard macro is running is noted here rather than asked
later: by the time the macro's caller reaches
`donkey--mark-run-mode-post-command', `executing-kbd-macro' has gone
back to nil and the only way to tell an armed-by-macro mode from an
armed-by-keypress one is to have written it down."
  (donkey--mark-run-exit)
  (setq donkey--mark-run-armed-in-macro (and executing-kbd-macro t))
  ;; Belt and braces: the `donkey--mark-run-exit' above has already
  ;; emptied this and `donkey--mark-run-redo' with it, so no test can
  ;; tell the line from its absence.  Left because a reader looking
  ;; for where a run's steps begin should find the answer here.
  (setq donkey--mark-run-history nil)
  (add-hook 'pre-command-hook #'donkey--mark-run-mode-pre-command)
  (add-hook 'post-command-hook #'donkey--mark-run-mode-post-command)
  (setq donkey--mark-run-exit-function
        (set-transient-map donkey-mark-run-mode-map
                           #'donkey--mark-run-mode-keep-p
                           #'donkey--mark-run-exit))
  (message "%s" donkey--mark-run-mode-hint))

(defun donkey--adoptable-selection-p ()
  "Return non-nil when there is a selection worth taking into a run.

Active and not empty.  `donkey-set-mark' activates a mark without
covering anything yet, and `region-active-p' says yes to that: adopting
it made the first object key grow from the CURSOR, so `v M w' from
mid-word took the tail of the word where `M w' takes two whole ones.
Asked by `donkey-mark-run-toggle' to decide whether to adopt and by
`donkey-mark-run-adopt' to refuse when it should not have been called
-- one test, so the two cannot drift into disagreeing about what a
selection is.

`mark-active' rather than `region-active-p', so a selection made with
`transient-mark-mode' off is still one to adopt -- see
`donkey--mark-extending-p'.  `(mark t)' for the reading, since a mark
that is live but not \"active\" in that mode's sense is exactly the
case this exists to handle."
  (and mark-active (mark t) (/= (point) (mark t))))

(defun donkey-mark-run-adopt ()
  "Adopt the active selection into a mark run and enter the mode.

What \`M' does when a selection already exists: a visual-line
session, a `v' region, or a selection the prefixed mark keys built
all carry over, and the object keys grow them from there -- `m w M
w' extends the marked word rather than starting over, and a `V J'
selection keeps its lines when `M' takes it.  Canceling is still one
key away: \`M' again, inside the mode, or \`C-g'.

The region is normalized to the family's layout first -- point at the
start, mark at the forward end -- because a visual-line session grown
downward leaves them the other way around, and the family's ends are
fixed: forward keys push the mark, backward keys walk point.  Without
the swap, `w' after adopting such a selection walked its TOP edge
down instead of growing the bottom.

A visual-line session is also widened to whole lines as it is taken,
through the same `donkey--visual-line-region-bounds' the session's own
\`y' and \`d' go through: those sessions leave the newline that ends
the last line outside the region deliberately, and a run that
inherited the region raw dropped it -- `V J M d' removed the text of
two lines and left behind the blank line their newline still ended.
Every other selection is adopted exactly as it shows.

Any visual-line anchor is cleared: the session, if one was live, is
over -- the run owns the selection now, and a later \`V' starts a
fresh line session rather than resuming a dead one.

Refuses without a selection to adopt, and an EMPTY active region is no
selection: `donkey-set-mark' plants a mark and activates it without
covering anything yet, and adopting that made the first object key
grow from the cursor -- `v M w' from mid-word took the tail of the
word, where \`M' alone marks the whole of it and `M w' two.
`donkey-mark-run-toggle' enters the mode empty-handed in that case
instead.

A member of `donkey--mark-run-commands', which is the point of being
a command of its own: `donkey-mark-run-toggle' renames its adopting
press to this name in `this-command', and membership is what lets the
object key that follows read the adopted selection as a run in
progress instead of marking afresh over it."
  (interactive)
  (unless (donkey--adoptable-selection-p)
    (user-error "No selection to adopt"))
  ;; One call does both jobs: the car is the region's start and the cdr
  ;; its end whichever side point was on, and a live visual-line session
  ;; comes back widened to whole lines.
  (let ((span (donkey--visual-line-region-bounds)))
    (set-mark (cdr span))
    (goto-char (car span)))
  (setq donkey-visual-anchor nil)
  (donkey--mark-run-enter))

(defun donkey-mark-run-toggle ()
  "Enter mark run mode, adopting any active selection; \`M' again cancels.

Mark run mode is the `m' prefix held down for you: the object keys
\`w' \`W' \`b' \`B' \`s' \`S' run exactly the commands their
`m'-prefixed keys run, through `donkey-mark-run-mode-map'.  The press
itself is `m w' -- see below -- so `M w b' selects what `m w m w m b'
selects: two words forward and one back.  Every letter grows the one
selection, counts work, and objects mix mid-run, all per
`donkey--mark-run-commands'.

The paragraph pair is the one the mode does not letter.  \`p' and
\`P' stay the paste keys here, because a selection you cannot paste
over is a worse loss than a paragraph you have to spell `m p' -- and
`m p' and `m P' do grow a run from inside the mode, being the same
family commands.

The reminder in the echo area stays up for the whole mode: each
letter re-shows it (unlogged) over the mark command's own message,
which the visible selection already repeats -- see
`donkey--mark-run-mode-post-command'.

\`h' \`j' \`k' \`l' move point inside the mode without ending the
run, adjusting the selection's near end the way `j'/`k' adjust a
visual-line session; a motion may even cross the mark, passing the
selection through empty before the object keys grow it again -- the
freeform a `v' region has always had.  The line and buffer edges are
the exception among the motions: with a run live `g h', `g l', `g g'
and `g e' own an end apiece and stretch it, so `M g h g l' takes the
whole line's text and `M g g g e' the whole buffer, rather than each
undoing the other.  The object keys really do grow
it again: whichever way round a motion, a \`*' or a negative count
has left the ends, `donkey--normalize-mark-run' puts them back before
the next object is added.

The mode needs no explicit exit: any key outside the mode's own lets
it lapse and then does its ordinary job -- `M w d' selects two
words and deletes them.  \`M' pressed again cancels the selection and the
mode with it, and \`C-g' does the same, as it does for every
selection.

Two presses are held back from ending it.  A key that does nothing at
all -- one the normal state leaves unbound, or \`DEL' -- leaves the
run standing, so a mistyped key costs a beep rather than the
selection.  And \`V' is refused outright by `donkey-mark-run-refuse':
a visual-line session cannot own a mark run's selection, and the press
used to drop the run and anchor a fresh line session without saying
so.  Leave the run first -- \`M', \`C-g', or any key that uses the
selection -- and \`V' is itself again.

\`u' puts the run back where the last press found it and \`U' puts it
forward again, one press per step.  A run only ever grows -- `b'
after `w' adds a word at the other end rather than taking one back --
so without them a press that reached further than it looked left
cancelling and starting again as the only way out.

\`.' repeats the last press, as it does everywhere: `M w .' is three
words, and each \`.' is a step of its own for \`u' to take back.  The
key runs `repeat', which the mode reads as the command it repeats --
see `donkey--mark-run-press-command'.  `M .' repeats the toggle itself,
which takes the selection up again as if freshly pressed, with no
steps behind it.

The `m' prefix is the one key that neither runs nor ends the mode: it
still reaches the normal map, so `m w' inside the mode runs
`donkey-mark-word' -- the very command the bare `w' runs here -- and
the mode's tests are about COMMANDS, not keys, so the run simply
carries on.  `M m w w' selects three words, and the two spellings
can be mixed mid-run.  It is also how paragraphs are reached, their
letters having been given back to pasting: `M m p' grows the word
to its paragraph and stays in the mode.

With nothing selected the press marks a WORD on its way in, so the
mode arrives holding the thing nearly every run starts from.  \`M'
alone is a selected word: the press is `m w', and the mode's promise
is one sentence long -- each letter after it is one more `m'-prefixed
press.

WHICH word is `donkey-mark-word's answer and not a second rule: the
one under the cursor, or from a gap the one ahead of it.  The two agree
because a reader who presses `m w' from a gap and gets the word ahead
expects `M' to do the same, and because the alternative bought
nothing.  Declining on whitespace left an EMPTY run, and an empty run
cannot be grown by moving -- the `donkey--mark-run-adjusters' continue
a visible run only -- while the keys that can grow it mark afresh
regardless.  Where `donkey-mark-word' itself refuses, in a buffer with
no word in it at all, the mode still starts: entering is what the key
is for.

The word is the run's FIRST PRESS, and the letter after it GROWS: the
press leaves `donkey-mark-word' in `this-command' once the word is
marked, the way the adopting branch leaves `donkey-mark-run-adopt'
there, so the family test finds a member in `last-command' when the
letter arrives.  \`M' \`w' is two words, \`M' \`b' is the word and the
one before it, and \`M' \`s' is the word grown forward to the end of
its sentence -- what `m w m s' selects, the whole sentence being `m s'
and then \`M'.  For a long time the word was a head start instead,
this command keeping its own name so that the letter after it marked
afresh and `M w w b' spelled `m w m w m b'.  What that cost was the
letter after \`M' re-marking the very word already on screen: `M w'
looked like nothing happening, `M b' the same, and only the second
letter grew -- while `M l w' and `M * w' DID grow from the word, the
adjusters being members, so the word was a press to some keys and
none to others.  A press to every key, it needs no explaining.

Pressed with an active selection this ADOPTS it into the mode instead
of entering empty-handed -- see `donkey-mark-run-adopt': a
visual-line session, a `v' region, or a prefix-built selection all
carry over, and the in-mode \`M' is where canceling lives, the same
second-press shape `donkey-visual-line-toggle' has.  This press once
canceled ANY active selection; adoption replaced that after live use,
because \"transfer my selection into the mode\" kept being the
intent and \"throw it away\" kept being the result.  There are two
exceptions, and neither of them stops the mode starting.  A rectangle
has no forward end for the family to own, so a stale
`rectangle-mark-mode' is disabled per
`donkey--ensure-non-rectangle-selection' and the block is dropped
rather than adopted -- and then the press goes on to start a run as it
would over nothing at all, which is what it used to return without
doing.  And an EMPTY active region -- a bare `v', which
has activated a mark but covered nothing yet -- is dropped rather than
adopted, so the press marks the whole word at point in its place:
`v M' mid-word takes the word, as \`M' alone does.

This command is NOT a member of `donkey--mark-run-commands', and the
renames are why it need not be.  A press that marked or adopted
leaves the name of the command that did it, and a press that did
neither -- a buffer with no word in it -- keeps its own, so the letter
after THAT one marks afresh rather than growing from whatever stale
mark the buffer held.  Membership would have let the stale mark
qualify.  An earlier design instead anchored an empty selection at
point and WAS a member, so the first letter grew from the cursor --
`M w' from mid-word took the tail of the word.  The mode form marks
the whole object, exactly as the prefixed key does, which is the
promise this key makes: the same behavior, minus the prefix."
  (interactive)
  (let ((was-rectangle (bound-and-true-p rectangle-mark-mode)))
    (cond
     ((and (not was-rectangle) (donkey--adoptable-selection-p))
      ;; The rename is what makes the adoption stick: this command is
      ;; no family member, and without it the object key that follows
      ;; would mark afresh over the selection just adopted.
      (setq this-command 'donkey-mark-run-adopt)
      (donkey-mark-run-adopt))
     (t
      ;; Including an EMPTY active region, which is dropped rather than
      ;; adopted -- see `donkey-mark-run-adopt' for why `v M' must still
      ;; mark the whole word rather than the tail of it.  And a
      ;; RECTANGLE, which has no forward end for the family to own:
      ;; dropped like the rest, and then the mode starts here as it
      ;; would over nothing at all.  It used to cancel and return
      ;; without entering, the one selection whose `M' left the mode
      ;; off -- so the press did nothing but clear, and the obvious
      ;; second press found `last-command' equal to `this-command'
      ;; and grew the word from the mark the rectangle left, selecting
      ;; from where the rectangle began to the word under point.
      ;;
      ;; No `donkey--ensure-non-rectangle-selection' of its own here.
      ;; The word marked below is `donkey-mark-word', which comes
      ;; through the funnel first as every selection command does, so
      ;; the rectangle and any visual-line anchor are cleared there
      ;; whether the word is found or not.  This command used to call
      ;; it too, before the `cond', which was harmless while the funnel
      ;; only disabled a rectangle and stopped being so once it cleared
      ;; the anchor as well: the adopting branch above NEEDS the anchor,
      ;; `donkey-mark-run-adopt' widening a live session to whole lines
      ;; through it, and `V J M d' fell back to the raw region.
      (deactivate-mark)
      ;; The word IS `m w', with no position it declines from.
      ;; It used to run only with point ON a word, so that from a gap
      ;; the mode arrived empty rather than reaching for a word -- the
      ;; reasoning being that a key which says "start selecting" should
      ;; not reach the way one that says "word" may.
      ;;
      ;; That was wrong twice.  A reader who presses `m w' from a gap
      ;; and gets the word ahead expects `M' to agree, and there is no
      ;; reading under which the two should differ.  (WHICH neighbor
      ;; that is belongs to `donkey-mark-word' -- see `donkey--mark-reach'
      ;; -- and this key inherits the answer by calling it.)  And what the
      ;; declining bought was an empty run, which turns out to be worth
      ;; nothing: `h' `j' `k' `l', `g h', `g l' and `g e' all select
      ;; NOTHING from one -- the adjusters continue a VISIBLE run only
      ;; -- so an empty run cannot be grown by moving, and the keys
      ;; that can grow it (`w', `J', `%') mark afresh whether it is
      ;; there or not.  It was an affordance in name only.
      ;;
      ;; `last-command' is bound away for the call.  The word is a
      ;; FRESH mark, and `donkey-mark-word' decides between marking
      ;; and growing by reading `last-command' -- a family member left
      ;; there by an older run whose selection is gone, as `m w' and
      ;; then a hook that deactivated the mark leaves one, would have
      ;; it GROW from whatever mark was lying about instead of marking
      ;; the word under point.
      ;;
      ;; Marked BEFORE the mode is armed, which is only about what is
      ;; left on screen.  `donkey--mark-run-enter' ends by showing the
      ;; reminder, and the word's own "Word marked" used to land on
      ;; top of it -- so the mode's only sign was missing for the
      ;; whole first press of every run that starts on a word, which
      ;; is most of them, and `M d' never showed it at all.  The
      ;; rename below makes the post-command repaint cover this press
      ;; as well now; the order stays, so that the reminder is the
      ;; last thing said whether the repaint runs or not.
      (condition-case nil
          (progn
            (let ((last-command nil))
              (donkey-mark-word))
            ;; The word marked, the press IS `m w': renamed in
            ;; `this-command' to the command that marked, as the
            ;; adopting branch above renames to its own.  The rename
            ;; is what makes the letter that follows GROW the word --
            ;; see the docstring for what leaving the toggle's name
            ;; here cost.  Only on success: a press that found no word
            ;; keeps the toggle's own name, so the letter after it
            ;; marks afresh rather than growing from whatever mark the
            ;; buffer happened to hold.
            (setq this-command 'donkey-mark-word))
        ;; Refused only where `m w' refuses: a buffer with no word in
        ;; it at all, before point or after.  The mode still starts,
        ;; empty, because entering is what the key is for and a buffer
        ;; with nothing to mark is no reason to refuse the mode.  Only
        ;; the refusal is caught; anything else going wrong in a mark
        ;; command is worth hearing about.
        (user-error nil))
      (donkey--mark-run-enter)))))

(defun donkey-mark-word (&optional count)
  "Select the entire word at or adjacent to point.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

From the gap between two words the one AHEAD is marked, and from the
gap at the end of the buffer, where nothing is ahead, the last one.
`donkey-mark-word-backward' takes the one BEHIND from the same gap, so
from a space the two keys are the two words on either side of it; see
`donkey--mark-reach' for the rule, and for the report of the two keys
marking the same word that changed it.  `donkey-mark-symbol',
`donkey-mark-sentence' and `donkey-mark-paragraph' answer the same way.

Pressing the key again immediately EXTENDS the selection by another
word rather than re-marking the same one, and keeps extending until
the buffer runs out.  See `donkey--mark-extending-p'.
`donkey-mark-word-backward' continues the same run from the other end,
in either order -- `m w m w m b' is two words forward and one back --
and so does every other member of `donkey--mark-run-commands', each
adding one object of its own kind at its own end: `m w m s' is the
word grown forward to the end of its sentence.

COUNT marks that many words.  A negative COUNT marks that many words
before the one point normalizes onto, and a COUNT of zero marks one, as
a bare press does -- see `donkey--object-count' for why zero no longer
marks nothing the way `mark-word' itself reads it."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((extend (donkey--mark-run-continuing-p)))
    (unless extend
     (let ((origin (point)))
      ;; From a gap, onto the word ahead -- its first character, past
      ;; whatever punctuation lies before it -- or, for a backward press
      ;; and at the end of a buffer, onto the start of the word behind.
      ;; `donkey--mark-reach' says which is tried first.  `backward-word'
      ;; returns non-nil when it found a word, which is the answer the
      ;; reach wants; the skip forward is answered by where it stopped.
      (unless (donkey--point-on-word-or-symbol-char-p)
        (donkey--mark-reach-from-gap
         (lambda () (skip-syntax-forward "^w") (not (eobp)))
         (lambda () (backward-word 1))))
      ;; See `donkey-mark-symbol' for why this is a `user-error' rather
      ;; than letting `beginning-of-thing' signal a bare `error': a
      ;; buffer with no word in it at all (empty, or nothing but
      ;; whitespace and punctuation) is a normal thing to press this on
      ;; by accident.  Reached only once neither side of the gap has one.
      ;; The search is undone before reporting.  Both reaches move point
      ;; looking for something to mark, and a refusal that leaves it
      ;; where the search gave up has moved the cursor and marked
      ;; nothing, which reads as the key half working.  Live: in
      ;; `help-mode' on the space after \=`press \=‘C-x C-f\=’ now\=',
      ;; \=`m W\=' walked back over the whole quoted sequence and
      ;; reported from there, nine characters from the key.
      (unless (donkey--real-thing-at-point 'word)
        (goto-char origin)
        (user-error "No word at or before point"))
      (beginning-of-thing 'word)))
    ;; Walking point onto the word's start is skipped when extending:
    ;; it would land on the START of the word already selected, and
    ;; `mark-word' measures its extension from there, so running it
    ;; would grow the region by nothing and then by one word from the
    ;; wrong end.  (Squaring the RUN's ends up is a different job, and
    ;; `donkey--mark-run-continuing-p' has already done it.)
    ;;
    ;; ALLOW-EXTEND only permits the extension; `mark-word' still decides
    ;; for itself, and its test is `(eq last-command this-command)' or a
    ;; visible region beginning at point.  Neither holds for a companion
    ;; press onto a run some hook deactivated mid-way -- there it pushed
    ;; a fresh mark and collapsed the run to one word.  Presenting the
    ;; press as a repeat, exactly when `donkey--mark-extending-p' says
    ;; the run is live, is what `donkey-mark-sentence' does for
    ;; `mark-end-of-sentence' and for the same reason; the binding is
    ;; the identity on a plain repeat.
    (let ((last-command (if extend this-command last-command))
          ;; `mark-word' reads the mark with plain `mark' -- twice, to
          ;; pick its direction and to measure from -- and a run whose
          ;; region a hook deactivated arrives here with the mark set
          ;; and inactive, where plain `mark' refuses to answer unless
          ;; this is on.  It is on by default; turning it off was all
          ;; it took to make the extension signal `mark-inactive'
          ;; instead of growing.  See `donkey--normalize-mark-run'.
          (mark-even-if-inactive t)
          (n (donkey--object-count count)))
      (if (and (not extend) (< n 0))
          ;; `mark-word' measures a negative count from POINT, and point
          ;; is the START of the word being counted from, so the
          ;; separator in front of it came along -- see
          ;; `donkey--object-end-before'.  Only the fresh press is taken
          ;; over; a run continuing with a negative count is native's
          ;; question and stays native's.
          (let ((far (donkey--object-end-before
                      (point) #'backward-word #'forward-word)))
            (forward-word n)
            (push-mark far t)
            (activate-mark))
        (mark-word n extend))))
  (message "Word marked"))

(defun donkey--mark-backward (count motion delegate label)
  "Mark one object backward, or grow a run backward by COUNT of them.

The shared body of the four backward mark commands.  MOTION walks
point back over the objects, DELEGATE marks the object at point when
no run is in progress, and LABEL names the object in the echo message.

The selection layout is the family's: mark at the forward end, point
at the start, so the forward keys grow a run by pushing the mark and
these four by walking point.  Either key continues the run the other
started, in either order, as does every other member of
`donkey--mark-run-commands' -- see `donkey--mark-extending-p'.

A fresh press marks through DELEGATE rather than through MOTION, so
it cannot disagree with the forward key about which object is the one
at point; the remaining COUNT - 1 objects are walked afterwards.  From
a GAP the two keys are meant to disagree -- this one takes the object
behind, the forward key the one ahead -- and `donkey--mark-reach',
bound around the call, is how the delegate is told which side it is
marking for.  The delegate cannot itself decide to extend: reaching it
here means `last-command' names no member of the family, and the
delegate applies the same test.  It also brings its own no-object `user-error'
and its own rectangle cleanup; the extending branch has no rectangle
to clean, because the press that started the run disabled any stale
`rectangle-mark-mode' and a rectangle can only have activated since
through a command that ended the run.

The extending branch re-asserts the mark.  The forward direction
re-activates it on every press as a side effect of
`set-mark'/`mark-word'; moving point activates nothing, so without
this a region some hook deactivated mid-run would keep growing
invisibly -- point moves, nothing shows, and the next \`d' acts on a
selection the user cannot see.  A no-op when the region is active.

A COUNT below 1 is treated as 1.  A negative count already means
something in this family -- the forward commands read it as reaching
BEHIND point -- and these four are that direction, so there is nothing
left for it to name here; zero is a bare press for every mark key.
Running out of buffer stops and keeps what is selected, matching the
forward direction at the end
of the buffer."
  (let ((n (max 1 (or count 1))))
    (if (donkey--mark-run-continuing-p)
        (progn
          (funcall motion n)
          (activate-mark)
          (message "%s marked" label))
      (let ((donkey--mark-reach 'behind))
        (funcall delegate))
      (when (> n 1)
        (funcall motion (1- n))))))

(defun donkey-mark-word-backward (&optional count)
  "Select the word at point, or grow a word selection BACKWARD.

The other end of `donkey-mark-word's run, and one of the four sharing
`donkey--mark-backward' -- which is where the run layout, the fresh
delegation, the re-activation and the count rule are all explained.
From \"that\" in \"for text that is not saved\", `m w m w m b' selects
\"text that is\": two words forward, one back.

Pressed fresh on a word, this and `m w' select the same word.  Pressed
fresh in the gap between two, this selects the word BEHIND and `m w'
the one ahead, so from the space after \"text\" the two keys are
\"text\" and \"that\" -- see `donkey--mark-reach'.  They used to agree
on \"text\", which is the report that changed it.

COUNT marks or extends by that many words."
  (interactive "p")
  (donkey--mark-backward count
                         (lambda (n) (forward-word (- n)))
                         #'donkey-mark-word
                         "Word"))

(defun donkey--region-blank-p ()
  "Return non-nil if only whitespace and newlines lie in the region.

Walks the region in place rather than copying it into a string to
match against: the callers ask this of whatever they just marked, and
a paragraph or sentence selection can be large.  Both bounds are read
before point moves; `region-end' is a function of point, and reading
it after the skip returned the skip's own position."
  (let ((beg (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "[:space:]\n" end)
      (= (point) end))))

(defun donkey--object-end-before (origin backward forward)
  "Return where the object just before ORIGIN ends.

The far end of a selection a NEGATIVE count asks for.  Such a count
names the objects BEHIND the one point normalizes onto, so the selection
stops where the nearest of them ends: not at ORIGIN, which is the start
of the object being counted from, and not wherever counting the same
number of objects forward again happens to land.

Both of those were tried, and each was wrong in its own direction.
`donkey-mark-word' handed the negative case to `mark-word', which
measures from point, so the separator came with it -- asked for the word
before \"four\" it marked \"three \" rather than \"three\".
`donkey-mark-symbol' counted forward again from the start it had
reached, which overshoots as soon as the buffer runs out behind: asked
for the three symbols before the second word of a five-word buffer, it
marked the second and third words as well, reaching past the very point
the count was measured from.

BACKWARD and FORWARD are the object\\='s own motions.  Going back one and
forward one lands on the end of the object behind, from any position a
caller has normalized; the `min' is for a caller that has not, where
FORWARD could return past where it started."
  (save-excursion
    (goto-char origin)
    (funcall backward 1)
    (funcall forward 1)
    (min (point) origin)))

(defun donkey--refuse-blank-mark (object &optional origin)
  "Drop the region and report when it is nothing but whitespace.

OBJECT names what was asked for, so the message reads \"No sentence at
or before point\" or \"No paragraph at or before point\" -- the two
commands whose motions walk to the end of a blank buffer and back
rather than signaling, and so end up \"marking\" the blank.

ORIGIN is where the key was pressed, and point goes back there before
the report.  Both callers walk point about looking for an object before
they know whether there is one -- forward to a sentence end and back, or
back to a paragraph start -- and a refusal that leaves it where the
search gave up has moved the cursor and marked nothing, which reads as
the key half working.  Measured on a buffer of whitespace with the key
pressed at position 3: the cursor came to rest at position 1.
`donkey-mark-word' and `donkey-mark-symbol' answer the same way, and
did so first; this is the same rule reaching the other two keys.

For a FRESH press only, which is why the callers keep their own
condition: a run may legitimately cover blank -- `M J' on an indented
empty line of a buffer with no word for \`M' to take marks whitespace,
and the next object key is asked to grow it -- and refusing there
deactivated the mark, throwing away a selection the user could see
over the state the run started from."
  (when (donkey--region-blank-p)
    (deactivate-mark)
    (when origin
      (goto-char origin))
    (user-error "No %s at or before point" object)))

(defun donkey-mark-sentence (&optional count)
  "Select sentence at point.

With no sentence to be found -- an empty buffer, or one holding only
blank lines or whitespace -- reports a `user-error' rather than letting
the sentence motions signal.  They raise a bare `end-of-buffer' there,
and in a buffer of only newlines a bare `error' reading \"Invalid search
bound (wrong side of point)\", an internal that says nothing to whoever
pressed the key and pops the debugger for anyone running with
`debug-on-error' on.  `donkey-mark-word' and `donkey-mark-symbol' guard
the same way.

Converted after the fact rather than gated beforehand: the obvious gate,
`(thing-at-point \\='sentence)', also returns nil with point on the blank
line below real prose -- a case this command handles correctly today --
so gating on it would reject work it can actually do.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

From the gap between two sentences the one AHEAD is marked -- the same
answer `donkey-mark-word', `donkey-mark-symbol' and
`donkey-mark-paragraph' give from the gap between two of their own
objects -- and from the gap at the end of the buffer, where nothing is
ahead, the last one.  `donkey-mark-sentence-backward' takes the one
BEHIND from the same gap; see `donkey--mark-reach' for the rule and the
report behind it.  This command reached forward from every gap once,
and was turned round to reach behind so that one cursor position meant
one thing under every mark key; it reaches forward again now that the
whole family does, and keeps what the turn bought at the end of a
buffer, which marks the last sentence where it used to report \"No
sentence after point\".

COUNT marks that many sentences.  Unlike the other mark commands a COUNT
below 1 is treated as 1 here: `mark-end-of-sentence' counts from the
start this command normalizes onto, so a count of 0 selects nothing at
all and a negative one reaches back over the sentence already behind
that start -- neither of which is a sentence at point.  A COUNT reaching
past the last sentence marks what there is and stops, the way every
other counted command does.

Pressing the key again immediately EXTENDS the selection by another
sentence rather than re-marking the same one, and keeps extending until
the buffer runs out.  That comes from `mark-end-of-sentence', which
grows the region whenever `last-command' is this command again; the
other mark commands do not, since `mark-word' and friends gate it behind
an ALLOW-EXTEND argument that is nil when called from Lisp.
`donkey-mark-sentence-backward' continues the same run from the other
end -- as does every member of `donkey--mark-run-commands', in either
order -- see `donkey--mark-extending-p', and see below for how a
continuation reaches `mark-end-of-sentence's own test."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((origin (point))
        (extending (donkey--mark-run-continuing-p)))
   ;; A fresh press normalizes onto a sentence start; a run in progress
   ;; must not, or growing a WORD selection with `m s' would silently
   ;; walk the region's start back to its sentence's start as a side
   ;; effect of the continuation.  The siblings skip normalization when
   ;; extending for the same reason; here the skip sits inside the
   ;; `condition-case' so the handlers and their reasoning stay put.
   (condition-case nil
      (unless extending
        ;; Forward first, then back.  `backward-sentence' alone lands on
        ;; the PREVIOUS sentence whenever point is already sitting at a
        ;; sentence start, so pressing this with the cursor on the first
        ;; letter -- the most natural place to press it -- marked the
        ;; sentence before the one under the cursor.  Reported live on
        ;; the scratch message, with point on the \"T\" of \"To create a
        ;; file\": it selected \"This buffer is for text that is not
        ;; saved, and for Lisp evaluation.\" instead.  Going forward to
        ;; the end of the sentence containing point first makes the
        ;; backward step land on that same sentence's start from every
        ;; position within it.
        (forward-sentence 1)
        (backward-sentence 1)
        ;; Landing AHEAD of where we started means point was not inside a
        ;; sentence at all -- it was in the gap before this one.  For the
        ;; forward key that landing is the answer: the sentence ahead,
        ;; which is what `donkey-mark-word', `donkey-mark-symbol' and
        ;; `donkey-mark-paragraph' give from the gap between two of their
        ;; objects.  A backward press -- `donkey--mark-reach' says which
        ;; this is -- takes the sentence behind instead, so from one gap
        ;; `m s' and `m S' are the two sentences on either side of it.
        ;;
        ;; Unless there is nothing behind: in the leading gap of a buffer
        ;; the sentence ahead is the only one there is, and stepping back
        ;; would drag the leading whitespace into the selection.  The test
        ;; is whether any prose precedes ORIGIN at all --
        ;; `donkey--text-before-p', which `donkey-mark-paragraph' asks
        ;; from a blank line for the same reason.
        (when (and (eq donkey--mark-reach 'behind)
                   (> (point) origin)
                   (donkey--text-before-p origin))
          (backward-sentence 1)))
    ;; Before the general handler, which would otherwise catch this and
    ;; refuse.  The forward step signals `end-of-buffer' for a real
    ;; buffer whose last sentence has no newline after it -- point-max IS
    ;; the trailing gap there, with nothing ahead to normalize onto.  The
    ;; sentence BEHIND is the answer, the same one `donkey-mark-word' and
    ;; `donkey-mark-paragraph' give from the end of a buffer.
    ;;
    ;; A buffer with no sentence in it at all reaches here too, and ends
    ;; up marking whitespace, which the guard further down rejects with
    ;; "No sentence at or before point".  The two used to need telling
    ;; apart, because a buffer with prose in it got the misleading "No
    ;; sentence at or before point" while a trailing newline -- giving
    ;; the forward step somewhere to land -- produced a different message
    ;; from a different guard.  Which one a reader saw depended on
    ;; whether their file ended with a newline.  Both now mark the last
    ;; sentence, so there is nothing left to tell apart.
    (end-of-buffer
     (goto-char (point-max))
     (backward-sentence 1))
    ;; Point goes back before reporting, for the reason
    ;; `donkey--refuse-blank-mark' gives: the normalization above has
    ;; already walked forward to a sentence end and back by the time this
    ;; is reached.
    (error (goto-char origin)
           (user-error "No sentence at or before point")))
  ;; Continuing a run any other family member started or last grew:
  ;; the extension lives inside `mark-end-of-sentence', whose own test
  ;; is `(eq last-command this-command)' -- it cannot know about
  ;; companions.  Presenting a companion press as a repeat, exactly
  ;; when `donkey--mark-extending-p' says the run is live, lets the
  ;; native extension fire from the mark instead of collapsing the far
  ;; end back to the first sentence; the binding is the identity on a
  ;; plain repeat.
  (let ((last-command (if extending this-command last-command))
        ;; See `donkey-mark-word': `mark-end-of-sentence' reads the mark
        ;; with plain `mark' too, and a deactivated run must still grow.
        (mark-even-if-inactive t))
    (condition-case nil
        (mark-end-of-sentence (max 1 (or count 1)))
      ;; A count running past the last sentence marks what there is and
      ;; stops, like the counted deletes and every other mark command --
      ;; `forward-sentence' inside `mark-end-of-sentence' signals a bare
      ;; `end-of-buffer' there instead, which the guard below then reported
      ;; as "No sentence at or before point": a flat contradiction of the
      ;; screen, which is showing one.  Bare \\[universal-argument] means
      ;; FOUR, so `C-u m s' hit this on any buffer of three sentences or
      ;; fewer -- confirmed on "One thing.  Two thing.  Three thing.".
      (end-of-buffer (push-mark (point-max) nil t))
      (error (goto-char origin)
             (user-error "No sentence at or before point"))))
  ;; Going forward first means the motions no longer signal in a buffer
  ;; holding nothing but whitespace -- they simply walk to its end and
  ;; back, "marking" the blank.  Reject that here so such a buffer still
  ;; reports rather than selecting nothing of substance.
  (unless extending
    (donkey--refuse-blank-mark "sentence" origin))
  (message "Sentence marked")))

(defun donkey-mark-sentence-backward (&optional count)
  "Select the sentence at point, or grow a sentence selection BACKWARD.

The other end of `donkey-mark-sentence's run, sharing
`donkey--mark-backward' with the other three backward keys.  The
forward partner's extension lives inside `mark-end-of-sentence' rather
than in a branch of its own, so continuing a run THIS command started
is arranged inside `donkey-mark-sentence' -- see the `last-command'
binding there.

At the buffer's start `backward-sentence' signals nothing: it walks to
the start of the paragraph's text and stays, so an overshooting count
keeps what is selected -- confirmed by probe.

From the gap between two sentences this takes the one BEHIND, where
`donkey-mark-sentence' takes the one ahead -- see `donkey--mark-reach'.

COUNT marks or extends by that many sentences."
  (interactive "p")
  (donkey--mark-backward count #'backward-sentence
                         #'donkey-mark-sentence "Sentence"))

;; Exactly one blank line comes with a paragraph, whichever side it is on.
;;
;; `backward-paragraph' lands ON the blank line before a paragraph, so
;; every paragraph but the FIRST arrives with one blank already included
;; and none after it.  The first has nothing before it to land on, so it
;; used to come with no blank at all -- and the same key that left
;;
;;   "Alpha.\n\nGamma.\n"     after deleting a middle paragraph
;;
;; left
;;
;;   "\nBeta.\n\nGamma.\n"    after deleting the top one
;;
;; a stray blank at the head of the buffer.  Absorbing the following
;; blank in that case evens it out: deleting any paragraph now leaves its
;; neighbors separated by exactly one blank line, which is what
;; `mark-paragraph' does not do and what vi's `ap' text object does.
;;
;; One line, not `skip-chars-forward': a run of several blank lines
;; between paragraphs is the author's spacing, and swallowing all of it
;; would take more than the paragraph asked for.  Confirmed against
;; "A.\n\n\n\nB.\n", where deleting either paragraph leaves two blanks
;; standing.
(defun donkey--absorb-paragraph-blank (start)
  "Extend point over one following blank line, when START owns no leading one.

Called with point at the end of a paragraph selection that began at
START.  Does nothing when the selection already begins on a blank line,
or when there is no blank line to take.  It once took the count as well
and did nothing at zero; `donkey--object-count' reads zero as one now,
so a selection always has a paragraph in it by the time this runs."
  (when (and (save-excursion
               (goto-char start)
               (not (looking-at-p "^[[:space:]]*$")))
             (looking-at-p "^[[:space:]]*$")
             (< (point) (point-max)))
    (forward-line 1)))

(defun donkey-mark-paragraph (&optional count)
  "Select the paragraph at or adjacent to point.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

With no paragraph to be found -- an empty buffer, or one holding only
blank lines or whitespace -- reports a `user-error', the way
`donkey-mark-word', `donkey-mark-symbol' and `donkey-mark-sentence' all
already do.  This one was the odd member of the family: the paragraph
motions do not signal on a blank buffer, they simply walk to its end and
back, so it announced \"Paragraph marked\" over an empty buffer with no
region active at all, and over a buffer of nothing but newlines it
\"marked\" the blank.  A command that reports success and leaves nothing
selected is worse than one that says it found nothing.

Checked on the result rather than beforehand, matching
`donkey-mark-sentence': a blank line BETWEEN two paragraphs is a normal
place to press this from and marks the paragraph below, so gating on
what is under point would reject work this command does correctly.

From a blank line between two paragraphs the one BELOW is marked, as
`donkey-mark-word' marks the word ahead from the space between two, and
from blank lines at the end of the buffer the last one.
`donkey-mark-paragraph-backward' takes the one ABOVE from the same blank
line -- see `donkey--mark-reach' for the rule and the report behind it.
Both used to take the one above.  Whichever is taken comes with ONE
blank line, as below.

Point is left at the START of the selection and the mark at its end,
which is where `mark-paragraph' leaves them and where
`donkey-mark-word', `donkey-mark-symbol' and `donkey-mark-sentence'
leave them.

Exactly ONE blank line comes with the paragraph: the one before it, or
-- for the first paragraph in the buffer, which has none before it --
the one after.  So deleting a paragraph leaves the separator between
its neighbors intact rather than a stray blank line, wherever in the
buffer it sits.

Pressing the key again immediately EXTENDS the selection by another
paragraph -- see `donkey--mark-extending-p' -- and
`donkey-mark-paragraph-backward' continues the same run from the other
end, in either order, as does every other member of
`donkey--mark-run-commands'.

COUNT marks that many paragraphs.  A negative COUNT marks that many
paragraphs before the one point normalizes onto, and a COUNT of zero
marks one, as a bare press does -- see `donkey--object-count'."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((n (donkey--object-count count))
        (origin (point))
        (extending (donkey--mark-run-continuing-p)))
    (if extending
        ;; Grown by moving the MARK, which is the end this command owns
        ;; -- the same shape as `donkey-mark-symbol' and as
        ;; `mark-paragraph's own ALLOW-EXTEND branch.
        ;;
        ;; The blank-line rule has to be applied here too, or repeating
        ;; the key stops agreeing with a count: `m p m p' from the first
        ;; paragraph gave one blank fewer than `C-u 2 m p', because only
        ;; the fresh branch below knew to absorb one.  Caught by the
        ;; test named donkey-repeating-a-mark-key-equals-a-count.
        (set-mark (save-excursion
                    (let ((start (point)))
                      (goto-char (mark t))
                      (forward-paragraph n)
                      (donkey--absorb-paragraph-blank start)
                      (point))))
      ;; Point ends at the START, mark at the end.  It used to be the
      ;; other way round, which made this the only mark command that
      ;; inverted native: `mark-paragraph' finishes with
      ;; `backward-paragraph' and leaves point where the selection
      ;; begins.  Being the odd one out cost something concrete -- the
      ;; first attempt at extending here handed native's ALLOW-EXTEND
      ;; branch a mark-at-start region, and since native grows by
      ;; pushing the MARK forward it collapsed the selection onto the
      ;; paragraph's first character.
      ;;
      ;; From a BLANK line the two neighbors are a choice, and the choice
      ;; is the key's -- see `donkey--mark-reach'.  The forward key takes
      ;; the paragraph below: forward over it and back lands on the last
      ;; blank line before it, so the selection carries one blank as it
      ;; does from anywhere else, where the step back alone would carry
      ;; every blank line between the two paragraphs.  The backward key
      ;; steps back alone and takes the one above, as both keys did from
      ;; every blank line until `m w' and `m b' were found marking the
      ;; same word from the same space.  Either falls through to the
      ;; other side where its own is empty: `forward-paragraph' at the
      ;; end of a buffer stays put and the step back finds the last
      ;; paragraph, and a backward press with no text above steps
      ;; forward first rather than dragging the leading blank lines in.
      ;; A line with text on it is never a gap, wherever on it point is.
      ;;
      ;; From a line with text on it the start is found the way
      ;; `mark-paragraph' finds it: forward to the paragraph's end and
      ;; back from there.  `backward-paragraph' alone is not safe from
      ;; the paragraph's FIRST character: it takes a shortcut when the
      ;; line above is truly empty and lands on it, but when that line
      ;; holds whitespace it walks back over the separator into the
      ;; paragraph before, so `m p' on the first character of a
      ;; paragraph under a spaces-only line marked the paragraph ABOVE.
      ;; Confirmed live in fundamental, text and org modes.  Going
      ;; forward first lands on the same paragraph from any position in
      ;; it, and still on the empty line before it where there is one,
      ;; so the one-blank-line rule reads as before.
      (if (and (save-excursion
                 (beginning-of-line)
                 (looking-at-p "[[:space:]]*$"))
               (eq donkey--mark-reach 'behind)
               (donkey--text-before-p origin))
          (backward-paragraph 1)
        (forward-paragraph 1)
        (backward-paragraph 1))
      (let ((start (point)))
        (forward-paragraph n)
        (donkey--absorb-paragraph-blank start)
        (push-mark (point) nil t)
        (goto-char start))
      (activate-mark))
    (unless extending
      (donkey--refuse-blank-mark "paragraph" origin))
    (message "Paragraph marked")))

(defun donkey-mark-paragraph-backward (&optional count)
  "Select the paragraph at point, or grow a paragraph selection BACKWARD.

The other end of `donkey-mark-paragraph's run, sharing
`donkey--mark-backward' with the other three backward keys.

The one-blank-line rule needs no backward counterpart to
`donkey--absorb-paragraph-blank': `backward-paragraph' lands BEFORE the
blank line that precedes the paragraph it walks over, so the separator
that used to lead the selection simply becomes interior -- confirmed by
probe, growing back from \"Beta\" onto \"Alpha\" selects both
paragraphs with the one blank line between them and no stray blank at
either end.

From a blank line between two paragraphs this takes the one ABOVE,
where `donkey-mark-paragraph' takes the one below -- see
`donkey--mark-reach'.

COUNT marks or extends by that many paragraphs."
  (interactive "p")
  (donkey--mark-backward count #'backward-paragraph
                         #'donkey-mark-paragraph "Paragraph"))

(defun donkey-mark-symbol (&optional count)
  "Select the entire symbol at or adjacent to point.

Punctuation at either end is omitted from the selection -- a trailing
comma or period, and the quotes around a name in prose.  An expression
prefix is not punctuation and stays: \\='bar marks as \\='bar.  See
`donkey--trim-symbol-punctuation' and `donkey--trim-symbol-prefix'.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

From the gap between two symbols the one AHEAD is marked, and from the
gap at the end of the buffer, where nothing is ahead, the last one.
`donkey-mark-symbol-backward' takes the one BEHIND from the same gap --
see `donkey--mark-reach' for the rule and the report behind it.
`donkey-mark-word', `donkey-mark-sentence' and `donkey-mark-paragraph'
answer the same way.  Brackets and quotes are crossed on the way, so
from the opening paren of \"(foo bar)\" the mark is \"foo\", where sexp
motion used to land on the paren and refuse.

Pressing the key again immediately EXTENDS the selection by another
symbol -- see `donkey--mark-extending-p' -- and
`donkey-mark-symbol-backward' continues the same run from the other
end, in either order, as does every other member of
`donkey--mark-run-commands'.

COUNT marks that many symbols.  A negative COUNT marks that many symbols
before the one point normalizes onto, and a COUNT of zero marks one, as
a bare press does -- see `donkey--object-count'."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((n (donkey--object-count count)))
   (if (donkey--mark-run-continuing-p)
      ;; Grown by moving the MARK, which is where this command leaves the
      ;; far end of its selection -- it finishes with `backward-sexp', so
      ;; point sits at the START.  The same shape as `mark-word's own
      ;; extend branch, and the punctuation trim has to run again because
      ;; the new end is a new symbol with its own possible trailing "."
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (forward-sexp n)
                  (when (> n 0)
                    (donkey--trim-symbol-punctuation))
                  (point)))
    (let ((origin (point)))
     ;; From a gap, onto the symbol ahead -- its first character of word
     ;; or symbol syntax, across whatever punctuation, quotes and
     ;; brackets lie before it, for the reason `donkey--back-to-symbol-char'
     ;; gives for the step the other way -- or, for a backward press and
     ;; at the end of a buffer, onto the last character of the symbol
     ;; behind.  `donkey--mark-reach' says which is tried first.  The
     ;; reach forward is by syntax and not by `forward-sexp', which lands
     ;; on the bracket of a list rather than on the symbol inside it, and
     ;; so refused "(foo bar)" from its opening paren and "  (foo)" from
     ;; the whitespace before it.
     (unless (donkey--point-on-word-or-symbol-char-p)
       (donkey--mark-reach-from-gap
        (lambda () (skip-syntax-forward "^w_") (not (eobp)))
        #'donkey--back-to-symbol-char))
    ;; `beginning-of-thing' signals a bare `error' when there is no
    ;; symbol to be found, which pops the debugger for anyone running
    ;; with `debug-on-error' on.  Reaching it is ordinary, not
    ;; exceptional: a buffer of brackets and punctuation has no symbol
    ;; on either side of any gap in it -- confirmed on "  ()  ".
    ;; Undone before reporting -- see `donkey-mark-word' for the shape
    ;; and for the press that showed it.
    (unless (donkey--real-thing-at-point 'symbol)
      (goto-char origin)
      (user-error "No symbol at or before point"))
    (beginning-of-thing 'symbol))
    ;; Each trim belongs to an END of the selection, and the two
    ;; directions reach their ends differently.  A forward count walks to
    ;; the far end and back, so both ends are positions the motions
    ;; visit.  A negative count walks to the START and takes its far end
    ;; from the object behind point -- counting forward again from that
    ;; start reaches past point once the buffer runs out behind, which is
    ;; what `donkey--object-end-before' exists to stop.  A count of zero
    ;; marks nothing and needs neither trim.
    (if (< n 0)
        (let ((far (save-excursion
                     (goto-char (donkey--object-end-before
                                 (point) #'backward-sexp #'forward-sexp))
                     (donkey--trim-symbol-punctuation)
                     (point))))
          (forward-sexp n)
          (donkey--trim-symbol-prefix)
          (push-mark far t))
      (forward-sexp n)
      (when (> n 0)
        (donkey--trim-symbol-punctuation))
      (push-mark (point) t)
      ;; Back over the same number of symbols the first step covered.
      ;; Going back one regardless left the region holding only the LAST
      ;; symbol of a counted run -- a count of 2 over "foo-a bar-b"
      ;; marked just "bar-b".
      (backward-sexp n)
      (when (> n 0)
        (donkey--trim-symbol-prefix)))
    (activate-mark)))
  (message "Symbol marked"))

(defun donkey-mark-symbol-backward (&optional count)
  "Select the symbol at point, or grow a symbol selection BACKWARD.

The other end of `donkey-mark-symbol's run, sharing
`donkey--mark-backward' with the other three backward keys.

The trailing trim does not run here.  `donkey--trim-symbol-punctuation'
drops a trailing \".\" or \",\" because that punctuation attaches to the
END of a name, and this walk lands on symbol STARTS; whatever punctuation
separated the symbols becomes interior to the selection -- confirmed by
probe, growing back from \"baz\" over \"foo, bar baz\" selects all of it,
comma in place.

`donkey--trim-symbol-prefix' does run, for the punctuation the walk
lands ON rather than passes over.  `backward-sexp' stops where a sexp
starts, which in prose is in front of the quote before the name.

The delegate a fresh press goes through also brings the
trailing-punctuation trim along with it, and from the gap between two
symbols it marks the one BEHIND for this key, where `donkey-mark-symbol'
takes the one ahead -- see `donkey--mark-reach'.

COUNT marks or extends by that many symbols."
  (interactive "p")
  (donkey--mark-backward count
                         (lambda (n)
                           ;; At the buffer's start `backward-sexp'
                           ;; simply stops; before an unmatched opener it
                           ;; signals without moving.  Either way the
                           ;; selection is left as it was, matching how
                           ;; the forward direction runs out of buffer.
                           (condition-case nil
                               (backward-sexp n)
                             (scan-error nil))
                           (donkey--trim-symbol-prefix))
                         #'donkey-mark-symbol
                         "Symbol"))

;; The non-toggling behavior is left as stock deliberately: "v" is
;; `set-mark-command' and nothing else, so `C-u v' still pops the mark ring
;; and anything built on `set-mark-command' keeps working.  Documented in
;; the tutor and the README rather than papered over here.
(defun donkey-set-mark ()
  "Call `set-mark-command', disabling a stale `rectangle-mark-mode' first.

See `donkey--ensure-non-rectangle-selection' for why.

This does NOT toggle, unlike its two neighbors `donkey-visual-line-toggle'
\(\"V\") and `donkey-rectangle-mark-mode' (\"m v\"), which both cancel the
selection they started when pressed again.  `set-mark-command' re-anchors:
a second press drops a fresh mark at point and carries on selecting from
there, so the previous selection is discarded but the buffer is still in
a selecting state.  \\[keyboard-quit] is what lets go."
  (interactive)
  (donkey--ensure-non-rectangle-selection)
  (call-interactively #'set-mark-command)
  ;; Only when a selection actually started.  A PREFIXED press pops the
  ;; mark ring instead of setting the mark, and a second bare press over
  ;; a live selection deactivates it -- neither leaves anything to
  ;; advertise, and `set-mark-command' has already said which happened.
  ;;
  ;; `mark-active' rather than `region-active-p', for the reason
  ;; `donkey-rectangle-mark-mode' gives: the latter also wants
  ;; `transient-mark-mode', which is nil in a `--batch' Emacs and would
  ;; read as no selection there however active the mark.
  (when mark-active
    (add-hook 'deactivate-mark-hook #'donkey--clear-selection-hint nil t)
    (setq donkey--linear-selection-active t)
    (message "%s" donkey--linear-selection-hint)))

;; "%" was the one selection key bound straight to a stock command, so it
;; was the one that did not clear a stale rectangle: one left active from an
;; earlier `donkey-rectangle-mark-mode' session survived underneath the new
;; whole-buffer selection, and `donkey-delete' then killed a zero-width
;; rectangle -- one empty string per line -- leaving the buffer completely
;; untouched with no error to explain it.  It also left that emptiness in
;; `killed-rectangle', where "P" would have pasted it back.
;;
;; Invoked via `call-interactively', as `donkey-set-mark' does for
;; `set-mark-command': `mark-whole-buffer' is declared `interactive-only', so
;; calling it directly is a byte-compiler error here.
(defun donkey-mark-whole-buffer ()
  "Select the whole buffer, clearing a stale rectangle selection first.

See `donkey--ensure-non-rectangle-selection' for why every command that
establishes a selection has to do this."
  (interactive)
  (donkey--ensure-non-rectangle-selection)
  (call-interactively #'mark-whole-buffer))

;;; ---------------------------------------------------------------------------
;;; Banked Line Selection
;;; ---------------------------------------------------------------------------

(defface donkey-banked-selection
  '((t :inherit secondary-selection))
  "Face marking lines banked with `donkey-bank-selection'.

Inherits `secondary-selection', so banked lines stay distinct from
the live region."
  :group 'donkey)

(defvar-local donkey--banked-overlays nil
  "Overlays covering the whole lines banked in this buffer.")

(defun donkey--whole-line-span (beg end)
  "Return (START . END) covering every whole line touched by BEG..END.

END extends past the final line's newline when there is one, so a
banked line carries its own line break and deleting it removes the
line rather than leaving a blank."
  (save-excursion
    (let ((start (progn (goto-char (min beg end))
                        (line-beginning-position)))
          (finish (progn (goto-char (max beg end))
                         ;; A region ending exactly at a line beginning
                         ;; came from the line ABOVE -- do not swallow the
                         ;; next line just because point sits at its start.
                         (when (and (bolp) (> (point) (min beg end)))
                           (forward-char -1))
                         (min (point-max) (1+ (line-end-position))))))
      (cons start finish))))

(defun donkey--prune-banked-overlays ()
  "Drop banked overlays that no longer cover any text.

An overlay collapses to zero width when the line it banked is removed
by ordinary editing; such a bank highlights nothing, and is dropped."
  (setq donkey--banked-overlays
        (seq-filter (lambda (ov)
                      (or (and (overlay-buffer ov)
                               (< (overlay-start ov) (overlay-end ov)))
                          (ignore (delete-overlay ov))))
                    donkey--banked-overlays)))

(defun donkey--banked-spans ()
  "Return usable banked spans as a list of (START . END), in buffer order.

Prunes collapsed overlays first, so every span returned covers real
text.  Spans reaching outside the buffer's accessible portion are left
out, not pruned: they count again once the buffer is widened.  Every
span is widened to the whole lines its overlay touches, two overlays
that come to share a line are reported once, and spans that merely
touch stay separate."
  (donkey--prune-banked-overlays)
  (let ((spans (sort (delq nil
                           (mapcar (lambda (ov)
                                     (let ((start (overlay-start ov))
                                           (end (overlay-end ov)))
                                       (and (>= start (point-min))
                                            (<= end (point-max))
                                            (donkey--whole-line-span start end))))
                                   donkey--banked-overlays))
                     (lambda (a b) (< (car a) (car b)))))
        merged)
    (dolist (span spans (nreverse merged))
      (if (and merged (< (car span) (cdr (car merged))))
          (setcdr (car merged) (max (cdr (car merged)) (cdr span)))
        (push span merged)))))

(defun donkey-banked-spans ()
  "Return this buffer's banked lines as a list of (START . END) conses.

They come in buffer order, and nil when nothing is banked.

The public name for reading what donkey has banked, for other packages to
build on.  `donkey-bank-selection' banks whole lines, so every span runs
from the beginning of a line to the beginning of the line after the last
one it covers.

Spans are safe to hand to `buffer-substring' or `delete-region': ones
whose text has gone are pruned, and ones outside the buffer's accessible
portion are left out, since overlay positions ignore narrowing while
those functions do not.

Spans are not merged, so two banked blocks that happen to touch arrive as
two conses rather than one.  Merge them yourself if you need the ranges
disjoint.

Prefer this over the internal it wraps: the double-dashed name is
donkey's own and free to change shape, this one is not."
  (donkey--banked-spans))

(defun donkey--merge-spans (spans)
  "Merge overlapping or touching SPANS, a list of (START . END) in order."
  (let (merged)
    (dolist (span spans (nreverse merged))
      (if (and merged (<= (car span) (cdr (car merged))))
          (setcdr (car merged) (max (cdr (car merged)) (cdr span)))
        (push (cons (car span) (cdr span)) merged)))))

(defun donkey--banked-selection-p ()
  "Return non-nil if this buffer has any live banked lines."
  (and donkey--banked-overlays (donkey--banked-spans)))

(defun donkey--live-rectangle-p ()
  "Return non-nil when a rectangle selection is on screen right now.

The live selection you are looking at wins, and the bank is the
fallback: `y' and `d' take the rectangle and leave every bank
standing, and `p' over banks ignores a rectangle merely sitting in
`killed-rectangle'."
  (and (use-region-p) (bound-and-true-p rectangle-mark-mode)))

(defun donkey-clear-banked-selection ()
  "Discard every banked line in this buffer.

Leaves the buffer text and the live region alone -- this only throws
away what `donkey-bank-selection' set aside."
  (interactive)
  (let ((count (donkey--banked-line-count)))
    (mapc #'delete-overlay donkey--banked-overlays)
    (setq donkey--banked-overlays nil)
    ;; `any', so the commands that spend a bank stay quiet and a
    ;; keyboard macro still reports.
    (when (called-interactively-p 'any)
      (message (if (zerop count)
                   "No banked lines"
                 (format "Discarded %d banked line%s"
                         count (if (= 1 count) "" "s")))))))

(defun donkey--effective-line-spans ()
  "Return the spans `y'/`d' should act on while lines are banked.

The banked lines plus, when a region is also active, the whole lines
it covers -- so the selection you are looking at right now counts
without having to bank it first.  Overlapping and adjacent spans are
merged, so banking the same line twice, or banking a line adjacent to
the live region, never duplicates or splits text."
  (donkey--merge-spans
   (sort (append (donkey--banked-spans)
                 (when (use-region-p)
                   (list (donkey--whole-line-span (region-beginning)
                                                  (region-end)))))
         (lambda (a b) (< (car a) (car b))))))

(defun donkey-bank-selection (&optional count)
  "Set the current selection aside and release the mark to keep navigating.

Banks every whole line the active region touches (or just the current
line when no region is active), highlights them with
`donkey-banked-selection', then deactivates the mark so ordinary
navigation and a fresh selection can continue.  Repeat as often as
needed: `donkey-copy', `donkey-delete' and `donkey-yank' then act on all
banked lines at once, plus whatever region happens to be active at the
time, so the final piece never has to be banked explicitly.

`donkey-change' is the exception: it changes the character at point
and leaves banks standing.

COUNT banks that many lines starting at the one point is on, exactly as
selecting them first and pressing this once would -- the toggle below
included.  A COUNT below 2 is the plain single-line press.

This toggles, with or without a region.  With no region, point on an
already-banked line unbanks that line.  With a region whose lines are
ALL already banked, the whole block is unbanked in one press -- which
is how to take back a multi-line bank without either walking it line
by line or clearing every other bank too.  A region covering an only
partly-banked block banks the rest instead, so a block only ever
toggles off once it is uniformly on; press again to then clear it.

Banked lines are discarded automatically once `donkey-copy',
`donkey-delete' or `donkey-yank' consumes them -- and only the spans
actually acted on are spent, so banks outside the accessible portion of
a narrowed buffer survive to count again;
`donkey-clear-banked-selection' discards them without doing anything
else."
  (interactive "p")
  ;; A COUNT with no region reads as the region it would have taken
  ;; to select those lines, the toggle rule included.
  (if (or (use-region-p) (> (prefix-numeric-value count) 1))
      (let* ((span (if (use-region-p)
                       (donkey--whole-line-span (region-beginning) (region-end))
                     (donkey--whole-line-span
                      (point)
                      (save-excursion
                        (forward-line (1- (max 1 (or count 1))))
                        (line-end-position)))))
             (lines (count-lines (car span) (cdr span)))
             (unbanking (donkey--span-lines-banked-p (car span) (cdr span))))
        (if unbanking
            (donkey--unbank-span (car span) (cdr span))
          (donkey--bank-span (car span) (cdr span)))
        (deactivate-mark)
        (message "%s %d line%s (%d total)%s"
                 (if unbanking "Unbanked" "Banked")
                 lines
                 (if (= 1 lines) "" "s")
                 (donkey--banked-line-count)
                 (if unbanking "" " -- navigate, then y/d/p")))
    (let ((existing (donkey--banked-overlays-at (point))))
      (if existing
          (progn
            (donkey--delete-banked-overlays existing)
            (message "Unbanked this line (%d total)"
                     (donkey--banked-line-count)))
        (let ((span (donkey--whole-line-span (point) (point))))
          ;; The empty final line of a newline-terminated buffer spans
          ;; no text; there is nothing to bank.
          (if (>= (car span) (cdr span))
              (message "Nothing to bank -- empty final line")
            (donkey--bank-span (car span) (cdr span))
            (message "Banked this line (%d total) -- navigate, then y/d/p"
                     (donkey--banked-line-count))))))))

(defun donkey--banked-overlays-at (pos)
  "Return every banked overlay touching the line POS is on.

The test is whether the overlay and the line share any text, asked of
the whole line rather than of POS.  A list, because one line can hold
several: two banked lines joined into one keep both overlays.
Candidates come from `overlays-in'; the `donkey-banked' property tells
this package's overlays from any other package's."
  (let ((span (donkey--whole-line-span pos pos)))
    (seq-filter (lambda (ov) (overlay-get ov 'donkey-banked))
                (overlays-in (car span) (cdr span)))))

(defun donkey--banked-overlay-at (pos)
  "Return a banked overlay covering the line POS is on, or nil.

The yes-or-no form of `donkey--banked-overlays-at', for the callers
that only ask whether the line is banked.  Anything that REMOVES a bank
goes through the list, since a line can carry more than one overlay."
  (car (donkey--banked-overlays-at pos)))

(defun donkey--delete-banked-overlays (overlays)
  "Delete OVERLAYS and forget them, so the line they covered is unbanked."
  (dolist (ov overlays)
    (delete-overlay ov)
    (setq donkey--banked-overlays (delq ov donkey--banked-overlays))))

(defun donkey--banked-run-at (pos)
  "Return the contiguous banked run covering POS as (START . END), or nil.

A run is a maximal group of adjacent banked lines.  Banked lines are
stored one overlay per line (see `donkey--bank-span'), so the run is
recovered by merging the live spans and picking the merged one covering
POS's line -- the same merge `donkey--effective-line-spans' uses, so a
run is exactly the stretch that `y'/`d' would treat as one piece."
  (let ((line-start (car (donkey--whole-line-span pos pos))))
    (seq-find (lambda (span)
                (and (<= (car span) line-start)
                     (< line-start (cdr span))))
              (donkey--merge-spans (donkey--banked-spans)))))

(defun donkey-unbank-line ()
  "Unbank the banked line at point, leaving every other bank alone.

Unlike `donkey-bank-selection', this only ever removes: pressing it on
a line that is not banked reports so instead of banking it, so it is
safe to lean on when clearing up a bank without watching the state of
each line.  To drop a whole contiguous run at once use
`donkey-unbank-section'; for everything, `donkey-clear-banked-selection'."
  (interactive)
  (let ((overlays (donkey--banked-overlays-at (point))))
    (if (not overlays)
        (message "No banked line at point")
      (donkey--delete-banked-overlays overlays)
      (message "Unbanked this line (%d total)"
               (donkey--banked-line-count)))))

(defun donkey-unbank-section ()
  "Unbank the whole contiguous banked run point is standing on.

The run is every banked line adjacent to this one (see
`donkey--banked-run-at') -- exactly the stretch `y'/`d' would treat as
one piece -- so a block banked line by line comes back off in a single
press, with no need to re-select it.  Banks outside the run are left
untouched.  Reports and does nothing when point is not on a banked
line, rather than guessing at a nearby run the user may not be looking
at."
  (interactive)
  (let ((run (donkey--banked-run-at (point))))
    (if (not run)
        (message "No banked section at point")
      (let ((lines (count-lines (car run) (cdr run))))
        (donkey--unbank-span (car run) (cdr run))
        (message "Unbanked %d line%s (%d total)"
                 lines
                 (if (= 1 lines) "" "s")
                 (donkey--banked-line-count))))))

(defun donkey--map-line-spans (beg end fn)
  "Call FN once per whole line between BEG and END, with that line's span.

FN receives a (START . END) cons, one line at a time."
  (declare (indent 2))
  (save-excursion
    (goto-char (min beg (point-max)))
    ;; END clamped and an explicit stop, so this cannot spin on a
    ;; stale span.
    (let ((limit (min end (point-max)))
          (done nil))
      (while (and (not done) (< (point) limit))
        (let ((line-span (donkey--whole-line-span (point) (point))))
          (funcall fn line-span)
          (if (> (cdr line-span) (point))
              (goto-char (cdr line-span))
            (setq done t)))))))

(defun donkey--span-lines-banked-p (beg end)
  "Return non-nil if EVERY whole line in BEG..END is already banked.

Used by `donkey-bank-selection' to decide whether a region press banks
or unbanks.  Requiring ALL of them, rather than any, is what makes a
press over a partly-banked block complete it instead of clearing it:
the block only toggles off once it is uniformly on, the same rule the
single-line toggle follows."
  (let ((all t))
    (donkey--map-line-spans beg end
      (lambda (span)
        (unless (donkey--banked-overlay-at (car span))
          (setq all nil))))
    all))

(defun donkey--unbank-span (beg end)
  "Unbank every whole line in BEG..END that is currently banked.

Every overlay touching each line goes, since a line can carry more
than one."
  (donkey--map-line-spans beg end
    (lambda (span)
      (donkey--delete-banked-overlays
       (donkey--banked-overlays-at (car span))))))

(defun donkey--bank-span (beg end)
  "Bank every whole line in BEG..END that is not already banked.

Creates one overlay per LINE, so any one line can be unbanked on its
own; adjacent spans are merged at use time."
  (donkey--map-line-spans beg end
    (lambda (line-span)
      (unless (donkey--banked-overlay-at (car line-span))
        (let ((ov (make-overlay (car line-span) (cdr line-span) nil nil t)))
          (overlay-put ov 'face 'donkey-banked-selection)
          (overlay-put ov 'donkey-banked t)
          (overlay-put ov 'priority -50)
          ;; Evaporate, so an emptied buffer does not regrow the bank
          ;; over whatever replaces the line.
          (overlay-put ov 'evaporate t)
          (push ov donkey--banked-overlays))))))

(defun donkey--banked-line-count ()
  "Return how many lines are currently banked.

Counts LINES, not overlays, through `donkey--span-line-count', which
is how `donkey-copy' and `donkey-delete' count what they report."
  (donkey--span-line-count (donkey--banked-spans)))

(defun donkey--span-line-count (spans)
  "Return how many buffer lines SPANS cover in total.

Counts via `count-lines' rather than counting newlines in the extracted
text, so a banked blank line still counts as a line."
  (apply #'+ (mapcar (lambda (span) (count-lines (car span) (cdr span)))
                     spans)))

(defun donkey--consume-banked-spans (spans)
  "Unbank only the lines in SPANS, leaving every other bank alone.

What `y', `d' and `p' spend when they act on a bank -- as against
`donkey-clear-banked-selection', which is the explicit \"discard
everything\" command and says so in its name.

SPANS comes from `donkey--effective-line-spans', so it is exactly what
was acted on, region included."
  (dolist (span spans)
    (donkey--unbank-span (car span) (cdr span))))

(defun donkey--copy-banked-selection ()
  "Copy every banked line (plus any active region's lines) as one kill."
  (let* ((spans (donkey--effective-line-spans))
         (lines (donkey--span-line-count spans))
         (text (mapconcat (lambda (span)
                            (buffer-substring (car span) (cdr span)))
                          spans "")))
    (kill-new text)
    (donkey--consume-banked-spans spans)
    (deactivate-mark)
    (message "Copied %d line%s" lines (if (= 1 lines) "" "s"))))

(defun donkey--replace-banked-selection-with-paste (&optional count)
  "Replace banked lines (plus any active region's lines) with a paste.

COUNT inserts that many copies -- see `donkey--paste-times'.  The lines
are removed once regardless, so a count of zero over banked lines is a
delete, which is what replacing them with nothing means.

Banked lines are a selection, so a paste replaces them exactly as it
replaces an active region.  The paste lands where the first span
started.  The lines are deleted rather than killed, so the kill ring
still holds what is being pasted.  A paste bringing no newline of its
own gets the taken line ending restored behind it, as
`donkey--paste-restoring-line-ending' states for both line selections.

Consumes the bank, the way `donkey-copy' and `donkey-delete' do, after
the read-only check."
  (barf-if-buffer-read-only)
  (let* ((spans (donkey--effective-line-spans))
         (lines (donkey--span-line-count spans))
         (target (car (car spans)))
         ;; Read before the deletions below shift every position.
         (took-newline (eq (char-before (cdr (car spans))) ?\n)))
    ;; BEFORE the deletions, as `donkey--delete-banked-selection' does:
    ;; they shrink the buffer, and spans computed against the old text
    ;; then point past `point-max'.
    (donkey--consume-banked-spans spans)
    (dolist (span (reverse spans))
      (delete-region (car span) (cdr span)))
    (deactivate-mark)
    (goto-char target)
    (donkey--paste-restoring-line-ending (or count 1) took-newline)
    (message "Replaced %d line%s" lines (if (= 1 lines) "" "s"))))

(defun donkey--delete-banked-selection ()
  "Kill every banked line (plus any active region's lines) as one kill.

Deletes back to front so each span's positions stay valid while the
earlier ones are still being removed.

The read-only check runs first, before anything is consumed."
  (barf-if-buffer-read-only)
  (let* ((spans (donkey--effective-line-spans))
         (lines (donkey--span-line-count spans))
         (text (mapconcat (lambda (span)
                            (buffer-substring (car span) (cdr span)))
                          spans "")))
    (kill-new text)
    ;; BEFORE the deletions, not after: they shrink the buffer, and spans
    ;; computed against the old text then point past `point-max'.
    (donkey--consume-banked-spans spans)
    (dolist (span (reverse spans))
      (delete-region (car span) (cdr span)))
    (deactivate-mark)
    (message "Deleted %d line%s" lines (if (= 1 lines) "" "s"))))

;;; ---------------------------------------------------------------------------
;;; Donkey Describe Bindings
;;; ---------------------------------------------------------------------------

(defun donkey--desc-bindings-collect-leaves (map prefix)
  "Recursively walk MAP and return a list of (FULL-KEY . DEF) cons cells.

PREFIX is the accumulated key sequence string for the current path."
  (let (acc)
    (map-keymap
     (lambda (key def)
       (when def
         (let ((full-key (concat prefix (key-description (vector key)))))
           (unless (and (eq key 'remap)
                        (keymapp def)
                        (lookup-key def [self-insert-command]))
             (cond
              ((keymapp def)
               (dolist (leaf (donkey--desc-bindings-collect-leaves
                              def (concat full-key " ")))
                 (push leaf acc)))
              ((and (consp def) (keymapp (cdr def)))
               (dolist (leaf (donkey--desc-bindings-collect-leaves
                              (cdr def) (concat full-key " ")))
                 (push leaf acc)))
              (t
               (push (cons full-key def) acc)))))))
     map)
    (nreverse acc)))

(defun donkey--desc-bindings-group (full-key)
  "Return the prefix group FULL-KEY belongs to, or \"single\".

The group is everything before the first space, so \"m DEL\" and
\"m <deletechar>\" both land in \"m\".  A key description containing no
space at all -- a bare letter, a modified key, or a named function key
such as \"<backspace>\" -- is a single key."
  (if (string-match "\\(.+?\\) " full-key)
      (match-string 1 full-key)
    "single"))

(defun donkey--binding-group-name (prefix)
  "Return a human-readable group name for PREFIX."
  (cond
   ((string= prefix "single") "Single Keys")
   ((string= prefix "g")      "Goto / Scroll")
   ((string= prefix "m")      "Mark Objects")
   ((string= prefix "r")      "Search / Replace")
   ((string= prefix "z")      "Scroll")
   (t (format "%s Prefix" (upcase prefix)))))

(defun donkey--desc-bindings-insert-map (map)
  "Insert MAP's leaf bindings at point, grouped by prefix.

Single keys lead, the prefix groups follow in alphabetical order, and
keys sort within their group.  Each group gets a header, and command
names are clickable buttons."
  (let ((sorted-raw
         (sort (donkey--desc-bindings-collect-leaves map "")
               (lambda (a b)
                 (let ((ga (donkey--desc-bindings-group (car a)))
                       (gb (donkey--desc-bindings-group (car b))))
                   (cond
                    ((string= ga gb) (string< (car a) (car b)))
                    ((string= ga "single") t)
                    ((string= gb "single") nil)
                    (t (string< ga gb))))))))
    (let ((prev-group nil)
          (lines-added 0))
      (dolist (entry sorted-raw)
        (let* ((full-key (car entry))
               (def      (cdr entry))
               (group    (donkey--desc-bindings-group full-key))
               (new-block-p (not (equal prev-group group))))
          ;; A header for every group, the first included; the blank
          ;; separator only between blocks.
          (when new-block-p
            (when (> lines-added 0) (insert "\n"))
            (insert (propertize (format "  %s" (donkey--binding-group-name group))
                                'face '(bold font-lock-comment-delimiter-face)))
            (insert "\n")
            (insert (propertize (make-string 50 ?-)
                                'face 'font-lock-comment-face) "\n"))
          ;; Key column
          (insert (propertize (format "%-14s " full-key)
                              'face 'font-lock-variable-name-face))
          ;; Command name as clickable button
          (if (symbolp def)
              (insert-text-button (symbol-name def)
                                  'action (lambda (_) (describe-function def))
                                  'follow-link t
                                  'help-echo (format "Describe %s" def))
            (insert "[complex]"))
          (insert "\n")
          (setq lines-added (1+ lines-added)
                prev-group  group))))))

(defun donkey-describe-bindings ()
  "Display DONKEY's key bindings, normal state and mark run mode.

Bindings are grouped by prefix, separated by blank rows and section
headers.  Command names are clickable buttons that open their
documentation.

Mark run mode's keys are listed here because \\[describe-bindings]
cannot show them: they live in a transient map."
  (interactive)
  (unless (boundp 'donkey-normal-mode-map)
    (user-error "Variable `donkey-normal-mode-map' is not defined yet"))
  (let ((buf (get-buffer-create "*DONKEY Bindings*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; Title
      (insert (propertize "DONKEY Normal Mode Key Bindings\n"
                          'face '(bold font-lock-function-name-face :height 1.2)))
      (insert (propertize (make-string 50 ?=)
                          'face 'font-lock-comment-face) "\n\n")
      ;; Column header
      (insert (propertize (format "%-14s %s\n" "KEY" "COMMAND")
                          'face 'font-lock-keyword-face))
      (insert (propertize (make-string 50 ?-)
                          'face 'font-lock-comment-face) "\n")
      (donkey--desc-bindings-insert-map donkey-normal-mode-map)
      ;; Mark run mode, under a title of its own.
      (insert "\n")
      (insert (propertize "Mark Run Mode Key Bindings\n"
                          'face '(bold font-lock-function-name-face :height 1.2)))
      (insert (propertize (make-string 50 ?=)
                          'face 'font-lock-comment-face) "\n")
      ;; The key is looked up in `donkey-normal-mode-map' directly; no
      ;; DONKEY map is active in this help buffer.
      (insert (propertize
               (format "Live only while the mode is on -- %s starts it.\n"
                       (key-description
                        (where-is-internal 'donkey-mark-run-toggle
                                           donkey-normal-mode-map t)))
               'face 'font-lock-comment-face))
      (insert (propertize (make-string 50 ?-)
                          'face 'font-lock-comment-face) "\n")
      (donkey--desc-bindings-insert-map donkey-mark-run-mode-map)
      ;; Footer
      (insert "\n")
      (insert (propertize (make-string 50 ?=)
                          'face 'font-lock-comment-face) "\n")
      (insert (propertize "q: quit  |  RET or click: describe command"
                          'face 'font-lock-comment-face))
      ;; Buffer settings
      (special-mode)
      (setq-local buffer-read-only t)
      (setq-local truncate-lines t)
      ;; Local keymap — avoids polluting shared special-mode-map
      (let ((local-map (make-sparse-keymap)))
        (set-keymap-parent local-map special-mode-map)
        (keymap-set local-map "q"   #'quit-window)
        (keymap-set local-map "RET" #'push-button)
        (use-local-map local-map))

      (goto-char (point-min)))
    (display-buffer buf)))

;;; ---------------------------------------------------------------------------
;;; Donkey Tutor
;;; ---------------------------------------------------------------------------

(defconst donkey--tutor-content
  "DONKEY tutor
=============

This is an ordinary, editable buffer, and the text you are reading is the
text you will practice on.  Changing it is the point -- nothing here is
precious, and \\[donkey-tutor] gives you a fresh copy whenever you want one.

Lines beginning with \">>\" are things to DO.  Everything else is
explanation.

DONKEY has two states.  In NORMAL state the letter keys are commands; in
INSERT state they type.  The modeline shows which: DONKEY[N] or DONKEY[I].
If a key ever does something you did not expect, you are probably in the
other state -- press \\`C-g' to get back to NORMAL.

\\`C-g' also cancels a selection, and it is worth pressing at the end of
any lesson that made one.  A selection left active changes what the next
lesson's keys do: several of them act on the selection when there is one
and on the character or line at point when there is not.

You may also see DONKEY[E] one day, in a terminal or shell buffer.  That
is INSERT state with NORMAL state permanently out of reach, so none of
what follows applies there and \\`C-g' quits rather than switching state.
Nothing in this tutor will put you in it; \"Your Emacs still works\" at the
end says what it is for.

To stop, kill this buffer.


Lesson 1 -- moving around
-------------------------

Movement sits on the home row:

    \\[backward-char] left    \\[next-line] down    \\[previous-line] up    \\[forward-char] right

>> Walk the cursor down to the ---> line, along it, and back, using only
   those four keys.

   ---> Move along this line and back again before going on.

Bigger jumps:

    \\[forward-word] next word     \\[backward-word] previous word
    \\[beginning-of-buffer] buffer start   \\[end-of-buffer] buffer end

>> Press \\[beginning-of-buffer] to jump to the top of this buffer, then come back here.

Big jumps are easy to get wrong, so they are cheap to undo.  \\[donkey-jump-back]
returns to where you were before the last one -- reach for \\`g l' to get to
the end of the LINE, slip and hit \\`g e' instead, and you are at the end of
the BUFFER.  One keystroke puts it right.

>> Press \\[end-of-buffer] to shoot to the end of this buffer, then \\[donkey-jump-back] to come
   straight back to this line.

Press it again to keep walking back through earlier positions, or give it
a count to take several at once: \\`C-u 3' \\[donkey-jump-back] lands where three presses
land.  It is a recovery key rather than a filing system: for places you
mean to return to deliberately, Emacs' own bookmarks are the right tool.


Lesson 2 -- counts
------------------

A count works wherever \"how many\" means something, and it always means
exactly that.  Give it as C-u N before the key.

Every motion takes one, and so does every \\`m' key that selects a thing,
along with DONKEY-DELETE-KEYS, \\[donkey-copy], \\[donkey-change], \\[donkey-yank], \\[donkey-open-below], \\[donkey-open-above] and \\[donkey-visual-line-toggle].  The keys with no \"how many\" in them
-- entering INSERT at the cursor, the other two selection toggles, asking
for help -- ignore a count rather than refusing it, so a guess there
costs nothing.

If you are coming from vi, note the \\`C-u'.  A bare \\`3' does NOT start a
count here -- digits are unbound in NORMAL state.  Worse than doing
nothing, \\`3' \\[next-line] reports \"3 is undefined\" and then runs the \\[next-line] on its
own, so you move ONE line instead of three.  The count is dropped, not
obeyed.  \\`C-u 3' \\[next-line] is how it is said.

>> Press \\`C-u 3' \\[next-line] and watch the cursor move three lines in one go.

>> Put the cursor on \"one\" below and press \\`C-u 5' \\[forward-word] -- five words in
   one go, leaving the cursor just after \"five\".  \\[forward-word] lands at the END
   of each word, so counting stops there rather than on the next one.

   ---> one two three four five six seven eight nine ten

Counts work the same way on the editing commands you are about to meet.
Lessons 4 and 5 each end with a line to try one on, once the command
itself has been introduced -- a count is easier to see when you already
know what it is counting.

Where the cursor sits matters more for some commands than others, and it
is worth knowing which is which before you start counting:

  - Counting CHARACTERS counts from the cursor.  Put it on the first
    character you mean to affect; one place off and you act on the wrong
    five.
  - Counting or selecting THINGS -- a word, a sentence, a paragraph --
    does not care where inside the thing you are.  Anywhere in the word
    is anywhere in the word.

The second kind is why the exercises for it say \"anywhere\", and the
first kind is why the others name an exact character.


Lesson 3 -- typing
------------------

To type, enter INSERT state.  DONKEY then gets out of the way completely
and Emacs behaves exactly as it always does.

    \\[donkey-insert-here] before the cursor      \\[donkey-insert-after] after the cursor
    \\[donkey-insert-beginning-of-line] at the start of the line   \\[donkey-insert-end-of-line] at the end of the line
    \\[donkey-open-below] open a line below       \\[donkey-open-above] open a line above

Both open keys take a count: \\`C-u 3' \\[donkey-open-below] opens three lines below and
leaves you on the first of them, with two empty lines under it.

>> Put the cursor on the full stop below, press \\[donkey-insert-here], type the missing
   word -- it is \"dog\" -- then press \\`C-g' to return to NORMAL.

   ---> The quick brown fox jumps over the lazy .


Lesson 4 -- deleting and changing
---------------------------------

    DONKEY-DELETE-KEYS delete the character under the cursor, or the selection
    \\[donkey-change]   change it -- deletes, then drops you into INSERT

>> Fix the doubled letters on the ---> line using DONKEY-DELETE-KEYS.

   ---> Thiis liine haas extraa letterss in itt.

>> Put the cursor on the \"w\" of \"wrong\" below -- the FIRST character, not
   just somewhere in the word -- then press \\`C-u 5' \\[donkey-change], type \"right\",
   and press \\`C-g'.

   ---> This word is wrong and needs replacing.

   One character off and you replace the wrong five: starting on the
   \"r\" gives \"wrightand\".  Lesson 5 shows the way round that.

Both take a count, the same one Lesson 2 described.

>> Put the cursor on the first \"x\" below and press \\`C-u 3' DONKEY-DELETE-KEYS.
   All three go at once.

   ---> xxxand the rest of the line stays


Lesson 5 -- selecting things
----------------------------

\\[donkey-set-mark] drops a mark and starts a selection that grows as you move: press
it, move, and everything between is selected.  \\`C-g' lets go.

Pressing \\[donkey-set-mark] a second time does not let go.  It drops a fresh mark
where you are standing and starts a new selection from there, so the
one you had is gone but you are still selecting.

>> Put the cursor at the start of the ---> line, press \\[donkey-set-mark], then move
   right with \\[forward-char] and down with \\[next-line].  Press \\`C-g' to drop the
   selection.

   ---> Select part of this line by hand before meeting the shortcuts.

That is the manual way.  Usually it is quicker to select the thing you
mean:

    \\[donkey-mark-word] a word        \\[donkey-mark-symbol] a symbol
    \\[donkey-mark-sentence] a sentence    \\[donkey-mark-paragraph] a paragraph
    \\[donkey-mark-whole-buffer] the whole buffer

>> Put the cursor anywhere in the first ---> sentence and press \\[donkey-mark-sentence].  The
   whole sentence is selected, however long it is.  Press \\[donkey-mark-sentence] again:
   the selection GROWS to take the second sentence as well.

   ---> Selecting by meaning beats counting characters.  It also reads better.

All four of these grow on a second press, and keep growing until the
buffer runs out.  A count says the same thing in one go, so \\[donkey-mark-word] \\[donkey-mark-word]
and \\`C-u 2' \\[donkey-mark-word] agree.  Any other key in between ends the run, and
the next press starts a fresh selection.

All four can grow BACKWARD too: \\[donkey-mark-word-backward] adds the word before the
selection, \\[donkey-mark-symbol-backward] the symbol, \\[donkey-mark-sentence-backward] the sentence, \\[donkey-mark-paragraph-backward] the paragraph.
Either direction continues the same run, so \\[donkey-mark-word] \\[donkey-mark-word] \\[donkey-mark-word-backward] takes
two words forward and then one back.  Pressed fresh on a word, each
selects the same word its forward partner would; pressed in the gap
between two, \\[donkey-mark-word] takes the word ahead and \\[donkey-mark-word-backward] the one behind, so
from one space the two keys are the two words on either side of it.
Runs mix objects, too: each press adds one object of its own kind at
its own end, so \\[donkey-mark-word] \\[donkey-mark-sentence] grows the word selection forward to the
end of its sentence.

\\[donkey-mark-run-toggle] holds the prefix down for you.  In mark run mode the bare letters
w W b B s S mark and grow exactly as their m-prefixed keys do, so \\`M' \\`w' \\`b'
selects what three prefixed presses select, the press itself being
the first of them.  A reminder of the object keys stays in the echo
area for as long as the mode is on, and goes when the mode does -- as
one does for v, V and m v too, each naming what its own selection
answers to.  It names the keys whose SUBJECT the mode changes and no
others -- w moves by a word in normal state and marks one here --
while a key that keeps its subject is left out: h j k l still move,
J and K still work on lines, \\`.' still repeats, u and U still step
back and forward.  All of them are below.

The press arrives holding a word, that being what nearly every run
starts from -- so \\`M' alone is a selected word, and \\`M' DONKEY-DELETE-KEYS takes it.
Which word is m w's answer exactly: the one under the cursor, or from
a gap the one ahead of it, so the two keys never disagree.  The word is
the run's first press, and every letter after it grows: \\`M' is m w,
and \\`M' \\`w' is two words.

>> Put the cursor on \"three\" in the ---> line and press \\[donkey-mark-run-toggle]: one
   word is selected.  Press \\`w': two words, no prefix in sight.  Press
   \\`b' and the word before joins them.  Now press DONKEY-DELETE-KEYS to take all
   three.

   ---> one two three four five six

J and K grow the selection by lines, J at the bottom end and K at the
top, each finishing its own end's line first: from a word, \\`J' reaches
the end of its line, newline and all, \\`K' the start, and \\`J' \\`K' is the
whole line -- so a delete after the pair removes it outright, and
from a line's first word \\`J' alone is enough.  h j k l move point and
adjust the selection's near end; g h and g l are the two that own an
end apiece instead, so they add up -- \\`M' \\`g' \\`h' \\`g' \\`l' takes the whole
line's text, in either order.  * trades which end the motions hold;
the object keys trade it back before they grow, so they never need
thinking about.

\\`u' puts the run back where the last press found it and \\`U' puts it
forward again, one press per step, the motions and * included.  A run
only ever grows, so without them a press that reached further than it
looked left cancelling and starting again as the only way out.
Outside the mode those two keys are undo and redo, and inside it they
are the same idea one level down -- of the run rather than the buffer.
\\`.' repeats the last press, as it does everywhere: \\`M' \\`w' \\`.' is three
words, and each \\`.' is one step for \\`u' to take back.

>> Press \\[donkey-mark-run-toggle] on \"three\" in the ---> line above, then
   \\`w' \\`w' \\`w': four words.  Too many?  Press \\`u' twice to take two
   of them back, and \\`U' once if you went one step too far.

Paragraphs keep their m prefix there, since p and P stay the paste
keys inside the mode: m p and m P grow a run just as the bare letters
do.  A selection you already have -- from \\[donkey-set-mark], \\[donkey-visual-line-toggle] or the mark keys -- is
ADOPTED by \\[donkey-mark-run-toggle] rather than dropped, and the object keys grow it from there.

Any other key returns to normal state and does its ordinary job in the
same press, so \\[donkey-mark-run-toggle] \\`w' \\`w' DONKEY-DELETE-KEYS selects two words and deletes them with
no explicit exit.  Two presses are held back from that: a key that
does nothing -- one that is unbound -- leaves the run standing, so a
mistyped key costs a beep rather than the selection, and \\[donkey-visual-line-toggle] says to
leave the run first rather than quietly taking it over.  \\[donkey-mark-run-toggle] again --
or \\`C-g' -- drops the selection and the mode with it.

A word stops at a hyphen or underscore; a symbol runs straight through
one.  On a name held together by them the two select very different
things, which is why both keys exist.

>> Put the cursor on the \"m\" of \"mail\" in the ---> line and press \\[donkey-mark-word]:
   only that one word is selected.  Press \\`C-g', then press \\[donkey-mark-symbol]
   from the same spot: the whole name is selected, hyphen, underscore
   and all.

   ---> Call send-mail_to when the queue drains.

Which characters hold a name together is the major mode's decision, not
DONKEY's.  Hyphen and underscore usually do.  Period and comma do in
some code modes -- in Emacs Lisp, \\[donkey-mark-symbol] on \"foo.bar\" takes the whole
of it -- and not in others.  One part is DONKEY's own: a period or comma
at the END is always left out, so \\[donkey-mark-symbol] on the last name in a sentence
gives you the name without the full stop.

This buffer is plain text, where neither counts, so there is nothing
here to try that on.  It is worth knowing before the first time you
press \\[donkey-mark-symbol] in code and wonder why the answer differs.

A selection is something to act ON, so the editing keys from Lesson 4
follow straight on from it: \\[donkey-change] changes what is selected rather than the
character under the cursor.

>> Put the cursor on \"replace\" below, press \\[donkey-mark-word] then \\[donkey-change], type
   \"change\", and press \\`C-g'.  Selecting first means you never count
   the characters.

   ---> Words to replace without counting anything.

These take a count too -- the same one Lesson 2 described.

>> Put the cursor on \"alpha\" below and press \\`C-u 2' \\[donkey-mark-word].  Two words
   are selected instead of one.

   ---> alpha beta gamma delta

You can also select by delimiter.  \\[donkey-mark-inner] asks for a character and selects
what is INSIDE the nearest pair; \\[donkey-mark-outer] includes the delimiters too.

>> Put the cursor between the parentheses below and press \\[donkey-mark-inner] then \\`(' --
   the text inside is selected.  Try \\[donkey-mark-outer] then \\`(' to include the brackets.

   ---> call(this argument here)

Standing ON a delimiter, either end of the pair, nothing is asked: the
character under the cursor is the answer, so \\[donkey-mark-inner] alone is enough
there, and the \\`(' you type from habit does no harm.

>> Put the cursor on the \"(\" of the ---> line above and press \\[donkey-mark-inner] by
   itself.  The same text is selected, with no question asked.

A count means levels out, so \\`C-u 2' \\[donkey-mark-inner] \\`(' from the inner pair selects the
outer one.

>> Put the cursor on \"deep\" below and press \\`C-u 2' \\[donkey-mark-inner] then \\`('.

   ---> outer (middle (deep) middle) outer

The pairs \\[donkey-mark-inner] and \\[donkey-mark-outer] know are a FIXED list: the brackets, quotes
straight and curly, and a row of markup characters such as * = ~ and
_.  They look for the character itself and nothing else, wherever it
stands -- a \"<\" is a pair to them in plain text as much as in HTML,
and a bracket inside a string or a comment counts the same as one
outside it.

The list is yours to change.  The variable donkey-mark-pair-delimiters
holds the pairs, and adding # to it makes # a delimiter too -- any
character you like, X included.  \\[describe-variable] on it shows the pairs in force,
and the README shows the two lines an addition takes.

\\[donkey-mark-sexp-inner] and \\[donkey-mark-sexp-outer] do the same job without asking.  They read the buffer's
syntax table and find the enclosing brackets themselves, whatever kind
those turn out to be -- useful in code, where the nearest pair is as
likely to be square or curly as round, and where a bracket inside a
string is no bracket at all.  Which characters count is the major
mode's decision, as it was for \\[donkey-mark-symbol]: in an HTML buffer \"<\" and \">\"
are a pair to \\[donkey-mark-sexp-inner], and in plain text or C they are not.

    \\[donkey-mark-inner] and \\[donkey-mark-outer]   you name the delimiter, from a fixed list
    \\[donkey-mark-sexp-inner] and \\[donkey-mark-sexp-outer]   DONKEY reads it from the buffer's syntax

>> Put the cursor inside the angle brackets below and press \\[donkey-mark-inner] then \\`<':
   the inside is selected.  Press \\`C-g', then \\[donkey-mark-sexp-inner] from the same
   spot: it refuses, since this is a plain-text buffer and \"<\" is no
   bracket here.

   ---> a <tag with attributes> in text

>> Put the cursor on the \"2\" below and press \\[donkey-mark-sexp-inner].  \"1 2 3\" is selected
   without you naming the bracket.  Press \\[donkey-mark-sexp-outer] instead and the square
   brackets come with it.

   ---> (defun f (a b) [1 2 3])

Counts go outward here too, and cross bracket types on the way out.

>> From that same \"2\", press \\`C-u 2' \\[donkey-mark-sexp-inner].  The selection jumps past the
   square brackets to what is inside the surrounding parentheses.


Lesson 6 -- whole lines
-----------------------

\\[donkey-visual-line-toggle] starts a line selection anchored on the current line.  \\[donkey-visual-next-line] and
\\[donkey-visual-previous-line] then grow it a whole line at a time, and they take counts too.
So does \\[donkey-visual-line-toggle] itself: \\`C-u 3' \\[donkey-visual-line-toggle] selects three lines in one press.

>> Put the cursor on the first ---> line, press \\[donkey-visual-line-toggle], then \\[donkey-visual-next-line] twice, then DONKEY-DELETE-KEYS.
   All three lines go, leaving no blank behind.

   ---> first line to remove
   ---> second line to remove
   ---> third line to remove

The highlight stops at the end of the last line, so the newline that ends
it never LOOKS selected -- but \\[donkey-copy] and DONKEY-DELETE-KEYS take it anyway.  That
is why the three lines above go completely, instead of leaving three
empty ones behind, and why a \\[donkey-copy] here pastes back as whole lines rather
than running into whatever line it lands on.  Worth knowing before you
report it: the selection is one character shorter than what it takes.

Lines can be put back together as well as taken apart.  \\[donkey-join-line] pulls the
line BELOW up onto the one you are on, tidying the whitespace at the
join -- the direction you want when you are sitting on a line deciding
to absorb what follows.

>> Put the cursor on the first ---> line below and press \\[donkey-join-line].  The
   second line joins it.  Press it again and the third comes up too.

   ---> a sentence broken
   ---> across three
   ---> separate lines

A count joins that many lines at once, so \\`C-u 2' \\[donkey-join-line] from the first
line would have done both in one go.  On the last line there is nothing
below to pull up, so nothing happens and it tells you.

Selected lines join as one.  \\[donkey-visual-line-toggle] \\[donkey-visual-next-line] \\[donkey-visual-next-line] \\[donkey-join-line] on the three lines above
would have made the one line too, and dropped the selection -- the way
vi's J reads a selection.  A selection inside one line joins that line
with the next, so the key never sits idle on one.

Emacs\\=' own \\`M-^' is untouched and joins the other way -- it pulls the
line you are ON up onto the one above.  Every Meta binding still works
here, so both directions are available.


Lesson 7 -- copy and paste
--------------------------

    \\[donkey-copy] copy    DONKEY-DELETE-KEYS cut    \\[donkey-yank] paste    \\[donkey-yank-rectangle] paste a rectangle

Emacs' own \\[yank-pop] still steps back through earlier copies after a
paste, in both states.  DONKEY does not rebind it.

A count on \\[donkey-yank] pastes that many copies.

>> Copy the word \"echo\" below with \\[donkey-mark-word] then \\[donkey-copy], then press \\`C-u 3' \\[donkey-yank] at the
   end of the line.

   ---> echo

Pasting REPLACES whatever is selected, rather than inserting alongside
it.  That is worth knowing before the next lesson, where it is how a
whole set of banked lines gets swapped in one press.


Lesson 8 -- banking, which is DONKEY's own idea
-----------------------------------------------

Most editors make you copy one stretch at a time.  DONKEY lets you set
aside lines from anywhere in the buffer and act on all of them at once.

    \\[donkey-bank-selection] bank this line, or every line a selection touches
        (press it again on a banked line to take it back)
        (\\`C-u 3' \\[donkey-bank-selection] banks three lines, as selecting them would)
    \\[donkey-unbank-line] unbank this line     \\[donkey-unbank-section] unbank the whole run
    \\[donkey-clear-banked-selection] discard every bank

Banked lines stay highlighted while you carry on moving around.  When you
press \\[donkey-copy] or DONKEY-DELETE-KEYS, every banked line is taken at once, as a single
piece, in the order they appear in the buffer -- and \\[donkey-yank] swaps the whole set
in for whatever you copied.

>> Bank the first and third shopping lines below with \\[donkey-bank-selection], then press \\[donkey-copy].
   The message reads \"Copied 2 lines\".  Now put the cursor on the
   (paste here) line, press \\[donkey-visual-line-toggle] to select it, and press \\[donkey-yank]: both
   banked lines arrive at once, and the line you had selected is gone.

   The \\[donkey-visual-line-toggle] is what makes the marker line disappear: pasting replaces a
   selection, as Lesson 7 showed.  Without it the two lines would simply
   have been inserted, leaving the marker sitting underneath them.

   ---> milk
   ---> nails
   ---> bread

   (paste here)

Whatever is selected right now counts as well, so the last piece never has
to be banked explicitly.

>> Bank the \"milk\" line again, then select the \"bread\" line with \\[donkey-set-mark] and
   press \\[donkey-copy].  Both are copied, though only one was banked.


Lesson 9 -- columns
-------------------

\\[donkey-rectangle-mark-mode] makes the selection a RECTANGLE.  Instead of a run of text it
covers the same columns on every line it spans -- for editing a column of
a table, or the leading characters of a block of lines.

    \\[donkey-rectangle-mark-mode] start a rectangle selection (press it again to cancel)

>> Put the cursor on the first \"1\" below, press \\[donkey-rectangle-mark-mode], then \\[next-line] twice and
   \\[forward-char] twice.  Only the block of digits is highlighted, not the
   words.  Press DONKEY-DELETE-KEYS to cut it out.

   Count the presses off the highlight rather than off the characters:
   the anchor column counts as one, so reaching the third digit takes two
   presses and not three.

   ---> 111 alpha
   ---> 222 beta
   ---> 333 gamma

The selection is released by the cut, so you can put the cursor back on
the first line and press \\[donkey-yank-rectangle] straight away -- the block goes back where
it came from.  A rectangle has its own paste key: \\[donkey-yank] pastes ordinary
text, \\[donkey-yank-rectangle] pastes columns, and neither has to guess which you meant.

\\[donkey-change] on a rectangle replaces the block on EVERY line it spans, in one
go.  It asks for the replacement in the minibuffer -- \"String rectangle:\"
-- rather than dropping you into INSERT state, so type the text there and
press RET.  You stay in NORMAL state throughout; nothing appears in the
buffer until you confirm.

>> Put the cursor on the first \"7\" below, press \\[donkey-rectangle-mark-mode], then \\[next-line] twice
   and \\[forward-char] twice.  Press \\[donkey-change], type \"##\" and press RET.  All three
   rows lose their digits together.

   ---> 777 red
   ---> 888 green
   ---> 999 blue

\\[donkey-rectangle-mark-mode] on its own selects one CHARACTER on one line, so pressing \\[donkey-change]
right after it changes that single character -- correct, but rarely
what you wanted.  \\[next-line] gives the block its rows and \\[forward-char] its width, and
both have to happen before \\[donkey-change].

>> Put the cursor on the first \"5\" below, press \\[donkey-rectangle-mark-mode], and press \\[donkey-change]
   immediately -- no \\[next-line], no \\[forward-char].  One character goes.  That is
   the whole difference between the two exercises.

   ---> 555 solo

The block does not have to cover any text at all.  A rectangle with NO
width INSERTS instead of replacing, which is how the same text goes at
the front, or the end, of a run of lines at once.

\\[donkey-rectangle-mark-mode] always starts one character wide, so for a prefix take that
width straight back off with \\[backward-char] before going down.

A rectangle measures COLUMNS, though, and one character is not always
one column.  On a TAB it is eight, and on a wide character -- CJK, say
-- it is two, so \\[donkey-rectangle-mark-mode] followed by \\[donkey-change] on a tab-indented line replaces
the whole indent rather than one space of it.  Start from a character
you can see if you want to change just it.

>> Put the cursor on the \"r\" of \"red\" below, press \\[donkey-rectangle-mark-mode] then \\[backward-char],
   then \\[next-line] twice.  Press \\[donkey-change], type \"// \" and press RET.  Nothing is
   replaced; every row simply gains a front.

   ---> red
   ---> green
   ---> blue

For a suffix, start from the end of the line instead.  There is nothing
to the right to widen into, so the rectangle is already zero-width and
\\[backward-char] is not wanted.

>> Put the cursor on the first row below and press \\[move-end-of-line], then
   \\[donkey-rectangle-mark-mode], then \\[next-line] twice.  Press \\[donkey-change], type \" ;\" and press
   RET.

   ---> aaaaa
   ---> bbbbb
   ---> ccccc

A suffix lands on a COLUMN, though, not at the end of each line -- the
column the FIRST row happened to end on.  On rows of equal length, as
above, those are the same place.  On ragged rows they are not: a short
row is padded out to the column, and a long one is split at it.

Two things worth knowing before you rely on it:

  - A rectangle lives in DONKEY's own store, NOT the system clipboard.
    \\[donkey-yank] puts it back inside Emacs; another application will paste
    whatever was on the clipboard before.  A block of columns has no
    shape a flat clipboard could carry, and Emacs' own rectangle
    commands behave the same way.
  - Pasting a rectangle ONTO a rectangle needs the same number of rows.
    A three-row block over a two-row selection is refused with a message
    rather than half-applied.


Lesson 10 -- changing your mind
-------------------------------

Four keys start a selection: \\`v', \\`V', \\[donkey-mark-run-toggle] and \\`m' \\`v'.  You can go from any one
to any other without letting go first, and what becomes of the
selection you already had is the whole of what there is to know.

    \\[donkey-mark-run-toggle]     ADOPTS it.  It becomes the run's starting selection,
          and the object keys grow it from there.
    m v   REINTERPRETS it.  The region's corners become the block.
    V     starts FRESH, anchored on the line the cursor is on.
    v     RE-ANCHORS.  What you had is dropped and a new selection
          starts, empty, where the cursor stands.

>> Put the cursor on \"beta\" in the ---> line and press \\`v', then \\`l' three
   times, which selects \"bet\".  Now press \\[donkey-mark-run-toggle] and then \\`w': the run
   ADOPTED those three characters and grew them to the end of the word
   they were part of.

   ---> alpha beta gamma delta

Three exceptions are worth knowing.

Inside a mark run, \\`v' and \\`V' are REFUSED.  Neither can take a run's
selection over honestly, and both used to drop it without saying so, so
DONKEY says how to let go instead: \\[donkey-mark-run-toggle], \\`C-g', or any action key.

>> With that run still up, press \\`v'.  The selection does not move,
   and the echo area tells you how to leave.  Press \\[donkey-mark-run-toggle] to cancel.

A rectangle is the one selection \\[donkey-mark-run-toggle] will not adopt: a block has no
forward end for the object keys to own.  So it is dropped, and a fresh
run starts on the word under the cursor.

>> Draw a rectangle with \\`m' \\`v' \\`j' \\`l', then press \\[donkey-mark-run-toggle].  The block
   is gone and a single word is selected in its place.

   ---> one two three
   ---> four five six

And coming to \\`m' \\`v' FROM \\`V' gives a FULL-WIDTH block.  \\`V' selects
whole lines, so the corners the rectangle inherits are the whole line's
width -- not the narrow block you were drawing.  Start \\`m' \\`v' from
nothing, or from a \\`v' selection, when you want a narrow one.

The \\`m' keys end nothing.  \\`m' \\`w' inside a run grows the run, and
outside one it marks and leaves you selecting.  Over a rectangle it
clears the block first, no mark command being able to act on columns.
\\`%' is the same: inside a run it takes the whole buffer, and the run
carries on.

Pressing a starter twice cancels it -- \\`V' \\`V', \\`m' \\`v' \\`m' \\`v',
\\[donkey-mark-run-toggle] \\[donkey-mark-run-toggle] -- except \\`v', which re-anchors instead.


Lesson 11 -- when two selections disagree
-----------------------------------------

Banked lines are whole lines.  A rectangle is columns.  No command can act
on both at once, so DONKEY has one rule:

    THE SELECTION YOU ARE LOOKING AT WINS.  THE BANK IS THE FALLBACK.

    with a rectangle drawn    \\[donkey-copy] and DONKEY-DELETE-KEYS take the rectangle; banks stay
    with no rectangle drawn   \\[donkey-copy] and DONKEY-DELETE-KEYS take the banked lines
    with lines banked         \\[donkey-yank] replaces the banked lines

The rule is only needed for TAKING, because only \\[donkey-copy] and
DONKEY-DELETE-KEYS can be pointed at either kind of selection.  Pasting
needs no rule: \\[donkey-yank] is text and \\[donkey-yank-rectangle] is columns, and
they read from different stores.

First give \\[donkey-yank] something to paste.  A rectangle never reaches the kill
ring, so a rectangle copy on its own leaves \\[donkey-yank] with nothing to insert
and it says so.

>> Put the cursor on the \"col two\" line below and press \\[donkey-visual-line-toggle] then \\[donkey-copy].
   That is an ordinary whole-line copy, on the kill ring where \\[donkey-yank] looks.

   ---> keep this banked
   ---> col one
   ---> col two

>> Now bank the \"keep\" line with \\[donkey-bank-selection], draw a rectangle over the first
   three characters of both \"col\" lines, and press \\[donkey-copy].  The rectangle
   is copied -- and the \"keep\" line is STILL highlighted.  Nothing was
   spent.

   Copying a rectangle also drops it, the way copying any selection
   does, so there is no rectangle on screen by the time you press the
   next key.  Nothing has to be dismissed first.

>> Press \\[donkey-yank].  The banked line is replaced by the copied line: the
   bank was the selection still on screen, so it won.

>> Press \\[donkey-yank-rectangle].  The rectangle lands, exactly as it would have
   before that paste, or an hour later.  It is in its own store, and
   nothing you do to the kill ring reaches it.

Neither store is ever emptied by work on the other.  Take the rectangle
and your banks are still waiting; take the banks and the rectangle is
still in its store.

Acting on banked lines does spend them: \\[donkey-copy], DONKEY-DELETE-KEYS and
\\[donkey-yank] each empty the bank as they use it, the way any selection is
consumed by the command that acts on it.  \\[donkey-clear-banked-selection] is
the one key that clears banks WITHOUT using them, and
\\[donkey-unbank-line] drops a single line.


Your Emacs still works
----------------------

DONKEY is meant to be an addition, not a replacement.  In BOTH states
your \\`C-x' and \\`C-c' prefixes, \\[execute-extended-command], \\`C-h', isearch, the arrow keys, every
Meta binding and every package you have bound behave exactly as they
always did.  Nothing was taken away to make room for the letters above.

A modal editor cannot be entirely free, though, and the price is short
enough to state in full.

In INSERT state, one key changes: \\`C-g' returns to NORMAL state.  It still
clears the selection, and in the minibuffer it still quits, so the escape
hatch is where you expect it.  It stops a keyboard macro that is being
recorded, too; the only errand of Emacs' own \\`C-g' it skips is signaling
a quit condition, which nothing in these lessons needs.

In NORMAL state, four things differ:

  - Letters run commands instead of typing.  That is the whole idea.
  - Digits are not counts.  \\`3 j' does nothing; \\`C-u 3' \\[next-line] moves down three.
  - RET does nothing in a buffer you are editing -- a stray newline in
    NORMAL state is rarely what was meant.  In dired, magit and org-agenda
    it is NOT inert: it still opens the file, visits the entry, follows
    the link, because the key is handed back to the mode that owns it.
  - BACKSPACE and DELETE do nothing, so a slip cannot damage the buffer
    from NORMAL state.  Use DONKEY-DELETE-KEYS.

>> Try it: press \\`C-x' \\`C-s' below, or \\[execute-extended-command] and then RET to abort.  Neither is
   DONKEY's, and both work from NORMAL state exactly as usual.

Searching is Emacs' own and DONKEY leaves it alone: \\`C-s' forward,
\\`C-r' back.  Replacing is DONKEY's, on \\[query-replace] and \\[replace-regexp].

Worth knowing if you come from vi: \\`/' does nothing here and
\\[donkey-describe-bindings] lists bindings, so the search key is \\`C-s' rather than
either of them.

\\[help-with-tutorial] opens Emacs' own tutorial, which teaches those and the rest
of what DONKEY does not touch.  This one covers only what DONKEY
changed.

In dired and other special buffers DONKEY is on too, and its letters win
where they collide -- but a key the mode bound that DONKEY does not use
still works.  In dired, \\`n', \\`t', \\`q', \\`^' and \\`+' are still dired's.  Terminals
and shells (eshell, term, vterm) stay in INSERT throughout, so nothing
is suppressed underneath a running process.  Their modeline says
DONKEY[E] rather than DONKEY[I]: still INSERT, but NORMAL state cannot
be reached there at all, so \\`C-g' quits the way stock Emacs does
instead of switching state.


That is the working set
-----------------------

\\[donkey-describe-bindings] lists every binding, grouped by prefix, whenever you want the
full picture.  Everything above takes a count, and counts always mean the
same thing.

Kill this buffer when you are done."
  "Text of the DONKEY tutor, before key substitution.

A string constant rather than a file shipped beside `donkey.el': DONKEY
installs by dropping a single file onto `load-path' -- the first method
the README documents -- so a sibling data file would simply be missing
for most installations, and missing at the moment a new user is least
equipped to work out why.

Written with `substitute-command-keys' escapes rather than literal keys,
so a reader who has rebound anything is taught the keys they actually
have rather than the ones this file was written with.

One token is not a `substitute-command-keys' escape:
\"DONKEY-DELETE-KEYS\" is replaced by `donkey--tutor-delete-keys' before
substitution runs.  `\\\\[donkey-delete]' would name only one of the two
keys it is on -- `substitute-command-keys' picks whichever it finds
first, which is \"x\" -- so the tutor never mentioned \"d\" at all,
despite it being the Helix binding and the one half the audience will
reach for.")

(defun donkey--tutor-delete-keys ()
  "Return the keys running `donkey-delete', as prose: \"d or x\".

Computed rather than written into `donkey--tutor-content' so a reader
who has rebound either key is still taught the keys they actually have
-- the same promise the `substitute-command-keys' escapes make, which
`\\\\[donkey-delete]' cannot keep here because it names one binding and
this command has two.

Each key is wrapped in the \\=\\\\=` KEY \\=' escape rather than returned bare,
so `substitute-command-keys' gives it the `help-key-binding' face -- the
same treatment every other key in the tutor gets.  Returned as raw text
first, the two keys were the only ones in the whole buffer rendering as
plain prose, which reads as an oversight in a document whose entire job
is showing you keys.  The escapes are processed because this runs BEFORE
`substitute-command-keys', not after.

Sorted, because `where-is-internal' returns keymap order: that put the
vi key ahead of the Helix one purely by where the two `keymap-set'
calls happen to sit, and would silently reorder the sentence if they
were ever swapped.

Falls back to naming the command when it has no keys at all, which is
what `substitute-command-keys' does for an unbound command and is
better than a sentence ending in nothing."
  (let ((keys (mapcar (lambda (k) (format "\\`%s'" (key-description k)))
                      (where-is-internal #'donkey-delete
                                         donkey-normal-mode-map))))
    (setq keys (sort keys #'string<))
    (cond
     ((null keys) "\\[donkey-delete]")
     ((null (cdr keys)) (car keys))
     (t (mapconcat #'identity keys "/")))))

;; Progress is deliberately not saved to disk, as Emacs' own tutorial does:
;; there is nothing here worth keeping once it has been read, and a stray
;; file in the user's home directory is a worse outcome than retyping a
;; lesson.
(defun donkey-tutor ()
  "Open the DONKEY tutor: a buffer to learn DONKEY by editing it.

The tutor is an ordinary editable buffer holding its own instructions,
the way \\[help-with-tutorial] and vimtutor both work.

Returns to an existing tutor buffer rather than rebuilding it, so the
lesson survives being buried behind other windows; killing the buffer is
what starts over."
  (interactive)
  (let ((existing (get-buffer "*DONKEY Tutor*")))
    (if existing
        (progn
          ;; Returning to a tutor whose DONKEY has been switched off puts
          ;; a reader in front of a document about keys where none of the
          ;; keys work: `j\=' types a literal "j" into the lesson, and
          ;; \=`g ?\=' -- the key for reopening this very buffer -- is not
          ;; bound at all.  Nothing on screen explains it, since the text
          ;; still names every binding.
          ;;
          ;; Only when it is OFF.  If DONKEY is live the state is left
          ;; exactly as it was, INSERT included: coming back to a buried
          ;; tutor mid-exercise should return the lesson as it was left,
          ;; which is the same promise that keeps the buffer instead of
          ;; rebuilding it.
          (with-current-buffer existing
            (unless (bound-and-true-p donkey-mode)
              (donkey-mode 1)
              (donkey-enter-normal)))
          (pop-to-buffer existing))
      (let ((buf (get-buffer-create "*DONKEY Tutor*")))
        (with-current-buffer buf
          (text-mode)
          ;; DONKEY's keymap has to be live BEFORE the text is substituted.
          ;; `substitute-command-keys' resolves against the current buffer's
          ;; active maps, so substituting first renders every binding as
          ;; "M-x donkey-unbank-line" instead of "m u" -- silently, and
          ;; worst for exactly the commands a new reader most needs named.
          (donkey-mode 1)
          (donkey-enter-normal)
          ;; Before `substitute-command-keys', and with the keymap already
          ;; live above, so `donkey--tutor-delete-keys' resolves against
          ;; the same maps every other key in the tutor does.
          (insert (substitute-command-keys
                   (replace-regexp-in-string
                    "DONKEY-DELETE-KEYS"
                    (donkey--tutor-delete-keys)
                    donkey--tutor-content t t)))
          (goto-char (point-min))
          (set-buffer-modified-p nil))
        (pop-to-buffer buf)))))

;;; ---------------------------------------------------------------------------
;;; Donkey Normal Mode Keymap Definition
;;; ---------------------------------------------------------------------------

(defvar donkey-normal-mode-map nil
  "Keymap for DONKEY Normal state.")

(when (null donkey-normal-mode-map)
  (setq donkey-normal-mode-map (make-sparse-keymap)))

(suppress-keymap donkey-normal-mode-map t)

;; Navigation
(keymap-set donkey-normal-mode-map "h" #'backward-char)
(keymap-set donkey-normal-mode-map "j" #'next-line)
(keymap-set donkey-normal-mode-map "k" #'previous-line)
(keymap-set donkey-normal-mode-map "l" #'forward-char)

;; Visual Line Extension
(keymap-set donkey-normal-mode-map "J" #'donkey-visual-next-line)
(keymap-set donkey-normal-mode-map "K" #'donkey-visual-previous-line)

;; Insert mode entry
(keymap-set donkey-normal-mode-map "A" #'donkey-insert-end-of-line)
(keymap-set donkey-normal-mode-map "I" #'donkey-insert-beginning-of-line)
(keymap-set donkey-normal-mode-map "O" #'donkey-open-above)
(keymap-set donkey-normal-mode-map "a" #'donkey-insert-after)
(keymap-set donkey-normal-mode-map "i" #'donkey-insert-here)
(keymap-set donkey-normal-mode-map "o" #'donkey-open-below)

;; Editing operations
(keymap-set donkey-normal-mode-map "D" #'kill-line)
(keymap-set donkey-normal-mode-map "c" #'donkey-change)
;; Two keys, one command, deliberately.  Both match what their editor's
;; users already press: "d" is Helix's delete-the-selection, and "x" is
;; Vim's -- which deletes the character under the cursor in normal state
;; and the selection in visual state, exactly the char-or-region split
;; `donkey-delete' implements.  Note "d" is NOT operator-pending as it is
;; in Vim -- there is no "d w"/"d d"; select first, or use "D" for the
;; rest of the line.
(keymap-set donkey-normal-mode-map "d" #'donkey-delete)
(keymap-set donkey-normal-mode-map "x" #'donkey-delete)
(keymap-set donkey-normal-mode-map "C" #'donkey-comment-dwim)
;; Joining lives at "g j", NOT on "C-j" -- see `donkey-join-line' for why
;; that key was never free: it is `eval-print-last-sexp' in `*scratch*',
;; and a minor-mode map outranks the major mode.

;; Yank/Paste
(keymap-set donkey-normal-mode-map "P" #'donkey-yank-rectangle)
(keymap-set donkey-normal-mode-map "p" #'donkey-yank)
(keymap-set donkey-normal-mode-map "y" #'donkey-copy)

;; Motions
(keymap-set donkey-normal-mode-map "B" #'backward-sexp)
(keymap-set donkey-normal-mode-map "W" #'forward-sexp)
(keymap-set donkey-normal-mode-map "b" #'backward-word)
(keymap-set donkey-normal-mode-map "w" #'forward-word)
(keymap-set donkey-normal-mode-map "S" #'donkey-jump-back)

;; Visual selection
(keymap-set donkey-normal-mode-map "V" #'donkey-visual-line-toggle)
(keymap-set donkey-normal-mode-map "v" #'donkey-set-mark)
(keymap-set donkey-normal-mode-map "M" #'donkey-mark-run-toggle)

;; Wrap region with delimiter (region-active only; see donkey-wrap-region)
(dolist (ch donkey-wrap-delimiters)
  (keymap-set donkey-normal-mode-map (char-to-string ch) #'donkey-wrap-region))

;; Mark objects
(keymap-set donkey-normal-mode-map "m A" #'donkey-mark-sexp-outer)
(keymap-set donkey-normal-mode-map "m a" #'donkey-mark-outer)
(keymap-set donkey-normal-mode-map "m I" #'donkey-mark-sexp-inner)
(keymap-set donkey-normal-mode-map "m i" #'donkey-mark-inner)
(keymap-set donkey-normal-mode-map "m p" #'donkey-mark-paragraph)
(keymap-set donkey-normal-mode-map "m P" #'donkey-mark-paragraph-backward)
(keymap-set donkey-normal-mode-map "m s" #'donkey-mark-sentence)
(keymap-set donkey-normal-mode-map "m S" #'donkey-mark-sentence-backward)
(keymap-set donkey-normal-mode-map "m v" #'donkey-rectangle-mark-mode)
(keymap-set donkey-normal-mode-map "m w" #'donkey-mark-word)
(keymap-set donkey-normal-mode-map "m W" #'donkey-mark-symbol)
(keymap-set donkey-normal-mode-map "m b" #'donkey-mark-word-backward)
(keymap-set donkey-normal-mode-map "m B" #'donkey-mark-symbol-backward)
(keymap-set donkey-normal-mode-map "m l" #'donkey-bank-selection)
;; Backspace and Delete both clear the bank.  "DEL" is Emacs's name for
;; ASCII 127, which is what BACKSPACE sends -- the physical Delete key is
;; a different key entirely and arrives as <deletechar> in a terminal or
;; <delete> on a graphical frame, so all three are bound rather than
;; leaving whichever key the user reaches for reporting "is undefined".
(keymap-set donkey-normal-mode-map "m u" #'donkey-unbank-line)
(keymap-set donkey-normal-mode-map "m U" #'donkey-unbank-section)
(keymap-set donkey-normal-mode-map "m DEL" #'donkey-clear-banked-selection)
(keymap-set donkey-normal-mode-map "m <deletechar>" #'donkey-clear-banked-selection)
(keymap-set donkey-normal-mode-map "m <delete>" #'donkey-clear-banked-selection)

;; Buffer navigation
(keymap-set donkey-normal-mode-map "%" #'donkey-mark-whole-buffer)
(keymap-set donkey-normal-mode-map "." #'repeat)
(keymap-set donkey-normal-mode-map ":" #'donkey-goto-line)
(keymap-set donkey-normal-mode-map ">" #'donkey-indent-region-or-line)
(keymap-set donkey-normal-mode-map "?" #'donkey-describe-bindings)
(keymap-set donkey-normal-mode-map "U" #'undo-redo)
(keymap-set donkey-normal-mode-map "u" #'undo)
(keymap-set donkey-normal-mode-map "z z" #'recenter-top-bottom)
(keymap-set donkey-normal-mode-map "g e" #'end-of-buffer)
;; Deliberately the same command as "g e": "g e" is what Helix binds the
;; end of the buffer to, "G" is Vim's, so whichever editor a user arrives
;; from the key they already know works.  ("g g" needs no such twin --
;; both editors already use it for the start of the buffer.)
(keymap-set donkey-normal-mode-map "G" #'end-of-buffer)
(keymap-set donkey-normal-mode-map "g g" #'beginning-of-buffer)

;; Under `g' rather than on a letter of its own.  The letters vi uses for
;; motions -- f, t, F, T, e, E, n, N -- are all still free in this map, and
;; taking one for a command a reader runs once would spend a key that a
;; motion will want later.  `g t' is free too, but meant `beginning-of-buffer'
;; in 1.0.1, and silently repurposing a binding someone may still have in
;; their fingers is worse than an obscure one.  `?' already opens the
;; bindings list, so `g ?' reads as the guided version of the same question.
(keymap-set donkey-normal-mode-map "g ?" #'donkey-tutor)
(keymap-set donkey-normal-mode-map "g h" #'beginning-of-line)
(keymap-set donkey-normal-mode-map "g j" #'donkey-join-line)
(keymap-set donkey-normal-mode-map "g l" #'move-end-of-line)
(keymap-set donkey-normal-mode-map "g Q" #'fill-paragraph)
(keymap-set donkey-normal-mode-map "g q" #'fill-region)

;; Search/Replace (Multi-key)
(keymap-set donkey-normal-mode-map "r r" #'replace-regexp)
(keymap-set donkey-normal-mode-map "r q" #'query-replace)

;; Enter/Return Key (Context Aware)
(keymap-set donkey-normal-mode-map "<enter>" #'donkey-enter-dwim)
(keymap-set donkey-normal-mode-map "RET" #'donkey-enter-dwim)

;; Block raw typing keys in NORMAL state.
;;
;; All FOUR key names, not just the graphical pair.  BACKSPACE and DELETE
;; each arrive under a different name depending on the frame: a GUI frame
;; sends <backspace> and <delete>, a terminal sends DEL (ASCII 127) and
;; <deletechar>.  Only the first two were bound, so the block worked on a
;; GUI and did nothing in a terminal, where the unbound names fell
;; through to the global map and still deleted text from NORMAL state --
;; exactly what this section exists to prevent, absent for the users
;; most likely to be running `emacs -nw'.
;;
;; The `m DEL' bindings a few lines above already got this right, and
;; carry the same explanation; it simply was not carried up here.
;; Binding DEL at top level does not disturb them: `m' is a prefix, so
;; `m DEL' is a different key sequence entirely.
(keymap-set donkey-normal-mode-map "<backspace>" #'ignore)
(keymap-set donkey-normal-mode-map "<delete>" #'ignore)
(keymap-set donkey-normal-mode-map "DEL" #'ignore)
(keymap-set donkey-normal-mode-map "<deletechar>" #'ignore)
(keymap-set donkey-normal-mode-map "," #'ignore)
(keymap-set donkey-normal-mode-map "-" #'ignore)
(keymap-set donkey-normal-mode-map "/" #'ignore)
(keymap-set donkey-normal-mode-map ";" #'ignore)
(keymap-set donkey-normal-mode-map "_" #'ignore)

;;; ---------------------------------------------------------------------------
;;; Donkey Insert Mode Keymap Definition
;;; ---------------------------------------------------------------------------

(defvar donkey-insert-mode-map nil
  "Keymap for DONKEY Insert state.

Minimal keymap: all keys fall through to the major mode and global map,
providing unmodified Emacs behavior.  The `C-g' key runs the command
`donkey--exit-insert' to return to Normal state.")

(when (null donkey-insert-mode-map)
  (setq donkey-insert-mode-map (make-sparse-keymap)))

;;; ---------------------------------------------------------------------------
;;; Donkey Mode Definitions
;;; ---------------------------------------------------------------------------

(define-minor-mode donkey-normal-mode
  "DONKEY Normal state - modal navigation and editing.

Each buffer maintains its own DONKEY state independently.  When
enabled, `donkey-insert-mode' is automatically disabled and vice
versa."
  :group 'donkey
  :lighter " DONKEY[N]"
  :keymap donkey-normal-mode-map
  (when donkey-normal-mode
    (when (bound-and-true-p donkey-insert-mode)
      (donkey-insert-mode -1))))

(define-minor-mode donkey-insert-mode
  "DONKEY Insert state - passthrough to standard Emacs input.

All keys fall through to the major mode and global keymap.
\\[donkey--exit-insert] returns to Normal state.

The lighter reads \" DONKEY[E]\" instead of \" DONKEY[I]\" in a
`donkey-excluded-modes' buffer, where Normal state cannot be reached.
It is computed on redisplay, so a buffer whose major mode changes
underneath the state shows the right letter."
  :group 'donkey
  :lighter (:eval (donkey--insert-state-lighter))
  :keymap donkey-insert-mode-map
  (when donkey-insert-mode
    (when (bound-and-true-p donkey-normal-mode)
      (donkey-normal-mode -1))))

;;; ---------------------------------------------------------------------------
;;; Cursor Management
;;; ---------------------------------------------------------------------------

(defcustom donkey-cursor-normal 'box
  "Cursor shape when DONKEY Normal state is active.

Set to nil to fall back to global `cursor-type'."
  :type '(choice (const box) (const bar) (const hbar) (const hollow)
                 (cons symbol integer)
                 (const :tag "Use Global Default" nil))
  :group 'donkey)

(defcustom donkey-cursor-insert '(bar . 2)
  "Cursor shape when DONKEY Insert state is active.

Set to nil to fall back to global `cursor-type'."
  :type '(choice (const box) (const bar) (const hbar) (const hollow)
                 (cons symbol integer)
                 (const :tag "Use Global Default" nil))
  :group 'donkey)

(defcustom donkey-decscusr-denied-terminals
  '("dumb" "linux")
  "List of terminal type prefixes where DECSCUSR is suppressed.

Terminal types reported by `tty-type' that match any prefix in
this list (via `string-prefix-p') will not receive cursor shape
escape sequences.  These terminals either lack VT cursor control
or use a non-DECSCUSR mechanism for cursor shapes.

Common entries:
  \"dumb\"  — no escape sequence support whatsoever
  \"linux\" — Linux framebuffer console; uses ioctls, not DECSCUSR

Users may add entries for terminals that exhibit garbled output
when DECSCUSR sequences are sent.

\"dumb\", \"unknown\" and \"cons25\" are refused by
`donkey--terminal-supports-decscusr-p' whatever this list says;
\"dumb\" in the default value is documentation, and removing it
changes nothing."
  :type '(repeat string)
  :group 'donkey)

(defun donkey--cursor-type-to-decscusr (type)
  "Convert cursor TYPE to DECSCUSR escape sequence.

Every shape Emacs accepts for `cursor-type' has a mapping, as a bare
symbol and as the (SHAPE . SIZE) pair.  Anything else falls back to
the terminal's own default."
  (pcase type
    ('box         "\e[2 q")    ; Steady block
    ('hollow      "\e[0 q")    ; Blinking block (default)
    ('bar         "\e[6 q")    ; Steady bar
    (`(bar . ,_)  "\e[6 q")    ; Steady bar, ignore width
    ;; Bare `hbar' as well as the (hbar . WIDTH) form, as for `bar'.
    ('hbar        "\e[4 q")    ; Steady underline
    (`(hbar . ,_) "\e[4 q")    ; Steady underline, ignore height
    (_ "\e[0 q")))             ; Fallback to default

(defun donkey--terminal-supports-decscusr-p ()
  "Return non-nil if the current terminal likely supports DECSCUSR.

Returns nil for graphical frames and for terminals whose type
matches a prefix in `donkey-decscusr-denied-terminals'.
Falls back to the `TERM' environment variable when `tty-type'
returns nil, and performs a conservative guess based on known
capable terminal names.

Nil under `--batch' too, whatever `TERM' says; a test that stubs a
capable terminal binds `noninteractive' to nil."
  (and (not noninteractive)
       (not (display-graphic-p))
       (let ((tty (or (tty-type) (getenv "TERM"))))
         (when tty
           (and (not (cl-some
                      (lambda (prefix)
                        (string-prefix-p prefix tty))
                      donkey-decscusr-denied-terminals))
                (not (member tty '("dumb" "unknown" "cons25"))))))))

(defun donkey--send-cursor-sequence (type)
  "Send DECSCUSR escape sequence for TYPE to terminal.

Suppresses output on graphical frames and on terminals listed in
`donkey-decscusr-denied-terminals'.  Wraps `send-string-to-terminal'
in `condition-case' to silently absorb I/O failures.  Sends the
sequence twice with a brief pause to improve delivery reliability
on terminals that drop bytes during state transitions."
  (when (donkey--terminal-supports-decscusr-p)
    (let ((seq (donkey--cursor-type-to-decscusr type)))
      (when seq
        (condition-case nil
            (progn
              (send-string-to-terminal seq)
              (sit-for 0.01)
              (send-string-to-terminal seq))
          (error nil))))))

(defvar donkey--last-applied-cursor-settings (make-hash-table :test 'eq)
  "Hash table mapping each terminal to the SETTING value last sent.

Lets `donkey--apply-cursor-setting' skip the terminal I/O when the
value is unchanged.  Keyed by terminal: a terminal's cursor is one
shared resource, whatever buffer last set it.")

(defvar-local donkey--cursor-type-owned nil
  "Non-nil while the buffer-local `cursor-type' is one DONKEY set.

`donkey--apply-cursor-setting' with a nil SETTING removes the local
value only when this is set, so a `cursor-type' another package set
is left alone.")

(defun donkey--apply-cursor-setting (setting)
  "Apply SETTING, falling back to global default if SETTING is nil.

In terminal mode, also sends DECSCUSR escape sequence for visual
cursor change -- but only when SETTING's effective value actually
changed since the last call for this terminal, to avoid redundant
terminal I/O (see `donkey--last-applied-cursor-settings').

The buffer-local write is skipped the same way when the value already
holds.  The terminal is only driven when the current buffer is the one
in the selected window, and the terminal cache is left alone
otherwise, so the next command in a visible buffer resyncs it through
`donkey--update-cursor-passive'."
  (cond
   (setting
    (unless (and (local-variable-p 'cursor-type)
                 (equal cursor-type setting))
      (setq-local cursor-type setting)
      ;; Owned only when the write happened.
      (setq donkey--cursor-type-owned t)))
   (donkey--cursor-type-owned
    (kill-local-variable 'cursor-type)
    (setq donkey--cursor-type-owned nil)))
  (when (eq (current-buffer) (window-buffer (selected-window)))
    ;; Read after the write above; both lookups stay inside the
    ;; shown-buffer check.
    (let ((terminal (frame-terminal))
          (effective cursor-type))
      (unless (equal effective
                     (gethash terminal
                              donkey--last-applied-cursor-settings))
        (puthash terminal effective donkey--last-applied-cursor-settings)
        (donkey--send-cursor-sequence effective)))))

(defun donkey--update-cursor (&optional passive)
  "Update cursor based on current DONKEY state.

With PASSIVE non-nil, does nothing when neither `donkey-normal-mode'
nor `donkey-insert-mode' is active in the current buffer, rather than
resetting `cursor-type' to the default; that is how the global
`post-command-hook' calls it, through `donkey--update-cursor-passive'.
Without PASSIVE, from the two state hooks, the reset is what a
transition to disabled needs."
  (cond
   ((bound-and-true-p donkey-normal-mode)
    (donkey--apply-cursor-setting donkey-cursor-normal))
   ((bound-and-true-p donkey-insert-mode)
    (donkey--apply-cursor-setting donkey-cursor-insert))
   ((not passive)
    (donkey--apply-cursor-setting nil))))

(defun donkey--update-cursor-passive ()
  "Resync the cursor via `donkey--update-cursor', passively.

Registered on the global `post-command-hook' by `donkey-mode' instead
of `donkey--update-cursor' directly, so buffers DONKEY never activated
Normal/Insert state in are left untouched instead of having
`cursor-type' reset out from under them."
  (donkey--update-cursor t))

(add-hook 'donkey-normal-mode-hook #'donkey--update-cursor)
(add-hook 'donkey-insert-mode-hook #'donkey--update-cursor)

;;; ---------------------------------------------------------------------------
;;; Terminal Denylist Management
;;; ---------------------------------------------------------------------------

(defun donkey-add-denylist-entry (terminal-prefix)
  "Add TERMINAL-PREFIX to `donkey-decscusr-denied-terminals'.

Updates the custom variable and saves to your customization file."
  (interactive
   (list (read-string "Terminal type prefix to deny: ")))
  (unless (member terminal-prefix donkey-decscusr-denied-terminals)
    (customize-set-variable 'donkey-decscusr-denied-terminals
                            (append donkey-decscusr-denied-terminals (list terminal-prefix)))
    (customize-save-variable 'donkey-decscusr-denied-terminals
                             donkey-decscusr-denied-terminals)
    (message "Added \"%s\" to DECSCUSR denylist" terminal-prefix)))

(defun donkey-remove-denylist-entry (terminal-prefix)
  "Remove TERMINAL-PREFIX from `donkey-decscusr-denied-terminals'.

Updates the custom variable and saves to your customization file."
  (interactive
   (list (read-string "Terminal type prefix to allow: ")))
  (when (member terminal-prefix donkey-decscusr-denied-terminals)
    (customize-set-variable 'donkey-decscusr-denied-terminals
                            (cl-remove terminal-prefix donkey-decscusr-denied-terminals :test #'string=))
    (customize-save-variable 'donkey-decscusr-denied-terminals
                             donkey-decscusr-denied-terminals)
    (message "Removed \"%s\" from DECSCUSR denylist" terminal-prefix)))

;;; ---------------------------------------------------------------------------
;;; Donkey Minibuffer Safety
;;; ---------------------------------------------------------------------------

(defvar donkey--minibuffer-pre-state-stack nil
  "Stack of DONKEY states saved before minibuffer activations.

Each element is (BUFFER . STATE), STATE being `normal', `insert' or
nil, one element per open minibuffer, innermost first.  Global, so it
can be read from whatever buffer is current at exit.")

(defun donkey--minibuffer-current-state ()
  "Return the current DONKEY state as a symbol."
  (cond
   ((bound-and-true-p donkey-normal-mode) 'normal)
   ((bound-and-true-p donkey-insert-mode) 'insert)
   (t nil)))

(defun donkey--minibuffer-setup ()
  "Save the originating buffer's DONKEY state; never leave Normal state on.

Pushes (BUFFER . STATE) for the buffer the minibuffer was entered
from, and switches Normal state off in the minibuffer should it be on,
so the minibuffer is plain Emacs passthrough."
  ;; Capture state from the buffer that initiated the minibuffer
  (let ((orig (window-buffer (minibuffer-selected-window))))
    (push (cons orig (with-current-buffer orig
                       (donkey--minibuffer-current-state)))
          donkey--minibuffer-pre-state-stack))
  ;; Guard against donkey-normal-mode somehow already being on here
  (when (bound-and-true-p donkey-normal-mode)
    (donkey-normal-mode -1)))

(defun donkey--minibuffer-exit ()
  "Restore the originating buffer's saved DONKEY state.

Always pops `donkey--minibuffer-pre-state-stack', to stay balanced
with `donkey--minibuffer-setup', but re-enters a state only while
`donkey-mode' is still on and the buffer is still live."
  (pcase-let ((`(,buf . ,saved-state)
               (pop donkey--minibuffer-pre-state-stack)))
    (when (and (bound-and-true-p donkey-mode)
               (buffer-live-p buf))
      (with-current-buffer buf
        (pcase saved-state
          ('normal (donkey-enter-normal))
          ('insert (donkey-enter-insert)))))))


;;; ---------------------------------------------------------------------------
;;; Insert to Normal Transition
;;; ---------------------------------------------------------------------------

(defun donkey-enter-normal ()
  "Switch to NORMAL state."
  (interactive)
  (donkey-normal-mode 1))

(defvar-local donkey--deferred-overlay-cleanup-timer nil
  "Buffer-local timer for deferred overlay cleanup after exiting insert mode.")

(defvar-local donkey--just-exited-from-insert nil
  "Buffer-local guard set when exiting insert mode.

Reset on next command to prevent re-entry race conditions.")

(defun donkey--clear-transient-overlays ()
  "Clear transient overlays left by highlighting packages.

Operates on the current buffer only."
  (let ((cleared 0)
        (transient-faces
         '(sp-show-pair-match-face
           sp-show-pair-mismatch-face
           show-paren-match
           show-paren-mismatch
           hl-paren-face))
        (beg (point-min))
        (end (point-max)))
    ;; Strategy 1: Direct variable access
    (when (boundp 'sp-show-pair-overlay-list)
      (dolist (ov sp-show-pair-overlay-list)
        (when (and (overlayp ov) (overlay-start ov))
          (delete-overlay ov)
          (setq cleared (1+ cleared)))))
    (when (and (boundp 'sp-overlay)
               (overlayp sp-overlay)
               (overlay-start sp-overlay))
      (delete-overlay sp-overlay)
      (setq cleared (1+ cleared)))
    (when (boundp 'show-paren--overlay)
      (when (and (overlayp show-paren--overlay)
                 (overlay-start show-paren--overlay))
        (delete-overlay show-paren--overlay)
        (setq cleared (1+ cleared))))
    (when (boundp 'highlight-parentheses--overlays)
      (dolist (ov highlight-parentheses--overlays)
        (when (and (overlayp ov) (overlay-start ov))
          (delete-overlay ov)
          (setq cleared (1+ cleared)))))
    ;; Strategies 2 (transient faces) and 3 (smartparens keymap
    ;; overlays) share one scan.  An overlay Smartparens still tracks
    ;; goes through its own `sp--remove-overlay'.
    (dolist (ov (overlays-in beg end))
      (when (overlay-start ov)
        (let ((face (overlay-get ov 'face))
              (km (overlay-get ov 'keymap)))
          (cond
           ((or (overlay-get ov 'donkey-cleanup)
                (and face
                     (cond
                      ((symbolp face)
                       (memq face transient-faces))
                      ((consp face)
                       (cl-some (lambda (f) (memq f transient-faces)) face)))))
            (delete-overlay ov)
            (setq cleared (1+ cleared)))
           ((and km
                 (or (and (boundp 'sp-pair-overlay-keymap)
                          (eq km sp-pair-overlay-keymap))
                     (and (boundp 'sp-overlay-keymap)
                          (eq km sp-overlay-keymap))))
            (if (and (boundp 'sp-pair-overlay-list)
                     (fboundp 'sp--remove-overlay)
                     (memq ov sp-pair-overlay-list))
                (sp--remove-overlay ov)
              (delete-overlay ov))
            (setq cleared (1+ cleared)))))))
    cleared))

(defun donkey--schedule-overlay-cleanup ()
  "Schedule deferred cleanup for overlays created by post-command hooks."
  (when donkey--deferred-overlay-cleanup-timer
    (cancel-timer donkey--deferred-overlay-cleanup-timer))
  (let ((buf (current-buffer)))
    (setq donkey--deferred-overlay-cleanup-timer
          (run-with-idle-timer
           0.01 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (donkey--clear-transient-overlays)
                 (setq donkey--deferred-overlay-cleanup-timer nil))))))))

(defun donkey--reset-exit-guard ()
  "Reset the exit guard on next command.  Allow re-entry of insert mode."
  (setq donkey--just-exited-from-insert nil)
  (remove-hook 'pre-command-hook #'donkey--reset-exit-guard t))

(defun donkey--leave-insert (&optional keep-mark)
  "Leave INSERT state for NORMAL and tidy up after the change.

The state change and nothing else: the mark is let go of, Normal state
is entered, and the overlay cleanup is scheduled.  With KEEP-MARK
non-nil the mark is left as it is, for a caller whose edit was refused
and whose selection therefore still means something -- see
`donkey-wrap-region', the one caller that passes it.  `donkey--exit-insert'
is this plus the errand that belongs to `C-g' as a key.

Letting go of the mark is guarded, `deactivate-mark-hook' not being
DONKEY's; from the state change on, Normal state is entered whatever
a hook does."
  (unless keep-mark
    (condition-case err
        (deactivate-mark)
      (error (message "DONKEY: deactivate-mark failed: %s"
                      (error-message-string err)))))
  (donkey-enter-normal)
  (unless (bound-and-true-p donkey-normal-mode)
    (donkey-normal-mode 1))
  (donkey--schedule-overlay-cleanup))

(defun donkey--exit-insert ()
  "Exit insert state and enter normal mode.

The `C-g' key of INSERT state.  Leaves the state through
`donkey--leave-insert' and then stops a keyboard macro that is being
recorded -- the one errand of `keyboard-quit' that the key keeps, see
`donkey--abort-keyboard-macro-definition'.  A command that only passes
through INSERT calls `donkey--leave-insert' itself, since it has no
`C-g' to stand in for.

In the minibuffer, in a `donkey-excluded-modes' buffer, or when
`donkey-insert-mode' is not active in the current buffer, delegates to
`keyboard-quit' instead."
  (interactive)
  (if (or (not (bound-and-true-p donkey-insert-mode))
          (minibufferp)
          (donkey--excluded-mode-p))
      (keyboard-quit)
    (donkey--leave-insert)
    ;; After the state change.
    (donkey--abort-keyboard-macro-definition)))

(defun donkey--abort-keyboard-macro-definition ()
  "Stop a keyboard macro that is being recorded, the way `keyboard-quit' does.

Runs after the state transition and cannot prevent it.  Any condition
is caught and reported."
  (when (bound-and-true-p defining-kbd-macro)
    (condition-case err
        (progn
          (when (fboundp 'kmacro-keyboard-quit)
            (kmacro-keyboard-quit))
          (setq defining-kbd-macro nil))
      (error
       (message "DONKEY: could not stop the keyboard macro: %s"
                (error-message-string err))))))

(defun donkey--intercept-quit-in-insert ()
  "Intercept the quit key in insert mode by raw key event or `sp-cancel' command.

Detects a raw quit keypress (or `sp-cancel') while in `donkey-insert-mode',
then calls `donkey--exit-insert' directly to ensure state transition occurs.

Skips the minibuffer and excluded-mode buffers, where the raw key
falls through to the direct `C-g' binding.  The exit is wrapped: any
condition is caught and reported rather than allowed to remove this
function from the hook."
  (when (and (bound-and-true-p donkey-insert-mode)
             (not donkey--just-exited-from-insert)
             (not (minibufferp))
             (not (donkey--excluded-mode-p))
             (or (equal (this-single-command-keys) [7])
                 (eq this-command 'sp-cancel)))
    (setq this-command 'ignore
          donkey--just-exited-from-insert t)
    ;; Local, so the reset fires for this buffer's next command.
    (add-hook 'pre-command-hook #'donkey--reset-exit-guard -100 t)
    (condition-case err
        (donkey--exit-insert)
      ;; Reported, not swallowed.
      (error
       (message "DONKEY: error leaving Insert state: %s"
                (error-message-string err)))
      (quit
       (message "DONKEY: quit while leaving Insert state")))))

;;; ---------------------------------------------------------------------------
;;; Smartparens Integration (Opt-in)
;;; ---------------------------------------------------------------------------

(defun donkey-setup-smartparens ()
  "Set up Smartparens integration.

Call this from your config after loading `smartparens' to bind
`C-g' in smartparens overlay keymaps.  This improves reliability
of `C-g' escape in terminal mode when inside nested smartparens
overlays."
  (interactive)
  (when (and (boundp 'smartparens-mode-map)
             (keymapp smartparens-mode-map))
    (keymap-set smartparens-mode-map "C-g" #'donkey--exit-insert))
  (when (and (boundp 'sp-pair-overlay-keymap)
             (keymapp sp-pair-overlay-keymap))
    (keymap-set sp-pair-overlay-keymap "C-g" #'donkey--exit-insert))
  (when (and (boundp 'sp-overlay-keymap)
             (keymapp sp-overlay-keymap))
    (keymap-set sp-overlay-keymap "C-g" #'donkey--exit-insert)))

;; Bind C-g directly in insert mode map
(keymap-set donkey-insert-mode-map "C-g" #'donkey--exit-insert)

;; The `pre-command-hook' backup for packages that override C-g is
;; installed by `donkey-mode' alongside the rest of the global hooks,
;; and by the state modes themselves for standalone use -- see
;; `donkey--state-hooks'.

(defun donkey--recover-quit-in-insert (orig data context caller)
  "Give a quit that unwound during Insert state its meaning: exit Insert.

Installed around `command-error-function' while `donkey-mode' is on.
ORIG is the wrapped handler; DATA, CONTEXT and CALLER are what the
command loop hands it.  A quit that unwinds while Insert state is on,
outside the minibuffer and outside an excluded mode, runs
`donkey--exit-insert'; everything else is passed to ORIG untouched,
as is a quit whose exit signals, after a message.

Coverage stops at quits the command loop never sees: one that
redisplay or a timer reports as its own does not arrive here."
  (if (and (eq (car-safe data) 'quit)
           (bound-and-true-p donkey-insert-mode)
           (not (minibufferp))
           (not (donkey--excluded-mode-p)))
      (condition-case err
          (donkey--exit-insert)
        (error
         (message "DONKEY: error recovering from quit: %s"
                  (error-message-string err))
         (funcall orig data context caller)))
    (funcall orig data context caller)))

;;; ---------------------------------------------------------------------------
;;; Input Method Management
;;; ---------------------------------------------------------------------------

(defvar-local donkey--saved-input-method nil
  "Buffer-local saved input method name for restoration on Insert entry.")

(defun donkey--on-normal-entry ()
  "Deactivate any active input method when entering Normal state.

The method is saved in `donkey--saved-input-method' for
`donkey--on-insert-entry' to restore."
  (when donkey-normal-mode
    (when current-input-method
      (setq donkey--saved-input-method current-input-method)
      (deactivate-input-method))))

(defun donkey--on-insert-entry ()
  "Reactivate on Insert entry the input method `donkey--on-normal-entry' saved.

Only when no input method is already active, so one turned on by
hand in the meantime is kept."
  (when donkey-insert-mode
    (when (and donkey--saved-input-method
               (not current-input-method))
      (activate-input-method donkey--saved-input-method))))

(defun donkey--on-input-method-activate ()
  "Immediately undo an input method activated while in Normal state.

Saves it the way `donkey--on-normal-entry' does.  On the global
`input-method-activate-hook', so activation by any means is caught."
  (when (bound-and-true-p donkey-normal-mode)
    (when current-input-method
      (setq donkey--saved-input-method current-input-method)
      (let (input-method-activate-hook)
        (deactivate-input-method)))))

(defun donkey--on-input-method-deactivate ()
  "Forget the saved input method if deactivated while still in Insert state.

So the next entry into Insert state does not reactivate a method the
user turned off by hand."
  (when (bound-and-true-p donkey-insert-mode)
    (setq donkey--saved-input-method nil)))

(defun donkey-disable-input-method ()
  "Turn off the input method for good, clearing DONKEY's saved state too.

Use this rather than `deactivate-input-method' in Normal state, where
the live input method is already off and only the saved one remains.
Both are cleared, whatever the state."
  (interactive)
  (setq donkey--saved-input-method nil)
  (when current-input-method
    (deactivate-input-method)))

(add-hook 'donkey-normal-mode-hook #'donkey--on-normal-entry)
(add-hook 'donkey-insert-mode-hook #'donkey--on-insert-entry)

;;; ---------------------------------------------------------------------------
;;; Enhanced Mode Activation Logic
;;; ---------------------------------------------------------------------------

(defun donkey--ensure-default-state ()
  "Enable DONKEY Normal state unless the current major mode is excluded.

For an excluded mode, enable Insert state (passthrough) instead.  A
minibuffer gets no state at all.  Returns non-nil if a state was
enabled.

Every sweep that enables DONKEY in a buffer goes through this
function: the sweep over `buffer-list' at enable time, the startup
resweep, and `after-change-major-mode-hook'."
  (cond
   ((minibufferp) nil)
   (t
    (let ((is-excluded-p (donkey--excluded-mode-p)))
      (cond
       (is-excluded-p
        (unless (bound-and-true-p donkey-insert-mode)
          (donkey-enter-insert)
          t))
       (t
        (unless (or (bound-and-true-p donkey-normal-mode)
                    (bound-and-true-p donkey-insert-mode))
          (donkey-enter-normal)
          t)))))))

;;; ---------------------------------------------------------------------------
;;; Mode Indicator
;;; ---------------------------------------------------------------------------

(defun donkey-indicator ()
  "Return the DONKEY state indicator string for the mode line.

\" DONKEY[N]\" in Normal state, \" DONKEY[I]\" in Insert state,
\" DONKEY[E]\" in Insert state in a `donkey-excluded-modes' buffer,
where Normal state cannot be reached, and the empty string when DONKEY
is off.  For building your own mode line; the lighter of
`donkey-insert-mode' makes the same distinction."
  (cond
   ((bound-and-true-p donkey-normal-mode) " DONKEY[N]")
   ((bound-and-true-p donkey-insert-mode) (donkey--insert-state-lighter))
   (t "")))

;;; ---------------------------------------------------------------------------
;;; Global Mode Toggle
;;; ---------------------------------------------------------------------------

(defvar donkey--startup-resweep-timer nil
  "One-shot idle timer for `donkey--startup-resweep', or nil.

Stored so the disable path of `donkey-mode' can cancel it.")

(defun donkey--startup-resweep ()
  "Apply DONKEY\\='s default state to buffers created during startup.

Scheduled unconditionally from the enable path on a one-shot idle
timer, for buffers that appear after the enable sweep and never pick
a major mode -- the startup screen foremost.  An extra sweep costs
nothing: `donkey--ensure-default-state' touches only buffers holding
no DONKEY state.

Coverage stops at a buffer made by a bare `get-buffer-create' after
this timer has run and never given a major mode; none is reachable in
normal use."
  (setq donkey--startup-resweep-timer nil)
  (when (bound-and-true-p donkey-mode)
    (donkey--sweep-buffers)))

(defun donkey--sweep-buffers (&optional fn)
  "Apply FN to every buffer, one at a time, errors contained per buffer.

FN defaults to `donkey--ensure-default-state', the enable-path sweep.
The disable path passes `donkey--disable-in-buffer' instead: both
directions run the user-facing `donkey-normal-mode-hook' and
`donkey-insert-mode-hook' in every buffer, and both have the same
stake in one hook's signal not aborting the rest of the loop.

Each buffer is its own `condition-case': an error is reported, the
buffer is skipped, and the sweep goes on."
  (let ((fn (or fn #'donkey--ensure-default-state)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (condition-case err
              (funcall fn)
            (error
             (message "DONKEY: error sweeping %s: %s"
                      (buffer-name) (error-message-string err)))))))))

(defun donkey--disable-in-buffer ()
  "Clear every piece of DONKEY state from the current buffer.

The disable path's per-buffer work, run through `donkey--sweep-buffers'
so one buffer's erroring hook cannot strand the rest."
  (when (bound-and-true-p donkey-normal-mode)
    (donkey-normal-mode -1))
  (when (bound-and-true-p donkey-insert-mode)
    (donkey-insert-mode -1))
  ;; Installed buffer-locally by `donkey-visual-line-toggle'.
  (remove-hook 'deactivate-mark-hook #'donkey--clear-visual-anchor t)
  ;; Its sibling, installed by `donkey-set-mark' and
  ;; `donkey-rectangle-mark-mode'.
  (remove-hook 'deactivate-mark-hook #'donkey--clear-selection-hint t)
  (setq donkey--linear-selection-active nil)
  ;; The local reset hook `donkey--intercept-quit-in-insert' installs,
  ;; and the flag it clears.
  (remove-hook 'pre-command-hook #'donkey--reset-exit-guard t)
  (setq donkey--just-exited-from-insert nil)
  ;; The idle timer `donkey--schedule-overlay-cleanup' arms: cancelled,
  ;; not run, and only when there is one.
  (when donkey--deferred-overlay-cleanup-timer
    (cancel-timer donkey--deferred-overlay-cleanup-timer)
    (setq donkey--deferred-overlay-cleanup-timer nil))
  ;; The anchor the removed hook existed to clear.
  (donkey--clear-visual-anchor)
  ;; Banked lines are DONKEY state drawn on the buffer.
  (donkey-clear-banked-selection)
  ;; Markers are pointed nowhere before the list is dropped.
  (dolist (m donkey--position-ring)
    (set-marker m nil))
  (setq donkey--position-ring nil
        donkey--position-index 0
        donkey--last-tracked-state nil)
  (donkey--apply-cursor-setting nil))

(defconst donkey--state-hooks
  '((pre-command-hook . donkey--intercept-quit-in-insert)
    (input-method-activate-hook . donkey--on-input-method-activate)
    (input-method-deactivate-hook . donkey--on-input-method-deactivate))
  "The (HOOK . FUNCTION) entries the STATE modes need, `donkey-mode' or not.

A subset of `donkey--global-hooks': the `C-g' backup for packages
that shadow the key, and the input-method fences around Normal state.
`donkey--install-state-hooks' adds them when a state is turned on
without `donkey-mode'.")

(defun donkey--install-state-hooks ()
  "Add the hooks in `donkey--state-hooks' when a DONKEY state is on.

Registered on `donkey-normal-mode-hook' and `donkey-insert-mode-hook',
so a standalone state activation -- no `donkey-mode' involved --
installs what it needs the moment it happens.  Guarded on a state
actually being on, because those mode hooks also fire on the way off."
  (when (or (bound-and-true-p donkey-normal-mode)
            (bound-and-true-p donkey-insert-mode))
    (pcase-dolist (`(,hook . ,fn) donkey--state-hooks)
      (add-hook hook fn))))

(add-hook 'donkey-normal-mode-hook #'donkey--install-state-hooks)
(add-hook 'donkey-insert-mode-hook #'donkey--install-state-hooks)

(defconst donkey--global-hooks
  `((after-change-major-mode-hook . donkey--ensure-default-state)
    (post-command-hook . donkey--track-position)
    (post-command-hook . donkey--show-selection-hint)
    (post-command-hook . donkey--check-post-command-non-editing)
    (post-command-hook . donkey--update-cursor-passive)
    (minibuffer-setup-hook . donkey--minibuffer-setup)
    (minibuffer-exit-hook . donkey--minibuffer-exit)
    ,@donkey--state-hooks)
  "Every (HOOK . FUNCTION) `donkey-mode' adds to Emacs\\='s own hooks.

One list, so the enable and disable paths cannot drift apart.
`deactivate-mark-hook' is not here: its functions are installed
buffer-locally by the commands that need them.  The
`donkey--state-hooks' tail is shared with the standalone state modes,
which reinstall those three on their own -- see
`donkey--install-state-hooks'.")

(defun donkey--install-global-hooks ()
  "Add every hook in `donkey--global-hooks'."
  (pcase-dolist (`(,hook . ,fn) donkey--global-hooks)
    (add-hook hook fn)))

(defun donkey--remove-global-hooks ()
  "Remove every hook in `donkey--global-hooks'."
  (pcase-dolist (`(,hook . ,fn) donkey--global-hooks)
    (remove-hook hook fn)))

;;;###autoload
(define-minor-mode donkey-mode
  "Toggle DONKEY Modal Editing globally.

When enabled, DONKEY activates its dual-state system (Normal/Insert)
in all buffers.  Buffers whose major mode is in
`donkey-excluded-modes' fall back to Insert state (passthrough).

When disabled, all DONKEY state is cleared from every buffer and
standard Emacs behavior is restored.  \\[donkey-mode] or `M-x
donkey-mode' to toggle."
  :global t
  :group 'donkey
  (if donkey-mode
      (progn
        (donkey--install-global-hooks)
        (donkey--sweep-buffers)
        ;; Once more at first idle, for buffers the startup sequence
        ;; creates after this sweep; see `donkey--startup-resweep'.
        (unless donkey--startup-resweep-timer
          (setq donkey--startup-resweep-timer
                (run-with-idle-timer 0.1 nil #'donkey--startup-resweep)))
        ;; A quit that unwinds during Insert state was a C-g eaten while
        ;; Lisp was running; recover its meaning.  See
        ;; `donkey--recover-quit-in-insert'.
        (add-function :around command-error-function
                      #'donkey--recover-quit-in-insert))
    ;; Mark run mode's map lives in `overriding-terminal-local-map',
    ;; terminal-wide; `donkey--mark-run-exit' takes down all of it and
    ;; is a no-op when nothing was armed.
    (donkey--mark-run-exit)
    (donkey--remove-global-hooks)
    ;; A timer left ticking for a switched-off mode is still state.
    (when donkey--startup-resweep-timer
      (cancel-timer donkey--startup-resweep-timer)
      (setq donkey--startup-resweep-timer nil))
    ;; An entry pushed by a still-open minibuffer has lost the pop
    ;; that balanced it.
    (setq donkey--minibuffer-pre-state-stack nil)
    (remove-function command-error-function #'donkey--recover-quit-in-insert)
    (donkey--sweep-buffers #'donkey--disable-in-buffer)))

;;; ---------------------------------------------------------------------------
;;; Donkey Version
;;; ---------------------------------------------------------------------------

(defconst donkey-version (package-get-version)
  "The version of DONKEY that is loaded, from the package header.

Read once, when the file is loaded, so it is the version of the code
running in this session rather than of the file now on disk.  The same
whether donkey.el or donkey.elc was loaded.  Nil when no readable
header was found; the command `donkey-version' reports that in words.")

(defun donkey-version ()
  "Show the version of DONKEY that is loaded in this session.

Interactively, shows it in the echo area.  From Lisp, returns the
version string, or nil when the package header could not be read at
load time."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "DONKEY %s" (or donkey-version "(version unknown)"))
    donkey-version))

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------

(provide 'donkey)

;;; donkey.el ends here

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
;; Version: 1.7.2
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
only once per session."
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
  ;; The tip fires once, on the first paste where it is eligible;
  ;; `display-graphic-p' is frame-dependent.
  (when (and (not donkey--clipboard-warning-shown)
             (not (display-graphic-p))
             (not (eq system-type 'darwin))
             (not (eq system-type 'windows-nt))
             (not (donkey--detect-clipboard-tools)))
    (setq donkey--clipboard-warning-shown t)
    (message "Tip: Install wl-clipboard (Wayland) or xclip/xsel (X11) for system clipboard.")))

(defun donkey--delete-active-region-safe ()
  "Delete the active region, if there is one, to make room for a paste.

Deleted rather than killed: every caller is about to paste, and
killing first would make the yank that follows pull back the text just
removed.  The replaced text stays recoverable through \\[undo]."
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

Checked before anything is removed."
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

Refuses via `user-error', without touching the buffer, when the
selection's row count differs from `killed-rectangle's.  The
destination is cleared with `delete-rectangle', not `kill-rectangle',
which would overwrite `killed-rectangle' with what it deletes; the
top-left corner is captured before the deletion."
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

Sideways, not stacked: a rectangle is a block of columns, so repeating
it means a wider block.  N below 1 pastes nothing, matching
`donkey--paste-times'."
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

A count below 1 inserts nothing, negative included, the way
`donkey-copy' copies nothing and `donkey-delete' deletes nothing at
zero."
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
line below.

Whether anything was pasted is measured by point, not by N: a paste
of nothing restores no newline."
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

An N below 1 pastes nothing, and the lines are still removed: asking to
replace them with nothing is a delete, the same reading the banked
counterpart gives its own count of zero."
  (let* ((span (donkey--visual-line-region-bounds))
         (took-newline (eq (char-before (cdr span)) ?\n)))
    (delete-region (car span) (cdr span))
    (deactivate-mark)
    (goto-char (car span))
    (donkey--paste-restoring-line-ending n took-newline)))

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
three.  Emacs\\=' own meaning of a prefix on a paste -- which
`kill-ring' entry to pull -- is still on `C-y' in INSERT state.

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

COUNT repeats each ROW sideways rather than stacking copies, so the
block gets wider.  A COUNT below 1 inserts nothing, as it does for
\\[donkey-yank]."
  (interactive "p")
  (cond
   ;; A rectangle of nothing but empty rows is nothing to paste.
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

A `V' session's region stops before the newline that ends its last
line; the span returned takes it in, through `donkey--whole-line-span',
so the two line selections agree on whole lines.  A character-wise
region made with `v' means the characters it covers, and is returned
untouched."
  (if (donkey--visual-line-session-active-p)
      (donkey--whole-line-span (region-beginning) (region-end))
    (cons (region-beginning) (region-end))))

(defun donkey--selection-to-act-on-p ()
  "Return non-nil when an action key has a linear selection to take.

The test `donkey-copy', `donkey-delete' and `donkey-change' share for
the linear case, in place of a bare `use-region-p'.  That one is nil
for an EMPTY active region, which is right for a `v' selection that
has not moved yet and wrong for a `V' session on an empty line, whose
newline is there to take.

A session whose widened span is EMPTY -- `V' on the buffer's last
line, when that line is empty and ends without a newline -- is still
no selection.  There is nothing to take, and `kill-region' over
nothing would push \"\" onto the ring; the count branches report
\"nothing to delete\" there, as they did."
  (or (use-region-p)
      (and (donkey--visual-line-session-active-p)
           (let ((bounds (donkey--visual-line-region-bounds)))
             (< (car bounds) (cdr bounds))))))

(defun donkey--kill-rectangle-guarded (kill-command empty-message)
  "Run KILL-COMMAND, keeping `killed-rectangle' safe from a no-width take.

Interactively invokes KILL-COMMAND -- `copy-rectangle-as-kill' or
`kill-rectangle' -- with its result taken aside, and commits it to
`killed-rectangle' only when it holds any text.  Returns non-nil exactly
when it does.

A rectangle with no width takes nothing but empty rows.  Then the
store is left alone, EMPTY-MESSAGE is shown, and the variable
`deactivate-mark' is cleared so the rectangle stays on screen, both
commands having set it themselves."
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
\"p\" lands in.  An empty line is a line too: `V y' on one copies its
newline.

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
   ;; Only a copy that happened clears the selection.
   (let ((copied
          (cond
           ;; Before the bank: the live selection wins.
           ((donkey--live-rectangle-p)
            ;; A no-width copy is not a copy, so it must not clear the
            ;; selection.
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
\\[donkey-yank] to put back.

With `rectangle-mark-mode' active, kills the rectangle via
`kill-rectangle', which fills `killed-rectangle' -- the store
\\[donkey-yank-rectangle] pastes from.  Like `donkey-copy', that
reaches `killed-rectangle' only and never the system clipboard.

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
    ;; Before the bank: the live selection wins.
    ((donkey--live-rectangle-p)
     ;; The guard is for the no-width press.
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
    ;; At `point-max' `delete-char' would signal a bare `end-of-buffer'.
    (message "End of buffer -- nothing to delete")))))

(defun donkey-join-line (&optional count)
  "Pull the following line up onto this one, or join the selected lines.

The direction every modal editor uses: the line below is absorbed
into the one point is on.  Emacs\\='s own `\\[delete-indentation]' goes
the other way, pulling the current line up onto the previous one, and
is untouched, so both directions are available.

COUNT joins that many following lines, so `C-u 3 g j' collapses three
lines into this one.  A COUNT below 1 joins nothing.

On the last line there is nothing to pull up and nothing happens; the
buffer's final newline is left alone, as every other whole-line
command here leaves it.

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

A selection nothing can be done with -- one line, the last in the
buffer -- is kept, and the message is the same as without one."
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

A region that ends at the start of a line does not touch that line,
which is exactly the count `count-lines' gives for the region's two
ends.  A `V' session ends at the END of its last line, so all of its
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
`donkey-visual-anchor', each being the state of one kind of selection
that only `deactivate-mark-hook' would otherwise take down.

Called by every DONKEY command that establishes a new selection,
before its own `push-mark' or `set-mark'.  The four delimiter commands
call it only once their search has found a pair, so a refused press
leaves the old selection standing whole, kind and all; the object
commands call it before their search, whose refusals mark nothing."
  (when (bound-and-true-p rectangle-mark-mode)
    (rectangle-mark-mode -1))
  (setq donkey-visual-anchor nil))

(defun donkey--clear-visual-anchor ()
  "Clear `donkey-visual-anchor' whenever the mark is deactivated.

On `deactivate-mark-hook', installed buffer-locally by
`donkey-visual-line-toggle' when it sets the anchor, so the anchor
never survives its region."
  (setq donkey-visual-anchor nil))


(defun donkey--visual-line-session-active-p ()
  "Return non-nil if point is continuing an active visual-line selection.

Requires an active region, a recorded `donkey-visual-anchor', and that
the mark still sits where a visual-line command would have left it --
either exactly AT the anchor (a line beginning) or at that anchor
line's end.  Those are the only two values `donkey-visual-line-toggle',
`donkey-visual-next-line' and `donkey-visual-previous-line' ever set
the mark to, depending on which side of the anchor point is on.

An anchor outside the accessible portion is not a session this can
continue."
  (and (region-active-p)
       donkey-visual-anchor
       ;; An anchor outside the accessible portion is no session.
       (<= (point-min) donkey-visual-anchor)
       (<= donkey-visual-anchor (point-max))
       (mark)
       (or (= (mark) donkey-visual-anchor)
           (= (mark) (save-excursion
                       (goto-char donkey-visual-anchor)
                       (line-end-position))))))

(defvar donkey--visual-line-hint
  "Visual line: J/K whole lines, j/k by char, V to cancel"
  "The echo-area reminder shown while a visual-line session is active.

Shown by `donkey-visual-line-toggle' at entry and kept visible across
the session's motions by `donkey--show-selection-hint'.")

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

Motions only, and only motions that never message, so a command that
said something of its own keeps its echo: a whitelist of the sessions'
own silent motions.  The `rectangle-' entries are the same motions
under the names `rectangle-mark-mode' remaps them to.  After any
command not listed here the reminder waits for the next listed
motion.")

(defun donkey--repaint-hint (hint)
  "Show HINT in the echo area without logging it.

Repainted from `post-command-hook' by the selection reminders, so
*Messages* is not filled with copies."
  (let ((message-log-max nil))
    (message "%s" hint)))

(defvar donkey--linear-selection-hint
  "Linear selection: any motion extends, v re-anchors, C-g to cancel"
  "The echo-area reminder shown while a `donkey-set-mark' selection lives.

Shown by `donkey-set-mark' at entry and kept VISIBLE across the
selection's motions by `donkey--show-selection-hint', the way
`donkey--visual-line-hint' and `donkey--mark-run-mode-hint' stay up
for their own modes.")

(defvar donkey--rectangle-hint
  "Rectangle: any motion sizes the block, m v or C-g to cancel"
  "The echo-area reminder shown while `rectangle-mark-mode' is on.

Shown by `donkey-rectangle-mark-mode' at entry and kept VISIBLE across
the block's motions by `donkey--show-selection-hint'.")

(defvar-local donkey--linear-selection-active nil
  "Non-nil while a selection `donkey-set-mark' started is still live.

Set by `donkey-set-mark', cleared by `donkey--clear-selection-hint' on
`deactivate-mark-hook'.  Buffer-local, as a selection is.")

(defun donkey--clear-selection-hint ()
  "Forget a linear selection, and take its reminder off the screen.

On `deactivate-mark-hook', installed buffer-locally by
`donkey-set-mark' and `donkey-rectangle-mark-mode' when each starts a
selection.  The reminder is taken down only when it is what is
showing, so a command that said something of its own keeps its echo;
both the linear and the rectangle reminder are checked, since both
selections end here."
  (setq donkey--linear-selection-active nil)
  (when (member (current-message)
                (list donkey--linear-selection-hint donkey--rectangle-hint))
    (message nil)))

(defun donkey--show-selection-hint ()
  "Keep the live selection's reminder visible across its own motions.

On `post-command-hook' for the life of `donkey-mode' -- registered in
`donkey--global-hooks' rather than per session, so two selections in
two buffers cannot strand or double it.

One function for all three selections, and the precedence is the
`cond': a rectangle first, then a visual-line session, then a linear
selection.  Mark run mode repaints from its own hook.  Repaints after
the motions in `donkey--hint-motions' and after nothing else.
Guarded, not signaling."
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
toggles.

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
      ;; hook `donkey--visual-line-start' installed.
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

Inside a rectangle this moves as `j' does there, through
`rectangle-next-line', which keeps the column, so the block grows by a
row.  `donkey-visual-previous-line' mirrors it."
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
`rectangle-previous-line'."
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
enabling, `rectangle-mark-mode' reinterprets that existing region as
a rectangle using its own corners.  Only a fresh selection is widened
by one column, and not at the end of a line or of the buffer."
  (interactive)
  (if (bound-and-true-p rectangle-mark-mode)
      (progn
        (rectangle-mark-mode -1)
        (deactivate-mark)
        (message "Rectangle: canceled"))
    (let ((had-active-region mark-active))
      (rectangle-mark-mode 1)
      (add-hook 'deactivate-mark-hook #'donkey--clear-selection-hint nil t)
      ;; One column of width for a fresh selection.  Not at the end of
      ;; the buffer, where there is nothing to widen into, and not at
      ;; the end of a line, where `right-char' would move the block to
      ;; column 0 of the next line.
      (unless (or had-active-region (eolp))
        (condition-case nil
            (right-char 1)
          (end-of-buffer nil)))
      ;; Last, so it is what stays.
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

It names the delimiters rather than listing them; \\[describe-variable]
on the variable shows the pairs in force."
  "Delimiter (see donkey-mark-pair-delimiters): ")

(defun donkey--mark-pair-unsupported-error (char)
  "Signal a `user-error' for CHAR not in `donkey-mark-pair-delimiters'.

A `user-error', the typo being an ordinary one.  CHAR is spelled the
way a key binding is, through `single-key-description', so an
unprintable key reads as its name."
  (user-error "Unsupported delimiter `%s'; see donkey-mark-pair-delimiters"
              (single-key-description char)))

(defun donkey--mark-pair-open-for (char)
  "Return the OPEN character of the pair CHAR belongs to.

CHAR itself when it opens a pair, and the opener when it closes one,
so the prompt takes \\=`)\\=' for \\=`(\\='.  A symmetric delimiter
answers itself.  Anything else comes back unchanged, for the caller to
reject by name."
  (cond ((assq char donkey-mark-pair-delimiters) char)
        ((rassq char donkey-mark-pair-delimiters)
         (car (rassq char donkey-mark-pair-delimiters)))
        (t char)))

(defun donkey--pair-delimiter-already-taken ()
  "Do nothing, silently, and keep the mark command's repeat alive.

What a delimiter key runs for one press after `m i' or `m a' resolved
its delimiter from the buffer.  Silent, and `this-command' is set to
`last-command', so a following `m i' still reads as a repeat."
  (interactive)
  (setq this-command last-command))

(defun donkey--suppress-one-pair-delimiter ()
  "Make the NEXT key harmless if it names a delimiter.

`m i' and `m a' read their delimiter from the character at point when
point is on one, so the delimiter a reader types from habit as the
third key would otherwise run as a command of its own -- every
delimiter is bound to `donkey-wrap-region'.  The press is swallowed
for one key, once a pair has been found: \\=`m i\\=' alone selects,
\\=`m i (\\=' selects and the paren does nothing, and a second paren
wraps.  A repeat arms it too.  Nothing is armed for a call from Lisp:
only when `this-command' is one of the two commands."
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
than from a key."
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

When ON-OPENER, the search goes forward from point for the close.
For a symmetric delimiter, where point may be sitting on the pair's
closing occurrence, a failed forward search falls back to treating
point as the closer and searches backward for the opener.  Asymmetric
pairs are scanned depth-aware, so nested occurrences of the same
delimiter resolve to the enclosing pair; symmetric ones use a plain
search, nesting having no meaning for them.

START-POS is the position of the opening delimiter; END-POS is the
position immediately after the closing delimiter.  Searches are
case-sensitive whatever the buffer's `case-fold-search'.  Point is
left where it was, found or not, and a miss is a `user-error'."
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

With point in \"writing\" in

    \"No use \"writing on paper.\" That\"

level 1 gives \"writing on paper.\" and level 2 gives the whole of the
outer quotation.

Signals a `user-error' when the text runs out of delimiters before the
count does, the same one the nesting-aware path signals."
  (let ((extra (1- levels))
        ;; Case-sensitive, like `donkey--mark-pair-positions'.
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
      ;; Widen first, with the real count, then flatten LEVELS so the
      ;; depth loop below is a no-op.
      (setq span (donkey--mark-pair-widen-symmetric open-char span levels)
            levels 1))
    (dotimes (_ (1- levels))
      (when (<= (car span) (point-min))
        (user-error "No enclosing `%c' beyond that level" open-char))
      (setq span (save-excursion
                   (goto-char (1- (car span)))
                   ;; Running out of enclosing pairs is ordinary -- a bare
                   ;; \\[universal-argument] asks for four levels -- so
                   ;; it is a `user-error' naming the level, not the
                   ;; scan's own message.
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
command repeats, so a stale entry is never consulted.")

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
  ;; A repeat re-runs the original search one level wider from the
  ;; same anchor, so repeating agrees with counting; it never prompts,
  ;; so the delimiter typed next is loose and swallowed once the pair
  ;; is found.
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
      ;; Refused before anything is marked, so a refusal changes
      ;; nothing.  An empty pair is a `user-error'; `m a' on it still
      ;; works, the delimiters being content there.
      (when (>= start end)
        (user-error "Empty selection between %c and %c" open-char close-char))
      ;; Only now, with a pair in hand, is the previous selection's
      ;; kind let go of.
      (donkey--ensure-non-rectangle-selection)
      ;; Mark at the end, point at the start, as every mark command.
      (push-mark end)
      (goto-char start)
      (activate-mark)
      (setq donkey--mark-pair-state
            (list anchor open-char close-char on-opener level))
      ;; Only a press that marked has a next key to protect.
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

The delimiter key is harmless when the prompt was skipped.  For
asymmetric pairs (e.g. `(' and `)'), nested occurrences of the SAME
pair resolve to the correctly balanced match -- e.g. the outer `(' of
\"(a(b)c)\" selects \"a(b)c\", not just up to the first `)' found.

This is a plain character scan, not syntax-table aware: unlike
`donkey-mark-sexp-inner', it does not know about strings or comments,
so the delimiter character appearing inside one can still throw off
the match.  Use `donkey-mark-sexp-inner' for balanced expressions in
real code instead.

With no matching pair either way (point outside any delimiter, and no
enclosing pair to fall back on), signals an error and leaves point
exactly where it was -- it never lands somewhere else in the buffer as
a side effect of the failed search.

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
same command repeats.")

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
expression has been found."
  ;; A repeat widens the original search from the same anchor.
  (let* ((state (and (donkey--mark-extending-p) donkey--mark-sexp-state))
         (anchor (if state (car state) (point)))
         (levels (+ (if state (cdr state) 0) (max 1 (or count 1)))))
    ;; Under `save-excursion', so a refused press leaves point where
    ;; it was.
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
      ;; With the expression in hand, and not before.
      (donkey--ensure-non-rectangle-selection)
      ;; Mark at the end, point at the start.
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

Point is left at the START of the selection and the mark at its end,
which is where `mark-sexp' leaves them and where the other DONKEY
mark commands leave them.

COUNT selects how many levels out to go."
  (interactive "p")
  (donkey--mark-sexp-select nil count))

(defun donkey--back-to-symbol-char ()
  "Move point back onto the last symbol character before it.

Punctuation, quotes and brackets are all crossed, none of them being
part of a symbol; the search is for a character of word or symbol
syntax and stops on the first.  Nothing moves when there is no such
character behind point, and the return value says which: non-nil when
point moved onto a symbol character, nil when it stayed."
  (let ((landing (save-excursion (skip-syntax-backward "^w_") (point))))
    (when (> landing (point-min))
      (goto-char (1- landing)))))

(defun donkey--trim-symbol-prefix ()
  "Move point forward over punctuation stuck to the front of the symbol ahead.

The mirror of `donkey--trim-symbol-punctuation', called with point at
the start of a backward symbol run.  Only PUNCTUATION syntax is shed:
an expression prefix -- \\=' and \\=` and # in Lisp -- introduces the
form after it and belongs with it, so marking \\='bar still gives
\\='bar.  The ceiling is the end of the sexp just traversed, so the
thing just added is never trimmed away."
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

Anything of punctuation SYNTAX goes the same way.  The two literal
characters stay named because Lisp gives \".\" symbol syntax and \",\"
the syntax of an expression prefix.  Stops short when the symbol IS
that punctuation, so \"...\" in Lisp keeps itself, and the floor is
the start of the sexp just traversed, so the thing just added is never
trimmed away."
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

`thing-at-point' reports the whole buffer as the `word' at point when
the buffer holds no word character anywhere, so the answer is checked
for a character of the right syntax.  `symbol' is asked the same way,
so the two commands cannot drift apart."
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

A NEGATIVE count keeps its meaning, the objects behind the one point
normalizes onto."
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

Whether a gap has an object behind it at all, asked by the sentence
and paragraph keys before stepping back from a gap.  The newline is
named because `[:space:]' is whitespace SYNTAX, which a newline has not
got in every major mode."
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
`donkey-mark-word' for the press that made that matter."
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
through its own steps.  None of them names an object.

Members of `donkey--mark-run-commands', so a run carries on across
them, but `donkey--mark-extending-p' continues only a VISIBLE run
through them: a motion member with no active region is just the
cursor having moved.")

(defconst donkey--mark-run-inert-commands
  '(undefined ignore donkey-mark-run-refuse)
  "The commands that change nothing, so a mark run survives them.

Every printable key the normal state leaves unbound resolves to
`undefined', and \`DEL' to `ignore'.  Listed here, they keep the mode
and count as companions in `donkey--mark-extending-p', so a mistyped
key costs a beep and nothing else.  `donkey-mark-run-refuse' exists to
leave a run standing, so it is here too.")

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
open for.  The adjusters are included so that adjusting point mid-run
reads as the run continuing; the plain motions stay out, so any of
them still ends a run.  `donkey-mark-run-toggle' itself is not a
member: a press that marked or adopted renames itself to the member
that did it.  `donkey-mark-run-adopt' is one.  `donkey-mark-whole-buffer'
is a member without being growable: \`%' replaces the selection, and
membership is what gets the press recorded for \`u' to take back.

The delimiter marks are not members: they count LEVELS, not objects.
`donkey-rectangle-mark-mode' is not, a rectangle having no forward
end.  And a `v' selection is not grown by any of these.")

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
of its own kind at its own end.

`this-command' is checked for being set at all before it is compared,
so a call from outside the command loop, where both it and
`last-command' are nil, does not read as a repeat."
  (and this-command
       (or (eq last-command this-command)
           (memq last-command companions))
       (mark t)
       ;; A motion member continues only a LIVE run; `mark-active', not
       ;; `region-active-p', since `transient-mark-mode' need not be on.
       (or (not (memq last-command donkey--mark-run-adjusters))
           mark-active)
       t))

(defun donkey--normalize-mark-run ()
  "Put point back at the selection's start and the mark at its far end.

The layout every object mark command relies on: point at the start,
mark at the forward end, so the forward keys grow a run by pushing the
mark and the backward keys by walking point.  Three things break it,
deliberately: \`*' trades the ends, a motion may walk point past the
mark, and a negative count finishes with the mark at the start.
Swapping back first makes every object key mean the one thing it
means everywhere else; a no-op on a run laid out the right way round.

Called from the EXTENDING branches only: a fresh press builds its own
layout, and the motions and \`*' must keep theirs.  Every read is
`(mark t)', so a run whose region a hook deactivated still answers."
  (when (and (mark t) (> (point) (mark t)))
    (let ((start (mark t)))
      (set-mark (point))
      (goto-char start))))

(defun donkey--mark-run-continuing-p ()
  "Return non-nil when this press continues a run, squaring it up first.

The continuation test and the squaring up in one call, so a key that
continues a run cannot see it sideways.  `donkey--mark-extending-p'
stays separate and pure: the delimiter marks ask it with no companions
and must not be squared up, counting levels rather than objects."
  (when (donkey--mark-extending-p donkey--mark-run-commands)
    (donkey--normalize-mark-run)
    t))

(defun donkey-mark-run-cancel ()
  "Drop the active selection and let mark run mode lapse.

Bound to \`M' inside `donkey-mark-run-mode-map'.  It is deliberately
no member of
`donkey--mark-run-commands' and no key of the mode map's family row,
so running it fails `donkey--mark-run-mode-keep-p' and the transient
map is gone by the next key."
  (interactive)
  (deactivate-mark)
  (message "Mark run: canceled"))

(defun donkey-mark-run-refuse ()
  "Refuse a key that would silently throw the mark run away.

Bound to \`V' and \`v' inside `donkey-mark-run-mode-map'.  Both start
a selection of their own, and would drop the run without using it.
\`M' or \`C-g' drops the selection, and any key that USES it -- `d',
`y', `p', `c', `x' -- takes it and goes.  Allowed by
`donkey--mark-run-mode-keep-p', so the refusal leaves the mode exactly
as it found it."
  (interactive)
  ;; Named after the key that reached it; from Lisp there is no such
  ;; key.
  (let ((key (key-description (this-command-keys))))
    (user-error "%s would drop the mark run: leave with M, C-g or an action key"
                (if (or (string-empty-p key) (not (called-interactively-p 'any)))
                    "That"
                  key))))

(defun donkey-mark-run-left (&optional count)
  "Move point back COUNT characters without ending the mark run.

Bound only inside `donkey-mark-run-mode-map': the plain motions must
keep ending runs, so the mode wraps its own, and point adjusts the
selection's near end without changing what any key means outside the
mode."
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
only while `last-command' is `next-line' or `previous-line', by name,
so a wrapper press is presented as the motion it stands in for.
Anything else is passed through untouched, and the caller binds
rather than sets, the real `last-command' being what tells
`donkey--mark-extending-p' the run is still live."
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

The pair owns FIXED ENDS, the way the object keys do: this one takes
the selection's start, `donkey-mark-run-line-end' its end, so they add
up -- `M g h g l' is the line's text from one edge to the other, in
either order.  With no run in progress the key is just a motion."
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
`g l'; see `donkey-mark-run-line-start' for the pair's fixed ends.

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

ARG is passed on raw when there is no run to stretch, where the key is
the ordinary `beginning-of-buffer' and reads it as that command does.
A run reaches the edge, ARG or no ARG."
  (interactive "P")
  (if (donkey--mark-run-continuing-p)
      (progn
        ;; `goto-char', not `beginning-of-buffer', which pushes a mark
        ;; whenever no region is active.
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

Stands in for `g e' and \`G'.

This one pushes the MARK, the forward end, so it cannot shrink what is
selected -- `point-max' is never behind the position it is measured
from.  Unlike `donkey-mark-run-line-end' it needs no measuring at all:
a buffer has one end, wherever the mark happens to sit.

ARG is passed on raw when there is no run to stretch, as
`donkey-mark-run-buffer-start' passes it."
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
this one, so the newline that ends the selection is inside it: that
one character is the difference between a line selection and the text
of a line, and `M J d' removes the line outright as `V d' does.  The
highlight reaches the next line's first column.

COUNT lines; a COUNT below 1 is treated as 1, the reading the
backward keys give theirs -- the other direction is \`K'."
  (interactive "p")
  (let ((n (max 1 (or count 1))))
    (if (donkey--mark-run-continuing-p)
        ;; `forward-line' from the mark adds N whole lines, completing a
        ;; mid-line mark's line first; at the end of a buffer with no
        ;; final newline it stops at `point-max'.
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

A fresh press marks the whole line, newline included.  The extending
branch walks POINT, which sits at a line START already, so the end the
newline belongs to is the one the partner owns.

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
`M w' selects.

A member of `donkey--mark-run-adjusters', so the run carries on and the
visible-run guard applies to what follows.  Refuses without an active
selection: `exchange-point-and-mark' would leap to some stale mark
and re-activate whatever lies between, which is not what a key for
trading the ends of a VISIBLE selection can mean."
  (interactive)
  ;; `mark-active', so the key works with `transient-mark-mode' off.
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
is seven.  `donkey--mark-run-press-command' sees through the key for
the keep test and the history, so each \`.' is one more press and one
more step for \`u' to take back.  `M .' repeats the toggle itself,
which takes the selection up again as if freshly pressed.

\`p' and \`P' are missing DELIBERATELY: they stay the paste keys, the
one ordinary edit the mode would otherwise make unreachable, and
paragraphs keep their `m' prefix.  \`m' is missing too, so `m w'
inside the mode reaches the normal map and grows the run: `M m w w'
selects three words, and `m p' and `m P' grow a run by paragraphs.")

(defvar donkey--mark-run-mode-hint
  "Mark run: w/b words, W/B symbols, s/S sentences, m p/m P paragraphs, * other end, M to cancel"
  "The echo-area reminder shown while mark run mode is active.

Kept visible for the whole mode by `donkey--mark-run-mode-post-command'.
It names the keys whose SUBJECT the mode changes, and no others; a key
that keeps its subject, such as the motions, is left out.  Paragraphs
keep their \`m' prefix, so they are spelled in full.  The complete
list is in the README and the tutor.")

(defvar donkey--mark-run-history nil
  "The run's earlier shapes, newest first, for \`u' to step back to.

Each entry is (POINT MARK ACTIVE), the selection as it stood BEFORE
one press changed it.  Pushed by `donkey--mark-run-mode-pre-command',
popped by `donkey-mark-run-step-back', and emptied whenever the mode is
disarmed, a run's history meaning nothing to the next run.")

(defvar donkey--mark-run-redo nil
  "The shapes \`u' has stepped back out of, newest first, for \`U'.

The other half of `donkey--mark-run-history', and the ordinary redo
bargain: `donkey-mark-run-step-back' pushes what it is leaving here
before it restores, `donkey-mark-run-step-forward' pops it and hands
it back, and any OTHER press in the run empties it, a new branch
having nothing to redo onto.  Emptied with the history whenever the
mode is disarmed.")

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
`repeat' here, `repeat' renaming `this-command' only once it runs.

It also names the nameless press -- see the comment below -- which is
the one thing here that is not about the history.

Guarded, not signaling: a function that errors on `pre-command-hook'
is silently removed for the session."
  ;; Name the nameless press: a sequence that resolved to nothing
  ;; arrives with `this-command' nil, and `undefined' -- a family
  ;; member -- is what its other spelling, a single unbound key, runs.
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

What it leaves is kept, so `donkey-mark-run-step-forward' on \`U' can
hand it back, until any other press in the run drops the redo.  A
member of `donkey--mark-run-commands', so the run carries on: `M w w u
w' grows from the restored selection instead of marking afresh."
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
shape before the first recorded press comes back as it was."
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
nothing else, and not while a count is being typed: count entry is
told apart by `prefix-arg', since the keys of a count arrive under the
name of the family member they followed.

The exit is the transient map's backstop, for a command that armed
the map while it was already running.  A command that is neither a
family member, nor part of entering a count, nor `donkey-mark-run-toggle'
itself ends the mode here; and a mode armed inside a keyboard macro,
as noted at entry in `donkey--mark-run-armed-in-macro', ends with the
first command to finish outside one.

Guarded, not signaling: a function that errors on `post-command-hook'
is silently removed for the session."
  (cond
   ((and donkey--mark-run-armed-in-macro (not executing-kbd-macro))
    (donkey--mark-run-exit))
   ((memq this-command donkey--mark-run-commands)
    ;; A count's keys arrive under the family member's name.
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

A key that DOES NOTHING does not end it either, nor does a mistyped
sequence that reached no command at all, nor `donkey-mark-run-refuse'
-- see `donkey--mark-run-inert-commands'.  \`.' is judged by the
command it repeats, through `donkey--mark-run-press-command'."
  (or (memq (donkey--mark-run-press-command) donkey--mark-run-commands)
      ;; A sequence that resolves to nothing arrives as nil: the same
      ;; mistype as an unbound key, under another spelling.
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

Reached on both sides of the foreign command that ends the mode, and
the reminder is cleared only when it is what is showing, so a command
that said something of its own keeps its echo."
  (remove-hook 'pre-command-hook #'donkey--mark-run-mode-pre-command)
  (remove-hook 'post-command-hook #'donkey--mark-run-mode-post-command)
  (setq donkey--mark-run-history nil)
  (setq donkey--mark-run-redo nil)
  ;; The reminder must not outlive the mode; cleared only when it is
  ;; what is showing.
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
  ;; Already emptied by the exit above; kept as the place where a
  ;; run's steps begin.
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

Active and not empty: `donkey-set-mark' activates a mark without
covering anything yet, and that is not a selection to adopt.  One
test for `donkey-mark-run-toggle' and `donkey-mark-run-adopt' alike.
`mark-active' and `(mark t)', so a selection made with
`transient-mark-mode' off is still one."
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
fixed: forward keys push the mark, backward keys walk point.

A visual-line session is widened to whole lines as it is taken,
through `donkey--visual-line-region-bounds'; every other selection is
adopted exactly as it shows.  Any visual-line anchor is cleared: the
run owns the selection now, and a later \`V' starts a fresh session.

Refuses without a selection to adopt, and an EMPTY active region is no
selection; `donkey-mark-run-toggle' enters the mode empty-handed in
that case instead.  A member of `donkey--mark-run-commands':
`donkey-mark-run-toggle' renames its adopting press to this name, and
membership is what lets the object key that follows grow the adopted
selection instead of marking afresh over it."
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

The paragraph pair is the one the mode does not letter: \`p' and
\`P' stay the paste keys here, and `m p' and `m P' grow a run from
inside the mode.  The reminder in the echo area stays up for the
whole mode.

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
a visual-line session cannot own a mark run's selection.  Leave the
run first -- \`M', \`C-g', or any key that uses the
selection -- and \`V' is itself again.

\`u' puts the run back where the last press found it and \`U' puts it
forward again, one press per step.

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
expects `M' to do the same.  Where `donkey-mark-word' itself
refuses, in a buffer with no word in it at all, the mode still starts:
entering is what the key is for.

The word is the run's FIRST PRESS, and the letter after it GROWS: the
press leaves `donkey-mark-word' in `this-command' once the word is
marked, the way the adopting branch leaves `donkey-mark-run-adopt'
there, so the family test finds a member in `last-command' when the
letter arrives.  \`M' \`w' is two words, \`M' \`b' is the word and the
one before it, and \`M' \`s' is the word grown forward to the end of
its sentence -- what `m w m s' selects, the whole sentence being `m s'
and then \`M'.

Pressed with an active selection this ADOPTS it into the mode instead
of entering empty-handed -- see `donkey-mark-run-adopt': a
visual-line session, a `v' region, or a prefix-built selection all
carry over, and the in-mode \`M' is where canceling lives, the same
second-press shape `donkey-visual-line-toggle' has.  Two selections
are dropped rather than adopted, and the mode starts over them as
over nothing at all: a rectangle, which has no forward end for the
family to own, and an EMPTY active region -- a bare `v' -- so that
`v M' mid-word takes the word, as `M' alone does.

This command is NOT a member of `donkey--mark-run-commands', and the
renames are why it need not be.  A press that marked or adopted
leaves the name of the command that did it, and a press that did
neither -- a buffer with no word in it -- keeps its own, so the letter
after THAT one marks afresh rather than growing from whatever stale
mark the buffer held."
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
      ;; An empty active region and a rectangle are dropped, not
      ;; adopted, and the mode starts as over nothing.  No
      ;; `donkey--ensure-non-rectangle-selection' here: the word marked
      ;; below comes through that funnel, and the adopting branch
      ;; above needs the anchor.
      (deactivate-mark)
      ;; The word IS `m w', reaching from a gap as it does.
      ;; `last-command' is bound away: the word is a fresh mark, not a
      ;; growth from whatever stale mark the buffer held.  Marked
      ;; before the mode is armed, so the reminder is the last thing
      ;; said.
      (condition-case nil
          (progn
            (let ((last-command nil))
              (donkey-mark-word))
            ;; The word marked, the press IS `m w': renamed so the
            ;; letter that follows grows it.  Only on success.
            (setq this-command 'donkey-mark-word))
        ;; Refused only where `m w' refuses; the mode still starts,
        ;; empty.  Only the refusal is caught.
        (user-error nil))
      (donkey--mark-run-enter)))))

(defun donkey-mark-word (&optional count)
  "Select the entire word at or adjacent to point.

From the gap between two words the one AHEAD is marked, and from the
gap at the end of the buffer, where nothing is ahead, the last one.
`donkey-mark-word-backward' takes the one BEHIND from the same gap, so
from a space the two keys are the two words on either side of it; see
`donkey--mark-reach' for the rule.  `donkey-mark-symbol',
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
a bare press does -- see `donkey--object-count'."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((extend (donkey--mark-run-continuing-p)))
    (unless extend
     (let ((origin (point)))
      ;; From a gap, onto the word ahead or, for a backward press and
      ;; at the end of a buffer, onto the word behind.
      (unless (donkey--point-on-word-or-symbol-char-p)
        (donkey--mark-reach-from-gap
         (lambda () (skip-syntax-forward "^w") (not (eobp)))
         (lambda () (backward-word 1))))
      ;; A buffer with no word in it at all is a `user-error', with the
      ;; search undone first so the cursor has not moved.
      (unless (donkey--real-thing-at-point 'word)
        (goto-char origin)
        (user-error "No word at or before point"))
      (beginning-of-thing 'word)))
    ;; Skipped when extending: `mark-word' measures its extension from
    ;; the start of the word already selected.  A companion press is
    ;; presented as a repeat, so `mark-word''s own extension fires from
    ;; the mark.
    (let ((last-command (if extend this-command last-command))
          ;; `mark-word' reads the mark with plain `mark', and a run
          ;; whose region a hook deactivated must still grow.
          (mark-even-if-inactive t)
          (n (donkey--object-count count)))
      (if (and (not extend) (< n 0))
          ;; `mark-word' measures a negative count from point, so a
          ;; fresh press takes its far end from `donkey--object-end-before'.
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
marking for.  The delegate brings its own no-object `user-error' and
its own rectangle cleanup.

The extending branch re-asserts the mark, since moving point
activates nothing.  A COUNT below 1 is treated as 1, and running out
of buffer stops and keeps what is selected."
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
\"text\" and \"that\" -- see `donkey--mark-reach'.

COUNT marks or extends by that many words."
  (interactive "p")
  (donkey--mark-backward count
                         (lambda (n) (forward-word (- n)))
                         #'donkey-mark-word
                         "Word"))

(defun donkey--region-blank-p ()
  "Return non-nil if only whitespace and newlines lie in the region.

Walks the region in place, a paragraph selection being large.  Both
bounds are read before point moves, `region-end' being a function of
point."
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
the report, so a refusal leaves the cursor where the key was.  For a
FRESH press only: a run may legitimately cover blank."
  (when (donkey--region-blank-p)
    (deactivate-mark)
    (when origin
      (goto-char origin))
    (user-error "No %s at or before point" object)))

(defun donkey-mark-sentence (&optional count)
  "Select sentence at point.

With no sentence to be found -- an empty buffer, or one holding only
blank lines or whitespace -- reports a `user-error' rather than letting
the sentence motions signal, as the other mark keys do.

From the gap between two sentences the one AHEAD is marked -- the same
answer `donkey-mark-word', `donkey-mark-symbol' and
`donkey-mark-paragraph' give from the gap between two of their own
objects -- and from the gap at the end of the buffer, where nothing is
ahead, the last one.  `donkey-mark-sentence-backward' takes the one
BEHIND from the same gap; see `donkey--mark-reach' for the rule and the
report behind it.

COUNT marks that many sentences.  Unlike the other mark commands a COUNT
below 1 is treated as 1 here: `mark-end-of-sentence' counts from the
start this command normalizes onto, so a count of 0 selects nothing at
all and a negative one reaches back over the sentence already behind
that start -- neither of which is a sentence at point.  A COUNT reaching
past the last sentence marks what there is and stops, the way every
other counted command does.

Pressing the key again immediately EXTENDS the selection by another
sentence rather than re-marking the same one, and keeps extending until
the buffer runs out.  `donkey-mark-sentence-backward' continues the
same run from the other end, as does every member of
`donkey--mark-run-commands' -- see `donkey--mark-extending-p'."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((origin (point))
        (extending (donkey--mark-run-continuing-p)))
   ;; Only a fresh press normalizes onto a sentence start.
   (condition-case nil
      (unless extending
        ;; Forward first, then back, so the step back lands on the
        ;; sentence containing point from every position in it.
        (forward-sentence 1)
        (backward-sentence 1)
        ;; Landing ahead of the origin means point was in the gap
        ;; before this sentence: the forward key takes it, and a
        ;; backward press steps back to the one behind unless nothing
        ;; lies behind.
        (when (and (eq donkey--mark-reach 'behind)
                   (> (point) origin)
                   (donkey--text-before-p origin))
          (backward-sentence 1)))
    ;; At `point-max' with no trailing newline the forward step
    ;; signals; the sentence behind is the answer.
    (end-of-buffer
     (goto-char (point-max))
     (backward-sentence 1))
    ;; Point goes back before reporting.
    (error (goto-char origin)
           (user-error "No sentence at or before point")))
  ;; A companion press is presented as a repeat, so
  ;; `mark-end-of-sentence''s own extension fires from the mark.
  (let ((last-command (if extending this-command last-command))
        ;; A deactivated run must still grow.
        (mark-even-if-inactive t))
    (condition-case nil
        (mark-end-of-sentence (max 1 (or count 1)))
      ;; A count running past the last sentence marks what there is
      ;; and stops.
      (end-of-buffer (push-mark (point-max) nil t))
      (error (goto-char origin)
             (user-error "No sentence at or before point"))))
  ;; A blank buffer is refused here, the motions not signaling on one.
  (unless extending
    (donkey--refuse-blank-mark "sentence" origin))
  (message "Sentence marked")))

(defun donkey-mark-sentence-backward (&optional count)
  "Select the sentence at point, or grow a sentence selection BACKWARD.

The other end of `donkey-mark-sentence's run, sharing
`donkey--mark-backward' with the other three backward keys.  At the
buffer's start an overshooting count keeps what is selected.

From the gap between two sentences this takes the one BEHIND, where
`donkey-mark-sentence' takes the one ahead -- see `donkey--mark-reach'.

COUNT marks or extends by that many sentences."
  (interactive "p")
  (donkey--mark-backward count #'backward-sentence
                         #'donkey-mark-sentence "Sentence"))

(defun donkey--absorb-paragraph-blank (start)
  "Extend point over one following blank line, when START owns no leading one.

Called with point at the end of a paragraph selection that began at
START.  Does nothing when the selection already begins on a blank line,
or when there is no blank line to take."
  (when (and (save-excursion
               (goto-char start)
               (not (looking-at-p "^[[:space:]]*$")))
             (looking-at-p "^[[:space:]]*$")
             (< (point) (point-max)))
    (forward-line 1)))

(defun donkey-mark-paragraph (&optional count)
  "Select the paragraph at or adjacent to point.

With no paragraph to be found -- an empty buffer, or one holding only
blank lines or whitespace -- reports a `user-error', the way
`donkey-mark-word', `donkey-mark-symbol' and `donkey-mark-sentence' all
do.

From a blank line between two paragraphs the one BELOW is marked, as
`donkey-mark-word' marks the word ahead from the space between two, and
from blank lines at the end of the buffer the last one.
`donkey-mark-paragraph-backward' takes the one ABOVE from the same blank
line -- see `donkey--mark-reach'.  Whichever is taken comes with ONE
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
        ;; Grown by moving the mark, and the blank-line rule applies
        ;; here too, so repeating the key agrees with a count.
        (set-mark (save-excursion
                    (let ((start (point)))
                      (goto-char (mark t))
                      (forward-paragraph n)
                      (donkey--absorb-paragraph-blank start)
                      (point))))
      ;; Point ends at the start, mark at the end.  From a blank line
      ;; the forward key takes the paragraph below and the backward key
      ;; the one above, each falling through to the other side where
      ;; its own is empty; a line with text on it is never a gap.  From
      ;; text the start is found as `mark-paragraph' finds it: forward
      ;; to the end and back.
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
that used to lead the selection simply becomes interior.

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

From the gap between two symbols the one AHEAD is marked, and from the
gap at the end of the buffer, where nothing is ahead, the last one.
`donkey-mark-symbol-backward' takes the one BEHIND from the same gap --
see `donkey--mark-reach' for the rule.  `donkey-mark-word',
`donkey-mark-sentence' and `donkey-mark-paragraph' answer the same
way.  Brackets and quotes are crossed on the way, so from the opening
paren of \"(foo bar)\" the mark is \"foo\".

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
      ;; Grown by moving the mark; the punctuation trim runs again for
      ;; the new end.
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (forward-sexp n)
                  (when (> n 0)
                    (donkey--trim-symbol-punctuation))
                  (point)))
    (let ((origin (point)))
     ;; From a gap, onto the symbol ahead by syntax, across punctuation,
     ;; quotes and brackets, or for a backward press and at the end of
     ;; a buffer onto the symbol behind.
     (unless (donkey--point-on-word-or-symbol-char-p)
       (donkey--mark-reach-from-gap
        (lambda () (skip-syntax-forward "^w_") (not (eobp)))
        #'donkey--back-to-symbol-char))
    ;; No symbol on either side of the gap is a `user-error', undone
    ;; before reporting.
    (unless (donkey--real-thing-at-point 'symbol)
      (goto-char origin)
      (user-error "No symbol at or before point"))
    (beginning-of-thing 'symbol))
    ;; Each trim belongs to an end of the selection; a negative count
    ;; takes its far end from the object behind point.
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
      (backward-sexp n)
      (when (> n 0)
        (donkey--trim-symbol-prefix)))
    (activate-mark)))
  (message "Symbol marked"))

(defun donkey-mark-symbol-backward (&optional count)
  "Select the symbol at point, or grow a symbol selection BACKWARD.

The other end of `donkey-mark-symbol's run, sharing
`donkey--mark-backward' with the other three backward keys.

The trailing trim does not run here: the walk lands on symbol STARTS,
and whatever punctuation separated the symbols becomes interior to the
selection.

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

(defun donkey-set-mark ()
  "Call `set-mark-command', disabling a stale `rectangle-mark-mode' first.

This does NOT toggle, unlike its two neighbors `donkey-visual-line-toggle'
\(\"V\") and `donkey-rectangle-mark-mode' (\"m v\"), which both cancel the
selection they started when pressed again.  `set-mark-command' re-anchors:
a second press drops a fresh mark at point and carries on selecting from
there, so the previous selection is discarded but the buffer is still in
a selecting state.  \\[keyboard-quit] is what lets go."
  (interactive)
  (donkey--ensure-non-rectangle-selection)
  (call-interactively #'set-mark-command)
  ;; Only when a selection actually started: a prefixed press pops the
  ;; mark ring and a second press deactivates.  `mark-active', so it
  ;; reads the same in `--batch'.
  (when mark-active
    (add-hook 'deactivate-mark-hook #'donkey--clear-selection-hint nil t)
    (setq donkey--linear-selection-active t)
    (message "%s" donkey--linear-selection-hint)))

;; `call-interactively': `mark-whole-buffer' is `interactive-only'.
(defun donkey-mark-whole-buffer ()
  "Select the whole buffer, clearing a stale rectangle selection first."
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
;;; Donkey Digraphs
;;; ---------------------------------------------------------------------------

(defvar quail-package-alist)
(defvar quail-current-package)
(declare-function quail-lookup-key "quail" (key &optional len not-reset-indices))

(defcustom donkey-digraph-line-spacing 0
  "Air under each row of the `donkey-digraph' chart's tables.

The rows of the common table, and of each script group of the full
table, are always padded to one height, the least the fonts drawing
the group allow, with the baseline at one and the same offset; this
adds that much below each row.  A whole number is pixels; a fraction
is that part of the padded row's height, so the air keeps its
proportion when the rows grow with their fonts or with the text
scale.  Zero, the default, adds nothing, and so does a negative
number or anything that is not a number.  Only a graphical frame
shows any of it."
  :type '(choice (integer :tag "Pixels")
                 (float :tag "Fraction of a padded row's height"))
  :group 'donkey)

(defconst donkey--digraph-common
  '(("'9" . "’") ("\"6" . "“") ("\"9" . "”") ("-N" . "–")
    ("-M" . "—") ("'6" . "‘") ("sb" . "•") ("e'" . "é")
    ("Pd" . "£") ("Rg" . "®") ("1'" . "′") ("Eu" . "€"))
  "Digraphs `donkey-digraph' lists first, each with what it types.

They are the twelve characters English web text uses most that a
keyboard lacks, the most frequent first.")

(defun donkey--digraph-result (digraph)
  "Return the string DIGRAPH types under the `rfc1345' input method, or nil.

DIGRAPH is the two characters as vi spells them; the input method
takes them after an ampersand, and that is what is looked up."
  (require 'quail)
  (unless (assoc "rfc1345" quail-package-alist)
    (load "quail/rfc1345" nil t))
  (let* ((quail-current-package (assoc "rfc1345" quail-package-alist))
         (key (concat "&" digraph))
         (translation (car-safe (quail-lookup-key key (length key)))))
    (cond ((integerp translation) (char-to-string translation))
          ((stringp translation) translation)
          ((and (vectorp translation) (> (length translation) 0))
           (aref translation 0)))))

(defvar donkey--digraph-table nil
  "Every digraph of the `rfc1345' input method as (CODE . STRING).
Built by `donkey--digraph-table' on first use, sorted by CODE.")

(defun donkey--digraph-translation-string (translation)
  "Return the string a quail map TRANSLATION stands for, or nil."
  (cond ((integerp translation) (char-to-string translation))
        ((stringp translation) (and (> (length translation) 0) translation))
        ((and (vectorp translation) (> (length translation) 0))
         (donkey--digraph-translation-string (aref translation 0)))
        ((and (consp translation) (vectorp (cdr translation)))
         (donkey--digraph-translation-string (cdr translation)))))

(defun donkey--digraph-table ()
  "Return every digraph of the `rfc1345' input method as (CODE . STRING).

CODE is the digraph as vi spells it, without the ampersand the input
method takes first; STRING is what it types.  Read from the method's
own table, so the list is whatever that Emacs ships, and cached."
  (or donkey--digraph-table
      (progn
        (require 'quail)
        (unless (assoc "rfc1345" quail-package-alist)
          (load "quail/rfc1345" nil t))
        (let ((map (nth 2 (assoc "rfc1345" quail-package-alist)))
              acc)
          (cl-labels ((walk (node prefix)
                        (when (> (length prefix) 1)
                          (let ((str (donkey--digraph-translation-string (car node))))
                            (when str (push (cons (substring prefix 1) str) acc))))
                        (dolist (entry (cdr node))
                          (when (and (consp entry) (characterp (car entry)))
                            (walk (cdr entry) (concat prefix (char-to-string (car entry))))))))
            (walk map ""))
          (setq donkey--digraph-table
                (sort acc (lambda (a b) (string< (car a) (car b)))))))))

(defun donkey--digraph-cell (text column &optional face)
  "Insert TEXT, then a space that aligns the next cell to COLUMN.

The space carries a `display' property, `(space :align-to COLUMN)',
which the redisplay honors to the pixel in a graphical frame and to
the column in a terminal, whatever the width of TEXT's glyphs.  FACE,
when given, is put on TEXT."
  (insert (if face (propertize text 'face face) text)
          (propertize " " 'display (list 'space :align-to column))))

(defconst donkey--digraph-scripts
  '((latin "Latin" phonetic) (greek "Greek") (cyrillic "Cyrillic")
    (hebrew "Hebrew") (arabic "Arabic") (symbol "Symbols") (kana "Kana")
    (bopomofo "Bopomofo") (han "Han") (cjk-misc "CJK, other"))
  "The scripts the chart groups the full table by, in this order.

Each entry is (SCRIPT NAME OTHER...): the `char-script-table' symbol,
the heading the group gets, and other scripts folded into it.  A
script not listed comes after these under its own name; characters
of no script, the private-use ones, come last.")

(defun donkey--digraph-groups (table)
  "Return TABLE's rows grouped by script, as (NAME . ROWS) in chart order.

TABLE is the digraph table.  The groups follow `donkey--digraph-scripts',
each holding its rows in TABLE's order."
  (let (by-script groups)
    (dolist (row table)
      (push row (alist-get (aref char-script-table (aref (cdr row) 0)) by-script)))
    (dolist (entry donkey--digraph-scripts)
      (let (rows)
        (dolist (script (cons (car entry) (cddr entry)))
          (setq rows (append (alist-get script by-script) rows))
          (setf (alist-get script by-script nil t) nil))
        (when rows
          (push (cons (cadr entry) (sort rows (lambda (a b) (string< (car a) (car b)))))
                groups))))
    (dolist (entry (sort (seq-filter #'car by-script)
                         (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
      (push (cons (capitalize (symbol-name (car entry))) (nreverse (cdr entry))) groups))
    (when (alist-get nil by-script)
      (push (cons "Private use" (nreverse (alist-get nil by-script))) groups))
    (nreverse groups)))

(defconst donkey--digraph-full-table
  '(full . "  \\S-+ \\(.+?\\) \\(U\\)\\+")
  "The full table's tag and row, as `donkey--digraph-rows' wants them.")

(defconst donkey--digraph-common-table
  '(common . "  \\S-+ \\(?2:&\\)\\S-+ \\(?1:\\S-+\\)$")
  "The common table's tag and row, as `donkey--digraph-rows' wants them.")

(defun donkey--digraph-rows (table)
  "Return the rows of TABLE in the chart, the current buffer.

TABLE is (TAG . ROW): the value the rows' leading blanks carry as
their `donkey-digraph-row' property, and a regexp for a row, whose
group 1 is the result cell and group 2 one character in the default
face.  Each row is returned as a list of its start, the result
cell's start and end, that character's position, and the row's
`donkey-digraph-group'."
  (save-excursion
    (goto-char (point-min))
    (let (rows)
      (while (not (eobp))
        (when (and (eq (get-text-property (point) 'donkey-digraph-row) (car table))
                   (looking-at (cdr table)))
          (push (list (point) (match-beginning 1) (match-end 1) (match-beginning 2)
                      (get-text-property (point) 'donkey-digraph-group))
                rows))
        (forward-line 1))
      (nreverse rows))))

(defun donkey--digraph-row-groups (table)
  "Return the rows of TABLE in the chart, grouped: a list of row lists.

Rows are grouped as `donkey--digraph-rows' lists them, a run of rows
with one `donkey-digraph-group' making a group."
  (let (groups current name)
    (dolist (row (donkey--digraph-rows table))
      (unless (and current (equal (nth 4 row) name))
        (when current (push (nreverse current) groups))
        (setq current nil name (nth 4 row)))
      (push row current))
    (when current (push (nreverse current) groups))
    (nreverse groups)))

(defun donkey--digraph-row-spec (window rows extra)
  "Return a `display' spec giving ROWS one height, or nil.

ROWS are rows of the chart, the current buffer, shown in WINDOW, as
`donkey--digraph-rows' lists them.  The spec is a stretch of space two
columns wide, with the largest ascent and the largest descent of any
font WINDOW draws the rows' result cells with, and EXTRA pixels more
below the baseline, to put on each row's leading blanks: every row is
then as tall as the tallest could be, with its baseline at one and
the same offset and EXTRA pixels of air under it, in a window of any
width, wrapped or truncated, at any text scale.  Returns nil in a
terminal, and when EXTRA is zero and the rows are already all one
height."
  (when (and rows (display-graphic-p (window-frame window)))
    (let ((cache (make-hash-table :test #'eq)) (ascent 0) (descent 0) default)
      (dolist (row rows)
        (dolist (pos (cons (nth 3 row) (number-sequence (nth 1 row) (1- (nth 2 row)))))
          (let* ((font (font-at pos window))
                 (pair (and font
                            (or (gethash font cache)
                                (puthash font
                                         (let ((info (font-info font)))
                                           (cons (aref info 8) (aref info 9)))
                                         cache)))))
            (when pair
              (when (= pos (nth 3 row)) (setq default pair))
              (setq ascent (max ascent (car pair))
                    descent (max descent (cdr pair)))))))
      (when default
        (let ((height (+ ascent descent extra)) (pct 0))
          (when (> height (+ (car default) (cdr default)))
            ;; The ascent is given as a percentage of the height: the
            ;; smallest one that comes to the pixels wanted.
            (while (and (< pct 100) (< (floor (/ (* height pct) 100.0)) ascent))
              (setq pct (1+ pct)))
            (list 'space :width 2 :height (list height) :ascent pct)))))))

(defun donkey--digraph-apply-spec (rows spec)
  "Put SPEC as the `display' of each of ROWS' leading blanks, or clear them."
  (dolist (row rows)
    (if spec
        (put-text-property (car row) (+ 2 (car row)) 'display spec)
      (remove-text-properties (car row) (+ 2 (car row)) '(display nil)))))

(defun donkey--digraph-group-spec (window rows)
  "Return the `display' spec padding ROWS to one height, with air, or nil.

ROWS are the rows of one group of the chart, the current buffer,
shown in WINDOW.  The spec pads them to the least height the fonts
drawing them allow and adds the air `donkey-digraph-line-spacing'
asks for, a fraction of it taken of the padded row's height.  Nil
when the rows are one height already and no air is asked for."
  (let* ((spacing donkey-digraph-line-spacing)
         (bare (donkey--digraph-row-spec window rows 0))
         (row-height (if bare
                         (car (plist-get (cdr bare) :height))
                       (frame-char-height (window-frame window))))
         (extra (cond ((and (integerp spacing) (> spacing 0)) spacing)
                      ((and (floatp spacing) (> spacing 0))
                       (round (* spacing row-height)))
                      (t 0))))
    (if (zerop extra) bare (donkey--digraph-row-spec window rows extra))))

(defun donkey--digraph-pad-rows (&optional window)
  "Pad the rows of the chart's tables to one height per group, with air.

The chart is the current buffer; WINDOW defaults to a window showing
it, and nothing happens when there is none.  Every row of the common
table, and of each script group of the full table, is padded to the
least height the fonts WINDOW draws the group with allow, and given
the air `donkey-digraph-line-spacing' asks for below.  The chart is
laid out once first, so that the fonts asked about are the ones the
display uses.  Runs when the chart is built and again whenever its
text is scaled, so the rows stay even, and evenly spaced, at every
scale."
  (let ((window (or window (get-buffer-window (current-buffer) t))))
    (when window
      (ignore (window-text-pixel-size window (point-min) (point-max) t))
      (with-silent-modifications
        (dolist (rows (cons (donkey--digraph-rows donkey--digraph-common-table)
                            (donkey--digraph-row-groups donkey--digraph-full-table)))
          (donkey--digraph-apply-spec rows (and rows (donkey--digraph-group-spec window rows))))))))

(defun donkey--digraph-code-points (string)
  "Return STRING's code points as \"U+XXXX\", space-separated."
  (mapconcat (lambda (c) (format "U+%04X" c)) string " "))

(defun donkey-digraph ()
  "Show how to type a character that is not on the keyboard.

Opens a buffer listing common digraphs -- the two-character codes of
RFC 1345, the same ones vi uses -- and the steps that type one in
Emacs through the `rfc1345' input method, followed by the
two ways to type any Unicode character by code point or name, and
then the whole table, grouped by script: every digraph the method
knows, with what it types, its code point and the character's name,
ready to copy from."
  (interactive)
  (let ((buf (get-buffer-create "*DONKEY Digraphs*"))
        (rule (propertize (make-string 50 ?-) 'face 'font-lock-comment-face))
        (head (lambda (text)
                (propertize text 'face '(bold font-lock-comment-delimiter-face)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize "DONKEY Digraphs\n"
                          'face '(bold font-lock-function-name-face :height 1.2)))
      (insert (propertize (make-string 50 ?=) 'face 'font-lock-comment-face) "\n\n")
      (insert (substitute-command-keys
               "A digraph is two characters that stand for one you cannot type
directly.  These are the codes of RFC 1345, the same ones vi uses;
Emacs holds them as the rfc1345 input method, each typed with an
ampersand in front of it.\n\n"))
      (insert (funcall head "  Common digraphs") "\n" rule "\n")
      (insert "  The twelve characters English web text uses most that a keyboard
  lacks, the most frequent first.\n\n")
      (insert "  ")
      (donkey--digraph-cell "DIGRAPH" 12 'font-lock-keyword-face)
      (donkey--digraph-cell "TYPE" 22 'font-lock-keyword-face)
      (insert (propertize "RESULT" 'face 'font-lock-keyword-face) "\n")
      (dolist (row donkey--digraph-common)
        (insert (propertize "  " 'donkey-digraph-row 'common))
        (donkey--digraph-cell (car row) 12 'font-lock-variable-name-face)
        (donkey--digraph-cell (concat "&" (car row)) 22)
        (insert (cdr row) "\n"))
      (insert "\n" (funcall head "  How to type one") "\n" rule "\n")
      (insert (substitute-command-keys
               "  1. Enter INSERT state.
  2. Press \\[toggle-input-method] and choose rfc1345.  Once is enough:
     after that \\[toggle-input-method] switches the same method on and off.
  3. Type an ampersand and the digraph: &e\\=' gives é.

  DONKEY turns the input method off in NORMAL state, so the letters
  stay commands, and back on when you return to INSERT.

  Or copy the character straight out of the table below.\n\n"))
      (insert (funcall head "  Any character, by code point or name") "\n" rule "\n")
      (insert (substitute-command-keys
               "  \\[insert-char], then a character name or its hex code, works in
  either state: \\[insert-char] 20ac RET gives €.

  On a Linux desktop, Ctrl+Shift+u starts GTK's own hex entry in a
  graphical frame: keep Ctrl and Shift held while you type the hex
  code, and the character appears when you release them.  Release
  after the u and the entry is over.  With an IBus daemon running the
  order is the other way round: release after the u, type the code,
  then Space or Enter.  In a terminal it depends on the terminal.\n\n"))
      (let ((table (donkey--digraph-table)))
        (insert (funcall head (format "  All %d digraphs, by script" (length table)))
                "\n" rule "\n")
        (insert "  ")
        (donkey--digraph-cell "DIGRAPH" 11 'font-lock-keyword-face)
        (donkey--digraph-cell "RESULT" 19 'font-lock-keyword-face)
        (donkey--digraph-cell "CODE" 30 'font-lock-keyword-face)
        (insert (propertize "NAME" 'face 'font-lock-keyword-face) "\n")
        (dolist (group (donkey--digraph-groups table))
          (insert "\n" (funcall head (format "  %s" (car group))) "\n")
          (dolist (row (cdr group))
            (insert (propertize "  " 'donkey-digraph-row 'full
                                'donkey-digraph-group (car group)))
            (donkey--digraph-cell (car row) 11 'font-lock-variable-name-face)
            (donkey--digraph-cell (cdr row) 19)
            (donkey--digraph-cell (donkey--digraph-code-points (cdr row)) 30)
            (insert (or (and (= (length (cdr row)) 1)
                             (get-char-code-property (aref (cdr row) 0) 'name))
                        "")
                    "\n"))))
      (insert "\n" (propertize (make-string 50 ?=) 'face 'font-lock-comment-face) "\n")
      (insert (propertize "q: quit  |  C-s: search" 'face 'font-lock-comment-face))
      (special-mode)
      (setq truncate-lines t)
      (add-hook 'text-scale-mode-hook #'donkey--digraph-pad-rows nil t)
      (goto-char (point-min)))
    (let ((window (display-buffer buf)))
      (with-current-buffer buf
        (donkey--digraph-pad-rows window)))))

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

A character that is not on your keyboard has a code of its own: the
command donkey-digraph lists them all and how to type them.

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

Written with `substitute-command-keys' escapes rather than literal
keys, so a reader who has rebound anything is taught the keys they
actually have.  One token is not an escape: \"DONKEY-DELETE-KEYS\" is
replaced by `donkey--tutor-delete-keys' before substitution runs, so
both keys of `donkey-delete' are named.")

(defun donkey--tutor-delete-keys ()
  "Return the keys running `donkey-delete', as prose: \"d or x\".

Computed rather than written into `donkey--tutor-content', so a reader
who has rebound either key is taught the keys they actually have.
Each key is wrapped in the key escape, so it gets the `help-key-binding'
face like every other key in the tutor; the keys are sorted; and with
no key at all the command is named instead."
  (let ((keys (mapcar (lambda (k) (format "\\`%s'" (key-description k)))
                      (where-is-internal #'donkey-delete
                                         donkey-normal-mode-map))))
    (setq keys (sort keys #'string<))
    (cond
     ((null keys) "\\[donkey-delete]")
     ((null (cdr keys)) (car keys))
     (t (mapconcat #'identity keys "/")))))

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
          ;; A tutor returned to with DONKEY off gets it back on; a
          ;; live one is left as it was, INSERT included.
          (with-current-buffer existing
            (unless (bound-and-true-p donkey-mode)
              (donkey-mode 1)
              (donkey-enter-normal)))
          (pop-to-buffer existing))
      (let ((buf (get-buffer-create "*DONKEY Tutor*")))
        (with-current-buffer buf
          (text-mode)
          ;; The keymap must be live before `substitute-command-keys'
          ;; resolves the bindings.
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

;;; donkey.el --- Opinionated Modal Editing -*- lexical-binding: t -*-

;; Copyright (C) 2026 Michael Jones
;; Author: Michael Jones <yardquit@pm.me>
;; Maintainer: Michael Jones
;; Assisted-by: Lumo 2.0 Max, Claude [Claude Code]
;; URL: https://github.com/yardquit/donkey
;; Version: 1.2.0
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
;; Optional Smartparens Integration:
;; If you use smartparens, call `(donkey-setup-smartparens)' in
;; your config after loading smartparens to bind C-g in smartparens
;; overlay keymaps.  This improves reliability of C-g escape in terminal
;; mode when inside nested smartparens overlays.

;;; Usage:
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

;; Coerced rather than trusted, for the same reason
;; `donkey--position-ring-limit' exists.  `donkey--excluded-mode-p' is
;; reached from `post-command-hook' via
;; `donkey--check-post-command-non-editing', and Emacs REMOVES a hook
;; function that signals -- silently, and for the rest of the session.
;; Repairing the variable afterwards does not bring it back; only
;; toggling `donkey-mode' off and on does.
;;
;; The misconfiguration is a plausible one rather than a perverse one.
;; These variables hold LISTS of modes, and
;;
;;   (setq donkey-excluded-modes 'dired-mode)
;;
;; -- one missing pair of parentheses -- is the obvious slip.  Confirmed
;; by driving real keys: the FIRST keypress after it logged "Error in
;; post-command-hook" and took away the catch-all that guarantees Normal
;; state can never be active in an excluded buffer.  Nothing on screen
;; connects the two, and the guarantee is gone for the session.
;;
;; A bare symbol is read as the one-element list it was meant to be,
;; rather than discarded: that is what the user asked for, and refusing
;; it would trade a crash for a silent no-op.
(defun donkey--mode-list (value)
  "Return VALUE as a list of major modes, never signalling.

A list is returned unchanged, a bare symbol is taken as a one-element
list, and anything else reads as the empty list."
  (cond ((listp value) value)
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

(defun donkey--excluded-mode-p ()
  "Return non-nil if the current major mode is in `donkey-excluded-modes'."
  (donkey--major-mode-in-p donkey-excluded-modes))

(defun donkey--insert-state-lighter ()
  "Return the modeline text for Insert state in the current buffer.

\" DONKEY[E]\" in a `donkey-excluded-modes' buffer, \" DONKEY[I]\"
everywhere else.  See `donkey-insert-mode' for why the two are told
apart at all.

One function rather than the same expression in two places:
`donkey-insert-mode's lighter and `donkey-indicator' both need it, and
they are four hundred lines apart, so a change to one would not
obviously want the other."
  (if (donkey--excluded-mode-p) " DONKEY[E]" " DONKEY[I]"))

(defun donkey--handle-non-editing-buffer ()
  "Bounce straight back to Insert state in an excluded major mode.

Runs whenever `donkey-normal-mode' just turned on.
`donkey--ensure-default-state' (on `after-change-major-mode-hook')
already keeps a FRESH buffer out of Normal state in an excluded mode,
but that only covers the buffer's initial activation.  This hook
catches the other way in: Normal state entered directly, e.g. via `M-x
donkey-normal-mode' or a keybinding, in a buffer that's already
excluded (comint/term/vterm/eshell) and currently, correctly, in
Insert state.  Registered on `donkey-normal-mode-hook', so it runs
immediately as part of that same toggle, before the user's next
keypress ever reaches the buffer.

See `donkey--check-post-command-non-editing' for the broader,
one-command-delayed safety net this doesn't cover: anything that sets
`donkey-normal-mode' to t WITHOUT going through the actual minor-mode
toggle function (so this hook never fires at all)."
  (when (donkey--excluded-mode-p)
    (when (bound-and-true-p donkey-normal-mode)
      (donkey-enter-insert))))

(add-hook 'donkey-normal-mode-hook #'donkey--handle-non-editing-buffer)

(defun donkey--check-post-command-non-editing ()
  "Force Insert state if Normal state is somehow active in an excluded mode.

Checked after any command whatsoever.  Registered on the global
`post-command-hook' by `donkey-mode', so it
runs after EVERY command in EVERY buffer, checking the raw
`donkey-normal-mode' variable directly rather than relying on a hook.
This is deliberately redundant with `donkey--handle-non-editing-buffer'
and `donkey--ensure-default-state': those two only run when the actual
toggle function/major-mode-change machinery runs, so anything that
sets `donkey-normal-mode' to t some OTHER way (a raw `setq-local', a
buggy or unusual third-party integration) would slip past both of them
undetected -- this is the catch-all that guarantees Normal state can
never survive more than one command's delay in a mode where
`suppress-keymap' would otherwise break subprocess/terminal
interaction entirely."
  (when (and (bound-and-true-p donkey-normal-mode)
             (donkey--excluded-mode-p))
    (donkey-enter-insert)))

;;; ---------------------------------------------------------------------------
;;; Org-Scratch Buffer Creation
;;; ---------------------------------------------------------------------------

(defun donkey-insert-org-scratch-message ()
  "Insert buffer message."
  (insert
   (substitute-command-keys
    (purecopy
     (concat "# This buffer is for scribbling in org-mode.\n"
             "# Start your scribble here and save to file with '"
             "\\[save-some-buffers]"
             "' for persistence.\n\n"))))
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
a number is read the same way rather than signalling -- see
`donkey--position-ring-limit' for why erroring there is not an option."
  :type 'integer
  :group 'donkey)

(defun donkey--position-ring-limit ()
  "Return `donkey-position-ring-max' as a usable count, never signalling.

`donkey--track-position' runs on `post-command-hook', where an error is
not merely noisy: Emacs REMOVES the offending function from the hook and
carries on.  One bad command therefore switches position tracking off
for the rest of the session, and repairing the variable afterwards does
not bring it back -- the hook no longer holds the function.  `S' then
reports \"Position 1/1\" forever, pointing at whatever marker happened to
land before the break, with nothing on screen to connect the two.

Reached by a plausible misconfiguration, not a perverse one:
`donkey-position-ring-max' blesses 0 as the way to switch tracking off,
and setting it to nil is the obvious guess for anyone who reaches for
nil to mean \"no limit\" or \"disabled\".  Confirmed by
driving real keys through `execute-kbd-macro': the second keypress
logged \"Error in post-command-hook\" and the tracker was gone from both
the global and the buffer-local hook value.

A non-number is read as 0 -- tracking off, which is what nil was reaching
for anyway.  A negative count means the same.  A float is truncated
rather than rejected, since it already worked."
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
  "Cons cell (BUFFER . POINT) captured after the previous command.")

(defun donkey--track-position ()
  "Record the previous cursor position.

Runs on `post-command-hook', recording point whenever it has moved
since the last command.  Independent of the mark ring and region.

Must not signal.  Emacs removes a `post-command-hook' function that
errors, so a single bad command would switch position tracking off for
the rest of the session -- see `donkey--position-ring-limit', which is
where the one value a user can get wrong is made safe."
  (unless (minibufferp)
    (let ((now-pt (point)))
      (when (and donkey--last-tracked-state
                 (/= (cdr donkey--last-tracked-state) now-pt))
        (let ((m (make-marker)))
          (set-marker m (cdr donkey--last-tracked-state))
          (push m donkey--position-ring)
          (when (> (length donkey--position-ring) (donkey--position-ring-limit))
            (set-marker (car (last donkey--position-ring)) nil)
            ;; Assigned back, not just called for effect: `nbutlast'
            ;; cannot destructively empty a ONE-element list -- it
            ;; returns nil while leaving the variable pointing at the
            ;; original cons.  With `donkey-position-ring-max' set to 0
            ;; (a reasonable way to switch position tracking off) every
            ;; trim hits exactly that case, so the ring kept the marker
            ;; that was just pointed nowhere and `donkey-jump-back'
            ;; failed with "Marker does not point anywhere".
            (setq donkey--position-ring
                  (nbutlast donkey--position-ring))))
        (setq donkey--position-index 0))
      (setq donkey--last-tracked-state (cons (current-buffer) now-pt)))))

;; Why narrowed-out positions are skipped:
;;
;; Marker positions are absolute and narrowing does not move them, so a ring
;; recorded before `narrow-to-region' (or `org-narrow-to-subtree', which Org
;; users press constantly) mostly holds positions the buffer is no longer
;; showing.  `goto-char' silently CLAMPS to the narrowing edge rather than
;; signalling, so those entries used to land point on the first or last
;; visible character while still reporting "Position 2/3" -- a claimed jump
;; to a recorded position that was really just a jump to the boundary.
;; `donkey--banked-spans' filters the same way and for the same reason.
(defun donkey-jump-back ()
  "Rotate to the next stored position in the ring and jump there.

Press repeatedly to cycle through the last `donkey-position-ring-max'
recorded positions in this buffer.

Intended mainly for undoing navigation mistakes: a big jump is easy to
mis-key, and this makes one cheap to take back -- reaching for `g l' and
slipping to `g e' lands you at the end of the buffer rather than the end
of the line, and one keystroke puts it right.
Other uses suggest themselves, but it is a recovery key rather than a
filing system, and no substitute for Emacs' own bookmarks: positions are
recorded automatically as you move, so nothing here is a place you chose
to remember.

Positions outside the accessible portion are skipped rather than jumped
to, and the count in the message is of the visible entries, so it matches
what pressing again will cycle through."
  (interactive)
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
      ;; The counter is used BEFORE it is advanced.  Advancing first made
      ;; the very first press skip the most recent entry and land on the
      ;; one before it -- so the key meant for taking a jump back always
      ;; overshot by exactly one recorded position.  Since a position is
      ;; recorded on every movement, that is one LINE out after `j'/`k'
      ;; and one CHARACTER out after `h'/`l', which is why it read as a
      ;; near miss rather than as landing somewhere unrelated.  Confirmed
      ;; live: from line 5, `G' then `S' arrived at line 4.
      (let* ((ring-len (length visible))
             (idx (if (>= donkey--position-index ring-len)
                      0
                    donkey--position-index)))
        (goto-char (nth idx visible))
        (setq donkey--position-index (1+ idx))
        (setq donkey--last-tracked-state (cons (current-buffer) (point)))
        (message "Position %d/%d" (1+ idx) ring-len))))))

(defun donkey-goto-line ()
  "Prompt for a line number and move point to the start of that line.

Out-of-range input is simply clamped by `forward-line' itself (past
the end of the buffer moves to the last line; zero, negative, or any
undershoot stops at the first).  `read-number' accepts fractional
input (e.g. \"3.5\", an easy typo for \"35\" or \"3\"), which is
rounded to the nearest whole line here rather than passed straight to
`forward-line' -- which requires an integer and would otherwise signal
a raw `wrong-type-argument' error instead of just going to a line."
  (interactive)
  (let ((target-line (round (read-number "Line: "))))
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
  "Deactivate the mark if there is an active region.

Uses `use-region-p' rather than `region-active-p': the latter is
`(and transient-mark-mode mark-active)' with no regard for whether the
region is empty, whereas `use-region-p' additionally requires it be
non-empty (per `use-empty-active-region'), matching what these Insert-
entry commands actually care about -- an empty active region has
nothing meaningful to deselect."
  (when (use-region-p)
    (deactivate-mark)))

(defun donkey-insert-here ()
  "Insert at current position - enters INSERT state."
  (interactive)
  (donkey--deactivate-region-if-active)
  (donkey-enter-insert))

(defun donkey-insert-after ()
  "Insert after current char - enters INSERT state."
  (interactive)
  (donkey--deactivate-region-if-active)
  (condition-case _err
      (forward-char 1)
    (end-of-buffer nil))
  (donkey-enter-insert))

(defun donkey-insert-beginning-of-line ()
  "Insert at beginning of line - enters INSERT state."
  (interactive)
  (donkey--deactivate-region-if-active)
  (beginning-of-line)
  (donkey-enter-insert))

(defun donkey-insert-end-of-line ()
  "Insert at end of line - enters INSERT state."
  (interactive)
  (donkey--deactivate-region-if-active)
  (move-end-of-line 1)
  (donkey-enter-insert))

(defun donkey-open-below ()
  "Open a new line below and enter INSERT state."
  (interactive)
  (donkey--deactivate-region-if-active)
  (move-end-of-line 1)
  (newline-and-indent)
  (donkey-enter-insert))

(defun donkey-open-above ()
  "Open a new line above and enter INSERT state."
  (interactive)
  (donkey--deactivate-region-if-active)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  (donkey-enter-insert))

;; Two notes on the choices here:
;;
;; Nothing has been decided about what changing a multi-line BANK ought to
;; do, and guessing at it silently would be worse than the present split, so
;; "c" leaves banks alone while "y", "d" and "p" spend them.  A known
;; difference rather than a discovery.
;;
;; The end-of-buffer guard exists because `delete-char' signals there, which
;; would abort before the state transition and leave Normal state active --
;; pressing "change" and silently staying in Normal, with only an "End of
;; buffer" message to explain it.  Caught the same way `donkey-insert-after'
;; catches it for its own `forward-char'.
(defun donkey-change (&optional count)
  "Delete the active region (or the character at point) and enter INSERT state.

Under `rectangle-mark-mode' the region is replaced via
`string-rectangle' and DONKEY stays in NORMAL state instead.
`string-rectangle' prompts for the replacement text in the minibuffer
and applies it to every covered line itself, so by the time it returns
the edit is already complete and there is nothing left to type --
dropping into INSERT there just means the next navigation keypress
self-inserts.  Confirmed live: after `m v', `c', a replacement string
and RET, pressing `j' and `l' typed a literal \"jl\" into the buffer
instead of moving.

A visual-line selection made with `V' is NOT widened to whole lines
here, unlike `donkey-copy' and `donkey-delete' -- see
`donkey--visual-line-region-bounds' for the widening those two do.  The
newline ending the last line is kept, so `V c' empties the line and
leaves point on it ready to type, rather than removing the line and
dropping INSERT state onto the following one.  That is what changing a
line means in vi, where `cc' is precisely the linewise change that keeps
its line; `V J c' likewise collapses the span to a single empty line.
Deliberate, and the one place the two line commands part company: `V d'
takes the newline because you asked for the line to go, `V c' keeps it
because you asked to replace what is on it.

Banked lines are not honoured either.  With lines banked via
`donkey-bank-selection' and no active region, this changes the character
at point and leaves the banks standing -- `y', `d' and `p' all act on
the bank instead.

INSERT state is entered even when there is nothing to delete, such as at
the very end of the buffer.

COUNT changes that many characters when no region is active.  A negative
COUNT changes that many characters before point and a COUNT of zero
changes none, matching `delete-char'; either way INSERT state is still
entered, which is what was actually asked for."
  (interactive "p")
  (if (use-region-p)
      (if (bound-and-true-p rectangle-mark-mode)
          (progn
            (call-interactively #'string-rectangle)
            ;; Explicit rather than implicit: the minibuffer
            ;; save/restore in `donkey--minibuffer-exit' already tends to
            ;; land back in Normal here, but that depends on this having
            ;; been reached FROM Normal state, which nothing guarantees
            ;; for a command also callable via \\[execute-extended-command].
            (donkey-enter-normal))
        (delete-region (mark) (point))
        (donkey-enter-insert))
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
  "Saved RET binding from buffer's local map when entering DONKEY Normal.

Captured once, in a `donkey-normal-mode-hook' function further down
this section, for any buffer whose major mode is NOT in
`donkey-editing-modes' -- e.g. `dired-mode', `magit-status-mode', an
`org-agenda' buffer, or any other special-mode-derived listing/UI
buffer where RET normally does something specific to that mode
\(open a file, visit a commit, jump to an entry...\).  Read back by
`donkey--non-editing-enter-handler', which calls it directly instead
of blocking Enter the way `donkey-enter-dwim' does in editing modes --
so Enter still does whatever that buffer's own major mode expects,
even though `donkey-normal-mode' has taken over its keymap.")

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

(defun donkey--editing-mode-p ()
  "Return non-nil if current major mode is in `donkey-editing-modes'."
  (donkey--major-mode-in-p donkey-editing-modes))

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
Returns command symbol or nil if no handler matches."
  (let* ((parent (and (fboundp 'org-element-at-point)
                      (org-element-at-point)))
         (ctx (and (fboundp 'org-element-context)
                   (org-element-context)))
         (ancestors (and parent
                         (fboundp 'org-element-lineage)
                         (org-element-lineage parent)))
         ;; `org-element-at-point' resolves to the enclosing container
         ;; (e.g. plain-list, for a checkbox `item') rather than the
         ;; specific element covering point, when point sits exactly at
         ;; `line-beginning-position' -- a very common position after
         ;; most navigation (e.g. `j'/`k').  One character forward
         ;; reliably resolves to the actual element there.  It must be
         ;; tried right after `parent' and before `ancestors': `parent'
         ;; here is itself just an outer ancestor (e.g. plain-list), so
         ;; checking the real `ancestors' list first would let a broader,
         ;; less specific enclosing element (e.g. an outer TODO headline)
         ;; win over the correct, more specific match.
         (fallback-parent (and (fboundp 'org-element-at-point)
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
  "Handle Enter in `org-mode' and markdown modes.  Return t if handled."
  (when (or (eq major-mode 'org-mode)
            (eq major-mode 'markdown-mode)
            (eq major-mode 'gfm-mode))
    (let ((handler (donkey--find-enter-handler)))
      (when handler
        (donkey--execute-handler handler)
        t))))

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

Uses `org-element-at-point' to detect :todo-type property and
dispatches `org-todo' accordingly.  No keyword string parsing needed."
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
        (org-todo 'todo))
       (t
        (org-todo 'todo))))))

(when donkey-default-enter-rules-enabled
  (donkey-add-enter-rule item :checkbox org-toggle-checkbox)
  (donkey-add-enter-rule headline :todo-type donkey-org-todo)
  (donkey-add-enter-rule link nil org-open-at-point markdown-follow-thing-at-point browse-url-at-point))

(defun donkey-enter-dwim ()
  "Smart Return handler for DONKEY Normal state.

Bound to both RET and <enter> in `donkey-normal-mode-map'.  Tries, in
order, stopping at the first one that reports it handled the key:

1. `donkey--org-agenda-enter-handler' -- delegates to whatever
   `org-agenda-mode-map' itself binds RET to (open item, visit entry).
2. `donkey--org-mode-enter-handler' -- for `org-mode'/`markdown-mode'/
   `gfm-mode', dispatches via `donkey--find-enter-handler' against the
   element at point (see `donkey-add-enter-rule' to register more
   element-type/command rules, e.g. from `config.el').
3. `donkey--non-editing-enter-handler' -- outside `donkey-editing-modes'
   (`dired-mode', `magit-status-mode', etc.), falls through to
   whatever RET was ORIGINALLY bound to before Normal state's keymap
   took over, via `donkey--saved-ret-binding'.

If none of these handle it -- ordinary `prog-mode'/`text-mode' buffers
being edited as code or plain text -- RET does nothing at all, on
purpose: inserting a literal newline in Normal state is rarely what
was intended, which is the entire reason `donkey-editing-modes' exists."
  (interactive)
  (cond
   ((donkey--org-agenda-enter-handler))
   ((donkey--org-mode-enter-handler))
   ((donkey--non-editing-enter-handler))))

(add-hook 'donkey-normal-mode-hook
          (lambda ()
            (unless (donkey--editing-mode-p)
              (setq donkey--saved-ret-binding
                    (lookup-key (current-local-map) (kbd "RET")))))
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
            ;; `org-edit-special' is outside `unwind-protect' on purpose: if
            ;; IT fails, there is no edit buffer to exit from.  Everything
            ;; after it (notably `comment-or-uncomment-region', which
            ;; signals when the src block's language has no comment syntax
            ;; defined -- e.g. `fundamental-mode') must not skip
            ;; `org-edit-src-exit' on error, or the user is left stranded
            ;; in the temporary edit buffer/window instead of back in the
            ;; Org buffer.
            (unwind-protect
                (if has-region
                    (let* ((cur-line-in-edit (line-number-at-pos))
                           (diff (- cur-line-in-edit cur-line))
                           (last-line (line-number-at-pos (point-max)))
                           ;; Clamp to the edit buffer's own line range: the
                           ;; region may extend past either end of the src
                           ;; block (e.g. selected from ordinary Org prose
                           ;; above it down into the block), and only the
                           ;; part actually inside the block exists here to
                           ;; comment.  Without clamping, `forward-line'
                           ;; silently clamps the out-of-range motions
                           ;; itself, but does so AFTER the range's width
                           ;; has already been computed from the unclamped
                           ;; numbers -- shifting the whole range downward
                           ;; and commenting the wrong lines.  Confirmed
                           ;; live: selecting Org text above a block through
                           ;; the block's second line commented all three of
                           ;; its lines, including one entirely outside the
                           ;; selection.  Point is always inside the block
                           ;; here (`donkey--in-org-src-block-p' passed) and
                           ;; is one end of the region, so the clamped range
                           ;; is always non-empty.
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
  "Non-nil after showing clipboard warning once per session.

Prevents spamming users with repeated tips on every yank operation.")

(defun donkey--detect-clipboard-tools ()
  "Detect available system clipboard tools.

Checks for wl-clipboard (Wayland), xclip/xsel (X11), and
pbcopy/pbpaste (macOS).  On Windows, native clipboard integration
is assumed.  Returns non-nil if any tool or native support is found.

Called fresh every time rather than cached, since the answer can
differ per frame: a single `emacs --daemon' process can have both a
GUI frame (opened via `emacsclient -c') and a terminal frame (via
`emacsclient -t') at once, each with different clipboard capabilities,
and a value cached once at load time would go stale for whichever
frame didn't exist yet when the daemon started."
  (cond
   ;; macOS: always has pbcopy/pbpaste
   ((eq system-type 'darwin) t)
   ;; Windows: native clipboard integration, no external tools needed
   ((eq system-type 'windows-nt) t)
   ;; Linux/BSD: check for Wayland and X11 clipboard tools
   ((or (executable-find "wl-copy")
        (executable-find "xclip")
        (executable-find "xsel")) t)
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
        :native-comp (fboundp 'native-comp-available-p)
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

Invokes `clipboard-yank' when the function is available; otherwise
falls back to `yank'.  If `clipboard-yank' signals an error
\(empty or inaccessible clipboard), falls back to `yank' from the
kill ring and emits an informative message with platform context.
Shows platform-appropriate installation tips only once per session."
  (let ((platform-context
         (cond
          ((eq system-type 'darwin) "macOS")
          ((eq system-type 'windows-nt) "Windows")
          (t "Linux/BSD"))))
    (condition-case err
        (if (fboundp 'clipboard-yank)
            (clipboard-yank)
          (yank))
      (error
       (yank)
       (message "Clipboard unavailable on %s; yanked from kill ring (%s)."
                platform-context
                (error-message-string err)))))
  ;; Show tip only once, and only for platforms that actually need external tools
  (when (and (not donkey--clipboard-warning-shown)
             (not (display-graphic-p))
             (not (donkey--detect-clipboard-tools))
             (not (eq system-type 'darwin))
             (not (eq system-type 'windows-nt)))
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
   ((null killed-rectangle)
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

Widened here rather than in the motions: moving point past the line
instead would make `forward-line' count from one line further along than
the user is on, so a single `J' grew the selection by two lines.  Tried
and rejected -- the geometry the motions rely on is load-bearing.

Only for a live visual-line session.  A character-wise region made with
`v' means the characters it covers, and is returned untouched."
  (if (donkey--visual-line-session-active-p)
      (donkey--whole-line-span (region-beginning) (region-end))
    (cons (region-beginning) (region-end))))

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
(defun donkey-copy (&optional count)
  "Copy the active region, or the character at point if no region is active.

With lines banked via `donkey-bank-selection', copies all of them
\\(plus any active region's lines) as a single kill instead.

A visual-line selection made with `V' is widened to whole lines before
being copied.  The highlight stops at the end of the last line, so the
newline ending it never looks selected -- but it IS copied, and the kill
pastes back as a complete line instead of splicing onto whatever line
\"p\" lands in.  See `donkey--visual-line-region-bounds'.

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
   (cond
    ;; Before the bank: a rectangle on screen is the live selection, and
    ;; the live selection wins.  See `donkey--live-rectangle-p'.
    ((donkey--live-rectangle-p)
     (call-interactively #'copy-rectangle-as-kill))
    ((donkey--banked-selection-p)
     (donkey--copy-banked-selection))
    ((use-region-p)
     (let ((bounds (donkey--visual-line-region-bounds)))
       (kill-ring-save (car bounds) (cdr bounds))))
    ((zerop n) nil)
    ((/= target (point))
     (kill-ring-save (point) target))
    ((< n 0)
     (message "Beginning of buffer -- nothing to copy"))
    (t
     (message "End of buffer -- nothing to copy"))))
  (deactivate-mark))

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
signalling.  A negative COUNT deletes that many characters before point
and a COUNT of zero deletes none, matching `delete-char'."
  (interactive "p")
  (let* ((n (or count 1))
         (target (max (point-min) (min (point-max) (+ (point) n)))))
   (cond
    ;; Before the bank, for the reason `donkey-copy' gives: see
    ;; `donkey--live-rectangle-p'.
    ((donkey--live-rectangle-p)
     (call-interactively #'kill-rectangle))
    ((donkey--banked-selection-p)
     (donkey--delete-banked-selection))
    ((use-region-p)
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
  "Pull the FOLLOWING line up onto this one, fixing up whitespace.

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
`V y', a banked copy and `D' were all checked."
  (interactive "p")
  (let ((n (max 0 (or count 1)))
        (joined 0))
    (while (and (> n 0) (not (donkey--no-line-below-p)))
      (join-line 1)
      (setq joined (1+ joined)
            n (1- n)))
    (when (and (zerop joined) (> (or count 1) 0))
      (message "No line below to join"))))

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

;; Three things worth knowing before changing this:
;;
;; The rectangle branch exists because `self-insert-command' operates on
;; `region-beginning'/`region-end' as a single linear span.  Run directly
;; against a rectangle selection it inserts the delimiters at the
;; rectangle's linear start/end buffer positions rather than on each covered
;; line, corrupting the buffer instead of wrapping anything.
;;
;; `delete-selection-mode' does NOT eat the selection here, despite being
;; active across the insertion: it acts from `pre-command-hook' on
;; `this-command's `delete-selection' property, and `this-command' is this
;; command, not the `self-insert-command' invoked from inside it.
;;
;; The return to Normal is in an `unwind-protect' because this is the one
;; command that enters Insert state BEFORE doing its real work.  If
;; `self-insert-command' signals, the transition back would be skipped and
;; leave the buffer stuck in Insert.  A read-only buffer does exactly that --
;; confirmed live: pressing a wrap delimiter over a region there reported
;; "Buffer is read-only" and silently left the modeline on DONKEY[I], from a
;; key pressed in Normal state.  The error still propagates after the
;; cleanup runs.
;;
;; Wrapping with `electric-pair-mode' confirmed live: selecting "hello" and
;; pressing "(" yields "(hello)".
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
the still-active region and wrap it.  Emacs's built-in
`electric-pair-mode' is enough; Smartparens' region-wrap works too.
With neither enabled the character is simply inserted at point, since
nothing is listening.  Then returns to Normal state, even if the
insertion signals."
  (interactive)
  (cond
   ((not (use-region-p))
    (call-interactively #'undefined))
   ;; The delimiter comes from `last-command-event', so this only means
   ;; anything when the invoking event IS a character.  Reached via
   ;; \\[execute-extended-command], or from a non-character binding such
   ;; as a function key: the rectangle path then hands a symbol to
   ;; `string' and signals `wrong-type-argument', and the linear path
   ;; hands it to `self-insert-command', which cannot insert it either.
   ((not (characterp last-command-event))
    (call-interactively #'undefined))
   ((bound-and-true-p rectangle-mark-mode)
    (donkey--wrap-rectangle-region last-command-event))
   (t
    (donkey-insert-mode 1)
    (unwind-protect
        (self-insert-command 1)
      (donkey--exit-insert)))))

;;; ---------------------------------------------------------------------------
;;; Mark and Text Object Selection Commands
;;; ---------------------------------------------------------------------------

(defun donkey--ensure-non-rectangle-selection ()
  "Disable `rectangle-mark-mode' if it is currently active.

`rectangle-mark-mode' only auto-disables via `deactivate-mark-hook',
which fires on the mark's active -> inactive transition -- not when a
command simply repositions an ALREADY-active mark, which is exactly
what `push-mark'/`set-mark'/`activate-mark' do for every Donkey
selection command (`donkey-mark-inner', `donkey-mark-paragraph',
`donkey-set-mark', etc.).  Without this, a rectangle selection left
active from an earlier, unrelated `donkey-rectangle-mark-mode' session
would silently persist underneath a brand new, intended-to-be-linear
selection, and the next `donkey-copy'/`donkey-delete'/`donkey-yank'
would misinterpret the new selection as a rectangle instead of the
intended linear span.  Confirmed live: after `m v' (rectangle-mark) on
one line, then `m p' (mark-paragraph) elsewhere without cancelling the
rectangle first, pressing `d' silently killed a zero-width \"rectangle\"
\(one empty string per line) instead of deleting the paragraph, with no
error and no visible change to the buffer at all.

Called at the start of every Donkey command that establishes a new
selection, before that command's own `push-mark'/`set-mark' call."
  (when (bound-and-true-p rectangle-mark-mode)
    (rectangle-mark-mode -1)))

(defvar-local donkey-visual-anchor nil
  "Anchor position for visual line selection.")

(defun donkey--clear-visual-anchor ()
  "Clear `donkey-visual-anchor' whenever the mark is deactivated.

Runs on `deactivate-mark-hook', so the anchor never survives past its
region regardless of what deactivated the mark — this command,
`keyboard-quit', or anything else.  Without this, a stale anchor left
over from an abandoned visual-line selection could hijack a later,
unrelated region activation (e.g. via `set-mark-command') in the same
buffer."
  (setq donkey-visual-anchor nil))

(add-hook 'deactivate-mark-hook #'donkey--clear-visual-anchor)

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

It also still rejects a STALE anchor, which is what this predicate
exists for.  Setting a brand new region while one is already active
\(e.g. `donkey-mark-inner', `donkey-mark-outer', or any other mark
command) never runs `deactivate-mark-hook' -- that hook only fires on
the active -> inactive transition, not when the region is simply
repositioned -- so `donkey--clear-visual-anchor' never gets a chance
to clear an anchor left over from an earlier session.  Those commands
do move the mark, though, to a position unrelated to the old anchor's
line, so the check below fails and the stale anchor is correctly
ignored.  Confirmed live: pressing `J' right after using
`donkey-mark-inner' to select \"hello\" (with a leftover anchor from an
earlier visual-line session) snapped the region all the way back to
the visual-line session's original anchor line instead of extending
\"hello\" by one line."
  (and (region-active-p)
       donkey-visual-anchor
       ;; An anchor outside the accessible portion is not a session this
       ;; can continue.  Buffer positions are absolute and narrowing does
       ;; not move them, so `V' followed by \\[narrow-to-region] (or
       ;; `org-narrow-to-subtree') leaves the anchor pointing at text the
       ;; buffer is no longer showing.  Both `goto-char' below and the
       ;; `set-mark' the J/K commands do afterwards silently CLAMP there
       ;; rather than signalling, so the selection quietly re-anchored on
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

;; Cancelling only a genuine session avoids reporting a misleading "Visual
;; line: cancelled" for a selection that was never a visual-line session.
;;
;; The highlight is left one character short deliberately -- see
;; `donkey--visual-line-region-bounds' for why the widening lives in
;; `donkey-copy' and `donkey-delete' rather than in the selection itself.
(defun donkey-visual-line-toggle ()
  "Start/cancel visual line selection.

Only cancels when a visual-line session is genuinely active (see
`donkey--visual-line-session-active-p').  Pressing this with some OTHER
active region -- a `donkey-mark-inner' selection, say -- starts a fresh
visual-line session anchored at the current line instead.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

What is highlighted is one character short of what `y' and `d' take.
The selection stops at the end of the last line, so the newline ending it
is NOT shown as selected -- but `donkey-copy' and `donkey-delete' widen a
live session to whole lines before acting, so the line break goes with
it: `d' removes the line outright rather than emptying it, and `y' gives
a kill that pastes back as a complete line."
  (interactive)
  (if (donkey--visual-line-session-active-p)
      (progn
        (deactivate-mark)
        (message "Visual line: cancelled"))
    (donkey--ensure-non-rectangle-selection)
    (setq donkey-visual-anchor (line-beginning-position))
    (set-mark (line-beginning-position))
    (end-of-line)
    (activate-mark)
    (message "Visual line: J/K whole lines, j/k by char, V to cancel")))

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
\[universal-argument] 5 J move a single line while
\[universal-argument] 5 j moved five."
  (interactive "p")
  (if (donkey--visual-line-session-active-p)
      (progn
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
    (forward-line (or count 1))))

(defun donkey-visual-previous-line (&optional count)
  "Move up COUNT lines, extending the visual-line selection if active.

See `donkey--visual-line-session-active-p' for what \"active\" means
here; otherwise this is a plain `forward-line' with a negative count.

Mirrors `donkey-visual-next-line': moving up while already above the
anchor keeps growing upward; moving up while still below the anchor
shrinks the selection back down toward it instead, covering the case
where `K' moves point up past the anchor line.

COUNT defaults to 1, and a negative COUNT moves down instead; see
`donkey-visual-next-line' for why no accumulation is needed."
  (interactive "p")
  (if (donkey--visual-line-session-active-p)
      (progn
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
    (forward-line (- (or count 1)))))

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
        (deactivate-mark))
    (let ((had-active-region mark-active))
      (rectangle-mark-mode 1)
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
          (end-of-buffer nil))))))

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
pairs you never use.  Order matters only for the `read-char' prompt
string, which lists the OPEN characters in this order."
  :type '(alist :key-type (character :tag "Open")
                :value-type (character :tag "Close"))
  :group 'donkey)

(defun donkey--mark-pair-open-chars-string (separator)
  "Return the open characters of `donkey-mark-pair-delimiters' joined by SEPARATOR."
  (mapconcat (lambda (pair) (char-to-string (car pair)))
             donkey-mark-pair-delimiters separator))

(defun donkey--mark-pair-prompt ()
  "Build the `read-char' prompt string from `donkey-mark-pair-delimiters'."
  (format "Char (%s): " (donkey--mark-pair-open-chars-string "")))

(defun donkey--mark-pair-unsupported-error (char)
  "Signal a `user-error' for CHAR not in `donkey-mark-pair-delimiters'.

A `user-error' rather than a bare `error': this is reached by answering
the `m i'/`m a' prompt with a character that is not a delimiter, which is
an ordinary typo on a prompt that lists nineteen accepted characters --
not a malfunction.  A bare `error' pops the debugger for anyone running
with `debug-on-error' on."
  (user-error "Unsupported delimiter '%c'.  Use: %s" char
              (donkey--mark-pair-open-chars-string " ")))

(defun donkey--mark-pair-read-delimiter ()
  "Return (OPEN-CHAR CLOSE-CHAR ON-OPENER) for the char pair to mark.

Uses the character at point when it is a recognized OPEN or CLOSE
delimiter (see `donkey-mark-pair-delimiters'); otherwise prompts via
`read-char'.  ON-OPENER is non-nil only when point sits on the OPEN
side -- when it sits on the CLOSE side of an asymmetric pair (e.g. `)'
for `(', where OPEN and CLOSE differ), OPEN-CHAR is still resolved
automatically here, but ON-OPENER comes back nil so
`donkey--mark-pair-positions' takes its search-backward-then-forward
path instead of assuming point is the opener."
  (let* ((default-char (char-after))
         (on-opener (and default-char (assq default-char donkey-mark-pair-delimiters)))
         (on-closer (and default-char (not on-opener)
                          (rassq default-char donkey-mark-pair-delimiters)))
         (open-char (cond (on-opener default-char)
                           (on-closer (car on-closer))
                           (t (read-char (donkey--mark-pair-prompt)))))
         (close-char (or (cdr (assq open-char donkey-mark-pair-delimiters))
                         (donkey--mark-pair-unsupported-error open-char))))
    (list open-char close-char on-opener)))

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
running out of buffer and signalling `search-failed', converted to the
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

(defun donkey--mark-pair-select (inner-p &optional count)
  "Shared implementation for `donkey-mark-inner'/`donkey-mark-outer'.

With INNER-P non-nil, selects the content between the delimiters,
excluding them; otherwise selects the delimiters too.

COUNT selects how many levels out to go -- see
`donkey--mark-pair-positions-nth'."
  (donkey--ensure-non-rectangle-selection)
  (pcase-let*
      ((`(,open-char ,close-char ,on-opener) (donkey--mark-pair-read-delimiter))
       (`(,start-pos . ,end-pos)
        (donkey--mark-pair-positions-nth open-char close-char on-opener
                                         (max 1 (or count 1)))))
    (push-mark (if inner-p (1+ start-pos) start-pos))
    (goto-char (if inner-p (1- end-pos) end-pos))
    (activate-mark)
    (when (>= (region-beginning) (region-end))
      (deactivate-mark)
      ;; A `user-error': an empty pair is ordinary in code -- `()' for a
      ;; no-argument call, `""' for an empty string -- so pressing `m i'
      ;; on one is a miss, not a malfunction, and a bare `error' popped
      ;; the debugger under `debug-on-error'.  `m a' on the same pair
      ;; still works, since there the delimiters themselves are content.
      (user-error "Empty selection between %c and %c" open-char close-char))
    (message (if inner-p
                 "Selected content for '%c'"
               "Selected OUTER content including '%c'")
             open-char)))

(defun donkey-mark-inner (&optional count)
  "Mark text INSIDE CHAR pairs (excluding delimiters).

Auto-detects the delimiter when point is on a recognized OPEN or CLOSE
character in `donkey-mark-pair-delimiters'; otherwise prompts via
`read-char'.  For asymmetric pairs (e.g. `(' and `)'), nested
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

COUNT selects how many levels out to go, so a count of 2 from inside a
nested pair marks the pair enclosing it -- including for symmetric
delimiters, where a level counts occurrences outward.  See
`donkey-mark-inner'."
  (interactive "p")
  (donkey--mark-pair-select nil count))

(defun donkey--mark-sexp-select (inner-p &optional count)
  "Shared implementation for `donkey-mark-sexp-inner'/`donkey-mark-sexp-outer'.

Uses the syntax table to identify delimiters (parentheses, brackets,
braces).  If point is on an opening or closing delimiter, uses that
pair; if point is inside a pair, finds the enclosing delimiters.

COUNT selects how many levels out to go, so a count of 2 marks the pair
enclosing the one that would be marked without it.  Point already on an
opening delimiter counts as being at that pair, so a count of 1 there
uses it rather than its parent.

With INNER-P non-nil, selects the expression's content, excluding its
delimiters, and errors if that content is empty (e.g. \"()\");
otherwise selects the delimiters too."
  (donkey--ensure-non-rectangle-selection)
  (let ((levels (max 1 (or count 1))))
    (condition-case nil
        (backward-up-list (if (looking-at "\\s(") (1- levels) levels))
      (scan-error
       (user-error "Not inside a balanced expression"))))
  (let ((start (if inner-p (1+ (point)) (point))) end)
    (condition-case nil
        (setq end (progn (forward-list 1)
                          (if inner-p (1- (point)) (point))))
      (scan-error
       (user-error "Unbalanced expression")))
    (when (and inner-p (>= start end))
      (user-error "Empty expression"))
    (push-mark start t)
    (goto-char end)
    (activate-mark)
    (message (if inner-p "Marked inner expression" "Marked outer expression"))))

(defun donkey-mark-sexp-inner (&optional count)
  "Mark content inside the balanced expression at point.

Uses the syntax table to identify delimiters (parentheses,
brackets, braces).  If point is on an opening or closing
delimiter, marks content within that pair.  If point is inside
a pair, finds the enclosing delimiters and marks everything
within, excluding the delimiters themselves.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

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

COUNT selects how many levels out to go."
  (interactive "p")
  (donkey--mark-sexp-select nil count))

(defun donkey--point-on-word-or-symbol-char-p ()
  "Return non-nil if the character after point has word or symbol syntax.

Used by `donkey-mark-word'/`donkey-mark-symbol' to decide whether
point already sits inside a word/symbol -- in which case there's
nothing to skip back to -- or needs to first move onto the nearest
one before marking it."
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
;; not adding the behaviour to `m s'; it was removing it from the rest.
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
(defun donkey--mark-extending-p ()
  "Return non-nil when a mark command should grow its selection.

True when the command now running is the one that ran last and it left
a mark behind -- the same test `mark-end-of-sentence' applies, which is
why `m s' has grown on a second press since before there was a rule.
Any other key in between ends the run."
  (and (eq last-command this-command)
       (mark t)
       t))

(defun donkey-mark-word (&optional count)
  "Select the entire word at or adjacent to point.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

Pressing the key again immediately EXTENDS the selection by another
word rather than re-marking the same one, and keeps extending until
the buffer runs out.  See `donkey--mark-extending-p'.

COUNT marks that many words.  A negative COUNT marks that many words
before the one point normalises onto, and a COUNT of zero marks nothing,
matching how `mark-word' itself reads its argument."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((extend (donkey--mark-extending-p)))
    (unless extend
      (unless (donkey--point-on-word-or-symbol-char-p)
        (backward-word 1))
      ;; See `donkey-mark-symbol' for why this is a `user-error' rather
      ;; than letting `beginning-of-thing' signal a bare `error': a
      ;; buffer with no word before point at all (empty, or nothing but
      ;; whitespace and punctuation) is a normal thing to press this on
      ;; by accident.
      (unless (thing-at-point 'word)
        (user-error "No word at or before point"))
      (beginning-of-thing 'word))
    ;; The normalisation above is skipped when extending: it walks point
    ;; back to the START of the word already selected, and `mark-word'
    ;; measures its extension from there, so running it would grow the
    ;; region by nothing and then by one word from the wrong end.
    (mark-word (or count 1) extend))
  (message "Word marked"))

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

COUNT marks that many sentences.  Unlike the other mark commands a COUNT
below 1 is treated as 1 here: this command is defined in terms of the
sentence AHEAD of point -- it normalises forward and then reports \"No
sentence after point\" for a selection that ends behind where it started
-- so a zero or negative count has nothing it could mean but that error.
A COUNT reaching past the last sentence marks what there is and stops,
the way every other counted command does.

Pressing the key again immediately EXTENDS the selection by another
sentence rather than re-marking the same one, and keeps extending until
the buffer runs out.  That comes from `mark-end-of-sentence', which
grows the region whenever `last-command' is this command again; the
other mark commands do not, since `mark-word' and friends gate it behind
an ALLOW-EXTEND argument that is nil when called from Lisp."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((origin (point)))
   (condition-case nil
      (progn
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
        (backward-sentence 1))
    ;; Before the general handler, which would otherwise catch this: the
    ;; forward step signals `end-of-buffer' both for a buffer with no
    ;; sentence in it at all AND for a real one whose last sentence has
    ;; no newline after it, where point-max IS the trailing gap.  Those
    ;; want different answers, and reporting "No sentence at or before
    ;; point" for the second contradicts a screen that is showing three
    ;; -- the same contradiction the count-overrun handler below was
    ;; written to stop, reached by the other route.
    ;;
    ;; A trailing newline hides it, since the forward step then has
    ;; somewhere to land and the trailing-gap guard at the end produces
    ;; the accurate message.  So which of the two a reader saw depended
    ;; on whether their file ended with a newline.
    (end-of-buffer
     (user-error
      (if (string-match-p "[^[:space:]\n]"
                          (buffer-substring-no-properties (point-min) origin))
          "No sentence after point"
        "No sentence at or before point")))
    (error (user-error "No sentence at or before point")))
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
    (error (user-error "No sentence at or before point")))
  ;; Going forward first means the motions no longer signal in a buffer
  ;; holding nothing but whitespace -- they simply walk to its end and
  ;; back, "marking" the blank.  Reject that here so such a buffer still
  ;; reports rather than selecting nothing of substance.
  (when (string-match-p "\\`[[:space:]\n]*\\'"
                        (buffer-substring-no-properties (region-beginning)
                                                        (region-end)))
    (deactivate-mark)
    (user-error "No sentence at or before point"))
  ;; Standing in the gap after a sentence means the one being asked for is
  ;; the one COMING, not the one just left behind -- which is what the
  ;; motions above already give for a gap in the middle of the text.  In
  ;; the trailing gap there is no next sentence to give, and marking the
  ;; previous one instead would answer a question that was not asked, so
  ;; say so.  A cursor inside a sentence always leaves that sentence's end
  ;; ahead of it, so this only ever fires from a gap.
  (when (<= (region-end) origin)
    (deactivate-mark)
    (user-error "No sentence after point"))
  (message "Sentence marked")))

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
place to press this from and marks the paragraph above, so gating on
what is under point would reject work this command does correctly.  A
COUNT of zero is exempt -- it is documented to mark nothing, and the
nothing came from the count rather than from the buffer.

Point is left at the START of the selection and the mark at its end,
which is where `mark-paragraph' leaves them and where
`donkey-mark-word', `donkey-mark-symbol' and `donkey-mark-sentence'
leave them.

COUNT marks that many paragraphs.  A negative COUNT marks that many
paragraphs before the one point normalises onto, and a COUNT of zero
marks nothing, matching how `forward-paragraph' reads its argument."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (let ((n (or count 1)))
    (if (donkey--mark-extending-p)
        ;; Grown by moving the MARK, which is the end this command owns
        ;; -- the same shape as `donkey-mark-symbol' and as
        ;; `mark-paragraph's own ALLOW-EXTEND branch.
        (set-mark (save-excursion
                    (goto-char (mark))
                    (forward-paragraph n)
                    (point)))
      ;; Point ends at the START, mark at the end.  It used to be the
      ;; other way round, which made this the only mark command that
      ;; inverted native: `mark-paragraph' finishes with
      ;; `backward-paragraph' and leaves point where the selection
      ;; begins.  Being the odd one out cost something concrete -- the
      ;; first attempt at extending here handed native's ALLOW-EXTEND
      ;; branch a mark-at-start region, and since native grows by
      ;; pushing the MARK forward it collapsed the selection onto the
      ;; paragraph's first character.
      (backward-paragraph 1)
      (let ((start (point)))
        (forward-paragraph n)
        (push-mark (point) nil t)
        (goto-char start))
      (activate-mark))
    (when (and (/= n 0)
               (string-match-p "\\`[[:space:]\n]*\\'"
                               (buffer-substring-no-properties (region-beginning)
                                                               (region-end))))
      (deactivate-mark)
      (user-error "No paragraph at or before point"))
    (message "Paragraph marked")))

(defun donkey-mark-symbol (&optional count)
  "Select the entire symbol at or adjacent to point.

Trailing commas or periods are omitted from the selection.

See `donkey--ensure-non-rectangle-selection' for why a stale active
`rectangle-mark-mode' selection is disabled first.

COUNT marks that many symbols.  A negative COUNT marks that many symbols
before the one point normalises onto, and a COUNT of zero marks nothing,
matching how `forward-sexp' reads its argument."
  (interactive "p")
  (donkey--ensure-non-rectangle-selection)
  (if (donkey--mark-extending-p)
      ;; Grown by moving the MARK, which is where this command leaves the
      ;; far end of its selection -- it finishes with `backward-sexp', so
      ;; point sits at the START.  The same shape as `mark-word's own
      ;; extend branch, and the punctuation trim has to run again because
      ;; the new end is a new symbol with its own possible trailing "."
      (let ((n (or count 1)))
        (set-mark (save-excursion
                    (goto-char (mark))
                    (forward-sexp n)
                    (when (> n 0)
                      (while (memq (char-before) '(?, ?.))
                        (backward-char 1)))
                    (point))))
    (unless (donkey--point-on-word-or-symbol-char-p)
      (condition-case nil
          (backward-sexp 1)
        (scan-error nil)))
    ;; `beginning-of-thing' signals a bare `error' when there is no
    ;; symbol to be found, which pops the debugger for anyone running
    ;; with `debug-on-error' on.  Reaching it is ordinary, not
    ;; exceptional: `backward-sexp' above lands on whatever sexp precedes
    ;; point, which on a blank line in code is typically a bracket rather
    ;; than a symbol -- confirmed with point on the trailing empty line
    ;; of "(foo bar)".
    (unless (thing-at-point 'symbol)
      (user-error "No symbol at or before point"))
    (beginning-of-thing 'symbol)
    (let ((n (or count 1)))
      (forward-sexp n)
      ;; Only a forward run leaves point at the far END of the selection,
      ;; where a trailing "," or "." is the thing to drop.  A negative
      ;; count leaves point at the region's START instead, and backing up
      ;; over punctuation there would reach into the symbol before it.
      (when (> n 0)
        (while (memq (char-before) '(?, ?.))
          (backward-char 1)))
      (push-mark (point) t)
      ;; Back over the same number of symbols the first step covered.
      ;; Going back one regardless left the region holding only the LAST
      ;; symbol of a counted run -- a count of 2 over "foo-a bar-b"
      ;; marked just "bar-b".
      (backward-sexp n))
    (activate-mark))
  (message "Symbol marked"))

;; The non-toggling behavior is left as stock deliberately: "v" is
;; `set-mark-command' and nothing else, so `C-u v' still pops the mark ring
;; and anything built on `set-mark-command' keeps working.  Documented in
;; the tutor and the README rather than papered over here.
(defun donkey-set-mark ()
  "Call `set-mark-command', disabling a stale `rectangle-mark-mode' first.

See `donkey--ensure-non-rectangle-selection' for why.

This does NOT toggle, unlike its two neighbours `donkey-visual-line-toggle'
\(\"V\") and `donkey-rectangle-mark-mode' (\"m v\"), which both cancel the
selection they started when pressed again.  `set-mark-command' re-anchors:
a second press drops a fresh mark at point and carries on selecting from
there, so the previous selection is discarded but the buffer is still in
a selecting state.  \\[keyboard-quit] is what lets go."
  (interactive)
  (donkey--ensure-non-rectangle-selection)
  (call-interactively #'set-mark-command))

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

Inherits `secondary-selection' so banked lines stay visually distinct
from the live region, which is the whole point: while banking you are
looking at two different things at once -- what is already set aside,
and what is selected right now."
  :group 'donkey)

(defvar-local donkey--banked-overlays nil
  "Overlays covering the whole lines banked in this buffer.

Overlays rather than plain positions: they move with the text as the
buffer is edited, they die automatically with the buffer, and they
double as the visual feedback -- one structure instead of a position
list plus a parallel set of highlights that could drift apart.")

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

An overlay collapses to zero width when the line it banked is later
removed by ordinary editing.  Such an overlay highlights nothing, so
the bank is invisible, yet it would still count as live: `y'/`d' would
act on the empty bank instead of on the character at point, pushing
\"\" over whatever was last copied -- the same silent empty-kill this
package already guards against at `point-max'.  It could not even be
toggled off, since `donkey--banked-overlay-at' requires POS to be
strictly inside the overlay and no position is ever inside an empty
range."
  (setq donkey--banked-overlays
        (seq-filter (lambda (ov)
                      (or (and (overlay-buffer ov)
                               (< (overlay-start ov) (overlay-end ov)))
                          (ignore (delete-overlay ov))))
                    donkey--banked-overlays)))

(defun donkey--banked-spans ()
  "Return usable banked spans as a list of (START . END), in buffer order.

Prunes collapsed overlays first (see `donkey--prune-banked-overlays'),
so every span returned covers real text.

Spans reaching outside the buffer's accessible portion are then left
out.  Overlay positions are absolute and unaffected by narrowing, so a
line banked before a `narrow-to-region' still reports its original
positions afterwards -- and `buffer-substring'/`delete-region' signal a
bare `args-out-of-range' for those.  Confirmed live: banking a line,
narrowing past it with \\[narrow-to-region], then pressing \"y\" reported
\"Args out of range: #<buffer *live*>, 1, 6\".

Filtered rather than pruned, because narrowing is temporary: the
overlays survive untouched and count again once the buffer is widened.
Everything reading spans therefore agrees on one definition -- what is
banked AND reachable right now -- so the counts reported while narrowed
describe exactly what \"y\" and \"d\" will act on."
  (donkey--prune-banked-overlays)
  (sort (delq nil
              (mapcar (lambda (ov)
                        (let ((start (overlay-start ov))
                              (end (overlay-end ov)))
                          (and (>= start (point-min))
                               (<= end (point-max))
                               (cons start end))))
                      donkey--banked-overlays))
        (lambda (a b) (< (car a) (car b)))))

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

The first half of donkey's rule for the one case banking cannot
compose with: THE LIVE SELECTION YOU ARE LOOKING AT WINS, AND THE BANK
IS THE FALLBACK.  A rectangle is columns and a bank is whole lines, so
no command can act on both; something has to give way, and the thing
you just drew and can see is the better guess at what you meant.

`y' and `d' therefore take the rectangle and leave every bank standing,
and `p' does the reverse in the reverse situation -- a bank outranks a
rectangle merely sitting in `killed-rectangle' from an earlier copy,
because that one is not on screen and the banks are.

The three used to disagree, each silently: `y' and `d' took the banks
and ignored a rectangle the user was looking at (leaving
`killed-rectangle' empty, so the rectangle they thought they had cut
was not even there to paste), while `p' took the rectangle and ignored
banks highlighted on screen.  Drawing a rectangle over two rows and
pressing `d' deleted three whole lines.

Discarding the banks at `m v' instead was considered and rejected:
banks are not in the undo system, `m v' sits on the same prefix as
`m w', `m u' and `m U', and a slip would throw away a collection built
up across a long file with no way back.  `m DEL' stays the only key
that discards everything.  This rule costs nothing and keeps the
workflow it would have broken -- banking lines as you scroll, fixing a
column somewhere in the middle, and still having the banks afterwards.

Note `m l' already collapses the two states in the other direction: it
banks the whole lines a rectangle covers and drops
`rectangle-mark-mode' with it, since `donkey-bank-selection'
deactivates the mark."
  (and (use-region-p) (bound-and-true-p rectangle-mark-mode)))

(defun donkey-clear-banked-selection ()
  "Discard every banked line in this buffer.

Leaves the buffer text and the live region alone -- this only throws
away what `donkey-bank-selection' set aside."
  (interactive)
  (let ((count (donkey--banked-line-count)))
    (mapc #'delete-overlay donkey--banked-overlays)
    (setq donkey--banked-overlays nil)
    ;; `any' rather than `interactive': the point is to stay quiet when
    ;; `donkey-copy'/`donkey-delete' clear the bank as part of consuming
    ;; it, which are plain Lisp calls.  `interactive' would additionally
    ;; report nothing under `noninteractive' or a keyboard macro, where
    ;; the feedback is still wanted.
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

(defun donkey-bank-selection ()
  "Set the current selection aside and release the mark to keep navigating.

Banks every whole line the active region touches (or just the current
line when no region is active), highlights them with
`donkey-banked-selection', then deactivates the mark so ordinary
navigation and a fresh selection can continue.  Repeat as often as
needed: `donkey-copy', `donkey-delete' and `donkey-yank' then act on all
banked lines at once, plus whatever region happens to be active at the
time, so the final piece never has to be banked explicitly.

`donkey-change' is the exception -- it changes the character at point
and leaves banks standing; see there.  The prompt says \"y/d/p\" for
that reason, and said \"y/d\" until `donkey-yank' learned to replace a
bank, which left the one command that had grown a new use unadvertised
on screen.

This toggles, with or without a region.  With no region, point on an
already-banked line unbanks that line.  With a region whose lines are
ALL already banked, the whole block is unbanked in one press -- which
is how to take back a multi-line bank without either walking it line
by line or clearing every other bank too.  A region covering a only
partly-banked block banks the rest instead, so a block only ever
toggles off once it is uniformly on; press again to then clear it.

Banked lines are discarded automatically once `donkey-copy',
`donkey-delete' or `donkey-yank' consumes them -- and only the spans
actually acted on are spent, so banks outside the accessible portion of
a narrowed buffer survive to count again;
`donkey-clear-banked-selection' discards them without doing anything
else."
  (interactive)
  (if (use-region-p)
      (let* ((span (donkey--whole-line-span (region-beginning) (region-end)))
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
    (let ((existing (donkey--banked-overlay-at (point))))
      (if existing
          (progn
            (delete-overlay existing)
            (setq donkey--banked-overlays
                  (delq existing donkey--banked-overlays))
            (message "Unbanked this line (%d total)"
                     (donkey--banked-line-count)))
        (let ((span (donkey--whole-line-span (point) (point))))
          ;; The empty final line of a newline-terminated buffer spans no
          ;; text at all: `line-beginning-position' and the clamped end
          ;; both land on `point-max'.  `donkey--bank-span' walks the span
          ;; line by line, so its loop body never runs and no overlay is
          ;; created -- reporting "Banked this line" there claimed a bank
          ;; that did not exist, in the same breath as "(0 total)".  Most
          ;; files end in a newline, so `g e' lands on exactly this spot.
          (if (>= (car span) (cdr span))
              (message "Nothing to bank -- empty final line")
            (donkey--bank-span (car span) (cdr span))
            (message "Banked this line (%d total) -- navigate, then y/d/p"
                     (donkey--banked-line-count))))))))

(defun donkey--banked-overlay-at (pos)
  "Return the banked overlay covering the line POS is on, or nil.

The containment test is anchored at that line's own start rather than
at POS itself.  A strict interior test on POS misses point sitting at
`point-max' on a banked FINAL line with no trailing newline, where the
overlay ends exactly at point -- confirmed: pressing the bank key there
re-banked the line instead of toggling it off, since the lookup found
nothing to remove.

Candidates come from `overlays-in', which Emacs answers from its own
position index, rather than from a scan of `donkey--banked-overlays'.
Scanning made this linear in the number of banked lines, and
`donkey--bank-span' calls it once per line, so banking a region cost
quadratic time: 0.01s for 200 lines, 0.22s for 1000, and 1.81s for 3000
-- a visible freeze for something as ordinary as selecting a whole file
and banking it.  The `donkey-banked' property is what distinguishes our
overlays from any other package's at the same position."
  (let* ((line-start (car (donkey--whole-line-span pos pos)))
         (probe-end (min (point-max) (1+ line-start))))
    (seq-find (lambda (ov)
                (and (overlay-get ov 'donkey-banked)
                     (<= (overlay-start ov) line-start)
                     (< line-start (overlay-end ov))))
              (overlays-in line-start probe-end))))

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
  (let ((ov (donkey--banked-overlay-at (point))))
    (if (not ov)
        (message "No banked line at point")
      (delete-overlay ov)
      (setq donkey--banked-overlays (delq ov donkey--banked-overlays))
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

FN receives a (START . END) cons.  Walking line by line, rather than
treating BEG..END as one range, is what keeps banking per-line
throughout -- see `donkey--bank-span' for why that matters."
  (declare (indent 2))
  (save-excursion
    (goto-char (min beg (point-max)))
    ;; END clamped and an explicit stop, so this cannot spin.  A span
    ;; reaching past `point-max' -- a stale one computed before the buffer
    ;; shrank, say -- leaves `donkey--whole-line-span' returning the
    ;; position it was given, and the loop would never advance.  Clamping
    ;; alone does not fix it either: `goto-char' clamps too, so jumping to
    ;; END would leave point short of it and the condition still true.  A
    ;; hung Emacs is a far worse failure than a span walked one line short.
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
  "Unbank every whole line in BEG..END that is currently banked."
  (donkey--map-line-spans beg end
    (lambda (span)
      (let ((ov (donkey--banked-overlay-at (car span))))
        (when ov
          (delete-overlay ov)
          (setq donkey--banked-overlays
                (delq ov donkey--banked-overlays)))))))

(defun donkey--bank-span (beg end)
  "Bank every whole line in BEG..END that is not already banked.

Creates one overlay per LINE rather than one spanning the whole run.
Adjacent lines would otherwise be absorbed into a single overlay, and
unbanking any line of that run would then drop the entire run instead
of just that one line -- confirmed live: banking two adjacent lines
and pressing the bank key again on the second reported \"Unbanked this
line (0 total)\" rather than leaving the first still banked.

Nothing is lost by keeping them separate: `donkey--effective-line-spans'
merges adjacent spans at use time, so a contiguous run is still copied
and deleted as one piece, and identically-faced adjacent overlays are
indistinguishable on screen."
  (donkey--map-line-spans beg end
    (lambda (line-span)
      (unless (donkey--banked-overlay-at (car line-span))
        (let ((ov (make-overlay (car line-span) (cdr line-span) nil nil t)))
          (overlay-put ov 'face 'donkey-banked-selection)
          (overlay-put ov 'donkey-banked t)
          (overlay-put ov 'priority -50)
          ;; Emptying a buffer collapses an overlay to zero width rather than
          ;; removing it, and this one advances with text inserted at its end
          ;; -- so refilling the buffer regrows it over whatever replaced the
          ;; line it banked.  A bank of one line silently becomes a bank of
          ;; the whole buffer, which `y' and `d' then act on while
          ;; `donkey--banked-line-count' still reports one.
          ;; `donkey--prune-banked-overlays' cannot catch it: the insertion
          ;; re-expands the overlay before the spans are next asked for, so
          ;; it never looks collapsed.  Evaporating removes it with its text.
          (overlay-put ov 'evaporate t)
          (push ov donkey--banked-overlays))))))

(defun donkey--banked-line-count ()
  "Return how many lines are currently banked.

Counts LINES, not overlays.  `donkey--bank-span' makes one overlay per
line, so the two agree right up until an edit grows one past the line it
was created for -- and an overlay that advances with text inserted at its
end does exactly that the moment a newline is typed inside a banked line.
Reporting the number of overlays then reported one line while `y' and `d'
acted on two, the same divergence `donkey--bank-span' documents for the
emptied-buffer case that `evaporate' handles: evaporating cannot help
here, because the text was never deleted.

`donkey--span-line-count' is what `donkey-copy' and `donkey-delete'
already use for the totals they report, so counting the same way is also
what keeps every message about the bank agreeing with every other."
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

The difference only shows under narrowing, and it showed as silent loss.
`donkey--banked-spans' filters to the accessible portion and promises
that the rest \"survive untouched and count again once the buffer is
widened\"; clearing the whole list broke that promise.  Banking two lines,
narrowing past one of them and pressing `y' copied the visible line --
correctly -- and threw the hidden one away without ever copying it.

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
replaces an active region -- previously `donkey-yank' was the one command
that could not see the bank at all, pasting at point and leaving the
highlighted lines sitting there untouched and still banked, while
`donkey-copy' and `donkey-delete' both acted on them and consumed them.

The paste lands where the FIRST span started, which is still a valid
position after the deletions: they run back to front, so nothing before
that span has moved by the time it is reached.

Deleted rather than killed, unlike `donkey--delete-banked-selection'.
That one is a kill because kill is the point of it; here `kill-new'
would push the replaced lines onto the kill ring and the paste below
would pull those back instead of what was being pasted.

Consumes the bank, the way `donkey-copy' and `donkey-delete' do."
  (let* ((spans (donkey--effective-line-spans))
         (lines (donkey--span-line-count spans))
         (target (car (car spans))))
    (dolist (span (reverse spans))
      (delete-region (car span) (cdr span)))
    (donkey--consume-banked-spans spans)
    (deactivate-mark)
    (goto-char target)
    (donkey--paste-times (or count 1) #'donkey--clipboard-yank)
    (message "Replaced %d line%s" lines (if (= 1 lines) "" "s"))))

(defun donkey--delete-banked-selection ()
  "Kill every banked line (plus any active region's lines) as one kill.

Deletes back to front so each span's positions stay valid while the
earlier ones are still being removed."
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
               (setq acc (append acc
                                 (donkey--desc-bindings-collect-leaves
                                  def (concat full-key " ")))))
              ((and (consp def) (keymapp (cdr def)))
               (setq acc (append acc
                                 (donkey--desc-bindings-collect-leaves
                                  (cdr def) (concat full-key " ")))))
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

(defun donkey-describe-bindings ()
  "Display all leaf keybindings in `donkey-normal-mode-map' with formatting.

Bindings are grouped by prefix, separated by blank rows and section
headers.  Command names are clickable buttons that open their
documentation."
  (interactive)
  (unless (boundp 'donkey-normal-mode-map)
    (user-error "Variable `donkey-normal-mode-map' is not defined yet"))
  (let* ((buf (get-buffer-create "*DONKEY Bindings*"))
         (raw (donkey--desc-bindings-collect-leaves donkey-normal-mode-map ""))
         ;; Sorted by GROUP first, then by key within the group.  Sorting
         ;; by key alone interleaves single keys with the prefix groups
         ;; alphabetically ("h" between "g t" and "m a"), and since a
         ;; header is emitted on every group transition, "Single Keys"
         ;; then appeared four separate times -- not the grouping the
         ;; docstring promises.  Single keys lead, prefixes follow in
         ;; alphabetical order.
         (sorted-raw
          (sort raw
                (lambda (a b)
                  (let ((ga (donkey--desc-bindings-group (car a)))
                        (gb (donkey--desc-bindings-group (car b))))
                    (cond
                     ((string= ga gb) (string< (car a) (car b)))
                     ((string= ga "single") t)
                     ((string= gb "single") nil)
                     (t (string< ga gb))))))))
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
      ;; Binding entries
      (let ((prev-group nil)
            (lines-added 0))
        (dolist (entry sorted-raw)
          (let* ((full-key (car entry))
                 (def      (cdr entry))
                 (group    (donkey--desc-bindings-group full-key))
                 (new-block-p (not (equal prev-group group))))
            ;; Header for every group, including the first -- otherwise
            ;; the leading block (single keys) is the one group left
            ;; unlabelled.  The blank separator is only wanted between
            ;; blocks, so it is skipped for the first.
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
                  prev-group  group))))
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
text you will practise on.  Changing it is the point -- nothing here is
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

Press it again to keep walking back through earlier positions.  It is a
recovery key rather than a filing system: for places you mean to return to
deliberately, Emacs' own bookmarks are the right tool.


Lesson 2 -- counts
------------------

Nearly every DONKEY command takes a count, and it always means the same
thing: how many.  Give it as C-u N before the key.

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
at the END is always left out, so \\[donkey-mark-symbol] on the last name in a
sentence gives you the name without the full stop.

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

A count means levels out, so \\`C-u 2' \\[donkey-mark-inner] \\`(' from the inner pair selects
outer one.

>> Put the cursor on \"deep\" below and press \\`C-u 2' \\[donkey-mark-inner] then \\`('.

   ---> outer (middle (deep) middle) outer

\\[donkey-mark-sexp-inner] and \\[donkey-mark-sexp-outer] do the same job without asking.  They read the
buffer's syntax table and find the enclosing brackets themselves,
whatever kind those turn out to be -- useful in code, where the nearest
pair is as likely to be square or curly as round.

    \\[donkey-mark-inner] and \\[donkey-mark-outer]   you name the delimiter
    \\[donkey-mark-sexp-inner] and \\[donkey-mark-sexp-outer]   DONKEY works it out

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
the first line and press \\[donkey-yank-rectangle] straight away -- the block goes
back where it came from.  A rectangle has its own paste key:
\\[donkey-yank] pastes ordinary text, \\[donkey-yank-rectangle] pastes columns,
and neither has to guess which you meant.

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
-- it is two, so \\[donkey-rectangle-mark-mode] followed by \\[donkey-change] on a tab-indented line
replaces the whole indent rather than one space of it.  Start from a
character you can see if you want to change just it.

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


Lesson 10 -- when two selections disagree
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

DONKEY is meant to be an addition, not a replacement.  In BOTH states your
\\`C-x' and \\`C-c' prefixes, \\[execute-extended-command], \\`C-h', isearch, the arrow keys, every
Meta binding and every package you have bound behave exactly as they always
did.  Nothing was taken away to make room for the letters above.

A modal editor cannot be entirely free, though, and the price is short
enough to state in full.

In INSERT state, one key changes: \\`C-g' returns to NORMAL state.  It still
clears the selection, and in the minibuffer it still quits, so the escape
hatch is where you expect it.  It stops a keyboard macro that is being
recorded, too; the only errand of Emacs' own \\`C-g' it skips is signalling
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
still works.  In dired, \\`n', \\`t', \\`q', \\`^' and \\`+' are still dired's.
Terminals and shells (eshell, term, vterm) stay in INSERT throughout, so
nothing is suppressed underneath a running process.  Their modeline says
DONKEY[E] rather than DONKEY[I]: still INSERT, but NORMAL state cannot
be reached there at all, so \\`C-g' quits the way stock Emacs does instead
of switching state.


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

Each key is wrapped in the \\=\\=` KEY \\=' escape rather than returned bare,
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
        (pop-to-buffer existing)
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

;; Wrap region with delimiter (region-active only; see donkey-wrap-region)
(dolist (ch donkey-wrap-delimiters)
  (keymap-set donkey-normal-mode-map (char-to-string ch) #'donkey-wrap-region))

;; Mark objects
(keymap-set donkey-normal-mode-map "m A" #'donkey-mark-sexp-outer)
(keymap-set donkey-normal-mode-map "m a" #'donkey-mark-outer)
(keymap-set donkey-normal-mode-map "m I" #'donkey-mark-sexp-inner)
(keymap-set donkey-normal-mode-map "m i" #'donkey-mark-inner)
(keymap-set donkey-normal-mode-map "m p" #'donkey-mark-paragraph)
(keymap-set donkey-normal-mode-map "m s" #'donkey-mark-sentence)
(keymap-set donkey-normal-mode-map "m v" #'donkey-rectangle-mark-mode)
(keymap-set donkey-normal-mode-map "m w" #'donkey-mark-word)
(keymap-set donkey-normal-mode-map "m W" #'donkey-mark-symbol)
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
`donkey-excluded-modes' buffer.  Insert state is the truthful answer
there -- keys really do pass through -- but it is a misleading one: it
suggests \\[donkey--exit-insert] would get you to Normal state, and in these buffers
nothing does.  Normal state is refused permanently, by
`donkey--ensure-default-state' on entry and
`donkey--handle-non-editing-buffer' for anything that gets in another
way, so a reader pressing \\[donkey--exit-insert] and watching the lighter not change had
no way to tell a deliberate refusal from a broken key.

Computed on redisplay rather than stored, because a buffer can change
major mode underneath the state -- `M-x shell-mode' in an ordinary
buffer makes it excluded without any DONKEY transition firing.
`donkey--excluded-mode-p' costs about a microsecond, which is nothing
beside the redisplay it is part of."
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
  :type '(choice (const box) (const bar) (const hollow)
                 (cons symbol integer)
                 (const :tag "Use Global Default" nil))
  :group 'donkey)

(defcustom donkey-cursor-insert '(bar . 2)
  "Cursor shape when DONKEY Insert state is active.

Set to nil to fall back to global `cursor-type'."
  :type '(choice (const box) (const bar) (const hollow)
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
when DECSCUSR sequences are sent."
  :type '(repeat string)
  :group 'donkey)

(defun donkey--cursor-type-to-decscusr (type)
  "Convert cursor TYPE to DECSCUSR escape sequence.

Maps all supported shapes including hollow (blinking)."
  (pcase type
    ('box         "\e[2 q")    ; Steady block
    ('hollow      "\e[0 q")    ; Blinking block (default)
    ('bar         "\e[6 q")    ; Steady bar
    (`(bar . ,_)  "\e[6 q")    ; Steady bar, ignore width
    (`(hbar . ,_) "\e[4 q")    ; Steady underline
    (_ "\e[0 q")))             ; Fallback to default

(defun donkey--terminal-supports-decscusr-p ()
  "Return non-nil if the current terminal likely supports DECSCUSR.

Returns nil for graphical frames and for terminals whose type
matches a prefix in `donkey-decscusr-denied-terminals'.
Falls back to the `TERM' environment variable when `tty-type'
returns nil, and performs a conservative guess based on known
capable terminal names."
  (and (not (display-graphic-p))
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

Sending happens via `donkey--send-cursor-sequence'.  Caching it here
lets `donkey--apply-cursor-setting' skip redundant terminal I/O when
called again with an unchanged value -- notably, entering Normal or Insert
state triggers this twice per transition, since each of
`donkey-normal-mode' and `donkey-insert-mode' toggles the other off as
part of its own body, running both modes' hooks (both of which include
`donkey--update-cursor') for what is conceptually one transition.

Keyed by terminal, not per-buffer: a terminal's actual cursor shape is
a single shared, global resource, so caching this per-buffer would let
a buffer's own cache report the shape as already-current right after a
DIFFERENT buffer's hook most recently changed what the terminal is
actually showing -- e.g. switching between a Normal-state window and
an Insert-state window via `other-window' applies the correct shape
the first time each buffer is visited, but a per-buffer cache would
then wrongly skip resending on returning to a previously-visited
buffer, since that buffer's own cache still (correctly, for itself)
remembers its own last self-applied value.")

(defun donkey--apply-cursor-setting (setting)
  "Apply SETTING, falling back to global default if SETTING is nil.

In terminal mode, also sends DECSCUSR escape sequence for visual
cursor change -- but only when SETTING's effective value actually
changed since the last call for this terminal, to avoid redundant
terminal I/O (see `donkey--last-applied-cursor-settings')."
  (let ((effective (cond
                    (setting setting)
                    ((local-variable-p 'cursor-type) cursor-type)
                    (t (default-value 'cursor-type))))
        (terminal (frame-terminal)))
    (if setting
        (setq-local cursor-type setting)
      (kill-local-variable 'cursor-type))
    (unless (equal effective (gethash terminal donkey--last-applied-cursor-settings))
      (puthash terminal effective donkey--last-applied-cursor-settings)
      (donkey--send-cursor-sequence effective))))

(defun donkey--update-cursor (&optional passive)
  "Update cursor based on current DONKEY state.

With PASSIVE non-nil, does nothing when neither `donkey-normal-mode'
nor `donkey-insert-mode' is active in the current buffer, rather than
resetting `cursor-type' to the default.  Used when called from the
global `post-command-hook' (see `donkey--update-cursor-passive' and
`donkey-mode') to resync the terminal cursor on window/buffer
switches: that hook runs for EVERY buffer that becomes current, not
just ones DONKEY manages, and a buffer that never ran any major-mode
setup (so `donkey--ensure-default-state' never applied to it) would
have `cursor-type' silently reset even though some unrelated package
may have set it there on purpose.  Without PASSIVE -- called from
`donkey-normal-mode-hook'/`donkey-insert-mode-hook', which only ever
fire for buffers DONKEY itself toggled -- the reset is exactly what a
Normal/Insert -> disabled transition needs."
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

Each element is normal, insert, or nil.  A stack rather than a
single slot so recursive minibuffer activations (nested reads,
e.g. via `enable-recursive-minibuffers') each restore their own
saved state on exit instead of clobbering one another.  Not
buffer-local because we need to read it after switching buffers.")

(defun donkey--minibuffer-current-state ()
  "Return the current DONKEY state as a symbol."
  (cond
   ((bound-and-true-p donkey-normal-mode) 'normal)
   ((bound-and-true-p donkey-insert-mode) 'insert)
   (t nil)))

(defun donkey--minibuffer-setup ()
  "Save the originating buffer's DONKEY state; never leave Normal state on.

The minibuffer is never actively put into Insert state here -- it
never runs `donkey--ensure-default-state' the way an ordinary buffer's
major-mode setup does, so `donkey-normal-mode' is essentially never
already on in a fresh minibuffer.  This is a defensive check for the
rare case where it somehow is, so the minibuffer instead falls through
to plain Emacs passthrough by default, same as any other buffer
Donkey never activated in."
  ;; Capture state from the buffer that initiated the minibuffer
  (let ((orig-state
         (with-current-buffer
             (window-buffer (minibuffer-selected-window))
           (donkey--minibuffer-current-state))))
    (push orig-state donkey--minibuffer-pre-state-stack))
  ;; Guard against donkey-normal-mode somehow already being on here
  (when (bound-and-true-p donkey-normal-mode)
    (donkey-normal-mode -1)))

(defun donkey--minibuffer-exit ()
  "Restore the originating buffer's saved DONKEY state.

Always pops `donkey--minibuffer-pre-state-stack' to keep it balanced
with `donkey--minibuffer-setup', but only re-enters Normal/Insert
state when `donkey-mode' is still globally on.  Without this check,
disabling `donkey-mode' while a minibuffer session is in progress
\(e.g. via a keybinding, from a recursive minibuffer) would have this
hook resurrect Normal or Insert state in the originating buffer on
exit, the same way a stray `C-g' through `donkey-setup-smartparens''
keymaps could before `donkey--exit-insert' gained its own
`donkey-mode' guard."
  (let ((saved-state (pop donkey--minibuffer-pre-state-stack)))
    (when (bound-and-true-p donkey-mode)
      (with-current-buffer
          (window-buffer (minibuffer-selected-window))
        (pcase saved-state
          ('normal (donkey-enter-normal))
          ('insert (donkey-enter-insert)))))))

(add-hook 'minibuffer-setup-hook #'donkey--minibuffer-setup)
(add-hook 'minibuffer-exit-hook #'donkey--minibuffer-exit)

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
    ;; Strategy 2: Buffer-wide scan for transient faces
    (dolist (ov (overlays-in beg end))
      (when (overlay-start ov)
        (let ((face (overlay-get ov 'face)))
          (when (or (overlay-get ov 'donkey-cleanup)
                    (and face
                         (cond
                          ((symbolp face)
                           (memq face transient-faces))
                          ((consp face)
                           (cl-some (lambda (f) (memq f transient-faces)) face)))))
            (delete-overlay ov)
            (setq cleared (1+ cleared))))))
    ;; Strategy 3: Remove overlays carrying smartparens keymap properties.
    ;;
    ;; For overlays Smartparens is actively tracking in
    ;; `sp-pair-overlay-list', go through its own `sp--remove-overlay'
    ;; instead of a raw `delete-overlay': deleting a still-tracked pair
    ;; overlay out from under Smartparens leaves a stale, deleted-overlay
    ;; reference sitting in that list.  `overlay-start'/`overlay-end' on
    ;; a deleted overlay return nil, and the very next command then
    ;; crashes `sp--pair-overlay-post-command-handler' (still registered
    ;; as a local `post-command-hook', since only `sp--remove-overlay'
    ;; also unregisters it) with
    ;; (wrong-type-argument number-or-marker-p nil).
    (dolist (ov (overlays-in beg end))
      (when (overlay-start ov)
        (let ((km (overlay-get ov 'keymap)))
          (when (and km
                     (or (and (boundp 'sp-pair-overlay-keymap)
                              (eq km sp-pair-overlay-keymap))
                         (and (boundp 'sp-overlay-keymap)
                              (eq km sp-overlay-keymap))))
            (if (and (boundp 'sp-pair-overlay-list)
                     (fboundp 'sp--remove-overlay)
                     (memq ov sp-pair-overlay-list))
                (sp--remove-overlay ov)
              (delete-overlay ov))
            (setq cleared (1+ cleared))))))
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

(defun donkey--exit-insert ()
  "Exit insert state and enter normal mode.

Removes active mark, enters normal mode, and schedules deferred
overlay cleanup.  In the minibuffer, in a `donkey-excluded-modes'
buffer, or when `donkey-insert-mode' is not actually active in the
current buffer, delegates to `keyboard-quit' instead.  The
`donkey-insert-mode' check matters because `donkey-setup-smartparens'
binds this command directly into Smartparens' own keymaps
\(`smartparens-mode-map' and its overlay keymaps), which are
independent of DONKEY's lifecycle: disabling `donkey-mode' turns off
`donkey-insert-mode' in every buffer but does not undo that binding,
so without this guard a stray `C-g' reaching this function through it
afterward would still turn `donkey-normal-mode' back on.  Checking
`donkey-insert-mode' rather than the global `donkey-mode' matters too:
`donkey-insert-mode'/`donkey-normal-mode' are usable standalone
without ever enabling `donkey-mode', and a `donkey-mode' check would
make `C-g' always fall through to `keyboard-quit' for that usage,
never actually transitioning to Normal state.

For the minibuffer/excluded-mode case: those buffers stay in Insert
state permanently, so forcing a Normal-state transition here would
just get reverted immediately, silently swallowing `C-g' and
preventing it from reaching the underlying mode (e.g. interrupting a
subprocess or aborting a recursive edit)."
  (interactive)
  (if (or (not (bound-and-true-p donkey-insert-mode))
          (minibufferp)
          (donkey--excluded-mode-p))
      (keyboard-quit)
    ;; Guarded because it runs `deactivate-mark-hook', which is not
    ;; DONKEY's: anything the user or a package put there could signal,
    ;; and this is the only step BEFORE the state transition.  Left
    ;; unguarded, a stranger's broken hook would strand the user in
    ;; Insert state on the very keypress meant to get them out.
    (condition-case err
        (deactivate-mark)
      (error (message "DONKEY: deactivate-mark failed: %s"
                      (error-message-string err))))
    ;; From here the promise is kept whatever happens.  `define-minor-mode'
    ;; sets the variable before running the body and hooks, so even a
    ;; `donkey-normal-mode-hook' that errors leaves Normal state on.
    (donkey-enter-normal)
    (unless (bound-and-true-p donkey-normal-mode)
      (donkey-normal-mode 1))
    (donkey--abort-keyboard-macro-definition)
    (donkey--schedule-overlay-cleanup)))

(defun donkey--abort-keyboard-macro-definition ()
  "Stop a keyboard macro that is being recorded, the way `keyboard-quit' does.

The one errand of Emacs\\=' own `C-g' that leaving Insert state used to
skip.  Pressing `C-g' part way through `\\[kmacro-start-macro]' looked
like it had abandoned the recording -- Normal state, box cursor, nothing
to suggest otherwise -- while every later keystroke was still being
recorded.  The only signal was `\\[kmacro-start-macro]' refusing later
with \"Already defining keyboard macro\".

Recoverable rather than destructive: `C-g' from NORMAL state runs the
real `keyboard-quit', which does stop it, so the escape always worked in
two presses.  This makes the first press mean what it looks like.

Runs AFTER the state transition and cannot prevent it.  Any condition is
caught and reported: this is reached from `pre-command-hook' via
`donkey--intercept-quit-in-insert', where a signal costs the user the
whole interception mechanism for the session, and no macro is worth
that.

Deliberately not the rest of `keyboard-quit'.  Insert state\\='s `C-g' is
an exit key, not a general abort, and folding in every stock side effect
would make it less predictable rather than more.  A macro left recording
is the one omission that leaves the editor in a state the user believes
it is not in."
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

Skips excluded-mode buffers entirely: there, `donkey--exit-insert'
calls `keyboard-quit', which signals a `quit' condition.  Emacs's
command loop treats ANY signal from a `pre-command-hook' function as a
malfunction, reports \"Error in pre-command-hook\", and permanently
removes the offending function from the hook — silently and
permanently disabling this whole interception mechanism, in every
buffer, after the very first `C-g' in an excluded-mode buffer.
Skipping here lets the raw key fall through to the direct `C-g'
binding instead, so `keyboard-quit' runs as an ordinary command
instead of from inside a hook, where signalling `quit' is safe.

The excluded-mode skip only closed the one path that was found.  Every
OTHER way `donkey--exit-insert' can signal removes this function just
as permanently, and there are several: the function `deactivate-mark'
runs `deactivate-mark-hook', entering Normal state runs
`donkey-normal-mode-hook' -- which is a user-facing hook anyone may
have added a cursor, theme or modeline function to -- and the overlay
cleanup cancels and schedules timers.  One error in any of them and
this whole mechanism is gone for the session, in every buffer, leaving
only a line in *Messages* to say so.

Confirmed by driving a real `C-g' through `execute-kbd-macro' with a
`donkey-normal-mode-hook' that errors: the interception was on the hook
before the keypress and gone after it.

So the call is wrapped.  Any condition is caught and reported rather
than allowed to propagate, because losing this function is worse than
whatever raised it: `C-g' returning to Normal state is the one promise
DONKEY makes unconditionally, and this hook is what keeps it when
something else has taken the key -- nested smartparens overlays, a
package binding `C-g' in its own map, a terminal where the direct
binding is not reached.  `donkey--exit-insert' guards its own one step
that runs before the state transition, so a failure partway through
still leaves the user in Normal state, which is what they asked for."
  (when (and (bound-and-true-p donkey-insert-mode)
             (not donkey--just-exited-from-insert)
             (not (minibufferp))
             (not (donkey--excluded-mode-p))
             (or (equal (this-single-command-keys) [7])
                 (eq this-command 'sp-cancel)))
    (setq this-command 'ignore
          donkey--just-exited-from-insert t)
    ;; LOCAL (4th arg) so the reset only fires once THIS buffer is
    ;; current again for its next command, not whichever buffer
    ;; happens to run the next command globally.
    (add-hook 'pre-command-hook #'donkey--reset-exit-guard -100 t)
    (condition-case err
        (donkey--exit-insert)
      ;; Reported, not swallowed: a real bug in the exit path should
      ;; still be visible, it just must not cost the user their escape
      ;; key for the rest of the session.
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

;; Pre-command hook backup for packages that override C-g
(add-hook 'pre-command-hook #'donkey--intercept-quit-in-insert)

;;; ---------------------------------------------------------------------------
;;; Input Method Management
;;; ---------------------------------------------------------------------------

(defvar-local donkey--saved-input-method nil
  "Buffer-local saved input method name for restoration on Insert entry.")

(defun donkey--on-normal-entry ()
  "Deactivate any active input method when entering Normal state.

Saved in `donkey--saved-input-method' for `donkey--on-insert-entry' to
restore later.  Input methods (e.g. for CJK or accented-character
entry) are for text entry; without this, Normal state's own
keybindings (h/j/k/l and the rest) would be run through whatever
conversion the active input method applies to raw keystrokes instead,
breaking navigation entirely for anyone using one."
  (when donkey-normal-mode
    (when current-input-method
      (setq donkey--saved-input-method current-input-method)
      (deactivate-input-method))))

(defun donkey--on-insert-entry ()
  "Reactivate on Insert entry the input method `donkey--on-normal-entry' saved.

Only acts when no input method is ALREADY active -- e.g. the user
manually turned a different one on while still in Normal state, via
`donkey--on-input-method-activate' below -- so this never clobbers
whichever one is genuinely current by the time Insert state resumes."
  (when donkey-insert-mode
    (when (and donkey--saved-input-method
               (not current-input-method))
      (activate-input-method donkey--saved-input-method))))

(defun donkey--on-input-method-activate ()
  "Immediately undo an input method activated while in Normal state.

Saves it the same way `donkey--on-normal-entry' does.  Registered on
the global `input-method-activate-hook' rather than a
DONKEY mode-hook, since this needs to catch activation through ANY
means -- `M-x set-input-method', `C-\\', a toggle command from some
other package -- not just the Normal-state entry transition itself.
Binds `input-method-activate-hook' to nil around the
`deactivate-input-method' call as a defensive measure, in case
deactivating one method ever indirectly triggers activating another
\(e.g. a language-specific default\), which would otherwise re-enter
this same function from within itself."
  (when (bound-and-true-p donkey-normal-mode)
    (when current-input-method
      (setq donkey--saved-input-method current-input-method)
      (let (input-method-activate-hook)
        (deactivate-input-method)))))

(defun donkey--on-input-method-deactivate ()
  "Forget the saved input method if deactivated while still in Insert state.

Only `donkey--on-normal-entry' deactivates the input method as part of
saving it for later restoration, and by the time its
`donkey-normal-mode-hook' runs, `donkey-insert-mode' has already been
turned off — so this only fires for deactivations that happen some
other way (e.g. the user manually toggles the input method off) while
Insert state is still active.  That is a deliberate choice, and
without clearing the saved value here, the next Normal-to-Insert
cycle would silently reactivate the very input method the user just
turned off."
  (when (bound-and-true-p donkey-insert-mode)
    (setq donkey--saved-input-method nil)))

(defun donkey-disable-input-method ()
  "Turn off the input method for good, clearing Donkey's saved state too.

Plain `deactivate-input-method' is not enough while in Normal state:
Donkey already deactivated the live input method on entry to Normal
and stashed it in `donkey--saved-input-method' for restoration on the
next Insert-state entry, so `current-input-method' is already nil and
`deactivate-input-method' -- guarded by `(when current-input-method
...)' -- is a silent no-op.  Since nothing was actually deactivated,
`input-method-deactivate-hook' never runs, so
`donkey--on-input-method-deactivate' never clears the saved value,
and the next Insert-state entry reactivates the very input method the
user just tried to turn off.  This command clears both unconditionally
regardless of which Donkey state is active when it's called."
  (interactive)
  (setq donkey--saved-input-method nil)
  (when current-input-method
    (deactivate-input-method)))

(add-hook 'donkey-normal-mode-hook #'donkey--on-normal-entry)
(add-hook 'donkey-insert-mode-hook #'donkey--on-insert-entry)
(add-hook 'input-method-activate-hook #'donkey--on-input-method-activate)
(add-hook 'input-method-deactivate-hook #'donkey--on-input-method-deactivate)

;;; ---------------------------------------------------------------------------
;;; Enhanced Mode Activation Logic
;;; ---------------------------------------------------------------------------

(defun donkey--ensure-default-state ()
  "Enable DONKEY Normal state unless the current major mode is excluded.

For excluded modes, enable DONKEY Insert state (passthrough) instead.
Returns non-nil if DONKEY was enabled."
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
        t)))))

;;; ---------------------------------------------------------------------------
;;; Mode Indicator
;;; ---------------------------------------------------------------------------

(defun donkey-indicator ()
  "Return state indicator string for modeline.

Returns ' DONKEY[N]' for Normal, ' DONKEY[I]' for Insert, ' DONKEY[E]'
for Insert in a `donkey-excluded-modes' buffer, and the empty string
otherwise.  Useful if you build your own mode-line and want to include
the DONKEY state.

The E case is still Insert state; it is reported separately because
Normal state cannot be reached from those buffers at all, and a lighter
that says Insert invites a reader to press \\[donkey--exit-insert] and conclude the key
is broken when nothing happens.  Kept in step with
`donkey-insert-mode's own lighter, which makes the same distinction."
  (cond
   ((bound-and-true-p donkey-normal-mode) " DONKEY[N]")
   ((bound-and-true-p donkey-insert-mode) (donkey--insert-state-lighter))
   (t "")))

;;; ---------------------------------------------------------------------------
;;; Global Mode Toggle
;;; ---------------------------------------------------------------------------

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
        (add-hook 'after-change-major-mode-hook #'donkey--ensure-default-state)
        (add-hook 'post-command-hook #'donkey--track-position)
        (add-hook 'post-command-hook #'donkey--check-post-command-non-editing)
        (add-hook 'post-command-hook #'donkey--update-cursor-passive)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (donkey--ensure-default-state))))
    (remove-hook 'after-change-major-mode-hook #'donkey--ensure-default-state)
    (remove-hook 'post-command-hook #'donkey--track-position)
    (remove-hook 'post-command-hook #'donkey--check-post-command-non-editing)
    (remove-hook 'post-command-hook #'donkey--update-cursor-passive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p donkey-normal-mode)
          (donkey-normal-mode -1))
        (when (bound-and-true-p donkey-insert-mode)
          (donkey-insert-mode -1))
        ;; Banked lines are Donkey state drawn on the buffer, and this
        ;; mode promises to clear all of it.  Left behind, the
        ;; highlights would be permanent: the only command that removes
        ;; them is `donkey-clear-banked-selection', reachable solely
        ;; through a Normal-state key that no longer exists once the
        ;; mode is off.
        (donkey-clear-banked-selection)
        (donkey--apply-cursor-setting nil)))))

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------

(provide 'donkey)

;;; donkey.el ends here

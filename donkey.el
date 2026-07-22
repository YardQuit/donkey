;;; donkey.el --- Opinionated Modal Editing -*- lexical-binding: t -*-

;; Copyright (C) 2026 Michael Jones
;; Author: Michael Jones <yardquit@pm.me>
;; Maintainer: Michael Jones
;; Assisted-by: Lumo 2.0 Max, Claude [Claude Code]
;; URL: https://github.com/yardquit/donkey
;; Version: 1.0.1
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

(defun donkey--excluded-mode-p ()
  "Return non-nil if the current major mode is in `donkey-excluded-modes'.

Checks both exact membership and derivation, so modes like
`shell-mode' (derived from `comint-mode') are caught consistently
everywhere this predicate is used."
  (or (memq major-mode donkey-excluded-modes)
      (apply #'derived-mode-p donkey-excluded-modes)))

(defun donkey--handle-non-editing-buffer ()
  "Enter insert mode in excluded major modes when `donkey-normal-mode' activates."
  (when (donkey--excluded-mode-p)
    (when (bound-and-true-p donkey-normal-mode)
      (donkey-enter-insert))))

(add-hook 'donkey-normal-mode-hook #'donkey--handle-non-editing-buffer)

(defun donkey--check-post-command-non-editing ()
  "Check after commands if we're in an excluded mode."
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
          (message "*org-scratch* buffer already exist, switching."))
      (donkey-create-org-scratch)
      (message "*org-scratch* buffer doesn't exist, creating."))))

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
         (sorted-raw (sort raw (lambda (a b) (string< (car a) (car b))))))
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
                 (group    (if (string-match "\\(.+?\\) " full-key)
                               (match-string 1 full-key)
                             "single"))
                 (new-block-p (and (> lines-added 0)
                                   (not (equal prev-group group)))))
            ;; Separator + header on group transition
            (when new-block-p
              (insert "\n")
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
;;; Line and Buffer Navigation Commands
;;; ---------------------------------------------------------------------------

(defcustom donkey-position-ring-max 10
  "Number of position markers retained in the ring."
  :type 'integer
  :group 'donkey)

(defvar-local donkey--position-ring nil
  "List of markers recording previous cursor positions, most recent first.")

(defvar-local donkey--position-index 0
  "Current rotation offset into `donkey--position-ring'.

0 = most recent entry.  Reset to 0 whenever a new position is
recorded.")

(defvar-local donkey--last-tracked-state nil
  "Cons cell (BUFFER . POINT) captured after the previous command.")

(defun donkey--track-position ()
  "Record previous cursor position when point changes.

Runs on `post-command-hook'.  Independent of the mark ring and
region."
  (unless (minibufferp)
    (let ((now-pt (point)))
      (when (and donkey--last-tracked-state
                 (/= (cdr donkey--last-tracked-state) now-pt))
        (let ((m (make-marker)))
          (set-marker m (cdr donkey--last-tracked-state))
          (push m donkey--position-ring)
          (when (> (length donkey--position-ring) donkey-position-ring-max)
            (set-marker (car (last donkey--position-ring)) nil)
            (nbutlast donkey--position-ring)))
        (setq donkey--position-index 0))
      (setq donkey--last-tracked-state (cons (current-buffer) now-pt)))))

(defun donkey-jump-back ()
  "Rotate to the next stored position in the ring and jump there.

Press repeatedly to cycle through the last `donkey-position-ring-max'
recorded positions in this buffer."
  (interactive)
  (if (null donkey--position-ring)
      (user-error "No positions recorded yet")
    (let ((ring-len (length donkey--position-ring)))
      (setq donkey--position-index (1+ donkey--position-index))
      (when (>= donkey--position-index ring-len)
        (setq donkey--position-index 0))
      (goto-char (nth donkey--position-index donkey--position-ring))
      (setq donkey--last-tracked-state (cons (current-buffer) (point)))
      (message "Position %d/%d"
               (1+ donkey--position-index) ring-len))))

(defun donkey-goto-line ()
  "Go to line number."
  (interactive)
  (let ((target-line (read-number "Line: ")))
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

(defun donkey-insert-here ()
  "Insert at current position - enters INSERT state."
  (interactive)
  (when (and mark-active (use-region-p))
    (deactivate-mark))
  (donkey-enter-insert))

(defun donkey-insert-after ()
  "Insert after current char - enters INSERT state."
  (interactive)
  (deactivate-mark)
  (condition-case _err
      (forward-char 1)
    (end-of-buffer nil))
  (donkey-enter-insert))

(defun donkey-insert-beginning-of-line ()
  "Insert at beginning of line - enters INSERT state."
  (interactive)
  (when (and mark-active (use-region-p))
    (deactivate-mark))
  (beginning-of-line)
  (donkey-enter-insert))

(defun donkey-insert-end-of-line ()
  "Insert at end of line - enters INSERT state."
  (interactive)
  (when (and mark-active (use-region-p))
    (deactivate-mark))
  (move-end-of-line 1)
  (donkey-enter-insert))

(defun donkey-open-below ()
  "Open a new line below and enter INSERT state."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (move-end-of-line 1)
  (newline-and-indent)
  (donkey-enter-insert))

(defun donkey-open-above ()
  "Open a new line above and enter INSERT state."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  (donkey-enter-insert))

(defun donkey-change ()
  "Change marked char or region - enters INSERT state."
  (interactive)
  (if (use-region-p)
      (progn
        (if (bound-and-true-p rectangle-mark-mode)
            (call-interactively #'string-rectangle)
          (delete-region (mark) (point)))
        (donkey-enter-insert))
    (delete-char 1)
    (donkey-enter-insert)))

(defcustom donkey-wrap-delimiters '(?\( ?\[ ?\{ ?\" ?\' ?\`)
  "Characters that trigger `donkey-wrap-region' in Normal state.

Bound in `donkey-normal-mode-map'; only takes effect while a
region is active (see `donkey-wrap-region').  Changing this after
`donkey.el' has loaded has no effect on already-bound keys -- set
it before loading, or re-run the `dolist' near
`donkey-normal-mode-map's definition."
  :type '(repeat character)
  :group 'donkey)

(defun donkey-wrap-region ()
  "Insert the pressed delimiter into the active region without deselecting.

Bound to each of `donkey-wrap-delimiters' in Normal state.  With
no active region, falls through to `undefined', same as any other
suppressed key.  With an active region, enters Insert state
without deactivating the mark, inserts the pressed character via
`self-insert-command' -- letting packages that hook it, such as
Smartparens' region-wrap, act on the still-active region -- then
returns to Normal state."
  (interactive)
  (if (not (use-region-p))
      (call-interactively #'undefined)
    (donkey-insert-mode 1)
    (self-insert-command 1)
    (donkey--exit-insert)))

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

;;; ---------------------------------------------------------------------------
;;; Enter DWIM
;;; ---------------------------------------------------------------------------

(defvar donkey--enter-rules nil
  "List of (ELEMENT-TYPE PROPERTY COMMAND1 COMMAND2 ...) for ENTER DWIM dispatch.")

(defvar-local donkey--saved-ret-binding nil
  "Saved RET binding from buffer's local map when entering DONKEY Normal.")

(defvar donkey-editing-modes
  '(prog-mode text-mode org-mode fundamental-mode conf-mode markdown-mode gfm-mode)
  "Major modes where Enter should be blocked to prevent accidental line breaks.")

(defun donkey--editing-mode-p ()
  "Return non-nil if current major mode is in `donkey-editing-modes'.

Checks both exact membership and derivation via `derived-mode-p', so
concrete modes like `python-mode' or `emacs-lisp-mode' (derived from
`prog-mode') are recognized, not just the literal parent-mode symbols
themselves — which are essentially never a real buffer's major mode."
  (or (memq major-mode donkey-editing-modes)
      (apply #'derived-mode-p donkey-editing-modes)))

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
         (result nil))
    ;; Context FIRST (inline elements like links within tables/headlines)
    (dolist (rule donkey--enter-rules)
      (when (null result)
        (let ((rule-type (nth 0 rule))
              (rule-cmds (nthcdr 2 rule)))
          (when (and ctx
                     (eq (car ctx) rule-type)
                     (null (nth 1 rule)))
            (dolist (candidate rule-cmds)
              (when (and (null result)
                         (fboundp candidate)
                         (commandp candidate))
                (setq result candidate)))))))
    ;; Parent, then ancestors — ALL rules checked per element level
    (dolist (elem (cons parent ancestors))
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
                (dolist (candidate rule-cmds)
                  (when (and (null result)
                             (fboundp candidate)
                             (commandp candidate))
                    (setq result candidate)))))))))
    result))

(defun donkey--execute-handler (cmd)
  "Execute CMD if it exists and is callable."
  (when (and cmd (fboundp cmd) (commandp cmd))
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

(when donkey-default-enter-rules-enabled
  (donkey-add-enter-rule item :checkbox org-toggle-checkbox)
  (donkey-add-enter-rule headline :todo-type donkey-org-todo)
  (donkey-add-enter-rule link nil org-open-at-point markdown-follow-thing-at-point browse-url-at-point))

(defun donkey-enter-dwim ()
  "Smart Return handler for DONKEY Normal State."
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
            (if has-region
                (let* ((cur-line-in-edit (line-number-at-pos))
                       (diff (- cur-line-in-edit cur-line))
                       (edit-beg-line (+ reg-beg-line diff))
                       (edit-end-line (+ reg-end-line diff)))
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
            (org-edit-src-exit)
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
      (let ((local-map (make-sparse-keymap)))
        (set-keymap-parent local-map
                           (if (boundp 'help-map)
                               help-map
                             special-mode-map))
        (keymap-set local-map "q" #'quit-window)
        (use-local-map local-map))
      (special-mode))))

;;; ---------------------------------------------------------------------------
;;; Yank and Delete Commands
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
  "Delete active region if one exists.

Uses the function `kill-active-region' if available (Emacs 29+), falling back to
the function `delete-active-region'.  Handles both cases gracefully."
  (when (use-region-p)
    (if (fboundp 'kill-active-region)
        (kill-active-region)
      (delete-active-region))))

(defun donkey-yank ()
  "Yank clipboard content, replacing the active region if present.

Falls back to the kill ring when the system clipboard is
inaccessible.  This provides consistent behavior across GUI and
terminal Emacs on Linux (X11/Wayland), macOS, and Windows."
  (interactive)
  (donkey--delete-active-region-safe)
  (donkey--clipboard-yank))

(defun donkey-yank-pop ()
  "Replace the last yanked text with the next `kill-ring' entry.

Removes the active region first if one is present."
  (interactive)
  (donkey--delete-active-region-safe)
  (yank-pop))

(defun donkey-delete ()
  "Delete character or region."
  (interactive)
  (if (use-region-p)
      (progn
        (if (bound-and-true-p rectangle-mark-mode)
            (call-interactively #'kill-rectangle)
          (kill-region (mark) (point))))
    (delete-char 1)))

;;; ---------------------------------------------------------------------------
;;; Mark and Text Object Selection Commands
;;; ---------------------------------------------------------------------------

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

(defun donkey-visual-line-toggle ()
  "Start/cancel visual line selection."
  (interactive)
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (message "Visual line: cancelled"))
    (setq donkey-visual-anchor (line-beginning-position))
    (set-mark (line-beginning-position))
    (end-of-line)
    (activate-mark)
    (message "Visual line: j/k to extend, V to cancel")))

(defun donkey-visual-next-line ()
  "Move down.  Extend visual selection if active."
  (interactive)
  (if (and (region-active-p) donkey-visual-anchor)
      (progn
        (forward-line 1)
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
    (forward-line 1)))

(defun donkey-visual-previous-line ()
  "Move up.  Extend visual selection if active."
  (interactive)
  (if (and (region-active-p) donkey-visual-anchor)
      (progn
        (forward-line -1)
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
    (forward-line -1)))

(defun donkey-rectangle-mark-mode ()
  "Toggle rectangle mark mode."
  (interactive)
  (if (bound-and-true-p rectangle-mark-mode)
      (progn
        (rectangle-mark-mode -1)
        (deactivate-mark))
    (rectangle-mark-mode 1)
    (right-char 1)))

(defun donkey-mark-inner ()
  "Mark text INSIDE CHAR pairs (excluding delimiters)."
  (interactive)
  (let* ((default-char (char-after))
         (supported-openers '(?: ?/ ?+ ?_ ?$ ?= ?* ?~ ?\| ?\\ ?\{ ?\[ ?\( ?\< ?\" ?\' ?\`))
         (on-opener (and default-char (memq default-char supported-openers)))
         (open-char (if on-opener
                        default-char
                      (read-char "Char (:/+_$=*~|\\{[<>'\"`): ")))
         (close-char nil)
         (start-pos nil)
         (end-pos nil))
    (setq close-char
          (cond
           ((= open-char ?\{) ?\})
           ((= open-char ?\[) ?\])
           ((= open-char ?\() ?\))
           ((= open-char ?\<) ?>)
           ((= open-char ?\") ?\")
           ((= open-char ?\') ?\')
           ((= open-char ?`) ?`)
           ((= open-char ?=) ?=)
           ((= open-char ?*) ?*)
           ((= open-char ?~) ?~)
           ((= open-char ?\|) ?\|)
           ((= open-char ?\\) ?\\)
           ((= open-char ?/) ?/)
           ((= open-char ?:) ?:)
           ((= open-char ?+) ?+)
           ((= open-char ?_) ?_)
           ((= open-char ?$) ?$)
           (t
            (error "Unsupported delimiter '%c'" open-char))))
    (if on-opener
        (setq start-pos (point))
      (if (and (char-after) (= (char-after) open-char))
          (setq start-pos (point))
        (condition-case nil
            (setq start-pos (search-backward (string open-char) nil nil))
          (search-failed
           (error "No '%c' found near cursor" open-char)))))
    (goto-char (1+ start-pos))
    (condition-case nil
        (setq end-pos (search-forward (string close-char) nil nil))
      (search-failed
       (error "No matching '%c' found after cursor" close-char)))
    (push-mark (1+ start-pos))
    (goto-char (1- end-pos))
    (activate-mark)
    (when (>= (region-beginning) (region-end))
      (deactivate-mark)
      (error "Empty selection between %c and %c" open-char close-char))
    (message "Selected content for '%c'" open-char)))

(defun donkey-mark-outer ()
  "Mark text INCLUDING CHAR pairs (delimiters included)."
  (interactive)
  (let* ((default-char (char-after))
         (supported-openers '(?: ?/ ?+ ?_ ?$ ?= ?* ?~ ?\| ?\\ ?\{ ?\[ ?\( ?\< ?\" ?\' ?\`))
         (on-opener (and default-char (memq default-char supported-openers)))
         (open-char (if on-opener
                        default-char
                      (read-char "Char (:/+_$=*~|\\{[<>'\"`): ")))
         (close-char nil)
         (start-pos nil)
         (end-pos nil))
    (setq close-char
          (cond
           ((= open-char ?\{) ?\})
           ((= open-char ?\[) ?\])
           ((= open-char ?\() ?\))
           ((= open-char ?\<) ?>)
           ((= open-char ?\") ?\")
           ((= open-char ?\') ?\')
           ((= open-char ?`) ?`)
           ((= open-char ?=) ?=)
           ((= open-char ?*) ?*)
           ((= open-char ?~) ?~)
           ((= open-char ?\|) ?\|)
           ((= open-char ?\\) ?\\)
           ((= open-char ?/) ?/)
           ((= open-char ?:) ?:)
           ((= open-char ?+) ?+)
           ((= open-char ?_) ?_)
           ((= open-char ?$) ?$)
           (t
            (error "Unsupported delimiter '%c'.  Use: { [ ( < ' \" ` | \\" open-char))))
    (if on-opener
        (setq start-pos (point))
      (if (and (char-after) (= (char-after) open-char))
          (setq start-pos (point))
        (condition-case nil
            (setq start-pos (search-backward (string open-char) nil nil))
          (search-failed
           (error "No '%c' found near cursor" open-char)))))
    (goto-char (1+ start-pos))
    (condition-case nil
        (setq end-pos (search-forward (string close-char) nil nil))
      (search-failed
       (error "No matching '%c' found after cursor" close-char)))
    (push-mark start-pos)
    (goto-char end-pos)
    (activate-mark)
    (when (>= (region-beginning) (region-end))
      (deactivate-mark)
      (error "Empty selection between %c and %c" open-char close-char))
    (message "Selected OUTER content including '%c'" open-char)))

(defun donkey-mark-sexp-inner ()
  "Mark content inside the balanced expression at point.

Uses the syntax table to identify delimiters (parentheses,
brackets, braces).  If point is on an opening or closing
delimiter, marks content within that pair.  If point is inside
a pair, finds the enclosing delimiters and marks everything
within, excluding the delimiters themselves."
  (interactive)
  (unless (looking-at "\\s(")
    (condition-case nil
        (backward-up-list)
      (scan-error
       (user-error "Not inside a balanced expression"))))
  (let ((start (1+ (point))) end)
    (condition-case nil
        (setq end (1- (progn (forward-list 1) (point))))
      (scan-error
       (user-error "Unbalanced expression")))
    (when (>= start end)
      (user-error "Empty expression"))
    (push-mark start t)
    (goto-char end)
    (activate-mark)
    (message "Marked inner expression")))

(defun donkey-mark-sexp-outer ()
  "Mark the balanced expression at point, including delimiters.

Uses the syntax table to identify delimiters (parentheses,
brackets, braces).  If point is on a delimiter, marks that
pair.  If point is inside a pair, finds the enclosing pair
and marks it including delimiters."
  (interactive)
  (unless (looking-at "\\s(")
    (condition-case nil
        (backward-up-list)
      (scan-error
       (user-error "Not inside a balanced expression"))))
  (let ((start (point)) end)
    (condition-case nil
        (setq end (progn (forward-list 1) (point)))
      (scan-error
       (user-error "Unbalanced expression")))
    (push-mark start t)
    (goto-char end)
    (activate-mark)
    (message "Marked outer expression")))

(defun donkey-mark-word ()
  "Select the entire word at or adjacent to point."
  (interactive)
  (unless (and (char-after)
               (member (char-syntax (char-after)) '(?\w ?_)))
    (backward-word 1))
  (beginning-of-thing 'word)
  (mark-word)
  (message "Word marked"))

(defun donkey-mark-sentence ()
  "Select sentence at point."
  (interactive)
  (backward-sentence 1)
  (mark-end-of-sentence 1)
  (message "Sentence marked"))

(defun donkey-mark-paragraph ()
  "Select the paragraph at or adjacent to point."
  (interactive)
  (backward-paragraph 1)
  (push-mark (point) nil t)
  (forward-paragraph 1)
  (activate-mark)
  (message "Paragraph marked"))

(defun donkey-mark-symbol ()
  "Select the entire symbol at or adjacent to point.

Trailing commas or periods are omitted from the selection."
  (interactive)
  (unless (and (char-after)
               (member (char-syntax (char-after)) '(?\w ?_)))
    (backward-sexp 1))
  (beginning-of-thing 'symbol)
  (forward-sexp 1)
  (while (memq (char-before) '(?, ?.))
    (backward-char 1))
  (push-mark (point) t)
  (backward-sexp 1)
  (activate-mark)
  (message "Symbol marked"))

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
(keymap-set donkey-normal-mode-map "d" #'donkey-delete)
(keymap-set donkey-normal-mode-map "x" #'donkey-delete)
(keymap-set donkey-normal-mode-map "C" #'donkey-comment-dwim)
(keymap-set donkey-normal-mode-map "C-j" #'join-line)

;; Yank/Paste
(keymap-set donkey-normal-mode-map "P" #'donkey-yank-pop)
(keymap-set donkey-normal-mode-map "p" #'donkey-yank)
(keymap-set donkey-normal-mode-map "y" #'kill-ring-save)

;; Motions
(keymap-set donkey-normal-mode-map "B" #'backward-sexp)
(keymap-set donkey-normal-mode-map "W" #'forward-sexp)
(keymap-set donkey-normal-mode-map "b" #'backward-word)
(keymap-set donkey-normal-mode-map "w" #'forward-word)
(keymap-set donkey-normal-mode-map "S" #'donkey-jump-back)

;; Visual selection
(keymap-set donkey-normal-mode-map "V" #'donkey-visual-line-toggle)
(keymap-set donkey-normal-mode-map "v" #'set-mark-command)

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

;; Buffer navigation
(keymap-set donkey-normal-mode-map "%" #'mark-whole-buffer)
(keymap-set donkey-normal-mode-map "." #'repeat)
(keymap-set donkey-normal-mode-map ":" #'donkey-goto-line)
(keymap-set donkey-normal-mode-map ">" #'donkey-indent-region-or-line)
(keymap-set donkey-normal-mode-map "?" #'donkey-describe-bindings)
(keymap-set donkey-normal-mode-map "U" #'undo-redo)
(keymap-set donkey-normal-mode-map "u" #'undo)
(keymap-set donkey-normal-mode-map "z z" #'recenter-top-bottom)
(keymap-set donkey-normal-mode-map "g e" #'end-of-buffer)
(keymap-set donkey-normal-mode-map "g g" #'beginning-of-buffer)
(keymap-set donkey-normal-mode-map "g h" #'beginning-of-line)
(keymap-set donkey-normal-mode-map "g l" #'move-end-of-line)
(keymap-set donkey-normal-mode-map "g Q" #'fill-paragraph)
(keymap-set donkey-normal-mode-map "g q" #'fill-region)
(keymap-set donkey-normal-mode-map "g t" #'beginning-of-buffer)

;; Search/Replace (Multi-key)
(keymap-set donkey-normal-mode-map "r r" #'replace-regexp)
(keymap-set donkey-normal-mode-map "r q" #'query-replace)

;; Enter/Return Key (Context Aware)
(keymap-set donkey-normal-mode-map "<enter>" #'donkey-enter-dwim)
(keymap-set donkey-normal-mode-map "RET" #'donkey-enter-dwim)

;; Block raw typing keys in NORMAL state
(keymap-set donkey-normal-mode-map "<backspace>" #'ignore)
(keymap-set donkey-normal-mode-map "<delete>" #'ignore)
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
`\donkey--exit-insert' to return to Normal state.")

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
\\[donkey--exit-insert] returns to Normal state."
  :group 'donkey
  :lighter " DONKEY[I]"
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

(defvar-local donkey--last-applied-cursor-setting 'donkey--cursor-unset
  "Last SETTING value actually sent to the terminal via
`donkey--send-cursor-sequence'.  Lets `donkey--apply-cursor-setting'
skip redundant terminal I/O when called again with an unchanged
value -- notably, entering Normal or Insert state triggers this
twice per transition, since each of `donkey-normal-mode' and
`donkey-insert-mode' toggles the other off as part of its own body,
running both modes' hooks (both of which include
`donkey--update-cursor') for what is conceptually one transition.")

(defun donkey--apply-cursor-setting (setting)
  "Apply SETTING, falling back to global default if SETTING is nil.

In terminal mode, also sends DECSCUSR escape sequence for visual
cursor change -- but only when SETTING's effective value actually
changed since the last call, to avoid redundant terminal I/O (see
`donkey--last-applied-cursor-setting')."
  (let ((effective (cond
                    (setting setting)
                    ((local-variable-p 'cursor-type) cursor-type)
                    (t (default-value 'cursor-type)))))
    (if setting
        (setq-local cursor-type setting)
      (kill-local-variable 'cursor-type))
    (unless (equal effective donkey--last-applied-cursor-setting)
      (setq donkey--last-applied-cursor-setting effective)
      (donkey--send-cursor-sequence effective))))

(defun donkey--update-cursor ()
  "Update cursor based on current DONKEY state."
  (cond
   ((bound-and-true-p donkey-normal-mode)
    (donkey--apply-cursor-setting donkey-cursor-normal))
   ((bound-and-true-p donkey-insert-mode)
    (donkey--apply-cursor-setting donkey-cursor-insert))
   (t
    (donkey--apply-cursor-setting nil))))

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
  "Save the originating buffer's DONKEY state and force Insert passthrough
in the minibuffer itself."
  ;; Capture state from the buffer that initiated the minibuffer
  (let ((orig-state
         (with-current-buffer
             (window-buffer (minibuffer-selected-window))
           (donkey--minibuffer-current-state))))
    (push orig-state donkey--minibuffer-pre-state-stack))
  ;; Force insert mode (passthrough) in the minibuffer itself
  (when (bound-and-true-p donkey-normal-mode)
    (donkey-normal-mode -1)))

(defun donkey--minibuffer-exit ()
  "Restore the originating buffer's saved DONKEY state.

Always pops `donkey--minibuffer-pre-state-stack' to keep it balanced
with `donkey--minibuffer-setup', but only re-enters Normal/Insert
state when `donkey-mode' is still globally on.  Without this check,
disabling `donkey-mode' while a minibuffer session is in progress
(e.g. via a keybinding, from a recursive minibuffer) would have this
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
buffer, or when `donkey-mode' is globally off, delegates to
`keyboard-quit' instead.  The `donkey-mode' check matters because
`donkey-setup-smartparens' binds this command directly into
Smartparens' own keymaps (`smartparens-mode-map' and its overlay
keymaps), which are independent of DONKEY's lifecycle: disabling
`donkey-mode' does not undo that binding, so without this guard a
stray `C-g' after disabling DONKEY would still land here and turn
`donkey-normal-mode' back on.

For the minibuffer/excluded-mode case: those buffers stay in Insert
state permanently, so forcing a Normal-state transition here would
just get reverted immediately, silently swallowing `C-g' and
preventing it from reaching the underlying mode (e.g. interrupting a
subprocess or aborting a recursive edit)."
  (interactive)
  (if (or (not (bound-and-true-p donkey-mode))
          (minibufferp)
          (donkey--excluded-mode-p))
      (keyboard-quit)
    (deactivate-mark)
    (donkey-enter-normal)
    (unless (bound-and-true-p donkey-normal-mode)
      (donkey-normal-mode 1))
    (donkey--schedule-overlay-cleanup)))

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
instead of from inside a hook, where signalling `quit' is safe."
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
    (donkey--exit-insert)))

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
  "Deactivate input method when entering Normal state."
  (when donkey-normal-mode
    (when current-input-method
      (setq donkey--saved-input-method current-input-method)
      (deactivate-input-method))))

(defun donkey--on-insert-entry ()
  "Reactivate saved input method when entering Insert state."
  (when donkey-insert-mode
    (when (and donkey--saved-input-method
               (not current-input-method))
      (activate-input-method donkey--saved-input-method))))

(defun donkey--on-input-method-activate ()
  "Prevent input method from staying active in Normal state."
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

Returns ' DONKEY[N]' for Normal, ' DONKEY[I]' for Insert, empty string
otherwise.  Useful if you build your own mode-line and want to
include the DONKEY state."
  (cond
   ((bound-and-true-p donkey-normal-mode) " DONKEY[N]")
   ((bound-and-true-p donkey-insert-mode) " DONKEY[I]")
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
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (donkey--ensure-default-state))))
    (remove-hook 'after-change-major-mode-hook #'donkey--ensure-default-state)
    (remove-hook 'post-command-hook #'donkey--track-position)
    (remove-hook 'post-command-hook #'donkey--check-post-command-non-editing)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p donkey-normal-mode)
          (donkey-normal-mode -1))
        (when (bound-and-true-p donkey-insert-mode)
          (donkey-insert-mode -1))
        (donkey--apply-cursor-setting nil)))))

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------

(provide 'donkey)

;;; donkey.el ends here

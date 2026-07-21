;;; donky.el --- Opinionated Modal Editing -*- lexical-binding: t -*-

;; Copyright (C) 2026 Michael Jones
;; Author: Michael Jones <yardquit@pm.me>
;; Maintainer: Michael Jones
;; Assisted-by: Lumo 2.0 Max
;; URL: https://github.com/yardquit/donky
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; Homepage: https://github.com/yardquit/donky

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
;; wherever possible. Custom commands only where beneficial.
;;
;; Optional Smartparens Integration:
;; If you use smartparens, call `(donky-setup-smartparens)' in
;; your config after loading smartparens to bind C-g in smartparens
;; overlay keymaps. This improves reliability of C-g escape in terminal
;; mode when inside nested smartparens overlays.

;;; Usage:
;; - Press C-g to enter DONKY-NORMAL state.
;; - In NORMAL: h,j,k,l navigate; i,I,a,A,o,O,c enter INSERT state.
;; - In INSERT: Standard Emacs behavior, press C-g to return to NORMAL.
;; - State indicators show in modeline: DONKY[N] = Normal, DONKY[I] = Insert.

;;; Code:

(require 'thingatpt) ;(donky-mark-word)
(require 'cl-lib)    ; Explicitly load cl-lib for cl-some, cl-incf
(eval-and-compile
  (declare-function org-open-at-point "org")     ;(donky-enter-dwim)
  (declare-function org-element-at-point "org")  ;(donky-enter-dwim)
  (declare-function org-edit-src-exit "org")     ;(donky-comment-dwim)
  (declare-function org-edit-special "org")      ;(donky-comment-dwim)
  (defvar donky-normal-mode-map nil)
  (defvar donky-insert-mode-map nil))

(defvar this-command)                          ;(donky--intercept-quit-in-insert)

;;; ---------------------------------------------------------------------------
;;; Donky Excluded-modes
;;; ---------------------------------------------------------------------------

(defcustom donky-excluded-modes
  '(comint-mode term-mode vterm-mode eshell-mode)
  "Major modes where DONKY Normal state should be permanently disabled.

These modes manage subprocess interaction or terminal emulation
where suppressing keys via `suppress-keymap' would break
functionality. Derived modes (e.g. `shell-mode' from
`comint-mode') are caught by `derived-mode-p' in
`donky--ensure-default-state'.

For modes like `dired-mode' or `magit-status-mode' where normal
mode is a preference rather than a necessity, add them here
explicitly if desired."
  :type '(repeat symbol)
  :group 'donky)

(defun donky--handle-non-editing-buffer ()
  "Enter insert mode in excluded major modes when `donky-normal-mode' activates."
  (when (member major-mode donky-excluded-modes)
    (when (bound-and-true-p donky-normal-mode)
      (donky-enter-insert))))

(add-hook 'donky-normal-mode-hook #'donky--handle-non-editing-buffer)

(defun donky--check-post-command-non-editing ()
  "Check after commands if we're in an excluded mode."
  (when (and (bound-and-true-p donky-normal-mode)
             (member major-mode donky-excluded-modes))
    (donky-enter-insert)))

(add-hook 'post-command-hook #'donky--check-post-command-non-editing)

;;; ---------------------------------------------------------------------------
;;; Org-Scratch Buffer Creation
;;; ---------------------------------------------------------------------------

(defun donky-insert-org-scratch-message ()
  "Insert buffer message."
  (insert
   (substitute-command-keys
    (purecopy
     (concat "# This buffer is for scribbling in org-mode.\n"
             "# Start your scribble here and save to file with '"
             "\\[save-some-buffers]"
             "' for persistence.\n\n"))))
  (goto-char (point-max)))

(defun donky-create-org-scratch ()
  "Create an _org-scratch_ buffer."
  (let ((buffer (get-buffer-create "*org-scratch*")))
    (switch-to-buffer buffer)
    (org-mode)
    (donky-insert-org-scratch-message)))

(defun donky-org-scratch ()
  "Create or switch to _org-scratch_."
  (interactive)
  (let ((org-scratch-buffer (get-buffer "*org-scratch*")))
    (if org-scratch-buffer
        (progn
          (switch-to-buffer org-scratch-buffer)
          (message "*org-scratch* buffer already exist, switching."))
      (donky-create-org-scratch)
      (message "*org-scratch* buffer doesn't exist, creating."))))

;;; ---------------------------------------------------------------------------
;;; Donky Describe Bindings
;;; ---------------------------------------------------------------------------

(defun donky--desc-bindings-collect-leaves (map prefix)
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
                                 (donky--desc-bindings-collect-leaves
                                  def (concat full-key " ")))))
              ((and (consp def) (keymapp (cdr def)))
               (setq acc (append acc
                                 (donky--desc-bindings-collect-leaves
                                  (cdr def) (concat full-key " ")))))
              (t
               (push (cons full-key def) acc)))))))
     map)
    (nreverse acc)))

(defun donky--binding-group-name (prefix)
  "Return a human-readable group name for PREFIX."
  (cond
   ((string= prefix "single") "Single Keys")
   ((string= prefix "g")      "Goto / Scroll")
   ((string= prefix "m")      "Mark Objects")
   ((string= prefix "r")      "Search / Replace")
   ((string= prefix "z")      "Scroll")
   (t (format "%s Prefix" (upcase prefix)))))

(defun donky-describe-bindings ()
  "Display all leaf keybindings in `donky-normal-mode-map' with formatting.

Bindings are grouped by prefix, separated by blank rows and section
headers. Command names are clickable buttons that open their
documentation."
  (interactive)
  (unless (boundp 'donky-normal-mode-map)
    (user-error "donky-normal-mode-map is not defined yet"))
  (let* ((buf (get-buffer-create "*DONKY Bindings*"))
         (raw (donky--desc-bindings-collect-leaves donky-normal-mode-map ""))
         (sorted-raw (sort raw (lambda (a b) (string< (car a) (car b))))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; Title
      (insert (propertize "DONKY Normal Mode Key Bindings\n"
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
              (insert (propertize (format "  %s" (donky--binding-group-name group))
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

(defcustom donky-position-ring-max 10
  "Number of position markers retained in the ring."
  :type 'integer
  :group 'donky)

(defvar donky--position-ring nil
  "List of markers recording previous cursor positions, most recent first.")

(defvar donky--position-index 0
  "Current rotation offset into `donky--position-ring'.

0 = most recent entry. Reset to 0 whenever a new position is
recorded.")

(defvar donky--last-tracked-state nil
  "Cons cell (BUFFER . POINT) captured after the previous command.")

(defun donky--track-position ()
  "Record previous cursor position when point or buffer change.

Runs on `post-command-hook'. Independent of the mark ring and
region."
  (unless (minibufferp)
    (let ((now-buf (current-buffer))
          (now-pt  (point)))
      (when (and donky--last-tracked-state
                 (or (not (eq (car donky--last-tracked-state) now-buf))
                     (/= (cdr donky--last-tracked-state) now-pt)))
        (let ((m (make-marker)))
          (set-marker m (cdr donky--last-tracked-state)
                      (car donky--last-tracked-state))
          (push m donky--position-ring)
          (when (> (length donky--position-ring) donky-position-ring-max)
            (set-marker (car (last donky--position-ring)) nil)
            (nbutlast donky--position-ring)))
        (setq donky--position-index 0))
      (setq donky--last-tracked-state (cons now-buf now-pt)))))

(defun donky-jump-back ()
  "Rotate to the next stored position in the ring and jump there.

Press repeatedly to cycle through the last `donky--position-ring-max'
recorded positions. Skips markers whose buffer has been killed."
  (interactive)
  (if (null donky--position-ring)
      (user-error "No positions recorded yet")
    (let ((ring-len (length donky--position-ring))
          target skipped)
      (cl-loop repeat ring-len
               until target
               do
               (setq donky--position-index (1+ donky--position-index))
               (when (>= donky--position-index ring-len)
                 (setq donky--position-index 0))
               (let* ((m (nth donky--position-index donky--position-ring))
                      (buf (and m (marker-buffer m))))
                 (if (and buf (> (marker-position m) 0))
                     (setq target m)
                   (cl-incf skipped))))
      (if target
          (progn
            (pop-to-buffer (marker-buffer target))
            (goto-char target)
            (setq donky--last-tracked-state (cons (current-buffer) (point)))
            (message "Position %d/%d"
                     (1+ donky--position-index) ring-len))
        (user-error "No valid positions in ring")))))

(defun donky-goto-line ()
  "Go to line number."
  (interactive)
  (let ((target-line (read-number "Line: ")))
    (goto-char (point-min))
    (forward-line (1- target-line))))

(defun donky-switch-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

;;; ---------------------------------------------------------------------------
;;; Indentation Commands
;;; ---------------------------------------------------------------------------

(defun donky-indent-region-or-line ()
  "Indent active region or current line."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (indent-region (line-beginning-position) (line-end-position))))

;;; ---------------------------------------------------------------------------
;;; Insert Entry Commands
;;; ---------------------------------------------------------------------------

(defun donky-enter-insert ()
  "Switch to INSERT state."
  (donky-insert-mode 1))

(defun donky-insert-here ()
  "Insert at current position - enters INSERT state."
  (interactive)
  (when (and mark-active (use-region-p))
    (deactivate-mark))
  (donky-enter-insert))

(defun donky-insert-after ()
  "Insert after current char - enters INSERT state."
  (interactive)
  (deactivate-mark)
  (condition-case _err
      (forward-char 1)
    (end-of-buffer nil))
  (donky-enter-insert))

(defun donky-insert-beginning-of-line ()
  "Insert at beginning of line - enters INSERT state."
  (interactive)
  (when (and mark-active (use-region-p))
    (deactivate-mark))
  (beginning-of-line)
  (donky-enter-insert))

(defun donky-insert-end-of-line ()
  "Insert at end of line - enters INSERT state."
  (interactive)
  (when (and mark-active (use-region-p))
    (deactivate-mark))
  (move-end-of-line 1)
  (donky-enter-insert))

(defun donky-open-below ()
  "Open a new line below and enter INSERT state."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (move-end-of-line 1)
  (newline-and-indent)
  (donky-enter-insert))

(defun donky-open-above ()
  "Open a new line above and enter INSERT state."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  (donky-enter-insert))

(defun donky-change ()
  "Change marked char or region - enters INSERT state."
  (interactive)
  (if (use-region-p)
      (progn
        (if (bound-and-true-p rectangle-mark-mode)
            (call-interactively #'string-rectangle)
          (delete-region (mark) (point)))
        (donky-enter-insert))
    (delete-char 1)
    (donky-enter-insert)))

(defun donky-org-todo ()
  "Toggle headline TODO state between TODO and DONE.

Uses `org-element-at-point' to detect :todo-type property and
dispatches `org-todo' accordingly. No keyword string parsing needed."
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

(defvar donky--enter-rules nil
  "List of (ELEMENT-TYPE PROPERTY COMMAND1 COMMAND2 ...) for ENTER DWIM dispatch.")

(defvar-local donky--saved-ret-binding nil
  "Saved RET binding from buffer's local map when entering DONKY Normal.")

(defvar donky-editing-modes
  '(prog-mode text-mode org-mode fundamental-mode conf-mode markdown-mode gfm-mode)
  "Major modes where Enter should be blocked to prevent accidental line breaks.")

(defun donky--editing-mode-p ()
  "Return non-nil if current major mode is in `donky-editing-modes'."
  (member major-mode donky-editing-modes))

(defun donky--register-enter-rule (rule)
  "Register RULE for ENTER DWIM dispatch."
  (add-to-list 'donky--enter-rules rule t))

(defmacro donky-add-enter-rule (element-type property &rest commands)
  "Add an ENTER rule with element type, property, and command fallback.

ELEMENT-TYPE specifies the org element type
\(e.g. `:todo-type', `:checkbox', or nil).
PROPERTY is the attribute to check on the element.
COMMANDS is a list of functions tried sequentially until one succeeds.

See `donky-enter-dwim' for how these rules are evaluated."
  (declare (indent 2))
  `(donky--register-enter-rule '(,element-type ,property ,@commands)))

(defcustom donky-default-enter-rules-enabled t
  "If non-nil, install default ENTER rules on load.

Set to nil in `config.el' if you want to define rules manually."
  :type 'boolean
  :group 'donky)

(defun donky--find-enter-handler ()
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
    (dolist (rule donky--enter-rules)
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
        (dolist (rule donky--enter-rules)
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

(defun donky--execute-handler (cmd)
  "Execute CMD if it exists and is callable."
  (when (and cmd (fboundp cmd) (commandp cmd))
    (call-interactively cmd)))

(defun donky--org-agenda-enter-handler ()
  "Handle Enter in `org-agenda' mode. Return t if handled, otherwise nil."
  (when (and (boundp 'org-agenda-mode-map)
             (derived-mode-p 'org-agenda-mode))
    (let ((ret-cmd (lookup-key org-agenda-mode-map (kbd "RET"))))
      (when (and ret-cmd
                 (not (eq ret-cmd 'undefined))
                 (commandp ret-cmd))
        (call-interactively ret-cmd)
        t))))

(defun donky--org-mode-enter-handler ()
  "Handle Enter in `org-mode' and markdown modes. Return t if handled."
  (when (or (eq major-mode 'org-mode)
            (eq major-mode 'markdown-mode)
            (eq major-mode 'gfm-mode))
    (let ((handler (donky--find-enter-handler)))
      (when handler
        (donky--execute-handler handler)
        t))))

(defun donky--non-editing-enter-handler ()
  "Handle Enter in non-editing modes. Return t if handled."
  (unless (donky--editing-mode-p)
    (when (and donky--saved-ret-binding
               (not (eq donky--saved-ret-binding 'undefined))
               (not (keymapp donky--saved-ret-binding))
               (commandp donky--saved-ret-binding))
      (call-interactively donky--saved-ret-binding)
      t)))

(when donky-default-enter-rules-enabled
  (donky-add-enter-rule item :checkbox org-toggle-checkbox)
  (donky-add-enter-rule headline :todo-type donky-org-todo)
  (donky-add-enter-rule link nil org-open-at-point markdown-follow-thing-at-point browse-url-at-point))

(defun donky-enter-dwim ()
  "Smart Return handler for DONKY Normal State."
  (interactive)
  (cond
   ((donky--org-agenda-enter-handler))
   ((donky--org-mode-enter-handler))
   ((donky--non-editing-enter-handler))))

(add-hook 'donky-normal-mode-hook
          (lambda ()
            (unless (donky--editing-mode-p)
              (setq donky--saved-ret-binding
                    (lookup-key (current-local-map) (kbd "RET")))))
          t)

;;; ---------------------------------------------------------------------------
;;; Comment DWIM
;;; ---------------------------------------------------------------------------

(defun donky--in-org-src-block-p ()
  "Return non-nil if point is inside an Org source block."
  (and (eq major-mode 'org-mode)
       (fboundp 'org-element-at-point)
       (let ((elem (org-element-at-point)))
         (and (consp elem) (eq (car elem) 'src-block)))))

(defun donky-comment-dwim ()
  "Comment/uncomment whole lines in region, or current line if no region.

When inside an Org source block, delegates to the block's native
major mode via `org-edit-special' for language-aware commenting,
then returns to the Org buffer."
  (interactive)
  (cond
   ((donky--in-org-src-block-p)
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
         (message "donky-comment-dwim (org-src): %s"
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

(defvar donky--clipboard-tools-available nil
  "Non-nil when system clipboard integration is available.

Checked synchronously at load time.  Non-nil when the platform
supports native clipboard access or when at least one of
`wl-copy' (Wayland), `xclip'/'xsel' (X11), or `pbcopy' (macOS)
is found in the variable `exec-path'.")

(defvar donky--clipboard-warning-shown nil
  "Non-nil after showing clipboard warning once per session.

Prevents spamming users with repeated tips on every yank operation.")

(defun donky--detect-clipboard-tools ()
  "Detect available system clipboard tools.

Checks for wl-clipboard (Wayland), xclip/xsel (X11), and
pbcopy/pbpaste (macOS). On Windows, native clipboard integration
is assumed. Returns non-nil if any tool or native support is found."
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

;; Run detection synchronously at load time
(setq donky--clipboard-tools-available (donky--detect-clipboard-tools))

(unless (or donky--clipboard-tools-available noninteractive)
  (message "Warning (donky): No system clipboard tools detected.
    Yank will fall back to the kill-ring. Install wl-clipboard
    (Wayland), xclip or xsel (X11) for system clipboard integration."))

;;; ---------------------------------------------------------------------------
;;; Clipboard Platform Diagnostics and Debugging
;;; ---------------------------------------------------------------------------

(defun donky--platform-info ()
  "Return a plist describing the current execution environment.

Includes system type, display backend, terminal type, and clipboard
availability. Useful for debugging platform-specific issues."
  (list :system-type system-type
        :display-type (if (display-graphic-p) 'gui 'terminal)
        :tty-type (tty-type)
        :term-env (getenv "TERM")
        :clipboard-tools-available donky--clipboard-tools-available
        :native-comp (fboundp 'native-comp-available-p)
        :emacs-version emacs-version))

(defun donky-debug-platform ()
  "Display detailed platform information for troubleshooting.

Shows system type, display backend, terminal configuration,
and clipboard tool availability. Useful when reporting bugs
or debugging platform-specific issues.

Output goes to a temporary buffer named '*DONKY Platform Debug*'."
  (interactive)
  (let ((info (donky--platform-info)))
    (with-output-to-temp-buffer "*DONKY Platform Debug*"
      (princ "=== DONKY Modal Platform Diagnostics ===\n\n")

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

    (with-current-buffer "*DONKY Platform Debug*"
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

(defun donky--clipboard-yank ()
  "Yank from the system clipboard with `kill-ring' fallback.

Invokes `clipboard-yank' when the function is available; otherwise
falls back to `yank'. If `clipboard-yank' signals an error
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
  (when (and (not donky--clipboard-warning-shown)
             (not (display-graphic-p))
             (not donky--clipboard-tools-available)
             (not (eq system-type 'darwin))
             (not (eq system-type 'windows-nt)))
    (setq donky--clipboard-warning-shown t)
    (message "Tip: Install wl-clipboard (Wayland) or xclip/xsel (X11) for system clipboard.")))

(defun donky--delete-active-region-safe ()
  "Delete active region if one exists.

Uses the function `kill-active-region' if available (Emacs 29+), falling back to
the function `delete-active-region'.  Handles both cases gracefully."
  (when (use-region-p)
    (if (fboundp 'kill-active-region)
        (kill-active-region)
      (delete-active-region))))

(defun donky-yank ()
  "Yank clipboard content, replacing the active region if present.

Falls back to the kill ring when the system clipboard is
inaccessible. This provides consistent behavior across GUI and
terminal Emacs on Linux (X11/Wayland), macOS, and Windows."
  (interactive)
  (donky--delete-active-region-safe)
  (donky--clipboard-yank))

(defun donky-yank-pop ()
  "Replace the last yanked text with the next `kill-ring' entry.

Removes the active region first if one is present."
  (interactive)
  (donky--delete-active-region-safe)
  (yank-pop))

(defun donky-delete ()
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

(defvar donky-visual-anchor nil
  "Anchor position for visual line selection.")

(defun donky-visual-line-toggle ()
  "Start/cancel visual line selection."
  (interactive)
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (setq donky-visual-anchor nil)
        (message "Visual line: cancelled"))
    (setq donky-visual-anchor (line-beginning-position))
    (set-mark (line-beginning-position))
    (end-of-line)
    (activate-mark)
    (message "Visual line: j/k to extend, V to cancel")))

(defun donky-visual-next-line ()
  "Move down. Extend visual selection if active."
  (interactive)
  (if (and (region-active-p) donky-visual-anchor)
      (progn
        (forward-line 1)
        (if (> (line-beginning-position) donky-visual-anchor)
            (progn
              (set-mark donky-visual-anchor)
              (end-of-line))
          (progn
            (set-mark (save-excursion
                        (goto-char donky-visual-anchor)
                        (line-end-position)))
            (beginning-of-line)))
        (activate-mark))
    (forward-line 1)))

(defun donky-visual-previous-line ()
  "Move up. Extend visual selection if active."
  (interactive)
  (if (and (region-active-p) donky-visual-anchor)
      (progn
        (forward-line -1)
        (if (< (line-beginning-position) donky-visual-anchor)
            (progn
              (set-mark (save-excursion
                          (goto-char donky-visual-anchor)
                          (line-end-position)))
              (beginning-of-line))
          (progn
            (set-mark donky-visual-anchor)
            (end-of-line)))
        (activate-mark))
    (forward-line -1)))

(defun donky-rectangle-mark-mode ()
  "Toggle rectangle mark mode."
  (interactive)
  (if (bound-and-true-p rectangle-mark-mode)
      (progn
        (rectangle-mark-mode -1)
        (deactivate-mark))
    (rectangle-mark-mode 1)
    (right-char 1)))

(defun donky-mark-inner ()
  "Mark text INSIDE CHAR pairs (excluding delimiters)."
  (interactive)
  (let* ((default-char (char-after))
         (supported-openers '(?: ?/ ?+ ?_ ?$ ?= ?* ?~ ?\{ ?\[ ?\( ?\< ?\" ?\' ?\`))
         (on-opener (and default-char (memq default-char supported-openers)))
         (open-char (if on-opener
                        default-char
                      (read-char "Char (:/+_$=*~{[<>'\"`): ")))
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

(defun donky-mark-outer ()
  "Mark text INCLUDING CHAR pairs (delimiters included)."
  (interactive)
  (let* ((default-char (char-after))
         (supported-openers '(?: ?/ ?+ ?_ ?$ ?= ?* ?~ ?\{ ?\[ ?\( ?\< ?\" ?\' ?\`))
         (on-opener (and default-char (memq default-char supported-openers)))
         (open-char (if on-opener
                        default-char
                      (read-char "Char (:/+_$=*~{[<>'\"`): ")))
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
           ((= open-char ?/) ?/)
           ((= open-char ?:) ?:)
           ((= open-char ?+) ?+)
           ((= open-char ?_) ?_)
           ((= open-char ?$) ?$)
           (t
            (error "Unsupported delimiter '%c'. Use: { [ ( < ' \" `" open-char))))
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

(defun donky-mark-sexp-inner ()
  "Mark content inside the balanced expression at point.

Uses the syntax table to identify delimiters (parentheses,
brackets, braces). If point is on an opening or closing
delimiter, marks content within that pair. If point is inside
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

(defun donky-mark-sexp-outer ()
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

(defun donky-mark-word ()
  "Select the entire word at or adjacent to point."
  (interactive)
  (unless (and (char-after)
               (member (char-syntax (char-after)) '(?\w ?_)))
    (backward-word 1))
  (beginning-of-thing 'word)
  (mark-word)
  (message "Word marked"))

(defun donky-mark-sentence ()
  "Select sentence at point."
  (interactive)
  (backward-sentence 1)
  (mark-end-of-sentence 1)
  (message "Sentence marked"))

(defun donky-mark-paragraph ()
  "Select the paragraph at or adjacent to point."
  (interactive)
  (backward-paragraph 1)
  (push-mark (point) nil t)
  (forward-paragraph 1)
  (activate-mark)
  (message "Paragraph marked"))

(defun donky-mark-symbol ()
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
;;; Donky Normal Mode Keymap Definition
;;; ---------------------------------------------------------------------------

(defvar donky-normal-mode-map nil
  "Keymap for DONKY Normal state.")

(when (null donky-normal-mode-map)
  (setq donky-normal-mode-map (make-sparse-keymap)))

(suppress-keymap donky-normal-mode-map t)

;; Navigation
(keymap-set donky-normal-mode-map "h" #'backward-char)
(keymap-set donky-normal-mode-map "j" #'next-line)
(keymap-set donky-normal-mode-map "k" #'previous-line)
(keymap-set donky-normal-mode-map "l" #'forward-char)

;; Visual Line Extension
(keymap-set donky-normal-mode-map "J" #'donky-visual-next-line)
(keymap-set donky-normal-mode-map "K" #'donky-visual-previous-line)

;; Insert mode entry
(keymap-set donky-normal-mode-map "A" #'donky-insert-end-of-line)
(keymap-set donky-normal-mode-map "I" #'donky-insert-beginning-of-line)
(keymap-set donky-normal-mode-map "O" #'donky-open-above)
(keymap-set donky-normal-mode-map "a" #'donky-insert-after)
(keymap-set donky-normal-mode-map "i" #'donky-insert-here)
(keymap-set donky-normal-mode-map "o" #'donky-open-below)

;; Editing operations
(keymap-set donky-normal-mode-map "D" #'kill-line)
(keymap-set donky-normal-mode-map "c" #'donky-change)
(keymap-set donky-normal-mode-map "d" #'donky-delete)
(keymap-set donky-normal-mode-map "x" #'donky-delete)
(keymap-set donky-normal-mode-map "C" #'donky-comment-dwim)
(keymap-set donky-normal-mode-map "C-j" #'join-line)

;; Yank/Paste
(keymap-set donky-normal-mode-map "P" #'donky-yank-pop)
(keymap-set donky-normal-mode-map "p" #'donky-yank)
(keymap-set donky-normal-mode-map "y" #'kill-ring-save)

;; Motions
(keymap-set donky-normal-mode-map "B" #'backward-sexp)
(keymap-set donky-normal-mode-map "W" #'forward-sexp)
(keymap-set donky-normal-mode-map "b" #'backward-word)
(keymap-set donky-normal-mode-map "w" #'forward-word)
(keymap-set donky-normal-mode-map "S" #'donky-jump-back)

;; Visual selection
(keymap-set donky-normal-mode-map "V" #'donky-visual-line-toggle)
(keymap-set donky-normal-mode-map "v" #'set-mark-command)

;; Mark objects
(keymap-set donky-normal-mode-map "m A" #'donky-mark-sexp-outer)
(keymap-set donky-normal-mode-map "m a" #'donky-mark-outer)
(keymap-set donky-normal-mode-map "m I" #'donky-mark-sexp-inner)
(keymap-set donky-normal-mode-map "m i" #'donky-mark-inner)
(keymap-set donky-normal-mode-map "m p" #'donky-mark-paragraph)
(keymap-set donky-normal-mode-map "m s" #'donky-mark-sentence)
(keymap-set donky-normal-mode-map "m v" #'donky-rectangle-mark-mode)
(keymap-set donky-normal-mode-map "m w" #'donky-mark-word)
(keymap-set donky-normal-mode-map "m W" #'donky-mark-symbol)

;; Buffer navigation
(keymap-set donky-normal-mode-map "%" #'mark-whole-buffer)
(keymap-set donky-normal-mode-map "." #'repeat)
(keymap-set donky-normal-mode-map ":" #'donky-goto-line)
(keymap-set donky-normal-mode-map ">" #'donky-indent-region-or-line)
(keymap-set donky-normal-mode-map "?" #'donky-describe-bindings)
(keymap-set donky-normal-mode-map "U" #'undo-redo)
(keymap-set donky-normal-mode-map "u" #'undo)
(keymap-set donky-normal-mode-map "z z" #'recenter-top-bottom)
(keymap-set donky-normal-mode-map "g e" #'end-of-buffer)
(keymap-set donky-normal-mode-map "g g" #'beginning-of-buffer)
(keymap-set donky-normal-mode-map "g h" #'beginning-of-line)
(keymap-set donky-normal-mode-map "g l" #'move-end-of-line)
(keymap-set donky-normal-mode-map "g Q" #'fill-paragraph)
(keymap-set donky-normal-mode-map "g q" #'fill-region)
(keymap-set donky-normal-mode-map "g t" #'beginning-of-buffer)

;; Search/Replace (Multi-key)
(keymap-set donky-normal-mode-map "r r" #'replace-regexp)
(keymap-set donky-normal-mode-map "r q" #'query-replace)

;; Enter/Return Key (Context Aware)
(keymap-set donky-normal-mode-map "<enter>" #'donky-enter-dwim)
(keymap-set donky-normal-mode-map "RET" #'donky-enter-dwim)

;; Block raw typing keys in NORMAL state
(keymap-set donky-normal-mode-map "<backspace>" #'ignore)
(keymap-set donky-normal-mode-map "<delete>" #'ignore)
(keymap-set donky-normal-mode-map "," #'ignore)
(keymap-set donky-normal-mode-map "-" #'ignore)
(keymap-set donky-normal-mode-map "/" #'ignore)
(keymap-set donky-normal-mode-map ";" #'ignore)
(keymap-set donky-normal-mode-map "_" #'ignore)

;;; ---------------------------------------------------------------------------
;;; Donky Insert Mode Keymap Definition
;;; ---------------------------------------------------------------------------

(defvar donky-insert-mode-map nil
  "Keymap for DONKY Insert state.

Minimal keymap: all keys fall through to the major mode and global map,
providing unmodified Emacs behavior.  The `C-g' key runs the command
`\donky--exit-insert' to return to Normal state.")

(when (null donky-insert-mode-map)
  (setq donky-insert-mode-map (make-sparse-keymap)))

;;; ---------------------------------------------------------------------------
;;; Donky Mode Definitions
;;; ---------------------------------------------------------------------------

(define-minor-mode donky-normal-mode
  "DONKY Normal state - modal navigation and editing.

Each buffer maintains its own DONKY state independently. When
enabled, `donky-insert-mode' is automatically disabled and vice
versa."
  :group 'donky
  :lighter " DONKY[N]"
  :keymap donky-normal-mode-map
  (when donky-normal-mode
    (when (bound-and-true-p donky-insert-mode)
      (donky-insert-mode -1))))

(define-minor-mode donky-insert-mode
  "DONKY Insert state - passthrough to standard Emacs input.

All keys fall through to the major mode and global keymap.
\\[donky--exit-insert] returns to Normal state."
  :group 'donky
  :lighter " DONKY[I]"
  :keymap donky-insert-mode-map
  (when donky-insert-mode
    (when (bound-and-true-p donky-normal-mode)
      (donky-normal-mode -1))))

;;; ---------------------------------------------------------------------------
;;; Cursor Management
;;; ---------------------------------------------------------------------------

(defcustom donky-cursor-normal 'box
  "Cursor shape when DONKY Normal state is active.

Set to nil to fall back to global `cursor-type'."
  :type '(choice (const box) (const bar) (const hollow)
                 (cons symbol integer)
                 (const :tag "Use Global Default" nil))
  :group 'donky)

(defcustom donky-cursor-insert '(bar . 2)
  "Cursor shape when DONKY Insert state is active.

Set to nil to fall back to global `cursor-type'."
  :type '(choice (const box) (const bar) (const hollow)
                 (cons symbol integer)
                 (const :tag "Use Global Default" nil))
  :group 'donky)

(defcustom donky--decscusr-denied-terminals
  '("dumb" "linux")
  "List of terminal type prefixes where DECSCUSR is suppressed.

Terminal types reported by `tty-type' that match any prefix in
this list (via `string-prefix-p') will not receive cursor shape
escape sequences. These terminals either lack VT cursor control
or use a non-DECSCUSR mechanism for cursor shapes.

Common entries:
  \"dumb\"  — no escape sequence support whatsoever
  \"linux\" — Linux framebuffer console; uses ioctls, not DECSCUSR

Users may add entries for terminals that exhibit garbled output
when DECSCUSR sequences are sent."
  :type '(repeat string)
  :group 'donky)

(defun donky--cursor-type-to-decscusr (type)
  "Convert cursor TYPE to DECSCUSR escape sequence.

Maps all supported shapes including hollow (blinking)."
  (pcase type
    ('box         "\e[2 q")    ; Steady block
    ('hollow      "\e[0 q")    ; Blinking block (default)
    ('bar         "\e[6 q")    ; Steady bar
    (`(bar . ,_)  "\e[6 q")    ; Steady bar, ignore width
    (`(hbar . ,_) "\e[4 q")    ; Steady underline
    (_ "\e[0 q")))             ; Fallback to default

(defun donky--terminal-supports-decscusr-p ()
  "Return non-nil if the current terminal likely supports DECSCUSR.

Returns nil for graphical frames and for terminals whose type
matches a prefix in `donky--decscusr-denied-terminals'.
Falls back to the `TERM' environment variable when `tty-type'
returns nil, and performs a conservative guess based on known
capable terminal names."
  (and (not (display-graphic-p))
       (let ((tty (or (tty-type) (getenv "TERM"))))
         (when tty
           (and (not (cl-some
                      (lambda (prefix)
                        (string-prefix-p prefix tty))
                      donky--decscusr-denied-terminals))
                (not (member tty '("dumb" "unknown" "cons25"))))))))

(defun donky--send-cursor-sequence (type)
  "Send DECSCUSR escape sequence for TYPE to terminal.

Suppresses output on graphical frames and on terminals listed in
`donky--decscusr-denied-terminals'. Wraps `send-string-to-terminal'
in `condition-case' to silently absorb I/O failures. Sends the
sequence twice with a brief pause to improve delivery reliability
on terminals that drop bytes during state transitions."
  (when (donky--terminal-supports-decscusr-p)
    (let ((seq (donky--cursor-type-to-decscusr type)))
      (when seq
        (condition-case nil
            (progn
              (send-string-to-terminal seq)
              (sit-for 0.01)
              (send-string-to-terminal seq))
          (error nil))))))

(defun donky--apply-cursor-setting (setting)
  "Apply SETTING, falling back to global default if SETTING is nil.

In terminal mode, also sends DECSCUSR escape sequence for visual
cursor change."
  (let ((effective (cond
                    (setting setting)
                    ((local-variable-p 'cursor-type) cursor-type)
                    (t (default-value 'cursor-type)))))
    (if setting
        (setq-local cursor-type setting)
      (kill-local-variable 'cursor-type))
    (donky--send-cursor-sequence effective)))

(defun donky--update-cursor ()
  "Update cursor based on current DONKY state."
  (cond
   ((bound-and-true-p donky-normal-mode)
    (donky--apply-cursor-setting donky-cursor-normal))
   ((bound-and-true-p donky-insert-mode)
    (donky--apply-cursor-setting donky-cursor-insert))
   (t
    (donky--apply-cursor-setting nil))))

(add-hook 'donky-normal-mode-hook #'donky--update-cursor)
(add-hook 'donky-insert-mode-hook #'donky--update-cursor)

;;; ---------------------------------------------------------------------------
;;; Terminal Denylist Management
;;; ---------------------------------------------------------------------------

(defun donky--add-denylist-entry (terminal-prefix)
  "Add TERMINAL-PREFIX to `donky--decscusr-denied-terminals'.

Updates the custom variable and saves to your customization file."
  (interactive
   (list (read-string "Terminal type prefix to deny: ")))
  (unless (member terminal-prefix donky--decscusr-denied-terminals)
    (customize-set-variable 'donky--decscusr-denied-terminals
                            (append donky--decscusr-denied-terminals (list terminal-prefix)))
    (customize-save-variable 'donky--decscusr-denied-terminals
                             donky--decscusr-denied-terminals)
    (message "Added \"%s\" to DECSCUSR denylist" terminal-prefix)))

(defun donky--remove-denylist-entry (terminal-prefix)
  "Remove TERMINAL-PREFIX from `donky--decscusr-denied-terminals'.

Updates the custom variable and saves to your customization file."
  (interactive
   (list (read-string "Terminal type prefix to allow: ")))
  (when (member terminal-prefix donky--decscusr-denied-terminals)
    (customize-set-variable 'donky--decscusr-denied-terminals
                            (cl-remove terminal-prefix donky--decscusr-denied-terminals :test #'string=))
    (customize-save-variable 'donky--decscusr-denied-terminals
                             donky--decscusr-denied-terminals)
    (message "Removed \"%s\" from DECSCUSR denylist" terminal-prefix)))

;;; ---------------------------------------------------------------------------
;;; Donky Minibuffer Safety
;;; ---------------------------------------------------------------------------

(defvar donky--minibuffer-pre-state nil
  "Track DONKY state before entering minibuffer.

Value is normal, insert, or nil. Not buffer-local because we
need to read it after switching buffers.")

(defun donky--minibuffer-current-state ()
  "Return the current DONKY state as a symbol."
  (cond
   ((bound-and-true-p donky-normal-mode) 'normal)
   ((bound-and-true-p donky-insert-mode) 'insert)
   (t nil)))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            ;; Capture state from the buffer that initiated the minibuffer
            (let ((orig-state
                   (with-current-buffer
                       (window-buffer (minibuffer-selected-window))
                     (donky--minibuffer-current-state))))
              (setq donky--minibuffer-pre-state orig-state))
            ;; Force insert mode (passthrough) in the minibuffer itself
            (when (bound-and-true-p donky-normal-mode)
              (donky-normal-mode -1))))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            ;; Restore state in the originating buffer
            (with-current-buffer
                (window-buffer (minibuffer-selected-window))
              (pcase donky--minibuffer-pre-state
                ('normal (donky-enter-normal))
                ('insert (donky-enter-insert))))
            (setq donky--minibuffer-pre-state nil)))

;;; ---------------------------------------------------------------------------
;;; Insert to Normal Transition
;;; ---------------------------------------------------------------------------

(defun donky-enter-normal ()
  "Switch to NORMAL state."
  (interactive)
  (donky-normal-mode 1))

(defvar-local donky--deferred-overlay-cleanup-timer nil
  "Buffer-local timer for deferred overlay cleanup after exiting insert mode.")

(defvar-local donky--just-exited-from-insert nil
  "Buffer-local guard set when exiting insert mode.

Reset on next command to prevent re-entry race conditions.")

(defun donky--clear-transient-overlays ()
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
          (when (or (overlay-get ov 'donky-cleanup)
                    (and face
                         (cond
                          ((symbolp face)
                           (memq face transient-faces))
                          ((consp face)
                           (cl-some (lambda (f) (memq f transient-faces)) face)))))
            (delete-overlay ov)
            (setq cleared (1+ cleared))))))
    ;; Strategy 3: Remove overlays carrying smartparens keymap properties
    (dolist (ov (overlays-in beg end))
      (when (overlay-start ov)
        (let ((km (overlay-get ov 'keymap)))
          (when (and km
                     (or (and (boundp 'sp-pair-overlay-keymap)
                              (eq km sp-pair-overlay-keymap))
                         (and (boundp 'sp-overlay-keymap)
                              (eq km sp-overlay-keymap))))
            (delete-overlay ov)
            (setq cleared (1+ cleared))))))
    cleared))

(defun donky--schedule-overlay-cleanup ()
  "Schedule deferred cleanup for overlays created by post-command hooks."
  (when donky--deferred-overlay-cleanup-timer
    (cancel-timer donky--deferred-overlay-cleanup-timer))
  (let ((buf (current-buffer)))
    (setq donky--deferred-overlay-cleanup-timer
          (run-with-idle-timer
           0.01 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (donky--clear-transient-overlays)
                 (setq donky--deferred-overlay-cleanup-timer nil))))))))

(defun donky--reset-exit-guard ()
  "Reset the exit guard on next command. Allow re-entry of insert mode."
  (setq donky--just-exited-from-insert nil)
  (remove-hook 'pre-command-hook #'donky--reset-exit-guard))

(defun donky--exit-insert ()
  "Exit insert state and enter normal mode.

Removes active mark, enters normal mode, and schedules deferred
overlay cleanup. In the minibuffer, delegates to `keyboard-quit'."
  (interactive)
  (if (minibufferp)
      (keyboard-quit)
    (deactivate-mark)
    (donky-enter-normal)
    (unless (bound-and-true-p donky-normal-mode)
      (donky-normal-mode 1))
    (donky--schedule-overlay-cleanup)))

(defun donky--intercept-quit-in-insert ()
  "Intercept the quit key in insert mode by raw key event or `sp-cancel' command.

Detects a raw quit keypress (or `sp-cancel') while in `donky-insert-mode',
then calls `donky--exit-insert' directly to ensure state transition occurs."
  (when (and (bound-and-true-p donky-insert-mode)
             (not donky--just-exited-from-insert)
             (not (minibufferp))
             (or (equal (this-single-command-keys) [7])
                 (eq this-command 'sp-cancel)))
    (setq this-command 'ignore
          donky--just-exited-from-insert t)
    (add-hook 'pre-command-hook #'donky--reset-exit-guard -100)
    (donky--exit-insert)))

;;; ---------------------------------------------------------------------------
;;; Smartparens Integration (Opt-in)
;;; ---------------------------------------------------------------------------

(defun donky-setup-smartparens ()
  "Set up Smartparens integration.

Call this from your config after loading `smartparens' to bind
C-g in smartparens overlay keymaps. This improves reliability
of C-g escape in terminal mode when inside nested smartparens
overlays."
  (interactive)
  (when (and (boundp 'smartparens-mode-map)
             (keymapp smartparens-mode-map))
    (keymap-set smartparens-mode-map "C-g" #'donky--exit-insert))
  (when (and (boundp 'sp-pair-overlay-keymap)
             (keymapp sp-pair-overlay-keymap))
    (keymap-set sp-pair-overlay-keymap "C-g" #'donky--exit-insert))
  (when (and (boundp 'sp-overlay-keymap)
             (keymapp sp-overlay-keymap))
    (keymap-set sp-overlay-keymap "C-g" #'donky--exit-insert)))

;; Bind C-g directly in insert mode map
(keymap-set donky-insert-mode-map "C-g" #'donky--exit-insert)

;; Pre-command hook backup for packages that override C-g
(add-hook 'pre-command-hook #'donky--intercept-quit-in-insert)

;;; ---------------------------------------------------------------------------
;;; Input Method Management
;;; ---------------------------------------------------------------------------

(defvar-local donky--saved-input-method nil
  "Buffer-local saved input method name for restoration on Insert entry.")

(defun donky--on-normal-entry ()
  "Deactivate input method when entering Normal state."
  (when donky-normal-mode
    (when current-input-method
      (setq donky--saved-input-method current-input-method)
      (deactivate-input-method))))

(defun donky--on-insert-entry ()
  "Reactivate saved input method when entering Insert state."
  (when donky-insert-mode
    (when (and donky--saved-input-method
               (not current-input-method))
      (activate-input-method donky--saved-input-method))))

(defun donky--on-input-method-activate ()
  "Prevent input method from staying active in Normal state."
  (when (bound-and-true-p donky-normal-mode)
    (when current-input-method
      (setq donky--saved-input-method current-input-method)
      (let (input-method-activate-hook)
        (deactivate-input-method)))))

(add-hook 'donky-normal-mode-hook #'donky--on-normal-entry)
(add-hook 'donky-insert-mode-hook #'donky--on-insert-entry)
(add-hook 'input-method-activate-hook #'donky--on-input-method-activate)

;;; ---------------------------------------------------------------------------
;;; Enhanced Mode Activation Logic
;;; ---------------------------------------------------------------------------

(defun donky--ensure-default-state ()
  "Enable DONKY Normal state unless the current major mode is excluded.

For excluded modes, enable DONKY Insert state (passthrough) instead.
Returns non-nil if DONKY was enabled."
  (let ((is-excluded-p
         (or (memq major-mode donky-excluded-modes)
             (apply #'derived-mode-p donky-excluded-modes))))
    (cond
     (is-excluded-p
      (unless (bound-and-true-p donky-insert-mode)
        (donky-enter-insert)
        t))
     (t
      (unless (or (bound-and-true-p donky-normal-mode)
                  (bound-and-true-p donky-insert-mode))
        (donky-enter-normal)
        t)))))

;;; ---------------------------------------------------------------------------
;;; Mode Indicator
;;; ---------------------------------------------------------------------------

(defun donky-indicator ()
  "Return state indicator string for modeline.

Returns ' DONKY[N]' for Normal, ' DONKY[I]' for Insert, empty string
otherwise. Useful if you build your own mode-line and want to
include the DONKY state."
  (cond
   ((bound-and-true-p donky-normal-mode) " DONKY[N]")
   ((bound-and-true-p donky-insert-mode) " DONKY[I]")
   (t "")))

;;; ---------------------------------------------------------------------------
;;; Global Mode Toggle
;;; ---------------------------------------------------------------------------

;;;###autoload
(define-minor-mode donky
  "Toggle DONKY Modal Editing globally.

When enabled, DONKY activates its dual-state system (Normal/Insert)
in all buffers. Buffers whose major mode is in
`donky-excluded-modes' fall back to Insert state (passthrough).

When disabled, all DONKY state is cleared from every buffer and
standard Emacs behavior is restored. \\[donky] or `M-x
donky' to toggle."
  :global t
  :group 'donky
  (if donky
      (progn
        (add-hook 'after-change-major-mode-hook #'donky--ensure-default-state)
        (add-hook 'post-command-hook #'donky--track-position)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (donky--ensure-default-state))))
    (remove-hook 'after-change-major-mode-hook #'donky--ensure-default-state)
    (remove-hook 'post-command-hook #'donky--track-position)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p donky-normal-mode)
          (donky-normal-mode -1))
        (when (bound-and-true-p donky-insert-mode)
          (donky-insert-mode -1))
        (donky--apply-cursor-setting nil)))))

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------

(provide 'donky)

;;; donky.el ends here

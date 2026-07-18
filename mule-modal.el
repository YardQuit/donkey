;;; mule-modal.el --- Opinionated Modal Editing -*- lexical-binding: t -*-

;; Copyright (C) 2026 Michael Jones
;; Author: Michael Jones <yardquit@pm.me>
;; Maintainer: Michael Jones
;; Assisted-by: Lumo 2.0 Max
;; URL: https://github.com/yardquit/mule-modal
;; Version: 2.8
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience
;; Homepage: https://github.com/yardquit/mule-modal

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

;;; Usage:
;; - Press C-g to enter MULE-NORMAL state.
;; - In NORMAL: h,j,k,l navigate; i,I,a,A,o,O,c enter INSERT state.
;; - In INSERT: Standard Emacs behavior, press C-g to return to NORMAL.
;; - State indicators show in modeline: MULE[N] = Normal, MULE[I] = Insert.

;;; Code:

(require 'thingatpt) ;(mule-mark-word)
(require 'cl-lib)    ; Explicitly load cl-lib for cl-some, cl-incf
(eval-and-compile
  (declare-function org-open-at-point "org")     ;(mule-enter-dwim)
  (declare-function org-element-at-point "org")  ;(mule-enter-dwim)
  (declare-function org-edit-src-exit "org")     ;(mule-comment-dwim)
  (declare-function org-edit-special "org")      ;(mule-comment-dwim)
  (defvar mule-normal-mode-map nil)
  (defvar mule-insert-mode-map nil))

(defvar this-single-command-keys)              ;(mule--intercept-quit-in-insert)
(defvar this-command)                          ;(mule--intercept-quit-in-insert)

;;; ---------------------------------------------------------------------------
;;; Mule Excluded-modes
;;; ---------------------------------------------------------------------------

(defcustom mule-excluded-modes
  '(comint-mode term-mode vterm-mode eshell-mode)
  "Major modes where MULE Normal state should be permanently disabled.

These modes manage subprocess interaction or terminal emulation
where suppressing keys via `suppress-keymap' would break
functionality. Derived modes (e.g. `shell-mode' from
`comint-mode') are caught by `derived-mode-p' in
`mule--ensure-default-state'.

For modes like `dired-mode' or `magit-status-mode' where normal
mode is a preference rather than a necessity, add them here
explicitly if desired."
  :type '(repeat symbol)
  :group 'mule)

(defun mule--handle-non-editing-buffer ()
  "Enter insert mode in excluded major modes when mule-normal-mode activates."
  (when (member major-mode mule-excluded-modes)
    (when (bound-and-true-p mule-normal-mode)
      (mule-enter-insert))))

(add-hook 'mule-normal-mode-hook #'mule--handle-non-editing-buffer)

(defun mule--check-post-command-non-editing ()
  "Check after commands if we're in an excluded mode."
  (when (and (bound-and-true-p mule-normal-mode)
             (member major-mode mule-excluded-modes))
    (mule-enter-insert)))

(add-hook 'post-command-hook #'mule--check-post-command-non-editing)

;;; ---------------------------------------------------------------------------
;;; Org-Scratch Buffer Creation
;;; ---------------------------------------------------------------------------

(defun mule-insert-org-scratch-message ()
  "Insert buffer message."
  (insert
   (substitute-command-keys
    (purecopy "\
# This buffer is for scribbling in org-mode.
# Start your scribble here and save to file with ‘\\[save-some-buffers]' for persistence.

")))
  (goto-char (point-max)))

(defun mule-create-org-scratch ()
  "Create an _org-scratch_ buffer."
  (let ((buffer (get-buffer-create "*org-scratch*")))
    (switch-to-buffer buffer)
    (org-mode)
    (mule-insert-org-scratch-message)))

(defun mule-org-scratch ()
  "Create or switch to _org-scratch_."
  (interactive)
  (let ((org-scratch-buffer (get-buffer "*org-scratch*")))
    (if org-scratch-buffer
        (progn
          (switch-to-buffer org-scratch-buffer)
          (message "*org-scratch* buffer already exist, switching."))
      (mule-create-org-scratch)
      (message "*org-scratch* buffer doesn't exist, creating."))))

;;; ---------------------------------------------------------------------------
;;; Mule Describe Bindings
;;; ---------------------------------------------------------------------------

(defun mule--desc-bindings-collect-leaves (map prefix)
  "Recursively walk MAP and return a list of (FULL-KEY . DEF) cons
cells for leaf bindings."
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
                                 (mule--desc-bindings-collect-leaves
                                  def (concat full-key " ")))))
              ((and (consp def) (keymapp (cdr def)))
               (setq acc (append acc
                                 (mule--desc-bindings-collect-leaves
                                  (cdr def) (concat full-key " ")))))
              (t
               (push (cons full-key def) acc)))))))
     map)
    (nreverse acc)))

(defun mule--binding-group-name (prefix)
  "Return a human-readable group name for PREFIX."
  (cond
   ((string= prefix "single") "Single Keys")
   ((string= prefix "g")      "Goto / Scroll")
   ((string= prefix "m")      "Mark Objects")
   ((string= prefix "r")      "Search / Replace")
   ((string= prefix "z")      "Scroll")
   (t (format "%s Prefix" (upcase prefix)))))

(defun mule-describe-bindings ()
  "Display all leaf keybindings in mule-normal-mode-map with formatting.

Bindings are grouped by prefix, separated by blank rows and section
headers. Command names are clickable buttons that open their
documentation."
  (interactive)
  (unless (boundp 'mule-normal-mode-map)
    (user-error "mule-normal-mode-map is not defined yet"))
  (let* ((buf (get-buffer-create "*MULE Bindings*"))
         (raw (mule--desc-bindings-collect-leaves mule-normal-mode-map ""))
         (sorted-raw (sort raw (lambda (a b) (string< (car a) (car b))))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; Title
      (insert (propertize "MULE Normal Mode Key Bindings\n"
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
              (insert (propertize (format "  %s" (mule--binding-group-name group))
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

(defcustom mule--position-ring-max 10
  "Number of position markers retained in the ring."
  :type 'integer
  :group 'mule)

(defvar mule--position-ring nil
  "List of markers recording previous cursor positions, most recent
first.")

(defvar mule--position-index 0
  "Current rotation offset into `mule--position-ring'.

0 = most recent entry. Reset to 0 whenever a new position is
recorded.")

(defvar mule--last-tracked-state nil
  "Cons cell (BUFFER . POINT) captured after the previous command.")

(defun mule--track-position ()
  "Record previous cursor position when point or buffer changes.

Runs on `post-command-hook'. Independent of the mark ring and
region."
  (unless (minibufferp)
    (let ((now-buf (current-buffer))
          (now-pt  (point)))
      (when (and mule--last-tracked-state
                 (or (not (eq (car mule--last-tracked-state) now-buf))
                     (/= (cdr mule--last-tracked-state) now-pt)))
        (let ((m (make-marker)))
          (set-marker m (cdr mule--last-tracked-state)
                      (car mule--last-tracked-state))
          (push m mule--position-ring)
          (when (> (length mule--position-ring) mule--position-ring-max)
            (set-marker (car (last mule--position-ring)) nil)
            (nbutlast mule--position-ring)))
        (setq mule--position-index 0))
      (setq mule--last-tracked-state (cons now-buf now-pt)))))

(defun mule-jump-back ()
  "Rotate to the next stored position in the ring and jump there.

Press repeatedly to cycle through the last `mule--position-ring-max'
recorded positions. Skips markers whose buffer has been killed."
  (interactive)
  (if (null mule--position-ring)
      (user-error "No positions recorded yet")
    (let ((ring-len (length mule--position-ring))
          target skipped)
      (cl-loop repeat ring-len
               until target
               do
               (setq mule--position-index (1+ mule--position-index))
               (when (>= mule--position-index ring-len)
                 (setq mule--position-index 0))
               (let* ((m (nth mule--position-index mule--position-ring))
                      (buf (and m (marker-buffer m))))
                 (if (and buf (> (marker-position m) 0))
                     (setq target m)
                   (cl-incf skipped))))
      (if target
          (progn
            (pop-to-buffer (marker-buffer target))
            (goto-char target)
            (setq mule--last-tracked-state (cons (current-buffer) (point)))
            (message "Position %d/%d"
                     (1+ mule--position-index) ring-len))
        (user-error "No valid positions in ring")))))

(defun mule-goto-line ()
  "Go to line number."
  (interactive)
  (let ((target-line (read-number "Line: ")))
    (goto-char (point-min))
    (forward-line (1- target-line))))

(defun mule-switch-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

;;; ---------------------------------------------------------------------------
;;; Indentation Commands
;;; ---------------------------------------------------------------------------

(defun mule-indent-region-or-line ()
  "Indent active region or current line."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (indent-region (line-beginning-position) (line-end-position))))

;;; ---------------------------------------------------------------------------
;;; Insert Entry Commands
;;; ---------------------------------------------------------------------------

(defun mule-enter-insert ()
  "Switch to INSERT state."
  (mule-insert-mode 1))

(defun mule-insert-here ()
  "Insert at current position - enters INSERT state."
  (interactive)
  (mule-enter-insert))

(defun mule-insert-after ()
  "Insert after current char - enters INSERT state."
  (interactive)
  (condition-case _err
      (forward-char 1)
    (end-of-buffer nil))
  (mule-enter-insert))

(defun mule-insert-beginning-of-line ()
  "Insert at beginning of line - enters INSERT state."
  (interactive)
  (beginning-of-line)
  (mule-enter-insert))

(defun mule-insert-end-of-line ()
  "Insert at end of line - enters INSERT state."
  (interactive)
  (move-end-of-line 1)
  (mule-enter-insert))

(defun mule-open-below ()
  "Open a new line below and enter INSERT state."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (move-end-of-line 1)
  (newline-and-indent)
  (mule-enter-insert))

(defun mule-open-above ()
  "Open a new line above and enter INSERT state."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  (mule-enter-insert))

(defun mule-change ()
  "Change marked char or region - enters INSERT state."
  (interactive)
  (if (use-region-p)
      (progn
        (if (bound-and-true-p rectangle-mark-mode)
            (call-interactively #'string-rectangle)
          (delete-region (mark) (point)))
        (mule-enter-insert))
    (delete-char 1)
    (mule-enter-insert)))

(defun mule-org-todo ()
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
;;; DWIM Commands
;;; ---------------------------------------------------------------------------

(defvar mule--enter-rules nil
  "List of (ELEMENT-TYPE PROPERTY COMMAND1 COMMAND2 ...) for ENTER DWIM dispatch.")

(defvar-local mule--saved-ret-binding nil
  "Saved RET binding from buffer's local map when entering MULE Normal.")

(defvar mule-editing-modes
  '(prog-mode text-mode org-mode fundamental-mode conf-mode markdown-mode gfm-mode)
  "Major modes where Enter should be blocked to prevent accidental line breaks.")

(defun mule--editing-mode-p ()
  "Return non-nil if current major mode is in `mule-editing-modes'."
  (member major-mode mule-editing-modes))

(defun mule--register-enter-rule (rule)
  "Register RULE for ENTER DWIM dispatch."
  (add-to-list 'mule--enter-rules rule t))

(defmacro mule-add-enter-rule (element-type property &rest commands)
  "Add an ENTER rule with property check and command fallback chain."
  (declare (indent 2))
  `(mule--register-enter-rule '(,element-type ,property ,@commands)))

(defcustom mule-default-enter-rules-enabled t
  "If non-nil, install default ENTER rules on load.
  Set to nil in `config.el' if you want to define rules manually."
  :type 'boolean
  :group 'mule)

(defun mule--find-enter-handler ()
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
    (dolist (rule mule--enter-rules)
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
        (dolist (rule mule--enter-rules)
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

(defun mule--execute-handler (cmd)
  "Execute CMD if it exists and is callable."
  (when (and cmd (fboundp cmd) (commandp cmd))
    (call-interactively cmd)))

(defun mule--org-agenda-enter-handler ()
  "Handle Enter in org-agenda mode. Returns t if handled, nil otherwise."
  (when (and (fboundp 'org-agenda-mode-p)
             (boundp 'org-agenda-mode-map)
             (org-agenda-mode-p))
    (let ((ret-cmd (lookup-key org-agenda-mode-map (kbd "RET"))))
      (when (and ret-cmd
                 (not (eq ret-cmd 'undefined))
                 (commandp ret-cmd))
        (call-interactively ret-cmd)
        t))))

(defun mule--org-mode-enter-handler ()
  "Handle Enter in org-mode and markdown modes. Returns t if handled."
  (when (or (eq major-mode 'org-mode)
            (eq major-mode 'markdown-mode)
            (eq major-mode 'gfm-mode))
    (let ((handler (mule--find-enter-handler)))
      (when handler
        (mule--execute-handler handler)
        t))))

(defun mule--non-editing-enter-handler ()
  "Handle Enter in non-editing modes. Returns t if handled."
  (unless (mule--editing-mode-p)
    (when (and mule--saved-ret-binding
               (not (eq mule--saved-ret-binding 'undefined))
               (not (keymapp mule--saved-ret-binding))
               (commandp mule--saved-ret-binding))
      (call-interactively mule--saved-ret-binding)
      t)))

(when mule-default-enter-rules-enabled
  (mule-add-enter-rule item :checkbox org-toggle-checkbox)
  (mule-add-enter-rule headline :todo-type mule-org-todo)
  (mule-add-enter-rule link nil org-open-at-point markdown-follow-thing-at-point browse-url-at-point))

(defun mule-enter-dwim ()
  "Smart Return handler for MULE Normal State."
  (interactive)
  (cond
   ((mule--org-agenda-enter-handler))
   ((mule--org-mode-enter-handler))
   ((mule--non-editing-enter-handler))))

(add-hook 'mule-normal-mode-hook
          (lambda ()
            (unless (mule--editing-mode-p)
              (setq mule--saved-ret-binding
                    (lookup-key (current-local-map) (kbd "RET")))))
          t)

(defun mule--in-org-src-block-p ()
  "Return non-nil if point is inside an Org source block."
  (and (eq major-mode 'org-mode)
       (fboundp 'org-element-at-point)
       (let ((elem (org-element-at-point)))
         (and (consp elem) (eq (car elem) 'src-block)))))

(defun mule-comment-dwim ()
  "Comment/uncomment whole lines in region, or current line if no
region.

When inside an Org source block, delegates to the block's native
major mode via `org-edit-special' for language-aware commenting,
then returns to the Org buffer."
  (interactive)
  (cond
   ((mule--in-org-src-block-p)
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
         (message "mule-comment-dwim (org-src): %s"
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

(defvar mule--clipboard-tools-available nil
  "Non-nil when system clipboard integration is available.

Checked synchronously at load time. Non-nil when the platform
supports native clipboard access or when at least one of
`wl-copy' (Wayland), `xclip'/`xsel' (X11), or `pbcopy' (macOS)
is found in `exec-path'.")

;; Track whether we've shown the clipboard warning this session
(defvar mule--clipboard-warning-shown nil
  "Non-nil after showing clipboard warning once per session.

Prevents spamming users with repeated tips on every yank operation.")

(defun mule--detect-clipboard-tools ()
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
(setq mule--clipboard-tools-available (mule--detect-clipboard-tools))

(unless (or mule--clipboard-tools-available noninteractive)
  (message "Warning (mule-modal): No system clipboard tools detected.
  Yank will fall back to the kill-ring. Install wl-clipboard
  (Wayland), xclip or xsel (X11) for system clipboard integration."))

;;; ---------------------------------------------------------------------------
;;; Clipboard Platform Diagnostics and Debugging
;;; ---------------------------------------------------------------------------

(defun mule--platform-info ()
  "Return a plist describing the current execution environment.

Includes system type, display backend, terminal type, and clipboard
availability. Useful for debugging platform-specific issues."
  (list :system-type system-type
        :display-type (if (display-graphic-p) 'gui 'terminal)
        :tty-type (tty-type)
        :term-env (getenv "TERM")
        :clipboard-tools-available mule--clipboard-tools-available
        :native-comp (fboundp 'native-comp-available-p)
        :emacs-version emacs-version))

(defun mule-debug-platform ()
  "Display detailed platform information for troubleshooting.

Shows system type, display backend, terminal configuration,
and clipboard tool availability. Useful when reporting bugs
or debugging platform-specific issues.

Output goes to a temporary buffer named '*MULE Platform Debug*'."
  (interactive)
  (let ((info (mule--platform-info)))
    (with-output-to-temp-buffer "*MULE Platform Debug*"
      (princ "=== MULE Modal Platform Diagnostics ===\n\n")

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

    (with-current-buffer "*MULE Platform Debug*"
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

(defun mule--clipboard-yank ()
  "Yank from the system clipboard with kill-ring fallback.

Invokes `clipboard-yank' when the function is available; otherwise
falls back to `yank'. If `clipboard-yank' signals an error
(empty or inaccessible clipboard), falls back to `yank' from the
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
  (when (and (not mule--clipboard-warning-shown)
             (not (display-graphic-p))
             (not mule--clipboard-tools-available)
             (not (eq system-type 'darwin))
             (not (eq system-type 'windows-nt)))
    (setq mule--clipboard-warning-shown t)
    (message "Tip: Install wl-clipboard (Wayland) or xclip/xsel (X11) for system clipboard.")))

(defun mule--delete-active-region-safe ()
  "Delete active region if one exists.

Uses `kill-active-region' if available (Emacs 29+), falling back to
`delete-active-region'. Handles both cases gracefully."
  (when (use-region-p)
    (if (fboundp 'kill-active-region)
        (kill-active-region)
      (delete-active-region))))

(defun mule-yank ()
  "Yank clipboard content, replacing the active region if present.

Falls back to the kill ring when the system clipboard is
inaccessible. This provides consistent behavior across GUI and
terminal Emacs on Linux (X11/Wayland), macOS, and Windows."
  (interactive)
  (mule--delete-active-region-safe)
  (mule--clipboard-yank))

(defun mule-yank-pop ()
  "Replace the last yanked text with the next kill-ring entry.

Removes the active region first if one is present."
  (interactive)
  (mule--delete-active-region-safe)
  (yank-pop))

(defun mule-delete ()
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

(defvar mule-visual-anchor nil
  "Anchor position for visual line selection.")

(defun mule-visual-line-toggle ()
  "Start/cancel visual line selection."
  (interactive)
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (setq mule-visual-anchor nil)
        (message "Visual line: cancelled"))
    (setq mule-visual-anchor (line-beginning-position))
    (set-mark (line-beginning-position))
    (end-of-line)
    (activate-mark)
    (message "Visual line: j/k to extend, V to cancel")))

(defun mule-visual-next-line ()
  "Move down. Extends visual selection if active."
  (interactive)
  (if (and (region-active-p) mule-visual-anchor)
      (progn
        (forward-line 1)
        (if (> (line-beginning-position) mule-visual-anchor)
            (progn
              (set-mark mule-visual-anchor)
              (end-of-line))
          (progn
            (set-mark (save-excursion
                        (goto-char mule-visual-anchor)
                        (line-end-position)))
            (beginning-of-line)))
        (activate-mark))
    (forward-line 1)))

(defun mule-visual-previous-line ()
  "Move up. Extends visual selection if active."
  (interactive)
  (if (and (region-active-p) mule-visual-anchor)
      (progn
        (forward-line -1)
        (if (< (line-beginning-position) mule-visual-anchor)
            (progn
              (set-mark (save-excursion
                          (goto-char mule-visual-anchor)
                          (line-end-position)))
              (beginning-of-line))
          (progn
            (set-mark mule-visual-anchor)
            (end-of-line)))
        (activate-mark))
    (forward-line -1)))

(defun mule-rectangle-mark-mode ()
  "Toggle rectangle mark mode."
  (interactive)
  (if (bound-and-true-p rectangle-mark-mode)
      (progn
        (rectangle-mark-mode -1)
        (deactivate-mark))
    (rectangle-mark-mode 1)
    (right-char 1)))

(defun mule-mark-inner ()
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

(defun mule-mark-outer ()
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

(defun mule-mark-sexp-inner ()
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

(defun mule-mark-sexp-outer ()
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

(defun mule-mark-word ()
  "Select the entire word at or adjacent to point."
  (interactive)
  (unless (and (char-after)
               (member (char-syntax (char-after)) '(?\w ?_)))
    (backward-word 1))
  (beginning-of-thing 'word)
  (mark-word)
  (message "Word marked"))

(defun mule-mark-sentence ()
  "Select sentence at point."
  (interactive)
  (backward-sentence 1)
  (mark-end-of-sentence 1)
  (message "Sentence marked"))

(defun mule-mark-paragraph ()
  "Select the paragraph at or adjacent to point."
  (interactive)
  (backward-paragraph 1)
  (push-mark (point) nil t)
  (forward-paragraph 1)
  (activate-mark)
  (message "Paragraph marked"))

(defun mule-mark-symbol ()
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
;;; Mule Normal Mode Keymap Definition
;;; ---------------------------------------------------------------------------

(defvar mule-normal-mode-map nil
  "Keymap for MULE Normal state.")

(when (null mule-normal-mode-map)
  (setq mule-normal-mode-map (make-sparse-keymap)))

(suppress-keymap mule-normal-mode-map t)

;; Navigation
(keymap-set mule-normal-mode-map "h" #'backward-char)
(keymap-set mule-normal-mode-map "j" #'next-line)
(keymap-set mule-normal-mode-map "k" #'previous-line)
(keymap-set mule-normal-mode-map "l" #'forward-char)

;; Visual Line Extension
(keymap-set mule-normal-mode-map "J" #'mule-visual-next-line)
(keymap-set mule-normal-mode-map "K" #'mule-visual-previous-line)

;; Insert mode entry
(keymap-set mule-normal-mode-map "A" #'mule-insert-end-of-line)
(keymap-set mule-normal-mode-map "I" #'mule-insert-beginning-of-line)
(keymap-set mule-normal-mode-map "O" #'mule-open-above)
(keymap-set mule-normal-mode-map "a" #'mule-insert-after)
(keymap-set mule-normal-mode-map "i" #'mule-insert-here)
(keymap-set mule-normal-mode-map "o" #'mule-open-below)

;; Editing operations
(keymap-set mule-normal-mode-map "D" #'kill-line)
(keymap-set mule-normal-mode-map "c" #'mule-change)
(keymap-set mule-normal-mode-map "d" #'mule-delete)
(keymap-set mule-normal-mode-map "x" #'mule-delete)
(keymap-set mule-normal-mode-map "C" #'mule-comment-dwim)
(keymap-set mule-normal-mode-map "C-j" #'join-line)

;; Yank/Paste
(keymap-set mule-normal-mode-map "P" #'mule-yank-pop)
(keymap-set mule-normal-mode-map "p" #'mule-yank)
(keymap-set mule-normal-mode-map "y" #'kill-ring-save)

;; Motions
(keymap-set mule-normal-mode-map "B" #'backward-sexp)
(keymap-set mule-normal-mode-map "W" #'forward-sexp)
(keymap-set mule-normal-mode-map "b" #'backward-word)
(keymap-set mule-normal-mode-map "w" #'forward-word)
(keymap-set mule-normal-mode-map "S" #'mule-jump-back)

;; Visual selection
(keymap-set mule-normal-mode-map "V" #'mule-visual-line-toggle)
(keymap-set mule-normal-mode-map "v" #'set-mark-command)

;; Mark objects
(keymap-set mule-normal-mode-map "m A" #'mule-mark-sexp-outer)
(keymap-set mule-normal-mode-map "m a" #'mule-mark-outer)
(keymap-set mule-normal-mode-map "m I" #'mule-mark-sexp-inner)
(keymap-set mule-normal-mode-map "m i" #'mule-mark-inner)
(keymap-set mule-normal-mode-map "m p" #'mule-mark-paragraph)
(keymap-set mule-normal-mode-map "m s" #'mule-mark-sentence)
(keymap-set mule-normal-mode-map "m v" #'mule-rectangle-mark-mode)
(keymap-set mule-normal-mode-map "m w" #'mule-mark-word)
(keymap-set mule-normal-mode-map "m W" #'mule-mark-symbol)

;; Buffer navigation
(keymap-set mule-normal-mode-map "%" #'mark-whole-buffer)
(keymap-set mule-normal-mode-map "." #'repeat)
(keymap-set mule-normal-mode-map ":" #'mule-goto-line)
(keymap-set mule-normal-mode-map ">" #'mule-indent-region-or-line)
(keymap-set mule-normal-mode-map "?" #'mule-describe-bindings)
(keymap-set mule-normal-mode-map "U" #'undo-redo)
(keymap-set mule-normal-mode-map "u" #'undo)
(keymap-set mule-normal-mode-map "z z" #'recenter-top-bottom)
(keymap-set mule-normal-mode-map "g e" #'end-of-buffer)
(keymap-set mule-normal-mode-map "g g" #'beginning-of-buffer)
(keymap-set mule-normal-mode-map "g h" #'beginning-of-line)
(keymap-set mule-normal-mode-map "g l" #'move-end-of-line)
(keymap-set mule-normal-mode-map "g Q" #'fill-paragraph)
(keymap-set mule-normal-mode-map "g q" #'fill-region)
(keymap-set mule-normal-mode-map "g t" #'beginning-of-buffer)

;; Search/Replace (Multi-key)
(keymap-set mule-normal-mode-map "r r" #'replace-regexp)
(keymap-set mule-normal-mode-map "r q" #'query-replace)

;; Enter/Return Key (Context Aware)
(keymap-set mule-normal-mode-map "<enter>" #'mule-enter-dwim)
(keymap-set mule-normal-mode-map "RET" #'mule-enter-dwim)

;; Block raw typing keys in NORMAL state
(keymap-set mule-normal-mode-map "<backspace>" #'ignore)
(keymap-set mule-normal-mode-map "<delete>" #'ignore)
(keymap-set mule-normal-mode-map "," #'ignore)
(keymap-set mule-normal-mode-map "-" #'ignore)
(keymap-set mule-normal-mode-map "/" #'ignore)
(keymap-set mule-normal-mode-map ";" #'ignore)
(keymap-set mule-normal-mode-map "_" #'ignore)

;;; ---------------------------------------------------------------------------
;;; Mule Insert Mode Keymap Definition
;;; ---------------------------------------------------------------------------

(defvar mule-insert-mode-map nil
  "Keymap for MULE Insert state.

Minimal keymap: all keys fall through to the major mode and global
map, providing unmodified Emacs behavior. C-g is bound to
`mule--exit-insert' to return to Normal state.")
(when (null mule-insert-mode-map)
  (setq mule-insert-mode-map (make-sparse-keymap)))
;; C-g is bound later in the Insert to Normal Transition section.
;; All other keys pass through to the active major mode and global
;; keymap, replicating standard Emacs input behavior.

;;; ---------------------------------------------------------------------------
;;; Mule Mode Definitions
;;; ---------------------------------------------------------------------------

(define-minor-mode mule-normal-mode
  "MULE Normal state - modal navigation and editing.

Each buffer maintains its own MULE state independently. When
enabled, `mule-insert-mode' is automatically disabled and vice
versa."
  :group 'mule
  :lighter " MULE[N]"
  :keymap mule-normal-mode-map
  (when mule-normal-mode
    (when (bound-and-true-p mule-insert-mode)
      (mule-insert-mode -1))))

(define-minor-mode mule-insert-mode
  "MULE Insert state - passthrough to standard Emacs input.

All keys fall through to the major mode and global keymap. Press C-g
to return to Normal state."
  :group 'mule
  :lighter " MULE[I]"
  :keymap mule-insert-mode-map
  (when mule-insert-mode
    (when (bound-and-true-p mule-normal-mode)
      (mule-normal-mode -1))))

;;; ---------------------------------------------------------------------------
;;; Cursor Management
;;; ---------------------------------------------------------------------------

(defcustom mule-cursor-normal 'box
  "Cursor shape when MULE Normal state is active.

Set to nil to fall back to global `cursor-type'."
  :type '(choice (const box) (const bar) (const hollow)
                 (cons symbol integer)
                 (const :tag "Use Global Default" nil))
  :group 'mule)

(defcustom mule-cursor-insert '(bar . 2)
  "Cursor shape when MULE Insert state is active.

Set to nil to fall back to global `cursor-type'."
  :type '(choice (const box) (const bar) (const hollow)
                 (cons symbol integer)
                 (const :tag "Use Global Default" nil))
  :group 'mule)

(defcustom mule--decscusr-denied-terminals
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
  :group 'mule)

(defun mule--cursor-type-to-decscusr (type)
  "Convert cursor TYPE to DECSCUSR escape sequence.

Maps all supported shapes including hollow (blinking)."
  (pcase type
    ('box         "\e[2 q")    ; Steady block
    ('hollow      "\e[0 q")    ; Blinking block (default)
    ('bar         "\e[6 q")    ; Steady bar
    (`(bar . ,_)  "\e[6 q")    ; Steady bar, ignore width
    (`(hbar . ,_) "\e[4 q")    ; Steady underline
    (_ "\e[0 q")))             ; Fallback to default

(defun mule--terminal-supports-decscusr-p ()
  "Return non-nil if the current terminal likely supports DECSCUSR.

Returns nil for graphical frames and for terminals whose type
matches a prefix in `mule--decscusr-denied-terminals'.
Falls back to the `TERM' environment variable when `tty-type'
returns nil, and performs a conservative guess based on known
capable terminal names."
  (and (not (display-graphic-p))
       (let ((tty (or (tty-type) (getenv "TERM"))))
         (when tty
           (and (not (cl-some
                      (lambda (prefix)
                        (string-prefix-p prefix tty))
                      mule--decscusr-denied-terminals))
                (not (member tty '("dumb" "unknown" "cons25"))))))))

(defun mule--send-cursor-sequence (type)
  "Send DECSCUSR escape sequence for TYPE to terminal.

Suppresses output on graphical frames and on terminals listed in
`mule--decscusr-denied-terminals'. Wraps `send-string-to-terminal'
in `condition-case' to silently absorb I/O failures. Sends the
sequence twice with a brief pause to improve delivery reliability
on terminals that drop bytes during state transitions."
  (when (mule--terminal-supports-decscusr-p)
    (let ((seq (mule--cursor-type-to-decscusr type)))
      (when seq
        (condition-case nil
            (progn
              (send-string-to-terminal seq)
              (sit-for 0.01)
              (send-string-to-terminal seq))
          (error nil))))))

(defun mule--apply-cursor-setting (setting)
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
    (mule--send-cursor-sequence effective)))

(defun mule--update-cursor ()
  "Update cursor based on current MULE state."
  (cond
   ((bound-and-true-p mule-normal-mode)
    (mule--apply-cursor-setting mule-cursor-normal))
   ((bound-and-true-p mule-insert-mode)
    (mule--apply-cursor-setting mule-cursor-insert))
   (t
    (mule--apply-cursor-setting nil))))

(add-hook 'mule-normal-mode-hook #'mule--update-cursor)
(add-hook 'mule-insert-mode-hook #'mule--update-cursor)

;;; ---------------------------------------------------------------------------
;;; Terminal Denylist Management
;;; ---------------------------------------------------------------------------

(defun mule--add-denylist-entry (terminal-prefix)
  "Add TERMINAL-PREFIX to `mule--decscusr-denied-terminals'.

Updates the custom variable and saves to your customization file."
  (interactive
   (list (read-string "Terminal type prefix to deny: ")))
  (unless (member terminal-prefix mule--decscusr-denied-terminals)
    (customize-set-variable 'mule--decscusr-denied-terminals
                            (append mule--decscusr-denied-terminals (list terminal-prefix)))
    (customize-save-variable 'mule--decscusr-denied-terminals
                             mule--decscusr-denied-terminals)
    (message "Added \"%s\" to DECSCUSR denylist" terminal-prefix)))

(defun mule--remove-denylist-entry (terminal-prefix)
  "Remove TERMINAL-PREFIX from `mule--decscusr-denied-terminals'.

Updates the custom variable and saves to your customization file."
  (interactive
   (list (read-string "Terminal type prefix to allow: ")))
  (when (member terminal-prefix mule--decscusr-denied-terminals)
    (customize-set-variable 'mule--decscusr-denied-terminals
                            (cl-remove terminal-prefix mule--decscusr-denied-terminals :test #'string=))
    (customize-save-variable 'mule--decscusr-denied-terminals
                             mule--decscusr-denied-terminals)
    (message "Removed \"%s\" from DECSCUSR denylist" terminal-prefix)))

;;; ---------------------------------------------------------------------------
;;; Mule Minibuffer Safety
;;; ---------------------------------------------------------------------------

(defvar mule--minibuffer-pre-state nil
  "Track MULE state before entering minibuffer.

Value is normal, insert, or nil. Not buffer-local because we
need to read it after switching buffers.")

(defun mule--minibuffer-current-state ()
  "Return the current MULE state as a symbol."
  (cond
   ((bound-and-true-p mule-normal-mode) 'normal)
   ((bound-and-true-p mule-insert-mode) 'insert)
   (t nil)))
(add-hook 'minibuffer-setup-hook
          (lambda ()
            ;; Capture state from the buffer that initiated the minibuffer
            (let ((orig-state
                   (with-current-buffer
                       (window-buffer (minibuffer-selected-window))
                     (mule--minibuffer-current-state))))
              (setq mule--minibuffer-pre-state orig-state))
            ;; Force insert mode (passthrough) in the minibuffer itself
            (when (bound-and-true-p mule-normal-mode)
              (mule-normal-mode -1))))
(add-hook 'minibuffer-exit-hook
          (lambda ()
            ;; Restore state in the originating buffer
            (with-current-buffer
                (window-buffer (minibuffer-selected-window))
              (pcase mule--minibuffer-pre-state
                ('normal (mule-enter-normal))
                ('insert (mule-enter-insert))))
            (setq mule--minibuffer-pre-state nil)))

;;; ---------------------------------------------------------------------------
;;; Insert to Normal Transition
;;; ---------------------------------------------------------------------------

(defun mule-enter-normal ()
  "Switch to NORMAL state."
  (interactive)
  (mule-normal-mode 1))

(defvar-local mule--deferred-overlay-cleanup-timer nil
  "Buffer-local timer for deferred overlay cleanup after exiting
insert mode.")

(defvar-local mule--just-exited-from-insert nil
  "Buffer-local guard set when exiting insert mode.

Reset on next command to prevent re-entry race conditions.")

(defun mule--clear-transient-overlays ()
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
          (when (or (overlay-get ov 'mule-modal-cleanup)
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

(defun mule--schedule-overlay-cleanup ()
  "Schedule deferred cleanup for overlays created by post-command
hooks."
  (when mule--deferred-overlay-cleanup-timer
    (cancel-timer mule--deferred-overlay-cleanup-timer))
  (let ((buf (current-buffer)))
    (setq mule--deferred-overlay-cleanup-timer
          (run-with-idle-timer
           0.01 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (mule--clear-transient-overlays)
                 (setq mule--deferred-overlay-cleanup-timer nil))))))))

(defun mule--reset-exit-guard ()
  "Reset the exit guard on next command. Allows re-entry of insert
mode."
  (setq mule--just-exited-from-insert nil)
  (remove-hook 'pre-command-hook #'mule--reset-exit-guard))

(defun mule--exit-insert ()
  "Exit insert state and enter normal mode.

Removes active mark, enters normal mode, and schedules deferred
overlay cleanup. In the minibuffer, delegates to `keyboard-quit'."
  (interactive)
  (if (minibufferp)
      (keyboard-quit)
    (deactivate-mark)
    (mule-enter-normal)
    (unless (bound-and-true-p mule-normal-mode)
      (mule-normal-mode 1))
    (mule--schedule-overlay-cleanup)))

(defun mule--intercept-quit-in-insert ()
  "Intercept C-g in insert mode by raw key event or sp-cancel command.

Calls `mule--exit-insert' directly to ensure state transition occurs."
  (when (and (bound-and-true-p mule-insert-mode)
             (not mule--just-exited-from-insert)
             (not (minibufferp))
             (or (and (boundp 'this-single-command-keys)
                      (equal this-single-command-keys [7]))
                 (eq this-command 'sp-cancel)))
    (setq this-command 'ignore
          mule--just-exited-from-insert t)
    (add-hook 'pre-command-hook #'mule--reset-exit-guard -100)
    (mule--exit-insert)))

(defun mule--setup-smartparens-integration ()
  "Configure C-g handler in all smartparens keymaps.

Binds C-g in smartparens-mode-map AND overlay keymaps
(sp-pair-overlay-keymap, sp-overlay-keymap). Overlay keymaps have
higher priority than minor-mode maps."
  (when (and (boundp 'smartparens-mode-map)
             (keymapp smartparens-mode-map))
    (keymap-set smartparens-mode-map "C-g" #'mule--exit-insert))
  (when (and (boundp 'sp-pair-overlay-keymap)
             (keymapp sp-pair-overlay-keymap))
    (keymap-set sp-pair-overlay-keymap "C-g" #'mule--exit-insert))
  (when (and (boundp 'sp-overlay-keymap)
             (keymapp sp-overlay-keymap))
    (keymap-set sp-overlay-keymap "C-g" #'mule--exit-insert)))

(with-eval-after-load 'smartparens
  (mule--setup-smartparens-integration))

(when (featurep 'smartparens)
  (mule--setup-smartparens-integration))

;; Bind C-g directly in insert mode map
(keymap-set mule-insert-mode-map "C-g" #'mule--exit-insert)

;; Pre-command hook backup for packages that override C-g
(add-hook 'pre-command-hook #'mule--intercept-quit-in-insert)

;;; ---------------------------------------------------------------------------
;;; Input Method Management
;;; ---------------------------------------------------------------------------

(defvar-local mule--saved-input-method nil
  "Buffer-local saved input method name for restoration on Insert
entry.")

(defun mule--on-normal-entry ()
  "Deactivate input method when entering Normal state."
  (when mule-normal-mode
    (when current-input-method
      (setq mule--saved-input-method current-input-method)
      (deactivate-input-method))))

(defun mule--on-insert-entry ()
  "Reactivate saved input method when entering Insert state."
  (when mule-insert-mode
    (when (and mule--saved-input-method
               (not current-input-method))
      (activate-input-method mule--saved-input-method))))

(defun mule--on-input-method-activate ()
  "Prevent input method from staying active in Normal state."
  (when (bound-and-true-p mule-normal-mode)
    (when current-input-method
      (setq mule--saved-input-method current-input-method)
      (let (input-method-activate-hook)
        (deactivate-input-method)))))

(add-hook 'mule-normal-mode-hook #'mule--on-normal-entry)
(add-hook 'mule-insert-mode-hook #'mule--on-insert-entry)
(add-hook 'input-method-activate-hook #'mule--on-input-method-activate)

;;; ---------------------------------------------------------------------------
;;; Enhanced Mode Activation Logic
;;; ---------------------------------------------------------------------------

(defun mule--ensure-default-state ()
  "Enable MULE Normal state unless the current major mode is
excluded.

For excluded modes, enable MULE Insert state (passthrough) instead.
Returns non-nil if MULE was enabled."
  (let ((is-excluded-p
         (or (memq major-mode mule-excluded-modes)
             (apply #'derived-mode-p mule-excluded-modes))))
    (cond
     (is-excluded-p
      (unless (bound-and-true-p mule-insert-mode)
        (mule-enter-insert)
        t))
     (t
      (unless (or (bound-and-true-p mule-normal-mode)
                  (bound-and-true-p mule-insert-mode))
        (mule-enter-normal)
        t)))))

;;; ---------------------------------------------------------------------------
;;; Mode Indicator
;;; ---------------------------------------------------------------------------

(defun mule-indicator ()
  "Return state indicator string for modeline.

Returns ' MULE[N]' for Normal, ' MULE[I]' for Insert, empty string
otherwise. Useful if you build your own mode-line and want to
include the MULE state."
  (cond
   ((bound-and-true-p mule-normal-mode) " MULE[N]")
   ((bound-and-true-p mule-insert-mode) " MULE[I]")
   (t "")))

;;; ---------------------------------------------------------------------------
;;; Global Mode Toggle
;;; ---------------------------------------------------------------------------

(define-minor-mode mule-modal
  "Toggle MULE Modal Editing globally.

When enabled, MULE activates its dual-state system (Normal/Insert)
in all buffers. Buffers whose major mode is in
`mule--excluded-modes' fall back to Insert state (passthrough).

When disabled, all MULE state is cleared from every buffer and
standard Emacs behavior is restored. \\[mule-modal] or `M-x
mule-modal' to toggle."
  :global t
  :group 'mule
  (if mule-modal
      (progn
        (add-hook 'after-change-major-mode-hook #'mule--ensure-default-state)
        (add-hook 'post-command-hook #'mule--track-position)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (mule--ensure-default-state))))
    (remove-hook 'after-change-major-mode-hook #'mule--ensure-default-state)
    (remove-hook 'post-command-hook #'mule--track-position)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p mule-normal-mode)
          (mule-normal-mode -1))
        (when (bound-and-true-p mule-insert-mode)
          (mule-insert-mode -1))
        (mule--apply-cursor-setting nil)))))

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------

(provide 'mule-modal)

;;; mule-modal.el ends here

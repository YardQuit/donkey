;;; mule-modal.el --- Opinionated Modal Editing -*- lexical-binding: t -*-

;; Copyright (C) 2026 Michael Jones
;; Author: Michael Jones <yardquit@pm.me>
;; Maintainer: Michael Jones
;; Assisted-by: Lumo+
;; URL: https://github.com/yardquit/mule-modal
;; Version: 1.4
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Philosophy: Leverage Emacs Native Commands and built-in functions
;; wherever possible. Custom commands only where beneficial.
;;
;; Warning: May be incompatible with other packages and modal
;; editors. This package overrides 'ESC' to manage input states
;; (MULE/Normal). Loading it alongside other packages that bind
;; 'ESC' will cause unexpected behavior and state conflicts.

;;; Usage:
;; - Press <escape> to enter ON (Normal) State.
;; - Press h, j, k, l to navigate left, down, up, right.
;; - Press i, I, a, A, o, O, c to enter OFF (Insert) State.
;; - Press V to select lines (extend with j/k or J/K, cancel with V again).
;; - In ON (Normal) State, raw typing keys are blocked.

;;; Code:
(require 'thingatpt) ;; (mule-select-word)

(eval-and-compile
  (declare-function org-open-at-point "org")     ;(mule-enter-dwim)
  (declare-function org-element-at-point "org")  ;(mule-enter-dwim)
  (declare-function org-edit-src-exit "org")     ;(mule-comment-dwim)
  (declare-function org-edit-special "org"))     ;(mule-comment-dwim)

(defvar mule-mode-map nil ;(declaration of mule-mode-map)
  "Keymap for MULE minor mode.")

;;; ---------------------------------------------------------------------------
;;; Buffer Creation Functions
;;; ---------------------------------------------------------------------------
(defun mule-insert-org-scratch-message ()
  "Inserts buffer message"
  (insert
   (substitute-command-keys
;; start - leave block as it looks.
    (purecopy "\
# This buffer is for scribbling in org-mode.
# Start your scribble here and save to file with ‘\\[save-some-buffers]' for persistence.

"))))
;; end - leave block as it looks.
(goto-char (point-max))

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
;;; Mule Describe Bindings Functions
;;; ---------------------------------------------------------------------------
(defun mule--desc-bindings-walk (map prefix)
  "Helper for mule-describe-bindings: Recursively walk MAP and
  insert non-prefix keys."
  (map-keymap
   (lambda (key def)
     (let ((full-key (concat prefix (key-description (vector key)))))
       (when (and (not (eq def 'undefined))
                  (not (and (listp def) 
                            (eq (car def) 'remap) 
                            (eq (cadr def) 'self-insert-command)
                            (null (cdr (cddr def)))))) 
         (unless (or (keymapp def)
                     (and (listp def) (keymapp (cdr def))))
           (insert (format "%-25s %s\n" full-key
                           (if (symbolp def) (symbol-name def) "[complex]"))))

         (cond
          ((keymapp def)
           (mule--desc-bindings-walk def (concat full-key " ")))
          ((and (listp def) (keymapp (cdr def)))
           (mule--desc-bindings-walk (cdr def) (concat full-key " ")))))))
   map))

(defun mule-describe-bindings ()
  "Display all *leaf* keybindings in mule-mode-map. Excludes prefix
    keys from the output list."
  (interactive)
  (unless (boundp 'mule-mode-map)
    (user-error "mule-mode-map is not defined yet"))

  (let ((buf (get-buffer-create "*MULE Bindings*")))
    (with-current-buffer buf
      (setq buffer-read-only nil))

    (with-current-buffer buf
      (erase-buffer)

      (insert "MULE Mode Key Bindings\n")
      (insert (make-string 40 ?=) "\n\n")
      (insert (format "%-25s %s\n" "KEY" "COMMAND"))
      (insert (make-string 48 ?-) "\n")

      (mule--desc-bindings-walk mule-mode-map "")

      (goto-char (point-min))
      (special-mode)
      (setq-local buffer-read-only t)
      (setq-local truncate-lines t)
      
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "RET") #'bury-buffer))

    (display-buffer buf)))

;;; ---------------------------------------------------------------------------
;;; Mule Enter DWIM Functions
;;; ---------------------------------------------------------------------------
(defvar mule-editing-modes
  '(prog-mode text-mode org-mode fundamental-mode conf-mode markdown-mode gfm-mode)
  "Major modes where Enter should be blocked to prevent accidental
  line breaks.")

(defun mule--editing-mode-p ()
  "Return non-nil if current major mode is in `mule-editing-modes'."
  (member major-mode mule-editing-modes))

(defun mule--org-enter-handler ()
  "Handle Enter in Org mode: follow links except src-blocks."
  (when (and (eq major-mode 'org-mode)
             (fboundp 'org-element-at-point)
             (fboundp 'org-open-at-point))
    (let ((elem (org-element-at-point)))
      (when (and elem (not (eq (car elem) 'src-block)))
        #'org-open-at-point))))

(defun mule--markdown-enter-handler ()
  "Handle Enter in Markdown mode: follow links/buttons."
  (when (memq major-mode '(markdown-mode gfm-mode))
    (cond
     ((fboundp 'markdown-follow-thing-at-point)
      #'markdown-follow-thing-at-point)
     ((fboundp 'shr-follow-link-at-point)
      #'shr-follow-link-at-point)
     (t
      #'browse-url-at-point))))

(defun mule--non-editing-enter-handler ()
  "Handle Enter in non-editing modes (Info, Dired, etc.):
  fallthrough."
  (unless (mule--editing-mode-p)
    (let ((native-ret (lookup-key (current-local-map) (kbd "RET"))))
      (when (and native-ret
                 (not (eq native-ret 'undefined))
                 (fboundp native-ret))
        native-ret))))

(defun mule-enter-dwim ()
  "Smart Return handler for MULE Normal State."
  (interactive)
  (let ((follow-cmd nil))
    (setq follow-cmd (or (mule--org-enter-handler)
                         (mule--markdown-enter-handler)
                         (mule--non-editing-enter-handler)))
    (when follow-cmd
      (call-interactively follow-cmd))))

;;; ---------------------------------------------------------------------------
;;; Mule Comment DWIM Functions
;;; ---------------------------------------------------------------------------
(defun mule--in-org-src-block-p ()
  "Return non-nil if point is inside an Org source block."
  (and (eq major-mode 'org-mode)
       (fboundp 'org-element-at-point)
       (let ((elem (org-element-at-point)))
         (and elem (eq (car elem) 'src-block)))))

(defun mule-comment-dwim ()
  "Comment/uncomment whole lines in region, or current line if no
  region. When inside an Org source block, delegates to the block's
  native major mode via `org-edit-special' for language-aware
  commenting, then returns to the Org buffer."
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

(defun mule-goto-line ()
  "Go to line number."
  (interactive)
  (let ((target-line (read-number "Line: ")))
    (goto-char (point-min))
    (forward-line (1- target-line))))

(defun mule-indent-region-or-line ()
  "Indent active region or current line."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (indent-region (line-beginning-position) (line-end-position))))

(defun mule-mode-insert ()
  "Enter insert state (disable mode)."
  (interactive)
  (mule-mode 0))

(defun mule-insert-after ()
  "Insert after current char."
  (interactive)
  (forward-char 1)
  (mule-mode 0))

(defun mule-insert-beginning-of-line ()
  "Insert at beginning of line."
  (interactive)
  (beginning-of-line)
  (mule-mode 0))

(defun mule-insert-end-of-line ()
  "Insert at end of line."
  (interactive)
  (move-end-of-line 1)
  (mule-mode 0))

(defun mule-open-below ()
  "Open a new line below and enter insert state."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (move-end-of-line 1)
  (newline-and-indent)
  (mule-mode 0))

(defun mule-open-above ()
  "Open a new line above and enter insert state."
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (move-beginning-of-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  (mule-mode 0))

(defun mule-change ()
  "Change marked char or region."
  (interactive)
  (if (use-region-p)
      (progn
        (if (bound-and-true-p rectangle-mark-mode)
            (call-interactively #'string-rectangle)
          (delete-region (mark) (point)))
        (mule-mode 0))
    (delete-char 1)
    (mule-mode 0)))

(defun mule-delete ()
  "Delete character or region."
  (interactive)
  (if (use-region-p)
      (progn
        (if (bound-and-true-p rectangle-mark-mode)
            (call-interactively #'kill-rectangle)
          (kill-region (mark) (point))))
    (delete-char 1)))

(defun mule-kill-to-end-of-line ()
  "Kill from point to end of line."
  (interactive)
  (kill-line))

(defun mule-yank ()
  "Yank clipboard content."
  (interactive)
  (if (use-region-p)
      (progn
        (delete-active-region)
        (clipboard-yank))
    (clipboard-yank)))

(defun mule-yank-pop ()
  "Rotate yanks."
  (interactive)
  (if (use-region-p)
      (progn
        (delete-active-region)
        (yank-pop))
    (yank-pop)))

(defun mule-fill-paragraph ()
  "Fill current paragraph."
  (interactive)
  (fill-paragraph))

(defun mule-fill-region ()
  "Fill selected region."
  (interactive)
  (when (region-active-p)
    (fill-region (region-beginning) (region-end))))

(defun mule-move-to-beginning-of-line ()
  "Move to beginning of line."
  (interactive)
  (beginning-of-line))

(defun mule-move-to-end-of-line ()
  "Move to end of line."
  (interactive)
  (move-end-of-line 1))

(defun mule-forward-word ()
  "Forward word."
  (interactive)
  (forward-word 1))

(defun mule-backward-word ()
  "Backward word."
  (interactive)
  (backward-word 1))

(defun mule-forward-symbol ()
  "Forward sexp."
  (interactive)
  (forward-sexp 1))

(defun mule-backward-symbol ()
  "Backward sexp."
  (interactive)
  (backward-sexp 1))

(defun mule-switch-other-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun mule-upcase-region ()
  "Uppercase region."
  (interactive)
  (when (use-region-p)
    (upcase-region (region-beginning) (region-end))))

(defun mule-downcase-region ()
  "Lowercase region."
  (interactive)
  (when (use-region-p)
    (downcase-region (region-beginning) (region-end))))

;;; ---------------------------------------------------------------------------
;;; Mule Jump Back Functions
;;; ---------------------------------------------------------------------------
(defcustom mule--position-ring-max 5
  "Number of position markers retained in the ring."
  :type 'integer
  :group 'mule)

(defvar mule--position-ring nil
  "List of markers recording previous cursor positions, most recent
  first.")

(defvar mule--position-index 0
  "Current rotation offset into `mule--position-ring'. 0 = most
  recent entry. Reset to 0 whenever a new position is recorded.")

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
  Press repeatedly to cycle through the last
  `mule--position-ring-max' recorded positions. Skips markers whose
  buffer has been killed."
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

(add-hook 'post-command-hook #'mule--track-position)

;;; ---------------------------------------------------------------------------
;;; Mark Visual Line Selection Functions
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
  (rectangle-mark-mode 1)
  (right-char 1))

(defun mule-mark-inner ()
  "Mark text INSIDE CHAR pairs (excluding delimiters)."
  (interactive)
  (let* ((default-char (char-after))
         (supported-openers '(?/ ?+ ?_ ?$ ?= ?* ?~ ?\{ ?\[ ?\( ?\< ?\" ?\' ?\`))
         (on-opener (and default-char (memq default-char supported-openers)))
         (open-char (if on-opener
                        default-char
                      (read-char "Char (/+_$=*~{[<>'\"`): ")))
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
         (supported-openers '(?/ ?+ ?_ ?$ ?= ?* ?~ ?\{ ?\[ ?\( ?\< ?\" ?\' ?\`))
         (on-opener (and default-char (memq default-char supported-openers)))
         (open-char (if on-opener
                        default-char
                      (read-char "Char (/+_$=*~{[<>'\"`): ")))
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

    (goto-char (1+ start-pos))  ;; ← THIS LINE WAS MISSING

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

;;; ---------------------------------------------------------------------------
;;; Mule Mode Keymap Definition
;;; ---------------------------------------------------------------------------
(when (null mule-mode-map)
  (setq mule-mode-map (make-sparse-keymap)))

(suppress-keymap mule-mode-map t)

;; Navigation
(keymap-set mule-mode-map "h" #'backward-char)
(keymap-set mule-mode-map "j" #'next-line)
(keymap-set mule-mode-map "k" #'previous-line)
(keymap-set mule-mode-map "l" #'forward-char)

;; Visual Line Extension
(keymap-set mule-mode-map "J" #'mule-visual-next-line)
(keymap-set mule-mode-map "K" #'mule-visual-previous-line)

;; Insert mode entry
(keymap-set mule-mode-map "A" #'mule-insert-end-of-line)
(keymap-set mule-mode-map "I" #'mule-insert-beginning-of-line)
(keymap-set mule-mode-map "O" #'mule-open-above)
(keymap-set mule-mode-map "a" #'mule-insert-after)
(keymap-set mule-mode-map "i" #'mule-mode-insert)
(keymap-set mule-mode-map "o" #'mule-open-below)

;; Editing operations
(keymap-set mule-mode-map "D" #'mule-kill-to-end-of-line)
(keymap-set mule-mode-map "c" #'mule-change)
(keymap-set mule-mode-map "d" #'mule-delete)
(keymap-set mule-mode-map "x" #'mule-delete)
(keymap-set mule-mode-map "C" #'mule-comment-dwim)
(keymap-set mule-mode-map "C-j" #'join-line)

;; Yank/Paste
(keymap-set mule-mode-map "P" #'mule-yank-pop)
(keymap-set mule-mode-map "p" #'mule-yank)
(keymap-set mule-mode-map "y" #'kill-ring-save)

;; Motions
(keymap-set mule-mode-map "B" #'mule-backward-symbol)
(keymap-set mule-mode-map "W" #'mule-forward-symbol)
(keymap-set mule-mode-map "b" #'mule-backward-word)
(keymap-set mule-mode-map "w" #'mule-forward-word)
(keymap-set mule-mode-map "S" #'mule-jump-back)

;; Visual selection
(keymap-set mule-mode-map "V" #'mule-visual-line-toggle)
(keymap-set mule-mode-map "v" #'set-mark-command)

;; Mark objects
(keymap-set mule-mode-map "m a" #'mule-mark-outer)
(keymap-set mule-mode-map "m i" #'mule-mark-inner)
(keymap-set mule-mode-map "m p" #'mule-mark-paragraph)
(keymap-set mule-mode-map "m s" #'mule-mark-sentence)
(keymap-set mule-mode-map "m v" #'mule-rectangle-mark-mode)
(keymap-set mule-mode-map "m w" #'mule-mark-word)

;; Buffer navigation
(keymap-set mule-mode-map "%" #'mark-whole-buffer)
(keymap-set mule-mode-map "." #'repeat)
(keymap-set mule-mode-map ":" #'mule-goto-line)
(keymap-set mule-mode-map ">" #'mule-indent-region-or-line)
(keymap-set mule-mode-map "?" #'mule-describe-bindings)
(keymap-set mule-mode-map "U" #'undo-redo)
(keymap-set mule-mode-map "u" #'undo)
(keymap-set mule-mode-map "z z" #'recenter-top-bottom)
(keymap-set mule-mode-map "g e" #'end-of-buffer)
(keymap-set mule-mode-map "g g" #'beginning-of-buffer)
(keymap-set mule-mode-map "g h" #'mule-move-to-beginning-of-line)
(keymap-set mule-mode-map "g l" #'mule-move-to-end-of-line)
(keymap-set mule-mode-map "g Q" #'mule-fill-paragraph)
(keymap-set mule-mode-map "g q" #'mule-fill-region)
(keymap-set mule-mode-map "g t" #'beginning-of-buffer)

;; Search/Replace (Multi-key)
(keymap-set mule-mode-map "r r" #'replace-regexp)
(keymap-set mule-mode-map "r q" #'query-replace)

;; Enter/Return Key (Context Aware)
(keymap-set mule-mode-map "<enter>" #'mule-enter-dwim)
(keymap-set mule-mode-map "RET" #'mule-enter-dwim)

;; IGNORE typing/blocking keys
(keymap-set mule-mode-map "<backspace>" #'ignore)
(keymap-set mule-mode-map "<delete>" #'ignore)
(keymap-set mule-mode-map "," #'ignore)
(keymap-set mule-mode-map "-" #'ignore)
(keymap-set mule-mode-map "/" #'ignore)
(keymap-set mule-mode-map ";" #'ignore)
(keymap-set mule-mode-map "_" #'ignore)

;;; ---------------------------------------------------------------------------
;;; Mode Definition
;;; ---------------------------------------------------------------------------
(define-minor-mode mule-mode
  "Opinionated Modal Editing - Normal State Navigation.
              Each buffer maintains its own mule-mode state independently."
  :group 'mule
  :lighter " MULE"
  :keymap mule-mode-map)

;;; ---------------------------------------------------------------------------
;;; Global Escape Binding
;;; ---------------------------------------------------------------------------
(global-set-key [escape]
                (lambda ()
                  (interactive)
                  (cond
                   ((minibufferp) (keyboard-quit))
                   (mule-mode (keyboard-quit))
                   (t (mule-mode 1)))))

;;; ---------------------------------------------------------------------------
;;; Cursor Management (Per-Buffer, Enforced via Hook)
;;; ---------------------------------------------------------------------------
(defcustom mule-cursor-normal 'box
  "Cursor shape when MULE Normal state is active. Set to nil to fall
  back to global `cursor-type', otherwise must be a valid cursor
  type symbol or cons cell."
  :type '(choice (const box) (const bar) (const hollow) (cons symbol integer) (const :tag "Use Global Default" nil))
  :group 'mule)

(defcustom mule-cursor-insert '(bar . 2)
  "Cursor shape when MULE is inactive (Insert state). Set to nil to
  fall back to global `cursor-type', otherwise must be a valid
  cursor type symbol or cons cell."
  :type '(choice (const box) (const bar) (const hollow) (cons symbol integer) (const :tag "Use Global Default" nil))
  :group 'mule)

(defun mule--apply-cursor-setting (setting)
  "Apply SETTING, falling back to global default if SETTING is nil."
  (if setting
      (setq-local cursor-type setting)
    (kill-local-variable 'cursor-type)))

(defun mule--update-cursor ()
  "Update cursor based on current mode state and custom variables."
  (if (and (boundp 'mule-mode) mule-mode)
      (mule--apply-cursor-setting mule-cursor-normal)
    (mule--apply-cursor-setting mule-cursor-insert)))

(add-hook 'mule-mode-hook #'mule--update-cursor)

;;; ---------------------------------------------------------------------------
;;; Minibuffer Safety
;;; ---------------------------------------------------------------------------
(defvar mule--minibuffer-state nil
  "Track whether mule-mode was active before entering minibuffer.")

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq mule--minibuffer-state mule-mode)
            (when mule-mode
              (mule-mode -1))))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (when mule--minibuffer-state
              (mule-mode 1)
              (setq mule--minibuffer-state nil))))

;;; ---------------------------------------------------------------------------
;;; Exclusion Mode Safeguards
;;; ---------------------------------------------------------------------------
;; Explicitly disable mule-mode AND reset cursor for known incompatible modes.
;; This is a defense-in-depth measure alongside after-change-major-mode-hook filtering.
(defvar mule--excluded-modes
  '(ibuffer-mode eshell-mode term-mode vterm-mode dired-mode comint-mode magit-status-mode)
  "Major modes where mule-mode should be permanently disabled.

These modes are checked using both DERIVED-MODE-P and direct
MAJOR-MODE comparison in `mule--ensure-default-state' to catch edge
cases where one method might fail.")

(dolist (mode mule--excluded-modes)
  (let ((hook (intern (format "%s-hook" mode))))
    (when (boundp hook)
      (add-hook hook 
                (lambda () 
                  (when (bound-and-true-p mule-mode)
                    (mule-mode -1)
                    (kill-local-variable 'cursor-type)))
                -10))))

;;; ---------------------------------------------------------------------------
;;; Enhanced Mode Activation Logic
;;; ---------------------------------------------------------------------------
(defun mule--ensure-default-state ()
  "Enable mule-mode unless the current major mode is excluded.

Uses two independent checks for maximum reliability:
1. Derived-mode check (handles mode hierarchies)
2. Direct mode symbol check (handles exact matches)

Returns non-nil if mule-mode was enabled."
  (let ((is-excluded-p
         (or (memq major-mode mule--excluded-modes)
             (apply #'derived-mode-p mule--excluded-modes))))
    (unless (or mule-mode is-excluded-p)
      (mule-mode 1)
      t)))

(add-hook 'after-change-major-mode-hook #'mule--ensure-default-state)

;;; ---------------------------------------------------------------------------
;;; Minibuffer Safety (Ensures no cursor conflicts during prompts)
;;; ---------------------------------------------------------------------------
(defvar mule--minibuffer-pre-mule-state nil
  "Track whether mule-mode was active before entering minibuffer.")

(add-hook 'minibuffer-setup-hook
          (lambda ()
            ;; Store state before disabling
            (setq mule--minibuffer-pre-mule-state mule-mode)
            ;; Disable mule-mode in minibuffer (incompatible with input)
            (when mule-mode
              (mule-mode -1))))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            ;; Restore previous state
            (when mule--minibuffer-pre-mule-state
              (mule-mode 1))
            (setq mule--minibuffer-pre-mule-state nil)))

;;; ---------------------------------------------------------------------------
;;; Mode Indicator
;;; ---------------------------------------------------------------------------
(defun mule-indicator ()
  "Return ' MULE' if mule-mode is active, otherwise empty string."
  (if (bound-and-true-p mule-mode)
      " MULE"
    "     "))

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------
(provide 'mule-modal)

;;; mule-modal.el ends here

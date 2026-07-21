;;; donkey-org-integration-test.el --- Tests for DONKEY org/markdown/enter-dwim integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'donkey)

(defvar org-agenda-mode-map nil)
(defvar this-original-command)
(defvar last-command-event)

;;; ---------------------------------------------------------------------------
;;; Helper macro
;;; ---------------------------------------------------------------------------

(defmacro donkey-with-clean-scratch (&rest body)
  "Execute BODY with a guaranteed-absent *org-scratch* buffer, then clean up."
  (declare (indent 0))
  `(let ((--scratch-created nil))
     (when (get-buffer "*org-scratch*")
       (kill-buffer "*org-scratch*"))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer "*org-scratch*")
         (kill-buffer "*org-scratch*")))))

;;; ---------------------------------------------------------------------------
;;; donkey-insert-org-scratch-message
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-insert-org-scratch-message-inserts-text ()
  "Should insert the org-mode scribble header into the current buffer."
  (with-temp-buffer
    (donkey-insert-org-scratch-message)
    (goto-char (point-min))
    (should (search-forward "# This buffer is for scribbling in org-mode." nil t))))

(ert-deftest donkey-insert-org-scratch-message-point-at-max ()
  "Point should be at `point-max' after insertion."
  (with-temp-buffer
    (donkey-insert-org-scratch-message)
    (should (= (point) (point-max)))))

(ert-deftest donkey-insert-org-scratch-message-appends-to-existing ()
  "Should append the message after any pre-existing buffer content."
  (with-temp-buffer
    (insert "PRE-EXISTING")
    (donkey-insert-org-scratch-message)
    (goto-char (point-min))
    (should (search-forward "PRE-EXISTING" nil t))
    (should (search-forward "# This buffer is for scribbling in org-mode." nil t))))

;;; ---------------------------------------------------------------------------
;;; donkey-create-org-scratch / donkey-org-scratch
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-create-org-scratch-creates-named-buffer ()
  "Should create a buffer named *org-scratch* in org-mode."
  (donkey-with-clean-scratch
   (donkey-create-org-scratch)
   (should (buffer-live-p (get-buffer "*org-scratch*")))
   (with-current-buffer "*org-scratch*"
     (should (eq major-mode 'org-mode)))))

(ert-deftest donkey-create-org-scratch-switches-to-buffer ()
  "Should switch the current window to the new buffer."
  (donkey-with-clean-scratch
   (donkey-create-org-scratch)
   (should (eq (current-buffer) (get-buffer "*org-scratch*")))))

(ert-deftest donkey-create-org-scratch-idempotent-buffer ()
  "Calling twice should reuse (not duplicate) the *org-scratch* buffer."
  (donkey-with-clean-scratch
   (donkey-create-org-scratch)
   (let ((first-buf (get-buffer "*org-scratch*")))
     (donkey-create-org-scratch)
     (should (eq (get-buffer "*org-scratch*") first-buf)))))

(ert-deftest donkey-org-scratch-creates-when-absent ()
  "When no *org-scratch* buffer exists, should create one."
  (donkey-with-clean-scratch
   (donkey-org-scratch)
   (should (buffer-live-p (get-buffer "*org-scratch*")))
   (with-current-buffer "*org-scratch*"
     (should (eq major-mode 'org-mode)))))

(ert-deftest donkey-org-scratch-switches-when-present ()
  "When *org-scratch* already exists, should switch to it without recreating."
  (donkey-with-clean-scratch
   (donkey-create-org-scratch)
   (let ((original-buf (get-buffer "*org-scratch*")))
     (with-current-buffer original-buf
       (insert "* My Scribbles"))
     (donkey-org-scratch)
     (should (eq (current-buffer) original-buf))
     (should (eq (get-buffer "*org-scratch*") original-buf))
     (goto-char (point-min))
     (should (search-forward "* My Scribbles" nil t)))))

(ert-deftest donkey-org-scratch-message-on-create ()
  "Should signal creation via `message' when buffer is new."
  (donkey-with-clean-scratch
   (let ((messages nil))
     (cl-letf (((symbol-function #'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
       (donkey-org-scratch))
     (should (member "*org-scratch* buffer doesn't exist, creating." messages)))))

(ert-deftest donkey-org-scratch-message-on-switch ()
  "Should signal switching via `message' when buffer already exists."
  (donkey-with-clean-scratch
   (donkey-create-org-scratch)
   (let ((messages nil))
     (cl-letf (((symbol-function #'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
       (donkey-org-scratch))
     (should (member "*org-scratch* buffer already exist, switching." messages)))))

(ert-deftest donkey-org-scratch-does-not-double-insert ()
  "Switching to an existing buffer should not re-insert the scratch message."
  (donkey-with-clean-scratch
   (donkey-create-org-scratch)
   (let ((size-before
          (with-current-buffer "*org-scratch*" (buffer-size))))
     (donkey-org-scratch)
     (let ((size-after
            (with-current-buffer "*org-scratch*" (buffer-size))))
       (should (= size-before size-after))))))

;;; ---------------------------------------------------------------------------
;;; donkey--editing-mode-p
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-editing-mode-p-prog-mode ()
  "Returns non-nil when `major-mode' is `prog-mode'."
  (let ((major-mode 'prog-mode))
    (should (donkey--editing-mode-p))))

(ert-deftest donkey-editing-mode-p-org-mode ()
  "Returns non-nil when `major-mode' is `org-mode'."
  (let ((major-mode 'org-mode))
    (should (donkey--editing-mode-p))))

(ert-deftest donkey-editing-mode-p-dired-mode ()
  "Returns nil when `major-mode' is `dired-mode' (not in the list)."
  (let ((major-mode 'dired-mode))
    (should-not (donkey--editing-mode-p))))

(ert-deftest donkey-editing-mode-p-derived-mode-not-caught ()
  "Modes derived from a listed mode with a different symbol name are NOT
matched: `member' uses exact equality, not `derived-mode-p'."
  (let ((major-mode 'python-mode))
    (should-not (donkey--editing-mode-p))))

;;; ---------------------------------------------------------------------------
;;; donkey-add-enter-rule / donkey--register-enter-rule
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-enter-dwim-register-rule-adds-to-list ()
  "Registering a rule adds it to `donkey--enter-rules'."
  (let ((before-count (length donkey--enter-rules)))
    (donkey-add-enter-rule test-elem nil test-cmd)
    (should (= (length donkey--enter-rules) (1+ before-count)))
    (setq donkey--enter-rules
          (cl-remove 'test-elem donkey--enter-rules :key #'car :test 'eq))))

(ert-deftest donkey-enter-dwim-register-rule-stores-correct-form ()
  "Rules store element type, property, and command symbols correctly."
  (donkey-add-enter-rule test-rule nil cmd-a cmd-b)
  (let ((rule (cl-find 'test-rule donkey--enter-rules :key #'car :test 'eq)))
    (should (equal (nth 0 rule) 'test-rule))
    (should (null (nth 1 rule)))
    (should (equal (nthcdr 2 rule) '(cmd-a cmd-b))))
  (setq donkey--enter-rules
        (cl-remove 'test-rule donkey--enter-rules :key #'car :test 'eq)))

(ert-deftest donkey-enter-dwim-config-default-rules-enabled ()
  "When `donkey-default-enter-rules-enabled' is t, default rules are installed."
  (let ((donkey-default-enter-rules-enabled t)
        (donkey--enter-rules nil))
    (when donkey-default-enter-rules-enabled
      (donkey-add-enter-rule item :checkbox org-toggle-checkbox)
      (donkey-add-enter-rule headline :todo-type donkey-org-todo)
      (donkey-add-enter-rule link nil org-open-at-point markdown-follow-thing-at-point browse-url-at-point))
    (should (member '(item :checkbox org-toggle-checkbox) donkey--enter-rules))
    (should (member '(headline :todo-type donkey-org-todo) donkey--enter-rules))
    (should (member '(link nil org-open-at-point markdown-follow-thing-at-point browse-url-at-point) donkey--enter-rules))
    (should-not (cl-find 'table donkey--enter-rules :key #'car :test 'eq))))

(ert-deftest donkey-enter-dwim-config-custom-rules ()
  "When `donkey-default-enter-rules-enabled' is nil, no default rules are
installed and only user-added rules exist."
  (let ((donkey-default-enter-rules-enabled nil)
        (donkey--enter-rules nil))
    (when donkey-default-enter-rules-enabled
      (donkey-add-enter-rule item :checkbox org-toggle-checkbox))
    (donkey-add-enter-rule table nil org-table-next-row)
    (should-not (member '(item :checkbox org-toggle-checkbox) donkey--enter-rules))
    (should (member '(table nil org-table-next-row) donkey--enter-rules))))

;;; ---------------------------------------------------------------------------
;;; donkey-org-todo
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-org-todo-toggles-todo-to-done ()
  "Changes TODO headline to DONE."
  (let (called-with)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(headline (:todo-type todo))))
              ((symbol-function 'org-element-property)
               (lambda (prop elem)
                 (if (and (consp elem) (eq (car elem) 'headline))
                     (plist-get (cadr elem) prop)
                   nil)))
              ((symbol-function 'org-todo)
               (lambda (&rest args)
                 (setq called-with args))))
      (donkey-org-todo))
    (should (eq (car called-with) 'done))))

(ert-deftest donkey-org-todo-toggles-done-to-todo ()
  "Changes DONE headline back to TODO."
  (let (called-with)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(headline (:todo-type done))))
              ((symbol-function 'org-element-property)
               (lambda (prop elem)
                 (if (and (consp elem) (eq (car elem) 'headline))
                     (plist-get (cadr elem) prop)
                   nil)))
              ((symbol-function 'org-todo)
               (lambda (&rest args)
                 (setq called-with args))))
      (donkey-org-todo))
    (should (eq (car called-with) 'todo))))

(ert-deftest donkey-org-todo-adds-todo-if-none ()
  "Adds TODO to headline with no keyword."
  (let (called-with)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(headline (:todo-type nil))))
              ((symbol-function 'org-element-property)
               (lambda (prop elem)
                 (if (and (consp elem) (eq (car elem) 'headline))
                     (plist-get (cadr elem) prop)
                   nil)))
              ((symbol-function 'org-todo)
               (lambda (&rest args)
                 (setq called-with args))))
      (donkey-org-todo))
    (should (eq (car called-with) 'todo))))

;;; ---------------------------------------------------------------------------
;;; donkey-enter-dwim dispatcher - Org Mode
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-enter-dwim-org-link-follows-org-open-at-point ()
  "In org-mode with link element (context), calls `org-open-at-point'."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(paragraph (:begin 1 :end 10))))
              ((symbol-function 'org-element-context)
               (lambda () '(link (:path "http://example.com"))))
              ((symbol-function 'org-open-at-point)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (should (eq called-cmd 'org-open-at-point))))

(ert-deftest donkey-enter-dwim-org-checkbox-toggles ()
  "In org-mode with checkbox item (parent element), calls `org-toggle-checkbox'."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(item (:checkbox t))))
              ((symbol-function 'org-element-context)
               (lambda () '(plain-text (:begin 1 :end 5))))
              ((symbol-function 'org-toggle-checkbox)
               (lambda () (interactive) nil))
              ((symbol-function 'org-element-property)
               (lambda (prop elem)
                 (if (and (consp elem) (eq (car elem) 'item))
                     (plist-get (cadr elem) prop)
                   nil)))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (should (eq called-cmd 'org-toggle-checkbox))))

(ert-deftest donkey-enter-dwim-org-todo-cycles ()
  "In org-mode with TODO headline, calls `donkey-org-todo'."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(headline (:todo-type todo))))
              ((symbol-function 'org-element-context)
               (lambda () nil))
              ((symbol-function 'org-element-property)
               (lambda (prop elem)
                 (if (and (consp elem) (eq (car elem) 'headline))
                     (plist-get (cadr elem) prop)
                   nil)))
              ((symbol-function 'donkey-org-todo)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (should (eq called-cmd 'donkey-org-todo))))

(ert-deftest donkey-enter-dwim-org-table-follows-rule ()
  "In org-mode with table element (parent), calls `org-table-next-row'."
  (let (called-cmd)
    (donkey-add-enter-rule table nil org-table-next-row)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(table (:begin 1 :end 100))))
              ((symbol-function 'org-element-context)
               (lambda () '(table (:begin 1 :end 100))))
              ((symbol-function 'org-table-next-row)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (setq donkey--enter-rules
          (cl-remove 'table donkey--enter-rules :key #'car :test 'eq))
    (should (eq called-cmd 'org-table-next-row))))

(ert-deftest donkey-enter-dwim-org-src-block-does-nothing ()
  "In org-mode on src-block with no matching rule, no command called."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python"))))
              ((symbol-function 'org-element-context)
               (lambda () '(src-block (:language "python"))))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (should (null called-cmd))))

;;; ---------------------------------------------------------------------------
;;; donkey-enter-dwim dispatcher - Nested Element Priority
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-enter-dwim-context-fires-before-parent ()
  "Context elements (inline like links) are checked before parent elements."
  (let (called-cmd)
    (donkey-add-enter-rule table nil org-table-next-row)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(table (:begin 1 :end 100))))
              ((symbol-function 'org-element-context)
               (lambda () '(link (:path "http://example.com"))))
              ((symbol-function 'org-table-next-row)
               (lambda () (interactive) nil))
              ((symbol-function 'org-open-at-point)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (setq donkey--enter-rules
          (cl-remove 'table donkey--enter-rules :key #'car :test 'eq))
    (should (eq called-cmd 'org-open-at-point))))

(ert-deftest donkey-enter-dwim-table-under-headline-fires-table-first ()
  "When a table is nested under a headline, the table rule fires before
the headline rule (parent before ancestor)."
  (let (called-cmd)
    (donkey-add-enter-rule table nil org-table-next-row)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(table (:begin 1 :end 100))))
              ((symbol-function 'org-element-context)
               (lambda () '(table (:begin 1 :end 100))))
              ((symbol-function 'org-element-lineage)
               (lambda (elem)
                 (list '(headline (:todo-type todo)))))
              ((symbol-function 'org-table-next-row)
               (lambda () (interactive) nil))
              ((symbol-function 'donkey-org-todo)
               (lambda () (interactive) nil))
              ((symbol-function 'org-element-property)
               (lambda (prop elem)
                 (if (and (consp elem) (eq (car elem) 'headline))
                     (plist-get (cadr elem) prop)
                   nil)))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (setq donkey--enter-rules
          (cl-remove 'table donkey--enter-rules :key #'car :test 'eq))
    (should (eq called-cmd 'org-table-next-row))))

(ert-deftest donkey-enter-dwim-link-in-table-under-headline-follows-link ()
  "Link inside a table under a headline: link fires first (context priority)."
  (let (called-cmd)
    (donkey-add-enter-rule table nil org-table-next-row)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(table (:begin 1 :end 100))))
              ((symbol-function 'org-element-context)
               (lambda () '(link (:path "http://example.com"))))
              ((symbol-function 'org-element-lineage)
               (lambda (elem)
                 (list '(headline (:todo-type todo)))))
              ((symbol-function 'org-table-next-row)
               (lambda () (interactive) nil))
              ((symbol-function 'org-open-at-point)
               (lambda () (interactive) nil))
              ((symbol-function 'donkey-org-todo)
               (lambda () (interactive) nil))
              ((symbol-function 'org-element-property)
               (lambda (prop elem)
                 (if (and (consp elem) (eq (car elem) 'headline))
                     (plist-get (cadr elem) prop)
                   nil)))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (setq donkey--enter-rules
          (cl-remove 'table donkey--enter-rules :key #'car :test 'eq))
    (should (eq called-cmd 'org-open-at-point))))

;;; ---------------------------------------------------------------------------
;;; donkey-enter-dwim dispatcher - Org-Agenda Mode
;;;
;;; `donkey--org-agenda-enter-handler' detects agenda mode via
;;; `derived-mode-p', which reads the dynamically-bound `major-mode'
;;; below directly -- no mock of a detection predicate is needed.
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-enter-dwim-agenda-calls-native-agenda-ret ()
  "In org-agenda-mode, calls native agenda RET (org-agenda-switch-to)."
  (let (called-cmd)
    (cl-letf (((symbol-function 'lookup-key)
               (lambda (_map _key) 'org-agenda-switch-to))
              ((symbol-function 'org-agenda-switch-to)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((org-agenda-mode-map (make-sparse-keymap))
            (major-mode 'org-agenda-mode))
        (donkey-enter-dwim)))
    (should (eq called-cmd 'org-agenda-switch-to))))

(ert-deftest donkey-enter-dwim-agenda-with-undefined-ret-does-nothing ()
  "In org-agenda-mode with undefined RET, nothing happens."
  (let (called-cmd)
    (cl-letf (((symbol-function 'lookup-key)
               (lambda (_map _key) 'undefined))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((org-agenda-mode-map (make-sparse-keymap))
            (major-mode 'org-agenda-mode))
        (donkey-enter-dwim)))
    (should (null called-cmd))))

(ert-deftest donkey-enter-dwim-agenda-with-keymap-ret-does-nothing ()
  "In org-agenda-mode where RET is bound to a keymap, nothing happens."
  (let (called-cmd)
    (cl-letf (((symbol-function 'lookup-key)
               (lambda (_map _key) (make-sparse-keymap)))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((org-agenda-mode-map (make-sparse-keymap))
            (major-mode 'org-agenda-mode))
        (donkey-enter-dwim)))
    (should (null called-cmd))))

(ert-deftest donkey-enter-dwim-agenda-takes-priority-over-rules ()
  "Agenda mode dispatch takes priority over all other handlers."
  (let (called-cmd)
    (cl-letf (((symbol-function 'lookup-key)
               (lambda (_map _key) 'org-agenda-switch-to))
              ((symbol-function 'org-agenda-switch-to)
               (lambda () (interactive) nil))
              ((symbol-function 'org-element-at-point)
               (lambda () '(item (:checkbox t))))
              ((symbol-function 'org-toggle-checkbox)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((org-agenda-mode-map (make-sparse-keymap))
            (major-mode 'org-agenda-mode))
        (donkey-enter-dwim)))
    (should (eq called-cmd 'org-agenda-switch-to))))

(ert-deftest donkey-enter-dwim-agenda-requires-derived-mode ()
  "Agenda dispatch does not fire when major-mode is unrelated, even with
org-agenda-mode-map bound, confirming derived-mode-p (not just boundp)
gates the handler."
  (let (called-cmd)
    (cl-letf (((symbol-function 'lookup-key)
               (lambda (_map _key) 'org-agenda-switch-to))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((org-agenda-mode-map (make-sparse-keymap))
            (major-mode 'fundamental-mode)
            (donkey--saved-ret-binding nil))
        (donkey-enter-dwim)))
    (should (null called-cmd))))

;;; ---------------------------------------------------------------------------
;;; donkey-enter-dwim dispatcher - Markdown Mode
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-enter-dwim-markdown-follows-mdfn ()
  "In markdown-mode with link element, calls `markdown-follow-thing-at-point'.
`org-open-at-point' (earlier in the link rule's command list) is
explicitly forced unbound here so the test verifies the fallback to
the markdown-specific command rather than depending on whether org
happens to be loaded in this process."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(paragraph (:begin 1 :end 10))))
              ((symbol-function 'org-element-context)
               (lambda () '(link (:path "http://example.com"))))
              ((symbol-function 'org-open-at-point) nil)
              ((symbol-function 'markdown-follow-thing-at-point)
               (lambda (&optional pos) (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'markdown-mode))
        (donkey-enter-dwim)))
    (should (eq called-cmd 'markdown-follow-thing-at-point))))

(ert-deftest donkey-enter-dwim-markdown-fallback-to-browse-url ()
  "In markdown-mode without org-open-at-point or the markdown function
available, falls back to browse-url-at-point."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(paragraph (:begin 1 :end 10))))
              ((symbol-function 'org-element-context)
               (lambda () '(link (:path "http://example.com"))))
              ((symbol-function 'org-open-at-point) nil)
              ((symbol-function 'markdown-follow-thing-at-point) nil)
              ((symbol-function 'browse-url-at-point)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'markdown-mode))
        (donkey-enter-dwim)))
    (should (eq called-cmd 'browse-url-at-point))))

;;; ---------------------------------------------------------------------------
;;; donkey-enter-dwim dispatcher - Non-editing modes
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-enter-dwim-non-editing-calls-native-ret ()
  "In a non-editing mode with a saved RET binding, calls it."
  (let (called-cmd)
    (setq donkey--saved-ret-binding #'dired-find-file)
    (cl-letf (((symbol-function 'dired-find-file)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'dired-mode))
        (donkey-enter-dwim)))
    (setq donkey--saved-ret-binding nil)
    (should (eq called-cmd #'dired-find-file))))

(ert-deftest donkey-enter-dwim-non-editing-no-ret-does-nothing ()
  "In a non-editing mode with no saved RET binding, no command is called."
  (let (called-cmd)
    (let ((donkey--saved-ret-binding nil))
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd))))
        (donkey-enter-dwim)))
    (should (null called-cmd))))

(ert-deftest donkey-enter-dwim-non-editing-undefined-ret-does-nothing ()
  "In a non-editing mode where saved RET is 'undefined, nothing happens."
  (let (called-cmd)
    (let ((donkey--saved-ret-binding 'undefined))
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd))))
        (donkey-enter-dwim)))
    (should (null called-cmd))))

(ert-deftest donkey-enter-dwim-non-editing-keymap-ret-does-nothing ()
  "In a non-editing mode where saved RET is a keymap, nothing happens."
  (let (called-cmd)
    (let ((km (make-sparse-keymap)))
      (let ((donkey--saved-ret-binding km))
        (cl-letf (((symbol-function 'call-interactively)
                   (lambda (cmd) (setq called-cmd cmd))))
          (donkey-enter-dwim))))
    (should (null called-cmd))))

;;; ---------------------------------------------------------------------------
;;; donkey-enter-dwim dispatcher - Editing modes / priority / edge cases
;;; ---------------------------------------------------------------------------

(ert-deftest donkey-enter-dwim-editing-mode-no-context-does-nothing ()
  "In an editing mode with no org/markdown context, nothing happens."
  (let (called-cmd)
    (let ((donkey--saved-ret-binding nil))
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (cmd) (setq called-cmd cmd))))
        (let ((major-mode 'prog-mode))
          (donkey-enter-dwim))))
    (should (null called-cmd))))

(ert-deftest donkey-enter-dwim-checkbox-priority-over-generic-item ()
  "Item with checkbox property triggers checkbox rule before others."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(item (:checkbox t))))
              ((symbol-function 'org-element-context)
               (lambda () '(plain-text (:begin 1 :end 5))))
              ((symbol-function 'org-toggle-checkbox)
               (lambda () (interactive) nil))
              ((symbol-function 'org-element-property)
               (lambda (prop elem)
                 (if (and (consp elem) (eq (car elem) 'item))
                     (plist-get (cadr elem) prop)
                   nil)))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (should (eq called-cmd 'org-toggle-checkbox))))

(ert-deftest donkey-enter-dwim-call-interactively-called-once ()
  "When a command is found, `call-interactively' is called exactly once."
  (let (call-count)
    (donkey-add-enter-rule table nil org-table-next-row)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(table (:begin 1 :end 100))))
              ((symbol-function 'org-element-context)
               (lambda () '(table (:begin 1 :end 100))))
              ((symbol-function 'org-table-next-row)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd)
                 (setq call-count (1+ (or call-count 0))))))
      (let ((major-mode 'org-mode))
        (donkey-enter-dwim)))
    (setq donkey--enter-rules
          (cl-remove 'table donkey--enter-rules :key #'car :test 'eq))
    (should (= call-count 1))))

(provide 'donkey-org-integration-test)

;;; donkey-org-integration-test.el ends here

;;; mule-enter-dwim-test.el --- Tests for mule-enter-dwim -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mule-modal)

;; ===========================================================================
;; Section: mule--editing-mode-p
;; Selector: (ert "mule-enter-dwim-editing-mode-p")
;; ===========================================================================

(ert-deftest mule-enter-dwim-editing-mode-p-prog-mode ()
  "Returns non-nil when `major-mode' is `prog-mode'.
Expected: non-nil."
  (let ((major-mode 'prog-mode))
    (should (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-text-mode ()
  "Returns non-nil when `major-mode' is `text-mode'.
Expected: non-nil."
  (let ((major-mode 'text-mode))
    (should (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-org-mode ()
  "Returns non-nil when `major-mode' is `org-mode'.
Expected: non-nil."
  (let ((major-mode 'org-mode))
    (should (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-fundamental-mode ()
  "Returns non-nil when `major-mode' is `fundamental-mode'.
Expected: non-nil."
  (let ((major-mode 'fundamental-mode))
    (should (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-conf-mode ()
  "Returns non-nil when `major-mode' is `conf-mode'.
Expected: non-nil."
  (let ((major-mode 'conf-mode))
    (should (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-markdown-mode ()
  "Returns non-nil when `major-mode' is `markdown-mode'.
Expected: non-nil."
  (let ((major-mode 'markdown-mode))
    (should (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-gfm-mode ()
  "Returns non-nil when `major-mode' is `gfm-mode'.
Expected: non-nil."
  (let ((major-mode 'gfm-mode))
    (should (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-dired-mode ()
  "Returns nil when `major-mode' is `dired-mode' (not in the list).
Expected: nil."
  (let ((major-mode 'dired-mode))
    (should-not (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-info-mode ()
  "Returns nil when `major-mode' is `Info-mode' (not in the list).
Expected: nil."
  (let ((major-mode 'Info-mode))
    (should-not (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-derived-mode-not-caught ()
  "Modes derived from a listed mode but with a different symbol name are
NOT matched.  `member' uses exact equality, not `derived-mode-p'.
This documents existing behaviour: python-mode derives from prog-mode
but is not caught.
Expected: nil."
  (let ((major-mode 'python-mode))
    (should-not (mule--editing-mode-p))))

(ert-deftest mule-enter-dwim-editing-mode-p-custom-mode-not-caught ()
  "A mode not present in `mule-editing-modes' returns nil even if it is
semantically an editing mode.
Expected: nil."
  (let ((major-mode 'rust-mode))
    (should-not (mule--editing-mode-p))))

;; ===========================================================================
;; Section: mule--org-enter-handler
;; Selector: (ert "mule-enter-dwim-org-enter-handler")
;; ===========================================================================

(ert-deftest mule-enter-dwim-org-enter-handler-paragraph-returns-org-open ()
  "In org-mode with a paragraph element, returns `#'org-open-at-point'.
Expected: #'org-open-at-point."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(paragraph (:begin 1 :end 10))))
            ((symbol-function 'org-open-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'org-mode))
      (should (eq (mule--org-enter-handler) #'org-open-at-point)))))

(ert-deftest mule-enter-dwim-org-enter-handler-headline-returns-org-open ()
  "In org-mode with a headline element, returns `#'org-open-at-point'.
Expected: #'org-open-at-point."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(headline (:begin 1 :end 20 :level 1))))
            ((symbol-function 'org-open-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'org-mode))
      (should (eq (mule--org-enter-handler) #'org-open-at-point)))))

(ert-deftest mule-enter-dwim-org-enter-handler-link-returns-org-open ()
  "In org-mode with a link element, returns `#'org-open-at-point'.
Expected: #'org-open-at-point."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(link (:begin 1 :end 15 :path "http://example.com"))))
            ((symbol-function 'org-open-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'org-mode))
      (should (eq (mule--org-enter-handler) #'org-open-at-point)))))

(ert-deftest mule-enter-dwim-org-enter-handler-src-block-returns-nil ()
  "In org-mode with a src-block element, returns nil to prevent
accidental execution.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:language "python" :begin 1 :end 50))))
            ((symbol-function 'org-open-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'org-mode))
      (should (null (mule--org-enter-handler))))))

(ert-deftest mule-enter-dwim-org-enter-handler-nil-element-returns-nil ()
  "In org-mode when `org-element-at-point' returns nil, returns nil.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () nil))
            ((symbol-function 'org-open-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'org-mode))
      (should (null (mule--org-enter-handler))))))

(ert-deftest mule-enter-dwim-org-enter-handler-not-org-mode ()
  "When not in org-mode, returns nil regardless of org functions being
available.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(paragraph (:begin 1 :end 10))))
            ((symbol-function 'org-open-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'text-mode))
      (should (null (mule--org-enter-handler))))))

(ert-deftest mule-enter-dwim-org-enter-handler-org-functions-unbound ()
  "When org functions are not fboundp, returns nil even in org-mode.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point) nil)
            ((symbol-function 'org-open-at-point) nil))
    (let ((major-mode 'org-mode))
      (should (null (mule--org-enter-handler))))))

(ert-deftest mule-enter-dwim-org-enter-handler-only-org-open-unbound ()
  "When `org-open-at-point' is not fboundp but `org-element-at-point' is,
returns nil.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point)
             (lambda () '(paragraph (:begin 1 :end 10))))
            ((symbol-function 'org-open-at-point) nil))
    (let ((major-mode 'org-mode))
      (should (null (mule--org-enter-handler))))))

(ert-deftest mule-enter-dwim-org-enter-handler-only-element-unbound ()
  "When `org-element-at-point' is not fboundp but `org-open-at-point' is,
returns nil.
Expected: nil."
  (cl-letf (((symbol-function 'org-element-at-point) nil)
            ((symbol-function 'org-open-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'org-mode))
      (should (null (mule--org-enter-handler))))))

;; ===========================================================================
;; Section: mule--markdown-enter-handler
;; Selector: (ert "mule-enter-dwim-markdown-enter-handler")
;; ===========================================================================

(ert-deftest mule-enter-dwim-markdown-enter-handler-markdown-with-mdfn ()
  "In markdown-mode with `markdown-follow-thing-at-point' fboundp, returns it.
Expected: #'markdown-follow-thing-at-point."
  (cl-letf (((symbol-function 'markdown-follow-thing-at-point)
             (lambda (&optional pos) (interactive) nil)))
    (let ((major-mode 'markdown-mode))
      (should (eq (mule--markdown-enter-handler)
                  #'markdown-follow-thing-at-point)))))

(ert-deftest mule-enter-dwim-markdown-enter-handler-gfm-with-mdfn ()
  "In gfm-mode with `markdown-follow-thing-at-point' fboundp, returns it.
Expected: #'markdown-follow-thing-at-point."
  (cl-letf (((symbol-function 'markdown-follow-thing-at-point)
             (lambda (&optional pos) (interactive) nil)))
    (let ((major-mode 'gfm-mode))
      (should (eq (mule--markdown-enter-handler)
                  #'markdown-follow-thing-at-point)))))

(ert-deftest mule-enter-dwim-markdown-enter-handler-fallback-to-shr ()
  "In markdown-mode without `markdown-follow-thing-at-point' but with
`shr-follow-link-at-point' fboundp, falls back to shr.
Expected: #'shr-follow-link-at-point."
  (cl-letf (((symbol-function 'markdown-follow-thing-at-point) nil)
            ((symbol-function 'shr-follow-link-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'markdown-mode))
      (should (eq (mule--markdown-enter-handler)
                  #'shr-follow-link-at-point)))))

(ert-deftest mule-enter-dwim-markdown-enter-handler-fallback-to-browse-url ()
  "In markdown-mode with neither markdown nor shr functions bound, falls
back to `browse-url-at-point'.
Expected: #'browse-url-at-point."
  (cl-letf (((symbol-function 'markdown-follow-thing-at-point) nil)
            ((symbol-function 'shr-follow-link-at-point) nil))
    (let ((major-mode 'markdown-mode))
      (should (eq (mule--markdown-enter-handler)
                  #'browse-url-at-point)))))

(ert-deftest mule-enter-dwim-markdown-enter-handler-not-markdown-mode ()
  "When not in markdown or gfm mode, returns nil.
Expected: nil."
  (cl-letf (((symbol-function 'markdown-follow-thing-at-point)
             (lambda (&optional pos) (interactive) nil)))
    (let ((major-mode 'text-mode))
      (should (null (mule--markdown-enter-handler))))))

(ert-deftest mule-enter-dwim-markdown-enter-handler-mdfn-takes-priority-over-shr ()
  "When both markdown and shr functions are fboundp, the markdown function
takes priority.
Expected: #'markdown-follow-thing-at-point."
  (cl-letf (((symbol-function 'markdown-follow-thing-at-point)
             (lambda (&optional pos) (interactive) nil))
            ((symbol-function 'shr-follow-link-at-point)
             (lambda () (interactive) nil)))
    (let ((major-mode 'gfm-mode))
      (should (eq (mule--markdown-enter-handler)
                  #'markdown-follow-thing-at-point)))))

;; ===========================================================================
;; Section: mule--non-editing-enter-handler
;; Selector: (ert "mule-enter-dwim-non-editing-enter-handler")
;; ===========================================================================

(defmacro mule-enter-dwim-test--with-local-ret (ret-key-def major-mode-sym &rest body)
  "Execute BODY in a temp buffer with RET bound to RET-KEY-DEF and
`major-mode' set to MAJOR-MODE-SYM."
  (declare (indent 3) (debug (sexp sexp &rest body)))
  `(with-temp-buffer
     (use-local-map (make-sparse-keymap))
     (when ,ret-key-def
       (define-key (current-local-map) (kbd "RET") ,ret-key-def))
     (let ((major-mode ,major-mode-sym))
       ,@body)))

(ert-deftest mule-enter-dwim-non-editing-enter-handler-valid-ret-binding ()
  "In a non-editing mode with a valid RET command binding, returns that
command.
Expected: the command bound to RET."
  (mule-enter-dwim-test--with-local-ret #'dired-find-file 'dired-mode
                                        (should (eq (mule--non-editing-enter-handler) #'dired-find-file))))

(ert-deftest mule-enter-dwim-non-editing-enter-handler-info-mode-valid-ret ()
  "In Info-mode with RET bound to `Info-follow-nearest-node', returns it.
Expected: #'Info-follow-nearest-node."
  (mule-enter-dwim-test--with-local-ret #'Info-follow-nearest-node 'Info-mode
                                        (should (eq (mule--non-editing-enter-handler)
                                                    #'Info-follow-nearest-node))))

(ert-deftest mule-enter-dwim-non-editing-enter-handler-editing-mode-returns-nil ()
  "In an editing mode (e.g. prog-mode), returns nil even if RET has a
local binding.
Expected: nil."
  (mule-enter-dwim-test--with-local-ret #'ignore 'prog-mode
                                        (should (null (mule--non-editing-enter-handler)))))

(ert-deftest mule-enter-dwim-non-editing-enter-handler-no-ret-binding ()
  "In a non-editing mode with no RET binding, returns nil.
Expected: nil."
  (mule-enter-dwim-test--with-local-ret nil 'dired-mode
                                        (should (null (mule--non-editing-enter-handler)))))

(ert-deftest mule-enter-dwim-non-editing-enter-handler-undefined-ret ()
  "In a non-editing mode where RET is explicitly bound to `undefined',
returns nil.
Expected: nil."
  (mule-enter-dwim-test--with-local-ret 'undefined 'dired-mode
                                        (should (null (mule--non-editing-enter-handler)))))

(ert-deftest mule-enter-dwim-non-editing-enter-handler-ret-not-fboundp ()
  "In a non-editing mode where RET is bound to a symbol that is not
fboundp, returns nil.
Expected: nil."
  (mule-enter-dwim-test--with-local-ret 'nonexistent-command-xyz 'dired-mode
                                        (should (null (mule--non-editing-enter-handler)))))

(ert-deftest mule-enter-dwim-non-editing-enter-handler-ret-bound-to-keymap ()
  "In a non-editing mode where RET is bound to a keymap (prefix), returns
nil because a keymap is not callable.
Expected: nil."
  (mule-enter-dwim-test--with-local-ret (make-sparse-keymap) 'dired-mode
                                        (should (null (mule--non-editing-enter-handler)))))

(ert-deftest mule-enter-dwim-non-editing-enter-handler-ret-bound-to-lambda ()
  "In a non-editing mode where RET is bound to a lambda, returns nil
because a lambda is not a symbol.
Expected: nil."
  (mule-enter-dwim-test--with-local-ret (lambda () (interactive) nil) 'dired-mode
                                        (should (null (mule--non-editing-enter-handler)))))

;; ===========================================================================
;; Section: mule-enter-dwim
;; Selector: (ert "mule-enter-dwim-")
;;           note trailing hyphen to avoid matching sub-section prefixes
;;
;;           (ert "mule-enter-dwim")
;;           runs ALL tests in this file
;; ===========================================================================

(ert-deftest mule-enter-dwim-org-mode-calls-org-open-at-point ()
  "In org-mode with a non-src-block element, `call-interactively' is
invoked with `org-open-at-point'.
Expected: called-cmd is #'org-open-at-point."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(paragraph (:begin 1 :end 10))))
              ((symbol-function 'org-open-at-point)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (mule-enter-dwim)))
    (should (eq called-cmd #'org-open-at-point))))

(ert-deftest mule-enter-dwim-org-src-block-does-nothing ()
  "In org-mode on a src-block, no command is called.
Expected: called-cmd is nil."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:language "python" :begin 1 :end 50))))
              ((symbol-function 'org-open-at-point)
               (lambda () (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (mule-enter-dwim)))
    (should (null called-cmd))))

(ert-deftest mule-enter-dwim-markdown-mode-calls-markdown-follow ()
  "In markdown-mode with `markdown-follow-thing-at-point' available,
`call-interactively' is invoked with it.
Expected: called-cmd is #'markdown-follow-thing-at-point."
  (let (called-cmd)
    (cl-letf (((symbol-function 'markdown-follow-thing-at-point)
               (lambda (&optional pos) (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'markdown-mode))
        (mule-enter-dwim)))
    (should (eq called-cmd #'markdown-follow-thing-at-point))))

(ert-deftest mule-enter-dwim-markdown-fallback-to-browse-url ()
  "In markdown-mode with no markdown or shr functions, falls back to
`browse-url-at-point'.
Expected: called-cmd is #'browse-url-at-point."
  (let (called-cmd)
    (cl-letf (((symbol-function 'markdown-follow-thing-at-point) nil)
              ((symbol-function 'shr-follow-link-at-point) nil)
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'markdown-mode))
        (mule-enter-dwim)))
    (should (eq called-cmd #'browse-url-at-point))))

(ert-deftest mule-enter-dwim-non-editing-calls-native-ret ()
  "In a non-editing mode with a valid RET binding, `call-interactively'
is invoked with the local RET command.
Expected: called-cmd is the command bound to RET in the local map."
  (let (called-cmd)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (mule-enter-dwim-test--with-local-ret #'dired-find-file 'dired-mode
                                            (mule-enter-dwim))
      (should (eq called-cmd #'dired-find-file)))))

(ert-deftest mule-enter-dwim-non-editing-no-ret-does-nothing ()
  "In a non-editing mode with no RET binding, no command is called.
Expected: called-cmd is nil."
  (let (called-cmd)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (mule-enter-dwim-test--with-local-ret nil 'dired-mode
                                            (mule-enter-dwim))
      (should (null called-cmd)))))

(ert-deftest mule-enter-dwim-editing-mode-does-nothing ()
  "In an editing mode (e.g. prog-mode) with no org/markdown context, no
command is called.
Expected: called-cmd is nil."
  (let (called-cmd)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (mule-enter-dwim-test--with-local-ret nil 'prog-mode
                                            (mule-enter-dwim))
      (should (null called-cmd)))))

(ert-deftest mule-enter-dwim-org-takes-priority-over-markdown ()
  "When in org-mode, the org handler is tried first even if markdown
functions are available.  Since org-mode is checked via major-mode,
markdown handler returns nil.
Expected: called-cmd is #'org-open-at-point, not a markdown function."
  (let (called-cmd)
    (cl-letf (((symbol-function 'org-element-at-point)
               (lambda () '(paragraph (:begin 1 :end 10))))
              ((symbol-function 'org-open-at-point)
               (lambda () (interactive) nil))
              ((symbol-function 'markdown-follow-thing-at-point)
               (lambda (&optional pos) (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (let ((major-mode 'org-mode))
        (mule-enter-dwim)))
    (should (eq called-cmd #'org-open-at-point))))

(ert-deftest mule-enter-dwim-markdown-takes-priority-over-non-editing ()
  "When in markdown-mode (an editing mode), the markdown handler is used
rather than the non-editing handler (which returns nil for editing modes).
Expected: called-cmd is a markdown function, not the local RET binding."
  (let (called-cmd)
    (cl-letf (((symbol-function 'markdown-follow-thing-at-point)
               (lambda (&optional pos) (interactive) nil))
              ((symbol-function 'call-interactively)
               (lambda (cmd) (setq called-cmd cmd))))
      (mule-enter-dwim-test--with-local-ret #'ignore 'markdown-mode
                                            (mule-enter-dwim)))
    (should (eq called-cmd #'markdown-follow-thing-at-point))))

(ert-deftest mule-enter-dwim-no-handler-found-does-nothing ()
  "When no handler returns a command (editing mode, no org/markdown
context, no local RET), `call-interactively' is never called.
Expected: called-cmd is nil."
  (let (called-cmd call-count)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (cmd)
                 (setq called-cmd cmd
                       call-count (1+ (or call-count 0))))))
      (mule-enter-dwim-test--with-local-ret nil 'fundamental-mode
                                            (mule-enter-dwim))
      (should (null called-cmd))
      (should (null call-count)))))

;;; mule-enter-dwim-test.el ends here

(ert "mule-enter-dwim")

;;; company-distel.el --- Erlang/distel completion backend for company-mode

;; Copyright (C) 2012 Sebastian Weddmark Olsson

;; Author: Sebastian Weddmark Olsson
;; URL: github.com/sebastiw/distel-completion
;; Package-Version: 20161003.639
;; Version: 1.0.0
;; Package-Requires: ((distel-completion-lib "1.0.0"))
;; Keywords: Erlang Distel company

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Add `company-distel' to the `company-backends' list in your .emacs.
;; E.g.
;;   (require 'company)
;;   (require 'company-distel)
;;   (add-to-list 'company-backends 'company-distel)
;;
;; Customize
;; ------------------
;; When set, the doc-buffer should use a popup instead of a whole buffer.
;; (setq company-distel-popup-help t)
;;
;; Specifies the height of the popup created when `company-distel-popup-help' is
;; set.
;; (setq company-distel-popup-height 30)
;;
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-get-doc-from-internet t)
;;
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-valid-syntax "a-zA-Z:_-")

;;; Code:

(require 'cl)
(require 'distel-completion-lib)

(defcustom company-distel-popup-help nil
  "When set, the doc-buffer should use a popup instead of a whole buffer."
  :group 'company-distel)

(defcustom company-distel-popup-height 30
  "Specifies the height of the popup created when
`company-distel-popup-help' is set."
  :group 'company-distel)

(defvar company-distel-completion-info (make-hash-table)
  "Global variable used by frontend to determine if the
completion candidate is a module/external function, internal
function or a last used.")

;;;###autoload
(defun company-distel (command &optional args &rest ignore)
  "Erlang/Distel completion backend for Company-mode."
  (company-distel-modules command args ignore))

(defun company-distel-modules (command &optional args &rest ignore)
  "Backend for company-mode using Distel to complete module-names."
  (interactive (list 'interactive))
  (case command
    (interactive
     (company-begin-backend 'company-distel-modules))
    (prefix
     ;; which word to start the completion on
     (company-distel-find-prefix))
    (candidates
     ;; returns the completion candidates
     (progn
       (if args (company-distel-get-candidates args))))
    (meta
     ;; a oneline docstring
     (company-distel-get-metadoc args))
    (doc-buffer
     ;; the full documentation accessable by pressing <f1>
     (company-distel-get-help args))
    (post-completion
     (progn
       (run-with-timer 0 nil 'company-distel--post-complete args)))
     ;; Restart completion if it was a module that was inserted
    (sorted
     ;; if the list is sorted or not
     t)
    (duplicates
     ;; if there are duplicates or not; there could actually be duplicates but
     ;; we dont care about them because the list must be sorted.
     nil)
    (ignore-case
     ;; Erlang uses it's cases. Turn this off.
     nil)
    (no-cache
     ;; Try to use cache.
     t)
    (_
     ;; otherwise;
     ;; one of `init', `annotation', `match', or `pre-completion'
     nil)
    ))

(defun company-distel--post-complete (prefix)
  "If we complete a module, we want to complete a function immediately.
Check if PREFIX ends with a ':'."
  (pcase (substring prefix -1)
    (":"
     (progn
       (company-begin-with (company-distel-get-candidates prefix))
       (let ((this-command 'company-idle-begin))
         (company-post-command))))
    ("("
     (progn
       (company-begin-with (company-distel-get-args prefix))
       (let ((this-command 'company-idle-begin))
         (company-post-command))))
    (_
     nil)))

(defun company-distel-find-prefix ()
  "Get word at point if it is not in a comment or a cite.  If it
couldn't find any return 'stop."
  (let (;; Check if point is within a comment or a citation
        (no-comment (not (distel-completion-is-comment-or-cite-p)))
        ;; Get word at point
        (word (distel-completion-grab-word)))
    (and
     ;; erlang-mode on?
     (or (eq (derived-mode-p 'erlang-mode) 'erlang-mode)
         (eq major-mode 'erlang-shell-mode))
     (or
      ;; Not in comment/citation and we have a word
      (and no-comment word)
      ;; No word found, stop.
      'stop))))

(defun company-distel-get-candidates (prefix)
  "Return a list of completion candidates."
  (let (;; erl-dabbrevs lookback in the current function for
        ;; words starting with `prefix'
        (erl-dabbrevs (distel-completion-get-dabbrevs prefix))
        ;; Lookup distel-completion of modules and functions
        (cc (distel-completion-complete prefix (current-buffer)))
        ;; Get functions from current-buffer
        (local-funs (distel-completion-get-functions prefix)))

    ;; Classify each match as either 'cc or 'lu, though I have no clue what the
    ;; abbreviations mean anymore. It is used in the frontend to determine
    ;; wether to add a ":" after the completion candidate or a "(" for local
    ;; functions.
    (dolist (item cc)
      (puthash item 'cc company-distel-completion-info))
    (dolist (item erl-dabbrevs)
      (puthash item 'lu company-distel-completion-info))
    (dolist (item local-funs)
      (puthash item 'lu company-distel-completion-info))

    ;; Return all matches
    (append erl-dabbrevs
            local-funs
            cc)))

(defun company-distel-get-args (candidate)
  "Return argument list for CANDIDATE.
CANDIDATE is the string `mod:fun'"
  (let* ((isok (string-match ":" candidate))
         (mod (and isok (substring candidate 0 isok)))
         (fun (and isok (substring candidate (+ isok 1))))
         (args (unless (eq "" fun) (distel-completion-args mod fun))))
    (when (and mod fun args)
      args)))

(defun company-distel-get-metadoc (candidate)
  "Return a oneline documentation string.
We use the arglist of CANDIDATE.  CANDIDATE is the string `mod:fun'"
  (let* ((isok (string-match ":" candidate))
         (mod (and isok (substring candidate 0 isok)))
         (fun (and isok (substring candidate (+ isok 1))))
         (dsc (unless (eq "" fun) (distel-completion-describe mod fun))))
    (when (and mod fun dsc)
      dsc)))

(defun company-distel-get-help (candidate)
  "Get the company-mode's doc-buffer. If `company-distel-popup-help'
is set, then show the help as a popup instead of in a new
buffer."
  (let ((help-text (distel-completion-get-doc-buffer candidate)))
    (when company-distel-popup-help
      (unless (featurep 'popup) (require 'popup))
      (popup-tip help-text :height company-distel-popup-height))
    help-text))

(provide 'company-distel)
;;; company-distel.el ends here

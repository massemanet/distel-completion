;;; distel-completion-lib.el --- Completion library for Erlang/Distel

;; Copyright (C) 2012 Sebastian Weddmark Olsson

;; Author: Sebastian Weddmark Olsson
;; URL: github.com/sebastiw/distel-completion
;; Version: 1.0.0
;; Keywords: Erlang Distel completion

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Completion library for Erlang using Distel as backend.  If you are interested
;; to see how it can be used you can checkout the packages
;; `auto-complete-distel' or `company-distel'
;; (github.com/sebastiw/distel-completion).
;;
;; This library can find documentation and do Distel request.  Use it to build
;; new completion modules.

;;; Code:

(require 'distel)

(defcustom distel-completion-get-doc-from-internet t
  "Try to find the documentation from erlang.org"
  :group 'distel-completion)

(defcustom distel-completion-valid-syntax "a-zA-Z:_-"
  "Which syntax to skip backwards to find start of word."
  :group 'distel-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; docs funs         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun distel-completion-get-doc-buffer (args)
  "Returns a buffer with the documentation for ARGS."
  (with-current-buffer (company-doc-buffer)
    (insert (distel-completion-get-doc-string args))
    (current-buffer)))

(defun distel-completion-get-doc-string (args)
  "Returns the documentation string for ARGS."
  (interactive)
  (let* ((isok (string-match ":" args))
	 (mod (substring args 0 isok))
	 (fun (and isok (substring args (+ isok 1))))
	 (doc (and fun (distel-completion-local-docs mod fun)))
	 (edocs (when (and distel-completion-get-doc-from-internet
                           mod
                           (or (not doc) (string= doc "")))
		  (distel-completion-get-docs-from-internet-p mod fun)))
	 (met (and fun (erl-format-arglists (distel-completion-get-metadoc mod fun))))
	 (to-show (or (and (not (string= doc "")) doc)
		      (and (not (string= edocs "")) edocs)
		      (and met (concat mod ":" fun met))
		      (format "Couldn't find any help for %s." args))))
    to-show))

(defun distel-completion-get-docs-from-internet-p (mod fun) ;; maybe version?
  "Download the documentation from internet."
  (let ((str
	 (with-current-buffer 
	     (url-retrieve-synchronously (format "http://www.erlang.org/doc/man/%s.html" mod))
	   (goto-char (point-min))
	   
	   ;; find <p> containing <a name="`FUN'"> then
	   ;; find <div class="REFBODY">
	   (let* ((m (re-search-forward (or (and fun (format "<p>.*?<a name=\"%s.*?\">" fun))
					    "<h3>DESCRIPTION</h3>") nil t))
		  (beg (and m (match-end 0)))
		  (end (and m (progn (re-search-forward "</p>.*?</div>" nil t)
				     (match-end 0)))))
	     (and beg end (buffer-substring beg end))))))
    (and str
	 (distel-completion-html-to-string str))))

(defun distel-completion-html-to-string (string)
  "Removes html-tags from `STRING'."
  (let ((replaces '(("</?p>" . "\n")
		    ("<br>" . "\n")
		    ("<[^>]*>" . "")
		    ("[[:space:]|\n]$" . "")
		    ("^[[:space:]]+" . "")
		    ("^\n[\n]+" . "")
		    ("&gt;" . ">")
		    ("&lt;" . "<"))))
    (dolist (tagpair replaces string)
      (setq string (replace-regexp-in-string (car tagpair) (cdr tagpair) string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Distel funs       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar distel-completion-sync nil
  "Global variable to sync between buffers.")

(defun distel-completion--wait-for-sync ()
  "Use global variable distel-completion-sync to sync between buffers."
  (if distel-completion-sync
      distel-completion-sync
    (sit-for 0.1)
    (distel-completion--wait-for-sync)))

(defun distel-completion-complete (search-string buf)
  "Complete SEARCH-STRING as a external module or function in BUF."
  (let* (;; Checks if there exists a ":" in the search-string
         (isok (string-match ":" search-string))
         ;; module (from start of string to ":")
         (mod (and isok (substring search-string 0 isok)))
         ;; function (from ":" to end of string)
         (fun (and isok (substring search-string (+ isok 1))))
         ;; node
         (node erl-nodename-cache))
    ;; Let distel complete the function or module, depending if ":" is part of
    ;; the search-string
    (if isok
        (distel-completion-function mod fun)
      (distel-completion-module search-string))
    ;; Add the module if needed
    (let* ((cands (distel-completion--wait-for-sync))
           (answer
            (progn
              (mapcar (lambda (item)
                        (concat mod (when mod ":") item))
                      cands))))
      answer)))

(defun distel-completion-describe (mod fun)
  "Get the documentation of function MOD:FUN."
  (let ((node erl-nodename-cache))
    (erl-spawn
      (erl-send-rpc node 'distel 'describe (list (intern mod)
                                                 (intern fun)))
      (&distel-completion-receive)))
  (distel-completion--wait-for-sync))

(defun distel-completion-args (mod fun)
  "Find the arguments to a function MOD:FUN."
  (let ((node erl-nodename-cache))
    (erl-spawn
      (erl-send-rpc node 'distel 'arglists (list mod fun))
      (&distel-completion-receive)))
  (distel-completion--wait-for-sync))

(defun distel-completion-module (module)
  "Get list of modulenames starting with MODULE."
  (erl-spawn
    (erl-send-rpc node 'distel 'modules (list module))
    (&distel-completion-receive))
  (distel-completion--wait-for-sync))

(defun distel-completion-function (module function)
  "Get list of function names in MODULE starting with FUNCTION."
  (erl-spawn
    (erl-send-rpc node 'distel 'functions (list module function))
    (&distel-completion-receive))
  (distel-completion--wait-for-sync))

(defun &distel-completion-receive ()
  "Receiver loop."
  (setq distel-completion-sync nil)
  (erl-receive ()
      ((['rex ['ok completions]]
	(setq distel-completion-sync completions))
       (other
	(message "Unexpected reply: %s" other)))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local buffer funs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun distel-completion-get-dabbrevs (args &optional times limit)
  "Use `dabbrev' to find last used commands beginning with ARGS
in current buffer only. TIMES is how many times it will match,
defaults to 5. LIMIT is how far from point it should search,
defaults to the start of the previous defun."
  (when (require 'dabbrev nil t)
    (dabbrev--reset-global-variables)
    (setq dabbrev-check-other-buffers nil
          dabbrev--last-expansion-location (point))
    (let ((times (or times 5))
          (dabbrev-limit (or limit (- (point) (save-excursion
                                                (backward-char (length args))
                                                (backward-paragraph)
                                                (point)))))
          (ret-list '())
          str)

      (while (and (> times 0) (dabbrev--find-expansion args 1 nil)))
      dabbrev--last-table)))

(defun distel-completion-get-functions (args)
  "Get all function definitions beginning with ARGS in the current buffer."
  (let ((case-fold-search nil)
        (funs '())
        fun-end)
    (save-excursion
      (goto-char (point-min))
      ;; Search for functions in the current-buffer starting with `args'
      (while (setq fun-end (re-search-forward (format "^%s.*?(" args) nil t))
        ;; add found functions as completion candidates
        (add-to-list 'funs (buffer-substring (match-beginning 0) (1- fun-end)))))
    funs))

(defun distel-completion-grab-word ()
  "Grab the current Erlang mod/fun/word."
  (interactive)
  (buffer-substring (point) (save-excursion
			      (skip-chars-backward distel-completion-valid-syntax)
			      (point))))

(defun distel-completion-is-comment-or-cite-p (&optional poin)
  "Returns t if point is inside a comment or a cite."
  (save-excursion
    (let ((po (or poin (point))))
      (goto-char po)
      (beginning-of-line)
      (re-search-forward "[%\|\"|\']" po t)
      (or (eql (char-before) ?%)
	  (and (or (eql (char-before) ?\")
		   (eql (char-before) ?\'))
	       (not (re-search-forward "[\"\|\']" po t)))))))


(provide 'distel-completion-lib)
;;; distel-completion-lib.el ends here

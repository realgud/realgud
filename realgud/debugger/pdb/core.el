;; Copyright (C) 2014-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'load-relative)
(require-relative-list '("../../common/track"
			 "../../common/core"
			 "../../common/lang")
		       "realgud-")
(require-relative-list '("init") "realgud:pdb-")


(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-lang-mode? 'realgud-lang)
(declare-function realgud-parse-command-arg 'realgud-core)
(declare-function realgud-query-cmdline 'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:pdb-minibuffer-history nil
  "minibuffer history list for the command `pdb'.")

(defvar realgud:pdb-remote-minibuffer-history nil
  "minibuffer history list for the command `pdb-remote'.")

(easy-mmode-defmap pdb-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of debugger startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun pdb-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'pdb-suggest-invocation
   pdb-minibuffer-local-map
   'realgud:pdb-minibuffer-history
   opt-debugger))

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun pdb-remote-query-cmdline (not-used)
  (realgud-query-cmdline
   'pdb-remote-suggest-invocation
   pdb-minibuffer-local-map
   'realgud:pdb-remote-minibuffer-history
   "telnet"))

(defun pdb-parse-cmd-args (orig-args)
  "Parse command line ORIG-ARGS for the annotate level and name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing:
* the command processor (e.g. python) and it's arguments if any - a list of strings
* the name of the debugger given (e.g. pdb) and its arguments - a list of strings
* the script name and its arguments - list of strings
* whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input:
  (map 'list 'symbol-name
   '(python2.6 -O -Qold ./gcd.py a b))

we might return:
   ((\"python2.6\" \"-O\" \"-Qold\") (\"pdb\") (\"/tmp/gcd.py\" \"a\" \"b\") nil)

Note that the script name path has been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  [python python-options] pdb pdb-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from
	(python-opt-two-args '())
	;; Python doesn't have mandatory 2-arg options in our sense,
	;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
	;;
	(python-two-args '())
	;; pdb doesn't have any arguments
	(pdb-two-args '())
	(pdb-opt-two-args '())
	(interp-regexp
	 (if (member system-type (list 'windows-nt 'cygwin 'msdos))
	     "^python[-0-9.]*\\(.exe\\)?$"
	   "^python[-0-9.]*$"))

	;; Things returned
	(annotate-p nil)
	(debugger-args '())
	(debugger-name nil)
	(interpreter-args '())
	(script-args '())
	(script-name nil)
	)

    (if (not (and args))
	;; Got nothing: return '(nil, nil)
	(list interpreter-args debugger-args script-args annotate-p)
      ;; else
      ;; Strip off optional "python" or "python182" etc.
      (when (string-match interp-regexp
			  (file-name-sans-extension
			   (file-name-nondirectory (car args))))
	(setq interpreter-args (list (pop args)))

	;; Strip off Python-specific options
	(while (and args
		    (string-match "^-" (car args)))
	  (setq pair (realgud-parse-command-arg
		      args python-two-args python-opt-two-args))
	  (nconc interpreter-args (car pair))
	  (setq args (cadr pair))))

      ;; Remove "pdb" from "pdb --pdb-options script
      ;; --script-options"
      (setq debugger-name (file-name-sans-extension
			   (file-name-nondirectory (car args))))
      (unless (string-match "^\\(pdb\\|cli.py\\)$" debugger-name)
	(message
	 "Expecting debugger name `%s' to be `pdb' or `cli.py'"
	 debugger-name))
      (setq debugger-args (list (pop args)))

      ;; Skip to the first non-option argument.
      (while (and args (not script-name))
	(let ((arg (car args)))
	  (cond
	   ;; Options with arguments.
	   ((string-match "^-" arg)
	    (setq pair (realgud-parse-command-arg
			args pdb-two-args pdb-opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name (realgud:expand-file-name-if-exists arg))
	      (setq script-args (cons script-name (cdr args))))
	   )))
      (list interpreter-args debugger-args script-args annotate-p))))

(defun pdb-parse-remote-cmd-args (orig-args)
    "Parse command line ORIG-ARGS
ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing:
* the command processor (e.g. python) and it's arguments if any - a list of strings
* the name of the debugger given (e.g. pdb) and its arguments - a list of strings
* the script name and its arguments - list of strings
* nil

For example for the following input:
  (map 'list 'symbol-name
   '(telnet localhost 6900))

we might return:
   ((\"telnet\" \"localhost\" \"6900\") nil nil nil)

Note that the script name path has been expanded via `expand-file-name'.
"
    (list orig-args nil nil nil)
  )

  ;; To silence Warning: reference to free variable
(defvar realgud:pdb-command-name)

(defun pdb-remote-suggest-invocation (debugger-name)
  "Suggest a pdb command invocation via `realgud-suggest-invocaton'"
  "telnet 127.0.0.1 4000"
  )


(defun pdb-suggest-invocation (debugger-name)
  "Suggest a pdb command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation realgud:pdb-command-name
			      realgud:pdb-minibuffer-history
			      "python" "\\.py"))

(defun pdb-reset ()
  "Pdb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (pdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*pdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun pdb-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'pdb-debugger-support-minor-mode minor-mode-map-alist)
;; 	  pdb-debugger-support-minor-mode-map-when-deactive))


(defun realgud:pdb-customize ()
  "Use `customize' to edit the settings of the `pdb' debugger."
  (interactive)
  (customize-group 'realgud:pdb))

(provide-me "realgud:pdb-")

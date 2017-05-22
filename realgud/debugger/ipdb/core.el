;; Copyright (C) 2016-2017 Free Software Foundation, Inc

;; Author: Sean Farley <sean@farley.io>, Rocky Bernstein (rocky@gnu.org)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.or/licenses/>.


(require 'comint)
(require 'load-relative)
(require-relative-list '("../../common/track"
			 "../../common/core"
			 "../../common/lang")
		       "realgud-")
(require-relative-list '("init") "realgud:ipdb-")


(declare-function realgud-lang-mode? 'realgud-lang)
(declare-function realgud-parse-command-arg 'realgud-core)
(declare-function realgud-query-cmdline 'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)
(declare-function realgud-get-cmdbuf   'realgud-buffer-helper)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:ipdb-minibuffer-history nil
  "minibuffer history list for the command `ipdb'.")

(defvar realgud:ipdb-remote-minibuffer-history nil
  "minibuffer history list for the command `ipdb-remote'.")

(easy-mmode-defmap ipdb-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of debugger startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun ipdb-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'ipdb-suggest-invocation
   ipdb-minibuffer-local-map
   'realgud:ipdb-minibuffer-history
   opt-debugger))

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun ipdb-remote-query-cmdline ()
  (realgud-query-cmdline
   'ipdb-suggest-invocation
   ipdb-minibuffer-local-map
   'realgud:ipdb-remote-minibuffer-history
   "telnet"))

(defun ipdb-parse-cmd-args (orig-args)
  "Parse command line ORIG-ARGS for the annotate level and name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing:
* the command processor (e.g. python) and it's arguments if any - a list of strings
* the name of the debugger given (e.g. ipdb) and its arguments - a list of strings
* the script name and its arguments - list of strings
* whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input:
  (map 'list 'symbol-name
   '(python2.6 -O -Qold ./gcd.py a b))

we might return:
   ((\"python2.6\" \"-O\" \"-Qold\") (\"ipdb\") (\"/tmp/gcd.py\" \"a\" \"b\") nil)

Note that the script name path has been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  [python python-options] ipdb ipdb-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from
	(python-opt-two-args '())
	;; Python doesn't have mandatory 2-arg options in our sense,
	;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
	;;
	(python-two-args '())
	;; ipdb doesn't have any arguments
	(ipdb-two-args '())
	(ipdb-opt-two-args '())
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

      ;; Remove "ipdb" from "ipdb --ipdb-options script
      ;; --script-options"
      (setq debugger-name (file-name-sans-extension
			   (file-name-nondirectory (car args))))
      (unless (string-match "^\\(ipdb\\|cli.py\\)$" debugger-name)
	(message
	 "Expecting debugger name `%s' to be `ipdb' or `cli.py'"
	 debugger-name))
      (setq debugger-args (list (pop args)))

      ;; Skip to the first non-option argument.
      (while (and args (not script-name))
	(let ((arg (car args)))
	  (cond
	   ;; Options with arguments.
	   ((string-match "^-" arg)
	    (setq pair (realgud-parse-command-arg
			args ipdb-two-args ipdb-opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name (expand-file-name arg))
	      (setq script-args (cons script-name (cdr args))))
	   )))
      (list interpreter-args debugger-args script-args annotate-p))))

(defun ipdb-parse-remote-cmd-args (orig-args)
    "Parse command line ORIG-ARGS
ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing:
* the command processor (e.g. python) and it's arguments if any - a list of strings
* the name of the debugger given (e.g. ipdb) and its arguments - a list of strings
* the script name and its arguments - list of strings
* nil

For example for the following input:
  (map 'list 'symbol-name
   '(telnet localhost 6900))

we might return:
   ((\"telnet\" \"localhost\" \"6900\") nil nil nil)

Note that the script name path has been expanded via `expand-file-name'.
"
    (list orig-args '("ipdb") nil nil nil)
  )

  ;; To silence Warning: reference to free variable
(defvar realgud:ipdb-command-name)

(defun ipdb-remote-suggest-invocation (debugger-name)
  "Suggest an ipdb command invocation via `realgud-suggest-invocaton'"
  "telnet 127.0.0.1 4000")

(defun ipdb-suggest-invocation (debugger-name)
  "Suggest a ipdb command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation (or debugger-name realgud:ipdb-command-name)
			      realgud:ipdb-minibuffer-history
			      "python" "\\.py"))

(defun ipdb-reset ()
  "Ipdb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (ipdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*ipdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun ipdb-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'ipdb-debugger-support-minor-mode minor-mode-map-alist)
;; 	  ipdb-debugger-support-minor-mode-map-when-deactive))

(defconst realgud:ipdb-complete-script
  (concat
   "from IPython import get_ipython;"
   "comp = '''%s''';"
   "prefix, candidates = get_ipython().Completer.complete(line_buffer = comp);"
   "print(';'.join([prefix] + candidates))"))

(defun realgud:ipdb-backend-complete ()
  "Send a command to the ipdb buffer and parse the output.

The idea here is to rely on the
`comint-redirect-send-command-to-process' function to send a
python command `realgud:ipdb-complete-script' that will return
the completions for the given input."
  (interactive)
  (let ((buffer (current-buffer))
        (cmdbuf (realgud-get-cmdbuf))
        (process (get-buffer-process (current-buffer)))
        (start-pos (save-excursion (comint-goto-process-mark) (point)))
        (end-pos (point)))

    ;; get the input string
    (when (> end-pos start-pos)
      (let* ((input-str (buffer-substring-no-properties start-pos end-pos))
             (command-str (format realgud:ipdb-complete-script input-str))
             (output-str (with-temp-buffer
                           (comint-redirect-send-command-to-process
                            command-str (current-buffer) process nil t)
                           ;; Wait for the process to complete
                           (with-current-buffer (process-buffer process)
                             (while (null comint-redirect-completed)
                               (accept-process-output nil 0 5))) ;; wait 5ms
                           (buffer-substring (point-min) (1- (point-max)))))
             (output-values (split-string output-str ";"))
             (prefix (car output-values)))
        (list (- end-pos (length prefix)) end-pos (cdr output-values))))))

(defun realgud:ipdb-completion-at-point ()
  (let ((ipdb (realgud:ipdb-backend-complete)))
    (when ipdb
      (list (nth 0 ipdb)
            (nth 1 ipdb)
            (nth 2 ipdb)
            :exclusive 'yes))))

(defun realgud:ipdb-customize ()
  "Use `customize' to edit the settings of the `ipdb' debugger."
  (interactive)
  (customize-group 'realgud:ipdb))

(provide-me "realgud:ipdb-")

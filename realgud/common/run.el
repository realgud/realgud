;; Copyright (C) 2015-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; (require 'term)

(require 'cl-lib)
(require 'shell)
(require 'load-relative)
(require-relative-list '("core" "track" "utils") "realgud-")
(require-relative-list '("buffer/command") "realgud-buffer-")

(declare-function realgud-cmdbuf-info-in-debugger?=   'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-cmd-args=       'realgud-buffer-command)
(declare-function realgud:track-set-debugger          'realgud-track)
(declare-function realgud-cmdbuf-info-starting-directory= 'realgud-buffer-command)
(declare-function realgud-parse-command-arg           'realgud-core)
(declare-function realgud:expand-file-name-if-exists  'realgud-core)
(declare-function realgud:flatten                     'realgud-utils)

(defvar starting-directory)

(defun realgud:parse-cmd-args(args two-args opt-two-args interp-regexp debugger-regexp
				   path-args-list annotate-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing:
* the command processor (e.g. bash) and it's arguments if any - a list of strings
* the name of the debugger given (e.g. bashdb) and its arguments - a list of strings.
  If there is no debugger, for example gdb, nodejs then nil is returned.
* the script name and its arguments - list of strings
* whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

The script name and options mentioning paths are file expanded

For example for the following input
  (map 'list 'symbol-name
   '(bash --norc bashdb -l . --emacs ./gcd.sh a b))

we might return:
   ((\"bash\" \"--norc\") (\"bashdb\" \"-l\" \"/tmp\" \"--emacs\") (\"/tmp/gcd.sh\" \"a\" \"b\") t)

Note that path elements have been expanded via `expand-file-name'.
"
  ;; Parse the following kind of pattern:
  ;;  [bash bash-options] bashdb bashdb-options script-name script-options
  (let (
	(pair)
	;; Things returned
	(script-name nil)
	(debugger-name nil)
	(interpreter-args '())
	(debugger-args '())
	(script-args '())
	(annotate-p nil))

    (if (not (and args))
	;; Got nothing: return '(nil, nil nil nil)
	(list interpreter-args debugger-args script-args annotate-p)
      ;; else
      ;; Strip off optional interpreter name
      (when (and interp-regexp
		 (string-match interp-regexp
			       (file-name-sans-extension
				(file-name-nondirectory (car args)))))
	(setq interpreter-args (list (pop args)))

	;; Strip off compiler/intepreter-specific options
	(while (and args
		    (string-match "^-" (car args)))
	  (setq pair (realgud-parse-command-arg
		      args two-args opt-two-args))
	  (nconc interpreter-args (car pair))
	  (setq args (cadr pair))))

      ;; Skip to the first non-option argument.
      (while (and args (not script-name))
	(let ((arg (car args)))
	  (cond
	   ;; path-like options
	   ((member arg path-args-list)
	    (setq arg (pop args))
	    (nconc debugger-args
		   (list arg (realgud:expand-file-name-if-exists
			      (pop args)))))
	   ;; Other options with arguments.
	   ((string-match "^-" arg)
	    (setq pair (realgud-parse-command-arg
			args two-args opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name (realgud:expand-file-name-if-exists arg))
	      (setq script-args (cons script-name (cdr args))))
	   )))
      (list interpreter-args debugger-args script-args annotate-p))))

(defun realgud:run-process(debugger-name script-filename cmd-args
					 minibuffer-history
					 &optional no-reset)
  "Runs `realgud-exec-shell' with DEBUGGER-NAME SCRIPT-FILENAME
and CMD-ARGS. If this succeeds, we save CMD-ARGS in command-buffer
for use if we want to restart.  If we don't succeed in running
the program, we will switch to the command buffer which shows
details of the error. The command buffer or nil is returned.

DEBUGGER-NAME is used in selecting the tracking mode inside the
command buffer. The debugger name and SCRIPT-FILENAME are used in
selecting a buffer name for the command buffer.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."

  (let ((cmd-buf))
    (setq cmd-buf
	  (apply 'realgud-exec-shell debugger-name script-filename
		 (car cmd-args) no-reset (cdr cmd-args)))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case?
    (let ((process (get-buffer-process cmd-buf)))
      (if (and process (eq 'run (process-status process)))
	  (progn
	    (switch-to-buffer cmd-buf)
	    (realgud:track-set-debugger debugger-name)
	    (realgud-cmdbuf-info-in-debugger?= 't)
	    (realgud-cmdbuf-info-cmd-args= cmd-args)
	    (when cmd-buf
	      (switch-to-buffer cmd-buf)
	      (when realgud-cmdbuf-info
		(let* ((info realgud-cmdbuf-info)
		       (cmd-args (realgud-cmdbuf-info-cmd-args info))
		       (cmd-str  (mapconcat 'identity  cmd-args " ")))
		  (if (boundp 'starting-directory)
		      (realgud-cmdbuf-info-starting-directory= starting-directory))
		  (set minibuffer-history
		       (cl-remove-duplicates
			(cons cmd-str (eval minibuffer-history)) :from-end)
		       ))
		)))
	;; else
	(progn
	  (if cmd-buf (switch-to-buffer cmd-buf))
	  (message "Error running command: %s" (mapconcat 'identity cmd-args " "))
	  )
	)
      )
    cmd-buf
    )
  )

(defun realgud:run-debugger (debugger-name query-cmdline-fn parse-cmd-args-fn
					   minibuffer-history
					   &optional opt-command-line
					   no-reset opt-script-name)
  "Invoke the a debugger and start the Emacs user interface.

String OPT-COMMAND-LINE specifies how to run DEBUGGER-NAME. You
will be prompted for a command line using QUERY-CMDLINE-FN is one
isn't supplied.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by PARSE-CMD-FN and path elements found by that
are expanded using `expand-file-name'.

If successful, The command buffer of the debugger process is returned.
Otherwise nil is returned.
"
  (let* ((cmd-str (or opt-command-line (funcall query-cmdline-fn debugger-name)))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (funcall parse-cmd-args-fn cmd-args))
	 (script-args (cl-caddr parsed-args))
	 (script-name (or opt-script-name (car script-args)))
	 (parsed-cmd-args
	  (cl-remove-if 'nil (realgud:flatten parsed-args)))
	 )
    (realgud:run-process debugger-name script-name parsed-cmd-args
			 minibuffer-history no-reset)
    )
  )

;; For name = trepan2 we produce:
;;
;; (defalias 'trepan2 'realgud:trepan2)
;; (defvar realgud:trepan2-delayed-minibuffer-history nil
;;   "minibuffer history list for the command `realgud:trepan2-delayed'.")

(defmacro realgud-deferred-invoke-setup (name)
  `(progn
     (defalias
       ',(intern (concat name "-delayed"))
       ',(intern (concat "realgud:" name "-delayed")))
     (defvar ,(intern (concat "realgud:" name "-delayed-minibuffer-history")) nil
      ,(format "minibuffer history for the command `%s-delayed'" name))
     ))

(provide-me "realgud:")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

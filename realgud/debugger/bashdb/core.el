;; Copyright (C) 2015-2017, 2021 Free Software Foundation, Inc

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

(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/core")
		       "realgud-")
(require-relative-list '("init") "realgud:bashdb-")

(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:bashdb-minibuffer-history nil
  "minibuffer history list for the command `realgud:bashdb'.")

(easy-mmode-defmap bashdb-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of bashdb startup command."
  :inherit minibuffer-local-map)

(defvar realgud:bashdb-file-remap (make-hash-table :test 'equal)
  "How to remap script files in bashdb when we otherwise can't
  find in the filesystem. The hash key is the file string we saw,
  and the value is associated filesystem string presumably in the
  filesystem")

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun bashdb-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'bashdb-suggest-invocation
   bashdb-minibuffer-local-map
   'realgud:bashdb-minibuffer-history
   opt-debugger))

;;; FIXME: DRY this with other *-parse-cmd-args routines
(defun bashdb-parse-cmd-args (orig-args)
  "Parse command line ORIG-ARGS for the annotate level and name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing:
* the command processor (e.g. bash) and it's arguments if any - a list of strings
* the name of the debugger given (e.g. bashdb) and its arguments - a list of strings
* the script name and its arguments - list of strings
* whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

The script name and options mentioning paths are file expanded

For example for the following input
  (map 'list 'symbol-name
   '(bash --norc bashdb -l . --emacs ./gcd.sh a b))

we might return:
   ((\"bash\" \"--norc\") (\"bashdb\" \"-l\" \"/tmp\" \"--emacs\") (\"/tmp/gcd.sh\" \"a\" \"b\") t)

Note that path elements have been expanded via `realgud:expand-file-name-if-exists'.
"

  ;; Parse the following kind of pattern:
  ;;  [bash bash-options] bashdb bashdb-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from
	;; bash doesn't have any optional two-arg options
	(bash-opt-two-args '())
	(bash-two-args '("o" "c"))

	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(bashdb-two-args '("A" "-annotate" "l" "-library"
			   "c" "-command" "-t" "-tty"
			   "x" "-eval-command"))
	(bashdb-opt-two-args '())
	(interp-regexp
	 (if (member system-type (list 'windows-nt 'cygwin 'msdos))
	     "^bash*\\(.exe\\)?$"
	   "^bash*$"))
	(bashdb-two-arg-name)
	(debugger-flag nil) ;; 't if "bash --debugger" given

	;; Things returned
	(script-name nil)
	(debugger-name nil)
	(interpreter-args '())
	(debugger-args '())
	(script-args '())
	(annotate-p nil))

    (if (not (and args))
	;; Got nothing: return '(nil, nil)
	(list interpreter-args debugger-args script-args annotate-p)
      ;; else
      ;; Strip off optional "bash" or "bash4" etc.
      (when (string-match interp-regexp
			  (file-name-sans-extension
			   (file-name-nondirectory (car args))))
	(setq interpreter-args (list (pop args)))

	;; Strip off bash-specific options
	(while (and args
		    (string-match "^-" (car args)))
	  (setq pair (realgud-parse-command-arg
		      args bash-two-args bash-opt-two-args))
	  (if (equal "--debugger" (caar pair))
	      (setq debugger-flag 't))
	  (nconc interpreter-args (car pair))
	  (setq args (cadr pair))))

      ;; Remove "bashdb" from "bashdb --bashdb-options script
      ;; --script-options"
      (unless debugger-flag
	(setq debugger-name (file-name-sans-extension
			     (file-name-nondirectory (car args))))
	(unless (string-match "^bashdb$" debugger-name)
	  (message
	   "Expecting debugger name `%s' to be `bashdb'"
	   debugger-name))
	(setq debugger-args (list (pop args)))
	)

      ;; Skip to the first non-option argument.
      (while (and args (not script-name))
	(let ((arg (car args)))
	  (cond
	   ;; Annotation or emacs option with level number.
	   ((member arg '("--annotate" "-A" "--emacs"))
	    (setq annotate-p t)
	    (nconc debugger-args (list (pop args))))
	   ;; Combined annotation and level option.
	   ((string-match "^--annotate=[0-9]" arg)
	    (nconc debugger-args (list (pop args)) )
	    (setq annotate-p t))
	   ;; Library option
	   ((member arg '("--library" "-l"))
	    (setq arg (pop args))
	    (nconc debugger-args
		   (list arg (realgud:expand-file-name-if-exists
			      (pop args)))))
	   ;; Other options with arguments.
	   ((string-match "^-" arg)
	    (setq pair (realgud-parse-command-arg
			args bashdb-two-args bashdb-opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name (realgud:expand-file-name-if-exists arg))
	      (setq script-args (cons script-name (cdr args))))
	   )))
      (list interpreter-args debugger-args script-args annotate-p))))

;; To silence Warning: reference to free variable
(defvar realgud:bashdb-command-name)

(defun bashdb-suggest-invocation (debugger-name)
  "Suggest a bashdb command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation (or debugger-name realgud:bashdb-command-name)
			      realgud:bashdb-minibuffer-history
			      "sh" "\\.\\(?:ba\\)?sh$"
			      realgud:bashdb-command-name))

(defun bashdb-reset ()
  "Bashdb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (bashdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*bashdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun bashdb-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'bashdb-debugger-support-minor-mode minor-mode-map-alist)
;; 	  bashdb-debugger-support-minor-mode-map-when-deactive))


(defun realgud:bashdb-customize ()
  "Use `customize' to edit the settings of the `bashdb' debugger."
  (interactive)
  (customize-group 'realgud:bashdb))

(provide-me "realgud:bashdb-")

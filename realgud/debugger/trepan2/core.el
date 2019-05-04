;; Copyright (C) 2010-2012, 2014-2016, 2018, 2019 Free Software Foundation, Inc

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
(require-relative-list '("../../common/track"
			 "../../common/core"
                         "../../common/file"
			 "../../common/lang")
		       "realgud-")
(require-relative-list '("init") "realgud:trepan2-")

(declare-function realgud:strip              'realgud)
(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)
(declare-function realgud:file-loc-from-line 'realgud-file)
(declare-function realgud:find-file          'realgud-file)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:trepan2-minibuffer-history nil
  "minibuffer history list for the command `realgud:trepan2'.")

(easy-mmode-defmap trepan2-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

(defvar realgud:trepan2-file-remap (make-hash-table :test 'equal)
  "How to remap Python files in trepan2 when we otherwise can't
  find in the filesystem. The hash key is the file string we saw,
  and the value is associated filesystem string presumably in the
  filesystem")

;; FIXME: this code could be generalized and put in a common place.
(defun realgud:trepan2-find-file(marker filename directory)
  "A find-file specific for python/trepan. We strip off trailing
blanks. Failing that we will prompt for a mapping and save that
in variable `realgud:trepan2-file-remap' when that works. In the future,
we may also consult PYTHONPATH."
  (let* ((transformed-file)
	 (cmdbuf (realgud-get-cmdbuf))
	 (stripped-filename (realgud:strip filename))
	 (ignore-re-file-list (realgud-cmdbuf-ignore-re-file-list cmdbuf))
	 (filename-remap-alist (realgud-cmdbuf-filename-remap-alist))
	 (remapped-filename
	  (assoc filename filename-remap-alist))
	)
    (cond
     ((file-exists-p filename) filename)
     ((file-exists-p stripped-filename) stripped-filename)
     ((realgud:file-ignore filename ignore-re-file-list)
      (message "tracking ignored for %s" filename) nil)
     (t
      ;; FIXME search PYTHONPATH if not absolute file
      (if remapped-filename
     	  (if (file-exists-p (cdr remapped-filename))
     	      (cdr remapped-filename)
     	    ;; else remove from map since no find
     	    (and (realgud-cmdbuf-filename-remap-alist=
     		  (delq (assoc remapped-filename filename-remap-alist)
     			filename-remap-alist))
     		  nil))
     	;; else
     	(let ((remapped-filename))
     	  (setq remapped-filename
     		(buffer-file-name
     		 (realgud:find-file marker stripped-filename
     				    directory "%s.py")))
     	  (when (and remapped-filename (file-exists-p remapped-filename))
     	    (realgud-cmdbuf-filename-remap-alist=
     	     (cons
     	      (cons filename remapped-filename)
     	      filename-remap-alist))
     	    ))
	))
     )))

(defun realgud:trepan2-loc-fn-callback(text filename lineno source-str
					    cmd-mark directory)
  (realgud:file-loc-from-line filename lineno cmd-mark source-str nil
			      'realgud:trepan2-find-file directory))

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun trepan2-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'trepan2-suggest-invocation
   trepan2-minibuffer-local-map
   'realgud:trepan2-minibuffer-history
   opt-debugger))

(defun trepan2-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing
- the command processor (e.g. python) and it's arguments if any - a list of strings
- the name of the debugger given (e.g. trepan2) and its arguments - a list of strings
- the script name and its arguments - list of strings
- whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input
  (map 'list 'symbol-name
   '(python2.6 -O -Qold --emacs ./gcd.py a b))

we might return:
   ((python2.6 -O -Qold) (trepan2 --emacs) (./gcd.py a b) 't)

NOTE: the above should have each item listed in quotes.
"

  ;; Parse the following kind of pattern:
  ;;  [python python-options] trepan2 trepan2-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from
	(python-opt-two-args '("c" "m" "Q" "W"))
	;; Python doesn't have mandatory 2-arg options in our sense,
	;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
	;;
	(python-two-args '())
	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(trepan2-two-args '("x" "-command" "e" "-execute"
			   "o" "-output"  "t" "-target"
			   "a" "-annotate"))
	(trepan2-opt-two-args '())
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

      ;; Remove "trepan2" from "trepan2 --trepan2-options script
      ;; --script-options"
      (setq debugger-name (file-name-sans-extension
			   (file-name-nondirectory (car args))))
      (unless (string-match "^\\(trepan2\\|cli.py\\)$" debugger-name)
	(message
	 "Expecting debugger name `%s' to be `trepan2' or `cli.py'"
	 debugger-name))
      (setq debugger-args (list (pop args)))

      ;; Skip to the first non-option argument.
      (while (and args (not script-name))
	(let ((arg (car args)))
	  (cond
	   ;; Annotation or emacs option with level number.
	   ((or (member arg '("--annotate" "-A"))
		(equal arg "--emacs"))
	    (setq annotate-p t)
	    (nconc debugger-args (list (pop args))))
	   ;; Combined annotation and level option.
	   ((string-match "^--annotate=[0-9]" arg)
	    (nconc debugger-args (list (pop args)) )
	    (setq annotate-p t))
	   ;; Options with arguments.
	   ((string-match "^-" arg)
	    (setq pair (realgud-parse-command-arg
			args trepan2-two-args trepan2-opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name (realgud:expand-file-name-if-exists arg))
	      (setq script-args (cons script-name (cdr args))))
	   )))
      (list interpreter-args debugger-args script-args annotate-p))))

;; To silence Warning: reference to free variable
(defvar realgud:trepan2-command-name)

(defun trepan2-suggest-invocation (debugger-name)
  "Suggest a trepan2 command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation (or debugger-name realgud:trepan2-command-name)
			      realgud:trepan2-minibuffer-history
			      "python" "\\.py"
			      realgud:trepan2-command-name))

(defun trepan2-reset ()
  "Trepan2 cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (trepan2-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*trepan2-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun trepan2-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'trepan2-debugger-support-minor-mode minor-mode-map-alist)
;; 	  trepan2-debugger-support-minor-mode-map-when-deactive))


(defun realgud:trepan2-customize ()
  "Use `customize' to edit the settings of the `trepan2' debugger."
  (interactive)
  (customize-group 'realgud:trepan2))

(provide-me "realgud:trepan2-")

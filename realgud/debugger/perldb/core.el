;; Copyright (C) 2011, 2013-2014, 2016 Free Software Foundation, Inc

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
(require-relative-list '("init") "realgud:perldb-")

(declare-function realgud-lang-mode?         'realgud-lang)
(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:perldb-minibuffer-history nil
  "minibuffer history list for the command `perldb'.")

(easy-mmode-defmap realgud:perldb-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of perldb startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun realgud:perldb-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'realgud:perldb-suggest-invocation
   realgud:perldb-minibuffer-local-map
   'realgud:perldb-minibuffer-history
   opt-debugger))

;;; FIXME: DRY this with other *-parse-cmd-args routines
(defun realgud:perldb-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing

* the command processor (e.g. perl) and it's arguments if any - a
  list of strings

* the script name and its arguments - list of strings

For example for the following input:
  (map 'list 'symbol-name
   '(perl -W -C /tmp -d ./gcd.pl a b))

we might return:
   ((\"perl\" \"-W\" \"-C\" \"-d\") nil (\"/tmp/gcd.pl\" \"a\" \"b\"))

Note that path elements have been expanded via `realgud:expand-file-name-if-exists'.
"

  ;; Parse the following kind of pattern:
  ;;  [perl perl-options] perldb perldb-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from
	(perl-opt-two-args '("0" "C" "D" "i" "l" "m" "-module" "x"))
	;; Perl doesn't have mandatory 2-arg options in our sense,
	;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
	;;
	(perl-two-args '())
	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(perldb-two-args '("e" "E"))
	(perldb-opt-two-args '())
	(interp-regexp
	 (if (member system-type (list 'windows-nt 'cygwin 'msdos))
	     "^perl\\(?:5[0-9.]*\\)\\(.exe\\)?$"
	   "^perl\\(?:5[0-9.]*\\)?$"))

	;; Things returned
	(script-name nil)
	(debugger-name nil)
	(interpreter-args '())
	(script-args '())
	)

    (if (not (and args))
	;; Got nothing
	(list interpreter-args nil script-args)
      ;; else
      ;; Remove "perl" or "perl5.10.1" etc.
      (when (string-match interp-regexp
			  (file-name-sans-extension
			   (file-name-nondirectory (car args))))
	(setq interpreter-args (list (pop args)))

	;; Skip to the first non-option argument
	(while (and args (not script-name))
	  (let ((arg (car args)))
	    (cond
	     ;; Options with arguments.
	     ((string-match "^-" (car args))
	      (setq pair (realgud-parse-command-arg
			  args perl-two-args perl-opt-two-args))
	      (nconc interpreter-args (car pair))
	      (setq args (cadr pair)))
	     ;; Anything else must be the script to debug.
	     (t (setq script-name (realgud:expand-file-name-if-exists arg))
		(setq script-args (cons script-name (cdr args))))
	     )))
	(list interpreter-args nil script-args)))
    ))

; # To silence Warning: reference to free variable
(defvar realgud:perldb-command-name)

(defun realgud:perldb-suggest-invocation (debugger-name)
  "Suggest a perldb command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation realgud:perldb-command-name
			      realgud:perldb-minibuffer-history
			      "perl" "\\.pl$"))

(defun realgud:perldb-reset ()
  "Perldb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (perldb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*perldb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun perldb-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'perldb-debugger-support-minor-mode minor-mode-map-alist)
;; 	  perldb-debugger-support-minor-mode-map-when-deactive))


(defun realgud:perldb-customize ()
  "Use `customize' to edit the settings of the `perldb' debugger."
  (interactive)
  (customize-group 'realgud:perldb))

(provide-me "realgud:perldb-")

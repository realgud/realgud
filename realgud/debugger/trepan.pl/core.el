;; Copyright (C) 2011-2012, 2014, 2016-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'load-relative)
(require-relative-list '("../../common/track"
                         "../../common/core"
                         "../../common/eval"
                         "../../common/lang")
                       "realgud-")
(require-relative-list '("init") "realgud:trepanpl-")

(declare-function realgud:eval-strip-default 'realgud-eval)
(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:trepanpl-minibuffer-history nil
  "minibuffer history list for the command `realgud:trepan.pl'.")

(easy-mmode-defmap realgud:trepanpl-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of trepanpl startup command."
  :inherit minibuffer-local-map)

(defun realgud:trepanpl-eval-filter-callback(output-str)
  (realgud:eval-strip-default realgud:trepanpl-prompt-regexp
   (if (string-match realgud:trepanpl-eval-result-prefix-regexp output-str)
       (substring output-str (match-end 0))
     output-str)))

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun realgud:trepanpl-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'realgud:trepanpl-suggest-invocation
   realgud:trepanpl-minibuffer-local-map
   'realgud:trepanpl-minibuffer-history
   opt-debugger))

;;; FIXME: DRY this with other *-parse-cmd-args routines
(defun realgud:trepanpl-parse-cmd-args (orig-args)
  "Parse command line ORIG-ARGS for the annotate level and name of script to debug.

ORIG_ARGS should contain a tokenized list of the command line to run.

We return the a list containing:
* the command processor (e.g. perl) and it's arguments if any - a list of strings
* the name of the debugger given (e.g. trepan.pl) and its arguments - a list of strings
* the script name and its arguments - list of strings

For example for the following input:
  (map 'list 'symbol-name
   '(perl5.10 -w -I . trepan.pl --cd . ./gcd.pl a b))

we might return:
   ((\"perl\" \"-w\" \"-I\" \"/tmp\") (\"trepan.pl\" \"cd\" \"/tmp\") (\"/tmp/gcd.pl\" \"a\" \"b\"))

Note that the script name path has been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  [perl perl-options] trepanpl trepanpl-options script-name script-options
  (let (
        (args orig-args)
        (pair)          ;; temp return from
        (perl-opt-two-args '("0" "C" "D" "i" "I" "l" "m" "-module" "x"))
        ;; Perl doesn't have mandatory 2-arg options in our sense,
        ;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
        ;;
        (perl-two-args '())
        ;; One dash is added automatically to the below, so
        ;; h is really -h and -host is really --host.
        (trepanpl-two-args '("h" "-host" "p" "-port"
                           "I" "-include"))
        (trepanpl-opt-two-args '())
        (interp-regexp
         (if (member system-type (list 'windows-nt 'cygwin 'msdos))
             "^perl\\(?:5[0-9.]*\\)\\(.exe\\)?$"
           "^perl\\(?:5[0-9.]*\\)?$"))

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
      ;; Strip off optional "perl" or "perl5.10.1" etc.
      (when (string-match interp-regexp
                          (file-name-sans-extension
                           (file-name-nondirectory (car args))))
        (setq interpreter-args (list (pop args)))

        ;; Strip off optional "perl" or "perl5.10.1" etc.
        (while (and args
                    (string-match "^-" (car args)))
          (setq pair (realgud-parse-command-arg
                      args perl-two-args perl-opt-two-args))
          (nconc interpreter-args (car pair))
          (setq args (cadr pair))))

      ;; Remove "trepan.pl" from "trepan.pl --trepan.pl-options script
      ;; --script-options"
      (setq debugger-name (file-name-nondirectory (car args)))
      (unless (string-match "^trepan.pl$" debugger-name)
	(message
	 "Expecting debugger name `%s' to be `trepan.pl'"
	 debugger-name))
      (setq debugger-args (list (pop args)))

      ;; Skip to the first non-option argument.
      (while (and args (not script-name))
	(let ((arg (car args)))
	  (cond
	   ((member arg
		    '("--cmddir" "--batch" "--cd" "--include" "-I" "--module" "-M"
		      "-c" "--command"))
	    (setq arg (pop args))
	    (nconc debugger-args
		   (list arg (expand-file-name (pop args)))))
	   ;; Other options with arguments.
	   ((string-match "^-" arg)
	    (setq pair (realgud-parse-command-arg
			args trepanpl-two-args trepanpl-opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name (realgud:expand-file-name-if-exists arg))
	      (setq script-args (cons script-name (cdr args))))
	   )))
      (list interpreter-args debugger-args script-args))
    ))

; # To silence Warning: reference to free variable
(defvar realgud:trepanpl-command-name)

(defun realgud:trepanpl-suggest-invocation (debugger-name)
  "Suggest a trepanpl command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation (or debugger-name realgud:trepanpl-command-name)
			      realgud:trepanpl-minibuffer-history
			      "perl" "\\.pl$" "trepan.pl"))

(defun realgud:trepanpl-reset ()
  "Trepanpl cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (trepanpl-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*trepanpl-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun trepanpl-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'trepanpl-debugger-support-minor-mode minor-mode-map-alist)
;;        trepanpl-debugger-support-minor-mode-map-when-deactive))


(defun realgud:trepanpl-customize ()
  "Use `customize' to edit the settings of the
`realgud:trepan.pl' debugger."
  (interactive)
  (customize-group 'realgud:trepanpl))

(provide-me "realgud:trepanpl-")

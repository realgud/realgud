;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/core")
		       "realgud-")
(require-relative-list '("init") "realgud-nodejs-")

(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar nodejs-minibuffer-history nil
  "minibuffer history list for the command `nodejs'.")

(easy-mmode-defmap nodejs-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun nodejs-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'nodejs-suggest-invocation
   nodejs-minibuffer-local-map
   'nodejs-minibuffer-history
   opt-debugger))

(defun nodejs-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing
- the command processor (e.g. nodejs) and it's arguments if any - a list of strings
- the name of the debugger given (e.g. nodejs) and its arguments - a list of strings
- the script name and its arguments - list of strings
- whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input
  (map 'list 'symbol-name
   '(bash -W -C /tmp nodejs --emacs ./gcd.rb a b))

we might return:
   ((bash -W -C) (nodejs --emacs) (./gcd.rb a b) 't)

NOTE: the above should have each item listed in quotes.
"

  ;; Parse the following kind of pattern:
  ;;  [bash bash-options] nodejs nodejs-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from
	;; bash doesn't have any optional two-arg options
	(bash-opt-two-args '())
	(bash-two-args '("o" "c"))

	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(nodejs-two-args '("A" "-annotate" "l" "-library"
			   "c" "-command" "-t" "-tty"
			   "x" "-eval-command"))
	(nodejs-opt-two-args '())
	(interp-regexp
	 (if (member system-type (list 'windows-nt 'cygwin 'msdos))
	     "^bash*\\(.exe\\)?$"
	   "^bash*$"))

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
      ;; Strip off optional "ruby" or "ruby182" etc.
      (when (string-match interp-regexp
			  (file-name-sans-extension
			   (file-name-nondirectory (car args))))
	(setq interpreter-args (list (pop args)))

	;; Strip off Ruby-specific options
	(while (and args
		    (string-match "^-" (car args)))
	  (setq pair (realgud-parse-command-arg
		      args bash-two-args bash-opt-two-args))
	  (nconc interpreter-args (car pair))
	  (setq args (cadr pair))))

      ;; Remove "nodejs" from "nodejs --nodejs-options script
      ;; --script-options"
      (setq debugger-name (file-name-sans-extension
			   (file-name-nondirectory (car args))))
      (unless (string-match "^nodejs$" debugger-name)
	(message
	 "Expecting debugger name `%s' to be `nodejs'"
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
			args nodejs-two-args nodejs-opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name arg)
	      (setq script-args args))
	   )))
      (list interpreter-args debugger-args script-args annotate-p))))

(defvar nodejs-command-name) ; # To silence Warning: reference to free variable
(defun nodejs-suggest-invocation (debugger-name)
  "Suggest a nodejs command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation nodejs-command-name nodejs-minibuffer-history
			   "Shell-script" "\\.sh$"))

(defun nodejs-reset ()
  "Nodejs cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (nodejs-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*nodejs-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun nodejs-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'nodejs-debugger-support-minor-mode minor-mode-map-alist)
;; 	  nodejs-debugger-support-minor-mode-map-when-deactive))


(defun nodejs-customize ()
  "Use `customize' to edit the settings of the `nodejs' debugger."
  (interactive)
  (customize-group 'nodejs))

(provide-me "realgud-nodejs-")

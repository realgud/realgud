(eval-when-compile (require 'cl))
  
(require 'load-relative)
(require-relative-list '("../common/track" "../common/core" "../common/lang")
		       "dbgr-")

;; FIXME: I think the following could be generalized and moved to 
;; dbgr-... probably via a macro.
(defvar rbdbgr-minibuffer-history nil
  "minibuffer history list for the command `rbdbgr'.")

(easy-mmode-defmap rbdbgr-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun rbdbgr-query-cmdline (&optional opt-debugger)
  (dbgr-query-cmdline 
   'rbdbgr-suggest-invocation
   rbdbgr-minibuffer-local-map
   'rbdbgr-minibuffer-history
   opt-debugger))

(defun rbdbgr-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing
- the command processor (e.g. ruby) and it's arguments if any - a list of strings
- the name of the debugger given (e.g. rbdbgr) and its arguments - a list of strings
- the script name and its arguments - list of strings
- whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input 
  (map 'list 'symbol-name
   '(ruby1.9 -W -C /tmp rbdbgr --emacs ./gcd.rb a b))

we might return:
   ((ruby1.9 -W -C) (rbdbgr --emacs) (./gcd.rb a b) 't)

NOTE: the above should have each item listed in quotes.
"

  ;; Parse the following kind of pattern:
  ;;  [ruby ruby-options] rbdbgr rbdbgr-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from 
	(ruby-opt-two-args '("0" "C" "e" "E" "F" "i"))
	;; Ruby doesn't have mandatory 2-arg options in our sense,
	;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
	;; 
	(ruby-two-args '())
	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(rbdbgr-two-args '("h" "-host" "p" "-port"
			   "I" "-include" "-r" "-require"))
	(rbdbgr-opt-two-args '())
	(interp-regexp 
	 (if (member system-type (list 'windows-nt 'cygwin 'msdos))
	     "^ruby[-0-9]*\\(.exe\\)?$"
	   "^ruby[-0-9]*$"))

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
	  (setq pair (dbgr-parse-command-arg 
		      args ruby-two-args ruby-opt-two-args))
	  (nconc interpreter-args (car pair))
	  (setq args (cadr pair))))

      ;; Remove "rbdbgr" from "rbdbgr --rbdbgr-options script
      ;; --script-options"
      (setq debugger-name (file-name-sans-extension
			   (file-name-nondirectory (car args))))
      (unless (string-match "^rbdbgr$" debugger-name)
	(message 
	 "Expecting debugger name `%s' to be `rbdbgr'"
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
	    (setq pair (dbgr-parse-command-arg 
			args rbdbgr-two-args rbdbgr-opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name arg)
	      (setq script-args args))
	   )))
      (list interpreter-args debugger-args script-args annotate-p))))

(defvar rbdbgr-command-name) ; # To silence Warning: reference to free variable
(defun rbdbgr-suggest-invocation (debugger-name)
  "Suggest a rbdbgr command invocation via `dbgr-suggest-invocaton'"
  (dbgr-suggest-invocation rbdbgr-command-name rbdbgr-minibuffer-history 
			   'dbgr-suggest-ruby-file))

(defun rbdbgr-goto-backtrace-line (pt)
  "Display the location mentioned by the Ruby traceback line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "backtrace" rbdbgr-pat-hash))

(defun rbdbgr-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "control-frame" rbdbgr-pat-hash))

(defun rbdbgr-goto-dollarbang-traceback-line (pt)
  "Display the location mentioned by a Ruby $! traceback line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "dollar-bang" rbdbgr-pat-hash))

(defun rbdbgr-reset ()
  "Rbdbgr cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (rbdbgr-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rbdbgr-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun rbdbgr-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'rbdbgr-debugger-support-minor-mode minor-mode-map-alist)
;; 	  rbdbgr-debugger-support-minor-mode-map-when-deactive))


(defun rbdbgr-customize ()
  "Use `customize' to edit the settings of the `rbdbgr' debugger."
  (interactive)
  (customize-group 'rbdbgr))

(provide-me "rbdbgr-")

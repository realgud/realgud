(eval-when-compile (require 'cl))
  
(require 'load-relative)
(require-relative-list '("../common/track" "../common/core") "dbgr-")

;; FIXME: I think the following could be generalized and moved to 
;; dbgr-... probably via a macro.
(defvar rdebug-minibuffer-history nil
  "minibuffer history list for the command `rdebug'.")

(easy-mmode-defmap rdebug-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun rdebug-query-cmdline (&optional opt-debugger)
  (dbgr-query-cmdline 
   'rdebug-suggest-invocation
   rdebug-minibuffer-local-map
   'rdebug-minibuffer-history
   opt-debugger))

(defun rdebug-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing
- the command processor (e.g. ruby) and it's arguments if any - a list of strings
- the name of the debugger given (e.g. rdebug) and its arguments - a list of strings
- the script name and its arguments - list of strings
- whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input 
  (map 'list 'symbol-name
   '(ruby1.9 -W -C /tmp rdebug --emacs ./gcd.rb a b))

we might return:
   ((ruby1.9 -W -C) (rdebug --emacs) (./gcd.rb a b) 't)

NOTE: the above should have each item listed in quotes.
"

  ;; Parse the following kind of pattern:
  ;;  [ruby ruby-options] rdebug rdebug-options script-name script-options
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
	(rdebug-two-args '("h" "-host" "p" "-port"
			   "I" "-include" "-r" "-require"))
	(rdebug-opt-two-args '())

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
      (when (string-match "^ruby[-0-9]*$"
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

      ;; Remove "rdebug" from "rdebug --rdebug-options script
      ;; --script-options"
      (setq debugger-name (file-name-sans-extension
			   (file-name-nondirectory (car args))))
      (unless (string-match "^rdebug$" debugger-name)
	(message 
	 "Expecting debugger name `%s' to be `rdebug'"
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
			args rdebug-two-args rdebug-opt-two-args))
	    (nconc debugger-args (car pair))
	    (setq args (cadr pair)))
	   ;; Anything else must be the script to debug.
	   (t (setq script-name arg)
	      (setq script-args args))
	   )))
      (list interpreter-args debugger-args script-args annotate-p))))

(defun rdebug-file-ruby-mode? (filename)
  "Return true if FILENAME is a buffer we are visiting a buffer
that is in ruby-mode"
  (let ((buffer (and filename (find-buffer-visiting filename)))
	(match-pos))
    (if buffer 
	(progn 
	  (save-current-buffer
	    (set-buffer buffer)
	    (setq match-pos (string-match "^ruby-" (format "%s" major-mode))))
	  (and match-pos (= 0 match-pos)))
      nil)))

(defvar rdebug-command-name)
(defun rdebug-suggest-invocation (debugger-name)
  "Suggest a rdebug command invocation via `dbgr-suggest-invocaton'"
  (dbgr-suggest-invocation rdebug-command-name rdebug-minibuffer-history 
			   'rdebug-suggest-ruby-file))

(defun rdebug-suggest-ruby-file ()
    "Suggest a Ruby file to debug. First priority is given to the
current buffer. If the major mode is Ruby-mode, then we are
done. If the major mode is not Ruby, we'll use priority 2 and we
keep going.  Then we will try files in the default-directory. Of
those that we are visiting we will see if the major mode is Ruby,
the first one we find we will return.  Failing this, we see if the
file is executable and has a .rb suffix. These have priority 8.
Failing that, we'll go for just having a .rb suffix. These have
priority 7. And other executable files have priority 6.  Within a
given priority, we use the first one we find."
    (let* ((file)
	   (file-list (directory-files default-directory))
	   (priority 2)
	   (is-not-directory)
	   (result (buffer-file-name)))
      (if (not (rdebug-file-ruby-mode? result))
	  (while (and (setq file (car-safe file-list)) (< priority 8))
	    (setq file-list (cdr file-list))
	    (if (rdebug-file-ruby-mode? file)
		(progn 
		  (setq result file)
		  (setq priority 
			(if (file-executable-p file)
			    (setq priority 8)
			  (setq priority 7)))))
	    ;; The file isn't in a Ruby-mode buffer,
	    ;; Check for an executable file with a .rb extension.
	    (if (and file (file-executable-p file)
		     (setq is-not-directory (not (file-directory-p file))))
		(if (and (string-match "\.rb$" file))
		    (if (< priority 6)
			(progn
			  (setq result file)
			  (setq priority 6))))
	      (if (and is-not-directory (< priority 5))
		  ;; Found some sort of executable file.
		  (progn
		    (setq result file)
		    (setq priority 5))))))
      result))

(defun rdebug-goto-traceback-line (pt)
  "Display the location mentioned by the Ruby traceback line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "traceback" rdebug-pat-hash))

(defun rdebug-goto-dollarbang-traceback-line (pt)
  "Display the location mentioned by the Ruby $! traceback line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "dollar-bang" rdebug-pat-hash))

(defun rdebug-reset ()
  "Rdebug cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (rdebug-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rdebug-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun rdebug-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'rdebug-debugger-support-minor-mode minor-mode-map-alist)
;; 	  rdebug-debugger-support-minor-mode-map-when-deactive))


(defun rdebug-customize ()
  "Use `customize' to edit the settings of the `rdebug' debugger."
  (interactive)
  (customize-group 'rdebug))

(provide-me "rdebug-")

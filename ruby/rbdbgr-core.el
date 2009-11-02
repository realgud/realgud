;; -------------------------------------------------------------------
;; Dependencies.
;;
(eval-when-compile (require 'cl))
  
(defun rbdbgr-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
		       (symbol-file 'rbdbgr-directory))))
    (if file-name
	(file-name-directory file-name)
      nil)))

(setq load-path (cons nil 
		      (cons (format "%s.." (rbdbgr-directory))
				    (cons (rbdbgr-directory) load-path))))
(require 'dbgr-track)
(require 'rbdbgr-regexp)
(require 'dbgr-core)
(require 'dbgr-scriptbuf-var)
(setq load-path (cdddr load-path))


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
   '(ruby1.8 -W -C /tmp rbdbgr --emacs ./gcd.rb a b))

we might return:
   ((ruby1.8 -W -C) (rbdbgr --emacs) (./gcd.rb a b) 't)

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

      ;; Remove "rbdbgr" from "rbdbgr --rbdbgr-options script
      ;; --script-options"
      (setq debugger-name (file-name-sans-extension
			   (file-name-nondirectory (car args))))
      (unless (string-match "^rbdbgr$" debugger-name)
	(message 
	 "Expecting debugger name to be rbdbgr"
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

(defun rbdbgr-file-ruby-mode? (filename)
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

;; FIXME generalize and put into dbgr-core.el
(defun rbdbgr-suggest-invocation (debugger-name)
  "Suggest a rbdbgr command invocation. If the current buffer is
a process buffer that that `dbgr-invocation' set and we are in
rbdbgr-track-mode, then use the value of that variable. Otherwise
we try to find a suitable Ruby program using `rbdbgr-suggest-ruby-file' and
prepend the debugger name onto that. FIXME: should keep a list of
previously used invocations.
"
  (cond
   ((and (boundp 'dbgr-invocation) 
	 (boundp 'rbdbgr-track-mode) 
	 rbdbgr-track-mode)
    (let ((result (car dbgr-invocation)))
      (reduce (lambda(result, x)
		(setq result (concat result " " x)))
	      dbgr-invocation)))
  ;; FIXME: keep a list of recent invocations
   (t (concat debugger-name " " (rbdbgr-suggest-ruby-file)))))


(defun rbdbgr-suggest-ruby-file ()
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
      (if (not (rbdbgr-file-ruby-mode? result))
	  (while (and (setq file (car-safe file-list)) (< priority 8))
	    (setq file-list (cdr file-list))
	    (if (rbdbgr-file-ruby-mode? file)
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

(defun rbdbgr-query-cmdline (&optional opt-debugger opt-cmd-name)
  "Prompt for a rbdbgr debugger command invocation to run.
Analogous to `gud-query-cmdline'"
  (let ((debugger (or opt-debugger
		   (dbgr-scriptbuf-var-name rbdbgr-scriptvar))))
    (read-from-minibuffer
     (format "Run %s (like this): " debugger)
     (rbdbgr-suggest-invocation debugger))))

(defun rbdbgr-goto-traceback-line (pt)
  "Display the location mentioned by the Ruby traceback line
described by PT."
  (interactive "d")
  (goto-line-for-pt-and-type pt "traceback" rbdbgr-pat-hash))

(defun rbdbgr-goto-dollarbang-traceback-line (pt)
  "Display the location mentioned by the Ruby $! traceback line
described by PT."
  (interactive "d")
  (goto-line-for-pt-and-type pt "dollar-bang" rbdbgr-pat-hash))

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


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rbdbgr-core)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-core.el ends here

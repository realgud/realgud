; (require 'term)
(require 'comint)
(eval-when-compile (require 'cl))

(require 'load-relative)
(provide 'dbgr-core)
(load-relative "dbgr-arrow" 'dbgr-core)
(load-relative "dbgr-procbuf" 'dbgr-core)
(load-relative "dbgr-scriptbuf" 'dbgr-core)
(load-relative "dbgr-track" 'dbgr-core)

(declare-function dbgr-set-arrow (src-marker))
(declare-function dbgr-scriptbuf-init(a b c d))
(declare-function dbgr-unset-arrow())
(declare-function dbgr-proc-src-marker (a))
(declare-function dbgr-track-set-debugger (name))

(defun dbgr-parse-command-arg (args two-args opt-two-args)
  "Return a cons node where the car is a list containing the
entire first option and the cdr is the remaining arguments from ARGS.

We determine if an option has length one or two using the lists
TWO-ARGS and OPT-TWO-ARGS. Both of these are list of 'options',
that is strings without the leading dash. TWO-ARGS takes a
mandatory additional argument. OPT-TWO-ARGS might take two
arguments. The rule for an optional argument that we use is if
the next parameter starts with a dash ('-'), it is not part of
the preceeding parameter when that parameter is optional.

NOTE: we don't check whether the first arguments of ARGS is an
option by testing to see if it starts say with a dash. So on
return the first argument is always removed.
"
  (let ((arg (car args))
	(d-two-args (mapcar (lambda(x) (concat "-" x)) two-args))
	(d-opt-two-args (mapcar (lambda(x) (concat "-" x)) opt-two-args))
	(remaining (cdr args)))
    (cond 
     ((member arg d-two-args)
      (if (not remaining)
	    (progn 
	      (message "Expecting an argument after %s. Continuing anyway."
		       arg)
	      (cons (list arg) (list remaining)))
	(cons (list arg (car remaining)) (list (cdr remaining)))))
     ((member arg d-opt-two-args)
      (if (and remaining (not (string-match "^-" (car remaining))))
	  (cons (list arg (car remaining)) (list (cdr remaining)))
	(cons (list arg) (list remaining))))
     (t (cons (list arg) (list remaining))))))

(defun dbgr-term-sentinel (process string)
  (with-current-buffer (process-buffer process)
    (lexical-let ((prev-marker (dbgr-proc-src-marker (current-buffer))))
      (if prev-marker (dbgr-unset-arrow (marker-buffer prev-marker)))
      (message "That's all folks.... %s" string))))

(defun dbgr-exec-shell (debugger-name script-filename program &rest args)
  "Run the specified COMMAND in under debugger DEBUGGER-NAME a
comint process buffer. ARGS are the arguments passed to the
PROGRAM.  At the moment, no piping of input is allowed.

SCRIPT-FILENAME will have local variable `dbgr-scriptvar' set which contains
the debugger name and debugger process buffer."
  (let* ((default-directory (file-name-directory script-filename))
	 (cmdproc-buffer
	  (get-buffer-create
	   (format "*%s %s shell*" 
		   (file-name-nondirectory debugger-name)
		   (file-name-nondirectory script-filename))))
	 (dbgr-buf (current-buffer))
	 (proc (get-buffer-process cmdproc-buffer)))
    (unless (and proc (eq 'run (process-status proc)))
      (save-excursion
	(set-buffer cmdproc-buffer)
	(setq default-directory default-directory)
	(insert "Current directory is " default-directory "\n")
	
	;; For term.el
	;; (term-mode)
	;; (set (make-local-variable 'term-term-name) dbgr-term-name)
	;; (make-local-variable 'dbgr-parent-buffer)
	;; (setq dbgr-parent-buffer dbgr-buf)
	
	;; For comint.el. 
	(comint-mode)
	(comint-exec cmdproc-buffer debugger-name program nil args)
	
	(setq proc (get-buffer-process cmdproc-buffer))

	(if (and proc (eq 'run (process-status proc)))
	    (let ((src-buffer (find-file-noselect script-filename))
		  (cmdline-list (cons program args)))
	      (dbgr-track-set-debugger debugger-name)
	      (set-process-sentinel proc 'dbgr-term-sentinel)
	      (point-max)
	      (dbgr-scriptbuf-init src-buffer cmdproc-buffer 
				   debugger-name cmdline-list))
	  (error "Failed to invoke shell command")) ;; FIXME: add more info
	(process-put proc 'buffer cmdproc-buffer)))
    cmdproc-buffer))

;; Start of a term-output-filter for term.el
(defun dbgr-term-output-filter (process string)
  (let ((process-buffer (process-get process 'buffer)))
    (if process-buffer
	(save-current-buffer
	  (set-buffer process-buffer)
	  ;; (insert-before-markers (format "+++1 %s" string))
	  (insert-before-markers string)))))

;; -------------------------------------------------------------------
;; The end.
;;

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-core.el ends here

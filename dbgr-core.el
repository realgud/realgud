(require 'term)

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
      (if remaining (cdr remaining)
	  (progn 
	    (message "Expecting an argument after %s. Continuing anyway."
		     arg)
	    (cons (list arg) remaining))))
     ((member arg d-opt-two-args)
      (if (and remaining (not (string-match "^-" (car remaining))))
	  (cons (list arg (car remaining)) (cdr remaining))
	(cons (list arg) remaining)))
     (t (cons (list arg) remaining)))))

(defun dbgr-term-sentinel (proc string)
  (message "TTTThat's all folks.... %s" string)
)

(defun dbgr-exec-shell (debugger-name program &rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.
ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (let* ((term-buf
	  (generate-new-buffer
	   (format "*%s %s shell*" 
		   (file-name-nondirectory debugger-name)
		   (file-name-nondirectory program) )))
	 (dbgr-buf (current-buffer)))
    (save-current-buffer
      (switch-to-buffer term-buf)
      (term-mode)
      ;; (set (make-local-variable 'term-term-name) dbgr-term-name)
      ;; (make-local-variable 'dbgr-parent-buffer)
      ;; (setq dbgr-parent-buffer dbgr-buf)
      (term-exec term-buf debugger-name debugger-name nil (cons program args))
      (let ((proc (get-buffer-process term-buf)))
	(if (and proc (eq 'run (process-status proc)))
	    (set-process-sentinel proc 'dbgr-term-sentinel)
	  (error "Failed to invoke shell command")) ;; FIXME: add more info
	proc))))

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'dbgr-core)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-core.el ends here

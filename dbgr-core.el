(require 'term)

(defun dbgr-strip-command-arg (args two-args opt-two-args)
  "return ARGS with the first argument, an 'option'
removed. 

However if that option argument may take another argument, remove
that as well. TWO-ARGS is list of options (strings without the
leading dash) that take a mandatory additional
argument. OPT-TWO-ARGS is a list of options might take two
arguments. The rule for an optional argument we use: if the next
parameter starts with a dash ('-'), it is not part of the
preceeding parameter when that parameter is optional.

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
	    remaining)))
     ((member arg d-opt-two-args)
      (if (and remaining (not (string-match "^-" (car remaining))))
	  (cdr remaining)
	remaining))
     (t remaining))))

(defun dbgr-term-sentinel (proc string)
  (message "TTTThat's all folks.... %s" string)
)

(defun dbgr-exec-shell (debugger-name program &rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.
ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (let* ((term-buf
	  (generate-new-buffer
	   (format "*%s %s*" 
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
	  (error "Failed to invoke visual command")))))
  nil)

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'dbgr-core)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-core.el ends here

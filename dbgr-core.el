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

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'dbgr-core)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-core.el ends here

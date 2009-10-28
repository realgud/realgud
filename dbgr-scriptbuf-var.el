;;; rbdbg-scriptbuf-var.el --- debugger variables (other than regexps) 
;;; for a script to be debugged.
(eval-when-compile (require 'cl))

(defstruct rbdbg-scriptbuf-var
  "debugger object/structure specific to a (top-level) Ruby file
to be debugged."
  (name       :type string) ;; Name of debugger
  (cmd        :type string) ;; Debugger command invocation. FIXME: turn
                            ;; into a ring of recent invocations.
  (cmdproc)                 ;; buffer containing debugger process
)

(defvar rbdbgr-scriptvar (make-rbdbg-scriptbuf-var
		    :name "rbdbgr"
		    :cmd  "rbdbgr"
		    :cmdproc  nil
		    )
  "Debugger object for a process buffer.")
(make-variable-buffer-local 'rbdbgr-dbgr-scriptbuf-var)

(provide 'rbdbg-scriptbuf-var)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-scriptbuf-var.el ends here

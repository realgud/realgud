;;; rbdbg-scriptbuf-var.el --- debugger variables (other than regexps) 
;;; for a script to be debugged.
(eval-when-compile (require 'cl))


(defstruct dbg-scriptbuf-var
  "debugger object/structure specific to a (top-level) Ruby file
to be debugged."
  (name    nil)   ;; Name of debugger
  (cmd     nil)   ;; Debugger command invocation or this file was a
		  ;; main routine, or nil if not.
  (cmdproc nil)   ;; buffer of the associated debugger process
  (cur-pos nil)   ;; If not nil, the debugger thinks we are currently 
                  ;; positioned at a corresponding place in the program.
  ;; FILL IN THE FUTURE
  ;;(brkpt-alist '())  ;; alist of breakpoints the debugger has referring
                       ;; to this buffer. Each item is (brkpt-name . marker)
  ;;(loc-alist  '())   ;; alist of locations that the debugger has stopped
                       ;; on at some point in the past. Each item is
                       ;; (line-number . marker)
  ;; 
)

;; FIXME: support a list of dbgr-scriptvar's since we want to allow
;; a source buffer to potentially participate in several debuggers
;; which might be active.

(defvar dbgr-scriptvar (make-dbg-scriptbuf-var
		    :name "rbdbgr"
		    :cmd  "rbdbgr"
		    :cmdproc  nil
		    :cur-pos  nil
		    )
  "Debugger object for a process buffer.")
(make-variable-buffer-local 'rbdbgr-dbgr-scriptbuf-var)

(provide 'dbgr-scriptbuf-var)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-scriptbuf-var.el ends here

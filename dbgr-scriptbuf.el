;;; dbgr-scriptbuf.el --- code for a source-code buffer
(eval-when-compile (require 'cl))


(defstruct dbgr-scriptbuf-info
  "debugger object/structure specific to a (top-level) Ruby file
to be debugged."
  (name    nil)   ;; Name of debugger
  (cmd     nil)   ;; Debugger command invocation as a list of strings 
		  ;; or nil.
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

(make-variable-buffer-local 'dbgr-scriptbuf-info)

(defun dbgr-scriptbuf-init 
  (src-buffer cmdproc-buffer debugger-name cmdline-list)
  "Initialize SRC-BUFFER as a source-code buffer for a debugger.
CMDPROC-BUFFER is the process buffer containing the debugger. 
DEBUGGER-NAME is the name of the debugger.
as a main program."
  (save-excursion
    (set-buffer src-buffer)
    (put 'dbgr-scriptbuf-info 'variable-documentation 
	 "Debugger information for a buffer containing source code.")
    (setq dbgr-scriptbuf-info
	 (make-dbgr-scriptbuf-info
	  :name debugger-name
	  :cmd  cmdline-list
	  :cmdproc cmdproc-buffer))))

(defun dbgr-scriptbuf-command-string()
  "Get the command string invocation for this source buffer"
   (cond 
    ((and (boundp 'dbgr-scriptbuf-info) dbgr-scriptbuf-info
	  (dbgr-scriptbuf-info-cmd dbgr-scriptbuf-info))
     (mapconcat (lambda(x) x) 
		(dbgr-scriptbuf-info-cmd dbgr-scriptbuf-info)
		" "))
    (t nil)))

(provide 'dbgr-scriptbuf)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-scriptbuf.el ends here

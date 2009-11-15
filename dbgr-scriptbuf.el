;;; dbgr-scriptbuf.el --- code for a source-code buffer
(eval-when-compile 
  (require 'cl)
  (defvar dbgr-scriptbuf-info) ;; is buffer local
  (defvar dbgr-cmdbuf-info)        ;; in procbuf is buffer local
  (defvar cl-struct-dbgr-cmdbuf-info-tags) ;; Why do we need this?
  )


(defstruct dbgr-scriptbuf-info
  "debugger object/structure specific to a (top-level) Ruby file
to be debugged."
  (debugger-name) ;; Name of debugger. We could get this from
                  ;; the process command buffer, but we want to 
                  ;; store it here in case the command buffer
                  ;; disappears
  (cmd-args)      ;; Debugger command invocation as a list of strings 
		  ;; or nil. See above about why we don't get from
                  ;; the process command buffer.
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
(defalias 'dbgr-scriptbuf-info? 'dbgr-scriptbuf-p)

(require 'load-relative)
(require-relative "dbgr-helper")

(defun dbgr-scriptbuf? (buffer)
  "Return true if BUFFER is a debugger script buffer."
  (and (boundp 'dbgr-scriptbuf-info) 
	     (dbgr-scriptbuf-info? dbgr-scriptbuf-info)))

;; FIXME: DRY = access via a macro
(defun dbgr-scriptbuf-info-cmdproc=(info buffer)
  (setf (dbgr-scriptbuf-info-cmdproc info) buffer))

(defun dbgr-scriptbuf-info-debugger-name=(info value)
  (setf (dbgr-scriptbuf-info-debugger-name info) value))

(defun dbgr-scriptbuf-info-cmd-args=(info buffer)
  (setf (dbgr-scriptbuf-info-cmd-args info) buffer))

(declare-function fn-p-to-fn?-alias(sym))
(fn-p-to-fn?-alias 'dbgr-scriptbuf-info-p)
(declare-function dbgr-scriptbuf-info?(var))
(declare-function dbgr-cmdbuf-info-name(cmdbuf-info))

;; FIXME: support a list of dbgr-scriptvar's since we want to allow
;; a source buffer to potentially participate in several debuggers
;; which might be active.

(make-variable-buffer-local 'dbgr-scriptbuf-info)

(defun dbgr-scriptbuf-init 
  (src-buffer cmdproc-buffer debugger-name cmd-args)
  "Initialize SRC-BUFFER as a source-code buffer for a debugger.
CMDPROC-BUFFER is the process buffer containing the debugger. 
DEBUGGER-NAME is the name of the debugger.
as a main program."
  (with-current-buffer cmdproc-buffer
    (set-buffer src-buffer)
    (set (make-local-variable 'dbgr-scriptbuf-info)
	 (make-dbgr-scriptbuf-info
	  :debugger-name debugger-name
	  :cmd-args cmd-args
	  :cmdproc cmdproc-buffer))
    (put 'dbgr-scriptbuf-info 'variable-documentation 
	 "Debugger information for a buffer containing source code.")))

(defun dbgr-scriptbuf-init-or-update
  (src-buffer cmdproc-buffer)
  "Call `dbgr-scriptbuf-init' for SRC-BUFFER update `dbgr-scriptbuf-info' variables
in it with those from CMDPROC-BUFFER"
  (let ((debugger-name)
	(cmd-args))
   (with-current-buffer cmdproc-buffer
     (setq debugger-name (dbgr-cmdbuf-info-name dbgr-cmdbuf-info))
     (setq cmd-args (dbgr-cmdbuf-info-cmd-args dbgr-cmdbuf-info)))
  (with-current-buffer src-buffer
    (if (dbgr-scriptbuf-info? dbgr-scriptbuf-info)
	(progn
	  (dbgr-scriptbuf-info-cmdproc= dbgr-scriptbuf-info cmdproc-buffer)
	  (dbgr-scriptbuf-info-debugger-name= dbgr-scriptbuf-info debugger-name)
	  (dbgr-scriptbuf-info-cmd-args= dbgr-scriptbuf-info cmd-args)
	  )
      (dbgr-scriptbuf-init src-buffer cmdproc-buffer "unknown" nil)))))

(defun dbgr-scriptbuf-command-string(src-buffer)
  "Get the command string invocation for this source buffer"
  (with-current-buffer src-buffer
    (cond 
     ((and (dbgr-scriptbuf? src-buffer)
	   (dbgr-scriptbuf-info-cmd-args dbgr-scriptbuf-info))
      (mapconcat (lambda(x) x) 
		 (dbgr-scriptbuf-info-cmd-args dbgr-scriptbuf-info)
		 " "))
     (t nil))))
  
(provide 'dbgr-scriptbuf)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-scriptbuf.el ends here

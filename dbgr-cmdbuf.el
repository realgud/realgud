;;; dbgr-cmdbuf.el --- debugger process buffer things
(eval-when-compile 
  (require 'cl)
  (defvar dbgr-cmdbuf-info)
  (defvar dbgr-loc-hist-size) ;; in dbgr-lochist
  (defvar cl-struct-dbgr-loc-tags) ;; why do we need??
  (declare-function dbgr-unset-arrow (marker))
  )
(declare-function dbgr-loc-hist-item (item))
(declare-function dbgr-loc-marker (loc))
(declare-function make-dbgr-loc-hist ())

(defstruct dbgr-cmdbuf-info
  "The debugger object/structure specific to a process buffer."
  name         ;; Name of debugger
  cmdline      ;; Command-line invocation
  loc-regexp   ;; Location regular expression string
  ;; FIXME: use include?
  file-group
  line-group
  loc-hist     ;; ring of locations seen in the course of execution
               ;; see dbgr-lochist
)

(make-variable-buffer-local 'dbgr-cmdbuf-info)

(provide 'dbgr-cmdbuf)
(require 'load-relative)
(load-relative '("dbgr-arrow" "dbgr-lochist" "dbgr-loc") 'dbgr-cmdbuf)

(defun dbgr-cmdbuf-init
  (proc-buffer &optional debugger-name loc-regexp file-group line-group)
  "Initialize PROC-BUFFER for a working with a debugger.
DEBUGGER-NAME is the name of the debugger.
as a main program."
  (with-current-buffer proc-buffer
    (setq dbgr-cmdbuf-info
	  (make-dbgr-cmdbuf-info
	   :name (or debugger-name "unknown-debugger-name")
	   :loc-regexp loc-regexp
	   :file-group (or file-group -1)
	   :line-group (or line-group -1)
	   :loc-hist (make-dbgr-loc-hist)))
    (put 'dbgr-cmdbuf-info 'variable-documentation 
	 "Debugger object for a process buffer.")))

(defun dbgr-proc-debugger-name(proc-buff)
  "Return the debugger name recorded in the debugger process buffer."
  (with-current-buffer proc-buff (dbgr-cmdbuf-info-name dbgr-cmdbuf-info))
)

(defun dbgr-proc-loc-hist(proc-buff)
  "Return the history ring of locations that a debugger process has stored."
  (with-current-buffer proc-buff (dbgr-cmdbuf-info-loc-hist dbgr-cmdbuf-info))
)

(defun dbgr-proc-src-marker(proc-buff)
  "Return a marker to current source location stored in the history ring."
  (with-current-buffer proc-buff
    (lexical-let* ((loc (dbgr-loc-hist-item (dbgr-proc-loc-hist proc-buff))))
      (and loc (dbgr-loc-marker loc)))))

(provide 'dbgr-cmdbuf)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-cmdbuf.el ends here

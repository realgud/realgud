;;; dbgr-cmdbuf.el --- debugger process buffer things
(require 'cl)
(eval-when-compile 
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
  cmd-args      ;; Command-line invocation arguments
  loc-regexp   ;; Location regular expression string
  ;; FIXME: use include?
  file-group
  line-group
  loc-hist     ;; ring of locations seen in the course of execution
               ;; see dbgr-lochist
)
(defalias 'dbgr-cmdbuf-info? 'dbgr-cmdbuf-info-p)

(make-variable-buffer-local 'dbgr-cmdbuf-info)

(provide 'dbgr-cmdbuf)
(require 'load-relative)
(dolist (rel-file '("dbgr-arrow" "dbgr-lochist" "dbgr-loc"))
  (require-relative rel-file))

(defun dbgr-cmdbuf? (buffer)
  "Return true if BUFFER is a debugger command buffer."
  (and (boundp 'dbgr-cmdbuf-info) 
	     (dbgr-cmdbuf-info? dbgr-cmdbuf-info)))

;; FIXME: DRY = access via a macro. See also analogous
;; code in dbgr-scriptbuf
(defun dbgr-cmdbuf-info-cmd-args=(info value)
  (setf (dbgr-cmdbuf-info-cmd-args info) value))

(defun dbgr-cmdbuf-command-string(cmd-buffer)
  "Get the command string invocation for this command buffer"
  (with-current-buffer cmd-buffer
    (cond
     ((and (boundp 'dbgr-cmdbuf-info) dbgr-cmdbuf-info)
      (let* 
	  ((cmd-args (dbgr-cmdbuf-info-cmd-args dbgr-cmdbuf-info))
	   (result (car cmd-args)))
	(and cmd-args 
	     (reduce (lambda(result x)
		       (setq result (concat result " " x)))
		     cmd-args))))
     (t nil))))

(defun dbgr-cmdbuf-init
  (cmd-buf &optional debugger-name loc-regexp file-group line-group)
  "Initialize CMD-BUFER for a working with a debugger.
DEBUGGER-NAME is the name of the debugger.
as a main program."
  (with-current-buffer cmd-buf
    (setq dbgr-cmdbuf-info
	  (make-dbgr-cmdbuf-info
	   :name (or debugger-name "unknown-debugger-name")
	   :loc-regexp loc-regexp
	   :file-group (or file-group -1)
	   :line-group (or line-group -1)
	   :loc-hist (make-dbgr-loc-hist)))
    (put 'dbgr-cmdbuf-info 'variable-documentation 
	 "Debugger object for a process buffer.")))

(defun dbgr-proc-debugger-name(cmd-buf)
  "Return the debugger name recorded in the debugger process buffer."
  (with-current-buffer cmd-buf (dbgr-cmdbuf-info-name dbgr-cmdbuf-info))
)

(defun dbgr-proc-loc-hist(cmd-buf)
  "Return the history ring of locations that a debugger process has stored."
  (with-current-buffer cmd-buf (dbgr-cmdbuf-info-loc-hist dbgr-cmdbuf-info))
)

(defun dbgr-proc-src-marker(cmd-buf)
  "Return a marker to current source location stored in the history ring."
  (with-current-buffer cmd-buf
    (lexical-let* ((loc (dbgr-loc-hist-item (dbgr-proc-loc-hist cmd-buf))))
      (and loc (dbgr-loc-marker loc)))))

(provide 'dbgr-cmdbuf)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-cmdbuf.el ends here

;;; dbgr-procbuf.el --- debugger process buffer things
(eval-when-compile (require 'cl))
(require 'cl)

(defstruct dbgr-info
  "The debugger object/structure specific to a process buffer."
  (name       :type string) ; Name of debugger
  (loc-regexp :type string) ; Location regular expression string
  ; FIXME: use include?
  (file-group :type integer)
  (line-group :type integer)
  (loc-hist)    ; ring of locations seen in the course of execution
              ; see dbgr-lochist
)

(make-variable-buffer-local 'dbgr-info)


(defun dbgr-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'dbgr-procbuf))))
    (if file-name
        (file-name-directory file-name)
      nil)))

(eval-when-compile (require 'cl))
(setq load-path (cons nil (cons (dbgr-directory) load-path)))
(load "dbgr-loc")
(setq load-path (cddr load-path))

(declare-function make-dbgr-loc-hist ())

(defun dbgr-procbuf-init 
  (proc-buffer &optional debugger-name loc-regexp file-group line-group)
  "Initialize PROC-BUFFER for a working with a debugger.
DEBUGGER-NAME is the name of the debugger.
as a main program."
  (with-current-buffer proc-buffer
    (setq dbgr-info
	  (make-dbgr-info
	   :name (or debugger-name "unknown-debugger-name")
	    :loc-regexp loc-regexp
	    :file-group (or file-group -1)
	    :line-group (or line-group -1)
	    :loc-hist   (make-dbgr-loc-hist)))
    (put 'dbgr-info 'variable-documentation 
	 "Debugger object for a process buffer.")))

(defun dbgr-proc-debugger-name(proc-buff)
  "Return the debugger name recorded in the debugger process buffer."
  (with-current-buffer proc-buff (dbgr-info-name dbgr-info))
)

(defun dbgr-proc-loc-hist(proc-buff)
  "Return the history ring of locations that a debugger process has stored."
  (with-current-buffer proc-buff (dbgr-info-loc-hist dbgr-info))
)

(defun dbgr-proc-src-marker(proc-buff)
  "Return a marker to current source location stored in the history ring."
  (with-current-buffer proc-buff
    (lexical-let* ((loc (dbgr-loc-hist-item (dbgr-proc-loc-hist proc-buff))))
      (and loc (dbgr-loc-marker loc)))))

(provide 'dbgr-procbuf)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-procbuf.el ends here

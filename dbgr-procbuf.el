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
(defvar dbgr-info (make-dbgr-info
		    :name "unknown-debugger-name"
		    :loc-regexp nil
		    :file-group -1
		    :line-group -1
		    :loc-hist   nil)
  "Debugger object for a process buffer.")

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

(defun dbgr-proc-loc-hist()
  "Return the history ring of locations that a debugger process has stored."
  (dbgr-info-loc-hist dbgr-info)
)

(defun dbgr-proc-src-marker()
  "Return a marker to current source location stored in the history ring."
  (lexical-let* ((loc (dbgr-loc-hist-item (dbgr-proc-loc-hist))))
    (and loc (dbgr-loc-marker loc))))

(provide 'dbgr-procbuf)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-procbuf.el ends here

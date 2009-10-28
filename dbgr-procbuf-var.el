;;; dbgr-procbuf-var.el --- debugger variables (other than regexps) for
;;; a process buffer
(eval-when-compile (require 'cl))

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

(provide 'dbgr-procbuf-var)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-procbuf-var.el ends here

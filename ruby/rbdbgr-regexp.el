;;; rdebug-regexp.el --- Ruby debugger regular expressions

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;
(eval-when-compile (require 'cl))

(defun rbdbgr-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'rbdbgr-core))))
    (if file-name
        (file-name-directory file-name)
      nil)))

(setq load-path (cons nil 
		      (cons (format "%s.." (rbdbgr-directory))
				    (cons (rbdbgr-directory) load-path))))
(load "dbgr-regexp") ; for make-dbgr-loc-pat
(setq load-path (cdddr load-path))


(defvar rbdbgr-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match: traceback, prompt, etc. 
The values of a hash entry is a dbgr-dbgr-loc-pat struct")

;;  Regular expression that describes a rbdbgr command prompt
(setf (gethash "prompt" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp ".. (.*?\\(?:via \\)?\\([-a-zA-Z0-9_/.]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a Ruby traceback line.
(setf (gethash "traceback" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a Ruby $! string
(setf (gethash "dollar-bang" rbdbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2))


(provide 'rbdbgr-regexp)

;;; Local variables:
;;; eval:(put 'rbdbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rbdbgr-regexp.el ends here

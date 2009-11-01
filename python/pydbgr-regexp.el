;;; rdebug-regexp.el --- Ruby debugger regular expressions

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;
(eval-when-compile (require 'cl))

(defun pydbgr-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'pydbgr-core))))
    (if file-name
        (file-name-directory file-name)
      nil)))

(setq load-path (cons nil 
		      (cons (format "%s.." (pydbgr-directory))
				    (cons (pydbgr-directory) load-path))))
(load "dbgr-regexp") ; for make-dbgr-loc-pat
(setq load-path (cdddr load-path))


(defvar pydbgr-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match: traceback, prompt, etc. 
The values of a hash entry is a dbgr-dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression used to find a file location given by pydbgr.
;;
;; Program-location lines look like this:
;;   (/usr/bin/zonetab2pot.py:15): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>
;;  and in tracebacks like this:
;;   (/usr/bin/zonetab2pot.py:15)

(setf (gethash "prompt" pydbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;;  Regular expression that describes a Ruby traceback line.
(setf (gethash "traceback" pydbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^[ \t]+File \"\\(.+\\)\", line \\([0-9]+\\)"
       :file-group 1
       :line-group 2))

(provide 'pydbgr-regexp)

;;; Local variables:
;;; eval:(put 'pydbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; pydbgr-regexp.el ends here

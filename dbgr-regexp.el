;;; dbgr-regexp.el --- Debugger regular expressions for many kinds of
;;  debuggers

;; Here we have regular expressions and names for matched patterns
;; of those regular expressions.

;;; Code:

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;

(defstruct dbgr-loc-pat
  "Information to match and extract a file and line number location from
a string output by a debugger inside a process shell"
  (regexp)
  (file-group)
  (line-group))

(defvar dbgr-pat-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string. The values of a hash entry
  is a dbgr-loc-pat struct")

;; FIXME? If this file gets too long and cumbersome, split each debugger
;; section to its own file.

; Create one for the Ruby 1.9 debugger "rbdbgr". 
(setf (gethash "rbdbgr" dbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp ".. (\\(?:.+ \\(?:via\\|remapped\\) \\)?\\(.+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

; Create one for ruby-debug
(setf (gethash "rdebug" dbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:source \\)?\\(\\(?:[a-zA-Z]:\\)?\\(?:.+\\)\\):\\([0-9]+\\).*\n"
       :file-group 1
       :line-group 2))

;; Regular expression used to find a file location given by pydbgr.
;;
;; Program-location lines look like this:
;;   (/usr/bin/zonetab2pot.py:15): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>
;;  and in tracebacks like this:
;;   (/usr/bin/zonetab2pot.py:15)
(setf (gethash "pydbgr" dbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

; And the older Python debugger "pydb". 
(setf (gethash "pydb" dbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

; Create one for the Korn Shell debugger "kshdb". 
(setf (gethash "kshdb" dbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

; And Z Shell debugger "zshdb". 
(setf (gethash "zshdb" dbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

; One for the Bash debugger "bashdb". 
(setf (gethash "bashdb" dbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\))"
       :file-group 2
       :line-group 3))

; And finally for GNU Make + debugger "remake". 
(setf (gethash "remake" dbgr-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:^\\|\n\\)(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

(provide 'dbgr-regexp)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-regexp.el ends here

;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for Z shell debugger: remake 

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp" 
			 "../../common/loc" 
			 "../../common/init") 
		       "dbgr-")
(require-relative-list '("../../lang/posix-shell") "dbgr-lang-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar dbgr-remake-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
dbgr-loc-pat struct")

;; Regular expression that describes a remake location generally shown
;; before a command prompt.
;; For example:
;; -- (emacs-dbgr/dbgr/debugger/Makefile:168)
(setf (gethash "loc" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:^\\|\n\\)\\(?:.. \\)?(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\))"
       :file-group 1
       :line-group 2))

;; For example: 
;;   remake<10>
;;   remake<(5)> 
;;   remake<<1>>
(setf (gethash "prompt" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^remake<\\([0-9]\\)+> "
       :num 1
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) on target \\([^:]*\\): file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 3
       :line-group 4))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" dbgr-remake-pat-hash)
      (make-dbgr-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) on target .* cleared\n"
       :num 1))

;; FIXME: add these.
;; ;; Regular expression that describes a debugger "backtrace" command line.
;; ;; For example:
;; ;;   ->0 in file `/etc/apparmor/fns' at line 24
;; ;;   ##1 /etc/apparmor/fns called from file `/etc/init.d/apparmor' at line 35
;; ;;   ##2 /etc/init.d/apparmor called from file `/usr/bin/remake' at line 129
;; (setf (gethash "frame" dbgr-remake-pat-hash)
;;       (make-dbgr-loc-pat
;;        :regexp 	(concat dbgr-shell-frame-start-regexp
;; 			dbgr-shell-frame-num-regexp "[ ]?"
;; 			"\\(.*\\)"
;; 			dbgr-shell-frame-file-regexp
;; 			"\\(?:" dbgr-shell-frame-line-regexp "\\)?"
;; 			)
;;        :num 2
;;        :file-group 4
;;        :line-group 5)
;;       )

;; (setf (gethash "font-lock-keywords" dbgr-remake-pat-hash)
;;       '(
;; 	;; The frame number and first type name, if present.
;; 	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
;; 	;;      --^-
;; 	("^\\(->\\|##\\)\\([0-9]+\\) "
;; 	 (2 dbgr-backtrace-number-face))

;; 	;; File name.
;; 	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
;; 	;;          ---------^^^^^^^^^^^^^^^^^^^^-
;; 	("[ \t]+\\(in\\|from\\) file `\\(.+\\)'"
;; 	 (2 dbgr-file-name-face))

;; 	;; File name.
;; 	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
;; 	;;                                         --------^^
;; 	;; Line number.
;; 	("[ \t]+at line \\([0-9]+\\)$"
;; 	 (1 dbgr-line-number-face))
;; 	;; (trepan-frames-match-current-line
;; 	;;  (0 trepan-frames-current-frame-face append))
;; 	))

(setf (gethash "remake" dbgr-pat-hash) dbgr-remake-pat-hash)

(provide-me "dbgr-remake-")

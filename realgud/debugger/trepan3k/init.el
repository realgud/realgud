;;; Copyright (C) 2010-2015 Rocky Bernstein <rocky@gnu.org>
;;; trepan3k: Python 3.2 and beyond

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/python") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:trepan3k-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc 'realgud-loc)

;; Regular expression that describes a trepan3k location generally shown
;; before a command prompt.
;;
;; For example:
;;   (/usr/bin/zonetab2pot.py:15): <module>
;;   (/usr/bin/zonetab2pot.py:15 remapped <string>): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>
(setf (gethash "loc" realgud:trepan3k-pat-hash)
      realgud:python-trepan-loc-pat)

(setf (gethash "prompt" realgud:trepan3k-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(trepan3k) "
       ))

;; Regular expression that describes a trepan3k backtrace line.
;; For example:
;; ->0 get_distribution(dist='trepan==0.3.9')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 341
;; ##1 load_entry_point(dist='tr=0.3.9', group='console_scripts', name='tr')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 351
;; ##2 <module> exec()

(setf (gethash "debugger-backtrace" realgud:trepan3k-pat-hash)
      realgud:python-trepan-backtrace-pat)

;;  Regular expression that describes a Python backtrace line.
(setf (gethash "lang-backtrace" realgud:trepan3k-pat-hash)
      realgud-python-backtrace-loc-pat)

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:trepan3k-pat-hash)
      realgud:python-trepan-brkpt-set-pat)

;;  Regular expression that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" realgud:trepan3k-pat-hash)
      realgud:python-trepan-brkpt-del-pat)

;; Regular expression for a termination message.
(setf (gethash "termination" realgud:trepan3k-pat-hash)
       "^trepan3k: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:trepan3k-pat-hash)
      realgud:python-debugger-font-lock-keywords)

(setf (gethash "trepan3k" realgud-pat-hash) realgud:trepan3k-pat-hash)

(defvar realgud:trepan3k-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'shell' and the value is
  the trepan3k command to use, like 'python'")

(setf (gethash "shell" realgud:trepan3k-command-hash) "python")
(setf (gethash "until" realgud-command-hash) "continue %l")
(setf (gethash "trepan3k" realgud-command-hash) realgud:trepan3k-command-hash)

(provide-me "realgud:trepan3k-")

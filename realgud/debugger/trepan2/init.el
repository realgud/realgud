;;; Copyright (C) 2010-2012, 2014-2015 Rocky Bernstein <rocky@gnu.org>
;;; trepan2: Python 2.5 but less than 3K

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/python") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:trepan2-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc 'realgud-loc)

(setf (gethash "loc-callback-fn" realgud:trepan2-pat-hash) 'realgud:trepan2-loc-fn-callback)

;; Regular expression that describes a trepan2 location generally shown
;; before a command prompt.
;;
;; For example:
;;   (/usr/bin/zonetab2pot.py:15 @3): <module>
;;   (/usr/bin/zonetab2pot.py:15 remapped <string>): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>
(setf (gethash "loc" realgud:trepan2-pat-hash)
      realgud:python-trepan-loc-pat)

(setf (gethash "prompt" realgud:trepan2-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(trepan2) "
       ))

;; Regular expression that describes a trepan2 backtrace line.
;; For example:
;; ->0 get_distribution(dist='trepan==0.3.9')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 341
;; ##1 load_entry_point(dist='tr=0.3.9', group='console_scripts', name='tr')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 351
;; ##2 <module> exec()

(setf (gethash "debugger-backtrace" realgud:trepan2-pat-hash)
      realgud:python-trepan-backtrace-pat)

;;  Regular expression that describes a Python backtrace line.
(setf (gethash "lang-backtrace" realgud:trepan2-pat-hash)
      realgud-python-backtrace-loc-pat)

;;  Regular expression that describes location in a pytest error
(setf (gethash "pytest-error" realgud:trepan2-pat-hash)
      realgud-pytest-error-loc-pat)

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:trepan2-pat-hash)
      realgud:python-trepan-brkpt-set-pat)

;;  Regular expression that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" realgud:trepan2-pat-hash)
      realgud:python-trepan-brkpt-del-pat)

;; Regular expression for a termination message.
(setf (gethash "termination" realgud:trepan2-pat-hash)
      "^trepan2: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:trepan2-pat-hash)
      realgud:python-debugger-font-lock-keywords)

(setf (gethash "trepan2" realgud-pat-hash) realgud:trepan2-pat-hash)

(defvar realgud:trepan2-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'shell' and the value is
  the trepan2 command to use, like 'python'")

(setf (gethash "shell" realgud:trepan2-command-hash) "python")
(setf (gethash "until" realgud:trepan2-command-hash) "continue %l")

;; If your version of trepan2 doesn't support "quit!",
;; get a more recent version of trepan2
(setf (gethash "quit" realgud:trepan2-command-hash) "quit!")

(setf (gethash "trepan2" realgud-command-hash) realgud:trepan2-command-hash)

(provide-me "realgud:trepan2-")

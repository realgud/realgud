;; Copyright (C) 2010-2012, 2014-2017, 2019, 2020 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; trepan2: Python 2.5 - 2.7; for 3.0+ see trepan3k

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/python") "realgud-lang-")

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud:trepan2-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc 'realgud-loc)

(setf (gethash "loc-callback-fn" realgud:trepan2-pat-hash) 'realgud:trepan2-loc-fn-callback)

;; realgud-loc-pat that describes a trepan2 location generally shown
;; before a command prompt.
;;
;; For example:
;;   (/usr/bin/zonetab2pot.py:15 @3): <module>
;;   (/usr/bin/zonetab2pot.py:15 remapped <string>): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>
(setf (gethash "loc" realgud:trepan2-pat-hash)
      realgud:python-trepan-loc-pat)

;; An initial list of regexps that don't generally have files
;; associated with them and therefore we should not try to find file
;; associations for them.  This list is used to seed a field of the
;; same name in the cmd-info structure inside a command buffer. A user
;; may add additional files to the command-buffer's re-ignore-list.
(setf (gethash "ignore-re-file-list" realgud:trepan2-pat-hash)
      (list realgud-python-ignore-file-re))

;; realgud-loc-pat that describes a trepan2 prompt.
;; Note: the prompt in nested debugging
;; For example:
;; (trepan2)
;; ((trepan2))
;; ((trepan2:Server Thread-11))
(setf (gethash "prompt" realgud:trepan2-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(+trepan2\\(?:[:].+\\)?)+ "
       ))

;; realgud-loc-pat that describes a trepan2 backtrace line.
;; For example:
;; ->0 get_distribution(dist='trepan==0.3.9')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 341
;; ##1 load_entry_point(dist='tr=0.3.9', group='console_scripts', name='tr')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 351
;; ##2 <module> exec()

(setf (gethash "debugger-backtrace" realgud:trepan2-pat-hash)
      realgud:python-trepan-backtrace-pat)

;; realgud-loc-pat that describes a line a Python "info break" line.
;; For example:
;; 1   breakpoint    keep y   at /usr/local/bin/trepan2k:7
(setf (gethash "debugger-breakpoint" realgud:trepan2-pat-hash)
      realgud-python-breakpoint-pat)

;;  realgud-loc-pat that describes a Python backtrace line.
(setf (gethash "lang-backtrace" realgud:trepan2-pat-hash)
      realgud-python-backtrace-loc-pat)

;;  realgud-loc-pat expression that describes location in a pytest error
(setf (gethash "pytest-error" realgud:trepan2-pat-hash)
      realgud-pytest-error-loc-pat)

;;  realgud-loc-pat that describes location in a flake8 message
(setf (gethash "flake8-msg" realgud:trepan2-pat-hash)
      realgud-flake8-msg-loc-pat)

;;  realgud-loc-pat that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud:trepan2-pat-hash)
      realgud:python-trepan-brkpt-set-pat)

;;  realgud-loc-pat that describes a "delete breakpoint" line
(setf (gethash "brkpt-del" realgud:trepan2-pat-hash)
      realgud:python-trepan-brkpt-del-pat)

;; realgud-loc-pat that describes a debugger "disable" (breakpoint) response.
;; For example:
;;   Breakpoint 4 disabled.
(setf (gethash "brkpt-disable" realgud:trepan2-pat-hash)
      realgud:python-trepan-brkpt-disable-pat)

;; realgud-loc-pat that describes a debugger "enable" (breakpoint) response.
;; For example:
;;   Breakpoint 4 enabled.
(setf (gethash "brkpt-enable" realgud:trepan2-pat-hash)
      realgud:python-trepan-brkpt-disable-pat)

;; realgud-loc-pat for a termination message.
(setf (gethash "termination" realgud:trepan2-pat-hash)
      "^trepan2: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud:trepan2-pat-hash)
      realgud:python-debugger-font-lock-keywords)

(setf (gethash "font-lock-breakpoint-keywords" realgud:trepan2-pat-hash)
      realgud:python-debugger-font-lock-breakpoint-keywords)

(setf (gethash "trepan2" realgud-pat-hash) realgud:trepan2-pat-hash)

(defvar realgud:trepan2-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'shell' and the value is
  the trepan2 command to use, like 'python'")

(setf (gethash "eval"             realgud:trepan2-command-hash) "eval %s")
(setf (gethash "pprint"           realgud:trepan2-command-hash) "pp %s")
(setf (gethash "info-breakpoints" realgud:trepan2-command-hash) "info break")
(setf (gethash "shell"            realgud:trepan2-command-hash) "python")
(setf (gethash "until"            realgud:trepan2-command-hash) "continue %l")

;; Stuff to suport locals window
;; (setf (gethash "info-locals-names-list" realgud:trepan2-command-hash) "eval('\\n'.join(locals().keys()))")
;; (setf (gethash "info-value" realgud:trepan2-command-hash) "pp %s")
;; (setf (gethash "info-type" realgud:trepan2-command-hash) "eval type(%s)")

;; If your version of trepan2 doesn't support "quit!",
;; get a more recent version of trepan2
(setf (gethash "quit" realgud:trepan2-command-hash) "quit!")

(setf (gethash "trepan2" realgud-command-hash) realgud:trepan2-command-hash)

(provide-me "realgud:trepan2-")

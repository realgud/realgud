;; Copyright (C) 2011, 2014-2016 Free Software Foundation, Inc

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

;;; Common Python constants and regular expressions.
(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track")
		       "realgud-")

(declare-function realgud-goto-line-for-pt 'realgud-track)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defconst realgud-python-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp "^[ \t]+File \"\\(.+\\)\", line \\([0-9]+\\)"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Python backtrace (or
traceback) line."  )

;;  Regular expression that pseudo-files in caller. For example:
;;    <string>
(defconst realgud-python-ignore-file-re "<string>"
  "Regular expression that pseudo-files of caller()")

(defun realgud-python-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{realgud-example-map-standard}"
  (define-key map (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)
  (define-key map (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
  (define-key map (kbd "C-c !e") 'realgud:pytest-goto-errmsg-line)
  (define-key map (kbd "C-c !8") 'realgud:flake8-goto-msg-line)
  )


;; Things common to the trepan Python debuggers

(defconst realgud:python-trepan-frame-start-regexp
  "\\(?:^\\|\n\\)\\(->\\|##\\)")

(defconst realgud:python-trepan-frame-num-regexp
  "\\([0-9]+\\)")

;; Regular expression that describes a trepan2/3k location generally shown
;; before a command prompt.
;;
;; For example:
;;   (/usr/bin/zonetab2pot.py:15 @10): <module>
;;   (/usr/bin/zonetab2pot.py:15 remapped <string>): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>

(defconst realgud:python-trepan-loc-pat
      (make-realgud-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\)\\(?: @[0-9]+\\)?\\(?: remapped .*?\\)?): \\(?:<module>\\)?\\(?:\n.. [0-9]+ \\(.*?\\)\n\\)?"
       :file-group 1
       :line-group 2
       :text-group 3
       :ignore-file-re  realgud-python-ignore-file-re)
      "A realgud-loc-pat struct that describes a Python trepan
      location line."  )

;; Regular expression that describes a trepan2/3k backtrace line.
;; For example:
;; ->0 get_distribution(dist='trepan==0.3.9')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 341
;; ##1 load_entry_point(dist='tr=0.3.9', group='console_scripts', name='tr')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 351
;; ##2 <module> exec()

(defconst realgud:python-trepan-backtrace-pat
  (make-realgud-loc-pat
   :regexp (concat
	    realgud:python-trepan-frame-start-regexp
	    realgud:python-trepan-frame-num-regexp "[ ]"
	    "\\(?:.*?)\\)\\(?:[\n\t ]+?\\)"
	    "\\(?:called from file \\)?'\\([^:]+?\\)' at line \\([0-9]+\\)")
   :num 2
   :file-group 3
   :line-group 4
   ))

;; FIXME breakpoints aren't locations. It should be a different structure
;; realgud-loc that describes a trepan2/3k "info breakpoints" line.
;; For example:
;; 1   breakpoint    keep y   at /home/rocky/.pyenv/versions/3.7.2/lib/python3.7/importlib/_bootstrap.py:1019
;; 2   breakpoint    keep y   at /home/rocky/.pyenv/versions/3.7.2/lib/python3.7/importlib/_bootstrap.py:1023
;; 3   breakpoint    keep y   at /home/rocky/.pyenv/versions/3.7.2/lib/python3.7/importlib/_bootstrap.py:1
(defconst realgud-python-breakpoint-pat
  (make-realgud-loc-pat
   :regexp (format "^%s[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)[ \t]+\\([yn]\\)[ \t]+.*at \\(.+\\):%s"
		   realgud:regexp-captured-num realgud:regexp-captured-num)
   :num 1
   :text-group 2  ;; misnamed Is "breakpoint" or "watchpoint"
   :string 3      ;; misnamed. Is "keep" or "del"
   :file-group 5
   :line-group 6)
  "A realgud-loc-pat struct that describes a Python breakpoint."  )

;;  Regular expression that describes a "breakpoint set" line
(defconst realgud:python-trepan-brkpt-set-pat
  (make-realgud-loc-pat
   :regexp "^Breakpoint \\([0-9]+\\) set at line \\([0-9]+\\)[ \t\n]+of file[ \t\n]+\\(.+\\)\\(\n\\|$\\)"
   :num 1
   :file-group 3
   :line-group 2))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
(defconst realgud:python-trepan-brkpt-del-pat
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :num 1))

;; Regular expression that describes a debugger "disable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 disabled.
(defconst realgud:python-trepan-brkpt-disable-pat
  (make-realgud-loc-pat
   :regexp (format "^Breakpoint %s disabled"
		   realgud:regexp-captured-num)
   :num 1))

;; Regular expression that describes a debugger "enable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 enabled.
(defconst realgud:python-trepan-brkpt-enable-pat
  (make-realgud-loc-pat
   :regexp (format "^Breakpoint %s enabled"
		   realgud:regexp-captured-num)
   :num 1))

(defconst realgud:python-debugger-font-lock-keywords
  '(
    ;; The frame number and first type name, if present.
    ("^\\(->\\|##\\)\\([0-9]+\\) \\(<module>\\)? *\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\(.+\\))?"
     (2 realgud-backtrace-number-face)
     (4 font-lock-function-name-face nil t))     ; t means optional.

    ;; Parameter sequence, E.g. gcd(a=3, b=5)
    ;;                             ^^^^^^^^^
    ("(\\(.+\\))"
     (1 font-lock-variable-name-face))

    ;; File name. E.g  file '/test/gcd.py'
    ;;                 ------^^^^^^^^^^^^-
    ("[ \t]+file '\\([^ ]+*\\)'"
     (1 realgud-file-name-face))

    ;; Line number. E.g. at line 28
    ;;                  ---------^^
    ("[ \t]+at line \\([0-9]+\\)$"
     (1 realgud-line-number-face))

    ;; Function name.
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (1 font-lock-type-face)
     (2 font-lock-function-name-face))
    ;; (trepan2-frames-match-current-line
    ;;  (0 trepan2-frames-current-frame-face append))
    ))

(defconst realgud:python-debugger-font-lock-breakpoint-keywords
  '(
    ;; The breakpoint number, type and disposition
    ;; 1   breakpoint    keep y   at /home/rocky/.pyenv/versions/3.7.2/bin/trepan3k:6
    ;; ^   ^^^^^^^^^^    ^^^^
    ("^\\([0-9]+\\)[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)"
     (1 realgud-breakpoint-number-face)
     (2 font-lock-function-name-face nil t)     ; t means optional.
     (3 font-lock-function-name-face nil t))     ; t means optional.

    ;; 1   breakpoint    keep y   at /home/rocky/.pyenv/versions/3.7.2/bin/trepan3k:6
    ;;                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
    ("[ \t]+at \\(.+*\\):\\([0-9]+\\)"
     (1 realgud-file-name-face)
     (2 realgud-line-number-face))
    ))

(defconst realgud-pytest-error-loc-pat
  (make-realgud-loc-pat
   :regexp "^\\(.*\\):\\([0-9]+\\): in "
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Pytest error line"
  )


;; FIXME: there is probably a less redundant way to do the following
;; FNS.
(defun realgud:pytest-goto-errmsg-line (pt)
  "Display the location mentioned by the pytest error at PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "pytest-error"))


(defconst realgud-flake8-msg-loc-pat
  (make-realgud-loc-pat
   :regexp "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): [EFWCN]\\([0-9]+\\) "
   :file-group 1
   :line-group 2
   :char-offset-group 3
   )
  "A realgud-loc-pat struct that describes a flake8 warning or error line"
  )


;; FIXME: there is probably a less redundant way to do the following
;; FNS.
(defun realgud:flake8-goto-msg-line (pt)
  "Display the location mentioned by the flake8 warning or error."
  (interactive "d")
  (realgud-goto-line-for-pt pt "flake8-msg"))


(provide-me "realgud-lang-")

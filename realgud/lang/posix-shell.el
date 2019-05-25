;; Copyright (C) 2015-2016, 2019 Free Software Foundation, Inc

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

;;
;; Common POSIX-Shell like constants and regular expressions.
;; Actually a lot of this is not about POSIX shell as it is about the
;; common-ness of bashdb, zshdb, and kshdb. But since those are the
;; *only* debuggers I know of for POSIX shells, it's not too much of
;; a stretch to think of this as for all "shell".

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc"
			 "../common/track" "../common/send")
		       "realgud-")
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defconst realgud-shell-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Shell backtrace (or
traceback) line."  )

(defconst realgud-shell-frame-start-regexp
  "\\(?:^\\|\n\\)\\(->\\|##\\)")

(defconst realgud-shell-frame-num-regexp
  realgud:regexp-captured-num)

(defconst realgud-shell-frame-file-regexp
  "[ \t\n]+\\(?:in\\|from\\) file `\\(.+\\)'")

(defconst realgud-shell-frame-line-regexp
  (format "[ \t\n]+at line %s\\(?:\n\\|$\\)" realgud:regexp-captured-num))

(defun realgud-posix-shell-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{realgud-example-map-standard}"
  (define-key map (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)
  (define-key map (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
  )

;; Regular expression that describes a bashdb/zshdb location generally shown
;; before a command prompt.
;; For example:
;;   (/etc/init.d/apparmor:35):
(defconst realgud:POSIX-debugger-loc-pat
      (make-realgud-loc-pat
       :regexp "\\(?:^\\|\n\\)(\\([^:]+\\):\\([0-9]*\\)):\\(?:\n\\(.+\\)\\)?"
       :file-group 1
       :line-group 2
       :text-group 3)
      "A realgud-loc-pat struct that describes a POSIX shell debugger
      location line.")

;; realgud-loc that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `/etc/apparmor/fns' at line 24
;;   ##1 /etc/apparmor/fns called from file `/etc/init.d/apparmor' at line 35
;;   ##2 /etc/init.d/apparmor called from file `/usr/bin/zshdb' at line 129
(defconst realgud:POSIX-debugger-backtrace-pat
  (make-realgud-loc-pat
   :regexp 	(concat realgud-shell-frame-start-regexp
			realgud-shell-frame-num-regexp "[ ]?"
			"\\(.*\\)"
			realgud-shell-frame-file-regexp
			"\\(?:" realgud-shell-frame-line-regexp "\\)?"
			)
   :num 2
   :file-group 4
   :line-group 5)
  "A realgud-loc-pat struct that describes a Python trepan
      backtrace location line." )

;; FIXME breakpoints aren't locations. It should be a different structure
;; realgud-loc that describes a bashdb/zshdb/kshdb "info breakpoints" line.
;; For example:
;; 1   breakpoint keep y   /Users/rocky/.bashrc:17
;; 2   breakpoint keep y   /Users/rocky/.bashrc:18
(defconst realgud:POSIX-debugger-breakpoint-pat
  (make-realgud-loc-pat
   :regexp (format "^%s[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)[ \t]+\\([yn]\\)[ \t]+\\(.+\\):%s"
		   realgud:regexp-captured-num realgud:regexp-captured-num)
   :num 1
   :text-group 2  ;; misnamed Is "breakpoint" or "watchpoint"
   :string 3      ;; misnamed. Is "keep" or "del"
   :file-group 5
   :line-group 6)
  "A realgud-loc-pat struct that describes a bashdb/zshdb/kshdb breakpoint."  )

;;  Regular expression that describes a "breakpoint set" line
(defconst realgud:POSIX-debugger-brkpt-set-pat
  (make-realgud-loc-pat
   :regexp (format "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line %s.\n"
		   realgud:regexp-captured-num)
   :num 1
   :file-group 2
   :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(defconst realgud:POSIX-debugger-brkpt-del-pat
  (make-realgud-loc-pat
   :regexp (format "^Deleted breakpoint %s\n"
		   realgud:regexp-captured-num)
   :num 1))

;; Regular expression that describes a debugger "disable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 disabled.
(defconst realgud:POSIX-debugger-brkpt-disable-pat
  (make-realgud-loc-pat
   :regexp (format "^Breakpoint entry %s disabled."
		   realgud:regexp-captured-num)
   :num 1))

;; Regular expression that describes a debugger "enable" (breakpoint) response.
;; For example:
;;   Breakpoint entry 4 enabled.
(defconst realgud:POSIX-debugger-brkpt-enable-pat
  (make-realgud-loc-pat
   :regexp (format "^Breakpoint entry %s enabled."
		   realgud:regexp-captured-num)
   :num 1))

(defconst realgud:POSIX-debugger-font-lock-keywords
  '(
    ;; The frame number and first type name, if present.
    ;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
    ;;      --^-
    ("^\\(->\\|##\\)\\([0-9]+\\) "
     (2 realgud-backtrace-number-face))

    ;; File name.
    ;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
    ;;          ---------^^^^^^^^^^^^^^^^^^^^-
    ("[ \t]+\\(in\\|from\\) file `\\(.+\\)'"
     (2 realgud-file-name-face))

    ;; File name.
    ;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
    ;;                                         --------^^
    ;; Line number.
    ("[ \t]+at line \\([0-9]+\\)$"
     (1 realgud-line-number-face))
    ;; (trepan-frames-match-current-line
    ;;  (0 trepan-frames-current-frame-face append))
    ))

(defconst realgud:POSIX-debugger-font-lock-breakpoint-keywords
  '(
    ;; The breakpoint number, type and disposition
    ;; 1   breakpoint    keep y   /Users/rocky/.bashrc:6
    ;; ^   ^^^^^^^^^^    ^^^^
    ("^\\([0-9]+\\)[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)"
     (1 realgud-breakpoint-number-face)
     (2 font-lock-function-name-face nil t)     ; t means optional.
     (3 font-lock-function-name-face nil t))     ; t means optional.

    ;; 1   breakpoint    keep y   /Users/rocky/.bashrc:6
    ;;                            ^^^^^^^^^^^^^^^^^^^^ ^
    ("[ \t]+\\(.+*\\):\\([0-9]+\\)"
     (1 realgud-file-name-face)
     (2 realgud-line-number-face))
    ))

(provide-me "realgud-lang-")

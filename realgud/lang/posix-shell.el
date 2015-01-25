;;; Copyright (C) 2010-2011, 2015 Rocky Bernstein <rocky@gnu.org>
;;;
;;; Common POSIX-Shell like constants and regular expressions.
;;; Actually a lot of this is not about POSIX shell as it is about the
;;; common-ness of bashdb, zshdb, and kshdb. But since those are the
;;; *only* debuggers I know of for POSIX shells, it's not too much of
;;; a stretch to think of this as for all "shell".
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc"
			 "../common/track" "../common/send")
		       "realgud-")

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
  "\\([0-9]+\\)")

(defconst realgud-shell-frame-file-regexp
  "[ \t\n]+\\(?:in\\|from\\) file `\\(.+\\)'")

(defconst realgud-shell-frame-line-regexp
  "[ \t\n]+at line \\([0-9]+\\)\\(?:\n\\|$\\)")

(defun realgud-posix-shell-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{realgud-example-map-standard}"
  (define-key map (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)
  (define-key map (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
  )


;; Patterns common to the my POSIX shell debuggers

(defconst realgud:python-trepan-frame-start-regexp
  "\\(?:^\\|\n\\)\\(->\\|##\\)")

(defconst realgud:python-trepan-frame-num-regexp
  "\\([0-9]+\\)")

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

;; Regular expression that describes a debugger "backtrace" command line.
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

;;  Regular expression that describes a "breakpoint set" line
(defconst realgud:POSIX-debugger-brkpt-set-pat
  (make-realgud-loc-pat
   :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
   :num 1
   :file-group 2
   :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(defconst realgud:POSIX-debugger-brkpt-del-pat
  (make-realgud-loc-pat
   :regexp "^Removed \\([0-9]+\\) breakpoint(s).\n"
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

(provide-me "realgud-lang-")

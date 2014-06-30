;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
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


(provide-me "realgud-lang-")

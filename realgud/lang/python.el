;;; Copyright (C) 2011, 2014 Rocky Bernstein <rocky@gnu.org>
;;; Common Python constants and regular expressions.
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track")
		       "realgud-")


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
  )

(provide-me "realgud-lang-")

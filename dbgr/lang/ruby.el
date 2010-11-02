;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Common Ruby regular expressions and things
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc") "dbgr-")

;;  Regular expression that describes a Ruby backtrace (or traceback) line.
(defconst dbgr-ruby-backtrace-loc-pat
  (make-dbgr-loc-pat
   :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
   :file-group 1
   :line-group 2))

(provide-me "dbgr-lang-")

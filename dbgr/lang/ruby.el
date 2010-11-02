;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
;;; Common Ruby regular expressions and things
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc") "dbgr-")


(defconst dbgr-ruby-backtrace-loc-pat
  (make-dbgr-loc-pat
   :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
   :file-group 1
   :line-group 2)
  "A dbgr-loc-pat struct that describes a Ruby backtrace (or
traceback) line."  )

(defconst dbgr-ruby-dollar-bang
      (make-dbgr-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2)
  "A dbgr-loc-pat that struct that describes a Ruby $! string."
)


(provide-me "dbgr-lang-")

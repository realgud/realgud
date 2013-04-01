;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
;;; FIXME - think of a better name.
;;; Debugger regular expressions for many kinds of
;;;  debuggers

;;; Here we have regular expressions and names for matched patterns
;;; of those regular expressions.

;;; Code:

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;

(eval-when-compile (require 'cl))

(defstruct realgud-loc-pat
  "Information to match and extract a file and line number location from
a string output by a debugger inside a process shell"
  (num)
  (regexp)
  (file-group)
  (line-group)
  (char-offset-group)
  (instruction-address-group)
  (column-group)
  (ignore-file-re)
)

(defvar realgud-pat-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string. The values of a hash entry
  is a realgud-loc-pat struct")

(defvar realgud-command-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string. The values of a hash
  entry is a hash table mapping cannonic command name
  debugger-specific command name. For example, for trepanning:
  'quit' -> 'quit!'")

(provide 'realgud-regexp)

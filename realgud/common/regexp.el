;;; Copyright (C) 2010-2011, 2014-2015 Rocky Bernstein <rocky@gnu.org>
;;; FIXME - think of a better name.
;;;
;;; Debugger regular expressions for many kinds of
;;;  debuggers

;;; Here we have hash tables used in each kind of debugger
;;; and names for patterns matching fields in a location
;;; structure

;;; Code:

;; -------------------------------------------------------------------
;; Variables defining regular expressions (regexp:s).
;;

(eval-when-compile (require 'cl))

(defstruct realgud-loc-pat
  "Information to match and extract position and other related information typically
output by a debugger inside a process shell"
  (num)                ;; General number. Could be for example
		       ;; breakpoint number,
  (string)             ;; General string, Could be for example a list of
                       ;; breakpoint number. Or can be used if for example
                       ;; if we need more than one in a complicated re
                       ;; where we can't assign a single number to a
                       ;; file position as in Perl locations.
  (regexp)             ;; a stack position, or thread number.
  (file-group)         ;; Filename position in struct
  (line-group)         ;; Line number position in struct
  (alt-file-group)     ;; Used when regexp is too complicated and use \|
                       ;; e.g. perldb file loc regexps
  (alt-line-group)     ;; ditto
  (char-offset-group)  ;; Character offset position in struct
  (instruction-address-group)
  (column-group)
  (ignore-file-re)     ;; Some debuggers create pseudo files in eval strings
                       ;; for example "(eval)" in Ruby and Perl
  (text-group)         ;; Some source text that should found at position
  (class-group)        ;; Java doesn't refer to files, but class names
  (event-group)        ;; Stopping event, e.g.statement, breakpoint,
		       ;; call, return, exception, etc.
)

(defconst realgud:regexp-captured-num  "\\([0-9]+\\)")

(defvar realgud-pat-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string. The values of a hash entry
  is a realgud-loc-pat struct")

(defvar realgud-command-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string. The values of a hash
  entry is a hash table mapping cannonic command name
  debugger-specific command name. For example, for trepanning:
  'quit' -> 'quit!'")

(provide 'realgud-regexp)

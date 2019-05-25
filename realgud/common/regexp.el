;; Copyright (C) 2010-2011, 2014-2016, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

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

(eval-when-compile (require 'cl-lib))

(cl-defstruct realgud-loc-pat
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

  ;; FIXME: fix code to handle lists of locs and then remove
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
  (function-group)     ;; function name
)

(defconst realgud:regexp-captured-num  "\\([0-9]+\\)")

(defvar realgud-pat-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string.
The values of a hash entry is a realgud-loc-pat struct")

(defvar realgud-command-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string.
The value of a hash entry is a hash table mapping cannonic command name
  debugger-specific command name.  For example, for trepanning:
  'quit' -> 'quit!'")

(defvar realgud:variable-basename-hash (make-hash-table :test 'equal)
  "Hash key is the debugger name, a string.
The value of a hash
entry is the base name to use that variables of that debugger use.
  For example, for 'gdb' it is 'realgud:gdb'.")

(provide 'realgud-regexp)

;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Stock Perl debugger perldb

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../regexp" "../loc") "dbgr-")

(defvar dbgr-pat-hash)
(declare-function make-dbgr-loc-pat (dbgr-loc))

(defvar perldb-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match: backtrace, prompt, etc. 
The values of a hash entry is a dbgr-loc-pat struct")

(declare-function make-dbgr-loc "dbgr-loc" (a b c d e f))

;; Regular expression that describes a perldb location generally shown
;; before a command prompt.
;;
;; Program-location lines look like this:
;;   main::(/usr/bin/latex2html:102):
;;   main::CODE(0x9407ac8)(l2hconf.pm:6):
;; or MS Windows:
;;   ???
(setf (gethash "loc" perldb-pat-hash)
      (make-dbgr-loc-pat
       :regexp "\\(?:CODE(0x[0-9a-h]+)\\)?(\\(.+\\):\\(\[0-9]+\\)):"
       :file-group 1
       :line-group 2))

(setf (gethash "prompt" perldb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "  DB<\\(\d+\\)> "
       :num 1
       ))

;;  Regular expression that describes a Perl backtrace line.
;; $ = main::BEGIN() called from file `(eval 19)[/usr/ibn/latex2html:126]' line 2
(setf (gethash "backtrace" perldb-pat-hash)
      (make-dbgr-loc-pat
       :regexp   "^called from file `\\(.+\\)\", line \\([0-9]+\\)"
       :file-group 1
       :line-group 2))

(setf (gethash "perldb" dbgr-pat-hash) perldb-pat-hash)

(provide-me "dbgr-init-")

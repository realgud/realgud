;; Copyright (C) 2011, 2014, 2016, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Common Perl constants and regular expressions.
(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track")
		       "realgud-")

(declare-function realgud-goto-line-for-pt 'realgud-track)
(declare-function make-realgud-loc-pat 'realgud-regexp)

;;  Regular expression that describes a Perl Carp backtrace line.
;;  at /tmp/foo.pl line 7
;; 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at /tmp/foo.pl line 4
;; 	main::foo(3) called at /tmp/foo.pl line 8
(defconst realgud-perl-carp-loc-pat
      (make-realgud-loc-pat
       :regexp   (concat
		  "\\(?:^\\|
\\)"
		  "\\(?:[ \t]+\\(?:\\|.* called \\)at \\(.*\\) line \\([0-9]+\\)\\)")
       :file-group 1
       :line-group 2)
  "A realgud-loc-pat struct that describes a line used in a Carp message"  )

(defconst realgud-perl-errmsg-loc-pat
      (make-realgud-loc-pat
       :regexp   (concat
		  " at \\(.+\\) line \\([0-9]+\\).$")
       :file-group 1
       :line-group 2)
  "A realgud-loc-pat struct that describes a line used in an error message"  )

;;  Regular expression that pseudo-files in caller. For example:
;;    (eval 1006)[../example/eval.pl:5]
(defconst realgud-perl-ignore-file-re "(eval [0-9]+)\\(\\[.+\\]\\)?"
  "Regular expression that pseudo-files of caller()")

;; FIXME: there is probably a less redundant way to do the following
;; FNS.
(defun realgud:perl-goto-errmsg-line (pt)
  "Display the location mentioned by the Perl error message described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "perl-errmsg"))

(defun realgud-perl-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{realgud-example-map-standard}"
  (define-key map (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)
  (define-key map (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
  (define-key map (kbd "C-c !e") 'realgud:perl-goto-errmsg-line)
  )

(provide-me "realgud-lang-")

;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;; Common Perl constants and regular expressions.
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track") 
		       "dbgr-")


;;  Regular expression that describes a Perl Carp backtrace line.
;;  at /tmp/foo.pl line 7
;; 	main::__ANON__('Illegal division by zero at /tmp/foo.pl line 4.\x{a}') called at /tmp/foo.pl line 4
;; 	main::foo(3) called at /tmp/foo.pl line 8
(defconst dbgr-perl-carp-loc-pat
      (make-dbgr-loc-pat
       :regexp   (concat 
		  "\\(?:^\\|
\\)"
		  "\\(?:[ \t]+\\(?:\\|.* called \\)at \\(.*\\) line \\([0-9]+\\)\\)")
       :file-group 1
       :line-group 2)
  "A dbgr-loc-pat struct that describes a line used in a Carp message"  )

(defconst dbgr-perl-errmsg-loc-pat
      (make-dbgr-loc-pat
       :regexp   (concat 
		  " at \\(.+\\) line \\([0-9]+\\).$")
       :file-group 1
       :line-group 2)
  "A dbgr-loc-pat struct that describes a line used in an error message"  )

;;  Regular expression that pseudo-files in caller. For example:
;;    (eval 1006)[../example/eval.pl:5]
(defconst dbgr-perl-ignore-file-re "(eval [0-9]+)\\(\\[.+\\]\\)?"
  "Regular expression that pseudo-files of caller()")

;; FIXME: there is probably a less redundant way to do the following
;; FNS. 
(defun dbgr-perl-goto-errmsg-line (pt)
  "Display the location mentioned by the Perl error message described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt pt "perl-errmsg"))

(defun dbgr-perl-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{dbgr-example-map-standard}"
  (define-key map (kbd "C-c !b") 'dbgr-goto-debugger-backtrace-line)
  (define-key map (kbd "C-c !!") 'dbgr-goto-lang-backtrace-line)
  (define-key map (kbd "C-c !e") 'dbgr-perl-goto-errmsg-line)
  )

(provide-me "dbgr-lang-")

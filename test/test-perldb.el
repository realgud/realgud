;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/perldb/perldb.el")

(declare-function realgud:perldb-parse-cmd-args 'realgud:perldb)
(declare-function realgud:perldb        'realgud:perldb)
(declare-function __FILE__              'load-relative)

(declare-function realgud-perldb-parse-cmd-args 'realgud-perldb)
(test-simple-start)

(note "realgud:perldb-parse-cmd-args")
(assert-equal (list
	       '("perl" "-W" "-d") nil
	       (list (expand-file-name"gcd.rb") "a" "b"))
	      (realgud:perldb-parse-cmd-args
	       '("perl" "-W" "-d" "gcd.rb" "a" "b")))
(assert-equal (list
	       '("perl5.10.1" "-C" "/tmp" "-d") nil
		(list (expand-file-name "gcd.rb")))
	      (realgud:perldb-parse-cmd-args
	       '("perl5.10.1" "-C" "/tmp" "-d" "gcd.rb")))

(end-tests)

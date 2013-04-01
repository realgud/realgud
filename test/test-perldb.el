(require 'test-simple)
(load-file "../realgud/debugger/perldb/perldb.el")

(test-simple-start)

(note "realgud-perldb-parse-cmd-args")
(assert-equal '(("perl" "-W" "-d") ("gcd.rb" "a" "b"))
	      (realgud-perldb-parse-cmd-args
	       '("perl" "-W" "-d" "gcd.rb" "a" "b")))
(assert-equal '(("perl5.10.1" "-C" "/tmp" "-d") ("gcd.rb"))
	      (realgud-perldb-parse-cmd-args
	       '("perl5.10.1" "-C" "/tmp" "-d" "gcd.rb")))

(end-tests)

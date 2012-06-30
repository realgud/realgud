(require 'test-simple)
(load-file "../dbgr/debugger/perldb/perldb.el")

(test-simple-start)

(note "dbgr-perldb-parse-cmd-args")
(assert-equal '(("perl" "-W" "-d") ("gcd.rb" "a" "b"))
	      (dbgr-perldb-parse-cmd-args 
	       '("perl" "-W" "-d" "gcd.rb" "a" "b")))
(assert-equal '(("perl5.10.1" "-C" "/tmp" "-d") ("gcd.rb"))
	      (dbgr-perldb-parse-cmd-args 
	       '("perl5.10.1" "-C" "/tmp" "-d" "gcd.rb")))

(end-tests)


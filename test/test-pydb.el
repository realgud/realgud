(require 'test-simple)
(load-file "../realgud/debugger/pydb/pydb.el")
(declare-function pydb-parse-cmd-args 'realgud:pydb)

(test-simple-start)

(note "pydb-parse-cmd-args")
(assert-equal '(nil ("pydb") ("foo") nil)
	      (pydb-parse-cmd-args '("pydb" "foo")))
(assert-equal '(nil ("pydb") ("program.py" "foo") nil)
	      (pydb-parse-cmd-args
	       '("pydb" "program.py" "foo")))
(end-tests)

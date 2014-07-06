(require 'test-simple)
(load-file "../realgud/debugger/trepan2/trepan2.el")
(declare-function trepan2-parse-cmd-args 'realgud:trepan2)
(declare-function __FILE__               'require-relative)


(test-simple-start)

(note "trepan2-parse-cmd-args")

(assert-equal '(nil ("trepan2") ("foo") nil)
	      (trepan2-parse-cmd-args '("trepan2" "foo")))
(assert-equal '(nil ("trepan2" "-n") ("foo") nil)
	      (trepan2-parse-cmd-args '("trepan2" "-n" "foo")))
(assert-equal '(("/usr/bin/python") ("trepan2" "--different")
		("foo") nil)
	      (trepan2-parse-cmd-args
	       '("/usr/bin/python" "trepan2"
		 "--different" "foo")))
(assert-equal '(nil ("program.py") ("foo") nil)
	      (trepan2-parse-cmd-args '("program.py" "foo")))
(assert-equal '(nil ("trepan2") ("program.py" "foo") nil)
	      (trepan2-parse-cmd-args
	       '("trepan2" "program.py" "foo")))

(end-tests)

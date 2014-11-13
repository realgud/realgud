(require 'test-simple)
(load-file "../realgud/debugger/jdb/core.el")
(test-simple-start)

(declare-function __FILE__  'require-relative)
(declare-function realgud:jdb-parse-cmd-args 'realgud:jdb-core)

(assert-equal '("jdb" nil ("TestMe"))
	      (realgud:jdb-parse-cmd-args '("jdb" "TestMe")))
(end-tests)

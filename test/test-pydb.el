;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/debugger/pydb/pydb.el")
(declare-function pydb-parse-cmd-args 'realgud:pydb)
(declare-function __FILE__            'load-relative)

(test-simple-start)

(note "pydb-parse-cmd-args")
(assert-equal '(nil ("pydb") ("foo") nil)
	      (pydb-parse-cmd-args '("pydb" "foo")))
(assert-equal '(nil ("pydb") ("program.py" "foo") nil)
	      (pydb-parse-cmd-args
	       '("pydb" "program.py" "foo")))
(end-tests)

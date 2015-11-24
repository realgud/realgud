;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/trepan2/trepan2.el")
(load-file "../realgud/debugger/trepan2/core.el")
(load-file "../realgud.el")

(declare-function trepan2-parse-cmd-args    'realgud:trepan2)
(declare-function realgud:trepan2-find-file 'realgud:trepan2-core)
(declare-function __FILE__                  'load-relative)


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

(note "realgud:trepan2-find-file")
(assert-nil (realgud:trepan2-find-file "<string>")
	    "Should ignore psuedo file")

(eval-when-compile
  (defvar test-python-file))

(set (make-local-variable 'test-python-file)
     (concat (file-name-directory (__FILE__)) "gcd.py"))
(assert-equal test-python-file (realgud:trepan2-find-file test-python-file)
	    "Should ignore psuedo file")

(end-tests)

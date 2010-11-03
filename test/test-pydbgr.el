(require 'test-unit)
(load-file "../dbgr/debugger/pydbgr/pydbgr.el")

(test-unit-clear-contexts)

(context "pydbgr"
	 (tag pydbgr)

	 (specify "pydbgr-parse-cmd-args"
	      (assert-equal '(nil ("pydbgr") ("foo") nil)
	 		    (pydbgr-parse-cmd-args '("pydbgr" "foo")))
	      (assert-equal '(nil ("pydbgr" "-n") ("foo") nil)
	 		    (pydbgr-parse-cmd-args '("pydbgr" "-n" "foo")))
	      (assert-equal '(nil ("pydbgr" "--annotate=1") ("foo") t)
	 		    (pydbgr-parse-cmd-args
	 		     '("pydbgr" "--annotate=1" "foo")))
	      (assert-equal '(nil ("mypydbgr" "--annotate=1") ("foo") t)
	 		    (pydbgr-parse-cmd-args
	 		     '("mypydbgr" "--annotate=1" "foo")))
	      (assert-equal '(("python") ("pydbgr" "--annotate") ("1" "foo") t)
	 		    (pydbgr-parse-cmd-args
	 		     '("python" "pydbgr" "--annotate" "1" "foo")))
	      (assert-equal '(("/usr/bin/python") ("pydbgr" "--different") 
			      ("foo") nil)
	 		    (pydbgr-parse-cmd-args
	 		     '("/usr/bin/python" "pydbgr" 
	 		       "--different" "foo")))
	      (assert-equal '(nil ("program.py") ("foo") nil)
	 		    (pydbgr-parse-cmd-args '("program.py" "foo")))
	      (assert-equal '(nil ("pydbgr") ("program.py" "foo") nil)
	 		    (pydbgr-parse-cmd-args 
	 		     '("pydbgr" "program.py" "foo")))
	      )
	 )

(test-unit "pydbgr")


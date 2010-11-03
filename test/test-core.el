(setq trepan-core "../dbgr/debugger/trepan/core.el")
(require 'test-unit)
(load-file "../dbgr/common/core.el")
(load-file "../dbgr/debugger/trepan/core.el")

(test-unit-clear-contexts)

(context "dbgr-core.el"
	 (tag cmd-args)
	 (lexical-let ((opt-two-args '("0" "C" "e" "E" "F" "i")))
	   (specify "Two args found, none remain afterwards though."
		    (assert-equal '(("-0" "a") nil)
		     (dbgr-parse-command-arg '("-0" "a") '() opt-two-args)))

	   (specify "One arg not found."
	   	    (assert-equal
	   	     '(("-5") ("a" "-0"))
	   	     (dbgr-parse-command-arg '("-5" "a" "-0") '() 
	   				       opt-two-args)))
	   (specify "Degenerate case - no args"
	   	    (assert-equal 
	   	     '((nil) nil)
	   	     (dbgr-parse-command-arg '() '() opt-two-args)))

	   (specify "Two mandatory args"
	   	    (assert-equal 
	   	     '(("--port" "123") ("bar"))
	   	    (dbgr-parse-command-arg 
	   	     '("--port" "123" "bar") '("-port") '())))

	   (specify "Separate Ruby with its arg from debugger and its arg."
	   	    (assert-equal 
	   	     '(("/usr/bin/ruby1.9" "-W") ("trepan") ("foo") nil)
	   	     (trepan-parse-cmd-args
	   	      '("/usr/bin/ruby1.9" "-W" "trepan" "foo"))))
	   
	   (specify "ruby with two args and trepan with two args"
	   	    (assert-equal 
	   	     '(("ruby1.9" "-T3") ("trepan" "--port" "123") ("bar") nil)
	   	     (trepan-parse-cmd-args
	   	      '("ruby1.9" "-T3" "trepan" "--port" "123" "bar"))))

	   (specify "trepan with annotate args"
	   	    (assert-equal
	   	     '(nil ("trepan" "--port" "1" "--annotate=3")
	   		   ("foo" "a") t)
	   	     (trepan-parse-cmd-args
	   	      '("trepan" "--port" "1" "--annotate=3" "foo" "a"))))

	   (specify "trepan with --emacs in the wrong place"
	   	    (assert-equal
	   	     '(nil ("trepan" "--port" "123")
	   		   ("foo" "--emacs" "a") nil)
	   	     (trepan-parse-cmd-args
	   	      '("trepan" "--port" "123" "foo" "--emacs" "a"))))

	   (specify "trepan with emacs"
	   	    (assert-equal 
	   	     '(("ruby" "-I/usr/lib/ruby") 
	   	       ("trepan" "-h" "foo" "--emacs") 
	   	       ("baz") t)
	   	     (trepan-parse-cmd-args
	   	      '("ruby" "-I/usr/lib/ruby" "trepan" "-h" "foo" 
	   		"--emacs" "baz"))))
	   ))

(test-unit "cmd-args")


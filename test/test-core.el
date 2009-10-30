(setq rbdbgr-core "../ruby/rbdbgr-core.el")
(load-file "./behave.el")
(load-file "../dbgr-core.el")
(load-file rbdbgr-core)

(behave-clear-contexts)

(context "dbgr-core.el"
	 (tag cmd-args)
	 (lexical-let ((opt-two-args '("0" "C" "e" "E" "F" "i")))
	   (specify "Two args found, none remain afterwards though."
		    (expect-equal '(("-0" "a") nil)
		     (dbgr-parse-command-arg '("-0" "a") '() opt-two-args)))

	   (specify "One arg not found."
	   	    (expect-equal
	   	     '(("-5") ("a" "-0"))
	   	     (dbgr-parse-command-arg '("-5" "a" "-0") '() 
	   				       opt-two-args)))
	   (specify "Degenerate case - no args"
	   	    (expect-equal 
	   	     '((nil) nil)
	   	     (dbgr-parse-command-arg '() '() opt-two-args)))

	   (specify "Two mandatory args"
	   	    (expect-equal 
	   	     '(("--port" "123") ("bar"))
	   	    (dbgr-parse-command-arg 
	   	     '("--port" "123" "bar") '("-port") '())))

	   (specify "Separate Ruby with its arg from debugger and its arg."
	   	    (expect-equal 
	   	     '(("/usr/bin/ruby1.9" "-W") ("rbdbgr") ("foo") nil)
	   	     (rbdbgr-parse-cmd-args
	   	      '("/usr/bin/ruby1.9" "-W" "rbdbgr" "foo"))))
	   
	   (specify "ruby with two args and rbdbgr with two args"
	   	    (expect-equal 
	   	     '(("ruby1.9" "-T3") ("rbdbgr" "--port" "123") ("bar") nil)
	   	     (rbdbgr-parse-cmd-args
	   	      '("ruby1.9" "-T3" "rbdbgr" "--port" "123" "bar"))))

	   (specify "rbdbgr with annotate args"
	   	    (expect-equal
	   	     '(nil ("rbdbgr" "--port" "1" "--annotate=3")
	   		   ("foo" "a") t)
	   	     (rbdbgr-parse-cmd-args
	   	      '("rbdbgr" "--port" "1" "--annotate=3" "foo" "a"))))

	   (specify "rbdbgr with --emacs in the wrong place"
	   	    (expect-equal
	   	     '(nil ("rbdbgr" "--port" "123")
	   		   ("foo" "--emacs" "a") nil)
	   	     (rbdbgr-parse-cmd-args
	   	      '("rbdbgr" "--port" "123" "foo" "--emacs" "a"))))

	   (specify "rbdbgr with emacs"
	   	    (expect-equal 
	   	     '(("ruby" "-I/usr/lib/ruby") 
	   	       ("rbdbgr" "-h" "foo" "--emacs") 
	   	       ("baz") t)
	   	     (rbdbgr-parse-cmd-args
	   	      '("ruby" "-I/usr/lib/ruby" "rbdbgr" "-h" "foo" 
	   		"--emacs" "baz"))))
	   ))

;; FIXME: move to another file.
(context "rbdbgr-core.el"
	 (tag cmd-args)
	 (specify "ruby-mode? with Lisp file"
		  (expect-nil
		   (rbdbgr-file-ruby-mode? rbdbgr-core)))
	 
	 (specify "ruby-mode? with Ruby file"
		  (save-excursion (find-file "./gcd.rb"))
		  (expect-t
		   (rbdbgr-file-ruby-mode? "./gcd.rb")))
	 
	 (specify "rbdbgr-suggest-ruby-file"
		  (expect-equal "gcd.rb"
				(rbdbgr-suggest-ruby-file)))
	 )

(behave "cmd-args")


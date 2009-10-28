(setq rbdbgr-core "../rbdbgr-core.el")
(load-file "./behave.el")
(load-file rbdbgr-core)

(behave-clear-contexts)

(context "command argument processing: "
	 (tag cmd-args)
	 (lexical-let ((opt-two-args '("0" "C" "e" "E" "F" "i")))
	   (specify "Two args found, none remain afterwards though."
		    (expect 
		     (rbdbgr-strip-command-arg '("-0" "a") '() opt-two-args)
		     equal nil))
	   (specify "Two args not found, strip first arg though."
		    (expect 
		     (rbdbgr-strip-command-arg '("-5" "a" "-0") '() opt-two-args)
		     equal '("a" "-0")))
	   (specify "Degenerate case - no args"
		    (expect 
		     (rbdbgr-strip-command-arg '() '() opt-two-args)
		     equal nil))

	   (specify "Strip ruby and its arg."
		    (expect 
		     (rbdbgr-get-script-name 
		      '("/usr/bin/ruby1.9" "-W" "rbdbgr" "foo"))
		     equal '("foo" nil)))
	   
	   (specify "ruby with two args and rbdbgr with two args"
		    (expect
		     (rbdbgr-get-script-name 
		      '("ruby1.9" "-T3" "rbdbgr" "--port" "123" "bar"))
		     equal '("bar" nil)))

	   (specify "rbdbgr with annotate args"
		    (expect
		     (rbdbgr-get-script-name 
		      '("rbdbgr" "--port 123" "--annotate=3" "foo"))
		     equal '("foo" t)))

	   (specify "rbdbgr with emacs"
		    (expect
		     (rbdbgr-get-script-name 
		      '("ruby" "-I/usr/lib/ruby" "rbdbgr" "-h" "foo" 
			"--emacs" "baz"))
		     equal '("baz" t)))

	   ;; FIXME: move to another file.
	   (specify "ruby-mode? with Lisp file"
		    (expect 
		     (rbdbgr-file-ruby-mode? rbdbgr-core) equal nil))
	   ;; (specify "ruby-mode? with Ruby file"
	   ;; 	    ;; Set major mode to Ruby-mode
	   ;; 	    (expect 
	   ;; 	     (rbdbgr-file-ruby-mode? rbdbgr-core) equal t))
	   ))



(behave "cmd-args")


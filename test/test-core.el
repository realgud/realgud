(setq rbdbgr-core "../ruby/rbdbgr-core.el")
(load-file "./behave.el")
(load-file "../dbgr-core.el")
(load-file rbdbgr-core)

(behave-clear-contexts)

(context "command argument processing: "
	 (tag cmd-args)
	 (lexical-let ((opt-two-args '("0" "C" "e" "E" "F" "i")))
	   (specify "Two args found, none remain afterwards though."
		    (expect-nil
		     (dbgr-strip-command-arg '("-0" "a") '() opt-two-args)))

	   (specify "Two args not found, strip first arg though."
		    (expect-equal
		     '("a" "-0")
		     (dbgr-strip-command-arg '("-5" "a" "-0") '() 
					       opt-two-args)))
	   (specify "Degenerate case - no args"
		    (expect-nil
		     (dbgr-strip-command-arg '() '() opt-two-args)))

	   (specify "Strip ruby and its arg."
		    (expect-equal '("foo" nil)
		     (rbdbgr-get-script-name 
		      '("/usr/bin/ruby1.9" "-W" "rbdbgr" "foo"))))
	   
	   (specify "ruby with two args and rbdbgr with two args"
		    (expect-equal '("bar" nil)
		     (rbdbgr-get-script-name 
		      '("ruby1.9" "-T3" "rbdbgr" "--port" "123" "bar"))))

	   (specify "rbdbgr with annotate args"
		    (expect-equal '("foo" t)
		     (rbdbgr-get-script-name 
		      '("rbdbgr" "--port 123" "--annotate=3" "foo"))))

	   (specify "rbdbgr with emacs"
		    (expect-equal '("baz" t)
		     (rbdbgr-get-script-name 
		      '("ruby" "-I/usr/lib/ruby" "rbdbgr" "-h" "foo" 
			"--emacs" "baz")))))
	 )

;; FIXME: move to another file.
(context "rbdbgr-file-ruby-mode?"
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


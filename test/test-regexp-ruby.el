(require 'test-unit)

;; Don't have a pat-hash for ruby, so we need something that pulls in
;; Ruby.
(load-file "../dbgr/debugger/rdebug/init.el")

(test-unit-clear-contexts)

(setq bt  (gethash "rails-backtrace" dbgr-rdebug-pat-hash))

(defun loc-match(text var) 
  (string-match (dbgr-loc-pat-regexp var) text)
)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "/tmp/rails-2.3.5/lib/tasks/databases.rake:360")
(context "traceback location matching"
	 (tag regexp-ruby)
	 (lexical-let ((text "/tmp/rails-2.3.5/lib/tasks/databases.rake:360"))
	   (specify "basic traceback location"
		    (assert-t (numberp (loc-match text bt))))
	   (specify "extract traceback file name"
		    (assert-equal "/tmp/rails-2.3.5/lib/tasks/databases.rake"
				  (match-string (dbgr-loc-pat-file-group bt)
						text)))
	   (specify "extract traceback line number"
		    (assert-equal "360"
				  (match-string (dbgr-loc-pat-line-group bt)
					      text)))
	   )

	 (lexical-let ((text 
			"/tmp/gems/rake-0.8.7/lib/rake.rb:597:in `invoke_with_call_chain'"))
	   (specify "traceback location with in"
		    (assert-t (numberp (loc-match text bt))))
	   (specify "extract traceback file name 2"
	   	  (assert-equal "/tmp/gems/rake-0.8.7/lib/rake.rb"
	   			(match-string (dbgr-loc-pat-file-group bt)
	   				      text)))
	   (specify "extract breakpoint line number 2"
		    (assert-equal "597"
				  (match-string (dbgr-loc-pat-line-group bt)
						text)))
	   )
	 )
(test-unit "regexp-ruby")


(load-file "../dbgr/debugger/trepanx/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)

(set (make-local-variable 'bps)
     (gethash "brkpt-set"       dbgr-trepanx-pat-hash))
(set (make-local-variable 'prompt)
     (gethash "prompt"          dbgr-trepanx-pat-hash))
(set (make-local-variable 'tb)
     (gethash "lang-backtrace"  dbgr-trepanx-pat-hash))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(set (make-local-variable 'text)
 " [0;31m                       Object#boom at tmp/boom.rb:2[0m")

(assert-t (numberp (tb-loc-match text))
	  "basic traceback location")

(assert-equal 0 (tb-loc-match text)
	      "match trepanx location")
(assert-equal "tmp/boom.rb"
	      (match-string (dbgr-loc-pat-file-group tb)
			    text)
	      "extract traceback file name")
(setq text 
      "            { } in main.__script__ at /tmp/blam.rb:5")
(assert-equal 0 (tb-loc-match text)
	      "find a trepanx location")
(assert-equal "/tmp/blam.rb"
	      (match-string (dbgr-loc-pat-file-group tb)
			    text)
	      "extract traceback file name")

(assert-equal "5"
	      (match-string (dbgr-loc-pat-line-group tb)
			    text) 
	      "extract traceback line number")
 
(note "prompt matching")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" dbgr-trepanx-pat-hash))
(prompt-match "((trepanx)): " nil "nested debugger prompt: %s")
(prompt-match "((trepanx@55)): " 
	      "@55" "nested debugger prompt with addr: %s")
(prompt-match "((trepanx@main)): " "@main" 
	      "nested debugger prompt with method: %s")
(setq prompt-str "trepanx:")
(assert-nil (loc-match prompt-str prompt-pat)
	    (format "invalid prompt %s" prompt-str))

(setq text "Set breakpoint 1: __script__() at /bin/irb:2 (@0)")

(assert-t (numberp (bp-loc-match text)) 
	  "basic breakpoint location")
(assert-equal "/bin/irb"
	      (match-string (dbgr-loc-pat-file-group bps)
			    text)
	      "extract breakpoint file name"
	      )
(assert-equal "2"
	      (match-string (dbgr-loc-pat-line-group bps)
			    text)
	      "extract breakpoint line number"
	      )

(end-tests)


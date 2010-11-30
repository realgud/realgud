(require 'test-unit)
(load-file "../dbgr/debugger/trepan/init.el")

(test-unit-clear-contexts)


(setq bps-pat    (gethash "brkpt-set"     dbgr-trepan-pat-hash))
(setq frame-pat  (gethash "frame"         dbgr-trepan-pat-hash))
(setq prompt-pat (gethash "prompt"        dbgr-trepan-pat-hash))
(setq tb-pat     (gethash "backtrace"     dbgr-trepan-pat-hash))
(setq ctrl-pat   (gethash "control-frame" dbgr-trepan-pat-hash))

(defun tb-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp tb-pat) text)
)

(defun bp-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp bps-pat) text)
)

(defun ctrl-frame-match(text) 
  (string-match (dbgr-loc-pat-regexp ctrl-pat) text)
)

(defun prompt-match(prompt-str) 
  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt-pat)
				prompt-str))
)


;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "	from /usr/local/bin/irb:12:in `<main>'")
(context "traceback location matching"
	 (tag regexp-trepan)
	 (lexical-let ((text "	from /usr/local/bin/irb:12:in `<main>'"))
	   (specify "basic traceback location"
		    (assert-t (numberp (tb-loc-match text))))
	   (specify "extract traceback file name"
	   	    (assert-equal "/usr/local/bin/irb"
				  (match-string (dbgr-loc-pat-file-group tb-pat)
	   				  text)))
	   (specify "extract traceback line number"
	   	    (assert-equal "12"
				  (match-string (dbgr-loc-pat-line-group tb-pat)
						text)))
	   )

	   (specify "frame"
		    (setq s1 "--> #0 METHOD Object#require(path) in file <internal:lib/require> at line 28
    #1 TOP Object#<top /tmp/linecache.rb> in file /tmp/linecache.rb
")
		    (setq frame-re (dbgr-loc-pat-regexp frame-pat))
		    (setq num-group (dbgr-loc-pat-num frame-pat))
		    (setq file-group (dbgr-loc-pat-file-group frame-pat))
		    (setq line-group (dbgr-loc-pat-line-group frame-pat))
	   	    (assert-equal 0 (string-match frame-re s1))
		    (assert-equal "0" (substring s1 
						 (match-beginning num-group)
						 (match-end num-group)))
		    (assert-equal "<internal:lib/require>"
				  (substring s1 
					     (match-beginning file-group)
					     (match-end file-group)))
		    (assert-equal "28"
				  (substring s1 
					     (match-beginning line-group)
					     (match-end line-group)))
		    (setq pos (match-end 0))

	   	    (assert-equal 77 (string-match frame-re s1 pos))
		    (assert-equal "1" (substring s1 
						 (match-beginning num-group)
						 (match-end num-group)))
		    (assert-equal "/tmp/linecache.rb"
				  (substring s1 
					     (match-beginning file-group)
					     (match-end file-group)))
		    )

	   (specify "prompt"
		    (prompt-match "(trepan): ")
	   	    (prompt-match "((trepan)): ")
	   	    (prompt-match "((trepan@55)): ")
		    (prompt-match "((trepan@main)): ")
		    )

	   (specify "control-frame"
	   	    (assert-equal 0 (ctrl-frame-match 
				     "c:0026 p:0181 s:0136 b:0136 l:000135 d:000135 METHOD /trepan-0.0.1/app/frame.rb:132 "
				     )
				  )
	   	    (assert-equal 0 (ctrl-frame-match 
				     "c:0030 p:0041 s:0144 b:0144 l:00226c d:00226c METHOD /gems/trepan-0.0.1/processor/eval.rb:15 "
				     )
				  )
	   	    (assert-equal 0 (ctrl-frame-match 
				     "c:0015 p:0139 s:0070 b:0070 l:000063 d:000069 BLOCK  /gems/app/core.rb:121"
				     )
				  )
		    )
	   
	   (lexical-let ((text "Breakpoint 1 set at line 9
	in file /usr/local/bin/irb,
	VM offset 2 of instruction sequence <top (required)>."))
	     (specify "basic breakpoint location"
		      (assert-t (numberp (bp-loc-match text))))
	     (specify "extract breakpoint file name"
		      (assert-equal "/usr/local/bin/irb"
				    (match-string (dbgr-loc-pat-file-group 
						   bps-pat) text)))
	     (specify "extract breakpoint line number"
		      (assert-equal "9"
				    (match-string (dbgr-loc-pat-line-group 
						   bps-pat) text)))
	     )
	   )

(test-unit "regexp-trepan")


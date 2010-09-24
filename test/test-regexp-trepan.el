(require 'test-unit)
(load-file "../dbgr/common/init/trepan.el")

(test-unit-clear-contexts)


(setq bps    (gethash "brkpt-set"     trepan-pat-hash))
(setq prompt (gethash "prompt"        trepan-pat-hash))
(setq tb     (gethash "backtrace"     trepan-pat-hash))
(setq ctrl   (gethash "control-frame" trepan-pat-hash))

(defun tb-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp tb) text)
)

(defun bp-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp bps) text)
)

(defun ctrl-frame-match(text) 
  (string-match (dbgr-loc-pat-regexp ctrl) text)
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
				  (match-string (dbgr-loc-pat-file-group tb)
	   				  text)))
	   (specify "extract traceback line number"
	   	    (assert-equal "12"
				  (match-string (dbgr-loc-pat-line-group tb)
						text)))
	   )

	   (specify "prompt"
	   	    (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt)
						  "(trepan): "))
	   	    (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt)
						  "((trepan)): "))
	   	    (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt)
						  "((trepan@55)): "))
	   	    (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt)
						  "((trepan@main)): "))
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
				    (match-string (dbgr-loc-pat-file-group bps)
						  text)))
	     (specify "extract breakpoint line number"
		      (assert-equal "9"
				    (match-string (dbgr-loc-pat-line-group bps)
						  text)))
	     )
	   )

(test-unit "regexp-trepan")


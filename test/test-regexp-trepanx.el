(require 'test-unit)
(load-file "../dbgr/debugger/trepanx/init.el")
(load-file "../dbgr/lang/ruby.el")

(test-unit-clear-contexts)


(setq bps    (gethash "brkpt-set"     dbgr-trepanx-pat-hash))
(setq prompt (gethash "prompt"        dbgr-trepanx-pat-hash))
(setq tb     (gethash "backtrace"     dbgr-trepanx-pat-hash))

(defun tb-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp tb) text)
)

(defun bp-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp bps) text)
)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text" [0;31m                       Object#boom at tmp/boom.rb:2[0m")

(context "traceback location matching"
	 (tag regexp-trepanx)
	 (specify "basic traceback location"
		  (assert-t (numberp (tb-loc-match text))))
	 (specify "extract traceback file name"
		  (assert-equal 0 (tb-loc-match text))
		  (assert-equal "tmp/boom.rb"
				(match-string (dbgr-loc-pat-file-group tb)
					      text))
		  (setq text 
			"            { } in main.__script__ at /tmp/blam.rb:5")
		  (assert-equal 0 (tb-loc-match text))
		  (assert-equal "/tmp/blam.rb"
				(match-string (dbgr-loc-pat-file-group tb)
					      text))
	 (specify "extract traceback line number"
		  (assert-equal "5"
				(match-string (dbgr-loc-pat-line-group tb)
					      text)))
	 
	;;  (specify "prompt"
	;; 	  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt)
	;; 					"(trepanx): "))
	;; 	  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt)
	;; 					"((trepanx)): "))
	;; 	  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt)
	;; 					"((trepanx@55)): "))
	;; 	  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt)
	;; 					  "((trepanx@main)): "))
	;; 	    )
	   
	;;    (lexical-let ((text "Breakpoint 1 set at line 9
	;; in file /usr/local/bin/irb,
	;; VM offset 2 of instruction sequence <top (required)>."))
	;;      (specify "basic breakpoint location"
	;; 	      (assert-t (numberp (bp-loc-match text))))
	;;      (specify "extract breakpoint file name"
	;; 	      (assert-equal "/usr/local/bin/irb"
	;; 			    (match-string (dbgr-loc-pat-file-group bps)
	;; 					  text)))
	;;      (specify "extract breakpoint line number"
	;; 	      (assert-equal "9"
	;; 			    (match-string (dbgr-loc-pat-line-group bps)
	;; 					  text)))
	;;      )
	 )
	 )

(test-unit "regexp-trepanx")


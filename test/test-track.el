(require 'test-unit)
(load-file "../dbgr/common/track.el")
(load-file "../dbgr/debugger/trepan/init.el")

(test-unit-clear-contexts)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger trepan. Others may follow.
;; FIXME: encapsulate this.
(makunbound 'dbgr-cmdbuf-info)

;; FIXME/WARNING the below is customized for trepan
(context "dbgr-track"
	 (tag dbgr-track)
	 (dbgr-cmdbuf-init (current-buffer) "trepan" 
			   (gethash "trepan" dbgr-pat-hash))

	 (setq filename (symbol-file 'test-unit))
	 (setq line-number 7)
	 (setq debugger-output (format "-> (%s:%d)\n(trepan):\n" 
						 filename line-number))
	 (setq loc (dbgr-track-loc debugger-output nil))
	   
	 (specify "loc extracted"
		  (assert-equal t (dbgr-loc-p loc)))
	 (specify "loc-remaining"
		  (assert-equal "\n(trepan):\n"
				(dbgr-track-loc-remaining debugger-output)))
	 (specify "loc filename extracted"
		  (assert-equal filename (dbgr-loc-filename loc)))
	 (specify "loc line-number extracted"
		  (assert-equal line-number (dbgr-loc-line-number loc)))

	 (setq bp-num 2)
	 (setq debugger-bp-output (format "Breakpoint %d set at line %d\n\tin file %s,\n"
					  bp-num line-number filename))
	 (setq loc (dbgr-track-bp-loc debugger-bp-output nil))

	 (specify "bp-loc extracted"
		  (assert-t (dbgr-loc-p loc))
		  (assert-equal bp-num (dbgr-loc-bp-num loc)))

	 (specify "dbgr-track-divert-prompt"
		  (dbgr-cmdbuf-info-divert-output?= dbgr-cmdbuf-info 't)
		  (setq dbgr-track-divert-string "")
		  (setq text 
			"--> #0 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9\n(trepan): ")
		  (dbgr-track-divert-prompt text (current-buffer))
		  (assert-equal "--> #0 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9\n"
				dbgr-track-divert-string)
		  (assert-equal nil (dbgr-sget 'cmdbuf-info 'divert-output?))
		  )


	 (specify "invalid cmdbuf"
		  (makunbound 'dbgr-cmdbuf-info)
		  (assert-raises error 
				 (dbgr-track-from-region (point-min) 
							 (point-max))
				 )
		  )
	 )


(test-unit "dbgr-track")


(require 'test-unit)
(load-file "../dbgr/common/track.el")

(test-unit-clear-contexts)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger rbdbgr. Others may follow.
;; FIXME: encapsulate this.
(makunbound 'dbgr-cmdbuf-info)

;; FIXME/WARNING the below is customized for rbdbgr
(context "dbgr-track"
	 (tag dbgr-track)
	 (dbgr-cmdbuf-init (current-buffer) "rbdbgr" 
			   (gethash "rbdbgr" dbgr-pat-hash))

	 (setq filename (symbol-file 'test-unit))
	 (setq line-number 7)
	 (setq debugger-output (format "-> (%s:%d)\n(rbdbgr):\n" 
						 filename line-number))
	 (setq loc (dbgr-track-loc debugger-output nil))
	   
	 (specify "loc extracted"
		  (assert-equal t (dbgr-loc-p loc)))
	 (specify "loc-remaining"
		  (assert-equal "\n(rbdbgr):\n"
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

	 (specify "invalid cmdbuf"
		  (makunbound 'dbgr-cmdbuf-info)
		  (assert-raises error 
				 (dbgr-track-from-region (point-min) 
							 (point-max))
				 )
		  )
	 )


(test-unit "dbgr-track")


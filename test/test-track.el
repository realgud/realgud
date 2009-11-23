(load-file "./behave.el")
(load-file "../regexp.el")
(load-file "../init.el")
(load-file "../loc.el")
(load-file "../cmdbuf.el")
(load-file "../track.el")

(behave-clear-contexts)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger rbdbgr. Others may follow.
;; FIXME: encapsulate this.

;; dbgr-cmdbuf-info is supposed to exist in the process buffer
;; and be buffer local
(dbgr-cmdbuf-init (current-buffer) "rbdbgr" (gethash "rbdbgr" dbgr-pat-hash))

;; FIXME/WARNING the below is customized for rbdbgr
(context "dbgr-track"
	 (tag dbgr-track)

	 (setq filename (symbol-file 'behave))
	 (setq line-number 7)
	 (setq debugger-output (format "-> (%s:%d)\n(rbdbgr):\n" 
						 filename line-number))
	 (setq loc (dbgr-track-loc debugger-output))
	   
	 (specify "loc extracted"
		  (assert-equal (dbgr-loc-p loc) t))
	 (specify "loc filename extracted"
		  (assert-equal (dbgr-loc-filename loc) filename))
	 (specify "loc line-number extracted"
		  (assert-equal (dbgr-loc-line-number loc) line-number))
	 )

(behave "dbgr-track")


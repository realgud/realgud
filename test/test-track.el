(require 'test-unit)
(load-file "../dbgr/regexp.el")
(load-file "../dbgr/init.el")
(load-file "../dbgr/loc.el")
(load-file "../dbgr/cmdbuf.el")
(load-file "../dbgr/track.el")

(test-unit-clear-contexts)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger rbdbgr. Others may follow.
;; FIXME: encapsulate this.

;; dbgr-cmdbuf-info is supposed to exist in the process buffer
;; and be buffer local
(dbgr-cmdbuf-init (current-buffer) "rbdbgr" (gethash "rbdbgr" dbgr-pat-hash))

;; FIXME/WARNING the below is customized for rbdbgr
(context "dbgr-track"
	 (tag dbgr-track)

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
		  (assert-equal t (dbgr-loc-p loc))
		  (assert-equal bp-num (dbgr-loc-bp-num loc)))
	 )


(test-unit "dbgr-track")


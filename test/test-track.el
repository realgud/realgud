(load-file "./behave.el")
(load-file "../dbgr-regexp.el")
(load-file "../dbgr-loc.el")
(load-file "../dbgr-procbuf-var.el")
(load-file "../dbgr-track.el")

(behave-clear-contexts)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger rbdbgr. Others may follow.
;; FIXME: encapsulate this.
(setq dbg-name "rbdbgr")
(setq loc-pat (gethash dbg-name dbgr-pat-hash))

;; dbgr-info is supposed to exist in the process buffer
;; and be buffer local
(make-variable-buffer-local 'dbgr-info)
(setq dbgr-info (make-dbgr-info
		 :name dbg-name
		 :loc-regexp (dbgr-loc-pat-regexp      loc-pat)
		 :file-group (dbgr-loc-pat-file-group  loc-pat)
		 :line-group (dbgr-loc-pat-line-group  loc-pat))) 

;; FIXME/WARNING the below is customized for rbdbgr
(context "dbgr-track"
	 (lexical-let* ((filename (symbol-file 'behave))
			(line-number 7)
			(debugger-output (format "-> (%s:%d)\n(rbdbgr):\n" 
						 filename line-number))
			(loc (dbgr-track-loc debugger-output)))
	   
	   (tag dbgr-track)
	   (specify "loc extracted"
		    (assert-equal (dbgr-loc-p loc) t))
	   (specify "loc filename extracted"
		    (assert-equal (dbgr-loc-filename loc) filename))
	   (specify "loc line-number extracted"
		    (assert-equal (dbgr-loc-line-number loc) line-number))
	   ))

(behave "dbgr-track")


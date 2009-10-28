(load-file "./behave.el")
(load-file "../ruby/rbdbgr-regexp.el")

(behave-clear-contexts)


(setq tb (gethash "traceback" rbdbgr-pat-hash))

(defun tb-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp tb) text)
)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "	from /usr/local/bin/irb:12:in `<main>'")
(context "traceback location matching: "
	 (tag regexp-rbdbgr)
	 (lexical-let ((text "	from /usr/local/bin/irb:12:in `<main>'"))
	   (specify "basic traceback location"
		    (expect (numberp (tb-loc-match text)) t))
	   (specify "extract file name"
	   	    (expect (match-string (dbgr-loc-pat-file-group tb)
	   				  text)
	   		    equal "/usr/local/bin/irb")))
	   (specify "extract line number"
	   	    (expect (match-string (dbgr-loc-pat-line-group tb)
	   				  text)
	   		    equal "12"))
	   )

(behave "regexp-rbdbgr")


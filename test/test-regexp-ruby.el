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
(context "traceback location matching"
	 (tag regexp-rbdbgr)
	 (lexical-let ((text "	from /usr/local/bin/irb:12:in `<main>'"))
	   (specify "basic traceback location"
		    (assert-t (numberp (tb-loc-match text))))
	   (specify "extract file name"
	   	    (assert-equal "/usr/local/bin/irb"
				  (match-string (dbgr-loc-pat-file-group tb)
	   				  text)))
	   (specify "extract line number"
	   	    (assert-equal "12"
				  (match-string (dbgr-loc-pat-line-group tb)
						text)))
	   ))

(behave "regexp-rbdbgr")


(require 'test-unit)
(load-file "../init/pydbgr.el")

(test-unit-clear-contexts)


(setq tb (gethash "traceback" pydbgr-pat-hash))

(defun tb-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp tb) text)
)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")
(context "traceback location matching"
	 (tag regexp-pydbgr)
	 (specify "basic traceback location"
		  (assert-t (numberp (tb-loc-match text))))
	 (specify "extract file name"
		  (assert-equal "/usr/lib/python2.6/code.py"
				(match-string (dbgr-loc-pat-file-group tb)
					      text)
				(format "Failing file group is %s" 
					(dbgr-loc-pat-file-group tb))))
	 (specify "extract line number"
		  (assert-equal "281"
				(match-string (dbgr-loc-pat-line-group tb)
					      text)))
	   )

(test-unit "regexp-pydbgr")


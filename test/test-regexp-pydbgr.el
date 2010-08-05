(require 'test-unit)
(load-file "../dbgr/common/init/pydbgr.el")

(test-unit-clear-contexts)


(setq bps    (gethash "brkpt-set" pydbgr-pat-hash))
(setq prompt (gethash "prompt"    pydbgr-pat-hash))
(setq tb     (gethash "traceback" pydbgr-pat-hash))

(setq tb (gethash "traceback" pydbgr-pat-hash))

(defun tb-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp tb) text)
)

(defun bp-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp bps) text)
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
					      text))
		  )
	 (lexical-let ((text "Breakpoint 1 set at line 13 of file /src/git/code/gcd.py"))
	   (specify "basic breakpoint location"
		    (assert-t (numberp (bp-loc-match text))))
	   (specify "extract breakpoint file name"
	   	    (assert-equal "/src/git/code/gcd.py"
				  (match-string (dbgr-loc-pat-file-group bps)
	   				  text)))
	   (specify "extract breakpoint line number"
	   	    (assert-equal "13"
				  (match-string (dbgr-loc-pat-line-group bps)
						text)))
	   )
	 )


(test-unit "regexp-pydbgr")


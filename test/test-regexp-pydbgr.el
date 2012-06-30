(require 'test-simple)
(load-file "../dbgr/debugger/pydbgr/init.el")

(test-simple-start)

(setq bps    (gethash "brkpt-set" dbgr-pydbgr-pat-hash))
(setq loc    (gethash "loc"       dbgr-pydbgr-pat-hash))
(setq tb     (gethash "lang-backtrace" dbgr-pydbgr-pat-hash))

(defun loc-match(text var) 
  (string-match (dbgr-loc-pat-regexp var) text)
)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")

(note "traceback location matching")
(assert-t (numberp (loc-match text tb)) "basic traceback location")
(assert-equal "/usr/lib/python2.6/code.py"
	      (match-string (dbgr-loc-pat-file-group tb)
			    text)
	      (format "Failing file group is %s" 
		      (dbgr-loc-pat-file-group tb) "extract file name"))
(assert-equal "281"
	      (match-string (dbgr-loc-pat-line-group tb)
			    text) "extract line number")

(note "breakpoint location matching")
(lexical-let ((text "Breakpoint 1 set at line 13 of file /src/git/code/gcd.py"))
  (assert-t (numberp (loc-match text bps))  "basic breakpoint location")
  (assert-equal "/src/git/code/gcd.py"
		(match-string (dbgr-loc-pat-file-group bps)
			      text)   "extract breakpoint file name")
  (assert-equal "13"
		(match-string (dbgr-loc-pat-line-group bps)
			      text)   "extract breakpoint line number")
  )

(note "prompt matching")
(lexical-let ((text "(c:\\working\\python\\helloworld.py:30): <module>"))
  (assert-t (numberp (loc-match text loc))   "MS DOS position location")
  (assert-equal "c:\\working\\python\\helloworld.py"
		(match-string (dbgr-loc-pat-file-group loc)
			      text)
		(format "Failing file group is %s" 
			(dbgr-loc-pat-file-group tb) "extract file name"))
  (assert-equal "30"
		(match-string (dbgr-loc-pat-line-group loc)
			      text)   "extract line number")

  )
(lexical-let ((text "(/usr/bin/ipython:24): <module>"))
  (assert-t (numberp (loc-match text loc))   "position location")
  (assert-equal "/usr/bin/ipython"
		(match-string (dbgr-loc-pat-file-group loc)
			      text)
		(format "Failing file group is %s" 
			(dbgr-loc-pat-file-group tb) "extract file name"))
  (assert-equal "24"
		(match-string (dbgr-loc-pat-line-group loc)
			      text)   "extract line number")
  )

(end-tests)


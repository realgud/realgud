(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/pydbgr/init.el")

(test-simple-start)

(setup-regexp-vars realgud-pydbgr-pat-hash)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")

(note "traceback location matching")
(assert-t (numberp (loc-match text helper-tb)) "basic traceback location")
(assert-equal "/usr/lib/python2.6/code.py"
	      (match-string (realgud-loc-pat-file-group helper-tb)
			    text)
	      (format "Failing file group is %s"
		      (realgud-loc-pat-file-group helper-tb) "extract file name"))
(assert-equal "281"
	      (match-string (realgud-loc-pat-line-group helper-tb)
			    text) "extract line number")

(note "breakpoint location matching")
(lexical-let ((text "Breakpoint 1 set at line 13 of file /src/git/code/gcd.py"))
  (assert-t (numberp (loc-match text helper-bps))  "basic breakpoint location")
  (assert-equal "/src/git/code/gcd.py"
		(match-string (realgud-loc-pat-file-group helper-bps)
			      text)   "extract breakpoint file name")
  (assert-equal "13"
		(match-string (realgud-loc-pat-line-group helper-bps)
			      text)   "extract breakpoint line number")
  )

(note "prompt matching")
(lexical-let ((text "(c:\\working\\python\\helloworld.py:30): <module>"))
  (assert-t (numberp (loc-match text helper-loc))   "MS DOS position location")
  (assert-equal "c:\\working\\python\\helloworld.py"
		(match-string (realgud-loc-pat-file-group helper-loc)
			      text)
		(format "Failing file group is %s"
			(realgud-loc-pat-file-group helper-tb) "extract file name"))
  (assert-equal "30"
		(match-string (realgud-loc-pat-line-group helper-loc)
			      text)   "extract line number")

  )
(lexical-let ((text "(/usr/bin/ipython:24): <module>"))
  (assert-t (numberp (loc-match text helper-loc))   "position location")
  (assert-equal "/usr/bin/ipython"
		(match-string (realgud-loc-pat-file-group helper-loc)
			      text)
		(format "Failing file group is %s"
			(realgud-loc-pat-file-group helper-tb) "extract file name"))
  (assert-equal "24"
		(match-string (realgud-loc-pat-line-group helper-loc)
			      text)   "extract line number")
  )

(lexical-let
    ((text "(/tmp/eval_stringzDKTfr.py:1 remapped <string>): <module>"))
  (assert-t (numberp (loc-match text helper-loc))   "position location")
  (assert-equal "/tmp/eval_stringzDKTfr.py"
		(match-string (realgud-loc-pat-file-group helper-loc)
			      text)
		(format "Failing file group is %s"
			(realgud-loc-pat-file-group helper-tb) "extract file name"))
  (assert-equal "1"
		(match-string (realgud-loc-pat-line-group helper-loc)
			      text)   "extract line number")
  )

(end-tests)

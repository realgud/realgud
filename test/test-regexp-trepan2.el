(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/trepan2/init.el")


(test-simple-start)

(eval-when-compile
  (defvar helper-tb)
  (defvar helper-bps)
  (defvar helper-loc)
  (defvar realgud:trepan2-pat-hash)
  (defvar trepan2-text)
)

(setup-regexp-vars realgud:trepan2-pat-hash)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(lexical-let
    ((trepan2-text
      "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input"))
  (note "traceback location matching")
  (assert-t (numberp (loc-match trepan2-text helper-tb))
	    "basic traceback location")

  (assert-equal "/usr/lib/python2.6/code.py"
		(match-string (realgud-loc-pat-file-group helper-tb)
			      trepan2-text)
		(format "extract file - failing file group is %s"
			(realgud-loc-pat-file-group helper-tb)))
  (assert-equal "281"
		(match-string (realgud-loc-pat-line-group helper-tb)
			      trepan2-text) "extract line number"))

(note "breakpoint location matching")
(lexical-let
    ((trepan2-text
      "Breakpoint 1 set at line 13 of file /src/git/code/gcd.py"))
  (assert-t (numberp (loc-match trepan2-text helper-bps))
	    "basic breakpoint location")
  (assert-equal "/src/git/code/gcd.py"
		(match-string (realgud-loc-pat-file-group helper-bps)
			      trepan2-text)   "extract breakpoint file name")
  (assert-equal "13"
		(match-string (realgud-loc-pat-line-group helper-bps)
			      trepan2-text)
		"extract breakpoint line number")
  )

(note "prompt matching")
(lexical-let
    ((trepan2-text "(c:\\working\\python\\helloworld.py:30): <module>"))
  (assert-t (numberp (loc-match trepan2-text helper-loc))
	    "MS DOS position location")
  (assert-equal "c:\\working\\python\\helloworld.py"
		(match-string (realgud-loc-pat-file-group helper-loc)
			      trepan2-text)
		(format "extract file - Failing file group is %s"
			(realgud-loc-pat-file-group helper-tb)))
  (assert-equal "30"
		(match-string (realgud-loc-pat-line-group helper-loc)
			      trepan2-text)   "extract line number")

  )
(lexical-let ((trepan2-text "(/usr/bin/ipython:24): <module>"))
  (assert-t (numberp (loc-match trepan2-text helper-loc))
	    "position location")
  (assert-equal "/usr/bin/ipython"
		(match-string (realgud-loc-pat-file-group helper-loc)
			      trepan2-text)
		(format "extract-file - failing file group is %s"
			(realgud-loc-pat-file-group helper-tb)))
  (assert-equal "24"
		(match-string (realgud-loc-pat-line-group helper-loc)
			      trepan2-text)
		"extract line number")
  )

(lexical-let
    ((trepan2-text
      "(/tmp/eval_stringzDKTfr.py:1 remapped <string>): <module>"))
  (assert-t (numberp (loc-match trepan2-text helper-loc))   "position location")
  (assert-equal "/tmp/eval_stringzDKTfr.py"
		(match-string (realgud-loc-pat-file-group helper-loc)
			      trepan2-text)
		(format "extract-file name - failing file group is %s"
			(realgud-loc-pat-file-group helper-tb)))
  (assert-equal "1"
		(match-string (realgud-loc-pat-line-group helper-loc)
			      trepan2-text)   "extract line number")
  )

(note "source text")

(lexical-let
    ((trepan2-text
      "(/usr/local/bin/trepan2:4): <module>\n-- 4 [34mimport[39;49;00m [39;49;00m[04m[36msys[39;49;00m\n(trepan2) "))
  (assert-t (numberp (loc-match trepan2-text helper-loc)) "source location")
  (assert-equal
   "[34mimport[39;49;00m [39;49;00m[04m[36msys[39;49;00m"
   (match-string (realgud-loc-pat-text-group helper-loc)
		 trepan2-text)   "extract source text")
)

(end-tests)

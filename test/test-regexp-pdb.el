(require 'test-unit)
(load-file "../dbgr/debugger/pdb/init.el")

(test-unit-clear-contexts)


(setq bps-pat    (gethash "brkpt-set" dbgr-pdb-pat-hash))
(setq loc-pat    (gethash "loc"       dbgr-pdb-pat-hash))
(setq prompt-pat (gethash "prompt"    dbgr-pdb-pat-hash))
(setq tb-pat     (gethash "lang-backtrace" dbgr-pdb-pat-hash))

(defun loc-match(text var) 
  (string-match (dbgr-loc-pat-regexp var) text)
)

(defun prompt-match(prompt-str msg-fmt)
  (assert-equal 0 (loc-match prompt-str prompt-pat)
		(format msg-fmt  prompt-str))
)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")
(context "traceback location matching"
	 (tag regexp-pdb)
	 (specify "basic traceback location"
		  (assert-t (numberp (loc-match text tb-pat))))
	 (specify "extract file name"
		  (assert-equal "/usr/lib/python2.6/code.py"
				(match-string (dbgr-loc-pat-file-group tb-pat)
					      text)
				(format "Failing file group is %s" 
					(dbgr-loc-pat-file-group tb-pat))))
	 (specify "extract line number"
		  (assert-equal "281"
				(match-string (dbgr-loc-pat-line-group tb-pat)
					      text))
		  ))

(context "breakpoint location matching"
	 (tag regexp-pdb)
	 (lexical-let ((text "Breakpoint 1 at /src/git/code/gcd.py:13"))
	   (specify "basic breakpoint location"
		    (assert-t (numberp (loc-match text bps-pat))))
	   (specify "extract breakpoint file name"
	   	    (assert-equal "/src/git/code/gcd.py"
				  (match-string (dbgr-loc-pat-file-group 
						 bps-pat)
						text)))
	   (specify "extract breakpoint line number"
	   	    (assert-equal "13"
				  (match-string (dbgr-loc-pat-line-group 
						 bps-pat)
						text)))
	   )
	 )

(context "pdb prompt matching"
	 (tag regexp-pdb)
	 ;; (lexical-let ((text "(c:\\working\\python\\helloworld.py:30): <module>"))
	 ;;   (specify "MS DOS position location"
	 ;; 	    (assert-t (numberp (loc-match text loc-pat))))
	 ;;   (specify "extract file name"
	 ;; 	    (assert-equal "c:\\working\\python\\helloworld.py"
	 ;; 			(match-string (dbgr-loc-pat-file-group loc-pat)
	 ;; 				      text)
	 ;; 			(format "Failing file group is %s" 
	 ;; 				(dbgr-loc-pat-file-group tb-pat))))
	 ;; (specify "extract line number"
	 ;; 	  (assert-equal "30"
	 ;; 			(match-string (dbgr-loc-pat-line-group loc-pat)
	 ;; 				      text)))

	 ;;   )
	 (lexical-let ((text "> /usr/bin/ipython(24)<module>"))
	   (specify "position location"
		    (assert-t (numberp (loc-match text loc-pat))))
	   (specify "extract file name"
		    (assert-equal "/usr/bin/ipython"
				(match-string (dbgr-loc-pat-file-group loc-pat)
					      text)
				(format "Failing file group is %s" 
					(dbgr-loc-pat-file-group tb-pat))))
	   (specify "extract line number"
		    (assert-equal "24"
				  (match-string (dbgr-loc-pat-line-group 
						 loc-pat)
						text)))
	   
	   )

	 (setq prompt-str "(Pdb) ")
	 (specify "prompt matching"
		  (prompt-match prompt-str "valid debugger prompt: %s")
		  (setq prompt-str "((Pdb)) ")
		  (prompt-match prompt-str "valid nested debugger prompt: %s")
		  (setq prompt-str "Pdb) ")
		  (assert-nil (numberp (loc-match prompt-str prompt-pat))
			      (format "%s %s" "invalid debugger prompt"
				      prompt-str))
		  )
	 )

(test-unit "regexp-pdb")


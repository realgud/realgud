(require 'test-simple)
(load-file "../dbgr/debugger/pdb/init.el")

(test-simple-start)


(set (make-local-variable 'bps-pat)
      (gethash "brkpt-set" dbgr-pdb-pat-hash))

(set (make-local-variable 'loc-pat)
     (gethash "loc"       dbgr-pdb-pat-hash))

(set (make-local-variable 'prompt-pat)
      (gethash "prompt"    dbgr-pdb-pat-hash))

(set (make-local-variable 'tb-pat)
      (gethash "lang-backtrace" dbgr-pdb-pat-hash))

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
(set (make-local-variable 'text)
     "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")
(note "traceback location matching")

(assert-t (numberp (loc-match text tb-pat)) "basic traceback location")

(assert-equal "/usr/lib/python2.6/code.py"
	      (match-string (dbgr-loc-pat-file-group tb-pat)
			    text)
	      (format "Failing file group is %s" 
		      (dbgr-loc-pat-file-group tb-pat) "extract file name"))

(assert-equal "281"
	      (match-string (dbgr-loc-pat-line-group tb-pat)
			    text) "extract line number")

(note "breakpoint location matching")

(setq text "Breakpoint 1 at /src/git/code/gcd.py:13")
(assert-t (numberp (loc-match text bps-pat)) "basic breakpoint location")

(assert-equal "/src/git/code/gcd.py"
	      (match-string (dbgr-loc-pat-file-group 
			     bps-pat)
			    text)   "extract breakpoint file name")


(assert-equal "13"
	      (match-string (dbgr-loc-pat-line-group 
			     bps-pat)
			    text)   "extract breakpoint line number")

(note "pdb prompt matching")

;; (set text "(c:\\working\\python\\helloworld.py:30): <module>")
;; 
;; (assert-t (numberp (loc-match text loc-pat)) "MS DOS position location")
;; ;; 
;; (assert-equal "c:\\working\\python\\helloworld.py"
;; 	(match-string (dbgr-loc-pat-file-group loc-pat)
;; 		      text)
;; 	(format "Failing file group is %s" 
;; 				(dbgr-loc-pat-file-group tb-pat))
;; 	"extract file name")
;; (assert-equal "30"
;; 	      (match-string (dbgr-loc-pat-line-group loc-pat)
;; 			    text) "extract line number")

(setq text "> /usr/bin/ipython(24)<module>")
(assert-t (numberp (loc-match text loc-pat)) "position location")
(assert-equal "/usr/bin/ipython"
	      (match-string (dbgr-loc-pat-file-group loc-pat)
			    text)
	      (format "Failing file group is %s" 
		      (dbgr-loc-pat-file-group tb-pat)
		      "extract file name"))
(assert-equal "24"
	      (match-string (dbgr-loc-pat-line-group 
			     loc-pat)
			    text)
	      "extract line number")


(set (make-local-variable 'prompt-str) "(Pdb) ")
(note "prompt matching")
(prompt-match prompt-str "valid debugger prompt: %s")
(setq prompt-str "((Pdb)) ")
(prompt-match prompt-str "valid nested debugger prompt: %s")
(setq prompt-str "Pdb) ")
(assert-nil (numberp (loc-match prompt-str prompt-pat))
	    (format "%s %s" "invalid debugger prompt"
		    prompt-str))

(end-tests)


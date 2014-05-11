(require 'test-simple)
(load-file "../realgud/debugger/pydb/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)


(set (make-local-variable 'bps-pat)
      (gethash "brkpt-set" realgud:pydb-pat-hash))

(set (make-local-variable 'loc-pat)
     (gethash "loc"       realgud:pydb-pat-hash))

(set (make-local-variable 'prompt-pat)
      (gethash "prompt"    realgud:pydb-pat-hash))

(set (make-local-variable 'tb-pat)
      (gethash "lang-backtrace" realgud:pydb-pat-hash))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(set (make-local-variable 'text)
     "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")
(note "traceback location matching")

(assert-t (numberp (loc-match text tb-pat)) "basic traceback location")

(assert-equal "/usr/lib/python2.6/code.py"
	      (match-string (realgud-loc-pat-file-group tb-pat)
			    text)
	      (format "Failing file group is %s"
		      (realgud-loc-pat-file-group tb-pat) "extract file name"))

(assert-equal "281"
	      (match-string (realgud-loc-pat-line-group tb-pat)
			    text) "extract line number")

(note "breakpoint location matching")

(setq text "Breakpoint 1 at /src/git/code/gcd.py:13")
(assert-t (numberp (loc-match text bps-pat)) "basic breakpoint location")

(assert-equal "/src/git/code/gcd.py"
	      (match-string (realgud-loc-pat-file-group
			     bps-pat)
			    text)   "extract breakpoint file name")


(assert-equal "13"
	      (match-string (realgud-loc-pat-line-group
			     bps-pat)
			    text)   "extract breakpoint line number")

;; (set text "(c:\\working\\python\\helloworld.py:30): <module>")
;;
;; (assert-t (numberp (loc-match text loc-pat)) "MS DOS position location")
;; ;;
;; (assert-equal "c:\\working\\python\\helloworld.py"
;; 	(match-string (realgud-loc-pat-file-group loc-pat)
;; 		      text)
;; 	(format "Failing file group is %s"
;; 				(realgud-loc-pat-file-group tb-pat))
;; 	"extract file name")
;; (assert-equal "30"
;; 	      (match-string (realgud-loc-pat-line-group loc-pat)
;; 			    text) "extract line number")

;; (setq text "> /usr/bin/ipython(24)<module>")
;; (assert-t (numberp (loc-match text loc-pat)) "position location")
;; (assert-equal "/usr/bin/ipython"
;; 	      (match-string (realgud-loc-pat-file-group loc-pat)
;; 			    text)
;; 	      (format "Failing file group is %s"
;; 		      (realgud-loc-pat-file-group tb-pat)
;; 		      "extract file name"))
;; (assert-equal "24"
;; 	      (match-string (realgud-loc-pat-line-group
;; 			     loc-pat)
;; 			    text)
;; 	      "extract line number")


(note "prompt matching")
(set (make-local-variable 'prompt-str) "(Pydb) ")
(prompt-match prompt-str nil "debugger prompt: %s")
(setq prompt-str "((Pydb)) ")
(prompt-match prompt-str nil "nested debugger prompt: %s")
(setq prompt-str "Pydb) ")
(assert-nil (numberp (loc-match prompt-str prompt-pat))
	    (format "%s %s" "invalid debugger prompt"
		    prompt-str))

(end-tests)

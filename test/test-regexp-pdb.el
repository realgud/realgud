;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/pdb/init.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__              'load-relative)
(declare-function prompt-match          'regexp-helper)

(test-simple-start)

(eval-when-compile
  (defvar loc-pat)   (defvar prompt-pat) (defvar realgud:pdb-pat-hash)
  (defvar tb-pat)    (defvar test-text)  (defvar prompt-str)
  (defvar bps-pat)
)

(set (make-local-variable 'bps-pat)
      (gethash "brkpt-set" realgud:pdb-pat-hash))

(set (make-local-variable 'loc-pat)
     (gethash "loc"       realgud:pdb-pat-hash))

(set (make-local-variable 'prompt-pat)
      (gethash "prompt"    realgud:pdb-pat-hash))

(set (make-local-variable 'tb-pat)
      (gethash "lang-backtrace" realgud:pdb-pat-hash))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq test-text
      "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")
(note "traceback location matching")

(assert-t (numberp (loc-match test-text tb-pat)) "basic traceback location")

(assert-equal "/usr/lib/python2.6/code.py"
	      (match-string (realgud-loc-pat-file-group tb-pat)
			    test-text))

(setq test-text
      "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")
(loc-match test-text tb-pat)
(assert-equal "281"
	      (match-string (realgud-loc-pat-line-group tb-pat)
			    test-text) "extract line number")

(note "breakpoint location matching")

(setq test-text "Breakpoint 1 at /src/git/code/gcd.py:13")
(assert-t (numberp (loc-match test-text bps-pat))
	  "basic breakpoint location")

(assert-equal "/src/git/code/gcd.py"
	      (match-string (realgud-loc-pat-file-group
			     bps-pat)
			    test-text)
	      "extract breakpoint file name")


(assert-equal "13"
	      (match-string (realgud-loc-pat-line-group
			     bps-pat)
			    test-text)   "extract breakpoint line number")

;; (set test-text "(c:\\working\\python\\helloworld.py:30): <module>")
;;
;; (assert-t (numberp (loc-match test-text loc-pat)) "MS DOS position location")
;; ;;
;; (assert-equal "c:\\working\\python\\helloworld.py"
;; 	(match-string (realgud-loc-pat-file-group loc-pat)
;; 		      test-text)
;; 	(format "Failing file group is %s"
;; 				(realgud-loc-pat-file-group tb-pat))
;; 	"extract file name")
;; (assert-equal "30"
;; 	      (match-string (realgud-loc-pat-line-group loc-pat)
;; 			    test-text) "extract line number")

(setq test-text "> /usr/bin/ipython(24)<module>")
(assert-t (numberp (loc-match test-text loc-pat)) "position location")
(assert-equal "/usr/bin/ipython"
	      (match-string (realgud-loc-pat-file-group loc-pat)
			    test-text)
	      (format "Failing file group is %s"
		      (realgud-loc-pat-file-group tb-pat)))
(assert-equal "24"
	      (match-string (realgud-loc-pat-line-group
			     loc-pat)
			    test-text)
	      "extract line number")


(note "prompt matching")
(set (make-local-variable 'prompt-str) "(Pdb) ")
(prompt-match prompt-str nil "debugger prompt: %s")
(setq prompt-str "((Pdb)) ")
(prompt-match prompt-str nil "nested debugger prompt: %s")
(setq prompt-str "Pdb) ")
(assert-nil (numberp (loc-match prompt-str prompt-pat))
	    (format "%s %s" "invalid debugger prompt"
		    prompt-str))

(end-tests)

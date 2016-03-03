;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/ipdb/init.el")
(load-file "./regexp-helper.el")

(declare-function __FILE__                       'load-relative)
(declare-function loc-match	                 'realgud-helper)
(declare-function prompt-match                   'regexp-helper)
(declare-function realgud-loc-pat-file-group     'realgud-regexp)
(declare-function realgud-loc-pat-line-group     'realgud-regexp)

(test-simple-start)

(eval-when-compile
  (defvar loc-pat)   (defvar prompt-pat) (defvar realgud:ipdb-pat-hash)
  (defvar tb-pat)    (defvar test-text)  (defvar prompt-str)
  (defvar bps-pat)
)

(set (make-local-variable 'bps-pat)
      (gethash "brkpt-set" realgud:ipdb-pat-hash))

(set (make-local-variable 'loc-pat)
     (gethash "loc"       realgud:ipdb-pat-hash))

(set (make-local-variable 'prompt-pat)
      (gethash "prompt"    realgud:ipdb-pat-hash))

(set (make-local-variable 'tb-pat)
      (gethash "lang-backtrace" realgud:ipdb-pat-hash))

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
(set (make-local-variable 'prompt-str) "ipdb> ")
(prompt-match prompt-str nil "debugger prompt: %s")
(setq prompt-str "ipdb")
(assert-nil (numberp (loc-match prompt-str prompt-pat))
	    (format "%s %s" "invalid debugger prompt"
		    prompt-str))

(end-tests)

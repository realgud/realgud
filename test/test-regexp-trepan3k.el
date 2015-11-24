;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/trepan3k/init.el")

(declare-function loc-match	                 'realgud-helper)
(declare-function prompt-match                   'regexp-helper)
(declare-function realgud-loc-pat-num            'realgud-regexp)
(declare-function realgud-loc-pat-regexp         'realgud-regexp)
(declare-function realgud-loc-pat-file-group     'realgud-regexp)
(declare-function realgud-loc-pat-line-group     'realgud-regexp)
(declare-function __FILE__                       'load-relative)
(declare-function setup-regexp-vars              'regexp-helper)
(declare-function realgud-loc-pat-text-group     'realgud-trepan3k-init)


(test-simple-start)

(eval-when-compile
  (defvar file-group)
  (defvar frame-re)
  (defvar line-group)
  (defvar num-group)
  (defvar test-pos)
  (defvar test-s1)
  (defvar helper-tb)
  (defvar helper-bps)
  (defvar prompt-pat)
  (defvar realgud-pat-bt)
  (defvar helper-loc)
  (defvar realgud:trepan3k-pat-hash)
  (defvar trepan3k-text)
)

(note "prompt matching")

(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:trepan3k-pat-hash))

(prompt-match "(trepan3k) ")

(setup-regexp-vars realgud:trepan3k-pat-hash)

(note "debugger-backtrace")
(setq realgud-pat-bt  (gethash "debugger-backtrace"
			     realgud:trepan3k-pat-hash))
(setq test-s1
      "##0 gcd(a=3, b=5) called from file '/tmp/test/gcd.py' at line 31
->1 <module> exec()
     '/tmp/test/gcd2.py' at line 41
")
(setq frame-re (realgud-loc-pat-regexp realgud-pat-bt))
(setq num-group (realgud-loc-pat-num realgud-pat-bt))
(setq file-group (realgud-loc-pat-file-group realgud-pat-bt))
(setq line-group (realgud-loc-pat-line-group realgud-pat-bt))
(assert-equal 0 (string-match frame-re test-s1))
(assert-equal "0" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/tmp/test/gcd.py"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "31"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(setq test-pos (match-end 0))

(assert-equal 64 (string-match frame-re test-s1 test-pos))
(assert-equal "1" (substring test-s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/tmp/test/gcd2.py"
	      (substring test-s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "41"
	      (substring test-s1
			 (match-beginning line-group)
			 (match-end line-group)))
(setq test-pos (match-end 0))
(assert-equal 120 test-pos)

(setq helper-tb  (gethash "lang-backtrace" realgud:trepan3k-pat-hash))

(note "traceback location matching")
(setq test-s1
      "  File \"/usr/lib/python2.6/code.py\", line 281, in raw_input")

(assert-t (numberp (loc-match test-s1 helper-tb))
	  "basic traceback location")

(assert-equal "/usr/lib/python2.6/code.py"
	      (match-string (realgud-loc-pat-file-group helper-tb)
			    test-s1)
	      (format "extract file - failing file group is %s"
		      (realgud-loc-pat-file-group helper-tb)))
(assert-equal "281"
	      (match-string (realgud-loc-pat-line-group helper-tb)
			    test-s1) "extract line number")

(note "breakpoint location matching")

(setq test-s1
      "Breakpoint 1 set at line 13 of file /src/git/code/gcd.py")

(assert-t (numberp (loc-match test-s1 helper-bps))
	  "basic breakpoint location")
(assert-equal "/src/git/code/gcd.py"
	      (match-string (realgud-loc-pat-file-group helper-bps)
			    test-s1)   "extract breakpoint file name")
(assert-equal "13"
	      (match-string (realgud-loc-pat-line-group helper-bps)
			    test-s1)
	      "extract breakpoint line number")
(setq test-s1 "(c:\\working\\python\\helloworld.py:30): <module>")
(assert-t (numberp (loc-match test-s1 helper-loc))
	  "MS DOS position location")
(assert-equal "c:\\working\\python\\helloworld.py"
	      (match-string (realgud-loc-pat-file-group helper-loc)
			    test-s1)
	      (format "extract file - Failing file group is %s"
		      (realgud-loc-pat-file-group helper-tb)))
(assert-equal "30"
	      (match-string (realgud-loc-pat-line-group helper-loc)
			    test-s1)   "extract line number")

(setq test-s1 "(/usr/bin/ipython:24): <module>")
(assert-t (numberp (loc-match test-s1 helper-loc))
	  "position location")
(assert-equal "/usr/bin/ipython"
	      (match-string (realgud-loc-pat-file-group helper-loc)
			    test-s1)
	      (format "extract-file - failing file group is %s"
		      (realgud-loc-pat-file-group helper-tb)))
(assert-equal "24"
	      (match-string (realgud-loc-pat-line-group helper-loc)
			    test-s1)
	      "extract line number")

(setq test-s1
      "(/tmp/eval_stringzDKTfr.py:1 remapped <string>): <module>")
(assert-t (numberp (loc-match test-s1 helper-loc))   "position location")
(assert-equal "/tmp/eval_stringzDKTfr.py"
	      (match-string (realgud-loc-pat-file-group helper-loc)
			    test-s1)
	      (format "extract-file name - failing file group is %s"
		      (realgud-loc-pat-file-group helper-tb)))
(assert-equal "1"
	      (match-string (realgud-loc-pat-line-group helper-loc)
			    test-s1)   "extract line number")

(note "source text")

(setq test-s1
      "(/usr/local/bin/trepan3k:4): <module>\n-- 4 [34mimport[39;49;00m [39;49;00m[04m[36msys[39;49;00m\n(trepan3k) ")
(assert-t (numberp (loc-match test-s1 helper-loc)) "source location")
(assert-equal
 "[34mimport[39;49;00m [39;49;00m[04m[36msys[39;49;00m"
 (match-string (realgud-loc-pat-text-group helper-loc)
	       test-s1)   "extract source text")

(end-tests)

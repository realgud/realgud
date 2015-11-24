;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/debugger/trepanjs/init.el")
(load-file "./regexp-helper.el")

(declare-function realgud-cmdbuf-info-loc-regexp 'realgud-buffer-command)
(declare-function cmdbuf-loc-match               'realgud-regexp)
(declare-function loc-match	                 'realgud-helper)
(declare-function prompt-match                   'regexp-helper)
(declare-function realgud-loc-pat-num            'realgud-regexp)
(declare-function realgud-loc-pat-regexp         'realgud-regexp)
(declare-function realgud-loc-pat-file-group     'realgud-regexp)
(declare-function realgud-loc-pat-line-group     'realgud-regexp)
(declare-function  make-realgud-cmdbuf-info      'realgud-regexp)
(declare-function realgud-cmdbuf-info-file-group 'realgud-regexp)
(declare-function realgud-cmdbuf-info-line-group 'realgud-regexp)
(declare-function __FILE__                       'load-relative)
(declare-function setup-regexp-vars              'regexp-helper)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)
  (defvar loc-pat)
  (defvar bt-pat)
  (defvar prompt-pat)
  (defvar realgud:trepanjs-pat-hash)
  (defvar realgud-pat-hash)
  (defvar helper-bps)
  (defvar test-dbgr)
  (defvar test-text)
  (defvar test-s1)
)

(setup-regexp-vars realgud:trepanjs-pat-hash)

(note "trepanjs prompt matching")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:trepanjs-pat-hash))
(prompt-match "(trepanjs) ")
(prompt-match "[1G[0J(trepanjs) [8G[1G[0Kconnecting... ok")

(note "trepanjs location matching")
(setq dbg-name "trepanjs")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq test-dbgr (make-realgud-cmdbuf-info
		 :debugger-name dbg-name
		 :loc-regexp (realgud-loc-pat-regexp loc-pat)
		 :file-group (realgud-loc-pat-file-group loc-pat)
		 :line-group (realgud-loc-pat-line-group loc-pat)))

(setq test-text "break in test/fixtures/break-in-module/main.js at line 1:23\n")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")

(string-match (realgud-cmdbuf-info-loc-regexp test-dbgr) test-text)
(assert-equal "test/fixtures/break-in-module/main.js"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text) "extract file name")

(string-match (realgud-cmdbuf-info-loc-regexp test-dbgr) test-text)
(assert-equal "1"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-text) "extract line number")

(note "breakpoint location matching")

(setq test-text
      "Breakpoint 2 set in file /tmp/gcd.js, line 2.
")

(assert-t (numberp (loc-match test-text helper-bps))
	  "basic breakpoint location")
(assert-equal "/tmp/gcd.js"
	      (match-string (realgud-loc-pat-file-group helper-bps)
			    test-text)   "extract breakpoint file name")
(assert-equal "2"
	      (match-string (realgud-loc-pat-line-group helper-bps)
			    test-text)
	      "extract breakpoint line number")

(note "debugger-backtrace")
(setq test-text
    "##1 in file /tmp/test/gcd.js at line 2:12"
)

(set (make-local-variable
      'bt-pat)
      (gethash "debugger-backtrace" realgud:trepanjs-pat-hash))

(end-tests)

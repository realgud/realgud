(require 'test-simple)
(load-file "../dbgr/debugger/trepan.pl/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)

(set (make-local-variable 'bps-pat)
     (gethash "brkpt-set"          dbgr-trepanpl-pat-hash))
(set (make-local-variable 'dbg-bt-pat)
     (gethash "debugger-backtrace" dbgr-trepanpl-pat-hash))
(set (make-local-variable 'prompt-pat)
     (gethash "prompt"             dbgr-trepanpl-pat-hash))
(set (make-local-variable 'lang-bt-pat)
     (gethash "lang-backtrace"     dbgr-trepanpl-pat-hash))
(set (make-local-variable 'ctrl-pat)
     (gethash "control-frame"      dbgr-trepanpl-pat-hash))

(note "prompt")
(prompt-match "(trepanpl): ")
(prompt-match "((trepanpl)): " nil "nested debugger prompt: %s")

(setq text "Breakpoint 1 set in /tmp/gcd.pl at line 9")

(assert-t (numberp (loc-match text bps-pat))
	  "basic breakpoint location")


(assert-equal "/tmp/gcd.pl"
 	      (match-string (dbgr-loc-pat-file-group 
 			     bps-pat) text) 
 	      "extract breakpoint file name")

(assert-equal "9"
	      (match-string (dbgr-loc-pat-line-group 
			     bps-pat) text) 
	      "extract breakpoint line number")

(end-tests)


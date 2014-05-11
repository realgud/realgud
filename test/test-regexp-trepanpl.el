(require 'test-simple)
(load-file "../realgud/debugger/trepan.pl/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)

(set (make-local-variable 'helper-bps)
     (gethash "brkpt-set"       realgud:trepanpl-pat-hash))
(set (make-local-variable 'prompt)
     (gethash "prompt"          realgud:trepanpl-pat-hash))
(set (make-local-variable 'helper-tb)
     (gethash "lang-backtrace"  realgud:trepanpl-pat-hash))

;; ;; FIXME: we get a void variable somewhere in here when running
;; ;;        even though we define it in lexical-let. Dunno why.
;; ;;        setq however will workaround this.
;; (set (make-local-variable 'text)
;;  " [0;31m                       Object#boom at tmp/boom.rb:2[0m")

;; (assert-t (numberp (tb-loc-match text))
;; 	  "basic traceback location")

;; (assert-equal 0 (tb-loc-match text)
;; 	      "match trepanx location")
;; (assert-equal "tmp/boom.rb"
;; 	      (match-string (realgud-loc-pat-file-group tb)
;; 			    text)
;; 	      "extract traceback file name")
;; (setq text
;;       "            { } in main.__script__ at /tmp/blam.rb:5")
;; (assert-equal 0 (tb-loc-match text)
;; 	      "find a trepanx location")
;; (assert-equal "/tmp/blam.rb"
;; 	      (match-string (realgud-loc-pat-file-group tb)
;; 			    text)
;; 	      "extract traceback file name")

;; (assert-equal "5"
;; 	      (match-string (realgud-loc-pat-line-group tb)
;; 			    text)
;; 	      "extract traceback line number")

(note "prompt matching")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:trepanpl-pat-hash))
(prompt-match "(trepanpl): ")
(prompt-match "((trepanpl)): " nil "nested debugger prompt: %s")
(setq prompt-str "trepanpl:")
(assert-nil (loc-match prompt-str prompt-pat)
	    (format "invalid prompt %s" prompt-str))

(setq text "Breakpoint 2 set in /tmp/File/Basename.pm at line 215")

(assert-t (numberp (bp-loc-match text))
	  "basic breakpoint location")
(assert-equal "/tmp/File/Basename.pm"
	      (match-string (realgud-loc-pat-file-group helper-bps)
			    text)
	      "extract breakpoint file name"
	      )
(assert-equal "215"
	      (match-string (realgud-loc-pat-line-group helper-bps)
			    text)
	      "extract breakpoint line number"
	      )

(setq text "Breakpoint 1 set in (eval 1177)[/Eval.pm:94] at line 5")
(assert-t (numberp (bp-loc-match text)) "eval breakpoint location")
(set (make-local-variable 'bps-pat)
     (gethash "brkpt-set"          realgud:trepanpl-pat-hash))
(set (make-local-variable 'dbg-bt-pat)
     (gethash "debugger-backtrace" realgud:trepanpl-pat-hash))
(set (make-local-variable 'prompt-pat)
     (gethash "prompt"             realgud:trepanpl-pat-hash))
(set (make-local-variable 'lang-bt-pat)
     (gethash "lang-backtrace"     realgud:trepanpl-pat-hash))
(set (make-local-variable 'ctrl-pat)
     (gethash "control-frame"      realgud:trepanpl-pat-hash))

(note "prompt")
(prompt-match "(trepanpl): ")
(prompt-match "((trepanpl)): " nil "nested debugger prompt: %s")

(setq text "Breakpoint 1 set in /tmp/gcd.pl at line 9")

(assert-t (numberp (loc-match text bps-pat))
	  "basic breakpoint location")


(assert-equal "/tmp/gcd.pl"
 	      (match-string (realgud-loc-pat-file-group
 			     bps-pat) text)
 	      "extract breakpoint file name")

(assert-equal "9"
	      (match-string (realgud-loc-pat-line-group
			     bps-pat) text)
	      "extract breakpoint line number")

(end-tests)

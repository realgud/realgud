;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../realgud/debugger/trepan.pl/init.el")
(load-file "./regexp-helper.el")

(eval-when-compile
  (defvar realgud:trepanpl-pat-hash)
  (defvar prompt-str)
  (defvar test-dbgr)
  (defvar carp-bt-re)
  (defvar file-group)
  (defvar line-group)
  (defvar test-text)
  (defvar dbg-bt-pat)
  (defvar bps-pat)
  (defvar realgud-bt-pat)
  (defvar brkpt-del)
  (defvar bp-del-pat)
  (defvar bp-enable-pat)
  (defvar bp-disable-pat)
  (defvar realgud-perl-ignnore-file-re)
)
(declare-function __FILE__   'load-relative)

(test-simple-start)

(set (make-local-variable 'helper-bps)
     (gethash "brkpt-set"       realgud:trepanpl-pat-hash))
(set (make-local-variable 'prompt)
     (gethash "prompt"          realgud:trepanpl-pat-hash))
(set (make-local-variable 'helper-tb)
     (gethash "lang-backtrace"  realgud:trepanpl-pat-hash))


(set (make-local-variable 'bp-del-pat)
      (gethash "brkpt-del" realgud:trepanpl-pat-hash))

(set (make-local-variable 'bp-enable-pat)
      (gethash "brkpt-enable" realgud:trepanpl-pat-hash))

(set (make-local-variable 'bp-disable-pat)
      (gethash "brkpt-disable" realgud:trepanpl-pat-hash))

(note "prompt matching")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:trepanpl-pat-hash))
(prompt-match "(trepanpl): ")
(prompt-match "((trepanpl)): " nil "nested debugger prompt: %s")
(setq prompt-str "trepanpl:")
(assert-nil (loc-match prompt-str prompt-pat)
	    (format "invalid prompt %s" prompt-str))

(setq test-text "Breakpoint 2 set in /tmp/File/Basename.pm at line 215")

(assert-t (numberp (bp-loc-match test-text))
	  "basic breakpoint location")
(assert-equal "/tmp/File/Basename.pm"
	      (match-string (realgud-loc-pat-file-group helper-bps)
			    test-text)
	      "extract breakpoint file name"
	      )
(assert-equal "215"
	      (match-string (realgud-loc-pat-line-group helper-bps)
			    test-text)
	      "extract breakpoint line number"
	      )

(setq test-text "Breakpoint 1 set in (eval 1177)[/Eval.pm:94] at line 5")
(assert-t (numberp (bp-loc-match test-text)) "eval breakpoint location")
(setq bps-pat
     (gethash "brkpt-set"          realgud:trepanpl-pat-hash))
(setq dbg-bt-pat
     (gethash "debugger-backtrace" realgud:trepanpl-pat-hash))
(setq prompt-pat
     (gethash "prompt"             realgud:trepanpl-pat-hash))
(setq lang-bt-pat
     (gethash "lang-backtrace"     realgud:trepanpl-pat-hash))

(note "prompt")
(prompt-match "(trepanpl): ")
(prompt-match "((trepanpl)): " nil "nested debugger prompt: %s")

(setq test-text "Breakpoint 1 set in /tmp/gcd.pl at line 9")

(assert-t (numberp (loc-match test-text bps-pat))
	  "basic breakpoint location")


(assert-equal "/tmp/gcd.pl"
 	      (match-string (realgud-loc-pat-file-group
 			     bps-pat) test-text)
 	      "extract breakpoint file name")

(assert-equal "9"
	      (match-string (realgud-loc-pat-line-group
			     bps-pat) test-text)
	      "extract breakpoint line number")

;; (note "breakpoint delete matching")
;; (setq test-text "Deleted breakpoint 1.\n")
;; (assert-t (numberp (loc-match test-text bp-del-pat)) "breakpoint delete matching")

(note "breakpoint enable matching")
(setq test-text "Breakpoint 4 enabled.\n")
(assert-t (numberp (loc-match test-text bp-enable-pat)) "breakpoint enable matching")


(note "breakpoint disable matching")
(setq test-text "Breakpoint 2 disabled.\n")
(assert-t (numberp (loc-match test-text bp-disable-pat)) "breakpoint disable matching")

(end-tests)

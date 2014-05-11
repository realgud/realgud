(require 'test-simple)
(load-file "../realgud/debugger/remake/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)

(set (make-local-variable 'prompt-pat)
     (gethash "prompt"             realgud:remake-pat-hash))
(set (make-local-variable 'frame-pat)
     (gethash "debugger-backtrace" realgud:remake-pat-hash))

(note "remake prompt")
(prompt-match "remake<10> "  "10")
(prompt-match	"remake<<1>> " "1" "recursive remake %s")

(note "remake debugger-backtrace")
(setq s1
      "=>#0  Makefile.in at /tmp/Makefile:216
  #1  Makefile at /tmp/Makefile:230
")

(set (make-local-variable 'frame-re)
     (realgud-loc-pat-regexp frame-pat))
(set (make-local-variable 'num-group)
     (realgud-loc-pat-num frame-pat))
(set (make-local-variable 'file-group)
     (realgud-loc-pat-file-group frame-pat))
(set (make-local-variable 'line-group)
     (realgud-loc-pat-line-group frame-pat))

(assert-equal 0 (string-match frame-re s1))
(assert-equal "0" (substring s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/tmp/Makefile"
	      (substring s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "216"
	      (substring s1
			 (match-beginning line-group)
			 (match-end line-group)))
(set (make-local-variable 'pos)
     (match-end 0))

(assert-equal 39 (string-match frame-re s1 pos))
(assert-equal "1" (substring s1
			     (match-beginning num-group)
			     (match-end num-group)))
(assert-equal "/tmp/Makefile"
	      (substring s1
			 (match-beginning file-group)
			 (match-end file-group)))
(assert-equal "230"
	      (substring s1
			 (match-beginning line-group)
			 (match-end line-group)))

(end-tests)

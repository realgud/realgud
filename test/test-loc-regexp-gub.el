(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/gub/init.el")

(test-simple-start)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "gub")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group  loc-pat)
		  :line-group (realgud-loc-pat-line-group  loc-pat)))


(setq text "interp/testdata/square.go:15:6")
(assert-t (numberp (cmdbuf-loc-match text dbgr)) "basic location")

(note "extract file name")
(assert-equal 0 (cmdbuf-loc-match text dbgr))
(assert-equal "interp/testdata/square.go"
	      (match-string (realgud-cmdbuf-info-file-group dbgr)
			    text))

(assert-equal "15"
	      (match-string
	       (realgud-cmdbuf-info-line-group dbgr)
	       text) "extract line number")

(setq text " interp/testdata/square.go:15:6")
(assert-nil (numberp (cmdbuf-loc-match text dbgr)) "location starts with blank")


(end-tests)

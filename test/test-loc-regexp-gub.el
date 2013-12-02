(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/gub/init.el")

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)
  (defvar loc-pat)
  (defvar test-dbgr)
  (defvar test-text)
)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger trepan. Others may follow.
;; FIXME: encapsulate this.
(setq dbg-name "gub")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq test-dbgr (make-realgud-cmdbuf-info
		 :debugger-name dbg-name
		 :loc-regexp (realgud-loc-pat-regexp loc-pat)
		 :file-group (realgud-loc-pat-file-group loc-pat)
		 :line-group (realgud-loc-pat-line-group loc-pat)))


(setq test-text "interp/testdata/square.go:15:6")
(assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")

(note "extract file name")
(assert-equal 0 (cmdbuf-loc-match test-text test-dbgr))
(assert-equal "interp/testdata/square.go"
	      (match-string (realgud-cmdbuf-info-file-group test-dbgr)
			    test-text))

(assert-equal "15"
	      (match-string
	       (realgud-cmdbuf-info-line-group test-dbgr)
	       test-text) "extract line number")

(setq test-text " interp/testdata/square.go:15:6")
(assert-nil (numberp (cmdbuf-loc-match test-text test-dbgr))
	    "location starts with blank")


(end-tests)

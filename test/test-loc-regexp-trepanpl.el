(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/trepan.pl/init.el")

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)
  (defvar realgud-pat-hash)
  (defvar loc-pat)
  (defvar dbgr)
  (defvar text)
)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepan.pl")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group  loc-pat)
		  :line-group (realgud-loc-pat-line-group  loc-pat)))


(setq text "-- main::(../example/gcd.pl:18)")
(assert-t (numberp (cmdbuf-loc-match text dbgr)) "basic location")

(note "extract file name")
(assert-equal 0 (cmdbuf-loc-match text dbgr))
(assert-equal "../example/gcd.pl"
	      (match-string (realgud-cmdbuf-info-file-group dbgr)
			    text))

(assert-equal "18"
	      (match-string
	       (realgud-cmdbuf-info-line-group dbgr)
	       text) "extract line number")


(end-tests)

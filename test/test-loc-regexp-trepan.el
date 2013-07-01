(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/trepan/init.el")

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)
  (defvar realgud-pat-hash)
  (defvar dbgr)
  (defvar text)
)


; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepan")
(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))

(setq dbgr (make-realgud-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (realgud-loc-pat-regexp loc-pat)
		  :file-group (realgud-loc-pat-file-group  loc-pat)
		  :line-group (realgud-loc-pat-line-group  loc-pat)))


(setq text "-- (/usr/local/bin/irb:9 @2)")
(assert-t (numberp (cmdbuf-loc-match text dbgr)) "basic location")

(note "extract file name")
(assert-equal 0 (cmdbuf-loc-match text dbgr))
(assert-equal "/usr/local/bin/irb"
	      (match-string (realgud-cmdbuf-info-file-group dbgr)
			    text))

(assert-equal "9"
	      (match-string
	       (realgud-cmdbuf-info-line-group dbgr)
	       text) "extract line number")

(setq text "-> (<internal:lib/rubygems/custom_require>:28 remapped /usr/lib/ruby/gems/1.9.1/gems/data/custom_require.rb:28 @2)")
(assert-t (numberp (cmdbuf-loc-match text dbgr)) "remapped location")

(assert-equal 0 (cmdbuf-loc-match text dbgr))
(assert-equal "/usr/lib/ruby/gems/1.9.1/gems/data/custom_require.rb"
	      (match-string (realgud-cmdbuf-info-file-group dbgr)
			    text) "extract remapped file name")

(assert-equal "28"
	      (match-string
	       (realgud-cmdbuf-info-line-group dbgr)
	       text) "extract remapped line number")

(setq text "C> (/tmp/c-func.rb:2)")
(assert-t (numberp (cmdbuf-loc-match text dbgr)) "basic location for C fn")

(assert-equal 0 (cmdbuf-loc-match text dbgr))
(assert-equal "/tmp/c-func.rb"
	      (match-string (realgud-cmdbuf-info-file-group dbgr)
			    text) "extract file name for C fn")

(assert-equal "2"
	      (match-string
	       (realgud-cmdbuf-info-line-group dbgr)
	       text) "extract line number for C fn")

(end-tests)

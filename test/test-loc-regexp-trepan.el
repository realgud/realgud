(require 'test-simple)
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/trepan/init.el")

(test-simple-start)

; Some setup usually done in setting up the buffer.
; We customize this for the debugger trepan. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "trepan")
(setq loc-pat (gethash "loc" (gethash dbg-name dbgr-pat-hash)))

(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group  loc-pat)
		  :line-group (dbgr-loc-pat-line-group  loc-pat)))


(defun loc-match(text) 
  (string-match (dbgr-cmdbuf-info-loc-regexp dbgr) text)
)


(setq text "-- (/usr/local/bin/irb:9 @2)")
(assert-t (numberp (loc-match text)) "basic location")

(note "extract file name")
(assert-equal 0 (loc-match text))
(assert-equal "/usr/local/bin/irb"
	      (match-string (dbgr-cmdbuf-info-file-group dbgr)
			    text))

(assert-equal "9"
	      (match-string 
	       (dbgr-cmdbuf-info-line-group dbgr)
	       text) "extract line number")

(setq text "-> (<internal:lib/rubygems/custom_require>:28 remapped /usr/lib/ruby/gems/1.9.1/gems/data/custom_require.rb:28 @2)")
(assert-t (numberp (loc-match text)) "remapped location")
	 
(assert-equal 0 (loc-match text))
(assert-equal "/usr/lib/ruby/gems/1.9.1/gems/data/custom_require.rb"
	      (match-string (dbgr-cmdbuf-info-file-group dbgr)
			    text) "extract remapped file name")

(assert-equal "28"
	      (match-string 
	       (dbgr-cmdbuf-info-line-group dbgr)
	       text) "extract remapped line number")

(setq text "C> (/tmp/c-func.rb:2)")
(assert-t (numberp (loc-match text)) "basic location for C fn")

(assert-equal 0 (loc-match text))
(assert-equal "/tmp/c-func.rb"
	      (match-string (dbgr-cmdbuf-info-file-group dbgr)
			    text) "extract file name for C fn")

(assert-equal "2"
	      (match-string 
	       (dbgr-cmdbuf-info-line-group dbgr)
	       text) "extract line number for C fn")

(end-tests)

(require 'test-unit)
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/gdb/init.el")

(test-unit-clear-contexts)


; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "gdb")

(setq loc-pat (gethash "loc" (gethash dbg-name dbgr-pat-hash)))
(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group loc-pat)
		  :line-group (dbgr-loc-pat-line-group loc-pat)))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "/home/rocky/c/ctest.c:80:2000:beg:0x8048748>")
(context "traceback location matching"
	 (tag regexp-gdb)
	 (defun loc-match(text) 
	   (string-match (dbgr-cmdbuf-info-loc-regexp dbgr) text)
	   )
	 (specify "basic location"
		  (assert-t (numberp (loc-match text))))
	 (specify "extract file name"
		  (assert-equal "/home/rocky/c/ctest.c"
				(match-string (dbgr-cmdbuf-info-file-group dbgr)
					      text)))
	 
	 (specify "extract line number"
		  (assert-equal "80"
				(match-string (dbgr-cmdbuf-info-line-group dbgr)
					      text)))
	 (specify "debugger-backtrace"
		  (setq dbgr-bt-pat  (gethash "debugger-backtrace"  
					     dbgr-gdb-pat-hash))
		  (setq s1
			"#0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
#1  0xb7e9f4a5 in *__GI___strdup (s=0xbffff760 \"/tmp/remake/remake\") at strdup.c:42
#2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
    at main.c:952
#46 0xb7f51b87 in vm_call_cfunc (th=0x804d188, reg_cfp=0xb7ba9e88, num=0, 
    recv=157798080, blockptr=0x0, me=0x80d12a0) at vm_insnhelper.c:410
")
		  (setq dbgr-bt-re (dbgr-loc-pat-regexp dbgr-bt-pat))
		  (setq file-group (dbgr-loc-pat-file-group dbgr-bt-pat))
		  (setq line-group (dbgr-loc-pat-line-group dbgr-bt-pat))
		  (assert-equal 0 (string-match dbgr-bt-re s1))
		  (assert-equal "main.c"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "935"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))

		  (setq pos (match-end 0))
		  (assert-equal 65 pos)
		  (assert-equal 65 (string-match dbgr-bt-re s1 pos))
		  (assert-equal "strdup.c"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "42"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))

		  (setq pos (match-end 0))
		  (assert-equal 149 pos)
		  (assert-equal 149 (string-match dbgr-bt-re s1 pos))
		  (assert-equal "main.c"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "952"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))

		  (setq pos (match-end 0))
		  (assert-equal 233 pos)
		  (assert-equal 233 (string-match dbgr-bt-re s1 pos))
		  (assert-equal "vm_insnhelper.c"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "410"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))
		  )		  
	 )

(test-unit "regexp-gdb")


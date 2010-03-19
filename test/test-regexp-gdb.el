(require 'test-unit)
(load-file "../dbgr/common/init/gdb.el")
(load-file "../dbgr/common/cmdbuf.el")

(test-unit-clear-contexts)


; Some setup usually done in setting up the buffer.
; We customize this for the debugger rbdbgr. Others may follow.
; FIXME: encapsulate this.
(setq dbg-name "gdb")
(setq loc-pat (gethash "loc" (gethash dbg-name dbgr-pat-hash)))
(setq bps (gethash "brkpt-set" gdb-pat-hash))

(setq dbgr (make-dbgr-cmdbuf-info
		  :debugger-name dbg-name
		  :loc-regexp (dbgr-loc-pat-regexp loc-pat)
		  :file-group (dbgr-loc-pat-file-group  loc-pat)
		  :line-group (dbgr-loc-pat-line-group  loc-pat)))

(defun loc-match(text) 
  (string-match (dbgr-cmdbuf-info-loc-regexp dbgr) text)
)

(defun bp-loc-match(text) 
  (string-match (dbgr-loc-pat-regexp bps) text)
)

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
(setq text "/home/rocky/c/ctest.c:80:2000:beg:0x8048748>")
(context "traceback location matching"
	 (tag regexp-gdb)
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
)

(test-unit "regexp-gdb")


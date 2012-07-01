(require 'test-simple)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/common/backtrace-mode.el")
(load-file "../dbgr/common/init.el")
(load-file "../dbgr/debugger/zshdb/init.el")
(load-file "./bt-helper.el")

(test-simple-start)

(note "fontify")
(setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
(setq temp-bt (generate-new-buffer "*bt-test*"))
(with-current-buffer temp-cmdbuf
  (switch-to-buffer temp-cmdbuf)
  (dbgr-cmdbuf-init temp-cmdbuf "zshdb" 
		    (gethash "zshdb" dbgr-pat-hash))
  
  (switch-to-buffer nil)
  )
(setup-bt 
 "->0 in file `/test/autogen.sh' at line 2
##1 /test/autogen.sh called from file `/usr/local/bin/zshdb' at line 121
"
 temp-bt temp-cmdbuf)
(with-current-buffer temp-bt
  (switch-to-buffer temp-bt)
  (goto-char (point-min))
  (dolist (pair 
	   '(
	     ("->" .    dbgr-backtrace-number )
	     ("/test" . dbgr-file-name)
	     ("line " . dbgr-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  (switch-to-buffer nil)
  )

(end-tests)


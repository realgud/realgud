(load-file "./bt-helper.el")
(load-file "../dbgr/debugger/zshdb/init.el")

(test-simple-start)

(setq temp-bt
      (setup-bt "zshdb"
		"->0 in file `/test/autogen.sh' at line 2
##1 /test/autogen.sh called from file `/usr/local/bin/zshdb' at line 121
"))
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
  )

(end-tests)


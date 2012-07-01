(load-file "./bt-helper.el")
(load-file "../dbgr/debugger/trepanx/init.el")

(test-simple-start)

(setq temp-bt
      (setup-bt "trepanx"
		"   0 Trepanning(Object)#debug_program(dbgr, ruby_path, program_to_debug) at /foo.rb:10
   3 main.__script__ at /bin/trepan:19
   4 Kernel(Object)#load(name) at kernel/common/kernel.rb:678
   5 main.__script__ at /home/rocky-rvm/.rvm/gems/rbx-head/bin/trepan:19
"))

(with-current-buffer temp-bt
  (switch-to-buffer temp-bt)
  (goto-char (point-min))
  (dolist (pair 
	   '(
	     ("   "    .   dbgr-backtrace-number )
	     ("Trepan" . font-lock-constant-face )
	     ("Objec"  . font-lock-variable-name-face )
	     ("#"      . font-lock-variable-name-face )
	     ("("      .  font-lock-variable-name-face )
	     ("/foo"   .  dbgr-file-name)
	     ("1"      . dbgr-line-number)
	     ("   "    . dbgr-backtrace-number)
	     ("mai"    . font-lock-constant-face )
	     ("/bin"   . dbgr-file-name)
	     ("1"     . dbgr-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  )

(end-tests)

(require 'test-simple)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/common/backtrace-mode.el")
(load-file "../dbgr/debugger/trepanx/init.el")
(load-file "./bt-helper.el")

(test-simple-start)

(note "fontify")
(setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
(setq temp-bt (generate-new-buffer "*bt-test*"))
(with-current-buffer temp-cmdbuf
  (switch-to-buffer temp-cmdbuf)
  (dbgr-cmdbuf-init temp-cmdbuf "trepanx" 
		    (gethash "trepanx" dbgr-pat-hash))
  
  (switch-to-buffer nil)
  )

(setup-bt 
 "   0 Trepanning(Object)#debug_program(dbgr, ruby_path, program_to_debug) at /foo.rb:10
   3 main.__script__ at /bin/trepan:19
   4 Kernel(Object)#load(name) at kernel/common/kernel.rb:678
   5 main.__script__ at /home/rocky-rvm/.rvm/gems/rbx-head/bin/trepan:19
"
 temp-bt temp-cmdbuf)
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
  (switch-to-buffer nil)
  )

(end-tests)

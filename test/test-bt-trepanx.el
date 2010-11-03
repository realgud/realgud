(require 'test-unit)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/debugger/trepanx/init.el")

(test-unit-clear-contexts)

(defun setup-bt(string temp-bt temp-cmdbuf)
  (with-current-buffer temp-bt
    (dbgr-backtrace-mode temp-cmdbuf)
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (insert string)
    (font-lock-fontify-buffer)
    (goto-char (point-min))
    ))


(context "dbgr-buffer-backtrace-trepanx"
	 (tag dbgr-buf-bt-trepanx)
	 (specify "fontify"
		  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
		  (with-current-buffer temp-cmdbuf
		    (dbgr-cmdbuf-init temp-cmdbuf "trepanx" 
				      (gethash "trepanx" dbgr-pat-hash))
		    
		    )
		  (setq temp-bt (generate-new-buffer "*bt-test*"))
		  (setup-bt 
"   0 Trepanning(Object)#debug_program(dbgr, ruby_path, program_to_debug) at /foo.rb:10
   3 main.__script__ at /bin/trepan:19
   4 Kernel(Object)#load(name) at kernel/common/kernel.rb:678
   5 main.__script__ at /home/rocky-rvm/.rvm/gems/rbx-head/bin/trepan:19
"
			    temp-bt temp-cmdbuf)
		  (with-current-buffer temp-bt
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
		  )
	 )
(test-unit "dbgr-buf-bt-trepanx")


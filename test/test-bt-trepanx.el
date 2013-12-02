(load-file "./bt-helper.el")
(load-file "../realgud/debugger/trepanx/init.el")

(declare-function setup-bt 'realgud-bt-helper)

(test-simple-start)

(eval-when-compile
  (defvar temp-bt)
)

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
	     ("   "    .   realgud-backtrace-number )
	     ("Trepan" . font-lock-constant-face )
	     ("Objec"  . font-lock-variable-name-face )
	     ("#"      . font-lock-variable-name-face )
	     ("("      .  font-lock-variable-name-face )
	     ("/foo"   .  realgud-file-name)
	     ("1"      . realgud-line-number)
	     ("   "    . realgud-backtrace-number)
	     ("mai"    . font-lock-constant-face )
	     ("/bin"   . realgud-file-name)
	     ("1"     . realgud-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  )

(end-tests)

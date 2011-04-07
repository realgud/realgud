(require 'test-unit)
(load-file "../dbgr/common/send.el")
(load-file "../dbgr/common/regexp.el")
(load-file "../dbgr/debugger/trepan/init.el")
(test-unit-clear-contexts)

(defvar temp-cmdbuf nil)
(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  (dbgr-cmdbuf-init temp-cmdbuf "trepan" (gethash "trepan" dbgr-pat-hash))
  (dbgr-srcbuf-init (current-buffer) temp-cmdbuf 
		    "trepan" 
		    '("/bin/trepan" "my-script" "arg1"))
)

(defun tear-down()
  (kill-buffer temp-cmdbuf)
)

(context "dbgr-send"
	 (tag send)
	 (specify "format no expand characters"
		  (dolist (str '("abc" "100%" "I feel %% today"))
		    (assert-equal str (dbgr-expand-format str)))
	   )
	 (specify "format %l - with arg"
		  (assert-equal "line 5" (dbgr-expand-format "line %p" 5)))
	 (specify "format %l - without arg"
		  (assert-equal "line " (dbgr-expand-format "line %p")))

	 (specify "format %s"
		  (assert-equal "hi, rocky!" 
				(dbgr-expand-format "h%s!" "i, rocky")))

	 (setup)
	 ;; Current buffer is now set up as a source buffer
	 (setq file-name (buffer-file-name))
	 (specify "File formatting"
		  (if (and file-name (dbgr-get-srcbuf (current-buffer)))
		      (dolist 
			  (pair 
			   (list 
			    (cons "%d" (file-name-directory file-name))
			    (cons "%x" file-name)
			    (cons "%X" (expand-file-name file-name))
			    (cons "%f" "test-send.el")
			    (cons "%F" "test-send")))
			(assert-equal (cdr pair) (dbgr-expand-format (car pair))))))
	 (tear-down)

	 (specify "dbgr-dbgr-command - not a command buffer"
		  (assert-raises error (dbgr-command "testing")))

)

(test-unit "send")


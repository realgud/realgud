(load-file "./behave.el")
(load-file "../dbgr-send.el")
(load-file "../dbgr-regexp.el")
(load-file "../dbgr-init.el")
(behave-clear-contexts)

(defvar temp-cmdbuf nil)
(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  (dbgr-cmdbuf-init temp-cmdbuf "rbdbgr" (gethash "rbdbgr" dbgr-pat-hash))
  (dbgr-srcbuf-init (current-buffer) temp-cmdbuf 
		    "rbdbgr" 
		    '("/bin/rbdbgr" "my-script" "arg1"))
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

	 (setup)
	 ;; Current buffer is now set up as a source buffer
	 (setq file-name (buffer-file-name))
	 (if file-name
	     (specify "File formatting"
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
)

(behave "send")


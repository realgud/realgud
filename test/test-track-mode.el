(require 'test-unit)
(load-file "../dbgr/rbdbgr/rbdbgr.el")
(test-unit-clear-contexts)

(defvar temp-cmdbuf nil)
(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  ;; (start-process "test-track-mode" temp-cmdbuf nil)
  (start-process "test-track-mode" temp-cmdbuf "/bin/sh")

  (dbgr-cmdbuf-init temp-cmdbuf "rbdbgr" (gethash "rbdbgr" dbgr-pat-hash))
  (with-current-buffer temp-cmdbuf 
    (rbdbgr-track-mode 't))
  (dbgr-srcbuf-init (current-buffer) temp-cmdbuf 
		    "rbdbgr" 
		    '("/bin/rbdbgr" "my-script" "arg1"))
)

(defun tear-down()
  (kill-buffer temp-cmdbuf)
)

(context "dbgr-track-mode"
	 (tag track-mode)
	 (setup)

	 ;; Current buffer is now set up as a source buffer

	 (specify "track-functions-mapped-to-keys"
		  (with-current-buffer temp-cmdbuf
		    (dolist (fn '(dbgr-track-hist-newest
				  dbgr-track-hist-newer
				  dbgr-track-hist-older
				  dbgr-track-hist-oldest))
		      (assert-nil (null (where-is-internal fn)) fn)
		      )
		    ))
				       
)

(test-unit "track-mode")


(require 'test-simple)
(load-file "../dbgr/common/buffer/source.el")
(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/debugger/trepan/init.el")

(test-simple-start)

(defvar temp-cmdbuf nil)
(defun tear-down()
  (kill-buffer temp-cmdbuf)
  (kill-buffer temp-srcbuf)
)

(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  (dbgr-cmdbuf-init temp-cmdbuf "trepan" (gethash "trepan" dbgr-pat-hash))
  (setq temp-srcbuf (find-file-noselect "./gcd.rb"))
)

(assert-nil (dbgr-srcbuf? (current-buffer)) "dbgr-srcbuf? before init")
(setq dbgr-srcbuf-info nil)
(assert-nil (dbgr-srcbuf? (current-buffer)) 
	    "dbgr-srcbuf? before init - but nil")
(assert-equal nil (dbgr-srcbuf-command-string (current-buffer))
	      "dbgr-srcbuf-command-string - uninit")

(note "dbgr-srcbuf-init")
(setup)
(dbgr-srcbuf-init temp-srcbuf temp-cmdbuf
		  "trepan"
		  '("/bin/trepan" "--emacs" "gcd.rb" "1"))
(assert-equal "trepan" 
	      (with-current-buffer temp-srcbuf
		(dbgr-srcbuf-info-debugger-name 
		 dbgr-srcbuf-info)))

(assert-t (dbgr-srcbuf? temp-srcbuf)
	  "dbgr-srcbuf? after init")

(assert-equal "/bin/trepan --emacs gcd.rb 1"
	      (dbgr-srcbuf-command-string 
	       temp-srcbuf)
	      "dbgr-srcbuf-command-string")

(assert-equal temp-cmdbuf
	      (with-current-buffer temp-srcbuf
		(dbgr-srcbuf-info-cmdproc dbgr-srcbuf-info)))
	 
(dbgr-srcbuf-init-or-update temp-srcbuf temp-cmdbuf)
(assert-equal temp-cmdbuf
	      (with-current-buffer temp-srcbuf
		(dbgr-srcbuf-info-cmdproc dbgr-srcbuf-info))
	      "dbgr-srcbuf-init-or-update - update")

(kill-buffer temp-srcbuf)
(setq temp-srcbuf (find-file-noselect "./gcd.rb"))
(dbgr-srcbuf-init-or-update temp-srcbuf temp-cmdbuf)
(assert-equal temp-cmdbuf
	      (with-current-buffer temp-srcbuf
			 (dbgr-srcbuf-info-cmdproc dbgr-srcbuf-info))
	      "dbgr-srcbuf-init-or-update - init")
(tear-down)

(end-tests)

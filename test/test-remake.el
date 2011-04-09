(require 'test-unit)
(load-file "../dbgr/common/core.el") ;; for dbgr-exec-shell
(load-file "../dbgr/debugger/remake/remake.el")

(test-unit-clear-contexts)

(defun dbgr-exec-shell (debugger-name script-filename program 
				      &optional no-reset &rest args)
  "Mock for dbgr-exec-shell. We copy the part of the real dbgr-exec-shell
file-name-directory that was failing"
  (let ((cmdproc-buffer (get-buffer-create "foo"))
	(starting-directory
		(or (file-name-directory script-filename)
		    default-directory "./")))
    (start-process "my-process" cmdproc-buffer "sleep" "10000")
    cmdproc-buffer
    )
  )

(context "remake"
	 (tag remake)

	 (specify "can deal with no Makefile name"
		  ;; If dbgr-remake is successful we switch buffers
		  (setq my-buf (current-buffer))
		  (dbgr-remake "remake --debugger")
		  (assert-t (not (eq (current-buffer) my-buf)))
		  (delete-process "foo")
		  (switch-to-buffer my-buf)
	      )
	 )

(test-unit "remake")


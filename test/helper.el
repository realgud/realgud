(require 'test-unit)

(makunbound 'file-exists?)
(load-file "../dbgr/common/helper.el")

(test-unit-clear-contexts)

(context 
 "helper functions"
 (tag helper)
 (specify "fn-p-to-fn?-alias"
 	  (makunbound 'symbol?)
 	  (fn-p-to-fn?-alias 'symbolp)
 	  (assert-t (symbol? 'foo)
 		    "symbol? be a defined function")

 	  (makunbound 'file-exists?)
 	  (fn-p-to-fn?-alias 'file-exists-p)
 	  (assert-t (file-exists? "./test-unit.el")
 		    "file-exists? should be a defined function")

 	  (makunbound 'equal?)
 	  (fn-p-to-fn?-alias 'equal)
 	  (assert-t (equal? "a" "a")
 		    "equal? should be a defined function")
 	  )
 
 (specify "buffer-killed? - nil"
 	  (assert-nil (buffer-killed? (current-buffer))))

 (specify "buffer-killed? - t"
 	  (let ((buf (get-buffer-create
 		      (generate-new-buffer-name "*temp file test*"))))
 	    (kill-buffer buf)
 	    (assert-t (buffer-killed? buf))))
 
 (specify "with-current-buffer-safe -t "
 	  (let ((buf (get-buffer-create
 		      (generate-new-buffer-name "*temp file test*"))))
 	    (assert-t
 	     (with-current-buffer-safe buf
 	       t))))
 
 (specify "with-current-buffer-safe - killed"
	  (let ((buf (get-buffer-create
		      (generate-new-buffer-name "*temp file test*"))))
	    (kill-buffer buf)
	    (assert-nil
	     (with-current-buffer-safe buf
	       t))))

 (specify "dbgr-sget"
	  (defstruct dbgr-xxx-info name)
	  (setq dbgr-xxx-info (make-dbgr-xxx-info))
	    (setf (dbgr-xxx-info-name dbgr-xxx-info) 20)
	    (assert-equal 20 (dbgr-sget 'xxx-info 'name))
	  )
)

(test-unit "helper")


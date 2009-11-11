(load-file "./behave.el")

(makunbound 'file-exists?)
(load-file "../dbgr-helper.el")

(behave-clear-contexts)


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
	  (assert-t (file-exists? "./behave.el")
		    "file-exists? should be a defined function")

	  (makunbound 'equal?)
	  (fn-p-to-fn?-alias 'equal)
	  (assert-t (equal? "a" "a")
		    "equal? should be a defined function")
	  ))

(behave "helper")


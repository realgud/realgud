(require 'test-unit)
(load-file "../dbgr/pydbgr/pydbgr.el")

(test-unit-clear-contexts)

(context "pydbgr"
	 (tag pydbgr)

	 (specify "pydbgr-suggest-python-file"
		  (assert-equal "gcd.py" 
				(pydbgr-suggest-python-file))
		  )
	 ;; (specify "pydbgr-get-script-name"
	 ;;      (assert-equal '("foo" nil) 
	 ;; 		    (pydbgr-get-script-name '("pydbgr" "foo")))
	 ;;      (assert-equal '("foo" nil) 
	 ;; 		    (pydbgr-get-script-name '("pydbgr" "-m" "foo")))
	 ;;      (assert-equal '("foo" t)
	 ;; 		    (pydbgr-get-script-name
	 ;; 		     '("pydbgr" "--emacs" "3" "foo")))
	 ;;      (assert-equal '("foo" t) 
	 ;; 		    (pydbgr-get-script-name
	 ;; 		     '("mypydbgr" "--annotate=1" "foo")))
	 ;;      (assert-equal '("foo" t) 
	 ;; 		    (pydbgr-get-script-name
	 ;; 		     '("ruby" "pydbgr" "--annotate" "1" "foo")))
	 ;;      (assert-equal '("foo" nil) 
	 ;; 		    (pydbgr-get-script-name
	 ;; 		     '("/usr/bin/ruby19" "pydbgr" 
	 ;; 		       "--emacs-basic" "foo")))
	 ;;      (assert-equal '("foo" nil) 
	 ;; 		    (pydbgr-get-script-name '("rdbg.rb" "foo")))
	 ;;      (assert-equal '("rdbg.rb" nil) 
	 ;; 		    (pydbgr-get-script-name 
	 ;; 		     '("pydbgr" "rdbg.rb" "foo")))
	 ;;      (assert-equal '("foo" t) 
	 ;; 		    (pydbgr-get-script-name 
	 ;; 		     '("pydbgr" "-A" "1" "foo")))
	 ;;      (assert-equal '("foo" nil)
	 ;; 		    (pydbgr-get-script-name
	 ;; 		     '("pydbgr" "--include" "me" "-n" "foo")))
	 ;;      (assert-equal '("foo" nil) 
	 ;; 		    (pydbgr-get-script-name
	 ;; 		     '("pydbgr" "--server" "-d" "--host"
	 ;; 		       "localhost" "foo" "-1")))
	 ;;      )
	 )

(test-unit "pydbgr")


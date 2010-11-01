(require 'test-unit)
(require 'font-lock)

(load-file "../dbgr/common/buffer/command.el")
(load-file "../dbgr/common/buffer/backtrace.el")
(load-file "../dbgr/rdebug/init.el")

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


(context "dbgr-buffer-backtrace-rdebug"
	 (tag dbgr-buf-bt-rdebug)
	 (specify "fontify"
		  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
		  (with-current-buffer temp-cmdbuf
		    (dbgr-cmdbuf-init temp-cmdbuf "rdebug" 
				      (gethash "rdebug" dbgr-pat-hash))
		    
		    )
		  (setq temp-bt (generate-new-buffer "*bt-test*"))
		  (setup-bt 
"--> #0 Object.gcd(a#Fixnum, b#Fixnum) 
       at line /test/gcd.rb:6
    #1 at line /test/gcd.rb:19
"
			    temp-bt temp-cmdbuf)
		  (with-current-buffer temp-bt
		    (goto-char (point-min))
		    (dolist (pair 
			     '(
			       ("#" .     font-lock-constant-face )
			       ("Objec" . font-lock-type-face )
			       ("gc"    . font-lock-function-name-face )
			       ("("     . font-lock-variable-name-face )
			       ("Fixnu" . font-lock-type-face )
			       ("/test" . dbgr-file-name)
			       (":"     . dbgr-line-number)
			       ("#"     . font-lock-constant-face)
			       ("/test" . dbgr-file-name)
			       (":"     . dbgr-line-number)
			       ))
		      (search-forward (car pair))
		      (assert-equal (cdr pair)
				    (get-text-property (point) 'face))
		      )
		    )
		  )
	 )
(test-unit "dbgr-buf-bt-rdebug")


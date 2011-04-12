(require 'test-unit)
(load-file "../dbgr/debugger/zshdb/init.el")

(test-unit-clear-contexts)

(setq prompt-pat (gethash "prompt"        dbgr-zshdb-pat-hash))
(setq frame-pat  (gethash "debugger-backtrace" dbgr-zshdb-pat-hash))

(defun prompt-match(prompt-str num-str) 
  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt-pat)
				prompt-str))
  (assert-equal num-str (substring prompt-str 
				   (match-beginning 1) (match-end 1)))
)

(context "zshdb prompt matching"
	 (tag regexp-zshdb)
	 (specify "prompt"
		  (prompt-match "zshdb<10> "  "10")
		  (prompt-match	"zshdb<(5)> " "5")
		  (prompt-match	"zshdb<<1>> " "1")
		  )
	 )

(context "zshdb frame matching"
	 (tag regexp-zshdb)

	 (specify "debugger-backtrace"
		  (setq s1
			"->0 in file `/etc/apparmor/functions' at line 24
##1 /etc/apparmor/functions called from file `/etc/init.d/apparmor' at line 35
##2 /etc/init.d/apparmor called from file `/usr/local/bin/zshdb' at line 129
")
		  (setq frame-re (dbgr-loc-pat-regexp frame-pat))
		  (setq num-group (dbgr-loc-pat-num frame-pat))
		  (setq file-group (dbgr-loc-pat-file-group frame-pat))
		  (setq line-group (dbgr-loc-pat-line-group frame-pat))
		  (assert-equal 0 (string-match frame-re s1))
		  (assert-equal "0" (substring s1 
					       (match-beginning num-group)
					       (match-end num-group)))
		  (assert-equal "/etc/apparmor/functions"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "24"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))
		  (setq pos (match-end 0))
		  
		  (assert-equal 49 (string-match frame-re s1 pos))
		  (assert-equal "1" (substring s1 
					       (match-beginning num-group)
					       (match-end num-group)))
		  (assert-equal "/etc/init.d/apparmor"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "35"
				(substring s1 
					   (match-beginning line-group)
					   (match-end line-group)))
		  (setq pos (match-end 0))
		  (assert-equal 128 (string-match frame-re s1 pos))
		  (assert-equal "2" (substring s1 
					       (match-beginning num-group)
					       (match-end num-group)))
		  (assert-equal "/usr/local/bin/zshdb"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "129"
				(substring s1
					   (match-beginning line-group)
					   (match-end line-group)))
		  )
	 )

(test-unit "regexp-zshdb")


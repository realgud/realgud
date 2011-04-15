(require 'test-unit)
(load-file "../dbgr/debugger/bashdb/init.el")

(test-unit-clear-contexts)

(setq prompt-pat (gethash "prompt"             dbgr-bashdb-pat-hash))

(defun prompt-match(prompt-str num-str) 
  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt-pat)
				prompt-str))
  (assert-equal num-str (substring prompt-str 
				   (match-beginning 1) (match-end 1)))
)

(context "bashdb prompt matching"
	 (tag regexp-bashdb)
	 (specify "prompt"
		  (prompt-match "bashdb<10> "  "10")
		  (prompt-match	"bashdb<(5)> " "5")
		  (prompt-match	"bashdb<<1>> " "1")
		  )
	 )

(context "bashdb frame matching"
	 (tag regexp-bashdb)

	 (specify "debugger-backtrace"
		  (setq dbgr-pat-bat  (gethash "debugger-backtrace" 
					     dbgr-bashdb-pat-hash))
		  (setq s1
			"->0 in file `/etc/apparmor/functions' at line 24
##1 source(\"/etc/apparmor/functions\") called from file `/etc/init.d/apparmor' at line 35
##2 source(\"/etc/init.d/apparmor\") called from file `/usr/local/bin/bashdb' at line 140
##3 main() called from file `/bin/bashdb' at line 0
")
		  (setq frame-re (dbgr-loc-pat-regexp dbgr-pat-bat))
		  (setq num-group (dbgr-loc-pat-num dbgr-pat-bat))
		  (setq file-group (dbgr-loc-pat-file-group dbgr-pat-bat))
		  (setq line-group (dbgr-loc-pat-line-group dbgr-pat-bat))
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
		  (assert-equal 138 (string-match frame-re s1 pos))
		  (assert-equal "2" (substring s1 
					       (match-beginning num-group)
					       (match-end num-group)))
		  (assert-equal "/usr/local/bin/bashdb"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  (assert-equal "140"
				(substring s1
					   (match-beginning line-group)
					   (match-end line-group)))
		  (setq pos (match-end 0))
		  (assert-equal 226 (string-match frame-re s1 pos))
		  (assert-equal "3" (substring s1 
					       (match-beginning num-group)
					       (match-end num-group)))
		  (assert-equal "/bin/bashdb"
				(substring s1 
					   (match-beginning file-group)
					   (match-end file-group)))
		  )
	 )

(test-unit "regexp-bashdb")


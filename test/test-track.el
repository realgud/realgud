(require 'test-simple)
(load-file "../dbgr/common/track.el")
(load-file "../dbgr/common/core.el")
(load-file "../dbgr/debugger/trepan/init.el")

(test-simple-start)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger trepan. Others may follow.
;; FIXME: encapsulate this.
(makunbound 'dbgr-cmdbuf-info)

;; FIXME/WARNING the below is customized for trepan
(dbgr-cmdbuf-init (current-buffer) "trepan" 
		  (gethash "trepan" dbgr-pat-hash))

(setq filename (symbol-file 'test-simple))
(setq line-number 7)
(setq debugger-output (format "-> (%s:%d)\n(trepan):\n" 
			      filename line-number))
(lexical-let ((loc (dbgr-track-loc debugger-output nil)))
  (assert-t (dbgr-loc-p loc)   "loc extracted")
  (assert-equal "\n(trepan):\n"
		(dbgr-track-loc-remaining debugger-output) "loc-remaining")
  (assert-equal filename (dbgr-loc-filename loc) "loc filename extracted")
  (assert-equal line-number (dbgr-loc-line-number loc)
		"loc line-number extracted")
  )

(note "dbgr-track-selected-frame")
(setq debugger-output "up 
--> #1 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9
   (/usr/local/bin/irb:9 @11)
require irb'
")
(assert-equal 1 (dbgr-track-selected-frame debugger-output))
	 
(setq debugger-output " 
--> #0 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9
   (/usr/local/bin/irb:9 @11)
require irb'
")
(assert-equal 0 (dbgr-track-selected-frame debugger-output))
	 
(setq debugger-output "
<- (<internal:lib/rubygems/custom_require>:38 remapped /usr/local/lib/ruby/gems/1.9.1/gems/trepanning-0.1.3.dev/data/custom_require.rb:38 @16)
R=> false
end
")
(assert-nil (dbgr-track-selected-frame debugger-output))


(note "dbgr-track-termination?")
(setq debugger-output "-- (/usr/local/bin/irb:9 @2)
require 'irb'
")
(assert-nil (dbgr-track-termination? debugger-output))
(setq debugger-output "Really quit? (N/y) y
trepan: That's all, folks...
")
(assert-t (dbgr-track-termination? debugger-output))


;; (setq debugger-bp-output (format "Breakpoint %d set at line %d\n\tin file %s.\n"
;; 				  bp-num line-number filename))
;; (setq bp-loc (dbgr-track-bp-loc debugger-bp-output nil))
;; (setq bp-num 2)

;; (specify "bp-loc extracted"
;; 	  (message "output: %s" debugger-bp-output)
;; 	  (message "bp-loc: %s" bp-loc)
;; 	  (message "bp-num: %d" bp-num)
;; 	  (assert-t (dbgr-loc-p bp-loc))
;; 	  (assert-equal bp-num (dbgr-loc-num bp-loc)))

;; (specify "dbgr-track-divert-prompt"
;; 	  (dbgr-cmdbuf-info-divert-output?= 't)
;; 	  (setq dbgr-track-divert-string "")
;; 	  (setq text 
;; 		"--> #0 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9\n(trepan): ")
;; 	  (setq dbgr-last-output-start (point-max))
;; 	  (dbgr-track-divert-prompt text (current-buffer) (point-max))
;; 	  (assert-equal "--> #0 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9\n"
;; 			dbgr-track-divert-string)
;; 	  (assert-equal nil (dbgr-sget 'cmdbuf-info 'divert-output?))
;; 	  )

(makunbound 'dbgr-cmdbuf-info)
(assert-raises error 
	       (dbgr-track-from-region (point-min) 
				       (point-max))
	       "invalid cmdbuf")

(end-tests)

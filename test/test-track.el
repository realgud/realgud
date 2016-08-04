;; -*- lexical-binding:t -*-

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/track.el")
(load-file "../realgud/common/core.el")
(load-file "../realgud/common/loc.el")
(load-file "../realgud/debugger/trepan/core.el")
(load-file "../realgud/debugger/trepan/init.el")

(declare-function __FILE__                     'load-relative)
(declare-function realgud-cmdbuf-init          'realgud-buffer-command)
(declare-function realgud-loc-filename         'realgud-loc)
(declare-function realgud-loc-p                'realgud-loc)
(declare-function realgud-loc-line-number      'realgud-loc)
(declare-function realgud:track-from-region    'realgud-track)
(declare-function realgud-track-loc            'realgud-track)
(declare-function realgud-track-loc-remaining  'realgud-track)
(declare-function realgud-track-selected-frame 'realgud-track)
(declare-function realgud-track-termination?   'realgud-track)

(test-simple-start)

(eval-when-compile
  (defvar debugger-output)
  (defvar line-number)
  (defvar realgud-pat-hash)
  (defvar test-filename)
)

;; Some setup usually done in setting up the buffer.
;; We customize this for the debugger trepan. Others may follow.
;; FIXME: encapsulate this.
(makunbound 'realgud-cmdbuf-info)

;; FIXME/WARNING the below is customized for trepan
(realgud-cmdbuf-init (current-buffer) "trepan"
		  (gethash "trepan" realgud-pat-hash))

(setq test-filename (symbol-file 'test-simple))
(setq line-number 7)
(setq debugger-output (format "-> (%s:%d)\nrequire 'foo'\n(trepan):\n"
			      test-filename line-number))
(let ((loc (realgud-track-loc debugger-output nil)))
  (assert-t (realgud-loc-p loc)   "loc extracted")
  (assert-equal "(trepan):\n"
		(realgud-track-loc-remaining debugger-output)
		"loc-remaining")
  (assert-equal test-filename (realgud-loc-filename loc)
		"loc filename extracted")
  (assert-equal line-number (realgud-loc-line-number loc)
		"loc line-number extracted")
  )

(note "realgud-track-selected-frame")
(setq debugger-output "up
--> #1 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9
   (/usr/local/bin/irb:9 @11)
require irb'
")
(assert-equal 1 (realgud-track-selected-frame debugger-output))

(setq debugger-output "
--> #0 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9
   (/usr/local/bin/irb:9 @11)
require irb'
")
(assert-equal 0 (realgud-track-selected-frame debugger-output))

(setq debugger-output "
<- (<internal:lib/rubygems/custom_require>:38 remapped /usr/local/lib/ruby/gems/1.9.1/gems/trepanning-0.1.3.dev/data/custom_require.rb:38 @16)
R=> false
end
")
(assert-nil (realgud-track-selected-frame debugger-output))


(note "realgud-track-termination?")
(setq debugger-output "-- (/usr/local/bin/irb:9 @2)
require 'irb'
")
(assert-nil (realgud-track-termination? debugger-output))
(setq debugger-output "Really quit? (N/y) y
trepan: That's all, folks...
")
(assert-t (realgud-track-termination? debugger-output))


;; (setq debugger-bp-output (format "Breakpoint %d set at line %d\n\tin file %s.\n"
;; 				  bp-num line-number test-filename))
;; (setq bp-loc (realgud-track-bp-loc debugger-bp-output nil))
;; (setq bp-num 2)

;; (specify "bp-loc extracted"
;; 	  (message "output: %s" debugger-bp-output)
;; 	  (message "bp-loc: %s" bp-loc)
;; 	  (message "bp-num: %d" bp-num)
;; 	  (assert-t (realgud-loc-p bp-loc))
;; 	  (assert-equal bp-num (realgud-loc-num bp-loc)))

;; (specify "realgud-track-divert-prompt"
;; 	  (realgud-cmdbuf-info-divert-output?= 't)
;; 	  (setq realgud-track-divert-string "")
;; 	  (setq text
;; 		"--> #0 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9\n(trepan): ")
;; 	  (setq realgud-last-output-start (point-max))
;; 	  (realgud-track-divert-prompt text (current-buffer) (point-max))
;; 	  (assert-equal "--> #0 TOP Object#<top /usr/local/bin/irb> in file /usr/local/bin/irb at line 9\n"
;; 			realgud-track-divert-string)
;; 	  (assert-equal nil (realgud-sget 'cmdbuf-info 'divert-output?))
;; 	  )

(makunbound 'realgud-cmdbuf-info)
(assert-raises error
	       (realgud:track-from-region (point-min)
				       (point-max))
	       "invalid cmdbuf")

(end-tests)

(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/trepan/init.el")
(declare-function realgud-cmdbuf-init 'realgud-buffer-command)

(test-simple-start)

(eval-when-compile
  (defvar temp-cmdbuf)
  (defvar realgud-pat-hash)
  (defvar realgud-cmdbuf-info)
)

(assert-nil (realgud-cmdbuf? (current-buffer))
	    "realgud-cmdbuf? before init")

(assert-equal nil (realgud-cmdbuf-command-string (current-buffer))
	      "realgud-cmdbuf-command-string - uninit")
(setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
(assert-t (realgud-cmdbuf-init temp-cmdbuf "trepan"
			    (gethash "trepan" realgud-pat-hash))
	  "realgud-cmdbuf-init")

(with-current-buffer temp-cmdbuf
  (switch-to-buffer temp-cmdbuf)
  (realgud-cmdbuf-info-cmd-args= '("command" "args"))
  (assert-equal "command args"
		(realgud-cmdbuf-command-string temp-cmdbuf))
  (assert-equal "trepan"
		(realgud-cmdbuf-debugger-name))
  (assert-equal nil
		(realgud-cmdbuf-info-srcbuf-list
		 realgud-cmdbuf-info)
		"srcbuf-list should start out nil")
  (realgud-cmdbuf-add-srcbuf (current-buffer) temp-cmdbuf)
  (assert-equal (list (current-buffer))
		(realgud-cmdbuf-info-srcbuf-list
		 realgud-cmdbuf-info)
		"should have added one item to srcbuf-list")
  (realgud-cmdbuf-add-srcbuf (current-buffer) temp-cmdbuf)
  (assert-equal (list (current-buffer))
		(realgud-cmdbuf-info-srcbuf-list
		 realgud-cmdbuf-info)
		"Second source buffer same as first; should have added still only one item.")
  (switch-to-buffer nil)
  )

(end-tests)

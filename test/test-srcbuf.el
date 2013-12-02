(require 'test-simple)
(load-file "../realgud/common/buffer/source.el")
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/trepan/init.el")

(declare-function realgud-cmdbuf-init 'realgud-buffer-command)
(declare-function realgud-srcbuf-init 'realgud-buffer-source)

(test-simple-start)

(defvar temp-cmdbuf nil)
(defun tear-down()
  (kill-buffer temp-cmdbuf)
  (kill-buffer temp-srcbuf)
)

(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  (realgud-cmdbuf-init temp-cmdbuf "trepan" (gethash "trepan" realgud-pat-hash))
  (setq temp-srcbuf (find-file-noselect "./gcd.rb"))
)

(assert-nil (realgud-srcbuf? (current-buffer)) "realgud-srcbuf? before init")
(setq realgud-srcbuf-info nil)
(assert-nil (realgud-srcbuf? (current-buffer))
	    "realgud-srcbuf? before init - but nil")
(assert-equal nil (realgud-srcbuf-command-string (current-buffer))
	      "realgud-srcbuf-command-string - uninit")

(note "realgud-srcbuf-init")
(setup)
(realgud-srcbuf-init temp-srcbuf temp-cmdbuf
		  "trepan"
		  '("/bin/trepan" "--emacs" "gcd.rb" "1"))
(assert-equal "trepan"
	      (with-current-buffer temp-srcbuf
		(realgud-srcbuf-info-debugger-name
		 realgud-srcbuf-info)))

(assert-t (realgud-srcbuf? temp-srcbuf)
	  "realgud-srcbuf? after init")

(assert-equal "/bin/trepan --emacs gcd.rb 1"
	      (realgud-srcbuf-command-string
	       temp-srcbuf)
	      "realgud-srcbuf-command-string")

(assert-equal temp-cmdbuf
	      (with-current-buffer temp-srcbuf
		(realgud-srcbuf-info-cmdproc realgud-srcbuf-info)))

(realgud-srcbuf-init-or-update temp-srcbuf temp-cmdbuf)
(assert-equal temp-cmdbuf
	      (with-current-buffer temp-srcbuf
		(realgud-srcbuf-info-cmdproc realgud-srcbuf-info))
	      "realgud-srcbuf-init-or-update - update")

(kill-buffer temp-srcbuf)
(setq temp-srcbuf (find-file-noselect "./gcd.rb"))
(realgud-srcbuf-init-or-update temp-srcbuf temp-cmdbuf)
(assert-equal temp-cmdbuf
	      (with-current-buffer temp-srcbuf
			 (realgud-srcbuf-info-cmdproc realgud-srcbuf-info))
	      "realgud-srcbuf-init-or-update - init")
(tear-down)

(end-tests)

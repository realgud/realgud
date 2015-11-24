;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/buffer/source.el")
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/trepan/init.el")
(load-file "../realgud/debugger/trepan/track-mode.el")

(declare-function realgud-cmdbuf-init 'realgud-buffer-command)
(declare-function realgud-srcbuf-init 'realgud-buffer-source)
(declare-function __FILE__            'load-relative)

(declare-function realgud-srcbuf?                   'realgud-buffer-source)
(declare-function realgud-srcbuf-loc-p              'realgud-loc)
(declare-function realgud-srcbuf-info-debugger-name 'realgud-loc)
(declare-function realgud-srcbuf-info-cmdproc       'realgud-track)
(declare-function realgud-srcbuf-init-or-update     'realgud-track)

(test-simple-start)

(eval-when-compile
  (defvar realgud-pat-hash)
  (defvar realgud-srcbuf-info)
  (defvar temp-srcbuf)
  (defvar test-filename)
)

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

(note "realgud-srcbuf-init")
(setup)
(realgud-srcbuf-init temp-srcbuf temp-cmdbuf)

(assert-t (realgud-srcbuf? temp-srcbuf)
	  "realgud-srcbuf? after init")

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

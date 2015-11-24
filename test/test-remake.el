;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/core.el") ;; for realgud-exec-shell
(load-file "../realgud/debugger/remake/remake.el")

(eval-when-compile (defvar my-buf))

(declare-function realgud:remake        'realgud:remake)
(declare-function __FILE__              'load-relative)

(test-simple-start)

(defun realgud-exec-shell (debugger-name script-filename program
				      &optional no-reset &rest args)
  "Mock for realgud-exec-shell. We copy the part of the real realgud-exec-shell
file-name-directory that was failing"
  (let ((cmdproc-buffer (get-buffer-create "foo"))
	(starting-directory
		(or (file-name-directory script-filename)
		    default-directory "./")))
    (start-process "my-process" cmdproc-buffer "sleep" "10000")
    cmdproc-buffer
    )
  )

(note "can deal with no Makefile name")
;; If realgud:remake is successful we switch buffers
(setq my-buf (current-buffer))
;; FIXME:
;; (realgud:remake "remake --debugger")
;; (assert-t (not (eq (current-buffer) my-buf)))
;; (delete-process "foo")
;; (switch-to-buffer my-buf)

(end-tests)

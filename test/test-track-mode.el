(require 'test-simple)
(load-file "../realgud/debugger/trepan/trepan.el")
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/common/track-mode.el")
(load-file "../realgud/common/backtrace-mode.el")

(declare-function realgud-cmdbuf-init 'realgud-buffer-command)
(declare-function realgud-srcbuf-init 'realgud-buffer-source)
(declare-function __FILE__            'require-relative)

(test-simple-start)

(defvar realgud-pat-hash)
(defvar temp-cmdbuf nil)

(declare-function trepan-track-mode 'realgud:trepan)
(declare-function realgud-track-mode-vars 'realgud-track-mode)
(declare-function realgud-backtrace-mode 'realgud-backtrace-mode)

(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  ;; (start-process "test-track-mode" temp-cmdbuf nil)
  (start-process "test-track-mode" temp-cmdbuf "/bin/sh")

  (realgud-cmdbuf-init temp-cmdbuf "trepan" (gethash "trepan" realgud-pat-hash))
  (with-current-buffer temp-cmdbuf
    (trepan-track-mode 't))
  (realgud-srcbuf-init (current-buffer) temp-cmdbuf)
)

(defun tear-down()
  (kill-buffer temp-cmdbuf)
)

(setup)

;; Current buffer is now set up as a source buffer

(with-current-buffer temp-cmdbuf
  (switch-to-buffer temp-cmdbuf)
  (dolist (fn '(realgud-track-hist-newest
		realgud-track-hist-newer
		realgud-track-hist-older
		realgud-track-hist-oldest))
    (assert-nil (null (where-is-internal fn))
		(format "track-functions-mapped-to-keys %s" fn))
    )
  (switch-to-buffer nil)
  )

(note "track-mode-vars")
(makunbound 'foo-track-mode)
(makunbound 'foo-track-mode-map)
(realgud-track-mode-vars "foo")
(dolist (var '("foo-track-mode-map" "foo-track-mode"))
  (let ((var-sym (intern var)))
    (assert-t (boundp var-sym))
    (assert-t (stringp (get var-sym 'variable-documentation)))
    ))

(end-tests)

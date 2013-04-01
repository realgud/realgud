(require 'test-simple)
(load-file "../realgud/debugger/trepan/trepan.el")
(test-simple-start)

(defvar temp-cmdbuf nil)
(defun setup ()
  (setq temp-cmdbuf (generate-new-buffer "*cmdbuf-test*"))
  ;; (start-process "test-track-mode" temp-cmdbuf nil)
  (start-process "test-track-mode" temp-cmdbuf "/bin/sh")

  (realgud-cmdbuf-init temp-cmdbuf "trepan" (gethash "trepan" realgud-pat-hash))
  (with-current-buffer temp-cmdbuf
    (trepan-track-mode 't))
  (realgud-srcbuf-init (current-buffer) temp-cmdbuf
		    "trepan"
		    '("/bin/trepan" "my-script" "arg1"))
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

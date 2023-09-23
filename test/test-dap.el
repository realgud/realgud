;; Tests for DAP -*- lexical-binding: t -*-

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -L %s -l %s" (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "loc-changes.elc")) buffer-file-name)

(require 'test-simple)

(test-simple-start)

(load-file "../realgud/dap/dap.el")

(setq test-message "Content-Length: 82

{\"seq\": 153, \"type\": \"request\", \"command\": \"next\", \"arguments\": { \"threadId\": 3 }}")

(setq server-buffer (generate-new-buffer "*server-buffer*"))
;; https://github.com/methane/echoserver/blob/master/server_elisp.el
(setq test-server
      (make-network-process
       :name "test-server"
       :buffer server-buffer
       :filter (lambda (process string)
                 (process-send-string process test-message) )
       ; (delete-process process) close connection
       :host 'local
       :service 't
       :server 't
       ; :filter-multibyte 't ??
       ))
(realgud--dap-start-client "127.0.0.1" (nth 1 (process-contact test-server )))

(defun setup-fake-cmdbuf ()
  (kill-all-local-variables)
  (realgud-cmdbuf-init (current-buffer) "dap" realgud:dap-pat-hash) )

(with-temp-buffer
  ;; https://github.com/rocky/emacs-test-simple/pull/13/files
  (insert "test")
  (assert-t 't)
  (assert-nil (equal (point-min) (point-max))
              "your version of test-simple is breaking buffers. make an update") )


(note "cmdbuf setup")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (assert-t (realgud-cmdbuf? (current-buffer)))
  (let-alist realgud-dap-message-que
    (assert-nil .que (pp-to-string .que))
    (assert-t (mutexp .mutex))
    (assert-t (condition-variable-p .notify-var))
    ))

(note "connection buffer is empty after passing full message")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (insert test-message)
  (let* ((ret (realgud--dap-parse-output)))
    (assert-t ret)
    (assert-t (equal (point-min) (point-max) ))
    ))

(note "empty connection buffer don't mess things up")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (let* ((ret (realgud--dap-parse-output)))
    (assert-nil ret)
    (assert-equal (point-min) (point-max) )
    (let-alist realgud-dap-message-que
      (assert-nil .que (pp-to-string .que)))
    ))

(note "trailing partial message")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (assert-equal 0 (proper-list-p (alist-get 'que realgud-dap-message-que)) "que len")
  (insert (concat test-message "x"))
  (let* ((ret (realgud--dap-parse-output)))
    (assert-t ret)
    (assert-nil (equal (point-min) (point-max)) "Buffer is empty!")
    (assert-t (string-equal "x" (buffer-string)) (concat "Buffer content is: " (buffer-string)) )
    (let-alist realgud-dap-message-que
      (assert-equal 1 (proper-list-p .que) (concat ".que: " (pp-to-string .que))) )
    ))

(note "2 messages")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (assert-equal 0 (proper-list-p (alist-get 'que realgud-dap-message-que)) "que len")
  (insert (concat test-message test-message))
  (let* ((ret (realgud--dap-parse-output)))
    (assert-t ret)
    (assert-nil (equal (point-min) (point-max)) "Buffer is empty!")
    (let-alist realgud-dap-message-que
      (assert-equal 1 (proper-list-p .que) (concat ".que: " (pp-to-string .que))) )
    )
  (let* ((ret (realgud--dap-parse-output)))
    (assert-t ret)
    (assert-t (equal (point-min) (point-max)) "Buffer is NOT empty!")
    (let-alist realgud-dap-message-que
      (assert-equal 2 (proper-list-p .que) (concat ".que: " (pp-to-string .que))) )
    ) )

(end-tests)

;; Tests for DAP -*- lexical-binding: t -*-

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -L %s -l %s" (file-name-directory (locate-library "load-relative.el")) (file-name-directory (locate-library "test-simple.el")) (file-name-directory (locate-library "loc-changes.el")) buffer-file-name)

(require 'test-simple)

(test-simple-start)

(load-file "../realgud/dap/dap.el")
(defvar backtrace-mode-map nil)
(defvar backtrace-print nil)
(setq test-message "Content-Length: 82

{\"seq\": 153, \"type\": \"request\", \"command\": \"next\", \"arguments\": { \"threadId\": 3 }}")

(defun realgud--dap-get-free-port nil
  (let* ((free-port-finder
          (make-network-process
           :name "free port finder"
           :filter (lambda (process string)
                     (process-send-string process "I just want to find free port.")
                     )
           :host 'local
           :service 't
           :server 't
       ; :filter-multibyte 't ??
           ))
         ;; https://github.com/methane/echoserver/blob/master/server_elisp.el
         (port (nth 1 (process-contact free-port-finder))))
    (delete-process free-port-finder)
    port
))

(defun setup-fake-cmdbuf ()
  (let* ((test-server)
         (port (realgud--dap-get-free-port)))
    (kill-all-local-variables)
  (setq-local test-server
                (make-network-process
                 :name "test-server"
                 :buffer (generate-new-buffer "*test-server-buffer*")
                 :filter (lambda (process string)
                           (process-send-string process
                                                string)
                           )
                 :host 'local
                 :service port
                 :server 't
                 ;; :filter-multibyte 't ??
                 ))
  (realgud-cmdbuf-init (current-buffer) "dap" realgud:dap-pat-hash)
  (realgud--dap-start-client "127.0.0.1"
                             port))
  )

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
  (assert-nil realgud-dap-message-que (pp-to-string realgud-dap-message-que))
  (assert-t (mutexp realgud-dap-mutex))
  (assert-t (condition-variable-p realgud-dap-notify-var))
  )

(note "connection buffer is empty after passing full message")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (insert test-message)
  (let* ((ret (realgud--dap-parse-output (current-buffer))))
    (assert-t ret)
    (assert-t (equal (point-min) (point-max) ))
    ))

(note "empty connection buffer don't mess things up")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (let* ((ret (realgud--dap-parse-output (current-buffer))))
    (assert-nil ret)
    (assert-equal (point-min) (point-max) )
    (assert-nil realgud-dap-message-que (pp-to-string realgud-dap-message-que)))
    )

(note "trailing partial message")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (assert-equal 0 (proper-list-p realgud-dap-message-que) "que len")
  (insert (concat test-message "x"))
  (let* ((ret (realgud--dap-parse-output (current-buffer))))
    (assert-t ret)
    (assert-nil (equal (point-min) (point-max)) "Buffer is empty!")
    (assert-t (string-equal "x" (buffer-string)) (concat "Buffer content is: " (buffer-string)) )
    (assert-equal 1 (proper-list-p realgud-dap-message-que) (concat "realgud-dap-message-que: " (pp-to-string realgud-dap-message-que)))
    ))

(note "2 messages")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (assert-equal 0 (proper-list-p (alist-get 'que realgud-dap-message-que)) "que len")
  (insert (concat test-message test-message))
  (let* ((ret (realgud--dap-parse-output (current-buffer))))
    (assert-t ret)
    (assert-nil (equal (point-min) (point-max)) "Buffer is empty!")
    (assert-equal 1 (proper-list-p realgud-dap-message-que) (concat "realgud-dap-message-que: " (pp-to-string realgud-dap-message-que)))
    )
  (let* ((ret (realgud--dap-parse-output (current-buffer))))
    (assert-t ret)
    (assert-t (equal (point-min) (point-max)) "Buffer is NOT empty!")
    (assert-equal 2 (proper-list-p realgud-dap-message-que) (concat "realgud-dap-message-que: " (pp-to-string realgud-dap-message-que)))
    ) )

(note "send message")
(with-temp-buffer
  (setup-fake-cmdbuf)
  (realgud--dap-process-send-message (realgud--dap-make-request-InitializeRequestArguments))
  (let* ((ret (realgud--dap-parse-output (current-buffer))))
    (message (pp-to-string realgud-dap-message-que))
    ))

(end-tests)

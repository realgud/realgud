;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'load-relative)
(require-relative-list '("../common/regexp")
		       "realgud-")
(require-relative-list '("../common/buffer/command")
		       "realgud-buffer-")


(defvar realgud:dap-pat-hash (make-hash-table :test 'equal)
  "dummy hash entry for compatibility")
(setf (gethash "loc" realgud:dap-pat-hash) (make-realgud-loc-pat))
(setf (gethash "prompt" realgud:dap-pat-hash) (make-realgud-loc-pat))
(setf (gethash "dap" realgud-pat-hash) realgud:dap-pat-hash)


(defun realgud--dap-headers-make-assoc (headers-str)
  (let* ((splitted (split-string headers-str ": " 't "[ \f\t\n\r\v]+")))
    `((,(nth 0 splitted) . ,(nth 1 splitted))) ))

(defun realgud--dap-parse-output ()
  "Parse DAP message.  Return 't if any message was found."
  (goto-char (point-min))
  (when (search-forward "\r\n\r\n" nil 't)
    (let* ((headers-end-point (match-end 0))
	   (headers (realgud--dap-headers-make-assoc
		     (buffer-substring (point-min)
				       (match-beginning 0))))
	   (len (string-to-number
		 (cdr (assoc "Content-Length" headers))))
	   (inhibit-read-only 't) )
      (when (>= (- (point-max) headers-end-point) len)
	(let* ((json-array-type 'list)
               (json-object-type 'hash-table)
               (json-false nil)
	       (body (json-read-from-string
		      (buffer-substring headers-end-point
					(+ headers-end-point len))))
	       (new-message (list :headers headers :body body)))
	  ;;let-alist cant be used as it's not place form
	  ;; (let-alist realgud-dap-message-que
	  ;;   (with-mutex .mutex
	  ;;     (if .que
	  ;; 	  (push new-message (cdr (last .que)) )
	  ;; 	(setf .que (list new-message)) )
	  ;;     (condition-notify .notify-var) )
	  ;;   )
	  (with-mutex (alist-get 'mutex realgud-dap-message-que)
	    (if (alist-get 'que realgud-dap-message-que)
		(push new-message (cdr (last (alist-get 'que realgud-dap-message-que))))
	      (setf (alist-get 'que realgud-dap-message-que) (list new-message)) )
	    (condition-notify (alist-get 'notify-var realgud-dap-message-que)) ) )
	(delete-region (point-min) (+ headers-end-point len))
	't) )) )

(defun realgud--dap-process-filter (proc string)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
	    (inhibit-read-only 't))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))

      (while (realgud--dap-parse-output) ) ; (thread-yield) ??
      )))

(defun realgud--dap-start-client (server port)
  ;; Create buffer with leading space, so it's hidden from user and have no undo history.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Names.html
  (let* ((name (concat " *dap " server ":" (number-to-string port) "*"))
	 (netbuff (get-buffer-create name))
	 )
      (make-network-process
       :name name
       :buffer netbuff
       :filter 'realgud--dap-process-filter
       :host server
       :service port
       ; :filter-multibyte 't ??
       )))

(defun realgud--dap-event-loop nil
  (with-mutex (alist-get 'mutex realgud-dap-message-que)
    (condition-wait (alist-get 'notify-var realgud-dap-message-que))
    (let* ((msg (pop (alist-get 'que realgud-dap-message-que)))
	   (msg-body (plist-get :body msg)))
      )
    ))

(defun realgud--dap-start-message-loop nil
  (with-current-buffer-safe (realgud-get-cmdbuf)
    ; TODO register thread in cmdbuf so we can kill it
    (make-thread (lambda nil (while 't (realgud--dap-message-handle)))
		 "realgud message handler")
    ))

(defgroup realgud:dap nil
  "The realgud interface to DAP debuggers."
  :group 'realgud
  :version "25.13") ; TODO which version??

(defcustom realgud:dap-command-name
  nil
  "File name for executing the DAP debugger and command options.
This should be an executable on your path, or an absolute file name.
You should set this variable in your project's directory variables"
  :type 'string
  :group 'realgud:dap
  :safe 'stringp)

(defvar realgud:dap-minibuffer-history nil
  "minibuffer history list for the DAP debuggers.")

(easy-mmode-defmap dap-minibuffer-local-map
  nil
  "Keymap for minibuffer prompting of debugger startup command."
  :inherit minibuffer-local-map)

(defvar dap-track-mode-map
  (make-sparse-keymap) ; TODO
  )

(defun dap-suggest-invocation (debugger-name)
  "Suggest a pdb command invocation via `realgud-suggest-invocaton'"
  "debugpy")

(defun dap-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'dap-suggest-invocation
   dap-minibuffer-local-map
   'realgud:dap-minibuffer-history
   opt-debugger))

(defun dap-track-mode-hook()
  (if dap-track-mode
      (progn
        (use-local-map dap-track-mode-map)
        (message "using dap mode map")
        )
    (message "dap track-mode-hook disable called")
    )
)

(define-minor-mode dap-track-mode
  "Minor mode for tracking pdb source locations inside a process shell via realgud. pdb is the stock Python debugger.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

a process shell.

\\{pdb-track-mode-map}
"
  :init-value nil
  :global nil
  :group 'realgud:dap
  :keymap dap-track-mode-map
  (realgud:track-set-debugger "dap")
  (if dap-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (dap-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
  )

(defun dap-parse-cmd-args (args)
    ;(list interpreter-args debugger-args script-args annotate-p)
    (list args nil nil nil))

;;;###autoload
(defun realgud:dap (&optional opt-cmd-line no-reset)
  "Invoke the DAP debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run pdb. You will be prompted
for a command line is one isn't supplied.
OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `pdb-parse-cmd-args' and path elements found by that
are expanded using `realgud:expand-file-name-if-exists'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
					; TODO doc
  (interactive)
  (realgud:run-debugger "dap" 'dap-query-cmdline
			'dap-parse-cmd-args
			'realgud:dap-minibuffer-history
			opt-cmd-line no-reset) )

(provide-me "realgud:dap-")

;;; dap.el ends here

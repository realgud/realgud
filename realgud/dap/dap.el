;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'load-relative)
(require-relative-list '("../common/regexp")
		       "realgud-")
(require-relative-list '("../common/buffer/command")
		       "realgud-buffer-")
(require-relative-list '("../debugger/dap/dap")
		       "realgud-")

(defvar realgud--dap--debug-msg-buf-to-message 't
  "For debugging proposes.  Print message buffer to messages buffer.")

(defun realgud--dap-make-type-Source (path)
  "Build source object for file under PATH.
https://microsoft.github.io/debug-adapter-protocol/specification#Types_Source"
  `(:path ,path :adapterData nil :checksums ('TODO) :origin ""
	  :presentationHint "normal" :sourceReference 0
	  :sources nil)
  )

;; TODO; rest of fields.
;; documentation says sth about assuming utf16, so i probably need temporary change, hopefully folliowing snippet enough. also i must disable feature columnsStartAt1
;; (let ((old-buffer-file-coding-system buffer-file-coding-system))
;;   (unwind-protect
;;       (progn
;; 	(set-buffer-file-coding-system 'utf-16 nil 't)
;; 	(current-column))
;;       (setq buffer-file-coding-system old-buffer-file-coding-system) ))

(defun realgud--dap-make-type-SourceBreakpoint (line)
  "Build SourceBreakpoint object pointing at LINE.
https://microsoft.github.io/debug-adapter-protocol/specification#Types_SourceBreakpoint"
  `(:line ,line)
  )

(defun realgud--dap-make-request-SetBreakpointsRequest (path line)
  "Build SetBreakpointsRequest payload.
Add breakpoint for source referenced by PATH at LINE.
https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-setbreakpoints-request"
  `(:command "setBreakpoints" :type "request" :arguments
	     (:breakpoints
		,(realgud--dap-make-type-SourceBreakpoint line)
		:source
		,(realgud--dap-make-type-Source path))
	     ))

(defun realgud--dap-make-request-InitializeRequestArguments nil
  `(:command "initialize" :type "request" :arguments
	     (
	      :adapterID "idk" ;; TODO
			 :linesStartAt1 t :columnsStartAt1 nil
			 :pathFormat "path" ;; TODO probably should be URI GNU/Linux->M$ windows connection
			 :supportsRunInTerminalRequest nil
			 :supportsArgsCanBeInterpretedByShell nil
			 :supportsVariablePaging nil
			 :supportsProgressReporting nil
			 :supportsInvalidatedEvent nil
			 :supportsMemoryEvent nil ;; TODO?
			 :supportsVariableType nil ;; TODO?
			 :supportsMemoryReferences nil ;; TODO?)
	     )))

(defun realgud--dap-headers-make-assoc (headers-str)
  (let* ((splitted (split-string headers-str ": " 't "[ \n\r]+")))
    `((,(nth 0 splitted) . ,(nth 1 splitted))) ))

(defun realgud--dap-parse-output (cmdbuff)
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
	  (with-current-buffer cmdbuff
	    (with-mutex realgud-dap-mutex
	      (if realgud-dap-message-que
		  (push new-message (cdr (last realgud-dap-message-que)))
		(setf realgud-dap-message-que (list new-message)) )
	      (condition-notify realgud-dap-notify-var) )) )
	(when realgud--dap--debug-msg-buf-to-message
	  (message (concat "===== DAP MESSAGE BEGIN =======\n"
			   (buffer-substring (point-min) (+ headers-end-point len))
			   (unless (equal (+ headers-end-point len) (point-max))
			     (concat "\n===== DAP MESSAGE TRAILING ====\n"
				     (buffer-substring (+ headers-end-point len) (point-max))))
			   "\n=== DAP MESSAGE END ===========\n"
			   )
		   ))
	(delete-region (point-min) (+ headers-end-point len))
	't) )) )

(defun realgud--dap-process-send-message (message-plist)
  "Serial plist MESSAGE-PLIST ans send to DAP server.
Add `seq' to message and increment it."
  ; TODO add new mutex? ; TODO run in loop in separated thread!!!
  (with-current-buffer (realgud-get-cmdbuf)
    (let* ((proc (realgud-sget 'cmdbuf-info 'dap-network-process))
	   (seq nil)
	   (message-str nil)
	   (len nil)
	   (json-false nil))
      (setq seq (realgud-sget 'cmdbuf-info 'dap-seq))
      (realgud-cmdbuf-info-dap-seq= (1+ seq))
      (setq message-str
	    (json-encode-plist (append message-plist `(:seq ,seq))))
      (setq len (string-bytes message-str))
      (when realgud--dap--debug-msg-buf-to-message
	(message (concat "===== DAP SEND BEGIN =======\n"
			 (concat "Content-Length: " (int-to-string len) "\r\n\r\n"
				 message-str)
			 "\n=== DAP MESSAGE END ===========\n"
			 )
		 ))
      (unless proc (error "dap-network-process is nil!!!"))
      (process-send-string proc
       (concat "Content-Length: " (int-to-string len) "\r\n\r\n"
	       message-str))
      )))

(defun realgud--dap-process-filter (proc string cmdbuff)
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

      (while (realgud--dap-parse-output cmdbuff) ) ; (thread-yield) ??
      )))

(defun realgud--dap-start-client (server port)
  ;; Create buffer with leading space, so it's hidden from user and have no undo history.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Names.html
  (let* (
	 (name
	  (concat " *dap " server ":" (number-to-string port) "*"))
	 (netbuff (get-buffer-create name))
	 (cmdbuff (realgud-get-cmdbuf))
	 )
    (with-current-buffer netbuff
      (delete-region (point-min) (point-max))) ;; Cleanup after previous connections
    ;; TODO make sure buffer is deleted once process is dead and kill process
    (with-current-buffer cmdbuff
      (realgud-cmdbuf-info-dap-msg-loop-thread=
       (make-thread (lambda nil (while 't (realgud--dap-event-handler cmdbuff)))
		    "realgud-message-handler"))
      (realgud-cmdbuf-info-dap-network-process=
       (make-network-process
	:name name
	:buffer netbuff
	:filter (lambda (proc string) (realgud--dap-process-filter proc string cmdbuff))
	:host server
	:service port
	;; :filter-multibyte 't ??
	)))))

(defun realgud--dap-event-handler (cmdbuf) ;; Catch and report errors
  (with-current-buffer cmdbuf
    (with-mutex realgud-dap-mutex
      (condition-wait realgud-dap-notify-var)
      (let* ((msg (pop realgud-dap-message-que))
	     (msg-body (plist-get msg :body))
	     (msg-type (gethash "type" msg-body)))
	(cond
	 ((string= msg-type "breakpoint") nil)
	 ('t (message (concat "Unsupported DAP message: " msg-type)))
	 )
	))))

(defun realgud--dap-cleanup nil
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (let ((proc (realgud-sget 'cmdbuf-info 'dap-network-process))
	  (thread (realgud-sget 'cmdbuf-info 'dap-msg-loop-thread)))
      (when (threadp thread)
	(thread-signal thread 'quit nil))
      (when (processp proc)
	(delete-process proc)))
    ;; kill buffer (process-contact proc :buffer)
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
  (concat "python -m debugpy --wait-for-client --log-to-stderr --listen 10000 /srv/git/youtube-dl/youtube-dl https://www.youtube.com/watch?v=OxnicJ-jPWo"))

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
  :group 'realgud:
  :keymap dap-track-mode-map
  (realgud:track-set-debugger "dap")
  (if dap-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (dap-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
  (set (make-local-variable 'realgud-command-name-hash) (make-hash-table :test 'equal))
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
			opt-cmd-line no-reset)
  (sit-for 2) ;; TODO smart wait until server is ready
  (realgud--dap-start-client "127.0.0.1" 10000)
  (realgud--dap-process-send-message (realgud--dap-make-request-InitializeRequestArguments))
  )

(provide-me "realgud:dap-")

;;; dap.el ends here

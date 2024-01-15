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
  `(:path ,path :presentationHint "normal" :sourceReference 0)
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
  (list :line line)
  )

(defun realgud--dap-make-request-SetBreakpointsRequest (path brk-points)
  "Build SetBreakpointsRequest payload.
Add breakpoint for source referenced by PATH at LINE.
https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-setbreakpoints-request"
  `(:command "setBreakpoints" :type "request" :arguments
	     (:breakpoints
	      ,(map 'vector 'identity brk-points) ;; Just fancy way to convert list to vector
	      :source
	      ,(realgud--dap-make-type-Source path))) )

(defun realgud--dap-make-request-InitializeRequestArguments nil
  (list :command "initialize" :type "request" :arguments
	(list
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

(defun realgud--dap-cmd-break (arg)
  (let* ((cmdbuf (realgud-get-cmdbuf))
	 (src-path (realgud-expand-format "%X"))
	 (new-brk-line (string-to-number (realgud-expand-format "%l")))
	 (brk-points)
	 )
    (with-current-buffer cmdbuf
      (setq brk-points
	    (append (gethash src-path (realgud-get-info 'dap-breakpoints) nil)
		    (list (realgud--dap-make-type-SourceBreakpoint new-brk-line))
		    )))
    (with-current-buffer cmdbuf
      (puthash src-path brk-points (realgud-get-info 'dap-breakpoints)))

    (realgud--dap-process-send-message
     (realgud--dap-make-request-SetBreakpointsRequest
      src-path
      brk-points)))
  )

(defun realgud--dap-cmd-step (&optional arg)
  (realgud--dap-process-send-message (realgud--dap-make-request-StepInRequest)))

(defun realgud--dap-cmd-next (&optional arg)
  (realgud--dap-process-send-message (realgud--dap-make-request-NextRequest)))

(defvar realgud--dap-debugger-state-dead 0)
(defvar realgud--dap-debugger-state-initialised 1)
(defvar realgud--dap-debugger-state-configuration-done 2)

(defun realgud--dap-handle-response-initialize (success body)
  ;; TODO. do something smart with capabilities
  (if success
      (setq realgud--dap-debugger-state-initialised
	    (realgud-sget 'cmdbuf-info 'dap-debugger-state)))
  )

(defun realgud--dap-cmd-restart nil
  (with-current-buffer (realgud-get-cmdbuf)
    (let* ((dbg-state (realgud-sget 'cmdbuf-info 'dap-debugger-state)))
      (cond
       ((eql dbg-state realgud--dap-debugger-state-dead)
	(message "Debugger is dead bro")) ;; TODO restart it then.
       ((eql dbg-state realgud--dap-debugger-state-initialised)
	(realgud--dap-process-send-message (realgud--dap-make-request-configurationDone)))
       ((eql dbg-state realgud--dap-debugger-state-configuration-done)
	(message "ur done"))
       ))))


(defun realgud--dap-cmd-variables nil
  (with-current-buffer (realgud-get-cmdbuf)
      (let ((request_seq nil))
	(setq request_seq
	      (realgud--dap-process-send-message
	       (realgud--dap-make-request-StackTraceRequest nil)))
	(puthash request_seq :variables realgud-dap-handler-response-hash)
	)))


;; {"seq": 16, "type": "response", "request_seq": 9, "success": true, "command": "setBreakpoints",
;; "body": {"breakpoints":
;; [
;;  {"verified": true, "id": 15, "source": {"path": "/srv/git/youtube-dl/yt_dlp/__main__.py", "sourceReference": 0, "presentationHint": "normal"}, "line": 8},
;;   {"verified": true, "id": 16, "source": {"path": "/srv/git/youtube-dl/yt_dlp/__main__.py", "sourceReference": 0, "presentationHint": "normal"}, "line": 17},
;;   {"verified": true, "id": 17, "source": {"path": "/srv/git/youtube-dl/yt_dlp/__main__.py", "sourceReference": 0, "presentationHint": "normal"}, "line": 14},
;;   {"verified": true, "id": 18, "source": {"path": "/srv/git/youtube-dl/yt_dlp/__main__.py", "sourceReference": 0, "presentationHint": "normal"}, "line": 12},
;;   {"verified": true, "id": 19, "source": {"path": "/srv/git/youtube-dl/yt_dlp/__main__.py", "sourceReference": 0, "presentationHint": "normal"}, "line": 17},
;;   {"verified": true, "id": 20, "source": {"path": "/srv/git/youtube-dl/yt_dlp/__main__.py", "sourceReference": 0, "presentationHint": "normal"}, "line": 11}]}}

(if (require 'yaml nil 'noerror)
    (defun realgud--pp-hash (hash) (yaml-encode hash))
  (defun realgud--pp-hash (hash) (pp-to-string hash)) )

(defun realgud--dap--debug-msg-current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  ;; https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
  (let* ((nowtime (current-time))
	 (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime)
	    (format ".%d]" now-ms))))

(defun realgud--dap--debug-msg (func-sym msg)
  (when realgud--dap--debug-msg-buf-to-message
    (let* ((time (realgud--dap--debug-msg-current-time-microseconds))
	   (func (pp-to-string func-sym))
	   (inhibit-read-only 't))
      (with-current-buffer (get-buffer-create "*dap debug log*")
	(end-of-buffer)
	(insert
	 (format-message "%s" (concat "\nXXXXXXXXXXX `" func "'@" time " XXXXXXXXXXXXXXX\n"
				      (if (stringp msg) msg (pp-to-string msg)) )))) )))

(defun realgud--dap-handle-response-setBreakpoints (body)
  (let* ((all-brk-points (realgud-get-info 'dap-breakpoints))
	 (brk-format "Breakpoint %s,   at %s:%s.\n")
	 (brk-points (gethash "breakpoints" body))
	 (brk-num 0) )
    ;; TODO probably need to cleanup all breakpiont and set em again
    (mapcar (lambda (brk-point)
	      (let* ((src (gethash "source" brk-point))
		     (path (gethash "path" src))
		     (line (gethash "line" brk-point))
		     (rg-fake-input
		      (format brk-format
			 (setq brk-num (1+ brk-num))
			 path line)))
		(realgud--dap--debug-msg "realgud--dap-handle-response-setBreakpoints"
					 (concat "passing fake input:\n" rg-fake-input))
		(realgud:track-add-breakpoint
		 rg-fake-input
		 nil; (point-marker) ;; Not used but rg wants it.
		 (current-buffer) )
		))
	    brk-points)
    (realgud--dap--debug-msg "realgud--dap-handle-response-setBreakpoints"
			     (concat "current brk-points in rg bp-list"
				     (realgud-sget 'cmdbuf-info 'bp-list)))
    ))

(defun realgud--dap-fake-divert-string-break nil
  (let* ((all-brk-points (realgud-get-info 'dap-breakpoints))
	 (brk-format "%d	breakpoint	keep y	0xTODO in TODO at %s:%d\n")
	 (brk-num 0) )

    (with-temp-buffer
      (maphash (lambda (src brk-points)
		 (mapcar (lambda (brk-point)
			   (insert
			    (format brk-format
				    (setq brk-num (1+ brk-num))
				    src
				    (plist-get brk-point :line)
				    )))
			 brk-points
			 ))
	       all-brk-points)
      (buffer-substring-no-properties (point-min) (point-max)) )
    ))

(defun realgud--dap-handle-response-event-telemetry (body)
  ;; TODO include it in realgud:cmdbuf-info-describe
  (let ((tele-data (realgud-sget 'cmdbuf-info 'dap-telemetry-data)))
    (if tele-data
	(realgud-cmdbuf-info-dap-telemetry-data=
	 (append tele-data body))
      (realgud-cmdbuf-info-dap-telemetry-data= (list body)) )))

(defun realgud--dap-handle-response-StackTrace (body handler-response)
  ""
  (let* ((stack-frames (gethash "stackFrames" body))
	 (first-stack-frame (car (mapcar 'identity stack-frames )))
	 (first-stack-frame-line-int (gethash "line" first-stack-frame))
	 (first-stack-frame-line (int-to-string first-stack-frame-line-int))
	 (first-stack-frame-source (gethash "source" first-stack-frame))
	 (first-stack-frame-source-path (gethash "path" first-stack-frame-source))
	 (fake-string
	  (format "%s:%s:0:beg:0x0000" first-stack-frame-source-path first-stack-frame-line))
	 )
    (realgud-cmdbuf-info-in-srcbuf?= 't) ; ??

    (if handler-response ;; Unconditional loc-track triggers refresh with starts infinite loop
	(let ((frameId (gethash "id" first-stack-frame)))
	(realgud--dap-process-send-message (realgud--dap-make-request-scopes-request frameId)))
      (realgud-track-loc-action
       (realgud-track-loc fake-string (point-marker)) (realgud-get-cmdbuf) nil 't) )
    ))

(defun realgud--dap-handle-resonse-scopes (body handler-response)
  (let* ((scopes (gethash "scopes" body))
	 (local-scope nil)
	 (local-var-ref nil))
    (setq local-scope
	  (car
	   (mapcar (lambda (scope) (when (string= "Locals" (gethash "name" scope) ) scope))
		   scopes)))
    (setq local-var-ref (gethash "variablesReference" local-scope))
    (realgud--dap-process-send-message (realgud--dap-make-request-variables-request local-var-ref))
    ))

(defun realgud--dap-handle-event-stopped (body)
  "https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-setbreakpoints-request#arrow_left-stopped-event"
  (realgud--dap-process-send-message (realgud--dap-make-request-StackTraceRequest nil)) )

(defun realgud--dap-handle-event-module (body)
  "https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-setbreakpoints-request#arrow_left-stopped-event"
  (realgud--dap-process-send-message (realgud--dap-make-request-StackTraceRequest nil)) )

(defun realgud--dap-make-request-scopes-request (frameId)
  "https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-scopes-request"
  `(:command "scopes" :type "request" :arguments
	     (:frameId ,frameId)) )

(defun realgud--dap-make-request-variables-request (variablesReference)
  "https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-scopes-request"
  `(:command "variables" :type "request" :arguments
	     (:variablesReference ,variablesReference)) )

(defun realgud--dap-make-request-StepInRequest ()
  `(:command "stepIn" :type "request" :arguments
	     (:threadId 1)))

(defun realgud--dap-make-request-NextRequest ()
  `(:command "next" :type "request" :arguments
	     (:threadId 1)))

(defun realgud--dap-make-request-configurationDone nil
  ;; https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-configurationdone-request
  `(:command "configurationDone" :type "request")
  )

(defun realgud--dap-make-request-LaunchRequest nil
  ;; https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-launch-request
  `(:command "launch" :type "request" :arguments
	     (:noDebug nil))
  )

(defun realgud--dap-make-request-AttachRequest nil
  ;; https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-attach-request
  `(:command "attach" :type "request" :arguments
	     (:restart nil)))

(defun realgud--dap-make-request-StackTraceRequest (threadId)
  ;; https://microsoft.github.io/debug-adapter-protocol/specification#leftwards_arrow_with_hook-attach-request
  `(:command "stackTrace" :type "request" :arguments
	     (:threadId 1)) )

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
	(let* ((json-array-type 'vector)
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
	(realgud--dap--debug-msg "realgud--dap-parse-output"
	 (concat "<<<<< DAP MESSAGE BEGIN <<<<<<<\n"
		 (buffer-substring (point-min) (+ headers-end-point len))
		 (unless (equal (+ headers-end-point len) (point-max))
		   (concat "\n+++++ DAP MESSAGE TRAILING ++++\n"
			   (buffer-substring (+ headers-end-point len) (point-max))))
		 "\n<<< DAP MESSAGE END <<<<<<<<<<<\n"
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
      (realgud--dap--debug-msg "realgud--dap-process-send-message"
       (concat ">>>>> DAP SEND BEGIN >>>>>>>\\n"
	       (concat "Content-Length: " (int-to-string len) "\r\n\r\n"
		       message-str)
	       "\\n>>> DAP MESSAGE END >>>>>>>>>>>\\n"
	       ))
      (unless proc (error "dap-network-process is nil!!!"))
      (process-send-string proc
       (concat "Content-Length: " (int-to-string len) "\r\n\r\n"
	       message-str))
      seq)))

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
       (make-thread (lambda nil
		      (while 't
			(condition-case-unless-debug error
			    (realgud--dap-event-handler cmdbuff)
			  ('quit (cl-return))
			  ('t (warn
			       (concat "Unhandled DAP error: " (pp-to-string error)))) )))
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

(defun realgud--dap-event-handler (cmdbuf) ;;  report errors
  (with-current-buffer cmdbuf
    (with-mutex realgud-dap-mutex
      (condition-wait realgud-dap-notify-var)
      (while realgud-dap-message-que
	(let* ((msg (pop realgud-dap-message-que))
	       (dap-message
		(or (plist-get msg :body) (make-hash-table))) ;; TODO rename :body to :raw-body or something
	       (msg-type (gethash "type" dap-message ""))
	       (msg-body (gethash "body" dap-message (make-hash-table)))
	       (command "")
	       (category "")
	       (event "")
	       (success nil)
	       (request_seq 0)
	       (handler-response nil))
	  (cond
	   ((string= msg-type "response")
	    (setq command (gethash "command" dap-message)
		  success (gethash "success" dap-message)
		  request_seq (gethash "request_seq" dap-message)
		  ))
	   ((string= msg-type "event")
	    (setq category (gethash "category" msg-body "")
		  event (gethash "event" dap-message ""))) )

	  (realgud--dap--debug-msg
	   "realgud--dap-event-handler"
	   (concat "handling message: " msg-type "\n" (realgud--pp-hash msg) ))

	  (setq handler-response
		(gethash request_seq realgud-dap-handler-response-hash nil))

	  (cond
	   ;; Response to initialize request. Get capabilities.
	   ((string= command "initialize")
	    (realgud--dap-handle-response-initialize success dap-message))
	   ;; Resoponse to setBreakpoints
	   ((and (string= msg-type "response") (string= command "setBreakpoints"))
	    (realgud--dap-handle-response-setBreakpoints msg-body))
	   ;; Telemetry
	   ((and (string= msg-type "event") (string= category "telemetry"))
	    (realgud--dap-handle-response-event-telemetry msg-body))
	   ;; Backtrace
	   ((and (string= msg-type "response") (string= command "stackTrace"))
	    (realgud--dap-handle-response-StackTrace msg-body handler-response))
	   ;; stopped event
	   ((and (string= msg-type "event") (string= event "stopped"))
	    (realgud--dap-handle-event-stopped msg-body))
	   ;; module
	   ((and (string= msg-type "event") (string= event "module"))
	    (realgud--dap-handle-event-module msg-body))
	   ;; Scope
	   ((and (string= msg-type "response") (string= command "scopes"))
	    (realgud--dap-handle-resonse-scopes msg-body handler-response))
	   ;; Variables
	   ((and (string= msg-type "response") (string= command "variables"))
	    (realgud--dap-handle-response-variables msg-body))
	   ;; Not implemented
	   ('t (realgud--dap--debug-msg
		"realgud--dap-event-handler"
		(concat "Unsupported DAP message: " msg-type ";" command category))) )
	  )))))

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
  (concat "python -m debugpy --wait-for-client --log-to-stderr --listen 10000 /srv/git/youtube-dl/yt_dlp/__main__.py https://www.youtube.com/watch?v=OxnicJ-jPWo"))

(defun dap-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'dap-suggest-invocation
   dap-minibuffer-local-map
   'realgud:dap-minibuffer-history
   opt-debugger))

(realgud-track-mode-vars "dap")
(defun dap-track-mode-hook()
  (if dap-track-mode
      (progn
        (use-local-map dap-track-mode-map)
        (message "using dap mode map") ;; TODO
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
  ;; Initialisaton sequence is in https://microsoft.github.io/debug-adapter-protocol/specification#arrow_left-initialized-event, TODO configurationDone
  (realgud:run-debugger "dap" 'dap-query-cmdline
			'dap-parse-cmd-args
			'realgud:dap-minibuffer-history
			opt-cmd-line no-reset)
  (sit-for 2) ;; TODO smart wait until server is ready
  (realgud--dap-start-client "127.0.0.1" 10000)
  (realgud--dap-process-send-message (realgud--dap-make-request-InitializeRequestArguments))
  (sit-for 2) ;; TODO smart wait until server is ready
  (realgud--dap-process-send-message (realgud--dap-make-request-AttachRequest))
  (sit-for 2) ;; TODO smart wait until server is ready
  )

(provide-me "realgud:dap-")

;;; dap.el ends here

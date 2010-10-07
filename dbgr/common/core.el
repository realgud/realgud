; (require 'term)
(if (< emacs-major-version 23)
    (error
     "You need at least Emacs 23 or greater to run this - you have version %d"
     emacs-major-version))

(require 'comint)
(require 'load-relative)
(require-relative-list
 '("fringe" "helper" "cmdbuf" "reset" "srcbuf") "dbgr-")

(declare-function dbgr-short-key-mode &optional arg)

(defvar dbgr-srcbuf-info)

(defun dbgr-suggest-invocation 
  (debugger-name minibuffer-history suggest-file-fn)
  "Suggest a debugger command invocation. If the current buffer
is a source file or process buffer previously set, then use the
value of that the command invocations found by buffer-local
variables. Next, try to use the first value of MINIBUFFER-HISTORY
if that exists.  Finally we try to find a suitable program file
using SUGGEST-FILE-FN."
  (let* ((buf (current-buffer))
	 (cmd-str-cmdbuf (dbgr-cmdbuf-command-string buf))
	 (cmd-str-srcbuf (dbgr-srcbuf-command-string buf))
	 )
    (cond
     ((and cmd-str-cmdbuf (equal debugger-name (dbgr-cmdbuf-debugger-name buf)))
      cmd-str-cmdbuf)
     ((and cmd-str-srcbuf (equal debugger-name (dbgr-srcbuf-debugger-name buf)))
      cmd-str-srcbuf)
     ((and minibuffer-history (listp minibuffer-history)) 
      (car minibuffer-history))
     (t (concat debugger-name " " (funcall suggest-file-fn))))))

(defun dbgr-query-cmdline 
  (suggest-invocation-fn 
   minibuffer-local-map
   minibuffer-history 
   &optional opt-debugger)
  "Prompt for a debugger command invocation to run.
Analogous to `gud-query-cmdline'. 

If you happen to be in a debugger process buffer, the last command invocation
for that first one suggested. Failing that, some amount of guessing is done
to find a suitable file via SUGGEST-INVOCATION-FN.

We also set filename completion and use a history of the prior rbdbgr
invocations "
  (let ((debugger (or opt-debugger
		   (dbgr-sget 'srcbuf-info 'debugger-name))))
    (read-from-minibuffer
     (format "Run %s (like this): " debugger)  ;; prompt string
     (funcall suggest-invocation-fn debugger)  ;; initial value
     minibuffer-local-map                      ;; keymap
     nil                                       ;; read - use default value
     minibuffer-history                        ;; history variable
     )))

(defun dbgr-parse-command-arg (args two-args opt-two-args)
  "Return a cons node where the car is a list containing the
entire first option and the cdr is the remaining arguments from ARGS.

We determine if an option has length one or two using the lists
TWO-ARGS and OPT-TWO-ARGS. Both of these are list of 'options',
that is strings without the leading dash. TWO-ARGS takes a
mandatory additional argument. OPT-TWO-ARGS might take two
arguments. The rule for an optional argument that we use is if
the next parameter starts with a dash ('-'), it is not part of
the preceeding parameter when that parameter is optional.

NOTE: we don't check whether the first arguments of ARGS is an
option by testing to see if it starts say with a dash. So on
return the first argument is always removed.
"
  (let ((arg (car args))
	(d-two-args (mapcar (lambda(x) (concat "-" x)) two-args))
	(d-opt-two-args (mapcar (lambda(x) (concat "-" x)) opt-two-args))
	(remaining (cdr args)))
    (cond 
     ((member arg d-two-args)
      (if (not remaining)
	    (progn 
	      (message "Expecting an argument after %s. Continuing anyway."
		       arg)
	      (cons (list arg) (list remaining)))
	(cons (list arg (car remaining)) (list (cdr remaining)))))
     ((member arg d-opt-two-args)
      (if (and remaining (not (string-match "^-" (car remaining))))
	  (cons (list arg (car remaining)) (list (cdr remaining)))
	(cons (list arg) (list remaining))))
     (t (cons (list arg) (list remaining))))))

(defun dbgr-terminate (&optional buf)
  "Resets state in all buffers associated with source or command buffer BUF)
This does things like remove fringe arrows breakpoint icons and
resets short-key mode."
  (interactive "bbuffer: ")
  (let ((cmdbuf (dbgr-get-cmdbuf buf)))
    (if cmdbuf
	(with-current-buffer cmdbuf
	  (dbgr-fringe-erase-history-arrows)
	  (if dbgr-cmdbuf-info
	      (dolist (srcbuf (dbgr-cmdbuf-info-srcbuf-list dbgr-cmdbuf-info))
		(if (dbgr-srcbuf? srcbuf)
		    (with-current-buffer srcbuf
		      (dbgr-fringe-erase-history-arrows)
		  (dbgr-short-key-mode 0)
		  (dbgr-bp-remove-icons (point-min) (point-max))
		  )))
	    )
	  )
      (error "Buffer %s does not seem to be attached to a debugger" 
	     (buffer-name))
      )
    )
  )

(defun dbgr-term-sentinel (process string)
  "Called when PROCESS dies. We call `dbgr-quit' to clean up."
  (let ((cmdbuf (dbgr-get-cmdbuf)))
    (if cmdbuf (dbgr-terminate cmdbuf)))
  (message "That's all folks.... %s" string))

(defun dbgr-exec-shell (debugger-name script-filename program 
				      &optional no-reset &rest args)
  "Run the specified SCRIPT-FILENAME in under debugger DEBUGGER-NAME a
comint process buffer. ARGS are the arguments passed to the
PROGRAM.  At the moment, no piping of input is allowed.

SCRIPT-FILENAME will have local variable `dbgr-script-info' set
which contains the debugger name and debugger process-command
buffer.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."

  (let* ((starting-directory
	  (or (file-name-directory script-filename)
	      default-directory "./"))
	 (cmdproc-buffer
	  (get-buffer-create
	   (format "*%s %s shell*" 
		   (file-name-nondirectory debugger-name)
		   (file-name-nondirectory script-filename))))
	 (dbgr-buf (current-buffer))
	 (process (get-buffer-process cmdproc-buffer)))
    (unless (and process (eq 'run (process-status process)))
      (with-current-buffer cmdproc-buffer
	(and (dbgr-cmdbuf?) (not no-reset) (dbgr-reset))
	(setq default-directory default-directory)
	(insert "Current directory is " default-directory "\n")
	
	;; For term.el
	;; (term-mode)
	;; (set (make-local-variable 'term-term-name) dbgr-term-name)
	;; (make-local-variable 'dbgr-parent-buffer)
	;; (setq dbgr-parent-buffer dbgr-buf)
	
	;; For comint.el. 
	(comint-mode)

	;; Making overlay-arrow-variable-list buffer local has to be
	;; done after running commint mode. FIXME: find out why and if
	;; this reason is justifyable. Also consider moving this somewhere
	;; else.
	(make-local-variable 'overlay-arrow-variable-list)
	(make-local-variable 'dbgr-overlay-arrow1)
	(make-local-variable 'dbgr-overlay-arrow2)
	(make-local-variable 'dbgr-overlay-arrow3)

	(comint-exec cmdproc-buffer debugger-name program nil args)
	
	(setq process (get-buffer-process cmdproc-buffer))

	(if (and process (eq 'run (process-status process)))
	    (let ((src-buffer (find-file-noselect script-filename))
		  (cmdline-list (cons program args)))
	      ;; is this right? 
	      (point-max)
	      (dbgr-srcbuf-init src-buffer cmdproc-buffer 
				debugger-name cmdline-list))
	  (insert (format "Failed to invoke shell command: %s %s" program args)))
	(process-put process 'buffer cmdproc-buffer)))
    cmdproc-buffer))

;; Start of a term-output-filter for term.el
(defun dbgr-term-output-filter (process string)
  (let ((process-buffer (process-get process 'buffer)))
    (if process-buffer
	(save-current-buffer
	  (set-buffer process-buffer)
	  ;; (insert-before-markers (format "+++1 %s" string))
	  (insert-before-markers string)))))

(provide-me "dbgr-")

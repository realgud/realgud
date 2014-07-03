;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
; (require 'term)

(require 'list-utils)
(require 'load-relative)
(require-relative-list '("core") "realgud-")
(require-relative-list '("buffer/command") "realgud-buffer-")

(declare-function realgud-cmdbuf-info-in-debugger?=   'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-cmd-args=       'realgud-buffer-command)

(defun realgud:run-process(debugger-name script-filename cmd-args
				      track-mode-hook &optional no-reset)
  "Runs `realgud-exec-shell' with DEBUGGER-NAME SCRIPT-FILENAME
and CMD-ARGS If this succeeds, we call TRACK-MODE-HOOK and save
CMD-ARGS in command-buffer for use if we want to restart.  If
we don't succeed in running the program, we will switch to the
command buffer which shows details of the error. The command
buffer or nil is returned"

  (let ((cmd-buf))
    (condition-case nil
	(setq cmd-buf
	      (apply 'realgud-exec-shell debugger-name script-filename
		     (car cmd-args) no-reset (cdr cmd-args)))
      (error nil))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case?
    (let ((process (get-buffer-process cmd-buf)))
      (if (and process (eq 'run (process-status process)))
	  (progn
	    (switch-to-buffer cmd-buf)
	    (funcall track-mode-hook)
	    (realgud-cmdbuf-info-in-debugger?= 't)
	    (realgud-cmdbuf-info-cmd-args= cmd-args)
	    )
	(progn
	  (if cmd-buf (switch-to-buffer cmd-buf))
	  (message "Error running command: %s %s" debugger-name script-filename)
	  )
	)
      )
    cmd-buf
    )
  )

(defun realgud:run-debugger (debugger-name query-cmdline-fn parse-cmd-args-fn
					   track-mode-hook
					   opt-command-line no-reset)
  "Invoke the a debugger and start the Emacs user interface.

String OPT-COMMAND-LINE specifies how to run DEBUGGER-NAME. You
will be prompted for a command line using QUERY-CMDLINE-FN is one
isn't supplied.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by PARSE-CMD-FN and path elements found by that
are expanded using `expand-file-name'.
"
  (let* ((cmd-str (or opt-command-line (funcall query-cmdline-fn debugger-name)))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (funcall parse-cmd-args-fn cmd-args))
	 (script-args (caddr parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
	  (list-utils-flatten (list (cadr parsed-args) (caddr parsed-args))))
	 )
    (realgud:run-process debugger-name script-name parsed-cmd-args
			 track-mode-hook no-reset)
    )
  )

(provide-me "realgud:")

;;; Copyright (C) 2013 Rocky Bernstein <rocky@gnu.org>
;;  `gub' Main interface to Go gub via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud-gub-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup gub nil
  "The Go SSA interpreter debugger: gub"
  :group 'processes
  :group 'dbgr
  :group 'make
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom gub-command-name
  "tortoise -run -interp=F"
  "File name for executing the Go SSA interpreter/debugger, gub, and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'gub)

(defun realgud-gub-fn (&optional opt-command-line no-reset)
  "See `realgud-gub' for details"

  (let* ((cmd-str (or opt-command-line (gub-query-cmdline "gub")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (gub-parse-cmd-args cmd-args))
	 (gub-program (car parsed-args))
	 (gub-args (cadr parsed-args))
	 (script-and-args (caddr parsed-args))
	 (cmd-buf))
    (realgud-run-process "gub" gub-program script-and-args
			 'gub-track-mode no-reset)

    ;; Parse the command line and pick out the script name.

    (condition-case nil
    	(setq cmd-buf
    	      (apply 'realgud-exec-shell "gub" script-and-args
    		     gub-program no-reset (cdr script-and-args)))
      (error nil))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case?
    (let ((process (get-buffer-process cmd-buf)))
      (if (and process (eq 'run (process-status process)))
    	  (progn
    	    (switch-to-buffer cmd-buf)
    	    (gub-track-mode 't)
    	    (realgud-cmdbuf-info-cmd-args= cmd-args)
    	    )
    	(message "Error running gub command"))
      )
    )
  )

;;;###autoload
(defun realgud-gub (&optional opt-command-line no-reset)
  "Invoke the Go SSA debugger, gub and start the Emacs user interface.

String COMMAND-LINE specifies how to run gub.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."


  (interactive)
  (realgud-gub-fn opt-command-line no-reset)
  )

(defalias 'gub 'realgud-gub)

(provide-me "realgud-")
;;; gub.el ends here

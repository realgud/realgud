;;  `dbgr-gdb' Main interface to pydbgr via Emacs
(require 'load-relative)
(require-relative-list '("../common/helper") "dbgr-")
(require-relative-list '("core" "track-mode") "dbgr-gdb-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup dbgr-gdb nil
  "The dbgr interface to gdb"
  :group 'processes
  :group 'tools
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom dbgr-gdb-command-name
  ;;"gdb --emacs 3"
  "gdb"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'dbgr-gdb)

(declare-function dbgr-gdb-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun dbgr-gdb (&optional opt-command-line no-reset)
  "Invoke the gdb Ruby debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run gdb. 

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  
  (interactive)
  (let* (
	 (cmd-str (or opt-command-line (dbgr-gdb-query-cmdline "gdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (dbgr-gdb-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (expand-file-name (car script-args)))
	 (cmd-buf))
  
    ;; Parse the command line and pick out the script name and whether
    ;; --annotate has been set.
  
    (condition-case nil
	(setq cmd-buf 
	      (apply 'dbgr-exec-shell "gdb" (car script-args)
		     (car cmd-args) nil
		     (cons script-name (cddr cmd-args))))
    (error nil))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case? 
    (let ((process (get-buffer-process cmd-buf)))
      (if (and process (eq 'run (process-status process)))
	  (progn
	    (switch-to-buffer cmd-buf)
	    (dbgr-gdb-track-mode 't)
	    (dbgr-cmdbuf-info-cmd-args= dbgr-cmdbuf-info cmd-args)
	    )
	(message "Error running gdb command"))
    )))

(provide-me "dbgr-")

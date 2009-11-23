;;  `pydbgr' Main interface to pydbgr via Emacs
(require 'load-relative)
(require-relative-list '("../helper") "dbgr-")
(require-relative-list '("pydbgr-core" "pydbgr-track-mode"))

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup pydbgr nil
  "The Python pydbgr debugger"
  :group 'processes
  :group 'tools)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom pydbgr-command-name
  ;;"pydbgr --emacs 3"
  "pydbgr"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'pydbgr)

(declare-function pydbgr-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun pydbgr (&optional opt-command-line)
  "Invoke the pydbgr Ruby debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run pydbgr."
  
  (interactive)
  (let* (
	 (cmd-str (or opt-command-line (pydbgr-query-cmdline "pydbgr")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (pydbgr-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
  
    ;; Parse the command line and pick out the script name and whether
    ;; --annotate has been set.
  
    (condition-case nil
	(setq cmd-buf 
	      (apply 'dbgr-exec-shell "pydbgr" script-name
		     (car cmd-args) (cdr cmd-args)))
    (error nil))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case? 
    (let ((proc (get-buffer-process cmd-buf)))
      (if (and proc (eq 'run (process-status proc)))
	  (progn
	    (switch-to-buffer cmd-buf)
	    (pydbgr-track-mode 't)
	    (dbgr-cmdbuf-info-cmd-args= dbgr-cmdbuf-info cmd-args)
	    )
	(message "Error running pydbgr command"))
    )))

(provide-me)

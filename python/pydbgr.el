;;  `pydbgr' Main interface to pydbgr via Emacs
(if (< emacs-major-version 22)
    (error
     "You need at least Emacs 22 or greater to run this - you have version %d"
     emacs-major-version))

(require 'load-relative)
(provide 'pydbgr)
(dolist 
    (rel-file 
     '("pydbgr-core" "pydbgr-track-mode"))
  (require-relative rel-file))

;; FIXME figure out if I can put this in something like a header file.
;; And we can then eliminate with what is in rbdbgr.el
(defvar dbgr-cmdbuf-info)
(declare-function dbgr-cmdbuf-info-cmd-args= (info cmd-args))
(declare-function dbgr-track-set-debugger (debugger-name))
(declare-function pydbgr-parse-cmd-args (args))
(declare-function pydbgr-query-cmdline (&optional debugger))

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
  
  ;; (gud-chdir-before-run nil))

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
	    (dbgr-track-set-debugger "pydbgr")

	    ;; FIXME: (pydbgr-track-mode 't) has problems
	    ;; until I figure out this out...
	    (setq pydbgr-track-mode 't)
	    (setq dbgr-track-mode 't)
	    (dbgr-track-mode-body)
	    (run-mode-hooks 'pydbgr-track-mode-hook)

	    (dbgr-cmdbuf-info-cmd-args= dbgr-cmdbuf-info cmd-args)
	    )
	(message "Error running pydbgr command"))
    )))

;;; pydbgr.el ends here


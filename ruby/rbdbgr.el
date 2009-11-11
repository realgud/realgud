;;  `rbdbgr' Main interface to rbdbgr via Emacs
(if (< emacs-major-version 22)
    (error
     "You need at least Emacs 22 or greater to run this - you have version %d"
     emacs-major-version))

(require 'load-relative)
(provide 'rbdbgr)
(load-relative '("rbdbgr-core" "rbdbgr-track-mode") 'rbdbgr)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup rbdbgr nil
  "The Ruby 1.9 debugger"
  :group 'processes
  :group 'tools)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom rbdbgr-command-name
  ;;"rbdbgr --emacs 3"
  "rbdbgr"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'rbdbgr)


;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun rbdbgr (&optional opt-command-line)
  "Invoke the rbdbgr Ruby debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run rbdbgr."
  
  (interactive)
  (let* (
	 (cmd-str (or opt-command-line (rbdbgr-query-cmdline "rbdbgr")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (rbdbgr-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (proc-buf))
  
  ;; Parse the command line and pick out the script name and whether
  ;; --annotate has been set.
  
  ;; (gud-chdir-before-run nil))

    (condition-case nil
	(setq proc-buf 
	      (apply 'dbgr-exec-shell "rbdbgr" script-name
		     (car cmd-args) (cdr cmd-args)))
    (error nil))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case? 
    (let ((proc (get-buffer-process proc-buf)))
      (if (and proc (eq 'run (process-status proc)))
	  (progn
	    (switch-to-buffer proc-buf)
	    ;; (set-process-filter (get-buffer-process proc-buf)
	    ;; 			'dbgr-term-output-filter)
	    (rbdbgr-track-mode 't)
	    (set (make-local-variable 'dbgr-invocation) cmd-args)
	    )
	(message "Error running rbdbgr command"))
    )))


(provide 'rbdbgr)

;;; rbdbgr.el ends here


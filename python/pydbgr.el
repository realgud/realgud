;;  `pydbgr' Main interface to pydbgr via Emacs
(if (< emacs-major-version 22)
    (error
     "You need at least Emacs 22 or greater to run this - you have version %d"
     emacs-major-version))

(defun pydbgr-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'pydbgr))))
    (if file-name
        (file-name-directory file-name)
      nil)))

(setq load-path (cons nil 
		      (cons (format "%s.." (pydbgr-directory))
				    (cons (pydbgr-directory) load-path))))

(load "pydbgr-core")
(load "pydbgr-track-mode")
(setq load-path (cdddr load-path))

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
	 (proc-buf))
  
  ;; Parse the command line and pick out the script name and whether
  ;; --annotate has been set.
  
  ;; (gud-chdir-before-run nil))

    (condition-case nil
	(setq proc-buf 
	      (apply 'dbgr-exec-shell "pydbgr" script-name
		     (car cmd-args) (cdr cmd-args)))
    (error nil))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case? 
    (let ((proc (get-buffer-process proc-buf)))
      (if (and proc (eq 'run (process-status proc)))
	  (save-current-buffer
	    (switch-to-buffer proc-buf)
	    ;; (set-process-filter (get-buffer-process proc-buf)
	    ;; 			'dbgr-term-output-filter)
	    (shell-mode)
	    (pydbgr-track-mode 't)
	    )
	(message "Error running pydbgr command"))
    )))


(provide 'pydbgr)

;;; pydbgr.el ends here


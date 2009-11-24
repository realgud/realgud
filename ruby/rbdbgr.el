;;  `rbdbgr' Main interface to rbdbgr via Emacs
(require 'load-relative)
(require-relative-list '("../helper") "dbgr-")
(require-relative-list '("rbdbgr-core" "rbdbgr-track-mode"))
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

(declare-function rbdbgr-track-mode (bool))

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
	 (cmd-buf))
  
    ;; Parse the command line and pick out the script name and whether
    ;; --annotate has been set.
  
    (condition-case nil
	(setq cmd-buf 
	      (apply 'dbgr-exec-shell "rbdbgr" script-name
		     (car cmd-args) (cdr cmd-args)))
    (error nil))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case? 
    (let ((process (get-buffer-process cmd-buf)))
      (if (and process (eq 'run (process-status process)))
	  (progn
	    (switch-to-buffer cmd-buf)
	    (rbdbgr-track-mode 't)
	    (dbgr-cmdbuf-info-cmd-args= dbgr-cmdbuf-info cmd-args)
	    )
	(message "Error running rbdbgr command"))
    )))


(provide 'rbdbgr)

;;; rbdbgr.el ends here

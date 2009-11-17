;;  `rbdbgr' Main interface to rbdbgr via Emacs
(require 'load-relative)
(dolist 
    (rel-file 
     '("../dbgr-helper" "rbdbgr-core" "rbdbgr-regexp" "rbdbgr-track-mode"))
  (require-relative rel-file))

(defvar rbdbgr-pat-hash)

;; FIXME figure out if I can put this in something like a header file.
;; And we can then eliminate with what is in pydbgr.el
(defvar dbgr-cmdbuf-info)
(defvar dbgr-track-mode)
(defvar rbdbgr-track-mode)
(declare-function dbgr-cmdbuf-info-cmd-args= (info cmd-args))
(declare-function dbgr-track-set-debugger (debugger-name))
(declare-function rbdbgr-parse-cmd-args (args))
(declare-function rbdbgr-query-cmdline (&optional debugger))
(declare-function dbgr-track-mode-body ())

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
	 (cmd-buf))
  
  ;; Parse the command line and pick out the script name and whether
  ;; --annotate has been set.
  
  ;; (gud-chdir-before-run nil))

    (condition-case nil
	(setq cmd-buf 
	      (apply 'dbgr-exec-shell "rbdbgr" script-name
		     (car cmd-args) (cdr cmd-args)))
    (error nil))
    ;; FIXME: Is there probably is a way to remove the
    ;; below test and combine in condition-case? 
    (let ((proc (get-buffer-process cmd-buf)))
      (if (and proc (eq 'run (process-status proc)))
	  (progn
	    (switch-to-buffer cmd-buf)
	    (dbgr-track-set-debugger "rbdbgr" rbdbgr-pat-hash)

	    ;; FIXME: (pydbgr-track-mode 't) has problems
	    ;; until I figure out this out...
	    (setq rbdbgr-track-mode 't)
	    (setq dbgr-track-mode 't)
	    (dbgr-track-mode-body)
	    (run-mode-hooks 'pydbgr-track-mode-hook)
	    (dbgr-cmdbuf-info-cmd-args= dbgr-cmdbuf-info cmd-args)
	    )
	(message "Error running rbdbgr command"))
    )))


(provide 'rbdbgr)

;;; rbdbgr.el ends here

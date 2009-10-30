;;  `rbdbgr' Main interface to rbdbgr via Emacs
(if (< emacs-major-version 22)
    (error
     "You need at least Emacs 22 or greater to run this - you have version %d"
     emacs-major-version))

(defun rbdbgr-directory ()
  "The directory of this file, or nil."
  (let ((file-name (or load-file-name
                       (symbol-file 'rbdbgr))))
    (if file-name
        (file-name-directory file-name)
      nil)))

(setq load-path (cons nil 
		      (cons (format "%s.." (rbdbgr-directory))
				    (cons (rbdbgr-directory) load-path))))

(load "rbdbgr-core")
(load "rbdbgr-track-mode")
(setq load-path (cdddr load-path))

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
	 (command-args (cdr cmd-args))
	 (command (car command-args))
	 (proc-buf))
  
  ;; Parse the command line and pick out the script name and whether
  ;; --annotate has been set.
  
  ;; (gud-chdir-before-run nil))
  
  (setq proc-buf 
	(apply 'dbgr-exec-shell "rbdbgr" 
	       (car cmd-args) (cdr cmd-args)))
  (save-current-buffer
    (switch-to-buffer proc-buf)
    (rbdbgr-track-mode 't))))


(provide 'rbdbgr)

;;; rbdbgr.el ends here


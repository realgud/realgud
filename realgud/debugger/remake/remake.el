;;; Copyright (C) 2011, 2013-2014 Rocky Bernstein <rocky@gnu.org>
;;  `remake' Main interface to remake via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track")  "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:remake-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup remake nil
  "The GNU Make debugger: remake"
  :group 'processes
  :group 'realgud
  :group 'make
  :version "23.1")

(declare-function remake-query-cmdline  'realgud:remake-core)
(declare-function remake-parse-cmd-args 'realgud:remake-core)
(declare-function realgud-run-process   'realgud-core)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom remake-command-name
  ;;"remake --emacs 3"
  "remake"
  "File name for executing the GNU make debugger, remake, and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'remake)

;;;###autoload
(defun realgud:remake-fn (&optional opt-cmd-line no-reset)
  "See `realgud:remake' for details"

  (let* ((cmd-str (or opt-cmd-line (remake-query-cmdline "remake")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (remake-parse-cmd-args cmd-args))
	 (remake-program (car parsed-args))
	 (makefile-name (or (cadr parsed-args) "Makefile"))
	 (makefile-args (caddr parsed-args))
	 (cmd-buf))
    (realgud-run-process "remake" makefile-name
		      (cons remake-program makefile-args)
		      'remake-track-mode no-reset)

    ;; ;; Parse the command line and pick out the script name and whether
    ;; ;; --annotate has been set.

    ;; (condition-case nil
    ;; 	(setq cmd-buf
    ;; 	      (apply 'realgud-exec-shell "remake" makefile-name
    ;; 		     remake-program no-reset makefile-args))
    ;;   (error nil))
    ;; ;; FIXME: Is there probably is a way to remove the
    ;; ;; below test and combine in condition-case?
    ;; (let ((process (get-buffer-process cmd-buf)))
    ;;   (if (and process (eq 'run (process-status process)))
    ;; 	  (progn
    ;; 	    (switch-to-buffer cmd-buf)
    ;; 	    (remake-track-mode 't)
    ;; 	    (realgud-cmdbuf-info-cmd-args= cmd-args)
    ;; 	    )
    ;; 	(message "Error running remake command"))
    ;;   )
    )
  )

(defalias 'remake 'realgud:remake)

(provide-me "realgud-")
;;; remake.el ends here

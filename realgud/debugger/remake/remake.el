;;; Copyright (C) 2011, 2013-2015 Rocky Bernstein <rocky@gnu.org>
;;  `remake' Main interface to remake via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track")  "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:remake-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:remake nil
  "The realgud interface to the GNU Make debugger"
  :group 'realgud
  :group 'make
  :version "24.1")

(declare-function remake-query-cmdline  'realgud:remake-core)
(declare-function remake-parse-cmd-args 'realgud:remake-core)
(declare-function realgud:run-debugger  'realgud:run)
(declare-function realgud:run-process   'realgud:run)

(defun realgud:remake-run-debugger (&optional opt-command-line
				    no-reset)
  "Invoke the a debugger and start the Emacs user interface.

String OPT-COMMAND-LINE specifies how to run DEBUGGER-NAME. You
will be prompted for a command line using QUERY-CMDLINE-FN is one
isn't supplied.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by PARSE-CMD-FN and path elements found by that
are expanded using `expand-file-name'.

If successful, The command buffer of the debugger process is returned.
Otherwise nil is returned.
"
  (let* ((cmd-str (or opt-command-line (remake-query-cmdline "remake")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (remake-parse-cmd-args cmd-args))
	 (debugger (car parsed-args))
	 (script-args (caddr parsed-args))
	 (script-name (cadr parsed-args))
	 )
    (realgud:run-process debugger script-name cmd-args
			 realgud:remake-minibuffer-history no-reset)
    )
  )

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:remake-command-name
  ;;"remake --emacs 3"
  "remake"
  "File name for executing the GNU make debugger, remake, and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:remake)

;;;###autoload
(defun realgud:remake (&optional opt-cmd-line no-reset)
  "See `realgud:remake' for details"
  (interactive)
  (realgud:remake-run-debugger opt-cmd-line no-reset)
  )

(defalias 'remake 'realgud:remake)

(provide-me "realgud-")
;;; remake.el ends here

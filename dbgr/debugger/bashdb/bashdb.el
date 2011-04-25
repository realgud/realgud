;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;  `bashdb' Main interface to bashdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "dbgr-")
(require-relative-list '("../../common/track") "dbgr-")
(require-relative-list '("core" "track-mode") "dbgr-bashdb-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup bashdb nil
  "The bash debugger: bashdb (dbgr variant)"
  :group 'processes
  :group 'dbgr
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom bashdb-command-name
  ;;"bashdb --emacs 3"
  "bashdb"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'bashdb)

(declare-function bashdb-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun dbgr-bashdb (&optional opt-command-line no-reset)
  "Invoke the bashdb Z-shell debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run bashdb.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  (interactive)
  (let* ((cmd-str (or opt-command-line (bashdb-query-cmdline "bashdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (bashdb-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (dbgr-run-process "bashdb" script-name cmd-args 
		      'bashdb-track-mode no-reset)
    ))

(defalias 'bashdb 'dbgr-bashdb)

(provide-me "dbgr-")

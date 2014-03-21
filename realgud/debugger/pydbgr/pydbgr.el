;;; Copyright (C) 2010, 2011, 2012 Rocky Bernstein <rocky@gnu.org>
;;  `pydbgr' Main interface to pydbgr via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper"
			 "../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud-pydbgr-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup pydbgr nil
  "The Python pydbgr debugger"
  :group 'processes
  :group 'realgud
  :group 'python
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom pydbgr-command-name
  ;;"pydbgr --emacs 3"
  "pydbgr"
  "File name for executing the Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'pydbgr)

(declare-function pydbgr-track-mode (bool))
(declare-function pydbgr-query-cmdline  'realgud-pydbgr-core)
(declare-function pydbgr-parse-cmd-args 'realgud-pydbgr-core)
(declare-function realgud-run-process 'realgud-core)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud-pydbgr (&optional opt-command-line no-reset)
  "Invoke the pydbgr Python debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run pydbgr.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."


  (interactive)
  (let* (
	 (cmd-str (or opt-command-line (pydbgr-query-cmdline
					"pydbgr")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (pydbgr-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (realgud-run-process "pydbgr" script-name cmd-args
		      'pydbgr-track-mode no-reset)
    )
  )


(defalias 'pydbgr 'realgud-pydbgr)

(provide-me "realgud-")

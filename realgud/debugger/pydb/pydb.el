;;; Copyright (C) 2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `pydb' Main interface to pydb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper"
			 "../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud-pydb-")

(declare-function pydb-query-cmdline  'realgud-pydb-core)
(declare-function pydb-parse-cmd-args 'realgud-pydb-core)
(declare-function realgud-run-process 'realgud-core)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup pydb nil
  "The Python pydb debugger"
  :group 'processes
  :group 'realgud
  :group 'python
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom pydb-command-name
  "pydb"
  "File name for executing the stock Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'pydb)

(declare-function pydb-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud-pydb (&optional opt-command-line no-reset)
  "Invoke the pydb Python debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run pydb.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."


  (interactive)
  (let* (
	 (cmd-str (or opt-command-line (pydb-query-cmdline
					"pydb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (pydb-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (realgud-run-process "pydb" script-name cmd-args
		      'pydb-track-mode no-reset)
    )
  )


(defalias 'pydb 'realgud-pydb)

(provide-me "realgud-")

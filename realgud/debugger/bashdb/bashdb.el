;;; Copyright (C) 2011, 2013-2014 Rocky Bernstein <rocky@gnu.org>
;;  `bashdb' Main interface to bashdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:bashdb-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup bashdb nil
  "The bash debugger: bashdb (realgud variant)"
  :group 'processes
  :group 'realgud
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom bashdb-command-name
  ;;"bashdb --emacs 3"
  "bashdb"
  "File name for executing the bash debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'bashdb)

(declare-function bashdb-track-mode (bool))
(declare-function bashdb-query-cmdline  'realgud:bashdb-core)
(declare-function bashdb-parse-cmd-args 'realgud:bashdb-core)
(declare-function realgud-run-process 'realgud-core)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:bashdb (&optional opt-command-line no-reset)
  "Invoke the bashdb shell debugger and start the Emacs user interface.

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
    (realgud-run-process "bashdb" script-name cmd-args
		      'bashdb-track-mode no-reset)
    ))

(defalias 'bashdb 'realgud:bashdb)

(provide-me "realgud-")

;;; Copyright (C) 2010-2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `trepan2' Main interface to trepan2 via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper"
			 "../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:trepan2-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup trepan2 nil
  "The Python trepan2 debugger"
  :group 'processes
  :group 'realgud
  :group 'python
  :version "23.1")

(declare-function trepan2-query-cmdline  'realgud:trepan2-core)
(declare-function trepan2-parse-cmd-args 'realgud:trepan2-core)
(declare-function realgud-run-process 'realgud-core)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom trepan2-command-name
  ;;"trepan2 --emacs 3"
  "trepan2"
  "File name for executing the Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'trepan2)

(declare-function trepan2-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:trepan2 (&optional opt-command-line no-reset)
  "Invoke the trepan2 Python debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run trepan2.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."


  (interactive)
  (let* (
	 (cmd-str (or opt-command-line (trepan2-query-cmdline
					"trepan2")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (trepan2-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (realgud-run-process "trepan2" script-name cmd-args
		      'trepan2-track-mode no-reset)
    )
  )


(defalias 'trepan2 'realgud:trepan2)

(provide-me "realgud-")

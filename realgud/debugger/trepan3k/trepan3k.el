;;; Copyright (C) 2010-2014 Rocky Bernstein <rocky@gnu.org>
;;  `trepan3k' Main interface to trepan3k via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper"
			 "../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:trepan3k-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup trepan3k nil
  "The Python trepan3k debugger"
  :group 'processes
  :group 'realgud
  :group 'python
  :version "23.1")

(declare-function trepan3k-query-cmdline  'realgud:trepan3k-core)
(declare-function trepan3k-parse-cmd-args 'realgud:trepan3k-core)
(declare-function realgud-run-process 'realgud-core)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom trepan3k-command-name
  ;;"trepan3k --emacs 3"
  "trepan3k"
  "File name for executing the Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'trepan3k)

(declare-function trepan3k-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:trepan3k (&optional opt-command-line no-reset)
  "Invoke the trepan3k Python debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run trepan3k.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."


  (interactive)
  (let* (
	 (cmd-str (or opt-command-line (trepan3k-query-cmdline
					"trepan3k")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (trepan3k-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (realgud-run-process "trepan3k" script-name cmd-args
		      'trepan3k-track-mode no-reset)
    )
  )


(defalias 'trepan3k 'realgud:trepan3k)

(provide-me "realgud-")

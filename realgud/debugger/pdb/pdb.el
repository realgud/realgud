;;; Copyright (C) 2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `pdb' Main interface to pdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper"
			 "../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:pdb-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup pdb nil
  "The Python pdb debugger (realgud variant)"
  :group 'processes
  :group 'realgud
  :group 'python
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom pdb-command-name
  "pdb"
  "File name for executing the stock Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'pdb)

(declare-function pdb-track-mode (bool))
(declare-function pdb-query-cmdline  'realgud:pdb-core)
(declare-function pdb-parse-cmd-args 'realgud:pdb-core)
(declare-function realgud-run-process 'realgud-core)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:pdb (&optional opt-command-line no-reset)
  "Invoke the pdb Python debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run pdb.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."


  (interactive)
  (let* (
	 (cmd-str (or opt-command-line (pdb-query-cmdline
					"pdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (pdb-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (realgud-run-process "pdb" script-name cmd-args
		      'pdb-track-mode no-reset)
    )
  )


(defalias 'pdb 'realgud:pdb)

(provide-me "realgud-")

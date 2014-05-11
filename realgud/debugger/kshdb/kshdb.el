;;; Copyright (C) 2011, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `kshdb' Main interface to kshdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:kshdb-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup kshdb nil
  "The Korn shell debugger: kshdb"
  :group 'processes
  :group 'realgud
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom kshdb-command-name
  ;;"kshdb --emacs 3"
  "kshdb"
  "File name for executing the kshdb and its command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'kshdb)

(declare-function kshdb-track-mode (bool))
(declare-function kshdb-query-cmdline  'realgud:kshdb-core)
(declare-function kshdb-parse-cmd-args 'realgud:kshdb-core)
(declare-function realgud-run-process 'realgud-core)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:kshdb (&optional opt-command-line no-reset)
  "Invoke the kshdb Z-shell debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run kshdb.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  (interactive)
  (let* ((cmd-str (or opt-command-line (kshdb-query-cmdline "kshdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (kshdb-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (realgud-run-process "kshdb" script-name cmd-args
		      'kshdb-track-mode no-reset)
    ))

(defalias 'kshdb 'realgud:kshdb)
(provide-me "realgud-")

;;; kshdb.el ends here

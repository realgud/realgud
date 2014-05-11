;;; Copyright (C) 2011, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `zshdb' Main interface to zshdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:zshdb-")

(declare-function zshdb-track-mode (bool))
(declare-function zshdb-query-cmdline  'realgud:zshdb-core)
(declare-function zshdb-parse-cmd-args 'realgud:zshdb-core)
(declare-function realgud-run-process 'realgud-core)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup zshdb nil
  "The Zsh debugger: zshdb"
  :group 'processes
  :group 'realgud
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom zshdb-command-name
  ;;"zshdb --emacs 3"
  "zshdb"
  "File name for executing the zshdb and its command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'zshdb)

(declare-function zshdb-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:zshdb (&optional opt-command-line no-reset)
  "Invoke the zshdb Z-shell debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run zshdb.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  (interactive)
  (let* ((cmd-str (or opt-command-line (zshdb-query-cmdline "zshdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (zshdb-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (realgud-run-process "zshdb" script-name cmd-args
		      'zshdb-track-mode no-reset)
    ))

(defalias 'zshdb 'realgud:zshdb)
(provide-me "realgud-")

;;; zshdb.el ends here

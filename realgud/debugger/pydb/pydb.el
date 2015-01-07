;;; Copyright (C) 2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `pydb' Main interface to pydb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")   "realgud:")
(require-relative-list '("core" "track-mode") "realgud:pydb-")

(declare-function pydb-query-cmdline   'realgud:pydb-core)
(declare-function pydb-parse-cmd-args  'realgud:pydb-core)
(declare-function realgud:run-debugger 'realgud:run)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:pydb nil
  "The realgud interface to the Python pydb debugger"
  :group 'realgud
  :group 'python
  :version "24.1")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:pydb-command-name
  "pydb"
  "File name for executing the stock Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:pydb)

(declare-function pydb-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:pydb (&optional opt-cmd-line no-reset)
  "Invoke the pydb Python debugger and start the Emacs user interface.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `pydb-parse-cmd-args' and path elements found by that
are expanded using `realgud:expand-file-name-if-exists'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)
  (realgud:run-debugger "pydb" 'pydb-query-cmdline 'pydb-parse-cmd-args
			'realgud:pydb-minibuffer-history
			opt-cmd-line no-reset)
  )


(defalias 'pydb 'realgud:pydb)

(provide-me "realgud-")

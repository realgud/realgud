;;; Copyright (C) 2010-2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `pydbgr' Main interface to pydbgr via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper"
			 "../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud-pydbgr-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:pydbgr nil
  "The realgud interface to the Python pydbgr debugger"
  :group 'realgud
  :group 'python
  :version "24.1")

(declare-function pydbgr-query-cmdline  'realgud-pydbgr-core)
(declare-function pydbgr-parse-cmd-args 'realgud-pydbgr-core)
(declare-function pydbgr-track-mode     'realgud:pydbgr-track-mode)
(declare-function realgud:run-debugger  'realgud:run)

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:pydbgr-command-name
  ;;"pydbgr --emacs 3"
  "pydbgr"
  "File name for executing the Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:pydbgr)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud-pydbgr (&optional opt-cmd-line no-reset)
  "Invoke the pydbgr Python debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `pydbgr-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "pydbgr"
			'pydbgr-query-cmdline
			'pydbgr-parse-cmd-args
			'realgud:pydbgr-minibuffer-history
			opt-cmd-line no-reset)
  )

(defalias 'pydbgr 'realgud-pydbgr)

(provide-me "realgud-")

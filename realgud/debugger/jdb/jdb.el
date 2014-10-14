;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
;;  `jdb' Main interface to jdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:jdb-")

(declare-function realgud:jdb-query-cmdline  'realgud:jdb-core)
(declare-function realgud:jdb-parse-cmd-args 'realgud:jdb-core)
(declare-function realgud:run-debugger 'realgud:run)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:jdb nil
  "The realgud interface to the Java's jdb debugger"
  :group 'java
  :group 'realgud
  :version "24.1")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:jdb-command-name
  ;;"jdb --emacs 3"
  "jdb"
  "File name for executing the Java debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:jdb)

;;;###autoload
(defun realgud:jdb (&optional opt-cmd-line no-reset)
  "Invoke the Java jdb debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `jdb-parse-cmd-args' and path elements found by that
are expanded using `expand-file-name'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)
  (realgud:run-debugger "jdb" 'realgud:jdb-query-cmdline
			'realgud:jdb-parse-cmd-args
			'jdb-track-mode-hook
			'realgud:jdb-minibuffer-history
			opt-cmd-line no-reset)
  )

(defalias 'jdb 'realgud:jdb)
(provide-me "realgud-")
;;; jdb.el ends here

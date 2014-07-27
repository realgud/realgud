;;; Copyright (C) 2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `pdb' Main interface to pdb via Emacs
(require 'load-relative)
(require-relative-list '("core" "track-mode") "realgud:pdb-")
(require-relative-list '("../../common/run")  "realgud:")
(require-relative-list '("core" "track-mode") "realgud:pdb-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:pdb nil
  "The realgud interface to the Python pdb debugger"
  :group 'realgud
  :version "24.1")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:pdb-command-name
  "pdb"
  "File name for executing the stock Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:pdb)
;; -------------------------------------------------------------------
;; The end.
;;

(declare-function pdb-track-mode       'realgud:pdb-track)
(declare-function pdb-query-cmdline    'realgud:pdb-core)
(declare-function pdb-parse-cmd-args   'realgud:pdb-core)
(declare-function realgud:run-debugger 'realgud:run)

;;;###autoload
(defun realgud:pdb (&optional opt-cmd-line no-reset)
  "Invoke the pdb Python debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run pdb. You will be prompted
for a command line is one isn't supplied.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `pdb-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "pdb" 'pdb-query-cmdline
			'pdb-parse-cmd-args
			'pdb-track-mode-hook
			'realgud:pdb-minibuffer-history
			opt-cmd-line no-reset)
  )


(defalias 'pdb 'realgud:pdb)

(provide-me "realgud-")

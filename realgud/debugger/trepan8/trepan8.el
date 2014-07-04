;;; Copyright (C) 2010-2011, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `trepan8' Main interface to trepan8 via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:trepan8-")

(declare-function trepan8-query-cmdline  'realgud:trepan8-core)
(declare-function trepan8-parse-cmd-args 'realgud:trepan8-core)
(declare-function realgud:run-debugger 'realgud:run)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup trepan8 nil
  "The Ruby \"trepanning\" debugger for Ruby 1.8"
  :group 'processes
  :group 'ruby
  :group 'realgud
  :version "23.1")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom trepan8-command-name
  ;;"trepan8 --emacs 3"
  "trepan8"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'trepan8)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:trepan8 (&optional opt-cmd-line no-reset)
  "Invoke the trepan8 Ruby debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepan8-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "trepan8" 'trepan8-query-cmdline 'trepan8-parse-cmd-args
			'trepan8-track-mode-hook opt-cmd-line no-reset)
  )

(defalias 'trepan8 'realgud:trepan8)

(provide-me "realgud-")

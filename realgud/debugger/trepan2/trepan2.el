;;; Copyright (C) 2010-2012, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `trepan2' Main interface to trepan2 via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:trepan2-")

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
(declare-function realgud:run-debugger   'realgud:run)

;; -------------------------------------------------------------------
;; User-definable variables
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
(defun realgud:trepan2 (&optional opt-cmd-line no-reset)
  "Invoke the trepan2 Python debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepan2-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "trepan2" 'trepan2-query-cmdline 'trepan2-parse-cmd-args
			'trepan2-track-mode-hook opt-cmd-line no-reset)
  )


(defalias 'trepan2 'realgud:trepan2)

(provide-me "realgud-")

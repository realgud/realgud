;; Copyright (C) 2010-2012, 2014-2017, 2020 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Main interface to trepan2 via Emacs

(require 'python) ; for python-shell-interpreter
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:trepan2-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 25.
(defgroup realgud:trepan2 nil
  "The realgud interface to the Python trepan2 debugger"
  :group 'realgud
  :group 'python
  :version "25.1")

(declare-function trepan2-query-cmdline  'realgud:trepan2-core)
(declare-function trepan2-parse-cmd-args 'realgud:trepan2-core)
(declare-function trepan2-track-mode     'realgud:pydbgr-track-mode)
(declare-function realgud:run-debugger   'realgud:run)
(declare-function realgud:run-process  'realgud:run)
(declare-function realgud:flatten      'realgud-utils)

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:trepan2-command-name
  ;;"trepan2 --emacs 3"
  "trepan2"
  "File name for executing the Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:trepan2)


;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defalias 'trepan2 'realgud:trepan2)

;;;###autoload
(defun realgud:trepan2 (&optional opt-cmd-line no-reset)
  "Invoke the trepan2 Python debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepan2-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "trepan2"
			'trepan2-query-cmdline
			'trepan2-parse-cmd-args
			'realgud:trepan2-minibuffer-history
			opt-cmd-line no-reset)
  )

;;;###autoload
(defun realgud:trepan2-delayed ()
  "This is like `trepan2', but assumes inside the program to be debugged, you
have a call to the debugger somewhere, e.g. 'from trepan.api import debug; debug()'.
Therefore we invoke python rather than the debugger initially.

"
  (interactive)
  (let* ((initial-debugger python-shell-interpreter)
	 (actual-debugger "trepan2")
	 (cmd-str (trepan2-query-cmdline initial-debugger))
	 (cmd-args (split-string-and-unquote cmd-str))
	 ;; XXX: python gets registered as the interpreter rather than
	 ;; a debugger, and the debugger position (nth 1) is missing:
	 ;; the script-args takes its place.
	 (parsed-args (trepan2-parse-cmd-args cmd-args))
	 (script-args (nth 1 parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
	  (cl-remove-if 'nil (realgud:flatten parsed-args))))
    (realgud:run-process actual-debugger script-name parsed-cmd-args
			 'realgud:trepan2-minibuffer-history)))

(realgud-deferred-invoke-setup "trepan2")

(provide-me "realgud-")

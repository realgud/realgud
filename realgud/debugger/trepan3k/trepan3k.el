;; Copyright (C) 2010-2014, 2016-2017, 2020 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'python) ; for python-shell-interpreter
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:trepan3k-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 25.
(defgroup realgud:trepan3k nil
  "The realgud interface to the Python debugger, trepan3k"
  :group 'realgud
  :group 'python
  :version "25.1")

(declare-function trepan3k-query-cmdline  'realgud:trepan3k-core)
(declare-function trepan3k-parse-cmd-args 'realgud:trepan3k-core)
(declare-function realgud:run-debugger    'realgud:run)
(declare-function realgud:run-process     'realgud:run)
(declare-function realgud:flatten         'realgud-utils)

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:trepan3k-command-name
  ;;"trepan3k --emacs 3"
  "trepan3k"
  "File name for executing the Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:trepan3k)

(declare-function trepan3k-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:trepan3k (&optional opt-cmd-line no-reset)
  "Invoke the trepan3k Python debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepan3k-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "trepan3k"
			'trepan3k-query-cmdline
			'trepan3k-parse-cmd-args
			'realgud:trepan3k-minibuffer-history
			opt-cmd-line no-reset)
  )

;;;###autoload
(defalias 'trepan3k 'realgud:trepan3k)

;;;###autoload
(defun realgud:trepan3k-delayed ()
  "This is like `trepan3k', but assumes inside the program to be debugged, you
have a call to the debugger somewhere, e.g. 'from trepan.api import debug; debug()'.
Therefore we invoke python rather than the debugger initially.
"
  (interactive)
  (let* ((initial-debugger python-shell-interpreter)
	 (actual-debugger "trepan3k")
	 (cmd-str (trepan3k-query-cmdline initial-debugger))
	 (cmd-args (split-string-and-unquote cmd-str))
	 ;; XXX: python gets registered as the interpreter rather than
	 ;; a debugger, and the debugger position (nth 1) is missing:
	 ;; the script-args takes its place.
	 (parsed-args (trepan3k-parse-cmd-args cmd-args))
	 (script-args (nth 1 parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
	  (cl-remove-if 'nil (realgud:flatten parsed-args))))
    (realgud:run-process actual-debugger script-name parsed-cmd-args
			 'realgud:trepan3k-delayed-minibuffer-history)))

(realgud-deferred-invoke-setup "trepan3k")

(provide-me "realgud-")

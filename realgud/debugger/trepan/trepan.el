;; Copyright (C) 2010-2011, 2013-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;  `trepan' Main interface to trepan via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:trepan-")

(declare-function realgud:trepan-query-cmdline  'realgud:trepan-core)
(declare-function realgud:trepan-parse-cmd-args 'realgud:trepan-core)
(declare-function realgud:run-debugger 'realgud:run)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 25.
(defgroup realgud:trepan nil
  "The realgud interface to the Ruby 1.9.2 1.9.3 \"trepanning\" debugger"
  :group 'ruby
  :group 'realgud
  :version "25.1")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:trepan-command-name
  ;;"trepan --emacs 3"
  "trepan"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:trepan)

;;;###autoload
(defun realgud:trepan (&optional opt-cmd-line no-reset)
  "Invoke the trepan Ruby debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepan-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "trepan" 'realgud:trepan-query-cmdline
			'realgud:trepan-parse-cmd-args
			'realgud:trepan-minibuffer-history
			opt-cmd-line no-reset)
  )

;;;###autoload
(defalias 'trepan 'realgud:trepan)
(provide-me "realgud-")
;;; trepan.el ends here

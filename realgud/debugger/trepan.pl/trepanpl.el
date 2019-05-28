;; Copyright (C) 2011, 2014, 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;  `trepanpl' Main interface to trepanpl via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track")  "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:trepanpl-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:trepanpl nil
  "The realgud interface to the Perl \"trepanning\" debugger"
  :group 'perl
  :group 'realgud
  :version "25.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:trepanpl-command-name
  "trepan.pl"
  "File name for executing the Perl debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:trepanpl)

;; -------------------------------------------------------------------
;; The end.
;;

(declare-function realgud:trepanpl-track-mode     'realgud-trepanpl-track)
(declare-function realgud:trepanpl-query-cmdline  'realgud-trepanpl-core)
(declare-function realgud:trepanpl-parse-cmd-args 'realgud-trepanpl-core)
(declare-function realgud:run-debugger             'realgud:run)

;;;###autoload
(defun realgud:trepan.pl (&optional opt-cmd-line no-reset)
  "Invoke the trepan.pl Perl debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run trepan.pl. You will be prompted
for a command line is one isn't supplied.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `realgud:trepanpl-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "trepan.pl" 'realgud:trepanpl-query-cmdline
			'realgud:trepanpl-parse-cmd-args
			'realgud:trepanpl-minibuffer-history
			opt-cmd-line no-reset))

;;;###autoload
(defalias 'trepan.pl 'realgud:trepan.pl)
(provide-me "realgud-")
;;; trepanpl.el ends here

;; Copyright (C) 2011, 2014-2017, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;  `perldb' Main interface to perl debugger via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:perldb-")

(declare-function realgud:run-debugger 'realgud:run)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:perldb nil
  "The realgud interface to the Perl debugger, perldb"
  :group 'realgud
  :version "25.1")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:perldb-command-name
  "perl -d"
  "Option to needed to run the Perl debugger"
  :type 'string
  :group 'realgud:perldb)

;; -------------------------------------------------------------------
;; The end.
;;

(declare-function realgud:perldb-query-cmdline  'realgud:perldb-core)
(declare-function realgud:perldb-parse-cmd-args 'realgud:perldb-core)

;;;###autoload
(defun realgud:perldb (&optional opt-cmd-line no-reset)
  "Invoke the Perl debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run perldb.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `perldb-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "perldb"
			'realgud:perldb-query-cmdline
			'realgud:perldb-parse-cmd-args
			'realgud:perldb-minibuffer-history
			opt-cmd-line no-reset))

;; (defalias 'perldb 'realgud:perldb)

(provide-me "realgud-")

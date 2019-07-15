;; Copyright (C) 2011, 2014-2016, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;  `kshdb' Main interface to kshdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:kshdb-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:kshdb nil
  "The realgud interface to the Korn shell debugger, kshdb"
  :group 'realgud
  :version "25.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:kshdb-command-name
  ;;"kshdb --emacs 3"
  "kshdb"
  "File name for executing the kshdb and its command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:kshdb)

(declare-function kshdb-track-mode (bool))
(declare-function kshdb-query-cmdline  'realgud:kshdb-core)
(declare-function kshdb-parse-cmd-args 'realgud:kshdb-core)
(declare-function realgud:run-process 'realgud-run)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:kshdb (&optional opt-command-line no-reset)
  "Invoke the Korn shell debugger, kshdb, and start the Emacs user interface.

String COMMAND-LINE specifies how to run kshdb.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  (interactive)
  (let* ((cmd-str (or opt-command-line (kshdb-query-cmdline "kshdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (kshdb-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf))
    (realgud:run-process "kshdb" script-name cmd-args
			 'realgud:kshdb-minibuffer-history
			 no-reset)
    ))

;;;###autoload
(defalias 'kshdb 'realgud:kshdb)
(provide-me "realgud-")

;;; kshdb.el ends here

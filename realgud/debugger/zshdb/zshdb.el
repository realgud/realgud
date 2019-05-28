;; Copyright (C) 2011, 2014-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;  `zshdb' Main interface to zshdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track")  "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:zshdb-")

(declare-function zshdb-track-mode (bool))
(declare-function zshdb-query-cmdline  'realgud:zshdb-core)
(declare-function zshdb-parse-cmd-args 'realgud:zshdb-core)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 25.
(defgroup realgud:zshdb nil
  "The realgud interface to the Zsh debugger, zshdb"
  :group 'realgud
  :version "25.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:zshdb-command-name
  ;;"zshdb --emacs 3"
  "zshdb"
  "File name for executing the zshdb and its command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:zshdb)

(declare-function zshdb-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

(declare-function zshdb-track-mode     'realgud-zshdb-track-mode)
(declare-function zshdb-query-cmdline  'realgud:zshdb-core)
(declare-function zshdb-parse-cmd-args 'realgud:zshdb-core)
(declare-function realgud:run-debugger 'realgud:run)

; ### FIXME: DRY with other top-level routines
;;;###autoload
(defun realgud:zshdb (&optional opt-cmd-line no-reset)
  "Invoke the zshdb Z-shell debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run zshdb.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `zshdb-parse-cmd-args' and path elements found by that
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
  (let ((cmd-buf
	 (realgud:run-debugger realgud:zshdb-command-name 'zshdb-query-cmdline
			       'zshdb-parse-cmd-args
			       'realgud:zshdb-minibuffer-history
			       opt-cmd-line no-reset)))
    ;; (if cmd-buf
    ;; 	(with-current-buffer cmd-buf
    ;; 	  ;; FIXME should allow customization whether to do or not
    ;; 	  ;; and also only do if hook is not already there.
    ;; 	  (realgud:zshdb-remove-ansi-schmutz)
    ;; 	  )
    ;;   )
    ))

;;;###autoload
(defalias 'zshdb 'realgud:zshdb)

(provide-me "realgud-")

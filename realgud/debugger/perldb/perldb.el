;;; Copyright (C) 2011, 2014 Rocky Bernstein <rocky@gnu.org>
;;  `perldb' Main interface to perl debugger via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:perldb-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup realgud:perldb nil
  "The Perl debugger (realgud variant)"
  :group 'processes
  :group 'realgud
  :group 'perl
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:perldb-command-name
  "perl -d"
  "Option to needed to run the Perl debugger"
  :type 'string
  :group 'realgud:perldb)

(declare-function realgud:perldb-track-mode (bool))
(declare-function realgud:perldb-query-cmdline  'realgud:perldb-core)
(declare-function realgud:perldb-parse-cmd-args 'realgud:perldb-core)
(declare-function realgud-run-process 'realgud-core)


;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:perldb (&optional opt-command-line no-reset)
  "Invoke the Perl debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run perldb.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  (interactive)
  (let* ((cmd-str (or opt-command-line (realgud:perldb-query-cmdline "perldb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (realgud:perldb-parse-cmd-args cmd-args))
	 (script-args (cadr parsed-args))
	 (script-name (car script-args))
	 (cmd-buf
	  (realgud-run-process "perldb" script-name cmd-args
			    'realgud:perldb-track-mode no-reset)

	  ))
    ))

(defalias 'perl5db 'realgud:perldb)
(defalias 'perldb 'realgud:perldb)

(provide-me "realgud-")
;;; perldb.el ends here

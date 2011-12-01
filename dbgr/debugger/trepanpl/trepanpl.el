;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;  `trepanpl' Main interface to trepanpl via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "dbgr-")
(require-relative-list '("../../common/track") "dbgr-")
(require-relative-list '("core" "track-mode") "dbgr-trepanpl-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup trepanpl nil
  "The Perl \"trepanning\" debugger"
  :group 'processes
  :group 'perl
  :group 'dbgr
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom dbgr-trepanpl-command-name
  ;;"trepanpl --emacs 3"
  "trepan.pl"
  "File name for executing the Perl debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'trepanpl)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun dbgr-trepanpl (&optional opt-command-line no-reset)
  "Invoke the trepan.pl Perl debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run trepanpl.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  (interactive)
  (let* ((cmd-str (or opt-command-line 
		      (dbgr-trepanpl-query-cmdline "trepan.pl")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (dbgr-trepanpl-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf
	   (dbgr-run-process "trepan.pl" script-name cmd-args 
			     'dbgr-trepanpl-track-mode no-reset)
	   ))
  ))

(defalias 'trepan.pl 'dbgr-trepanpl)
(defalias 'trepanpl 'dbgr-trepanpl)
(provide-me "dbgr-")
;;; trepanpl.el ends here

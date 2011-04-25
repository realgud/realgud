;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
;;  `trepan' Main interface to trepan via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "dbgr-")
(require-relative-list '("../../common/track") "dbgr-")
(require-relative-list '("core" "track-mode") "dbgr-trepan-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup trepan nil
  "The Ruby 1.9.2 \"trepanning\" debugger"
  :group 'processes
  :group 'ruby
  :group 'dbgr
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom trepan-command-name
  ;;"trepan --emacs 3"
  "trepan"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'trepan)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun dbgr-trepan (&optional opt-command-line no-reset)
  "Invoke the trepan Ruby debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run trepan.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  (interactive)
  (let* ((cmd-str (or opt-command-line (trepan-query-cmdline "trepan")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (trepan-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf
	   (dbgr-run-process "trepan" script-name cmd-args 
			     'trepan-track-mode no-reset)
	   ))
  ))

(defalias 'trepan 'dbgr-trepan)
(provide-me "dbgr-")
;;; trepan.el ends here

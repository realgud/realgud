;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
;;  `trepanpl' Main interface to trepanpl via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud-trepanpl-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup trepanpl nil
  "The Perl \"trepanning\" debugger"
  :group 'processes
  :group 'perl
  :group 'realgud
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud-trepanpl-command-name
  "trepan.pl"
  "File name for executing the Perl debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'trepanpl)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud-trepanpl (&optional opt-command-line no-reset)
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
		      (realgud-trepanpl-query-cmdline "trepan.pl")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (realgud-trepanpl-parse-cmd-args cmd-args))
	 (script-args (cdr cmd-args))
	 (script-name (car script-args))
	 (cmd-buf
	   (realgud-run-process "trepan.pl" script-name cmd-args
			     'realgud-trepanpl-track-mode no-reset)
	   ))
  ))

(defalias 'trepan.pl 'realgud-trepanpl)
(provide-me "realgud-")
;;; trepanpl.el ends here

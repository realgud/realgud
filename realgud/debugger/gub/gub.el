;;; Copyright (C) 2013 Rocky Bernstein <rocky@gnu.org>
;;  `gub' Main interface to Go gub via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud-gub-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup gub nil
  "The Go SSA interpreter debugger: gub"
  :group 'processes
  :group 'realgud
  :group 'make
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom gub-command-name
  "tortoise -run -gub= -interp=SS --"
  "File name for executing the Go SSA interpreter/debugger, gub, and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'gub)

(declare-function gub-query-cmdline  'realgud-gub-core)
(declare-function gub-parse-cmd-args 'realgud-gub-core)
(declare-function realgud-run-process 'realgud-core)


(defun realgud-gub-fn (&optional opt-command-line no-reset)
  "See `realgud-gub' for details"

  (let* ((cmd-str (or opt-command-line (gub-query-cmdline "gub")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (gub-parse-cmd-args cmd-args))
	 (gub-program (car parsed-args))
	 (gub-args (cadr parsed-args))
	 (go-prog-and-args (caddr parsed-args))
	 (script-filename (car go-prog-and-args))
	 (cmd-buf))
    (realgud-run-process gub-program script-filename cmd-args
			 'gub-track-mode no-reset)
    )
  )

;;;###autoload
(defun realgud-gub (&optional opt-command-line no-reset)
  "Invoke the Go SSA debugger, gub and start the Emacs user interface.

String COMMAND-LINE specifies how to run gub.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."


  (interactive)
  (realgud-gub-fn opt-command-line no-reset)
  )

(defalias 'gub 'realgud-gub)

(provide-me "realgud-")
;;; gub.el ends here

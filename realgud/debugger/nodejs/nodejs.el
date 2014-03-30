;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
;;  `nodejs' Main interface to nodejs debugger via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud-nodejs-")

(declare-function realgud-nodejs-remove-ansi-shmutz 'realgud-core)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup nodejs nil
  "The nodejs debugger"
  :group 'processes
  :group 'realgud
  :version "23.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom nodejs-command-name
  "node debug"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'nodejs)

(declare-function nodejs-track-mode (bool))
(declare-function nodejs-query-cmdline  'realgud-nodejs-core)
(declare-function nodejs-parse-cmd-args 'realgud-nodejs-core)
(declare-function realgud-run-process 'realgud-core)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud-nodejs (&optional opt-command-line no-reset)
  "Invoke the nodejs shell debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run nodejs.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."
  (interactive)
  (let* ((cmd-str (or opt-command-line (nodejs-query-cmdline "nodejs")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (nodejs-parse-cmd-args cmd-args))
	 (script-args (cadr parsed-args))
	 (script-name (car script-args))
	 (cmd-buf  (realgud-run-process nodejs-command-name script-name
					cmd-args
					'nodejs-track-mode no-reset))
	 )
    (if cmd-buf
	(with-current-buffer cmd-buf
	  ;; FIXME should allow customization whether to do or not
	  ;; and also only do if hook is not already there.
	  (realgud-nodejs-remove-ansi-shmutz)
	  )
      )
    ))

(defalias 'nodejs 'realgud-nodejs)

(provide-me "realgud-")

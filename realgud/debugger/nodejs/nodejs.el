;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
;;  `nodejs' Main interface to nodejs debugger via Emacs
(require 'list-utils)
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:nodejs-")

(declare-function realgud:nodejs-remove-ansi-shmutz 'realgud-core)
(declare-function realgud:run-debugger 'realgud:run)

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

;; -------------------------------------------------------------------
;; The end.
;;

(declare-function nodejs-track-mode     'realgud-nodejs-track-mode)
(declare-function nodejs-query-cmdline  'realgud:nodejs-core)
(declare-function nodejs-parse-cmd-args 'realgud:nodejs-core)
(declare-function realgud:run-process   'realgud:run)

; ### FIXME: DRY with other top-level routines
;;;###autoload
(defun realgud:nodejs (&optional opt-command-line no-reset)
  "Invoke the nodejs shell debugger and start the Emacs user interface.

String COMMAND-LINE specifies how to run nodejs.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `nodejs-parse-cmd-args' and path elements found by that
are expanded using `expand-file-name'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)
  (let* ((cmd-str (or opt-command-line (nodejs-query-cmdline "nodejs")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (nodejs-parse-cmd-args cmd-args))
	 (script-args (cadr parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
	  (list-utils-flatten (list (car parsed-args) (cadr parsed-args))))
	 (cmd-buf  (realgud:run-process "node" script-name
					parsed-cmd-args
					'nodejs-track-mode no-reset))
	 )
    (if cmd-buf
	(with-current-buffer cmd-buf
	  ;; FIXME should allow customization whether to do or not
	  ;; and also only do if hook is not already there.
	  (realgud:nodejs-remove-ansi-shmutz)
	  )
      )))

(defalias 'nodejs 'realgud:nodejs)

(provide-me "realgud-")

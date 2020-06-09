;; Copyright (C) 2015, 2020 Free Software Foundation, Inc
;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;  `gub' Main interface to Go gub via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud:gub-")
(require-relative-list '("../../common/run") "realgud:")

(eval-when-compile (require 'cl-lib))

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:gub nil
  "The realgud interface to the Go SSA interpreter debugger, gub"
  :group 'realgud
  :version "23.4")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:gub-command-name
  "tortoise -run -gub= -interp=SS --"
  "File name for executing the Go SSA interpreter/debugger, gub, and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:gub)

(declare-function gub-query-cmdline  'realgud-gub-core)
(declare-function gub-parse-cmd-args 'realgud-gub-core)
(declare-function realgud:run-process 'realgud:run)


(defun realgud-gub-fn (&optional opt-command-line no-reset)
  "See `realgud-gub' for details"

  (let* ((cmd-str (or opt-command-line (gub-query-cmdline "gub")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (gub-parse-cmd-args cmd-args))
	 (gub-program (car parsed-args))
	 (gub-args (cadr parsed-args))
	 (go-prog-and-args (cl-caddr parsed-args))
	 (script-filename (car go-prog-and-args))
	 (cmd-buf))
    (realgud:run-process gub-program script-filename cmd-args
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

;;;###autoload
(defalias 'gub 'realgud-gub)

(provide-me "realgud-")
;;; gub.el ends here

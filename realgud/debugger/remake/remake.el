;; Copyright (C) 2015-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;  `remake' Main interface to remake via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track")  "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:remake-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 25.
(defgroup realgud:remake nil
  "The realgud interface to the GNU Make debugger"
  :group 'realgud
  :group 'make
  :version "25.1")

(eval-when-compile (require 'cl-lib))

(declare-function remake-query-cmdline  'realgud:remake-core)
(declare-function remake-parse-cmd-args 'realgud:remake-core)
(declare-function realgud:run-debugger  'realgud:run)
(declare-function realgud:run-process   'realgud:run)

(defun realgud:remake-run-debugger (&optional opt-command-line
				    no-reset)
  "Invoke the a debugger and start the Emacs user interface.

String OPT-COMMAND-LINE specifies how to run DEBUGGER-NAME. You
will be prompted for a command line using QUERY-CMDLINE-FN is one
isn't supplied.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by PARSE-CMD-FN and path elements found by that
are expanded using `expand-file-name'.

If successful, The command buffer of the debugger process is returned.
Otherwise nil is returned.
"
  (let* ((cmd-str (or opt-command-line (remake-query-cmdline "remake")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (remake-parse-cmd-args cmd-args))
	 (debugger (car parsed-args))
	 (script-args (cl-caddr parsed-args))
	 (script-name (cadr parsed-args))
	 )
    (realgud:run-process debugger script-name cmd-args
			 realgud:remake-minibuffer-history no-reset)
    )
  )

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:remake-command-name
  ;;"remake --emacs 3"
  "remake"
  "File name for executing the GNU make debugger, remake, and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:remake)

;;;###autoload
(defun realgud:remake (&optional opt-cmd-line no-reset)
  "See `realgud:remake' for details"
  (interactive)
  (realgud:remake-run-debugger opt-cmd-line no-reset)
  )

;;;###autoload
(defalias 'remake 'realgud:remake)

(provide-me "realgud-")
;;; remake.el ends here

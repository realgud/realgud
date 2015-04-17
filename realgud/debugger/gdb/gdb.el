;; Copyright (C) 2015 Free Software Foundation, Inc

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
;;; Copyright (C) 2014-2015 Rocky Bernstein <rocky@gnu.org>

;;  `realgud:gdb' Main interface to gdb via Emacs
(require 'cl)
(require 'list-utils)
(require 'load-relative)
(require-relative-list '("../../common/helper" "../../common/utils")
		       "realgud-")
(require-relative-list '("core" "track-mode") "realgud:gdb-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:gdb nil
  "The realgud interface to gdb"
  :group 'realgud
  :version "24.1")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:gdb-command-name
  ;;"gdb --emacs 3"
  "gdb"
  "File name for executing the and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:gdb)

(declare-function realgud:gdb-track-mode     'realgud:gdb-track-mode)
(declare-function realgud-command            'realgud:gdb-core)
(declare-function realgud:gdb-parse-cmd-args 'realgud:gdb-core)
(declare-function realgud:gdb-query-cmdline  'realgud:gdb-core)
(declare-function realgud:run-process        'realgud-core)
(declare-function realgud:flatten            'realgud-utils)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:gdb (&optional opt-cmd-line no-reset)
  "Invoke the gdb debugger and start the Emacs user interface.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"

  (interactive)
  (let* ((cmd-str (or opt-cmd-line (realgud:gdb-query-cmdline "gdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (realgud:gdb-parse-cmd-args cmd-args))
	 (script-args (caddr parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
	  (cl-remove-if 'nil (realgud:flatten parsed-args)))
	 (cmd-buf (realgud:run-process realgud:gdb-command-name
				       script-name parsed-cmd-args
				       'realgud:gdb-minibuffer-history
				       nil))
	 )
    (if cmd-buf
	(with-current-buffer cmd-buf
	  (realgud-command "set annotate 1" nil nil nil)
	  )
      )
    )
  )

(provide-me "realgud-")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

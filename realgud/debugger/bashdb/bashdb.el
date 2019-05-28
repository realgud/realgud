;; Copyright (C) 2015-2016, 2018-2019 Free Software Foundation, Inc

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

;;  `bashdb' Main interface to bashdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/cmds"
			 "../../common/helper")
		       "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:bashdb-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 25.
(defgroup realgud:bashdb nil
  "The realgud interface to bashdb"
  :group 'realgud
  :version "25.3")

(declare-function realgud-command            'realgud:cmds)

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:bashdb-command-name
  ;;"bashdb --emacs 3"
  "bashdb"
  "File name for executing the bash debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:bashdb)

;; -------------------------------------------------------------------
;; The end.
;;

(declare-function bashdb-track-mode     'realgud-bashdb-track-mode)
(declare-function bashdb-query-cmdline  'realgud:bashdb-core)
(declare-function bashdb-parse-cmd-args 'realgud:bashdb-core)
(declare-function realgud:run-debugger 'realgud:run)

;;;###autoload
(defun realgud:bashdb (&optional opt-cmd-line no-reset)
  "Invoke the bashdb shell debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run bash. You will be prompted
for a command line is one isn't supplied.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `bashdb-parse-cmd-args' and path elements found by that
are expanded using `realgud:expand-file-name-if-exists'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)
  (realgud:run-debugger "bashdb"
			'bashdb-query-cmdline
			'bashdb-parse-cmd-args
			'realgud:bashdb-minibuffer-history
			opt-cmd-line no-reset)
  )

(defun realgud:bashdb-large (&optional opt-cmd-line no-reset)
  "Use this is the program you are debugging is large, say over 1,000 lines or so.
"
  (interactive)
  (let ((cmd-buf
	 (realgud:run-debugger "bashdb"
			       'bashdb-query-cmdline
			       'bashdb-parse-cmd-args
			       'realgud:bashdb-minibuffer-history
			       opt-cmd-line no-reset)
	 ))
    (if cmd-buf
	(let ((process (get-buffer-process cmd-buf)))
	  (if (and process (eq 'run (process-status process)))
	      (with-current-buffer cmd-buf
		(sleep-for 1)
		(realgud-command "frame 0" nil nil nil)
		)))
      )
    ))


;;;###autoload
(defalias 'bashdb 'realgud:bashdb)

;;;###autoload
(defalias 'bashdb-large 'realgud:bashdb-large)

(provide-me "realgud-")

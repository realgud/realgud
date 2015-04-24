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

;;  `trepan8' Main interface to trepan8 via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode")   "realgud:trepan8-")

(declare-function trepan8-query-cmdline  'realgud:trepan8-core)
(declare-function trepan8-parse-cmd-args 'realgud:trepan8-core)
(declare-function realgud:run-debugger   'realgud:run)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:trepan8 nil
  "The reagud interface toe th Ruby \"trepanning\" debugger for Ruby 1.8"
  :group 'ruby
  :group 'realgud
  :version "23.4")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:trepan8-command-name
  ;;"trepan8 --emacs 3"
  "trepan8"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:trepan8)

;; -------------------------------------------------------------------
;; The end.
;;

;;;###autoload
(defun realgud:trepan8 (&optional opt-cmd-line no-reset)
  "Invoke the trepan8 Ruby debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepan8-parse-cmd-args' and path elements found by that
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
  (realgud:run-debugger "trepan8" 'trepan8-query-cmdline
			'trepan8-parse-cmd-args
			'realgud:trepan8-minibuffer-history
			opt-cmd-line no-reset)
  )

(defalias 'trepan8 'realgud:trepan8)

(provide-me "realgud-")

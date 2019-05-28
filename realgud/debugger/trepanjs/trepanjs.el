;; Copyright (C) 2015-2016 Free Software Foundation, Inc

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
;;; Regular expressions for nodejs Javascript debugger.

;;  `trepanjs' Main interface to trepanjs via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("../../common/utils")    "realgud-")
(require-relative-list '("core" "track-mode") "realgud:trepanjs-")
(require-relative-list '("../../lang/js") "realgud-lang-")

(declare-function realgud:trepanjs-query-cmdline  'realgud:trepanjs-core)
(declare-function realgud:trepanjs-parse-cmd-args 'realgud:trepanjs-core)
(declare-function realgud:run-debugger 'realgud:run)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 25.
(defgroup realgud:trepanjs nil
  "The realgud interface to the Ruby 1.9.2 1.9.3 \"trepanjsning\" debugger"
  :group 'ruby
  :group 'realgud
  :version "25.1")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:trepanjs-command-name
  ;;"trepanjs --emacs 3"
  "trepanjs"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:trepanjs)

;;;###autoload
(defun realgud:trepanjs (&optional opt-cmd-line no-reset)
  "Invoke the trepanjs Ruby debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepanjs-parse-cmd-args' and path elements found by that
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
  (let ((cmd-buf
	 (realgud:run-debugger "trepanjs" 'realgud:trepanjs-query-cmdline
			       'realgud:trepanjs-parse-cmd-args
			       'realgud:trepanjs-minibuffer-history
			       opt-cmd-line no-reset)))
    ;; (if cmd-buf
    ;; 	(with-current-buffer cmd-buf
    ;; 	  ;; FIXME should allow customization whether to do or not
    ;; 	  ;; and also only do if hook is not already there.
    ;; 	  (realgud:remove-ansi-schmutz)
    ;; 	  )
    ;;   )
    ))

(defalias 'trepanjs 'realgud:trepanjs)
(provide-me "realgud-")
;;; trepanjs.el ends here

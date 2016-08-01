;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Author: Sean Farley <sean@farley.io>

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

;;  `ipdb' Main interface to ipdb via Emacs
(require 'load-relative)
(require-relative-list '("core" "track-mode") "realgud:ipdb-")
(require-relative-list '("../../common/run")  "realgud:")
(require-relative-list '("core" "track-mode") "realgud:ipdb-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:ipdb nil
  "The realgud interface to the Python ipdb debugger"
  :group 'realgud
  :version "24.3")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:ipdb-command-name
  "ipdb"
  "File name for executing the stock Python debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:ipdb)
;; -------------------------------------------------------------------
;; The end.
;;

(declare-function ipdb-track-mode       'realgud:ipdb-track)
(declare-function ipdb-query-cmdline    'realgud:ipdb-core)
(declare-function ipdb-parse-cmd-args   'realgud:ipdb-core)
(declare-function realgud:ipdb-completion-at-point 'realgud:ipdb-core)
(declare-function realgud:run-debugger 'realgud:run)

;;;###autoload
(defun realgud:ipdb (&optional opt-cmd-line no-reset)
  "Invoke the ipdb Python debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run ipdb. You will be prompted
for a command line is one isn't supplied.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `ipdb-parse-cmd-args' and path elements found by that
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
  (let ((cmd-buf (realgud:run-debugger "ipdb" 'ipdb-query-cmdline
                                       'ipdb-parse-cmd-args
                                       'realgud:ipdb-minibuffer-history
                                       opt-cmd-line no-reset))
        )
    (add-hook 'completion-at-point-functions
              'realgud:ipdb-completion-at-point nil t)
    (with-current-buffer cmd-buf
      (add-hook 'completion-at-point-functions
		'realgud:ipdb-completion-at-point nil t)
      )
    cmd-buf)
  )


;;;###autoload
(defun realgud:ipdb-remote (&optional opt-cmd-line no-reset)
  "Invoke the ipdb Python debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run ipdb. You will be prompted
for a command line is one isn't supplied.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `ipdb-parse-remote-cmd-args' and path elements found by that
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
  (let ((cmd-buf (realgud:run-debugger "ipdb" 'ipdb-remote-query-cmdline
                                       'ipdb-parse-remote-cmd-args
                                       'realgud:ipdb-remote-minibuffer-history
                                       opt-cmd-line no-reset "remote-ipdb"))
        )
    (add-hook 'completion-at-point-functions
              'realgud:ipdb-completion-at-point nil t)
    cmd-buf)
  )


(defalias 'ipdb 'realgud:ipdb)

(provide-me "realgud-")

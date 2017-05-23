;;  realgud:undodb-gdb  --- Main interface to undodb-gdb via Emacs
;;; Commentary:

;; Copyright (C) 2015-2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Author: Felipe Lema <felipelema@mortemale.org>

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

;;  `realgud:undodb-gdb' Main interface to undodb-gdb via Emacs
;;; Code:
(require 'load-relative)
(require-relative-list '("../../common/helper" "../../common/utils")
		       "realgud-")

(require-relative-list '("../../common/buffer/command"
			 "../../common/buffer/source")
		       "realgud-buffer-")

(require-relative-list '("core" "track-mode") "realgud:undodb-gdb-")

(declare-function realgud-cmdbuf? 'realgud-buffer-command)
(declare-function realgud:cmdbuf-associate 'realgud-buffer-source)
(declare-function realgud-parse-command-arg 'realgud-core)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:undodb-gdb nil
  "The realgud interface to undodb-gdb"
  :group 'realgud
  :version "24.3")

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:undodb-gdb-command-name
  ;;"undodb-gdb --emacs 3"
  "undodb-gdb"
  "File name for executing the and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:undodb-gdb)

(declare-function realgud:undodb-gdb-track-mode     'realgud:undodb-gdb-track-mode)
(declare-function realgud-command            'realgud:undodb-gdb-core)
(declare-function realgud:undodb-gdb-parse-cmd-args 'realgud:undodb-gdb-core)
(declare-function realgud:undodb-gdb-query-cmdline  'realgud:undodb-gdb-core)
(declare-function realgud:run-process        'realgud-core)
(declare-function realgud:flatten            'realgud-utils)

;; -------------------------------------------------------------------
;; The end.
;;

(defun realgud:undodb-gdb-pid-command-buffer (pid)
  "Return the command buffer used when undodb-gdb -p PID is invoked."
  (format "*undodb-gdb %d shell*" pid)
  )

(defun realgud:undodb-gdb-find-command-buffer (pid)
  "Find the among current buffers a buffer that is a realgud command buffer.
This realgud command buffer running undodb-gdb is identified by
 process number PID"
  (let ((find-cmd-buf (realgud:undodb-gdb-pid-command-buffer pid)))
    (dolist (buf (buffer-list))
      (if (and (equal find-cmd-buf (buffer-name buf))
             (realgud-cmdbuf? buf)
             (get-buffer-process buf))
          (return buf)))))

(defun realgud:undodb-gdb-pid (pid)
  "Start debugging undodb-gdb process with pid PID."
  (interactive "nEnter the pid that undodb-gdb should attach to: ")
  (realgud:undodb-gdb (format "%s -p %d" realgud:undodb-gdb-command-name pid))
  )

(defun realgud:undodb-gdb-pid-associate (pid)
  "Start debugging undodb-gdb process with pid PID.
Also, associate the current buffer to that realgud command buffer."
  (interactive "nEnter the pid that undodb-gdb should attach to and associate the current buffer to: ")
  (let* ((command-buf)
	 (source-buf (current-buffer))
	 )
    (realgud:undodb-gdb-pid pid)
    (setq command-buf (realgud:undodb-gdb-find-command-buffer pid))
    (if command-buf
	(with-current-buffer source-buf
	  (realgud:cmdbuf-associate))
      )))

;;;###autoload
(defun realgud:undodb-gdb (&optional opt-cmd-line no-reset)
  "Invoke the undodb-gdb debugger and start the Emacs user interface.

OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command.  If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset.  See `loc-changes-clear-buffer' to clear
fringe and marginal icons."

  (interactive)
  (let* ((cmd-str (or opt-cmd-line (realgud:undodb-gdb-query-cmdline "undodb-gdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (realgud:undodb-gdb-parse-cmd-args cmd-args))
	 (script-args (caddr parsed-args))
	 (script-name (or (car script-args) ""))
	 (parsed-cmd-args
          (cl-remove-if-not 'stringp (realgud:flatten parsed-args)))
	 (cmd-buf (realgud:run-process realgud:undodb-gdb-command-name
				       script-name parsed-cmd-args
				       'realgud:undodb-gdb-minibuffer-history
				       nil))
	 )
    (if cmd-buf
	(let ((process (get-buffer-process cmd-buf)))
	  (if (and process (eq 'run (process-status process)))
	      (with-current-buffer cmd-buf
		(realgud-command "set annotate 1" nil nil nil)
		)))
      )
    ))

(provide-me "realgud-")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
;;; undodb-gdb.el ends here

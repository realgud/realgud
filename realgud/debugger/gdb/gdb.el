;; Copyright (C) 2015-2016, 2019-2020 Free Software Foundation, Inc

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

;;  `realgud:gdb' Main interface to gdb via Emacs
(require 'load-relative)
(require-relative-list '("../../common/cmds"
			 "../../common/helper"
			 "../../common/utils")
		       "realgud-")

(require-relative-list '("../../common/run")
                       "realgud:")

(require-relative-list '("../../common/buffer/command"
			 "../../common/buffer/source")
		       "realgud-buffer-")

(require-relative-list '("core" "track-mode") "realgud:gdb-")

(declare-function realgud-cmdbuf? 'realgud-buffer-command)
(declare-function realgud:cmdbuf-associate 'realgud-buffer-source)
(declare-function realgud-parse-command-arg 'realgud-core)

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:gdb nil
  "The realgud interface to gdb"
  :group 'realgud
  :version "25.1")

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
(declare-function realgud-command            'realgud:cmds)
(declare-function realgud:gdb-parse-cmd-args 'realgud:gdb-core)
(declare-function realgud:gdb-query-cmdline  'realgud:gdb-core)
(declare-function realgud:run-process        'realgud:run)
(declare-function realgud:flatten            'realgud-utils)

;; -------------------------------------------------------------------
;; The end.
;;

(defun realgud:gdb-pid-command-buffer (pid)
  "Return the command buffer used when gdb -p PID is invoked"
  (format "*gdb %d shell*" pid)
  )

(defun realgud:gdb-find-command-buffer (pid)
  "Find the among current buffers a buffer that is a realgud command buffer
running gdb on process number PID"
  (let ((find-cmd-buf (realgud:gdb-pid-command-buffer pid))
	(found-buf))
    (dolist (buf (buffer-list))
      (if (and (equal find-cmd-buf (buffer-name buf))
		(realgud-cmdbuf? buf)
		(get-buffer-process buf))
	  (setq found-buf buf)))
    found-buf))

(defun realgud:gdb-pid (pid)
  "Start debugging gdb process with pid PID."
  (interactive "nEnter the pid that gdb should attach to: ")
  (realgud:gdb (format "%s -p %d" realgud:gdb-command-name pid))
  ;; FIXME: should add code to test if attach worked.
  )

(defun realgud:gdb-pid-associate (pid)
  "Start debugging gdb process with pid PID and associate the
current buffer to that realgud command buffer."
  (interactive "nEnter the pid that gdb should attach to and associate the current buffer to: ")
  (let* ((command-buf)
	 (source-buf (current-buffer))
	 )
    (realgud:gdb-pid pid)
    (setq command-buf (realgud:gdb-find-command-buffer pid))
    (if command-buf
	(with-current-buffer source-buf
	  (realgud:cmdbuf-associate))
      )))

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
	 (script-args (cl-caddr parsed-args))
	 (script-name (or (car script-args) ""))
	 (parsed-cmd-args
           (cl-remove-if-not 'stringp (realgud:flatten parsed-args)))
	 (cmd-buf (realgud:run-process realgud:gdb-command-name
				       script-name parsed-cmd-args
				       'realgud:gdb-minibuffer-history
				       nil))
	 )
    (if cmd-buf
	(let ((process (get-buffer-process cmd-buf)))
	  (if (and process (eq 'run (process-status process)))
	      (with-current-buffer cmd-buf
		(realgud-command "set annotate 1" nil nil nil)
                ;; In gdb, when setting breakpoint on function, we want it to produce an absolute
                ;; path, so set filename-display to absolute. We want:
                ;;   (gdb) b functionName
                ;;   Breakpoint 1 at 0x7fff607e4dd6: file /abs/path/to/file.cpp, line 273.
                ;; Without this, gdb will display the path supplied when the code was compiled, i.e.
                ;; if a relative path is supplied to gcc, gdb will display the relative path
                ;; tripping up realgud, causing it to ask if you want to blacklist the file.
                (realgud-command "set filename-display absolute" nil nil nil)
		)))
      )
    ))

(provide-me "realgud-")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

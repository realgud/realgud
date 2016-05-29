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

(require 'load-relative)
(require-relative-list  '("send" "core") "realgud-")
(require-relative-list  '("buffer/command") "realgud-buffer-")
(require-relative-list  '("buffer/source") "realgud-buffer-")

(declare-function buffer-killed? 'helper)
(declare-function realgud-cmdbuf-info-in-srcbuf?=   'realgud-buffer-command)
(declare-function realgud-cmdbuf?      'realgud-buffer-command)
(declare-function realgud-command      'realgud-send)
(declare-function realgud-get-cmdbuf   'realgud-buffer-helper)
(declare-function realgud-get-command  'realgud-buffer-command)
(declare-function realgud-get-bpnum-from-line-num 'realgud-buffer-source)

(declare-function realgud:terminate 'realgud-core)
(declare-function realgud:terminate-srcbuf 'realdgud-core)

(defcustom realgud-safe-mode t
  "Confirm certain commands before running them.
Similar to GDB's “set confirm”."
  :type 'boolean
  :group 'realgud)

(defun realgud:prompt-if-safe-mode(message)
  "Ask use to confirm current command if in safe mode.
Use MESSAGE plus a space as the prompt string.  Do not confirm
when command was run from a menu."
  (if (and realgud-safe-mode
           last-nonmenu-event
           (not (equal last-nonmenu-event '(tool-bar))))
      (when (y-or-n-p (concat message " "))
        (run-with-timer
         0 nil #'message
         "Customize `realgud-safe-mode' to disable confirmation prompts.")
        t)
    t))

(defun realgud:cmd-run-command(arg cmd-name default-cmd-template
                                   &optional no-record? frame-switch?
                                   realgud-prompts?)
  "Run debugger command CMD-NAME using DEFAULT-CMD-TEMPLATE
if none has been set in the command hash. If key is given we'll set
a shortcut for that key."
  (let ((buffer (current-buffer))
	(cmdbuf (realgud-get-cmdbuf))
	(cmd-hash)
	(cmd)
	)
    (with-current-buffer-safe cmdbuf
      (realgud-cmdbuf-info-in-srcbuf?= (not (realgud-cmdbuf? buffer)))
      (setq cmd-hash (realgud-cmdbuf-info-cmd-hash realgud-cmdbuf-info))
      (unless (and cmd-hash (setq cmd (gethash cmd-name cmd-hash)))
	(setq cmd default-cmd-template))
      )
    (if (equal cmd "*not-implemented*")
	(message "Command %s is not implemented for this debugger" cmd-name)
      ;; else
      (progn
	(realgud-command cmd arg no-record? frame-switch? realgud-prompts?)
	;; FIXME: Figure out how to update the position if the source
	;; buffer is displayed.
	;; (if frame-switch?
	;; 	(let* ((src-buffer (realgud-get-srcbuf-from-cmdbuf cmdbuf))
	;; 	       (src-window (get-buffer-window src-buffer))
	;; 	       ))
	;; 	  (with-selected-window src-window
	;; 	    (message "recentering...")
	;; 	    (realgud-recenter-arrow)
	;; 	  ))
        )
      )
    ))

(defun realgud:cmd-remap (arg cmd-name default-cmd-template
                              &optional key no-record? frame-switch?
                              realgud-prompts?)
  "Compatibility alias for `realgud:cmd-run-command'.
ARG, CMD-NAME, DEFAULT-CMD-TEMPLATE are as in `realgud:cmd-run-command'.
KEY is ignored.  NO-RECORD?, FRAME-SWITCH?, REALGUD-PROMPTS? are
as in `realgud:cmd-run-command'."
  (realgud:cmd-run-command arg cmd-name default-cmd-template
                   no-record? frame-switch?
                   realgud-prompts?))

(make-obsolete 'realgud:cmd-remap 'realgud:cmd-run-command "1.3.1")

(defun realgud:cmd-backtrace(arg)
  "Show the current call stack"
  (interactive "p")
  (realgud:cmd-run-command arg "backtrace" "backtrace")
  )

(defun realgud:cmd-break(arg)
  "Set a breakpoint at the current line"
  (interactive "p")
  (realgud:cmd-run-command arg "break" "break %X:%l"))

(defun realgud:cmd-clear(line-num)
  "Delete breakpoint at the current line"
  (interactive "p")
  (realgud:cmd-run-command line-num "clear" "clear %l"))

(defun realgud:cmd-continue(&optional arg)
    "Continue execution.
With prefix argument ARG, prompt for argument to \"continue\"
command.  In safe mode (or with prefix arg), confirm before
running."
    (interactive (when (consp current-prefix-arg)
                   (list (read-string "Continue args: " nil nil nil t))))
    (when (or arg (realgud:prompt-if-safe-mode
                   "Continue to next breakpoint?"))
      (realgud:cmd-run-command arg "continue" "continue")))

(defun realgud:bpnum-on-current-line()
  "Return number of one breakpoint on current line, if any.
If none is found, return nil."
  (realgud-get-bpnum-from-line-num (line-number-at-pos)))

(defun realgud:bpnum-from-prefix-arg()
  "Return number of one breakpoint on current line, if any.
If none is found, or if `current-prefix-arg' is a cons (i.e. a
C-u prefix arg), ask user for a breakpoint number.  If
`current-prefix-arg' is a number (i.e. a numeric prefix arg),
return it unmodified."
  (let ((must-prompt (consp current-prefix-arg))
        (current-bp (realgud:bpnum-on-current-line)))
    (list
     (if (numberp current-prefix-arg)
         current-prefix-arg
       (or (and (not must-prompt) current-bp)
           (read-number "Breakpoint number: " current-bp))))))

(defun realgud:cmd-delete(bpnum)
    "Delete breakpoint by number.
Interactively, find breakpoint on current line, if any.  With
numeric prefix argument, delete breakpoint with that number
instead.  With prefix argument (C-u), or when no breakpoint can
be found on the current line, prompt for a breakpoint number."
    (interactive (realgud:bpnum-from-prefix-arg))
    (realgud:cmd-run-command bpnum "delete" "delete %p"))

(defun realgud:cmd-disable(bpnum)
    "Disable breakpoint BPNUM.
Interactively, find breakpoint on current line, if any.  With
numeric prefix argument, disable breakpoint with that number
instead.  With prefix argument (C-u), or when no breakpoint can
be found on the current line, prompt for a breakpoint number."
    (interactive (realgud:bpnum-from-prefix-arg))
    (realgud:cmd-run-command bpnum "disable" "disable %p"))

(defun realgud:cmd-enable(bpnum)
    "Enable breakpoint BPNUM.
Interactively, find breakpoint on current line, if any.  With
numeric prefix argument, enable breakpoint with that number
instead.  With prefix argument (C-u), or when no breakpoint can
be found on the current line, prompt for a breakpoint number."
    (interactive (realgud:bpnum-from-prefix-arg))
    (realgud:cmd-run-command bpnum "enable" "enable %p"))

(defun realgud:cmd-eval(arg)
    "Evaluate an expression."
    (interactive "MEval expesssion: ")
    (realgud:cmd-run-command arg "eval" "eval %s")
)

(defun realgud:cmd-eval-region(start end)
    "Evaluate current region."
    (interactive "r")
    (let ((text (buffer-substring-no-properties start end)))
      (realgud:cmd-run-command text "eval" "eval %s")))

(defun realgud:cmd-eval-dwim()
  "Eval the current region if active; otherwise, prompt."
  (interactive)
  (call-interactively (if (region-active-p)
                          #'realgud:cmd-eval-region
                        #'realgud:cmd-eval)))

(defun realgud:cmd-finish(&optional arg)
    "Run until the completion of the current stack frame.

This command is often referred to as 'step out' as opposed to
'step over' or 'step into'."
    (interactive "p")
    (realgud:cmd-run-command arg "finish" "finish")
)

(defun realgud:cmd-frame(arg)
    "Change the current frame number to the value of the numeric argument.
If no argument specified use 0 or the most recent frame."
    (interactive "p")
    (realgud:cmd-run-command arg "frame" "frame %p" t t)
)

(defun realgud:cmd-kill(arg)
  "kill debugger process"
  (interactive "p")
  (realgud:cmd-run-command arg "kill" "kill" nil nil t)
  )

(defun realgud:cmd-newer-frame(&optional arg)
    "Move the current frame to a newer (more recent) frame.
With a numeric argument move that many levels forward."
    (interactive "p")
    (realgud:cmd-run-command arg "down" "down %p" "<" t t)
)

(defun realgud:cmd-next(&optional arg)
    "Step one source line at current call level.

With a numeric argument, step that many times. This command is
often referred to as 'step through' as opposed to 'step into' or
'step out'.

The definition of 'next' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped."
    (interactive "p")
    (realgud:cmd-run-command arg "next" "next %p")
)

(defun realgud:cmd-next-no-arg(&optional arg)
    "Step one source line at current call level.

The definition of 'next' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped."
    (interactive)
    (realgud:cmd-run-command nil "next" "next")
)

(defun realgud:cmd-older-frame(&optional arg)
  "Move the current frame to an older (less recent) frame.
With a numeric argument move that many levels back."
    (interactive "p")
    (realgud:cmd-run-command arg "up" "up %p" t t)
)

(defun realgud:cmd-repeat-last(&optional arg)
    "Repeat the last command (or generally what <enter> does."
    (interactive "")
    (realgud:cmd-run-command arg "repeat-last" "\n" t nil t)
)

(defun realgud:cmd-restart(&optional arg)
    "Restart execution."
    (interactive "")
    (realgud:cmd-run-command arg "restart" "run" t nil t)
)

(defun realgud:cmd-shell(&optional arg)
    "Drop to a shell."
    (interactive "")
    (realgud:cmd-run-command arg "shell" "shell")
)

(defun realgud:cmd-step(&optional arg)
    "Step one source line.

With a numeric argument, step that many times.
This command is often referred to as 'step into' as opposed to
'step over' or 'step out'.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped."
    (interactive "p")
    (realgud:cmd-run-command arg "step" "step %p")
)

(defun realgud:cmd-step-no-arg()
    "Step one source line.

The definition of 'step' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped."
    (interactive)
    (realgud:cmd-run-command nil "step" "step")
)

(defun realgud:cmd-terminate ()
  "Gently terminate source and command buffers without possibly
issuing a command to the underlying debuger.  Use this if the
underlying debugger has died or you want to get out of all
shortkey modes in the source window and possibly restart
debugging after editing source."
  (interactive)
  (realgud:terminate (current-buffer))
)

(defun realgud:cmd-until(&optional arg)
    "Run until the completion of the current stack frame.

Continue until the current line. In some cases this is really
two commands - setting a temporary breakpoint on the line and
continuing execution."
    (interactive "p")
    (realgud:cmd-run-command arg "until" "until")
)

(defun realgud:cmd-quit (&optional arg)
  "Gently terminate execution of the debugged program."
  (interactive "p")
  (let ((buffer (current-buffer))
	(cmdbuf (realgud-get-cmdbuf))
	(cmd-hash)
	(cmd)
	)
    (if cmdbuf
	(progn
	  (with-current-buffer cmdbuf
	    (realgud-cmdbuf-info-in-srcbuf?= (not (realgud-cmdbuf? buffer)))
	    (setq cmd-hash (realgud-cmdbuf-info-cmd-hash realgud-cmdbuf-info))
	    (unless (and cmd-hash (setq cmd (gethash "quit" cmd-hash)))
	      (setq cmd "quit"))
	    )
	  (realgud-command cmd arg 't)
	  (if cmdbuf (realgud:terminate cmdbuf))
	  )
      ; else
      (realgud:terminate-srcbuf buffer)
      )
    )
  )

(provide-me "realgud-")

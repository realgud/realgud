;; Copyright (C) 2015-2017, 2019 Free Software Foundation, Inc

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
(require 'thingatpt)
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

(defun realgud:cmd--line-number-from-prefix-arg ()
  "Guess or read a line number based on prefix arg.
Returns (nil) for current line, and a list whose car is the line
number otherwise."
  (cond
   ((numberp current-prefix-arg)
    current-prefix-arg)
   ((consp current-prefix-arg)
    (let* ((min-line (save-excursion
                       (goto-char (point-min))
                       (line-number-at-pos)))
           (max-line (save-excursion
                       (goto-char (point-max))
                       (line-number-at-pos)))
           (prompt (format "Line number (%d..%d)? " min-line max-line))
           (picked-line 0))
      (while (not (<= min-line picked-line max-line))
        (setq picked-line (read-number prompt)))
      (list picked-line)))))

(defmacro realgud:cmd--with-line-override (line &rest body)
  "Run BODY with %l format specifier bound to LINE.
This is needed because going to LINE explicitly would interfere
with other motion initiated by debugger messages."
  (declare (indent 1)
           (debug t))
  (let ((line-var (make-symbol "--line--")))
    `(let* ((,line-var ,line)
            (realgud-expand-format-overrides
             (cons (cons ?l (and ,line-var (number-to-string ,line-var)))
                   realgud-expand-format-overrides)))
       ,@body)))

(defconst realgud-cmd:default-hash
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "backtrace"         "backtrace" hash)
    (puthash "break"             "break %X:%l" hash)
    (puthash "break-fn"          "break %s" hash)
    (puthash "clear"             "clear %l" hash)
    (puthash "continue"          "continue" hash)
    (puthash "delete"            "delete %p" hash)
    (puthash "delete-all"        "delete" hash)
    (puthash "disable"           "disable %p" hash)
    (puthash "disable-all"       "disable" hash)
    (puthash "down"              "down %p" hash)
    (puthash "enable"            "enable %p" hash)
    (puthash "enable-all"        "enable" hash)
    (puthash "eval"              "eval %s" hash)
    (puthash "finish"            "finish" hash)
    (puthash "frame"             "frame %p" hash)
    (puthash "help"              "help" hash)
    (puthash "info-breakpoints"  "info breakpoints" hash)
    (puthash "jump"              "jump %l" hash)
    (puthash "kill"              "kill" hash)
    (puthash "next"              "next %p" hash)
    (puthash "repeat-last"       "\n" hash)
    (puthash "restart"           "run" hash)
    (puthash "shell"             "shell" hash)
    (puthash "step"              "step %p" hash)
    (puthash "tbreak"            "tbreak %X:%l" hash)
    (puthash "until"             "until" hash)
    (puthash "until-here"        "until %l" hash)
    (puthash "up"                "up %p" hash)
    hash)
  "Default hash of command name → debugger command.
This is used as a fallback when the debugger-specific command
hash does not specify a custom debugger command. The keys of the
hash contain all the debugger commands we know about.

If a value is *not-implemented*, then this command is not available
in a particular debugger.
")

(defun realgud:cmd-run-command(arg cmd-name &optional
                                   default-cmd-template no-record?
                                   frame-switch? realgud-prompts?)
  "Run debugger command CMD-NAME.
If CMD-NAME isn't set in the command buffer's command hash, use
DEFAULT-CMD-TEMPLATE and fall back to looking CMD-NAME up in
`realgud:cmd-get-cmd-hash'."
  (let* ((buffer (current-buffer))
         (cmdbuf (realgud-get-cmdbuf))
         (cmd-hash (with-current-buffer-safe cmdbuf
                     (realgud-cmdbuf-info-cmd-hash realgud-cmdbuf-info)))
         (cmd (or (and (hash-table-p cmd-hash)
                       (gethash cmd-name cmd-hash))
                  default-cmd-template
                  (gethash cmd-name realgud-cmd:default-hash))))
    (if (or (null cmd) (equal cmd "*not-implemented*"))
	(message "Command %s is not implemented for this debugger" cmd-name)
      (progn
        ;; Set flag to know which buffer to jump back to
        (with-current-buffer-safe cmdbuf
          (realgud-cmdbuf-info-in-srcbuf?= (not (realgud-cmdbuf? buffer))))
        ;; Run actual command
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
  "Show the current call stack."
  (interactive "p")
  (realgud:cmd-run-command arg "backtrace")
  )

(defun realgud:cmd-break (&optional line-number)
  "Set a breakpoint at the current line.
With prefix argument LINE-NUMBER, prompt for line number."
  (interactive (realgud:cmd--line-number-from-prefix-arg))
  ;; Note a file name may be picked up inside realgud:cmd-run-runcmd's stored pattern
  (realgud:cmd--with-line-override line-number
                                   (realgud:cmd-run-command line-number "break")))

(defun realgud:cmd-tbreak (&optional line-number)
  "Set a temporary breakpoint at the current line.
With prefix argument LINE-NUMBER, prompt for line number."
  (interactive (realgud:cmd--line-number-from-prefix-arg))
  (realgud:cmd--with-line-override line-number
                                   (realgud:cmd-run-command line-number "tbreak")))

(defun realgud:cmd-until-here (&optional line-number)
  "Continue until the current line.
With prefix argument LINE-NUMBER, prompt for line number."
  (interactive (realgud:cmd--line-number-from-prefix-arg))
  (realgud:cmd--with-line-override line-number
                                   (realgud:cmd-run-command line-number "until-here")))

(defun realgud:cmd-clear(&optional line-number)
  "Delete breakpoint at the current line.
With prefix argument LINE-NUMBER, prompt for line number."
  (interactive (realgud:cmd--line-number-from-prefix-arg))
  (realgud:cmd--with-line-override line-number
                                   (realgud:cmd-run-command line-number "clear")))

(defun realgud:cmd-jump(&optional line-number)
  "Jump to current line.
With prefix argument LINE-NUMBER, prompt for line number."
  (interactive (realgud:cmd--line-number-from-prefix-arg))
  (realgud:cmd--with-line-override line-number
    (realgud:cmd-run-command (line-number-at-pos) "jump")))

(defun realgud:cmd-continue(&optional arg)
    "Continue execution.
With prefix argument ARG, prompt for argument to \"continue\"
command.  In safe mode (or with prefix arg), confirm before
running."
    (interactive (when (consp current-prefix-arg)
                   (list (read-string "Continue args: " nil nil nil t))))
    (when (or arg (realgud:prompt-if-safe-mode
                   "Continue to next breakpoint?"))
      (realgud:cmd-run-command arg "continue")))

(defun realgud-get-bp-list()
  "Return breakpoint numbers as a list of strings. This can be used for
example in a completing read."
  (with-current-buffer (realgud-get-cmdbuf)
    ;; Remove duplicates doesn't seem to work on strings so
    ;; we need a separate mapcar outside to stringify
    ;; Also note that lldb breakpoint numbers can be dotted like
    ;; 5.1.
    (mapcar (lambda (num) (format "%s" num))
	    (cl-remove-duplicates
	     (mapcar (lambda(loc) (realgud-loc-num loc))
		     (realgud-cmdbuf-info-bp-list realgud-cmdbuf-info))))))

(defun realgud:bpnum-on-current-line()
  "Return number of one breakpoint on current line, if any.
If none is found, return nil."
  (realgud-get-bpnum-from-line-num (line-number-at-pos)))

(defun realgud:bpnum-from-prefix-arg(action-verb)
  "Return number of one breakpoint on current line, if any.
If none is found, or if `current-prefix-arg' is a cons (i.e. a
C-u prefix arg), ask user for a breakpoint number.  If
`current-prefix-arg' is a number (i.e. a numeric prefix arg),
return it unmodified."
  (let ((must-prompt (consp current-prefix-arg))
	(cmd-buffer (realgud-get-cmdbuf))
        (current-bp (realgud:bpnum-on-current-line)))
      (list
       (if (numberp current-prefix-arg)
	   current-prefix-arg
	 (or (and (not must-prompt) current-bp)
	     (string-to-number (completing-read (format "%s breakpoint number: " action-verb)
			      (realgud-get-bp-list)
			      nil nil current-bp)))))))

(defun realgud:cmd-delete(bpnum)
    "Delete breakpoint by number.
Interactively, find breakpoint on current line, if any.  With
numeric prefix argument, delete breakpoint with that number
instead.  With prefix argument (C-u), or when no breakpoint can
be found on the current line, prompt for a breakpoint number."
    (interactive (realgud:bpnum-from-prefix-arg "Delete"))
    (realgud:cmd-run-command bpnum "delete"))

(defun realgud:cmd-disable(bpnum)
    "Disable breakpoint BPNUM.
Interactively, find breakpoint on current line, if any.  With
numeric prefix argument, disable breakpoint with that number
instead.  With prefix argument (C-u), or when no breakpoint can
be found on the current line, prompt for a breakpoint number."
    (interactive (realgud:bpnum-from-prefix-arg "Disable"))
    (realgud:cmd-run-command bpnum "disable"))

(defun realgud:cmd-enable(bpnum)
    "Enable breakpoint BPNUM.
Interactively, find breakpoint on current line, if any.  With
numeric prefix argument, enable breakpoint with that number
instead.  With prefix argument (C-u), or when no breakpoint can
be found on the current line, prompt for a breakpoint number."
    (interactive (realgud:bpnum-from-prefix-arg "Enable"))
    (realgud:cmd-run-command bpnum "enable"))

(defun realgud-cmds--add-remove-bp (pos)
  "Add or delete breakpoint at POS."
  (save-excursion
    (goto-char pos)
    (let ((existing-bp-num (realgud:bpnum-on-current-line)))
      (if existing-bp-num
          (realgud:cmd-delete existing-bp-num)
        (realgud:cmd-break)))))

(defun realgud-cmds--mouse-add-remove-bp (event)
  "Add or delete breakpoint on line pointed to by EVENT.
EVENT should be a mouse click on the left fringe or margin."
  (interactive "e")
  (let* ((posn (event-end event))
         (pos (posn-point posn)))
    (when (numberp pos)
      (with-current-buffer (window-buffer (posn-window posn))
        (realgud-cmds--add-remove-bp pos)))))

(defun realgud:cmd-eval(arg)
    "Evaluate an expression."
    (interactive "MEval expression: ")
    (realgud:cmd-run-command arg "eval")
)

(defun realgud:cmd-eval-region(start end)
    "Evaluate current region."
    (interactive "r")
    (let ((text (buffer-substring-no-properties start end)))
      (realgud:cmd-run-command text "eval")))

(defun realgud:cmd-eval-dwim()
  "Eval the current region if active; otherwise, prompt."
  (interactive)
  (call-interactively (if (region-active-p)
                          #'realgud:cmd-eval-region
                        #'realgud:cmd-eval)))

(defun realgud:cmd-eval-at-point()
  "Eval symbol under point."
  (interactive)

  (beginning-of-thing 'symbol)
  (set-mark-command 'nil)
  (end-of-thing 'symbol)

  (realgud:cmd-run-command
   (read-string "Eval: " (thing-at-point 'symbol))
   "eval"))

(defun realgud:cmd-finish(&optional arg)
    "Run until the completion of the current stack frame.

This command is often referred to as 'step out' as opposed to
'step over' or 'step into'."
    (interactive "p")
    (realgud:cmd-run-command arg "finish")
)

(defun realgud:cmd-frame(arg)
    "Change the current frame number to the value of the numeric argument.
If no argument specified use 0 or the most recent frame."
    (interactive "p")
    (realgud:cmd-run-command arg "frame" nil t t)
)

(defun realgud:cmd-info-breakpoints()
  "Show all list of all breakpoints."
  (interactive "")
  (realgud:cmd-run-command nil "info-breakpoints")
  )

(defun realgud:cmd-info-locals-name-list()
  "Get list of locals value's names"
  (interactive "")
  (realgud:cmd-run-command nil "info-locals-names-list")
  )

(defun realgud:cmd-info-value(var-name)
  "Get value of single variable"
  (interactive "sVariable name: ")
  (realgud:cmd-run-command var-name "info-value")
  )

(defun realgud:cmd-info-type(var-name)
  "Get type of single variable"
  (interactive "sVariable name: ")
  (realgud:cmd-run-command var-name "info-type")
  )

(defun realgud:cmd-kill()
  "Kill debugger process."
  (interactive)
  (realgud:cmd-run-command nil "kill" nil nil nil t))

(defun realgud:cmd-newer-frame(&optional arg)
    "Move the current frame to a newer (more recent) frame.
With a numeric argument move that many levels forward."
    (interactive "p")
    (realgud:cmd-run-command arg "down" nil t t)
)

(defun realgud:cmd-next(&optional count)
    "Step one source line at current call level.

With numeric argument COUNT, step that many times. This command is
often referred to as `step through' as opposed to `step into' or
`step out'.

The definition of `next' is debugger specific, so see the
documentation of your debugger for a more complete definition of
what is getting stepped."
    (interactive "p")
    (realgud:cmd-run-command count "next"))

(defun realgud:cmd-next-no-arg()
  "Step one source line at current call level.

The definition of 'next' is debugger specific so, see the
debugger documentation for a more complete definition of what is
getting stepped."
    (interactive)
    (realgud:cmd-next))

(defun realgud:cmd-older-frame(&optional arg)
  "Move the current frame to an older (less recent) frame.
With a numeric argument move that many levels back."
    (interactive "p")
    (realgud:cmd-run-command arg "up" nil t t)
)

(defun realgud:cmd-repeat-last()
  "Repeat the last command (or generally what <enter> does."
  (interactive)
  (realgud:cmd-run-command nil "repeat-last" nil t nil t))

(defun realgud:cmd-restart()
  "Restart execution."
  (interactive)
  (if (realgud:prompt-if-safe-mode
		 "Restart program?")
      (realgud:cmd-run-command nil "restart" nil t nil t)))

(defun realgud:cmd-shell()
  "Drop to a shell."
  (interactive)
  (realgud:cmd-run-command nil "shell"))

(defun realgud:cmd-step(&optional count)
    "Step one source line.

With a numeric prefix argument COUNT, step that many times.
This command is often referred to as `step into' as opposed to
`step over' or `step out'.

The definition of `step' is debugger specific, so see the
documentation of your debugger for a more complete definition of
what is getting stepped."
    (interactive "p")
    (realgud:cmd-run-command count "step"))

(defun realgud:cmd-step-no-arg()
    "Step one source line.

The definition of `step' is debugger specific, so see the
documentation of your debugger for a more complete definition of
what is getting stepped."
    (interactive)
    (realgud:cmd-step))

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
    (realgud:cmd-run-command arg "until")
)

(defun realgud:cmd-quit (&optional arg)
  "Gently terminate execution of the debugged program."
  (interactive "p")
  (if (realgud:prompt-if-safe-mode
		 "Quit debugger?")
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
          (realgud-command cmd arg t)
	  (if cmdbuf (realgud:terminate cmdbuf))
	  )
	  ;; else
	  (realgud:terminate-srcbuf buffer)
	  )
	)
    ))

(provide-me "realgud-")

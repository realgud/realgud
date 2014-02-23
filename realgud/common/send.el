;;; Copyright (C) 2010, 2014 Rocky Bernstein <rocky@gnu.org>
(require 'comint)
(require 'load-relative)
(require-relative-list '("window") "realgud-")
(require-relative-list '("buffer/helper") "realgud-buffer-")

(declare-function realgud-get-cmdbuf        'realgud-buffer-helper)
(declare-function comint-goto-process-mark  'comint)
(declare-function comint-send-input         'comint)

(defun realgud-send-command-comint (process command-str)
  "Assume we are in a comint buffer. Insert COMMAND-STR and
send that input onto the process."
  (if (eq 'run (process-status process))
      (progn
	(comint-goto-process-mark)
	(setq comint-last-output-start
	      (setq realgud-last-output-start (point-marker)))
	(insert command-str)
	(comint-send-input)
	)
    ;; else
    (message "Process %s not in `run' state; not issuing %s"
	     process command-str)
    )
  )

(defalias 'comint-output-filter-orig
  (symbol-function 'comint-output-filter))

(defvar realgud-last-output-start)
(defun fake-output-filter(process string)
  (with-current-buffer (get-buffer-create "*realgud-process-output-temp*")
    (goto-char (point-max))
    (set (make-local-variable 'realgud-last-output-start)
	 (point-marker))
    (insert (concat "\n" string))
    (goto-char (point-max))))

(defun realgud-send-command-process (process command-str)
  "Sent Invoke the debugger COMMAND adding that command and the
results into the command buffer."
  (fset 'comint-output-filter (symbol-function 'fake-output-filter))
  (apply comint-input-sender (list process command-str))
  (sit-for 0.25) ;; FIXME with something better
  (fset 'comint-output-filter (symbol-function 'comint-output-filter-orig))
  )

;; Here are some other possibilities for functions.
;; Comint-specific: doesn't insert input into the buffer which is
;; what gud-call does.
;;   (apply comint-input-sender (list proc command))
;;
;; Works on any process-oriented buffer, not just comint.
;;   (process-send-string (get-buffer-process (current-buffer))
;;                        (concat command "\n"))


(defun realgud-send-command (command &optional opt-send-fn opt-buffer)
  "Invoke the debugger COMMAND adding that command and the
results into the command buffer."
  (let* ((cmdbuf (realgud-get-cmdbuf opt-buffer))
	 (send-command-fn (or opt-send-fn (function realgud-send-command-comint)))
	 )
    (if cmdbuf
	(with-current-buffer cmdbuf
	  (let ((process (get-buffer-process cmdbuf)))
	    (unless process
	      (realgud-cmdbuf-info-in-debugger?= nil)
	      (error "Command process buffer is not running")
	      )
	    (if (realgud-sget 'cmdbuf-info 'in-debugger?)
		(funcall send-command-fn process command)
	      (error "Command buffer doesn't think a debugger is running")
	      )
	    ))
      (error "Can't find command process buffer")
      )))

(defun realgud-send-command-invisible (command-str)
  (realgud-send-command command-str (function realgud-send-command-process)))


(defun realgud-expand-format (fmt-str &optional opt-str opt-buffer)
  "Expands commands format characters inside FMT-STRING using values
from the debugging session. OPT-STR is an optional string.
Some %-escapes in the string arguments are expanded. These are:

  %f -- Name without directory of current source file.
  %F -- Name without directory or extension of current source file.
  %x -- Name of current source file.
  %X -- Expanded name of current source file.
  %d -- Directory of current source file.
  %l -- Number of current source line.
  %p -- Numeric prefix argument converted to a string
        If no prefix argument %p is the null string.
  %s -- value of opt-str.

"
  (let* ((buffer (or opt-buffer (current-buffer)))
	 (srcbuf (realgud-get-srcbuf buffer))
	 (src-file-name (and srcbuf (buffer-file-name srcbuf)))
	 result)
    (while (and fmt-str
		(let ((case-fold-search nil))
		  (string-match "\\([^%]*\\)%\\([dfFlpxXs]\\)" fmt-str)))
      (let* ((key-str (match-string 2 fmt-str))
	     (key (string-to-char key-str)))
	(setq result
	      (concat
	       result (match-string 1 fmt-str)
	       (cond
		((eq key ?d)
		 (or (and src-file-name
			  (file-name-directory src-file-name))
		     "*source-file-not-found-for-%d"))
		((eq key ?f)
		 (or (and src-file-name
			  (file-name-nondirectory src-file-name))
		     "*source-file-not-found-for-%f*"))
		((eq key ?F)
		 (or (and src-file-name
			  (file-name-sans-extension
			   (file-name-nondirectory src-file-name)))
		     "*source-file-not-found-for-%F"))
		((eq key ?l)
		 (if srcbuf
		     (with-current-buffer srcbuf
		       (int-to-string
			(save-restriction
			  (widen)
			  (+ (count-lines (point-min) (point))
			     (if (bolp) 1 0)))))
		   "source-buffer-not-found-for-%l"))
		((eq key ?x)
		 (or (and src-file-name src-file-name)
		     "*source-file-not-found-for-%x"))
		((eq key ?X)
		 (or (and src-file-name (expand-file-name src-file-name))
		     "*source-file-not-found-for-%X"))
		;; ((eq key ?e)
		;;  (gud-find-expr))
		;; ((eq key ?a)
		;;  (gud-read-address))
		;; ((eq key ?c)
		;;   (gud-find-class srcbuf))
		((eq key ?p) (if opt-str (int-to-string opt-str) ""))
		((eq key ?s) opt-str)
		(t key)))))
      (setq fmt-str (substring fmt-str (match-end 2))))
    ;; There might be text left in FMT-STR when the loop ends.
    (concat result fmt-str)))

(defun realgud-command (fmt &optional arg no-record? frame-switch? realgud-prompts?)
  "Sends a command to the process associated with the command
buffer of the current buffer. A bit of checking is done before
sending the command to make sure that we can find a command
buffer, and that it has a running process associated with it.

FMT is a string which may contain format characters that are
expanded. See `realgud-expand-format' for a list of the format
characters and their meanings.

If NO-RECORD? is set, the command won't be recorded in the
position history. This is often done in status and information
gathering or frame setting commands and is generally *not* done
in commands that continue execution.

If FRAME-SWITCH? is set, the fringe overlay array icon is set to
indicate the depth of the frame.

If REALGUD-PROMPTS? is set, then then issuing the command will cause a
debugger prompt.
"
  (interactive "sCommand (may contain format chars): ")
  (let* ((command-str (realgud-expand-format fmt arg))
	 (cmd-buff (realgud-get-cmdbuf))
	 )
    (unless cmd-buff
      (error "Can't find command buffer for buffer %s" (current-buffer)))

    ;; Display the expanded command in the message area unless the
    ;; current buffer is the command buffer.
    (unless (realgud-cmdbuf?)
      (message "Command: %s" command-str))

    (with-current-buffer cmd-buff
      (let* ((process (get-buffer-process cmd-buff))
	     (last-output-end (point-marker))
	     (in-srcbuf? (realgud-sget 'cmdbuf-info 'in-srcbuf?))
	     )
	(unless process
	  (error "Can't find process for command buffer %s" cmd-buff))
	(unless (eq 'run (process-status process))
	  (error "Process %s isn't running; status %s" process
		 (process-status process)))

	(realgud-cmdbuf-info-no-record?= no-record?)
	(realgud-cmdbuf-info-frame-switch?= frame-switch?)

	;; Down the line we may handle prompting in a more
	;; sophisticated way. But for now, we handle this by forcing
	;; display of the command buffer.
	(if realgud-prompts? (realgud-window-cmd-undisturb-src nil 't))

	(realgud-send-command command-str (function realgud-send-command-comint))

	;; Wait for the process-mark to change before changing variables
	;; that effect the hook processing.
	(while (and (eq 'run (process-status process))
		    (equal last-output-end (process-mark process)))
	  (sit-for 0))

	;; Reset temporary hook-processing variables to their default state.
	(realgud-cmdbuf-info-no-record?= nil)
	(realgud-cmdbuf-info-frame-switch?=  nil)
	))))

(provide-me "realgud-")

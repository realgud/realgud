;; Copyright (C) 2010-2016, 2020 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; (require 'term)

(require 'comint)
(require 'load-relative)
(require 'loc-changes)
(require-relative-list '("fringe" "helper" "lang" "reset")
		       "realgud-")
(require-relative-list '("buffer/command" "buffer/source") "realgud-buffer-")

(declare-function comint-exec  'comint)
(declare-function comint-mode  'comint)
(declare-function realgud-bp-remove-icons             'realgud-bp)
(declare-function realgud:suggest-file-from-buffer    'realgud-lang)
(declare-function realgud-cmdbuf-args=                'realgud-buffer-command)
(declare-function realgud-cmdbuf-command-string       'realgud-buffer-command)
(declare-function realgud-cmdbuf-debugger-name        'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-bp-list=        'realgud-buffer-command)
(declare-function realgud-locals-terminate            'realgud-buffer-locals)
(declare-function realgud-cmdbuf-info-in-debugger?=   'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-starting-directory= 'realgud-buffer-command)
(declare-function realgud-cmdbuf-mode-line-update     'realgud-buffer-command)
(declare-function realgud-cmdbuf?                     'realgud-helper)
(declare-function realgud-command-string              'realgud-buffer-command)
(declare-function realgud-fringe-erase-history-arrows 'realgud-buffer-command)
(declare-function realgud-get-cmdbuf                  'realgud-helper)
(declare-function realgud:reset                       'realgud-reset)
(declare-function realgud-short-key-mode-setup        'realgud-shortkey)
(declare-function realgud-srcbuf-command-string       'realgud-buffer-source)
(declare-function realgud-srcbuf-debugger-name        'realgud-buffer-source)
(declare-function realgud-srcbuf-init                 'realgud-buffer-source)
(declare-function realgud-srcbuf?                     'realgud-buffer-source)
(declare-function realgud-suggest-lang-file           'realgud-lang)

(defvar realgud-srcbuf-info)
(defvar starting-directory)

(defun realgud:expand-file-name-if-exists (filename)
  "Return FILENAME expanded using `expand-file-name' if that name exists.
Otherwise, just return FILENAME."
  (let* ((expanded-filename (expand-file-name filename))
	 (result (cond ((file-exists-p expanded-filename)
			  expanded-filename)
			 ('t filename))))
    result)
)

(defun realgud-suggest-invocation
  (debugger-name _minibuffer-history lang-str lang-ext-regexp
		 &optional last-resort)
  "Suggest a debugger command invocation. If the current buffer
is a source file or process buffer previously set, then use the
value of that the command invocations found by buffer-local
variables.  Otherwise, we try to find a suitable program file
using LANG-STR and LANG-EXT-REGEXP."
  (let* ((buf (current-buffer))
	 (filename)
	 (cmd-str-cmdbuf (realgud-cmdbuf-command-string buf))
	 )
    (cond
     ((and cmd-str-cmdbuf (equal debugger-name (realgud-cmdbuf-debugger-name buf)))
      cmd-str-cmdbuf)
     ((setq filename (realgud:suggest-file-from-buffer lang-str))
      (concat debugger-name " " (shell-quote-argument filename)))
     (t (concat debugger-name " "
                (shell-quote-argument
                 (realgud-suggest-lang-file lang-str lang-ext-regexp last-resort))))
     )))

(defun realgud-query-cmdline
  (suggest-invocation-fn
   minibuffer-local-map
   minibuffer-history
   &optional opt-debugger)
  "Prompt for a debugger command invocation to run.
Analogous to `gud-query-cmdline'.

If you happen to be in a debugger process buffer, the last command invocation
for that first one suggested. Failing that, some amount of guessing is done
to find a suitable file via SUGGEST-INVOCATION-FN.

We also set filename completion and use a history of the prior
dbgr invocations "
  (let ((debugger (or opt-debugger
		   (realgud-sget 'srcbuf-info 'debugger-name))))
    (read-shell-command
     (format "Run %s (like this): " debugger)  ;; prompt string
     (funcall suggest-invocation-fn debugger)  ;; initial value
     minibuffer-history                        ;; history variable
     )))

(defun realgud-parse-command-arg (args two-args opt-two-args)
  "Return a cons node where the car is a list containing the
entire first option and the cdr is the remaining arguments from ARGS.

We determine if an option has length one or two using the lists
TWO-ARGS and OPT-TWO-ARGS. Both of these are list of 'options',
that is strings without the leading dash. TWO-ARGS takes a
mandatory additional argument. OPT-TWO-ARGS might take two
arguments. The rule for an optional argument that we use is if
the next parameter starts with a dash ('-'), it is not part of
the preceeding parameter when that parameter is optional.

NOTE: we don't check whether the first arguments of ARGS is an
option by testing to see if it starts say with a dash. So on
return the first argument is always removed.
"
  (let ((arg (car args))
	(d-two-args (mapcar (lambda(x) (concat "-" x)) two-args))
	(d-opt-two-args (mapcar (lambda(x) (concat "-" x)) opt-two-args))
	(remaining (cdr args)))
    (cond
     ((member arg d-two-args)
      (if (not remaining)
	    (progn
	      (message "Expecting an argument after %s. Continuing anyway."
		       arg)
	      (cons (list arg) (list remaining)))
	(cons (list arg (car remaining)) (list (cdr remaining)))))
     ((member arg d-opt-two-args)
      (if (and remaining (not (string-match "^-" (car remaining))))
	  (cons (list arg (car remaining)) (list (cdr remaining)))
	(cons (list arg) (list remaining))))
     (t (cons (list arg) (list remaining))))))

(defun realgud:terminate-srcbuf (&optional srcbuf)
  "Resets source buffer."
  (interactive "bsource buffer: ")
  (if (stringp srcbuf) (setq srcbuf (get-buffer srcbuf)))
  (with-current-buffer srcbuf
    (realgud-fringe-erase-history-arrows)
    (realgud-bp-remove-icons (point-min) (point-max))
    (when (realgud-srcbuf?)
      (realgud-short-key-mode-setup nil)
      (redisplay)
      )
    (loc-changes-clear-buffer)
    (realgud-srcbuf-mode -1)
    ))

(defun realgud:terminate (&optional buf)
  "Resets state in all buffers associated with source or command
buffer BUF) This does things like remove fringe arrows breakpoint
icons and resets short-key mode."
  (interactive "bbuffer: ")
  (if (stringp buf) (setq buf (get-buffer buf)))
  (let ((cmdbuf (realgud-get-cmdbuf buf)))
    (if cmdbuf
	(with-current-buffer cmdbuf
	  (realgud-locals-terminate)
	  (realgud-cmdbuf-info-in-debugger?= nil)
	  (realgud-cmdbuf-info-bp-list= '())
	  (realgud-cmdbuf-mode-line-update)
	  (realgud-fringe-erase-history-arrows)
	  (if realgud-cmdbuf-info
	      (dolist (srcbuf (realgud-cmdbuf-info-srcbuf-list realgud-cmdbuf-info))
		(if (realgud-srcbuf? srcbuf)
		    (with-current-buffer srcbuf
		      (realgud:terminate-srcbuf srcbuf)
		      ))
		)
	    )
	  )
      (error "Buffer %s does not seem to be attached to a debugger"
	     (buffer-name))
      )
    )
  )

(defun realgud:kill-buffer-hook ()
  "When a realgud command buffer is killed, call `realgud:terminate' to
clean up.
Note that `realgud-term-sentinel' is not helpful here because
the buffer and data associated with it are already gone."
  (when (realgud-cmdbuf?) (realgud:terminate (current-buffer)))
)
(add-hook 'kill-buffer-hook 'realgud:kill-buffer-hook)

(defun realgud-term-sentinel (process string)
  "Called when PROCESS dies. We call `realgud:terminate' to clean up."
  (let ((cmdbuf (realgud-get-cmdbuf)))
    (if cmdbuf (realgud:terminate cmdbuf)))
  (message "That's all folks.... %s" string))

(defun realgud:binary (file-name)
  "Return a whether FILE-NAME is executable or not or very large"
  (let* ((truename (file-chase-links file-name))
	 (output (shell-command-to-string
		  (format "file %s" truename)))
	 (filesize (nth 7 (file-attributes truename)))
	 )
    (cond
     ((string-match "ELF" output) t)
     ((and large-file-warning-threshold filesize
	   (> filesize large-file-warning-threshold)) t)
     ('t nil))))


(defun realgud-exec-shell (debugger-name script-filename program
				      &optional no-reset &rest args)
  "Run the specified SCRIPT-FILENAME in under debugger DEBUGGER-NAME a
comint process buffer. ARGS are the arguments passed to the
PROGRAM.  At the moment, no piping of input is allowed.

SCRIPT-FILENAME will have local variable `realgud-script-info' set
which contains the debugger name and debugger process-command
buffer.

Normally command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset."

  (let* ((non-nil-filename (or script-filename "+No filename+"))
	 (current-directory
	  (or (file-name-directory non-nil-filename)
	      default-directory "./"))
	 (cmdproc-buffer-name
	  (replace-regexp-in-string
	   "\s+" "\s"
	   (format "*%s %s shell*"
		   (file-name-nondirectory debugger-name)
		   (file-name-nondirectory non-nil-filename))))
	 (cmdproc-buffer (get-buffer-create cmdproc-buffer-name))
	 (realgud-buf (current-buffer))
	 (cmd-args (cons program args))
	 (process (get-buffer-process cmdproc-buffer)))

    (with-current-buffer cmdproc-buffer
      ;; If the found command buffer isn't for the same debugger
      ;; invocation command, rename that and start a new one.
      ;;
      ;; For example: "bashdb /tmp/foo" does not match "bashdb
      ;; /etc/foo" even though they both canonicalize to the buffer
      ;; "*bashdb foo shell*"
      (when (and (realgud-cmdbuf?)
		 (not
		  (equal cmd-args
			 (realgud-cmdbuf-info-cmd-args realgud-cmdbuf-info))
		  ))
	(rename-uniquely)
	(setq cmdproc-buffer (get-buffer-create cmdproc-buffer-name))
	(setq process nil)
	))

    (if (and process (eq 'run (process-status process)))
        cmdproc-buffer
      (with-current-buffer cmdproc-buffer
	(and (realgud-cmdbuf?) (not no-reset) (realgud:reset))
	(make-local-variable 'starting-directory)
	(setq starting-directory current-directory)

	(insert "Current directory: " current-directory "\n")
 	(insert "Command: " (mapconcat 'identity cmd-args " ") "\n")


	;; For term.el
	;; (term-mode)
	;; (set (make-local-variable 'term-term-name) realgud-term-name)
	;; (make-local-variable 'realgud-parent-buffer)
	;; (setq realgud-parent-buffer realgud-buf)

	;; For comint.el.
	(comint-mode)

	(let ((map (make-sparse-keymap)))
	  ;; Signals
	  (let ((signals-map (make-sparse-keymap "Signals")))
	    (define-key map [menu-bar signals] (cons "Signals" signals-map))
	    (define-key signals-map [eof]   '("EOF"   . comint-send-eof))
	    (define-key signals-map [kill]  '("KILL"  . comint-kill-subjob))
	    (define-key signals-map [quit]  '("QUIT"  . comint-quit-subjob))
	    (define-key signals-map [cont]  '("CONT"  . comint-continue-subjob))
	    (define-key signals-map [stop]  '("STOP"  . comint-stop-subjob))
	    (define-key signals-map [break] '("BREAK" . comint-interrupt-subjob)))
	  ;; Put them in the menu bar:
	  (setq menu-bar-final-items (append '(signals)
					     menu-bar-final-items))
	  map)


	;; Making overlay-arrow-variable-list buffer local has to be
	;; done after running commint mode. FIXME: find out why and if
	;; this reason is justifyable. Also consider moving this somewhere
	;; else.
	(make-local-variable 'overlay-arrow-variable-list)
	(make-local-variable 'realgud-overlay-arrow1)
	(make-local-variable 'realgud-overlay-arrow2)
	(make-local-variable 'realgud-overlay-arrow3)

	(condition-case failure
	    (comint-exec cmdproc-buffer debugger-name program nil args)
	  (error
	   (let ((text (format "%S\n" failure)))
	     (insert text)
	     (message text)(sit-for 1)
 	     text)))

 	(setq process (get-buffer-process cmdproc-buffer))

	(if (and process (eq 'run (process-status process)))
	  (let ((src-buffer)
		(cmdline-list (cons program args)))
	    ;; is this right?
	    (when (and script-filename (file-exists-p script-filename)
		       (not (realgud:binary script-filename)))
	      (setq src-buffer (find-file-noselect script-filename))
	      (point-max)
	      (realgud-srcbuf-init src-buffer cmdproc-buffer))
	    (process-put process 'buffer cmdproc-buffer))
	  ;; else
	  (let ((text
		 (format
		  "Failed to invoke debugger %s on program %s with args %s\n"
		  debugger-name program (mapconcat 'identity args " "))))
	    (with-current-buffer cmdproc-buffer (insert text))
	    (message text)
	  ))
    cmdproc-buffer))))

;; Start of a term-output-filter for term.el
(defun realgud-term-output-filter (process string)
  (let ((process-buffer (process-get process 'buffer)))
    (if process-buffer
	(save-current-buffer
	  (set-buffer process-buffer)
	  ;; (insert-before-markers (format "+++1 %s" string))
	  (insert-before-markers string)))))

(provide-me "realgud-")

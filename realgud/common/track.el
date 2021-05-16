;; Copyright (C) 2015-2020 Free Software Foundation, Inc

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

(declare-function realgud:terminate &optional cmdbuf)

(defconst realgud-track-char-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

;; Shell process buffers that we can hook into:
(require 'esh-mode)
(require 'ansi-color)
(require 'comint)

(require 'load-relative)
(require-relative-list
 '("core"           "file"     "fringe"
   "helper"         "init"     "loc"    "lochist"
   "regexp"         "shortkey" "window" "utils"
   "bp"
   ) "realgud-")


(require-relative-list
 '("buffer/command" "buffer/helper" "buffer/source") "realgud-buffer-")

(defcustom realgud-short-key-on-tracing? nil
"If non-nil, set short-key mode for any source buffer that is traced into"
  :type 'symbolp
  :group 'realgud)

(defcustom realgud-eval-message-print-length 1000
"If non-nil, truncate eval output into the echo area"
  :type 'symbolp
  :group 'realgud)

(declare-function buffer-killed?                        'realgud-helper)
(declare-function fn-p-to-fn?-alias                     'realgud-helper)
(declare-function realgud-bp-add-info                   'realgud-bp)
(declare-function realgud-bp-del-info                   'realgud-bp)
(declare-function realgud-bp-enable-disable-info        'realgud-bp)
(declare-function realgud-cmdbuf-add-srcbuf             'realgud-buffer-command)
(declare-function realgud-cmdbuf-debugger-name          'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-bp-list=          'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-divert-output?=   'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-in-debugger?      'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-in-debugger?=     'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-last-input-end=   'realgud-buffer-command)
(declare-function realgud-cmdbuf-init                   'realgud-buffer-command)
(declare-function realgud-cmdbuf-loc-hist               'realgud-buffer-command)
(declare-function realgud-cmdbuf-mode-line-update       'realgud-buffer-command)
(declare-function realgud-cmdbuf-mode-line-update       'realgud-buffer-command)
(declare-function realgud-cmdbuf-pat                    'realgud-buffer-command)
(declare-function realgud-cmdbuf?                       'realgud-buffer-command)
(declare-function realgud-cmdbuf-info-in-srcbuf?=       'realgud-buffer-command)
(declare-function realgud:debugger-name-transform       'realgud-helper)
(declare-function realgud:terminate                     'realgud-core)
(declare-function realgud:file-loc-from-line            'realgud-file)
(declare-function realgud-fringe-history-set            'realgud-fringe)
(declare-function realgud-get-cmdbuf                    'realgud-buffer-command)
(declare-function realgud-get-srcbuf-from-cmdbuf        'realgud-buffer-helper)
(declare-function realgud-loc-goto                      'realgud-loc)
(declare-function realgud-loc-hist-add                  'realgud-lochist)
(declare-function realgud-loc-hist-index                'realgud-lochist)
(declare-function realgud-loc-hist-item                 'realgud-lochist)
(declare-function realgud-loc?                          'realgud-loc)
(declare-function realgud-short-key-mode-setup          'realgud-shortkey)
(declare-function realgud-srcbuf-init-or-update         'realgud-source)
(declare-function realgud-srcbuf-loc-hist               'realgud-source)
(declare-function realgud-window-src                    'realgud-window)
(declare-function realgud-window-src-undisturb-cmd      'realgud-window)
(declare-function realgud-window-update-position        'realgud-window)
(declare-function realgud:join-string                   'realgud-utils)
(declare-function realgud:remove-ansi-schmutz-in-string 'realgud-utils)

(make-variable-buffer-local  (defvar realgud-track-mode))
(fn-p-to-fn?-alias 'realgud-loc-p)

(defvar realgud-track-divert-string
  ""
  "Some commands need information from the debugger to perform certain actions, such as show what breapoints exist, give back trace information. The output of debugger commands which need to be captured, are stored in this buffer-local string variable.")

(defvar starting-directory
  nil
  "When set this indicates the base directory that source code path should be based off of when the path is a relative path."
  )


(defvar realgud-command-name-hash
  nil
  "This buffer-local hash maps a debugger, like `gdb', or `pdb', to a hash table which describes how to implement generic debugger functions into the commands of that debugger. This information is set up by individual `init' function of the debugger. The keys at any given time will be those debuggers that have been used so far in the Emacs session.")

(defun realgud-track-comint-output-filter-hook(text)
  "An output-filter hook custom for comint shells.  Find
location/s, if any, and run the action(s) associated with
finding a new location/s.  The parameter TEXT appears because it
is part of the comint-output-filter-functions API. Instead we use
marks set in buffer-local variables to extract text"

  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next dbgr prompt, and then
  ;; check all text from comint-last-input-end to process-mark.

  ;; FIXME: Add unwind-protect?
  (if (and realgud-track-mode (realgud-cmdbuf? (current-buffer)))
      (let* ((cmd-buff (current-buffer))
	     (cmd-mark (point-marker))
	     (shortkey
	      (realgud-cmdbuf-info-src-shortkey?
	       realgud-cmdbuf-info))
	     (curr-proc (get-buffer-process cmd-buff))
	     (cmdbuf-last-output-end
	      (realgud-cmdbuf-info-last-input-end realgud-cmdbuf-info))
	     (last-output-end
	      (if curr-proc
		  (process-mark curr-proc)
		cmdbuf-last-output-end))
	     (last-output-start (max comint-last-input-start
				     (- last-output-end realgud-track-char-range))))
	;; Sometimes we get called twice and the second time nothing
	;; changes. Guard against this.
	(unless (= last-output-start last-output-end)
	  (unless (= last-output-end cmdbuf-last-output-end)
	    (setq last-output-start (max last-output-start
					 cmdbuf-last-output-end))
	    )
	  ;; Done with using old command buffer's last-input-end.
	  ;; Update that for next time.
	  (realgud-cmdbuf-info-last-input-end= last-output-start)
	  (realgud:track-from-region last-output-start
				     last-output-end cmd-mark cmd-buff
				     shortkey 't))
	)
    )
  )

(defun realgud-track-eshell-output-filter-hook()
  "An output-filter hook custom for eshell shells.  Find
location(s), if any, and run the action(s) associated with We use
marks set in buffer-local variables to extract text"

  ;; FIXME: Add unwind-protect?
  (if realgud-track-mode
      (let* ((cmd-buff (current-buffer))
	     (cmd-mark (point-marker))
	     (shortkey
	      (realgud-cmdbuf-info-src-shortkey?
	       realgud-cmdbuf-info))
	     (loc (realgud:track-from-region
		   eshell-last-output-start
		   eshell-last-output-end cmd-mark cmd-buff
		   shortkey)))
	(realgud-track-loc-action loc cmd-buff 't shortkey))
    ))

(defun realgud-track-term-output-filter-hook(text)
  "An output-filter hook custom for ansi-term shells.  Find
location/s, if any, and run the action(s) associated with
finding a new location/s.  The parameter TEXT appears because it
is part of the comint-output-filter-functions API. Instead we use
marks set in buffer-local variables to extract text"
  (if (and realgud-track-mode (realgud-cmdbuf? (current-buffer)))
      (realgud-track-loc text (point-marker))
    ))

(defun realgud:track-complain-if-not-in-cmd-buffer (&optional buf errorp)
  "Complain if BUF (default: current buffer) is not a command buffer.
With non-nil ERRORP, raise an exception.  Otherwise, print a
message."
  (setq buf (or buf (current-buffer)))
  (unless (realgud-cmdbuf? buf)
    (funcall (if errorp #'error #'message)
             "Buffer %s is not a debugger command buffer" buf)
    t))

(defun realgud:get-output-command(text)
  "Splits the TEXT by newline."
  (car (split-string text "\n")))

(defun realgud:get-eval-output(text)
  "Gets the output stripping the command and debugger prompt from the TEXT."
  (realgud:join-string (butlast (cdr (split-string text "\n"))) "\n"))

(defun realgud:get-command-name(command-name)
  "Gets the COMMAND-NAME for this particular debugger."
  (gethash command-name (buffer-local-value 'realgud-command-name-hash (current-buffer))))

(defun realgud:eval-command-p(text)
  "Checks the TEXT if the command that was ran was an eval command."
  (let ((cmd-name (realgud:get-command-name "eval")))
	(and (stringp cmd-name) (string-prefix-p (realgud:get-command-name "eval") (realgud:get-output-command text)))))

(defun realgud:truncate-eval-message(text)
  "Truncates the TEXT to the size of realgud-eval-message-print-length."
  (if (< realgud-eval-message-print-length (length text))
      (substring text 0 realgud-eval-message-print-length)
    text))

(defun realgud:message-eval-results(text)
  "Output the TEXT to the message area."
  (message (realgud:truncate-eval-message (realgud:get-eval-output text))))

(defun realgud:track-from-region(from to &optional cmd-mark opt-cmdbuf
				      shortkey-on-tracing? no-warn-if-no-match?)
  "Find and position a buffer at the location found in the marked region.

You might want to use this function interactively after marking a
region in a debugger-tracked shell buffer (see `realgud-track-mode')
or a more dedicated debugger command buffer.

The marked region location should match the regexp found in the
buffer-local variable `realgud-cmdbuf-info' structure under the
field loc-regexp. You can see what this is by
evaluating (realgud-cmdbuf-info-loc-regexp realgud-cmdbuf-info)"

  (interactive "r")
  (if (> from to) (cl-psetq to from from to))
  (let* ((text (realgud:remove-ansi-schmutz-in-string
                (buffer-substring-no-properties from to)))
	 (loc (realgud-track-loc text cmd-mark))
	 ;; If we see a selected frame number, it is stored
	 ;; in frame-num. Otherwise, nil.
	 (frame-num)
	 (text-sans-loc)
	 (cmdbuf (or opt-cmdbuf (current-buffer)))
	 )
    (unless (realgud:track-complain-if-not-in-cmd-buffer cmdbuf t)
      (if (realgud:eval-command-p text)
          (realgud:message-eval-results text))

      (if (not (equal "" text))
          (with-current-buffer cmdbuf
            (if (realgud-sget 'cmdbuf-info 'divert-output?)
                (realgud-track-divert-prompt text cmdbuf to))
            ;; FIXME: instead of these fixed filters,
            ;; put into a list and iterate over that.
            (realgud-track-termination? text)
            (setq text-sans-loc (or (realgud-track-loc-remaining text) text))
            (setq frame-num (realgud-track-selected-frame text))
            (if (and frame-num (not loc))
                (setq loc (realgud-track-loc-from-selected-frame
                           text cmd-mark)))

            (realgud:track-handle-breakpoints text-sans-loc cmd-mark cmdbuf)

            (if loc
                (let ((selected-frame
                       (or (not frame-num)
                           (eq frame-num (realgud-cmdbuf-pat "top-frame-num")))))
                  (realgud-track-loc-action loc cmdbuf (not selected-frame)
                                            shortkey-on-tracing?)
                  (realgud-cmdbuf-info-in-debugger?= 't)
                  (realgud-cmdbuf-mode-line-update)))

            )
        )
      )
    )
  )

(defun realgud:track-handle-breakpoints (text-sans-loc cmd-mark cmdbuf)
  (realgud:track-add-breakpoint text-sans-loc cmd-mark cmdbuf)
  (realgud:track-remove-breakpoints text-sans-loc cmd-mark cmdbuf))

(defun realgud:track-add-breakpoint (text-sans-loc cmd-mark cmdbuf)
  "Add a breakpoint fringe in source window if BP-LOC."
  (realgud-track-bp-enable-disable text-sans-loc
                                   (realgud-cmdbuf-pat "brkpt-enable")
                                   't)

  (let ((bp-loc (realgud-track-bp-loc text-sans-loc cmd-mark cmdbuf)))
    (if bp-loc
        (let ((src-buffer (realgud-loc-goto bp-loc)))
          (realgud-cmdbuf-add-srcbuf src-buffer cmdbuf)
          (with-current-buffer src-buffer
            (realgud-bp-add-info bp-loc))))))

(defun realgud:track-remove-breakpoints (text-sans-loc cmd-mark cmdbuf)
  "Remove all breakpoints in source window found in BP-LOCS."
  (realgud-track-bp-enable-disable text-sans-loc
                                   (realgud-cmdbuf-pat "brkpt-disable")
                                   nil)

  (dolist (bp-loc (realgud-track-bp-delete text-sans-loc cmd-mark cmdbuf))
    (let ((src-buffer (realgud-loc-goto bp-loc)))
      (realgud-cmdbuf-add-srcbuf src-buffer cmdbuf)
      (with-current-buffer src-buffer
        (realgud-bp-del-info bp-loc)))))

(defun realgud-track-hist-fn-internal(fn)
  "Update both command buffer and a source buffer to reflect the
selected location in the location history. If we started in a
command buffer, we stay in a command buffer. Moving inside a
command buffer always shows the corresponding source
file. However it is possible in shortkey mode to show only the
source code window, even the commmand buffer is updated albeit
unshown."

  (let ((cmdbuf (realgud-get-cmdbuf (current-buffer))))
    (if cmdbuf
	(let* ((loc-hist (realgud-cmdbuf-loc-hist cmdbuf))
	       (window (selected-window))
	       (position (funcall fn loc-hist))
	       (stay-in-cmdbuf?
		(or (eq (current-buffer) cmdbuf)
		    (with-current-buffer cmdbuf
		      (not (realgud-sget 'cmdbuf-info 'in-srcbuf?)))))
	       (loc (realgud-loc-hist-item loc-hist))
	       (srcbuf (realgud-get-srcbuf-from-cmdbuf cmdbuf loc))
	       )
	  (set-buffer (realgud-loc-goto loc))

	  ;; Make sure command buffer is updated
	  (realgud-window-update-position cmdbuf
				       (realgud-loc-cmd-marker loc))

	  ;; FIXME turn into fn. combine with realgud-track-loc-action.
	  (if stay-in-cmdbuf?
	      (let ((cmd-window (realgud-window-src-undisturb-cmd srcbuf)))
		(if cmd-window (select-window cmd-window)))
	    (realgud-window-src srcbuf)
	  )

	  ;; Make sure source buffer is updated
	  (realgud-window-update-position srcbuf
				       (realgud-loc-marker loc))

	  (message "history position %s line %s"
		   (realgud-loc-hist-index loc-hist)
		   (realgud-loc-line-number loc))
	  (select-window window)))
  ))

;; FIXME: Can we dry code more via a macro?
(defun realgud-track-hist-newer()
  (interactive)
  (realgud-track-hist-fn-internal 'realgud-loc-hist-newer))

(defun realgud-track-hist-newest()
  (interactive)
  (realgud-track-hist-fn-internal 'realgud-loc-hist-newest))

(defun realgud-track-hist-older()
  (interactive)
  (realgud-track-hist-fn-internal 'realgud-loc-hist-older))

(defun realgud-track-hist-oldest()
  (interactive)
  (realgud-track-hist-fn-internal 'realgud-loc-hist-oldest))

(defun realgud-track-loc-action (loc cmdbuf &optional not-selected-frame
				  shortkey-on-tracing?)
  "If loc is valid, show loc and do whatever actions we do for
encountering a new loc."
  (if (realgud-loc? loc)
      (let*
	  ((cmdbuf-loc-hist (realgud-cmdbuf-loc-hist cmdbuf))
	   (cmdbuf-local-overlay-arrow?
	    (with-current-buffer cmdbuf
	      (local-variable-p 'overlay-arrow-variable-list)))
	   (stay-in-cmdbuf?
	    (with-current-buffer cmdbuf
	      (not (realgud-sget 'cmdbuf-info 'in-srcbuf?))))
	   (shortkey-mode?
	    (with-current-buffer cmdbuf
	      (realgud-sget 'cmdbuf-info 'src-shortkey?)))
	   (srcbuf)
	   (srcbuf-loc-hist)
	   )

	(setq srcbuf (realgud-loc-goto loc))
	(realgud-srcbuf-init-or-update srcbuf cmdbuf)
	(setq srcbuf-loc-hist (realgud-srcbuf-loc-hist srcbuf))
	(realgud-cmdbuf-add-srcbuf srcbuf cmdbuf)

	(with-current-buffer srcbuf
	  (realgud-short-key-mode-setup
	   (and shortkey-on-tracing?
		(or realgud-short-key-on-tracing? shortkey-mode?))
	   ))

        ;; Do we need to go back to the process/command buffer because other
        ;; output-filter hooks run after this may assume they are in that
        ;; buffer? If so, we may have to use set-buffer rather than
	;; switch-to-buffer in some cases.
	(set-buffer cmdbuf)

	(unless (realgud-sget 'cmdbuf-info 'no-record?)
	  (realgud-loc-hist-add srcbuf-loc-hist loc)
	  (realgud-loc-hist-add cmdbuf-loc-hist loc)
	  (realgud-fringe-history-set cmdbuf-loc-hist cmdbuf-local-overlay-arrow?)
	  )

	;; FIXME turn into fn. combine with realgud-track-hist-fn-internal
	(if stay-in-cmdbuf?
	    (let ((cmd-window (realgud-window-src-undisturb-cmd srcbuf)))
	      (with-current-buffer srcbuf
		(if (and (boundp 'realgud-overlay-arrow1)
			 (markerp realgud-overlay-arrow1))
		    (progn
		      ;; Doesn't work
		      ;; (if not-selected-frame
		      ;; 	  (set-fringe-bitmap-face 'hollow-right-triangle
		      ;; 				  'realgud-overlay-arrow1)
		      ;; 			; else
		      ;; 	(set-fringe-bitmap-face 'realgud-right-triangle1
		      ;; 				'realgud-overlay-arrow1)
		      ;; 	)
		      (realgud-window-update-position srcbuf realgud-overlay-arrow1)))
		)
	      (if cmd-window (select-window cmd-window)))
	  ; else
	  (with-current-buffer srcbuf
	    (realgud-window-src srcbuf)
	    (realgud-window-update-position srcbuf realgud-overlay-arrow1))
	  ;; reset 'in-srcbuf' to allow the command buffer to keep point focus
	  ;; when used directly. 'in-srcbuf' is set 't' early in the stack
	  ;; (prior to common command code, e.g. this) when any command is run
	  ;; from a source buffer
	  (with-current-buffer cmdbuf
	    (realgud-cmdbuf-info-in-srcbuf?= nil))
	  )
	))
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (run-hooks 'realgud-update-hook) )
  )

(defun realgud-track-loc(text cmd-mark &optional opt-regexp opt-file-group
			   opt-line-group no-warn-on-no-match?)
  "Do regular-expression matching to find a file name and line number inside
string TEXT. If we match, we will turn the result into a realgud-loc struct.
Otherwise return nil."

  ;; NOTE: realgud-cmdbuf-info is a buffer variable local to the process running
  ;; the debugger. It contains a realgud-cmdbuf-info "struct". In that struct are
  ;; the fields loc-regexp, file-group, line-group, alt-file-group, and alt-line-group.
  ;;
  ;; By setting the the fields of realgud-cmdbuf-info appropriately, we
  ;; can accomodate a family of debuggers -- one at a time -- for the
  ;; buffer process.

  (unless (realgud:track-complain-if-not-in-cmd-buffer)
      (let
	  ((loc-regexp (or opt-regexp
			   (realgud-sget 'cmdbuf-info 'loc-regexp)))
	   (file-group (or opt-file-group
			   (realgud-sget 'cmdbuf-info 'file-group)))
	   (line-group (or opt-line-group
			   (realgud-sget 'cmdbuf-info 'line-group)))
	   (alt-file-group (realgud-sget 'cmdbuf-info 'alt-file-group))
	   (alt-line-group (realgud-sget 'cmdbuf-info 'alt-line-group))
	   (text-group (realgud-sget 'cmdbuf-info 'text-group))
	   (callback-loc-fn (realgud-sget 'cmdbuf-info 'callback-loc-fn))
	   )
	(if loc-regexp
	    (if (string-match loc-regexp text)
		(let* ((filename (or (match-string file-group text)
				     (match-string alt-file-group text)))
		       (line-str (or (match-string line-group text)
				     (match-string alt-line-group text)))
		       (source-str (and text-group
					(match-string text-group text)))
		       (lineno (string-to-number (or line-str "1")))
		       (directory
			(cond ((boundp 'starting-directory) starting-directory)
				     (t nil)))
		       )
		  (when source-str
		    (setq source-str (ansi-color-filter-apply
				      source-str)))
		  (cond ((and nil callback-loc-fn)
			 (funcall callback-loc-fn text
				  filename lineno source-str
				  cmd-mark directory))
			('t
			 (unless line-str
			   (message "line number not found -- using 1"))
			 (if (and filename lineno)
			     (realgud:file-loc-from-line filename lineno
							 cmd-mark
							 source-str nil
							 nil
							 directory
							 )
			   ;; else
			   nil)))))
	  ;; else
	  (and (message
		(concat "Buffer variable for regular expression pattern not"
                        " given and not passed as a parameter"))
               nil)))
    )
  )

(defun realgud-track-bp-loc(text &optional cmd-mark cmdbuf opt-ignore-re-file-list)
   "Do regular-expression matching to find a file name and line number inside
string TEXT. If we match, we will turn the result into a realgud-loc struct.
Otherwise return nil. CMD-MARK is set in the realgud-loc object created.
"

  ; NOTE: realgud-cmdbuf-info is a buffer variable local to the process
  ; running the debugger. It contains a realgud-cmdbuf-info "struct". In
  ; that struct is the regexp hash to match positions. By setting the
  ; the fields of realgud-cmdbuf-info appropriately we can accomodate a
  ; family of debuggers -- one at a time -- for the buffer process.

  (setq cmdbuf (or cmdbuf (current-buffer)))
  (with-current-buffer cmdbuf
    (unless (realgud:track-complain-if-not-in-cmd-buffer cmdbuf t)
      (let* ((loc-pat (realgud-cmdbuf-pat "brkpt-set"))
	     (shortkey-mode? (realgud-sget 'cmdbuf-info 'src-shortkey?))
	     (found-loc nil)
	     (loc-pat-list loc-pat))
	(unless (listp loc-pat-list)
	  (setq loc-pat-list (list loc-pat)))
	(while loc-pat-list
	  (setq loc-pat (car loc-pat-list))
	  (setq loc-pat-list (cdr loc-pat-list))
	  (let ((bp-num-group   (realgud-loc-pat-num loc-pat))
		  (loc-regexp     (realgud-loc-pat-regexp loc-pat))
		  (file-group     (realgud-loc-pat-file-group loc-pat))
		  (line-group     (realgud-loc-pat-line-group loc-pat))
		  (text-group     (realgud-loc-pat-text-group loc-pat))
		  (column-group   (realgud-loc-pat-column-group loc-pat))
		  (ignore-re-file-list (or opt-ignore-re-file-list
					   (realgud-sget 'cmdbuf-info 'ignore-re-file-list)))
		  (callback-loc-fn (realgud-sget 'cmdbuf-info 'callback-loc-fn))
		    )
	      (if loc-regexp
		  (if (string-match loc-regexp text)
		      (let* ((bp-num (and bp-num-group (match-string bp-num-group text)))
			     (filename
			      (if file-group
				  (match-string file-group text)
				(realgud-sget 'cmdbuf-info 'source-path)
				))
			     (line-str (match-string line-group text))
			     (source-str (and text-group (match-string text-group text)))
			     (lineno (string-to-number (or line-str "1")))
			     (column-str (and column-group (match-string column-group text)))
			     (column (string-to-number (or column-str "1")))
			     (directory
			      (cond ((boundp 'starting-directory) starting-directory)
				    (t nil)))
			     )
			(cond (callback-loc-fn
			       (if (setq found-loc (funcall callback-loc-fn text
							 filename lineno source-str
							 cmd-mark directory column))
				   ;; FIXME: dry with code in realgud-track-bp-file-line
				   (let ((bp-list (realgud-sget 'cmdbuf-info 'bp-list))
					 srcbuf)

				     ;; Add src buffer mentioned and set it possibly to go into shortkey mode
				     (setq srcbuf (realgud-loc-goto found-loc))
				     (realgud-cmdbuf-add-srcbuf srcbuf cmdbuf)
				     (realgud-srcbuf-init-or-update srcbuf cmdbuf)
				     (with-current-buffer srcbuf
				       (realgud-short-key-mode-setup
					(or realgud-short-key-on-tracing? shortkey-mode?)
					))

				     ;; Add breakpoint to list of breakpoints
				     (with-current-buffer-safe (marker-buffer (realgud-loc-marker found-loc))
				       (realgud-bp-add-info found-loc))

				     (unless (member found-loc bp-list)
				       (realgud-cmdbuf-info-bp-list= (cons found-loc bp-list)))
				     )
				 (setq loc-pat-list nil)))
			      (t
			       (unless line-str
				 (message "line number not found -- using 1"))
			       (if (setq found-loc
					 (realgud-track-bp-file-line cmd-mark cmdbuf filename lineno source-str bp-num directory column shortkey-mode?))
				   (setq loc-pat-list nil)))

			      )
			)
		    ))))
	found-loc)
      )))

(defun realgud-track-bp-file-line(cmd-mark cmdbuf filename lineno source-str bp-num directory column shortkey-mode?)
  (if (and filename lineno)
      (let* ((directory
	      (cond ((boundp 'starting-directory) starting-directory)
		    (t nil)))
	     (srcbuf)
	     (found-loc nil)
	     (loc-or-error
	      (realgud:file-loc-from-line
	       filename lineno
	       cmd-mark
	       source-str
	       (string-to-number bp-num)
	       nil directory
	       )))
	(if (stringp loc-or-error)
	    (progn
	      (message loc-or-error)
	      ;; set to return nil
	      (setq found-loc nil))
	  ;; else
	  (let ((loc loc-or-error)
		(bp-list (realgud-sget 'cmdbuf-info 'bp-list)))

	    ;; Add src buffer mentioned and set it possibly to go into shortkey mode
	    (setq srcbuf (realgud-loc-goto loc))
	    (realgud-cmdbuf-add-srcbuf srcbuf cmdbuf)
	    (realgud-srcbuf-init-or-update srcbuf cmdbuf)
	    (with-current-buffer srcbuf
	      (realgud-short-key-mode-setup
	       (or realgud-short-key-on-tracing? shortkey-mode?)
	       ))

	    ;; Add breakpoint to list of breakpoints
	    (with-current-buffer-safe (marker-buffer (realgud-loc-marker loc))
	      (realgud-bp-add-info loc))

	    (realgud-cmdbuf-info-bp-list= (delete-dups (cl-adjoin loc bp-list :test #'equal)))

	    ;; Set to return location
	    (setq found-loc loc-or-error)
	  ))
    found-loc
    )))


(defun realgud-track-bp-delete(text &optional cmd-mark cmdbuf ignore-re-file-list)
  "Do regular-expression matching to see if a breakpoint has been
deleted inside string TEXT. Return a list of breakpoint locations
of the breakpoints found in command buffer."

  ; NOTE: realgud-cmdbuf-info is a buffer variable local to the process
  ; running the debugger. It contains a realgud-cmdbuf-info "struct". In
  ; that struct is the regexp hash to match positions. By setting the
  ; the fields of realgud-cmdbuf-info appropriately we can accomodate a
  ; family of debuggers -- one at a time -- for the buffer process.

  (setq cmdbuf (or cmdbuf (current-buffer)))
  (with-current-buffer cmdbuf
    (unless (realgud:track-complain-if-not-in-cmd-buffer cmdbuf t)
      (let* ((loc-pat (realgud-cmdbuf-pat "brkpt-del")))
        (when loc-pat
          (let ((bp-num-group (realgud-loc-pat-num loc-pat))
                (loc-regexp   (realgud-loc-pat-regexp loc-pat)))
            (when (and loc-regexp (string-match loc-regexp text))
              (let* ((bp-nums-str (match-string bp-num-group text))
                     (bp-num-strs (split-string bp-nums-str "[^0-9]+" t))
                     (bp-nums (mapcar #'string-to-number bp-num-strs))
                     (info realgud-cmdbuf-info)
                     (all-bps (realgud-cmdbuf-info-bp-list info))
                     (found-locs nil))
                (dolist (loc all-bps)
                  (when (memq (realgud-loc-num loc) bp-nums)
                    (push loc found-locs)
                    ;; Remove loc from breakpoint list
                    (realgud-cmdbuf-info-bp-list=
                     (remove loc (realgud-cmdbuf-info-bp-list info)))))
                ;; return the locations
                found-locs))))))))

(defun realgud-track-bp-enable-disable(text loc-pat enable? &optional cmdbuf)
  "Do regular-expression matching see if a breakpoint has been enabled or disabled inside
string TEXT. If we match, we will do the action to the breakpoint found and return the
breakpoint location. Otherwise return nil.
"
  (setq cmdbuf (or cmdbuf (current-buffer)))
  (with-current-buffer cmdbuf
    (if (realgud-cmdbuf?)
	(let* ((found-loc nil))
	  (if loc-pat
	      (let ((bp-num-group (realgud-loc-pat-num loc-pat))
		    (loc-regexp   (realgud-loc-pat-regexp loc-pat)))
		(if (and loc-regexp (string-match loc-regexp text))
		    (let* ((bp-num (string-to-number (match-string bp-num-group text)))
			   (info realgud-cmdbuf-info)
			   (bp-list (realgud-cmdbuf-info-bp-list info))
			   (loc)
			   )
		      (while (and (not found-loc) (setq loc (car-safe bp-list)))
			(setq bp-list (cdr bp-list))
			(when (eq (realgud-loc-num loc) bp-num)
			  (setq found-loc loc)
			  (let ((src-buffer (realgud-loc-goto loc)))
			    (realgud-cmdbuf-add-srcbuf src-buffer cmdbuf)
			    (with-current-buffer src-buffer
			      (realgud-bp-enable-disable-info bp-num enable? loc src-buffer)
			      )))
			)
		      ;; return the location:
		      found-loc)
		  nil))
	    nil))
      (and (message "Current buffer %s is not a debugger command buffer"
		    (current-buffer)) nil)
      )
    )
)

(defun realgud-track-loc-remaining(text)
  "Return the portion of TEXT starting with the part after the
loc-regexp pattern"
  (if (realgud-cmdbuf?)
      (let* ((loc-pat (realgud-cmdbuf-pat "loc"))
	     (loc-regexp (realgud-loc-pat-regexp loc-pat))
	     )
	(if loc-regexp
	    (if (string-match loc-regexp text)
		(substring text (match-end 0))
	      nil)
	  nil))
    nil)
  )

(defun realgud-track-selected-frame(text)
  "Return a selected frame number found in TEXT or nil if none found."
  (if (realgud-cmdbuf?)
      (let ((selected-frame-pat (realgud-cmdbuf-pat "selected-frame"))
	    (frame-num-regexp)
	    )
	(if (and selected-frame-pat
		 (setq frame-num-regexp (realgud-loc-pat-regexp
					 selected-frame-pat)))
	    (if (string-match frame-num-regexp text)
		(let ((frame-num-group (realgud-loc-pat-num selected-frame-pat)))
		  (string-to-number (match-string frame-num-group text)))
	      nil)
	  nil))
    nil)
  )


(defun realgud-track-loc-from-selected-frame(text cmd-mark &optional
						  opt-regexp opt-ignore-re-file-list)
  "Return a selected frame number found in TEXT or nil if none found."
  (if (realgud-cmdbuf?)
      (let ((selected-frame-pat (realgud-cmdbuf-pat "selected-frame"))
	    (frame-num-regexp)
	    (ignore-re-file-list (or opt-ignore-re-file-list
				(realgud-sget 'cmdbuf-info 'ignore-re-file-list))))
	(if (and selected-frame-pat
		 (setq frame-num-regexp (realgud-loc-pat-regexp
					 selected-frame-pat)))
	    (if (string-match frame-num-regexp text)
		(let* ((file-group (realgud-loc-pat-file-group selected-frame-pat))
		       (line-group (realgud-loc-pat-line-group selected-frame-pat))
		       (filename (match-string file-group text))
		       (lineno (string-to-number (match-string line-group text))))
		  (if (and filename lineno)
		      (realgud:file-loc-from-line filename lineno
						  cmd-mark nil nil)
		    nil))
	      nil)
	  nil))
    nil))

(defun realgud-track-termination?(text)
  "Return 't and call `realgud:terminate' we we have a termination message"
  (if (realgud-cmdbuf?)
      (let ((termination-re (realgud-cmdbuf-pat "termination"))
	    )
	(if (and termination-re (string-match termination-re text))
	    (progn
	      (realgud:terminate (current-buffer))
	      't)
	  nil)
	)
    )
  )

(defun realgud-track-divert-prompt(text cmdbuf to)
  "Return a cons node of the part before the prompt-regexp and the part
   after the prompt-regexp-prompt. If not found return nil."
  (with-current-buffer cmdbuf
    ;; (message "+++3 %s, buf: %s" text (buffer-name))
    (if (realgud-cmdbuf?)
	(let* ((prompt-pat (realgud-cmdbuf-pat "prompt"))
	       (prompt-regexp (realgud-loc-pat-regexp prompt-pat))
	       )
	  (if prompt-regexp
	      (if (string-match prompt-regexp text)
		  (progn
		    (setq realgud-track-divert-string
			  (substring text 0 (match-beginning 0)))
		    ;; We've got desired output, so reset divert output.
		    (realgud-cmdbuf-info-divert-output?= nil)
		    (cond ((search-backward-regexp prompt-regexp)
			   (kill-region realgud-last-output-start (point))
			   (goto-char (point-max)))
			  ('t (kill-region realgud-last-output-start to)))
		    )
	      ))
	  )
      )
    )
  )

(defun realgud-goto-line-for-loc-pat (pt &optional opt-realgud-loc-pat)
  "Display the location mentioned in line described by
PT. OPT-REALGUD-LOC-PAT is used to get regular-expresion pattern
matching information. If not supplied we use the current buffer's \"location\"
pattern found via realgud-cmdbuf information. nil is returned if we can't
find a location. non-nil if we can find a location.
"
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let*
	((cmdbuf (current-buffer))
	 (cmd-mark (point-marker))
	 (curr-proc (get-buffer-process cmdbuf))
	 (start (line-beginning-position))
	 (end (line-end-position))
	 (loc-pat (or opt-realgud-loc-pat (realgud-cmdbuf-pat "loc")))
	 (loc)
	 )
      (unless (and loc-pat (realgud-loc-pat-p loc-pat))
	(error "Can't find location information for %s" cmdbuf))
      (setq loc (realgud-track-loc (buffer-substring-no-properties start end)
				cmd-mark
				(realgud-loc-pat-regexp loc-pat)
				(realgud-loc-pat-file-group loc-pat)
				(realgud-loc-pat-line-group loc-pat)
				nil
				))
      (if (stringp loc)
	  (message loc)
	(if loc (or (realgud-track-loc-action loc cmdbuf) 't)
	  nil))
      ))
  )

(defun realgud:populate-command-hash(key value)
  "Adds a KEY and VALUE to the realgud-command-name-hash the command name to a debugger specific command."
  (puthash key
           (replace-regexp-in-string "%.*" "" (car (split-string value " ")))
           realgud-command-name-hash))

(defun realgud-set-command-name-hash-to-buffer-local (command-hash)
  "Sets the eval string as a buffer local variable from the COMMAND-HASH."
  (set (make-local-variable 'realgud-command-name-hash) (make-hash-table :test 'equal))
  (maphash 'realgud:populate-command-hash command-hash))

(defun realgud:track-set-debugger (debugger-name)
  "Set debugger name and information associated with that
debugger for the buffer process. This info is returned or nil if
we can't find a debugger with that information.`.
"
  ;; FIXME: turn into fn which can be used by realgud-backtrack-set-debugger
  (interactive
   (list (completing-read "Debugger name: " realgud-pat-hash)))
  (let* ((base-variable-name
	  (or (gethash debugger-name realgud:variable-basename-hash)
	      debugger-name))
         (regexp-hash (gethash debugger-name realgud-pat-hash))
         (command-hash (gethash debugger-name realgud-command-hash))
	)
    (unless regexp-hash
      ;; FIXME: phase out realgud:debugger-name-transform
      (setq base-variable-name (realgud:debugger-name-transform debugger-name))
      (setq regexp-hash (gethash base-variable-name realgud-pat-hash))
      (setq command-hash (gethash base-variable-name realgud-command-hash))
      )

    (when command-hash
      (realgud-set-command-name-hash-to-buffer-local command-hash)
      )

    (if regexp-hash
	(let* (
	       (mode-name (concat " " (capitalize base-variable-name) "-Track"))
	       (specific-track-mode (intern (concat base-variable-name "-track-mode")))
	       )
	  (realgud-cmdbuf-init (current-buffer)
                               debugger-name regexp-hash
                               command-hash base-variable-name)
	  (if (and (not (eval specific-track-mode))
		   (functionp specific-track-mode))
	      (funcall specific-track-mode 't))
	  )
      (progn
	(message "I don't have %s listed as a debugger." debugger-name)
	nil)
      )))

;; FIXME: need better name for this and next fn.
(defun realgud-goto-line-for-pt-and-type (pt type pat-hash)
  "Position the source code at the location that is matched by
PAT-HASH with key TYPE. The line at PT is used as the string
to match against and has location info embedded in it"
  (realgud-goto-line-for-loc-pat pt (gethash type pat-hash)))


(defun realgud-goto-line-for-pt (pt pattern-key)
  "Position the source code at the location indicated by a
pattern found in the command buffer with pattern-key
PATTERN-KEY. (PATTERN-KEY is something like 'debugger-backtrace'
or 'loc'.) The line at PT is used as the string to match against
and has location info embedded in it"
  (interactive "d")
  (unless (realgud-cmdbuf?)
    (error "You need to be in a debugger command buffer to run this"))
  (let* ((debugger-name (realgud-cmdbuf-debugger-name))
	 (debugger-pat-hash (gethash debugger-name realgud-pat-hash)))
    (realgud-goto-line-for-pt-and-type pt pattern-key debugger-pat-hash)
    )
  )

(defun realgud:goto-debugger-backtrace-line (pt)
  "Position the source code at the location indicated by matching a
command buffer's debugger backtrace pattern against the line at PT."
  (interactive "d")
  (unless (realgud-goto-line-for-pt pt "debugger-backtrace")
    (message "Line didn't match a debugger backtrace location.")
    ))

(defun realgud:goto-lang-backtrace-line (pt)
  "Position the source code at the location indicated by matching a
command buffer's programming-language backtrace pattern against the line at PT."
  (interactive "d")
  (unless (realgud-goto-line-for-pt pt "lang-backtrace")
    (message "Line didn't match a programming-language backtrace location.")
    ))

(defun realgud:goto-debugger-loc-line (pt)
  "Position the source-code at the location indicated by matching a
command buffer's debugger location pattern against the line at PT."
  (interactive "d")
  (unless (realgud-goto-line-for-pt pt "loc")
    (message "Line didn't match a debugger location indicator line.")
    ))

(provide-me "realgud-")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

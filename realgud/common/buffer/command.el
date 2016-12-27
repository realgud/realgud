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
;;; process-command buffer things

(require 'load-relative)
(require 'json)
(require-relative-list
 '("../fringe"  "../loc" "../lochist" "../regexp")  "realgud-")
(require-relative-list '("info")  "realgud-buffer-")

(declare-function realgud-get-cmdbuf 'realgud-buffer-helper)

(eval-when-compile
  (byte-compile-disable-warning 'cl-functions)
  ;; Somehow disabling cl-functions causes the erroneous message:
  ;;   Warning: the function `reduce' might not be defined at runtime.
  ;; FIXME: isolate, fix and/or report back to Emacs developers a bug
  (byte-compile-disable-warning 'unresolved)
  (defvar realgud-cmdbuf-info)
  )
(require 'cl-lib)

(defface debugger-running
  '((((class color) (min-colors 16) (background light))
     (:foreground "Green4" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Green1" :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Green" :weight bold))
    (((class color)) (:foreground "green" :weight bold))
    (t (:weight bold)))
  "Face used to highlight debugger run information."
  :group 'realgud
  :version "24.3")

(defface debugger-not-running
  '((t :inherit font-lock-warning-face))
  "Face used when debugger or process is not running."
  :group 'realgud
  :version "24.3")


(cl-defstruct realgud-cmdbuf-info
  "The debugger object/structure specific to a process buffer."
  debugger-name        ;; Name of debugger
  base-variable-name   ;; prefix used in variables pertinent to this
                       ;; debugger sometimes it is the same as the debugger
                       ;; and sometimes it is different
  cmd-args             ;; Command-line invocation arguments
  frame-switch?        ;; Should the selected window be the source buffer or
		       ;; command buffer?
  in-srcbuf?           ;; If true, selected window should be the source buffer.
		       ;; Otherwise, the command buffer?
  last-input-end       ;; point where input last ended. Set from
                       ;; comint-last-input-end
  prior-prompt-regexp  ;; regular expression prompt (e.g.
                       ;; comint-prompt-regexp) *before* setting
                       ;; loc-regexp
  no-record?           ;; Should we update the location history?
  in-debugger?         ;; True if we think we are in a debugger
  src-shortkey?        ;; Are source buffers in realgud-short-key mode?
  regexp-hash          ;; hash table of regular expressions appropriate for
                       ;; this debugger. Eventually loc-regexp, file-group
                       ;; and line-group below will removed and stored here.
  srcbuf-list          ;; list of source buffers we have stopped at
  bt-buf               ;; backtrace buffer if it exists
  bp-list              ;; list of breakpoints
  divert-output?       ;; Output is part of a conversation between front-end
                       ;; debugger.
  cmd-hash             ;; Allows us to remap command names like
                       ;; quit => quit!
  callback-loc-fn      ;; If we need, as in the case of Java, to do
                       ;; special handling to map output to a file
                       ;; location, this is set to that special
                       ;; function
  callback-eval-filter ;; If set, this function strip extraneous output
                       ;; when evaluating an expression. For example,
                       ;; some trepan debuggers expression values prefaced with:
                       ;; $DB::D[0] =

  ;; FIXME: REMOVE THIS and use regexp-hash
  loc-regexp   ;; Location regular expression string
  file-group
  line-group
  alt-file-group
  alt-line-group
  text-group
  ignore-file-re

  loc-hist     ;; ring of locations seen in the course of execution
               ;; see realgud-lochist
  starting-directory    ;; directory where initial debug command was issued.
                        ;; this can be used to resolve relative file names
  )
(make-variable-buffer-local 'realgud-cmdbuf-info)
(make-variable-buffer-local 'realgud-last-output-start)

(defalias 'realgud-cmdbuf-info? 'realgud-cmdbuf-info-p)

;; FIXME: figure out how to put in a loop.
(realgud-struct-field-setter "realgud-cmdbuf-info" "bp-list")
(realgud-struct-field-setter "realgud-cmdbuf-info" "bt-buf")
(realgud-struct-field-setter "realgud-cmdbuf-info" "cmd-args")
(realgud-struct-field-setter "realgud-cmdbuf-info" "last-input-end")
(realgud-struct-field-setter "realgud-cmdbuf-info" "divert-output?")
(realgud-struct-field-setter "realgud-cmdbuf-info" "frame-switch?")
(realgud-struct-field-setter "realgud-cmdbuf-info" "in-srcbuf?")
(realgud-struct-field-setter "realgud-cmdbuf-info" "no-record?")
(realgud-struct-field-setter "realgud-cmdbuf-info" "prior-prompt-regexp")
(realgud-struct-field-setter "realgud-cmdbuf-info" "src-shortkey?")
(realgud-struct-field-setter "realgud-cmdbuf-info" "in-debugger?")
(realgud-struct-field-setter "realgud-cmdbuf-info" "callback-loc-fn")
(realgud-struct-field-setter "realgud-cmdbuf-info" "callback-eval-filter")
(realgud-struct-field-setter "realgud-cmdbuf-info" "starting-directory")

(defun realgud:cmdbuf-follow-buffer(event)
  (interactive "e")
  (let* ((pos (posn-point (event-end event)))
	 (buffer (get-text-property pos 'buffer)))
    (find-file-other-window (buffer-file-name buffer))))

(defun realgud:cmdbuf-buffers-describe (info)
  (let* ((buffer-list (realgud-cmdbuf-info-srcbuf-list info))
	 (debugger-name (realgud-cmdbuf-info-debugger-name info))
	 (file-remap-name  (intern (format "realgud:%s-file-remap" debugger-name)))
	 (file-remap (and (boundp file-remap-name) (eval file-remap-name)))
	 (filename)
	 (remapped-filename)
	 )
    (insert "** Source Buffers Seen (srcbuf-list)\n")

    (dolist (buffer buffer-list)
      (insert "  - ")
      (put-text-property
       (insert-text-button
	(setq filename (buffer-name buffer))
	'action 'realgud:cmdbuf-follow-buffer
	'help-echo "mouse-2: visit this file")
       (point)
       'buffer buffer)
      (when (setq remapped-filename (and file-remap (gethash filename file-remap)))
	(insert (format "\tremapped to: %s" remapped-filename)))
      (insert "\n")
      )))

(defun realgud:cmdbuf-info-describe (&optional buffer)
  "Display realgud-cmdcbuf-info fields of BUFFER.
BUFFER is either a debugger command or source buffer. If BUFFER is not given
the current buffer is used as a starting point.
Information is put in an internal buffer called *Describe*."
  (interactive "")
  (setq buffer (realgud-get-cmdbuf buffer))
  (if buffer
      (with-current-buffer buffer
	(lexical-let ((info realgud-cmdbuf-info)
	      (cmdbuf-name (buffer-name)))
	  (if info
	      (progn
		(switch-to-buffer (get-buffer-create "*Describe*"))
		(setq buffer-read-only 'nil)
		(delete-region (point-min) (point-max))
		(insert "#+STARTUP: showall\n")
		;;(insert "#+OPTIONS:    H:2 num:nil toc:t \\n:nil ::t |:t ^:nil -:t f:t *:t tex:t d:(HIDE) tags:not-in-toc\n")
		(insert (format "#+TITLE: Debugger info for %s\n" cmdbuf-name))
		(insert "** General Information (")
		(insert-text-button
		 "realgud-cmdbuf-info"
		 ;; FIXME figure out how to set buffer to cmdbuf so we get cmdbuf value
		 'action (lambda(button) (describe-variable 'realgud-cmdbuf-info))
		 'help-echo "mouse-2: help-on-variable")
		(insert ")\n")

		(mapc 'insert
		      (list
		       (format "  - Debugger name     ::\t%s\n"
			       (json-encode (realgud-cmdbuf-info-debugger-name info)))
		       (format "  - Command-line args ::\t%s\n"
			       (json-encode (realgud-cmdbuf-info-cmd-args info)))
		       (format "  - Starting directory ::\t%s\n"
			       (realgud-cmdbuf-info-starting-directory info))
		       (format "  - Selected window should contain source? :: %s\n"
			       (realgud-cmdbuf-info-in-srcbuf? info))
		       (format "  - Last input end    ::\t%s\n"
			       (realgud-cmdbuf-info-last-input-end info))
		       (format "  - Source should go into short-key mode? :: %s\n"
			       (realgud-cmdbuf-info-src-shortkey? info))
		       (format "  - Breakpoint list   ::\t %s\n"
			       (realgud-cmdbuf-info-bp-list info))
		       (format "  - Remap table for debugger commands ::\n\t%s\n"
			       (json-encode (realgud-cmdbuf-info-cmd-hash info)))
		       (format "  - Backtrace buffer  ::\t%s\n"
			       (realgud-cmdbuf-info-bt-buf info))
		       (format "  - In debugger?      ::\t%s\n"
			       (realgud-cmdbuf-info-in-debugger? info))
		       ))
		(insert "\n")
		(realgud:cmdbuf-buffers-describe info)
		(insert "\n")
		(realgud:loc-hist-describe (realgud-cmdbuf-info-loc-hist info))
		(goto-char (point-min))
		(realgud:info-mode)
		)
	    (message "realgud-cmdbuf-info is nil")
	  )
	))
    (message "Buffer %s is not a debugger source or command buffer; nothing done."
	     (or buffer (current-buffer)))
    )
  )

(defun realgud-cmdbuf? (&optional buffer)
  "Return true if BUFFER is a debugger command buffer."
  (with-current-buffer-safe
   (or buffer (current-buffer))
   (realgud-cmdbuf-info-set?)))

(defun realgud-cmdbuf-info-set? ()
  "Return true if realgud-cmdbuf-info is set."
  (and (boundp 'realgud-cmdbuf-info)
       realgud-cmdbuf-info
       (realgud-cmdbuf-info? realgud-cmdbuf-info)))

(defun realgud-cmdbuf-toggle-in-debugger? (&optional buffer)
  "Toggle state of whether we think we are in the debugger or not"
  (interactive "")
  (setq buffer (realgud-get-cmdbuf buffer))
  (if buffer
      (with-current-buffer buffer
	(realgud-cmdbuf-info-in-debugger?=
	 (not (realgud-sget 'cmdbuf-info 'in-debugger?)))
	(message "Command buffer is in debugger?: %s\n"
		 (realgud-cmdbuf-info-in-debugger? realgud-cmdbuf-info))
	(realgud-cmdbuf-mode-line-update)
	)
    (message "Buffer %s is not a debugger buffer; nothing done."
	     (or buffer (current-buffer)))
    )
  )

(defun realgud-cmdbuf-stay-in-source-toggle (&optional buffer)
  "Toggle state of whether we should stay in source code or not"
  (interactive "")
  (setq buffer (realgud-get-cmdbuf buffer))
  (if buffer
      (with-current-buffer buffer
	(realgud-cmdbuf-info-in-srcbuf?=
	 (not (realgud-sget 'cmdbuf-info 'in-srcbuf?)))
	(message "Selected window should contain source?: %s\n"
		 (realgud-cmdbuf-info-in-srcbuf? realgud-cmdbuf-info))
	)
    (message "Buffer %s is not a debugger buffer; nothing done."
	     (or buffer (current-buffer)))
    )
  )

(defun realgud-cmdbuf-add-srcbuf(srcbuf &optional cmdbuf)
  "Add SRCBUF to srcbuf-list field of INFO unless it is already included."
  (setq cmdbuf (or cmdbuf (current-buffer)))
  (if (realgud-cmdbuf? cmdbuf)
      (with-current-buffer-safe cmdbuf
	(unless (memq srcbuf (realgud-cmdbuf-info-srcbuf-list realgud-cmdbuf-info))
	  (setf (realgud-cmdbuf-info-srcbuf-list realgud-cmdbuf-info)
		(cons srcbuf (realgud-cmdbuf-info-srcbuf-list realgud-cmdbuf-info))))
	)
    )
  )

(defun realgud-cmdbuf-set-shortkey(&optional cmdbuf unset)
  (interactive "")
  (setq cmdbuf (or cmdbuf (current-buffer)))
  (if (realgud-cmdbuf? cmdbuf)
      (with-current-buffer-safe cmdbuf
	(setf (realgud-cmdbuf-info-src-shortkey? realgud-cmdbuf-info) (not unset))
	(message "Set source to shortkey is now %s" (not unset))
	))
  )

(defun realgud-cmdbuf-command-string(cmd-buffer)
  "Get the command string invocation for this command buffer"
    (cond
     ((realgud-cmdbuf? cmd-buffer)
      (with-current-buffer cmd-buffer
	(let*
	    ((cmd-args (realgud-sget 'cmdbuf-info 'cmd-args))
	     (result (car cmd-args)))
	  (and cmd-args
	       (reduce (lambda(result x)
			 (setq result (concat result " " x)))
		       cmd-args)))))
     (t nil)))

;; FIXME cmd-hash should not be optional. And while I am at it, remove
;; parameters loc-regexp, file-group, and line-group which can be found
;; inside pat-hash
;;
;; To do this however we need to fix up the caller
;; realgud:track-set-debugger by changing realgud-pat-hash to store a hash
;; rather than the loc, file, and line fields; those fields then get
;; removed.

(defun realgud-cmdbuf-init
    (cmd-buf debugger-name regexp-hash &optional cmd-hash base-variable-name
	     starting-directory)
  "Initialize CMD-BUF for a working with a debugger.
DEBUGGER-NAME is the name of the debugger; REGEXP-HASH are debugger-specific
values set in the debugger's init.el."
  (with-current-buffer-safe cmd-buf
    (let ((realgud-loc-pat (gethash "loc" regexp-hash))
	  (font-lock-keywords)
	  )
      (setq realgud-cmdbuf-info
	    (make-realgud-cmdbuf-info
	     :in-srcbuf? nil
	     :debugger-name debugger-name
             :base-variable-name (or base-variable-name debugger-name)
	     :loc-regexp (realgud-sget 'loc-pat 'regexp)
	     :file-group (realgud-sget 'loc-pat 'file-group)
	     :line-group (realgud-sget 'loc-pat 'line-group)
	     :alt-file-group (realgud-sget 'loc-pat 'alt-file-group)
	     :alt-line-group (realgud-sget 'loc-pat 'alt-line-group)
	     :text-group (realgud-sget 'loc-pat 'text-group)
	     :ignore-file-re (realgud-sget 'loc-pat 'ignore-file-re)
	     :loc-hist (make-realgud-loc-hist)
	     :regexp-hash regexp-hash
	     :bt-buf nil
	     :last-input-end (point-max)
	     :cmd-hash cmd-hash
	     :src-shortkey? t
	     :in-debugger? nil
	     :callback-loc-fn (gethash "loc-callback-fn" regexp-hash)
	     :callback-eval-filter (gethash "callback-eval-filter"
					    regexp-hash)
	     ))
      (setq font-lock-keywords (realgud-cmdbuf-pat "font-lock-keywords"))
      (if font-lock-keywords
	  (set (make-local-variable 'font-lock-defaults)
	       (list font-lock-keywords)))
      )

    (put 'realgud-cmdbuf-info 'variable-documentation
	 "Debugger object for a process buffer."))
  )

(defun realgud-cmdbuf-reset (cmd-buf)
  "nil out variable realgud-cmdbuf-info in CMD-BUF"
  (with-current-buffer-safe cmd-buf
    (setq realgud-cmdbuf-info nil)
  ))

(defun realgud-cmdbuf-debugger-name (&optional cmd-buf)
  "Return the debugger name recorded in the debugger command-process buffer."
  (with-current-buffer-safe (or cmd-buf (current-buffer))
    (if (realgud-cmdbuf?)
	(realgud-sget 'cmdbuf-info 'debugger-name)
      nil))
  )

(defun realgud-cmdbuf-pat(key)
  "Extract regexp stored under KEY in a realgud-cmdbuf via realgud-cmdbuf-info"
  (if (realgud-cmdbuf?)
      (let*
	  ((debugger-name (realgud-cmdbuf-debugger-name))
	   (regexp-hash (gethash debugger-name realgud-pat-hash))
	   (loc-pat (gethash key regexp-hash)))
	loc-pat)
    nil))

(defun realgud-cmdbuf-loc-hist(cmd-buf)
  "Return the history ring of locations that a debugger
command-process buffer has stored."
  (with-current-buffer-safe cmd-buf
    (realgud-sget 'cmdbuf-info 'loc-hist))
)

(defun realgud-cmdbuf-src-marker(cmd-buf)
  "Return a marker to current source location stored in the history ring."
  (with-current-buffer cmd-buf
    (lexical-let* ((loc (realgud-loc-hist-item (realgud-cmdbuf-loc-hist cmd-buf))))
      (and loc (realgud-loc-marker loc)))))

(defun realgud-cmdbuf-mode-line-update (&optional opt-cmdbuf)
  "Force update of command buffer to include process status"
  (let ((cmdbuf (realgud-get-cmdbuf opt-cmdbuf))
	(debug-status)
	(status)
	(cmd-process)
	)
    (if (and cmdbuf (buffer-name cmdbuf))
	(with-current-buffer cmdbuf
	  (setq cmd-process (get-buffer-process cmdbuf))
	  (setq debug-status
		(if (realgud-sget 'cmdbuf-info 'in-debugger?)
		    " debugger"
		  ""))
	  (setq status
		(if cmd-process
		    (list (propertize
			   (format ":%s%s"
				   (process-status cmd-process) debug-status)
			   'face 'debugger-running))
		  (list (propertize ":not running" 'face
			'debugger-not-running))
		  ))
	  (setq mode-line-process status)
	  ;; Force mode line redisplay soon.
	  (force-mode-line-update))
      ))
  )


(provide-me "realgud-buffer-")

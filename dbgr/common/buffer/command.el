;;; process-command buffer things
;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>

(require 'load-relative)
(require-relative-list
 '("../fringe" "../helper" "../loc" "../lochist" "../regexp") "dbgr-")

(eval-when-compile 
  (byte-compile-disable-warning 'cl-functions)
  ;; Somehow disabling cl-functions causes the erroneous message:
  ;;   Warning: the function `reduce' might not be defined at runtime.
  ;; FIXME: isolate, fix and/or report back to Emacs developers a bug
  (byte-compile-disable-warning 'unresolved)
  (defvar dbgr-cmdbuf-info)
  )
(require 'cl)

(defstruct dbgr-cmdbuf-info
  "The debugger object/structure specific to a process buffer."
  in-srcbuf?           ;; If true, selected window be the source buffer.
		       ;; Otherwise, the command buffer?
  debugger-name        ;; Name of debugger
  frame-switch?        ;; Should the selected window be the source buffer or
		       ;; command buffer?
  cmd-args             ;; Command-line invocation arguments
  prior-prompt-regexp  ;; regular expression prompt (e.g.
                       ;; comint-prompt-regexp) *before* setting
                       ;; loc-regexp
  no-record?           ;; Should we update the location history?
  src-shortkey?        ;; Are source buffers in dbgr-short-key mode?
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

  ;; FIXME: REMOVE THIS and use regexp-hash
  loc-regexp   ;; Location regular expression string
  file-group
  line-group

  loc-hist     ;; ring of locations seen in the course of execution
               ;; see dbgr-lochist
)
(make-variable-buffer-local 'dbgr-cmdbuf-info)
(make-variable-buffer-local 'dbgr-last-output-start)

(defalias 'dbgr-cmdbuf-info? 'dbgr-cmdbuf-info-p)

(defun dbgr-cmdbuf-info-describe ()
  (interactive "")
  (message "debugger-name: %s" 
	   (dbgr-cmdbuf-info-debugger-name dbgr-cmdbuf-info))
  (message "in source or command buffer: %s" 
	   (dbgr-cmdbuf-info-in-srcbuf? dbgr-cmdbuf-info))
  (message "command-line args: %s" 
	   (dbgr-cmdbuf-info-cmd-args dbgr-cmdbuf-info))
  (message "source should go into shortkey?: %s"
	   (dbgr-cmdbuf-info-src-shortkey? dbgr-cmdbuf-info))
  (message "command hash: %s"
	   (dbgr-cmdbuf-info-cmd-hash dbgr-cmdbuf-info))
  (message "backtrace-buffer: %s"
	   (dbgr-cmdbuf-info-bt-buf dbgr-cmdbuf-info))
  )

(defun dbgr-cmdbuf? (&optional buffer)
  "Return true if BUFFER is a debugger command buffer."
  (with-current-buffer-safe 
   (or buffer (current-buffer))
   (dbgr-cmdbuf-info-set?)))

(defun dbgr-cmdbuf-info-set? ()
  "Return true if dbgr-cmdbuf-info is set."
  (and (boundp 'dbgr-cmdbuf-info) 
       dbgr-cmdbuf-info
       (dbgr-cmdbuf-info? dbgr-cmdbuf-info)))

(defun dbgr-cmdbuf-add-srcbuf(srcbuf &optional cmdbuf)
  "Add SRCBUF to srcbuf-list field of INFO unless it is already included."
  (setq cmdbuf (or cmdbuf (current-buffer)))
  (if (dbgr-cmdbuf? cmdbuf)
      (with-current-buffer-safe cmdbuf
	(unless (memq srcbuf (dbgr-cmdbuf-info-srcbuf-list dbgr-cmdbuf-info))
	  (setf (dbgr-cmdbuf-info-srcbuf-list dbgr-cmdbuf-info) 
		(cons srcbuf (dbgr-cmdbuf-info-srcbuf-list dbgr-cmdbuf-info))))
	)
    )
  )

(defun dbgr-cmdbuf-set-shortkey(&optional cmdbuf unset)
  (interactive "")
  (setq cmdbuf (or cmdbuf (current-buffer)))
  (if (dbgr-cmdbuf? cmdbuf)
      (with-current-buffer-safe cmdbuf
	(setf (dbgr-cmdbuf-info-src-shortkey? dbgr-cmdbuf-info) (not unset))
	(message "Set source to shortkey is now %s" (not unset))
	))
  )

;; FIXME: DRY = access via a macro. See also analogous
;; code in dbgr-srcbuf
(defun dbgr-cmdbuf-info-bp-list=(value)
  (if (dbgr-cmdbuf-info? dbgr-cmdbuf-info)
      (setf (dbgr-cmdbuf-info-bp-list dbgr-cmdbuf-info) value)))

(defun dbgr-cmdbuf-info-cmd-args=(value)
  (setf (dbgr-cmdbuf-info-cmd-args dbgr-cmdbuf-info) value))

(defun dbgr-cmdbuf-info-divert-output?=(info value)
  (setf (dbgr-cmdbuf-info-divert-output? info) value))

(defun dbgr-cmdbuf-info-frame-switch?=(info value)
  (setf (dbgr-cmdbuf-info-frame-switch? info) value))

(defun dbgr-cmdbuf-info-in-srcbuf?=(info value)
  (setf (dbgr-cmdbuf-info-in-srcbuf? info) value))

(defun dbgr-cmdbuf-info-no-record?=(info value)
  (setf (dbgr-cmdbuf-info-no-record? info) value))

(defun dbgr-cmdbuf-info-bt-buf=(info value)
  (setf (dbgr-cmdbuf-info-bt-buf info) value))

(defun dbgr-cmdbuf-info-prior-prompt-regexp=(info value)
  (if (dbgr-cmdbuf-info? info)
      (setf (dbgr-cmdbuf-info-prior-prompt-regexp info) value)))

(defun dbgr-cmdbuf-info-src-shortkey?=(info value)
  (setf (dbgr-cmdbuf-info-src-shortkey? info) value))

(defun dbgr-cmdbuf-command-string(cmd-buffer)
  "Get the command string invocation for this command buffer"
    (cond
     ((dbgr-cmdbuf? cmd-buffer)
      (with-current-buffer cmd-buffer
	(let* 
	    ((cmd-args (dbgr-sget 'cmdbuf-info 'cmd-args))
	     (result (car cmd-args)))
	  (and cmd-args 
	       (reduce (lambda(result x)
			 (setq result (concat result " " x)))
		       cmd-args)))))
     (t nil)))

;; FIXME pat-hash should not be optional. And while I am at it, remove
;; parameters loc-regexp, file-group, and line-group which can be found
;; inside pat-hash
;;
;; To do this however we need to fix up the caller
;; dbgr-track-set-debugger by changing dbgr-pat-hash to store a hash
;; rather than the loc, file, and line fields; those fields then get
;; removed.

(defun dbgr-cmdbuf-init
  (cmd-buf debugger-name regexp-hash &optional cmd-hash)
  "Initialize CMD-BUF for a working with a debugger.
DEBUGGER-NAME is the name of the debugger.
as a main program."
  (with-current-buffer-safe cmd-buf
    (let ((dbgr-loc-pat (gethash "loc" regexp-hash))
	  (font-lock-keywords)
	  )
      (setq dbgr-cmdbuf-info
	    (make-dbgr-cmdbuf-info
	     :in-srcbuf? nil
	     :debugger-name debugger-name
	     :loc-regexp (dbgr-sget 'loc-pat 'regexp)
	     :file-group (dbgr-sget 'loc-pat 'file-group)
	     :line-group (dbgr-sget 'loc-pat 'line-group)
	     :loc-hist (make-dbgr-loc-hist)
	     :regexp-hash regexp-hash
	     :cmd-hash cmd-hash
	     :src-shortkey? 't
	     ))
      (setq font-lock-keywords (dbgr-cmdbuf-pat "font-lock-keywords"))
      (if font-lock-keywords
	  (set (make-local-variable 'font-lock-defaults)
	       (list font-lock-keywords)))
      )
    
    (put 'dbgr-cmdbuf-info 'variable-documentation 
	 "Debugger object for a process buffer."))
  )

(defun dbgr-cmdbuf-debugger-name (&optional cmd-buf)
  "Return the debugger name recorded in the debugger command-process buffer."
  (with-current-buffer-safe (or cmd-buf (current-buffer))
    (if (dbgr-cmdbuf?)
	(dbgr-sget 'cmdbuf-info 'debugger-name)
      nil))
  )

(defun dbgr-cmdbuf-pat(key)
  "Extract regexp stored under KEY in a dbgr-cmdbuf via dbgr-cmdbuf-info"
  (if (dbgr-cmdbuf?)
      (let*
	  ((debugger-name (dbgr-cmdbuf-debugger-name))
	   (regexp-hash (gethash debugger-name dbgr-pat-hash))
	   (loc-pat (gethash key regexp-hash)))
	loc-pat)
    nil))

(defun dbgr-cmdbuf-loc-hist(cmd-buf)
  "Return the history ring of locations that a debugger
command-process buffer has stored."
  (with-current-buffer-safe cmd-buf 
    (dbgr-sget 'cmdbuf-info 'loc-hist))
)

(defun dbgr-cmdbuf-src-marker(cmd-buf)
  "Return a marker to current source location stored in the history ring."
  (with-current-buffer cmd-buf
    (lexical-let* ((loc (dbgr-loc-hist-item (dbgr-cmdbuf-loc-hist cmd-buf))))
      (and loc (dbgr-loc-marker loc)))))

(provide-me "dbgr-buffer-")

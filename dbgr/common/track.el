(defconst dbgr-track-char-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

;; Shell process buffers that we can hook into:
(require 'esh-mode) 
(require 'comint)

(require 'load-relative)
(require-relative-list
 '("buffer" "cmdbuf"   "file"   "fringe" 
   "helper" "init"     "loc"    "lochist" 
   "regexp" "shortkey" "srcbuf" "window"
   "bp"
   ) "dbgr-")

(defcustom dbgr-short-key-on-tracing? nil
"If non-nil, set short-key mode for any source buffer that is traced into"
  :type 'symbolp
  :group 'dbgr)

(defvar dbgr-track-mode)
(fn-p-to-fn?-alias 'dbgr-loc-p)
(declare-function dbgr-loc?(loc))

(make-variable-buffer-local 'dbgr-track-mode)

(defun dbgr-track-comint-output-filter-hook(text)
  "An output-filter hook custom for comint shells.  Find
location/s, if any, and run the action(s) associated with
finding a new location/s.  The parameter TEXT appears because it
is part of the comint-output-filter-functions API. Instead we use
marks set in buffer-local variables to extract text"

  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next dbgr prompt, and then
  ;; check all text from comint-last-input-end to process-mark.

  ; FIXME: Add unwind-protect? 
  (if dbgr-track-mode
      (let* ((cmd-buff (current-buffer))
	     (cmd-mark (point-marker))
	     (curr-proc (get-buffer-process cmd-buff))
	     (last-output-end (process-mark curr-proc))
	     (last-output-start (max comint-last-input-end 
				     (- last-output-end dbgr-track-char-range))))
	(dbgr-track-from-region last-output-start 
				last-output-end cmd-mark)))
  )


(defun dbgr-track-eshell-output-filter-hook()
  "An output-filter hook custom for eshell shells.  Find
location(s), if any, and run the action(s) associated with We use
marks set in buffer-local variables to extract text"

  ;; FIXME: Add unwind-protect? 
  (if dbgr-track-mode
      (lexical-let* ((cmd-buff (current-buffer))
		     (cmd-mark (point-marker))
		     (loc (dbgr-track-from-region eshell-last-output-start 
						  eshell-last-output-end cmd-mark)))
	(dbgr-track-loc-action loc cmd-buff)))
  )

(defun dbgr-track-from-region(from to &optional cmd-mark)
  "Find and position a buffer at the location found in the marked region.

You might want to use this function interactively after marking a
region in a debugger-tracked shell buffer (see `dbgr-track-mode')
or a more dedicated debugger command buffer.

The marked region location should match the regexp found in the
buffer-local variable `dbgr-cmdbuf-info' structure under the
field loc-regexp. You can see what this is by
evaluating (dbgr-cmdbuf-info-loc-regexp dbgr-cmdbuf-info)"

  (interactive "r")
  (if (> from to) (psetq to from from to))
  (let* ((text (buffer-substring-no-properties from to))
	 (loc (dbgr-track-loc text cmd-mark))
	 (text-sans-loc)
	 (bp-loc)
	 (cmdbuf (current-buffer))
	 )
    (if (dbgr-cmdbuf? cmdbuf)
	(with-current-buffer cmdbuf
	  (setq text-sans-loc (or (dbgr-track-loc-remaining text) text))
	  (setq bp-loc (dbgr-track-bp-loc text-sans-loc cmd-mark cmdbuf))
	  (if bp-loc 
	      (let ((src-buffer (dbgr-loc-goto bp-loc)))
		(dbgr-cmdbuf-add-srcbuf src-buffer cmdbuf)
		(with-current-buffer src-buffer
		  (dbgr-bp-add-info bp-loc)
		  )))
	  (if loc (dbgr-track-loc-action loc cmdbuf)))
      ;; else
      (error "Buffer %s is not a debugger command buffer" cmdbuf))
    )
  )

(defun dbgr-track-hist-fn-internal(fn)
  "Update both command buffer and a source buffer to reflect the
selected location in the location history. If we started in a
command buffer, we stay in a command buffer. Moving inside a
command buffer always shows the corresponding source
file. However it is possible in shortkey mode to show only the
source code window, even the commmand buffer is updated albeit
unshown."

  (interactive)
  (let ((cmdbuf (dbgr-get-cmdbuf (current-buffer))))
    (if cmdbuf
	(let* ((loc-hist (dbgr-cmdbuf-loc-hist cmdbuf))
	       (srcbuf (dbgr-get-srcbuf-from-cmdbuf cmdbuf)) 
	       (window (selected-window))
	       (position (funcall fn loc-hist))
	       (stay-in-cmdbuf?
		(or (eq (current-buffer) cmdbuf)
		    (with-current-buffer cmdbuf
		      (not (dbgr-sget 'cmdbuf-info 'in-srcbuf?)))))
	       (loc (dbgr-loc-hist-item loc-hist)))
	  (set-buffer (dbgr-loc-goto loc))

	  ;; Make sure command buffer is updated
	  (dbgr-window-update-position cmdbuf 
				       (dbgr-loc-cmd-marker loc))

	  ;; FIXME turn into fn. combine with dbgr-track-loc-action.
	  (if stay-in-cmdbuf?
	      (let ((cmd-window (dbgr-window-src-undisturb-cmd srcbuf)))
		(if cmd-window (select-window cmd-window)))
	    (dbgr-window-src srcbuf)
	  )

	  ;; Make sure source buffer is updated
	  (dbgr-window-update-position srcbuf 
				       (dbgr-loc-marker loc))

	  (message "history position %s line %s" 
		   (dbgr-loc-hist-index loc-hist)
		   (dbgr-loc-line-number loc))
	  (select-window window)))
  ))

;; FIXME: Can we dry code more via a macro?
(defun dbgr-track-hist-newer()
  (interactive)
  (dbgr-track-hist-fn-internal 'dbgr-loc-hist-newer))

(defun dbgr-track-hist-newest()
  (interactive)
  (dbgr-track-hist-fn-internal 'dbgr-loc-hist-newest))

(defun dbgr-track-hist-older()
  (interactive)
  (dbgr-track-hist-fn-internal 'dbgr-loc-hist-older))

(defun dbgr-track-hist-oldest()
  (interactive)
  (dbgr-track-hist-fn-internal 'dbgr-loc-hist-oldest))

(defun dbgr-track-loc-action (loc cmdbuf)
  "If loc is valid, show loc and do whatever actions we do for
encountering a new loc."
  (if (dbgr-loc? loc)
      (let* 
	  ((cmdbuf-loc-hist (dbgr-cmdbuf-loc-hist cmdbuf))
	   (cmdbuf-local-overlay-arrow? 
	    (with-current-buffer cmdbuf 
	      (local-variable-p 'overlay-arrow-variable-list)))
	   (stay-in-cmdbuf?
	    (with-current-buffer cmdbuf
	      (not (dbgr-sget 'cmdbuf-info 'in-srcbuf?))))
	   (shortkey-mode?
	    (with-current-buffer cmdbuf
	      (dbgr-sget 'cmdbuf-info 'src-shortkey?)))
	   (srcbuf)
	   (srcbuf-loc-hist)
	   )

	(setq srcbuf (dbgr-loc-goto loc))
	(dbgr-srcbuf-init-or-update srcbuf cmdbuf)
	(setq srcbuf-loc-hist (dbgr-srcbuf-loc-hist srcbuf))
	(dbgr-cmdbuf-add-srcbuf srcbuf cmdbuf)

	(with-current-buffer srcbuf
	  (dbgr-short-key-mode-setup 
	   (or dbgr-short-key-on-tracing? shortkey-mode?)))

        ;; Do we need to go back to the process/command buffer because other
        ;; output-filter hooks run after this may assume they are in that
        ;; buffer? If so, we may have to use set-buffer rather than 
	;; switch-to-buffer in some cases.
	(set-buffer cmdbuf)

	(unless (dbgr-sget 'cmdbuf-info 'no-record?) 
	  (dbgr-loc-hist-add srcbuf-loc-hist loc)
	  (dbgr-loc-hist-add cmdbuf-loc-hist loc)
	  (dbgr-fringe-history-set cmdbuf-loc-hist cmdbuf-local-overlay-arrow?)
	  )

	;; FIXME turn into fn. combine with dbgr-track-hist-fn-internal
	(if stay-in-cmdbuf?
	    (let ((cmd-window (dbgr-window-src-undisturb-cmd srcbuf)))
	      (with-current-buffer srcbuf
		(if (and (boundp 'dbgr-overlay-arrow1)
			 (markerp dbgr-overlay-arrow1))
		    (dbgr-window-update-position srcbuf dbgr-overlay-arrow1)))
	      (if cmd-window (select-window cmd-window)))
	  (dbgr-window-src srcbuf)
	  )
	)))

(defun dbgr-track-loc(text cmd-mark &optional opt-regexp opt-file-group opt-line-group )
  "Do regular-expression matching to find a file name and line number inside
string TEXT. If we match, we will turn the result into a dbgr-loc struct.
Otherwise return nil."
  
  ; NOTE: dbgr-cmdbuf-info is a buffer variable local to the process running
  ; the debugger. It contains a dbgr-cmdbuf-info "struct". In that struct are
  ; the fields loc-regexp, file-group, and line-group. By setting the
  ; the fields of dbgr-cmdbuf-info appropriately we can accomodate a family
  ; of debuggers -- one at a time -- for the buffer process.

  (if (dbgr-cmdbuf?)
      (let 
	  ((loc-regexp (or opt-regexp 
			   (dbgr-sget 'cmdbuf-info 'loc-regexp)))
	   (file-group (or opt-file-group 
			   (dbgr-sget 'cmdbuf-info 'file-group)))
	   (line-group (or opt-line-group 
			   (dbgr-sget 'cmdbuf-info 'line-group))))
	(if loc-regexp
	    (if (string-match loc-regexp text)
		(let* ((filename (match-string file-group text))
		       (line-str (match-string line-group text)) 
		       (lineno (string-to-number (or line-str "1"))))
		  (unless line-str (message "line number not found -- using 1"))
		  (if (and filename lineno)
		      (dbgr-file-loc-from-line filename lineno cmd-mark)
		    nil)))
	  (and (message (concat "Buffer variable for regular expression pattern not"
			   " given and not passed as a parameter")) nil)))
    (and (message "Current buffer %s is not a debugger command buffer"
		  (current-buffer)) nil)
    ))

(defun dbgr-track-bp-loc(text &optional cmd-mark cmdbuf)
  "Do regular-expression matching to find a file name and line number inside
string TEXT. If we match, we will turn the result into a dbgr-loc struct.
Otherwise return nil."
  
  ; NOTE: dbgr-cmdbuf-info is a buffer variable local to the process
  ; running the debugger. It contains a dbgr-cmdbuf-info "struct". In
  ; that struct is the regexp hash to match positions. By setting the
  ; the fields of dbgr-cmdbuf-info appropriately we can accomodate a
  ; family of debuggers -- one at a time -- for the buffer process.

  (setq cmdbuf (or cmdbuf (current-buffer)))
  (with-current-buffer cmdbuf
    (if (dbgr-cmdbuf?)
	(let* ((loc-pat (dbgr-cmdbuf-pat "brkpt-set")))
	  (if loc-pat
	      (let ((bp-num-group (dbgr-loc-pat-bp-num loc-pat))
		    (loc-regexp   (dbgr-loc-pat-regexp loc-pat))
		    (file-group   (dbgr-loc-pat-file-group loc-pat))
		    (line-group   (dbgr-loc-pat-line-group loc-pat)))
		(if loc-regexp
		    (if (string-match loc-regexp text)
			(let* ((bp-num (match-string bp-num-group text))
			       (filename (match-string file-group text))
			       (line-str (match-string line-group text)) 
			       (lineno (string-to-number (or line-str "1")))
			       )
			  (unless line-str 
			    (message "line number not found -- using 1"))
			  (if (and filename lineno)
			      (let ((loc-or-error
				     (dbgr-file-loc-from-line 
				      filename lineno 
				      cmd-mark 
				      (string-to-number bp-num))))
				(if (stringp loc-or-error)
				    (progn 
				      (message loc-or-error) 
				      ;; set to return nil
				      nil)
				  ;; else
				  (progn 
				    ;; Add breakpoint to list of breakpoints
				    (dbgr-cmdbuf-info-bp-list= 
				     dbgr-cmdbuf-info 
				     (cons loc-or-error (dbgr-sget 'cmdbuf-info 'bp-list)))
				    ;; Set to return location
				    loc-or-error)))
			    nil)))
		  nil))
	    nil))
      (and (message "Current buffer %s is not a debugger command buffer"
		    (current-buffer)) nil)
      )
    )
)

(defun dbgr-track-loc-remaining(text)
  "Return the portion of TEXT starting with the part after the
loc-regexp pattern"
  (if (dbgr-cmdbuf?)
      (let* ((loc-pat (dbgr-cmdbuf-pat "loc"))
	     (loc-regexp (dbgr-loc-pat-regexp loc-pat))
	     )
	(if loc-regexp
	    (if (string-match loc-regexp text)
		(substring text (match-end 0))
	      nil)
	  nil))
    nil)
)
  
(defun dbgr-goto-line-for-loc-pat (pt &optional opt-dbgr-loc-pat)
  "Display the location mentioned in line described by
PT. OPT-DBGR-LOC-PAT is used to get regular-expresion pattern
matching information. If not supplied we use the currnet buffer's \"location\" 
pattern found via dbgr-cmdbuf information."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let* 
	((cmdbuf (current-buffer))
	 (cmd-mark (point-marker))
	 (curr-proc (get-buffer-process cmdbuf))
	 (start (line-beginning-position))
	 (end (line-end-position))
	 (loc-pat (or opt-dbgr-loc-pat (dbgr-cmdbuf-pat "loc")))
	 (loc)
	 )
      (unless (and loc-pat (dbgr-loc-pat-p loc-pat))
	(error "Can't find location information for %s" cmdbuf))
      (setq loc (dbgr-track-loc (buffer-substring-no-properties start end)
				cmd-mark
				(dbgr-loc-pat-regexp loc-pat)
				(dbgr-loc-pat-file-group loc-pat)
				(dbgr-loc-pat-line-group loc-pat)
				))
      (if loc (dbgr-track-loc-action loc cmdbuf)))))

(defun dbgr-track-set-debugger (debugger-name)
  "Set debugger name and information associated with that debugger for
the buffer process. This info is returned or nil if we can't find a 
debugger with that information"
  (interactive "sDebugger name: ")
  (let ((regexp-hash (gethash debugger-name dbgr-pat-hash)))
    (if regexp-hash
	(dbgr-cmdbuf-init (current-buffer) debugger-name regexp-hash)
      (progn 
	(message "I Don't have %s listed as a debugger." debugger-name)
	nil)
      )))

(defun dbgr-goto-line-for-pt-and-type (pt type pat-hash)
  "Display the location mentioned for PT given type PAT-HASH indexed TYPE."
  (dbgr-goto-line-for-loc-pat pt (gethash type pat-hash)))

  
(provide-me "dbgr-")

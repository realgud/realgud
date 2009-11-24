(defconst dbgr-track-char-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

;; Shell process buffers that we can hook into:
(require 'esh-mode) 
(require 'comint)

(require 'load-relative)
(require-relative-list
 '("buffer" "cmdbuf" "file" "fringe" 
   "helper" "init"   "loc"  "lochist" 
   "regexp" "srcbuf" "window" 
   ) "dbgr-")

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
      (lexical-let* ((cmd-buff (current-buffer))
		     (cmd-mark (point-marker))
		     (curr-proc (get-buffer-process cmd-buff))
		     (last-output-end (process-mark curr-proc))
		     (last-output-start (max comint-last-input-end 
					     (- last-output-end dbgr-track-char-range)))
		     (loc (dbgr-track-from-region last-output-start 
						  last-output-end cmd-mark)))
	
	(if loc (dbgr-track-loc-action loc cmd-buff)))))

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
	(dbgr-track-loc-action loc cmd-buff))))

(defun dbgr-track-from-region(from to &optional cmd-mark)
  "Show in another window the location found in the marked region.
The marked region location match the regexp found in buffer-local variable 
`dbgr-cmdbuf-info' structure with field loc-regexp. You can see what this is
by evaluating (dbgr-cmdbuf-info-loc-regexp dbgr-cmdbuf-info)"

  (interactive "r")
  (if (> from to) (psetq to from from to))
  (dbgr-track-loc (buffer-substring from to) cmd-mark))

(defun dbgr-track-hist-fn-internal(fn)
  (interactive)
  (let ((cmd-buff (dbgr-get-cmdbuf (current-buffer))))
    (if cmd-buff
	(let* ((loc-hist (dbgr-cmdbuf-loc-hist cmd-buff))
	       (window (selected-window))
	       (position (funcall fn loc-hist))
	       (loc (dbgr-loc-hist-item loc-hist)))
	  (dbgr-loc-goto loc 'dbgr-split-or-other-window)
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
	      (not (dbgr-sget 'cmdbuf-info 'in-srcbuf))))
	   (srcbuf)
	   (srcbuf-loc-hist)
	   )

	(setq srcbuf (dbgr-loc-goto loc))
	(dbgr-srcbuf-init-or-update srcbuf cmdbuf)
	(setq srcbuf-loc-hist (dbgr-srcbuf-loc-hist srcbuf))
	(dbgr-loc-hist-add srcbuf-loc-hist loc)
	(dbgr-loc-hist-add cmdbuf-loc-hist loc)
	(dbgr-fringe-history-set cmdbuf-loc-hist cmdbuf-local-overlay-arrow?)

        ;; Do we need to go back to the process/command buffer because other
        ;; output-filter hooks run after this may assume they are in that
        ;; buffer. If so, we may have to use set-buffer rather than 
	;; switch-to-buffer in some cases.
	(if stay-in-cmdbuf?
	    (progn
	      (dbgr-split-or-other-window srcbuf)
	      (if (and (boundp 'dbgr-overlay-arrow1)
		       (markerp dbgr-overlay-arrow1))
		  (goto-char dbgr-overlay-arrow1))
	      (switch-to-buffer-other-window cmdbuf))
	  (set-buffer cmdbuf))
	)))

(defun dbgr-track-loc(text &optional cmd-mark opt-regexp 
			   opt-file-group opt-line-group )
  "Do regular-expression matching to find a file name and line number inside
string TEXT. If we match we will turn the result into a dbgr-loc struct.
Otherwise return nil."
  
  ; NOTE: dbgr-cmdbuf-info is a buffer variable local to the process running
  ; the debugger. It contains a dbgr-cmdbuf-info "struct". In that struct are
  ; the fields loc-regexp, file-group, and line-group. By setting the
  ; the fields of dbgr-cmdbuf-info appropriately we can accomodate a family
  ; of debuggers -- one at a time -- for the buffer process.

  (if (dbgr-cmdbuf?)
      (lexical-let 
	  ((loc-regexp (or opt-regexp 
			   (dbgr-sget 'cmdbuf-info 'loc-regexp)))
	   (file-group (or opt-file-group 
			   (dbgr-sget 'cmdbuf-info 'file-group)))
	   (line-group (or opt-line-group 
			   (dbgr-sget 'cmdbuf-info 'line-group))))
	(if (and loc-regexp (string-match loc-regexp text))
	    (lexical-let* ((filename (match-string file-group text))
			   (line-str (match-string line-group text)) 
			   (lineno (string-to-number (or line-str "1"))))
	      (unless line-str (message "line number not found -- using 1"))
	      (if (and filename lineno)
		  (dbgr-file-loc-from-line filename lineno cmd-mark)
		nil))
	  nil))))
  
(defun dbgr-goto-line-for-loc-pat (pt loc-pat)
  "Display the location mentioned in line described by PT. LOC-PAT is used
to get regular-expresion pattern matching information."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (lexical-let* ((cmd-buff (current-buffer))
		   (cmd-mark (point-marker))
		   (curr-proc (get-buffer-process cmd-buff))
		   (start (line-beginning-position))
		   (end (line-end-position))
		   ;; FIXME check that loc-pat is not null and abort if it is.
		   (loc (dbgr-track-loc (buffer-substring start end)
					cmd-mark
					(dbgr-sget 'loc-pat 'regexp)
					(dbgr-sget 'loc-pat 'file-group)
					(dbgr-sget 'loc-pat 'line-group)
					)))
    (if loc (dbgr-track-loc-action loc cmd-buff)))))

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

;;; Local variables:
;;; End:

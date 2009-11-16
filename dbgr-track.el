;;; dbgr-track.el --- Debugger tracking a comint or eshell buffer.

(defconst dbgr-track-char-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

;; Shell process buffers that we can hook into:
(require 'esh-mode) 
(require 'comint)

(require 'load-relative)
(require-relative-list
 '("dbgr-helper" "dbgr-arrow" "dbgr-loc" "dbgr-lochist" 
   "dbgr-file" "dbgr-cmdbuf" "dbgr-srcbuf" "dbgr-window" 
   "dbgr-regexp"))

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

(defun dbgr-track-from-region(from to cmd-mark)
  (interactive "r")
  (if (> from to) (psetq to from from to))
  (dbgr-track-loc (buffer-substring from to) cmd-mark))

(defun dbgr-track-hist-fn-internal(fn)
  (interactive)
  (lexical-let* 
      ((cmd-buff (current-buffer))
       (loc-hist (dbgr-proc-loc-hist cmd-buff))
       (cmd-window (selected-window))
       (position (funcall fn loc-hist))
       (loc (dbgr-loc-hist-item loc-hist))
       )
    (dbgr-loc-goto loc 'dbgr-split-or-other-window)
    (message "history position %s line %s" 
	     (dbgr-loc-hist-index loc-hist)
	     (dbgr-loc-line-number loc))
    (select-window cmd-window)
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

(defun dbgr-track-loc-action (loc cmd-buff)
  "If loc is valid, show loc and do whatever actions we do for
encountering a new loc."
  (if (dbgr-loc? loc)
      (lexical-let* 
	  ((loc-hist (dbgr-proc-loc-hist cmd-buff))
	   (prev-marker (dbgr-proc-src-marker cmd-buff))
	   (src-buff))

	(if prev-marker (dbgr-unset-arrow (marker-buffer prev-marker)))
	(setq src-buff (dbgr-loc-goto loc 'dbgr-split-or-other-window))

	(dbgr-srcbuf-init-or-update src-buff cmd-buff)

        ;; We need to go back to the process/command buffer because other
        ;; output-filter hooks run after this may assume they are in that
        ;; buffer.
	(switch-to-buffer-other-window cmd-buff)

	;; hist add has to be done in cmd-buff since history is 
	;; buffer-local
	(dbgr-loc-hist-add loc-hist loc)
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
			   (dbgr-cmdbuf-info-loc-regexp dbgr-cmdbuf-info)))
	   (file-group (or opt-file-group 
			   (dbgr-cmdbuf-info-file-group dbgr-cmdbuf-info)))
	   (line-group (or opt-line-group 
			   (dbgr-cmdbuf-info-line-group dbgr-cmdbuf-info))))
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
					(dbgr-loc-pat-regexp loc-pat)
					(dbgr-loc-pat-file-group loc-pat)
					(dbgr-loc-pat-line-group loc-pat)
					)))
    (if loc (dbgr-track-loc-action loc cmd-buff)))))

(defun dbgr-track-set-debugger (debugger-name &optional regexp-hash)
  "Set debugger name and information associated with that debugger for
the buffer process. This info is returned or nil if we can't find a 
debugger with that information"
  (interactive "sDebugger name: ")
  (let 
      ((loc-pat (gethash debugger-name dbgr-pat-hash)))
    (if loc-pat 
	(dbgr-cmdbuf-init (current-buffer) debugger-name 
			  (dbgr-loc-pat-regexp loc-pat) 
			  (dbgr-loc-pat-file-group loc-pat)
			  (dbgr-loc-pat-line-group loc-pat)
			  regexp-hash)
      (progn 
	(message "I Don't have %s listed as a debugger." debugger-name)
	nil)
      )))

(defun dbgr-goto-line-for-pt-and-type (pt type pat-hash)
  "Display the location mentioned for PT given type PAT-HASH indexed TYPE."
  (dbgr-goto-line-for-loc-pat pt (gethash type pat-hash)))

  
(provide 'dbgr-track)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-track.el ends here

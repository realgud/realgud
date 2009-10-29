;;; dbgr-track.el --- Debugger tracking a comint or eshell buffer.

;; -------------------------------------------------------------------
;; Variables.
;;

(defconst dbgr-track-char-range 10000
  "Max number of characters from end of buffer to search for stack entry.")


;; -------------------------------------------------------------------
;; Dependencies.
;;

(require 'comint)

; For eshell-output-filter-functions, eshell-last-input-start:
(require 'esh-mode) 

(eval-when-compile
  (require 'cl)
  (setq load-path (cons nil (cons ".." load-path)))
  (load "dbgr-loc")
  (load "dbgr-lochist")
  (load "dbgr-file")
  (load "dbgr-procbuf-var")
  (load "dbgr-window")
  (load "dbgr-regexp")
  (setq load-path (cddr load-path)))
(require 'dbgr-file)
(require 'dbgr-loc)
(require 'dbgr-regexp)

(make-variable-buffer-local 'dbgr-info)
(defvar dbgr-info (make-dbgr-info
		    :name "unknown-debugger-name"
		    :loc-regexp nil
		    :file-group -1
		    :line-group -1
		    :loc-hist   nil)
  "Debugger object for a process buffer.")

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
  (lexical-let* ((proc-buff (current-buffer))
		 (proc-window (selected-window))
		 (curr-proc (get-buffer-process proc-buff))
		 (last-output-end (process-mark curr-proc))
		 (last-output-start (max comint-last-input-end 
				   (- last-output-end dbgr-track-char-range)))
		 (loc (dbgr-track-from-region last-output-start 
					       last-output-end)))

    (if loc (dbgr-track-loc-action loc proc-buff proc-window))))

(defun dbgr-track-eshell-output-filter-hook()
  "An output-filter hook custom for eshell shells.  Find
location(s), if any, and run the action(s) associated with We use
marks set in buffer-local variables to extract text"

  ; FIXME: Add unwind-protect? 
  (lexical-let ((proc-buff (current-buffer))
		(proc-window (selected-window))
		(loc (dbgr-track-from-region eshell-last-output-start 
					      eshell-last-output-end)))
    (dbgr-track-loc-action loc proc-buff proc-window)))

(defun dbgr-track-from-region(from to)
  (interactive "r")
  (if (> from to) (psetq to from from to))
  (dbgr-track-loc (buffer-substring from to)))

(defun dbgr-track-hist-fn-internal(fn)
  (interactive)
  (lexical-let* ((loc-hist (dbgr-info-loc-hist dbgr-info))
	 (cmd-window (selected-window))
	 (cmd-buff (current-buffer))
	 (position (funcall fn loc-hist))
	 (loc (dbgr-loc-hist-item loc-hist)))
    (dbgr-loc-goto loc 'dbgr-split-or-other-window)
    (message "history position %s line %s" 
	     (dbgr-loc-hist-index loc-hist)
	     (dbgr-loc-line-number loc))
    ; FIXME: Combine common code with loc-action? 
    ; See also comments why we do the below there.
    (set-buffer cmd-buff)
    (select-window cmd-window))
  )

; FIXME: Can we dry code more via a macro?
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

(defun dbgr-track-loc-action(loc &optional cmd-buff cmd-window)
  "If loc is valid, show loc and do whatever actions we do for
encountering a new loc."
  (if (dbgr-loc-p loc)
      (progn 
	(if (null cmd-buff) (setq cmd-buff (current-buffer)))
	(if (null cmd-window) (setq cmd-window (selected-window)))
	
	(dbgr-loc-goto loc 'dbgr-split-or-other-window)

        ; We need to go back to the process/command buffer because other
        ; output-filter hooks run after this may assume they are in that
        ; buffer.
	(set-buffer cmd-buff)

	; hist add has to be done in cmd-buff since dbgr-dbgr
	(dbgr-loc-hist-add (dbgr-info-loc-hist dbgr-info) loc)

        ; I like to stay on the debugger prompt rather than the found
        ; source location. Folks like Anders (who would like to totally
        ; get rid of the command line) no doubt feel differently about this.
        (select-window cmd-window))
    (message "%s" loc)))

(defun dbgr-track-loc(text &optional opt-regexp opt-file-group opt-line-group)
  "Do regular-expression matching to find a file name and line number inside
string TEXT. If we match we will turn the result into a dbgr-loc struct.
Otherwise return nil."
  
  ; NOTE: dbgr-info is a buffer variable local to the process running
  ; the debugger. It contains a dbgr-info "struct". In that struct are
  ; the fields loc-regexp, file-group, and line-group. By setting the
  ; the fields of dbgr-info appropriately we can accomodate a family
  ; of debuggers -- one at a time -- for the buffer process.

  (lexical-let ((loc-regexp (or opt-regexp (dbgr-info-loc-regexp dbgr-info)))
		(file-group (or opt-file-group (dbgr-info-file-group dbgr-info)))
		(line-group (or opt-line-group (dbgr-info-line-group dbgr-info))))
    (if (and loc-regexp (string-match loc-regexp text))
	(lexical-let* ((filename (match-string file-group text))
		       (line-str (match-string line-group text)) 
		       (lineno (string-to-number (or line-str "1"))))
	  (unless line-str (message "line number not found -- using 1"))
	  (if (and filename lineno)
	      (dbgr-file-loc-from-line filename lineno)
	    nil))
      nil)))
  
(defun dbgr-track-set-debugger (debugger-name)
  (interactive "sDebugger name: ")
  (lexical-let ((loc-pat (gethash debugger-name dbgr-pat-hash)))
    (if loc-pat 
	(setq dbgr-info
	     (make-dbgr-info
	      :name debugger-name
	      :loc-regexp (dbgr-loc-pat-regexp loc-pat)
	      :file-group (dbgr-loc-pat-file-group loc-pat)
	      :line-group (dbgr-loc-pat-line-group loc-pat)
	      :loc-hist   (make-dbgr-loc-hist)))
      (message "I Don't have %s listed as a debugger." debugger-name))))
  
;; -------------------------------------------------------------------
;; The end.
;;

(provide 'dbgr-track)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-track.el ends here

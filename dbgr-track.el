;;; dbgr-track.el --- Debugger tracking a comint or eshell buffer.

;; -------------------------------------------------------------------
;; Variables.
;;

(defconst dbgr-track-char-range 10000
  "Max number of characters from end of buffer to search for stack entry.")


;; -------------------------------------------------------------------
;; Dependencies.
;;

(eval-when-compile (require 'cl))
(require 'comint)

; For eshell-output-filter-functions, eshell-last-input-start:
(require 'esh-mode) 

(require 'load-relative)

(provide 'dbgr-track)
(load-relative "dbgr-loc" 'dbgr-track)
(load-relative "dbgr-lochist" 'dbgr-track)
(load-relative "dbgr-file" 'dbgr-track)
(load-relative "dbgr-procbuf" 'dbgr-track)
(load-relative "dbgr-scriptbuf" 'dbgr-track)
(load-relative "dbgr-window" 'dbgr-track)
(load-relative "dbgr-regexp" 'dbgr-track)

(declare-function dbgr-proc-src-marker ())
(declare-function dbgr-procbuf-init (a &optional b c d e))
(declare-function dbgr-scriptbuf-init-or-update (a b))

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
      (lexical-let* ((proc-buff (current-buffer))
		     (curr-proc (get-buffer-process proc-buff))
		     (last-output-end (process-mark curr-proc))
		     (last-output-start (max comint-last-input-end 
					     (- last-output-end dbgr-track-char-range)))
		     (loc (dbgr-track-from-region last-output-start 
						  last-output-end)))
	
	(if loc (dbgr-track-loc-action loc proc-buff)))))

(defun dbgr-track-eshell-output-filter-hook()
  "An output-filter hook custom for eshell shells.  Find
location(s), if any, and run the action(s) associated with We use
marks set in buffer-local variables to extract text"

  ; FIXME: Add unwind-protect? 
  (if dbgr-track-mode
      (lexical-let ((proc-buff (current-buffer))
		    (loc (dbgr-track-from-region eshell-last-output-start 
						 eshell-last-output-end)))
	(dbgr-track-loc-action loc proc-buff))))

(defun dbgr-track-from-region(from to)
  (interactive "r")
  (if (> from to) (psetq to from from to))
  (dbgr-track-loc (buffer-substring from to)))

(defun dbgr-track-hist-fn-internal(fn)
  (interactive)
  (lexical-let* 
      ((cmd-buff (current-buffer))
       (loc-hist (dbgr-proc-loc-hist cmd-buff))
       (cmd-window (selected-window))
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

(defun dbgr-track-loc-action (loc proc-buff)
  "If loc is valid, show loc and do whatever actions we do for
encountering a new loc."
  (if (dbgr-loc-p loc)
      (lexical-let* 
	  ((loc-hist (dbgr-proc-loc-hist proc-buff))
	   (prev-marker (dbgr-proc-src-marker proc-buff))
	   (src-buff))

	(if prev-marker (dbgr-unset-arrow (marker-buffer prev-marker)))
	(setq src-buff (dbgr-loc-goto loc 'dbgr-split-or-other-window))

	(dbgr-scriptbuf-init-or-update src-buff proc-buff)

        ;; We need to go back to the process/command buffer because other
        ;; output-filter hooks run after this may assume they are in that
        ;; buffer.
	(switch-to-buffer-other-window proc-buff)

	;; hist add has to be done in proc-buff since history is 
	;; buffer-local
	(dbgr-loc-hist-add loc-hist loc)
	)))

(defun dbgr-track-loc(text &optional opt-regexp opt-file-group opt-line-group)
  "Do regular-expression matching to find a file name and line number inside
string TEXT. If we match we will turn the result into a dbgr-loc struct.
Otherwise return nil."
  
  ; NOTE: dbgr-info is a buffer variable local to the process running
  ; the debugger. It contains a dbgr-info "struct". In that struct are
  ; the fields loc-regexp, file-group, and line-group. By setting the
  ; the fields of dbgr-info appropriately we can accomodate a family
  ; of debuggers -- one at a time -- for the buffer process.

  (if (and (boundp 'dbgr-info) dbgr-info)
      (lexical-let 
	  ((loc-regexp (or opt-regexp (dbgr-info-loc-regexp dbgr-info)))
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
	  nil))))
  
(defun dbgr-goto-line-for-loc-pat (pt loc-pat)
  "Display the location mentioned in line described by PT. LOC-PAT is used
to get regular-expresion pattern matching information."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (lexical-let* ((proc-buff (current-buffer))
		   (curr-proc (get-buffer-process proc-buff))
		   (start (line-beginning-position))
		   (end (line-end-position))
		   ;; FIXME check that loc-pat is not null and abort if it is.
		   (loc (dbgr-track-loc (buffer-substring start end)
					 (dbgr-loc-pat-regexp loc-pat)
					 (dbgr-loc-pat-file-group loc-pat)
					 (dbgr-loc-pat-line-group loc-pat)
					 )))
    (if loc (dbgr-track-loc-action loc proc-buff)))))

(defun dbgr-track-set-debugger (debugger-name)
  "Set debugger name and information associated with that debugger for
the buffer process. This info is returned or nil if we can't find a 
debugger with that information"
  (interactive "sDebugger name: ")
  (lexical-let ((loc-pat (gethash debugger-name dbgr-pat-hash)))
    (if loc-pat 
	(dbgr-procbuf-init (current-buffer) debugger-name 
			   (dbgr-loc-pat-regexp loc-pat) 
			   (dbgr-loc-pat-file-group loc-pat)
			   (dbgr-loc-pat-line-group loc-pat))
      (progn 
	(message "I Don't have %s listed as a debugger." debugger-name)
	nil)
      )))

(defun goto-line-for-pt-and-type (pt type pat-hash)
  "Display the location mentioned for PT given type PAT-HASH indexed TYPE."
  (dbgr-goto-line-for-loc-pat pt (gethash type pat-hash)))

  
;; -------------------------------------------------------------------
;; The end.
;;

(provide 'dbgr-track)

;;; Local variables:
;;; eval:(put 'dbgr-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; dbgr-track.el ends here

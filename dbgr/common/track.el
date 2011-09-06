(defconst dbgr-track-char-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

;; Shell process buffers that we can hook into:
(require 'esh-mode) 
(require 'comint)

(require 'load-relative)
(require-relative-list
 '("file"           "fringe" 
   "helper"         "init"     "loc"    "lochist" 
   "regexp"         "shortkey" "window"
   "bp"
   ) "dbgr-")

(require-relative-list
 '("buffer/command" "buffer/helper" "buffer/source") "dbgr-buffer-")

(defcustom dbgr-short-key-on-tracing? nil
"If non-nil, set short-key mode for any source buffer that is traced into"
  :type 'symbolp
  :group 'dbgr)

(defvar dbgr-track-mode)
(fn-p-to-fn?-alias 'dbgr-loc-p)
(declare-function dbgr-loc?(loc))

(make-variable-buffer-local 'dbgr-track-mode)
(defvar dbgr-track-divert-string)

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
  (if (and dbgr-track-mode (dbgr-cmdbuf? (current-buffer)))
      (let* ((cmd-buff (current-buffer))
	     (cmd-mark (point-marker))
	     (curr-proc (get-buffer-process cmd-buff))
	     (last-output-end (process-mark curr-proc))
	     (last-output-start (max comint-last-input-end 
				     (- last-output-end dbgr-track-char-range))))
	;; Sometimes e get called twice and the second time nothing
	;; Changes. Guard against this.
	(unless (= last-output-start last-output-end)
	  (dbgr-track-from-region last-output-start 
				  last-output-end cmd-mark cmd-buff
				  't 't))
	)
    )
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
	(dbgr-track-loc-action loc cmd-buff 't)))
  )

(defun dbgr-track-from-region(from to &optional cmd-mark opt-cmdbuf
				   shortkey-on-tracing? no-warn-if-no-match?)
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
	 (loc (dbgr-track-loc text cmd-mark nil nil nil no-warn-if-no-match?))
	 ;; If we see a selected frame number, it is stored
	 ;; in frame-num. Otherwise, nil.
	 (frame-num)  
	 (text-sans-loc)
	 (bp-loc)
	 (cmdbuf (or opt-cmdbuf (current-buffer)))
	 )
    (if (dbgr-cmdbuf? cmdbuf)
	(if (not (equal "" text))
	    (with-current-buffer cmdbuf
	      (if (dbgr-sget 'cmdbuf-info 'divert-output?)
		  (dbgr-track-divert-prompt text cmdbuf to))
	      ;; FIXME: instead of these fixed filters, 
	      ;; put into a list and iterate over that.
	      (dbgr-track-termination? text)
	      (setq text-sans-loc (or (dbgr-track-loc-remaining text) text))
	      (setq frame-num (dbgr-track-selected-frame text) text)
	      (setq bp-loc (dbgr-track-bp-loc text-sans-loc cmd-mark cmdbuf))
	      (if bp-loc 
		  (let ((src-buffer (dbgr-loc-goto bp-loc)))
		    (dbgr-cmdbuf-add-srcbuf src-buffer cmdbuf)
		    (with-current-buffer src-buffer
		      (dbgr-bp-add-info bp-loc)
		      )))
	      (if loc 
		  (let ((selected-frame 
			 (or (not frame-num) 
			     (eq frame-num (dbgr-cmdbuf-pat "top-frame-num")))))
		    (dbgr-track-loc-action loc cmdbuf (not selected-frame)
					   shortkey-on-tracing?)
		    (dbgr-cmdbuf-info-in-debugger?= 't)
		    (dbgr-cmdbuf-mode-line-update)
		    )
		)
	      )
	  )
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

  (let ((cmdbuf (dbgr-get-cmdbuf (current-buffer))))
    (if cmdbuf
	(let* ((loc-hist (dbgr-cmdbuf-loc-hist cmdbuf))
	       (window (selected-window))
	       (position (funcall fn loc-hist))
	       (stay-in-cmdbuf?
		(or (eq (current-buffer) cmdbuf)
		    (with-current-buffer cmdbuf
		      (not (dbgr-sget 'cmdbuf-info 'in-srcbuf?)))))
	       (loc (dbgr-loc-hist-item loc-hist))
	       (srcbuf (dbgr-get-srcbuf-from-cmdbuf cmdbuf loc)) 
	       )
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

(defun dbgr-track-loc-action (loc cmdbuf &optional not-selected-frame
				  shortkey-on-tracing?)
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
	   (and shortkey-on-tracing? 
		(or dbgr-short-key-on-tracing? shortkey-mode?))
	   ))

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
		    (progn 
		      ;; Doesn't work
		      ;; (if not-selected-frame
		      ;; 	  (set-fringe-bitmap-face 'hollow-right-triangle 
		      ;; 				  'dbgr-overlay-arrow1)
		      ;; 			; else 
		      ;; 	(set-fringe-bitmap-face 'dbgr-right-triangle1 
		      ;; 				'dbgr-overlay-arrow1)
		      ;; 	)
		      (dbgr-window-update-position srcbuf dbgr-overlay-arrow1)))
		)
	      (if cmd-window (select-window cmd-window)))
	  ; else
	  (with-current-buffer srcbuf
	    (dbgr-window-src srcbuf)
	    (dbgr-window-update-position srcbuf dbgr-overlay-arrow1))
	  )
	))
  )

(defun dbgr-track-loc(text cmd-mark &optional opt-regexp opt-file-group 
			   opt-line-group no-warn-on-no-match?)
  "Do regular-expression matching to find a file name and line number inside
string TEXT. If we match, we will turn the result into a dbgr-loc struct.
Otherwise return nil."
  
  ;; NOTE: dbgr-cmdbuf-info is a buffer variable local to the process running
  ;; the debugger. It contains a dbgr-cmdbuf-info "struct". In that struct are
  ;; the fields loc-regexp, file-group, and line-group. By setting the
  ;; the fields of dbgr-cmdbuf-info appropriately we can accomodate a family
  ;; of debuggers -- one at a time -- for the buffer process.

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
		    nil))
	      (unless no-warn-on-no-match? 
		(message "Unable to file and line number for given line"))
	      )
	  (and (message (concat "Buffer variable for regular expression pattern not"
				" given and not passed as a parameter")) nil)))
    (and (message "Current buffer %s is not a debugger command buffer"
		  (current-buffer)) nil)
    )
  )
  
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
	      (let ((bp-num-group (dbgr-loc-pat-num loc-pat))
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
  
(defun dbgr-track-selected-frame(text)
  "Return a selected frame number found in TEXT or nil if none found."
  (if (dbgr-cmdbuf?)
      (let ((selected-frame-pat (dbgr-cmdbuf-pat "selected-frame"))
	    (frame-num-regexp)
	    )
	(if (and selected-frame-pat 
		 (setq frame-num-regexp (dbgr-loc-pat-regexp 
					 selected-frame-pat)))
	    (if (string-match frame-num-regexp text)
		(let ((frame-num-group (dbgr-loc-pat-num selected-frame-pat)))
		  (string-to-number (match-string frame-num-group text)))
	      nil)
	  nil))
    nil)
  )


(defun dbgr-track-termination?(text)
  "Return 't and call dbgr-terminate-cmdbuf we we have a termination message"
  (if (dbgr-cmdbuf?)
      (let ((termination-re (dbgr-cmdbuf-pat "termination"))
	    )
	(if (and termination-re (string-match termination-re text))
	    (progn 
	      (dbgr-terminate (current-buffer))
	      't)
	  nil)
	)
    )
  )
  
(defun dbgr-track-divert-prompt(text cmdbuf to)
  "Return a cons node of the part before the prompt-regexp and the part 
   after the prompt-regexp-prompt. If not found return nil."
  (with-current-buffer cmdbuf
    ;; (message "+++3 %s, buf: %s" text (buffer-name))
    (if (dbgr-cmdbuf?)
	(let* ((prompt-pat (dbgr-cmdbuf-pat "prompt"))
	       (prompt-regexp (dbgr-loc-pat-regexp prompt-pat))
	       )
	  (if prompt-regexp
	      (if (string-match prompt-regexp text)
		  (progn 
		    (setq dbgr-track-divert-string 
			  (substring text 0 (match-beginning 0)))
		    ;; We've got desired output, so reset divert output.
		    (dbgr-cmdbuf-info-divert-output?= nil)
		    (kill-region dbgr-last-output-start to)
		    ;; FIXME: DELETE output. Or do elsewhere?
		    )
	      ))
	  )
      )
    )
  )
  
(defun dbgr-goto-line-for-loc-pat (pt &optional opt-dbgr-loc-pat)
  "Display the location mentioned in line described by
PT. OPT-DBGR-LOC-PAT is used to get regular-expresion pattern
matching information. If not supplied we use the current buffer's \"location\" 
pattern found via dbgr-cmdbuf information. nil is returned if we can't
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
      (if (stringp loc)
	  (message loc)
	(if loc (or (dbgr-track-loc-action loc cmdbuf) 't)
	  nil))
      ))
    )

(defun dbgr-track-set-debugger (debugger-name)
  "Set debugger name and information associated with that debugger for
the buffer process. This info is returned or nil if we can't find a 
debugger with that information"
  (interactive "sDebugger name: ")
  (let ((regexp-hash (gethash debugger-name dbgr-pat-hash))
	(command-hash (gethash debugger-name dbgr-command-hash))
	)
    (if regexp-hash
	(let* ((prefix 
		(cond 
		 ((equal debugger-name "gdb") "dbgr-gdb")
		 ((equal debugger-name "perldb") "dbgr-perldb")
		 ((equal debugger-name "trepanpl") "dbgr-trepanpl")
		 ('t debugger-name)))
	       (specific-track-mode (intern (concat prefix "-track-mode")))
	       )
	  (dbgr-cmdbuf-init (current-buffer) debugger-name regexp-hash
			    command-hash)
	  (if (and (not (eval specific-track-mode))
		   (functionp specific-track-mode))
	      (funcall specific-track-mode 't))
	  )
      (progn 
	(message "I Don't have %s listed as a debugger." debugger-name)
	nil)
      )))

;; FIXME: need better name for this and next fn.
(defun dbgr-goto-line-for-pt-and-type (pt type pat-hash)
  "Display the location mentioned for PT given type PAT-HASH indexed TYPE."
  (dbgr-goto-line-for-loc-pat pt (gethash type pat-hash)))


(defun dbgr-goto-line-for-pt (pt pattern-key)
  "Display the location mentioned by a backtrace line (e.g. Ruby $!)
described by PT."
  (interactive "d")
  (unless (dbgr-cmdbuf?)
    (error "You need to be in a debugger command buffer to run this"))
  (let* ((debugger-name (dbgr-cmdbuf-debugger-name))
	 (debugger-pat-hash (gethash debugger-name dbgr-pat-hash)))
    (dbgr-goto-line-for-pt-and-type pt pattern-key debugger-pat-hash)
    )
  )
  
(defun dbgr-goto-debugger-backtrace-line (pt)
  "Display the location mentioned by the debugger backtrace line
described by PT."
  (interactive "d")
  (unless (dbgr-goto-line-for-pt pt "debugger-backtrace")
    (message "Didn't match a debugger backtrace location.")
    ))

(defun dbgr-goto-lang-backtrace-line (pt)
  "Display the location mentioned by the programming-language backtrace line
described by PT."
  (interactive "d")
  (unless (dbgr-goto-line-for-pt pt "lang-backtrace")
    (message "Didn't match a programming-language backtrace location.")
    ))

(provide-me "dbgr-")

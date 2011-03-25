;;; Copyright (C) 2011 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))
  
(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/core") "dbgr-")
(require-relative-list '("init") "dbgr-remake-")

;; FIXME: I think the following could be generalized and moved to 
;; dbgr-... probably via a macro.
(defvar remake-minibuffer-history nil
  "minibuffer history list for the command `remake'.")

(easy-mmode-defmap remake-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun remake-query-cmdline (&optional opt-debugger)
  (dbgr-query-cmdline 
   'remake-suggest-invocation
   remake-minibuffer-local-map
   'remake-minibuffer-history
   opt-debugger))

(defun remake-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing

- the command processor (e.g. make)
- the Makefile name
- command args (which includes the makefile name)

For example for the following input 
  '(\"gmake\" \"-x\" \"/tmp/Makefile\")

we might return:
   (\"gmake\" \"/tmp/Makefile\" (\"-x\" \"/tmp/Makefile\"))

"

  (let (
	(args orig-args)
	(interp-regexp 
	 (if (member system-type (list 'windows-nt 'cygwin 'msdos))
	     "^\\(re\\)?make*\\(.exe\\)?$"
	   "^\\(re\\)?make*$"))

	;; Things returned
	(remake-name nil)
	(makefile-name nil)
	(remake-args '())
	)

    (if (not (and args))
	;; Got nothing
	(list remake-name makefile-name remake-args)
      ;; else
      ;; Strip off "make" or "remake" etc.
      (when (string-match interp-regexp
			  (file-name-sans-extension
			   (file-name-nondirectory (car args))))
	(setq remake-name (pop args))
	)

      ;; parse options
      (while args
	(let ((arg (pop args)))
	  (cond
	   ;; ;; Annotation or emacs option with level number.
	   ;; ((or (member arg '("--annotate" "-A"))
	   ;; 	(equal arg "--emacs"))
	   ;;  (setq annotate-p t)
	   ;;  (nconc debugger-args (list (pop args))))
	   ;; ;; Combined annotation and level option.
	   ;; ((string-match "^--annotate=[0-9]" arg)
	   ;;  (nconc debugger-args (list (pop args)) )
	   ;;  (setq annotate-p t))

	   ((member arg '("--file" "--makefile" "-f"))
	    (setq remake-args (nconc remake-args (list arg)))
	    (setq makefile-name (pop args))
	    (setq remake-args (nconc remake-args 
				     (list (format "%s" makefile-name)))))

	   ;; Anything else add to remake-args
	   ('t (setq remake-args (nconc remake-args (list arg))))
	   )))
      (list remake-name makefile-name remake-args))))

(defun remake-suggest-Makefile ()
 "Suggest a Makefile to debug. 

The first priority is given to the current buffer. If the major
mode matches GNUMakefile, then we are done. If not, we'll set
priority 2 (a low or easily overridden priority) and we keep
going.  Then we will try files in the default-directory. Of those
that we are visiting we check the major mode. The first one we
find we will return.  Failing this, we see if the file is
and matches REGEXP. These have priority 8.  Within a given
priority, we use the first one we find."
    (let* ((file)
	   (file-list (directory-files default-directory))
	   (makefile-regexp "\\(^[Mm]akefile$\\|\\.Makefile$\\|\\.mk\\)$")
	   (priority 2)
	   (is-not-directory)
	   (result (buffer-file-name)))
      (if (not (dbgr-lang-mode? result "makefile"))
	  (progn 
	    (while (and (setq file (car-safe file-list)) (< priority 8))
	      (setq file-list (cdr file-list))
	      (if (dbgr-lang-mode? file "makefile")
		  (progn 
		    (setq result file)
		    (if (string-match makefile-regexp file)
			(setq priority 8)
		      (setq priority 7))
		    ))
	      ;; The file isn't in a makefile-mode buffer,
	      ;; Check for an executable file with a .mk extension.
	      (if (setq is-not-directory (not (file-directory-p file)))
		  (if (and (string-match makefile-regexp file))
		      (if (< priority 6)
			  (progn
			    (setq result file)
			    (setq priority 6)))))
	    ;; (if (and (< priority 6)
	    ;; 	     (setq file (dbgr-suggest-file-from-buffer lang-str)))
	    ;; 	(setq result file))
	    )))
      result)
    )

(defvar remake-command-name) ; # To silence Warning: reference to free variable

(defun remake-suggest-invocation (debugger-name)
  "Suggest a remake command invocation via `dbgr-suggest-invocaton'"

  (let* ((buf (current-buffer))
	 (cmd-str-cmdbuf (dbgr-cmdbuf-command-string buf))
	 (cmd-str-srcbuf (dbgr-srcbuf-command-string buf))
	 )
    (cond
     ((and cmd-str-cmdbuf (equal debugger-name (dbgr-cmdbuf-debugger-name buf)))
      cmd-str-cmdbuf)
     ((and cmd-str-srcbuf (equal debugger-name (dbgr-srcbuf-debugger-name buf)))
      cmd-str-srcbuf)
     ((and minibuffer-history (listp minibuffer-history)) 
      (car minibuffer-history))
     (t (concat debugger-name " --debugger -f " 
		(remake-suggest-Makefile)))
     )))

;; Convert a command line as would be typed normally to run a script
;; into one that invokes an Emacs-enabled debugging session.
;; "--debugger" in inserted as the first switch.

(defun dbgr-remake-massage-args (command-line)
  (let* ((new-args (list "--debugger"))
	 (args (split-string-and-unquote command-line))
	 (program (car args))
	 (seen-e nil)
	 (shift (lambda ()
	 	  (setq new-args (cons (car args) new-args))
	 	  (setq args (cdr args)))))
    
    ;; Pass all switches and -e scripts through.
    (while (and args
    		(string-match "^-" (car args))
    		(not (equal "-" (car args)))
    		(not (equal "--" (car args))))
      (funcall shift))
    
    (if (or (not args)
    	    (string-match "^-" (car args)))
    	(error "Can't use stdin as the script to debug"))
    ;; This is the program name.
    (funcall shift)
    
    (while args
      (funcall shift))
    
    (nreverse new-args)
    )
  )

(defun remake-goto-backtrace-line (pt)
  "Display the location mentioned by the remake backtrace line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "backtrace" dbgr-remake-pat-hash))

(defun remake-goto-control-frame-line (pt)
  "Display the location mentioned by a control-frame line
described by PT."
  (interactive "d")
  (dbgr-goto-line-for-pt-and-type pt "control-frame" dbgr-remake-pat-hash))

(defun remake-reset ()
  "Remake cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (remake-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*remake-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun remake-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'remake-debugger-support-minor-mode minor-mode-map-alist)
;; 	  remake-debugger-support-minor-mode-map-when-deactive))


(defun remake-customize ()
  "Use `customize' to edit the settings of the `remake' debugger."
  (interactive)
  (customize-group 'remake))

(provide-me "dbgr-remake-")

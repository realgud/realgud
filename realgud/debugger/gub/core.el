;;; Copyright (C) 2013 Rocky Bernstein <rocky@gnu.org>
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/core") "realgud-")
(require-relative-list '("init") "realgud-gub-")

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar gub-minibuffer-history nil
  "minibuffer history list for the command `gub'.")

(easy-mmode-defmap gub-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun gub-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'gub-suggest-invocation
   gub-minibuffer-local-map
   'gub-minibuffer-history
   opt-debugger))

(defun gub-parse-cmd-args (orig-args)
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
	(gub-name nil)
	(makefile-name nil)
	(gub-args '())
	)

    (if (not (and args))
	;; Got nothing
	(list gub-name makefile-name gub-args)
      ;; else
      ;; Strip off "make" or "gub" etc.
      (when (string-match interp-regexp
			  (file-name-sans-extension
			   (file-name-nondirectory (car args))))
	(setq gub-name (pop args))
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
	    (setq gub-args (nconc gub-args (list arg)))
	    (setq makefile-name (pop args))
	    (setq gub-args (nconc gub-args
				     (list (format "%s" makefile-name)))))

	   ;; Anything else add to gub-args
	   ('t (setq gub-args (nconc gub-args (list arg))))
	   )))
      (list gub-name makefile-name gub-args))))

(defconst realgud-gub-auto-suffix-regexp
  "\\.\\(am\\|in\\)$"
  "Common automake and autoconf Makefile suffixes"
)

(defconst realgud-gub-makefile-regexp
  "\\(^[Mm]akefile$\\|\\.Makefile$\\|\\.mk\\)$"
  "Regular expression matching common Makefile names"
)

(defun gub-suggest-file-priority(filename)
  (let ((priority 2)
	(is-not-directory)
	)
    (if (realgud-lang-mode? filename "makefile")
	(progn
	  (if (string-match realgud-gub-makefile-regexp filename)
	      (setq priority 8)
	    (if (string-match realgud-gub-auto-suffix-regexp filename)
		(setq priority 5)
	      (setq priority 7)))
	  ))
    ;; The file isn't in a makefile-mode buffer,
    ;; Check for an executable file with a .mk extension.
    (if (setq is-not-directory (not (file-directory-p filename)))
	(if (and (string-match realgud-gub-makefile-regexp filename))
	    (if (< priority 6)
		(progn
		  (setq priority 6)))))
    priority
    )
)

(defun gub-suggest-gofile ()
 "Suggest a Go to debug.

The first priority is given to the current buffer. If the major
mode matches GNUMakefile and doesn't end in .am or .in, then we
are done. If not, we'll set priority 2 (a low or easily
overridden priority) and we keep going.  Then we will try files
in the default-directory. Of those that we are visiting we check
the major mode. There are demerits for a file ending in .in or
.am which are used by 'configure' and 'automake' respectively.

If the current buffer isn't a success, we see if the file matches
REGEXP. These have priority 9, 8 or 7 depending on whether there
is a .in or .am sufifx and there is a REGEXP match'.  Within a
given priority, we use the first one we find."
    (let* ((file)
	   (file-list (directory-files default-directory))
	   (priority 2)
	   (is-not-directory)
	   (result (buffer-file-name)))
      (if (not (realgud-lang-mode? result "makefile"))
	  (progn
	    (while (and (setq file (car-safe file-list)) (< priority 8))
	      (setq file-list (cdr file-list))
	      (let ((try-priority (gub-suggest-file-priority file)))
		(if (> try-priority priority)
		    (progn
		      (setq priority try-priority)
		      (setq result file)))
		))
	    ))
      result)
    )

(defvar gub-command-name) ; # To silence Warning: reference to free variable

(defun gub-suggest-invocation (debugger-name)
  "Suggest a gub command invocation via `realgud-suggest-invocaton'"

  (let* ((buf (current-buffer))
	 (cmd-str-cmdbuf (realgud-cmdbuf-command-string buf))
	 (cmd-str-srcbuf (realgud-srcbuf-command-string buf))
	 )
    (cond
     ((and cmd-str-cmdbuf (equal debugger-name (realgud-cmdbuf-debugger-name buf)))
      cmd-str-cmdbuf)
     ((and cmd-str-srcbuf (equal debugger-name (realgud-srcbuf-debugger-name buf)))
      cmd-str-srcbuf)
     ((and minibuffer-history (listp minibuffer-history))
      (car minibuffer-history))
     (t (concat debugger-name " --debugger -f "
		(gub-suggest-Makefile)))
     )))

;; Convert a command line as would be typed normally to run a script
;; into one that invokes an Emacs-enabled debugging session.
;; "--debugger" in inserted as the first switch.

(defun realgud-gub-massage-args (command-line)
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

(defun gub-reset ()
  "Gub cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (gub-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*gub-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun gub-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'gub-debugger-support-minor-mode minor-mode-map-alist)
;; 	  gub-debugger-support-minor-mode-map-when-deactive))


(defun gub-customize ()
  "Use `customize' to edit the settings of the `gub' debugger."
  (interactive)
  (customize-group 'gub))

(provide-me "realgud-gub-")

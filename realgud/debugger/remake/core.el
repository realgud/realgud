;; Copyright (C) 2011, 2014-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'load-relative)
(require-relative-list '("../../common/track" "../../common/core" "../../common/lang")
		       "realgud-")
(require-relative-list '("../../common/buffer/command")
		       "realgud-buffer-")
(require-relative-list '("init") "realgud:remake-")

(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)
(declare-function realgud-lang-mode?         'realgud-lang)
(declare-function realgud-cmdbuf-command-string
		                             'realgud-buffer-command)
(declare-function realgud-cmdbuf-debugger-name
		                             'realgud-buffer-command)
;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:remake-minibuffer-history nil
  "minibuffer history list for the command `remake'.")

(easy-mmode-defmap remake-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun remake-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'remake-suggest-invocation
   remake-minibuffer-local-map
   'realgud:remake-minibuffer-history
   opt-debugger))

(defun remake-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing

- the command processor (e.g. make)
- the Makefile name
- command args (which includes the makefile name)

For example for the following input
  '(\"remake\" \"-x\" \"/tmp/Makefile\")

we might return:
   (\"remake\" \"/tmp/Makefile\" (\"-x\" \"/tmp/Makefile\"))

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
	   ((member arg '("--file" "--makefile" "-f"))
	    (setq remake-args (nconc remake-args (list arg)))
	    (setq makefile-name (realgud:expand-file-name-if-exists
				 (pop args)))
	    (setq remake-args (nconc remake-args
				     (list (format "%s" makefile-name)))))

	   ;; Anything else add to remake-args
	   ('t (setq remake-args (nconc remake-args (list arg))))
	   )))
      (list remake-name makefile-name remake-args))))

(defconst realgud:remake-auto-suffix-regexp
  "\\.\\(am\\|in\\)$"
  "Common automake and autoconf Makefile suffixes"
)

(defconst realgud:remake-makefile-regexp
  "\\(^[Mm]akefile$\\|\\.Makefile$\\|\\.mk\\)$"
  "Regular expression matching common Makefile names"
)

(defun remake-suggest-file-priority(filename)
  (let ((priority 2)
	(is-not-directory)
	)
    (if (realgud-lang-mode? filename "makefile")
	(progn
	  (if (string-match realgud:remake-makefile-regexp filename)
	      (setq priority 8)
	    (if (string-match realgud:remake-auto-suffix-regexp filename)
		(setq priority 5)
	      (setq priority 7)))
	  ))
    ;; The file isn't in a makefile-mode buffer,
    ;; Check for an executable file with a .mk extension.
    (if (setq is-not-directory (not (file-directory-p filename)))
	(if (and (string-match realgud:remake-makefile-regexp filename))
	    (if (< priority 6)
		(progn
		  (setq priority 6)))))
    priority
    )
)

(defun remake-suggest-Makefile ()
 "Suggest a Makefile to debug.

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
	      (let ((try-priority (remake-suggest-file-priority file)))
		(if (> try-priority priority)
		    (progn
		      (setq priority try-priority)
		      (setq result file)))
		))
	    ))
      result)
    )

;; To silence Warning: reference to free variable
(defvar realgud:remake-command-name)

;; Note opt-debugger is not used. It has to be there because
;; realgud-suggest-invocation passes an argument.
(defun remake-suggest-invocation (&optional opt-debugger)
  "Suggest a remake command invocation via `realgud-suggest-invocaton'"

  (let* ((buf (current-buffer))
	 (debugger-name realgud:remake-command-name)
	 (cmd-str-cmdbuf (realgud-cmdbuf-command-string buf))
	 )
    (cond
     ((and cmd-str-cmdbuf (equal debugger-name (realgud-cmdbuf-debugger-name buf)))
      cmd-str-cmdbuf)
     ((and minibuffer-history (listp minibuffer-history))
      (car minibuffer-history))
     (t (concat debugger-name " --debugger -f "
		(remake-suggest-Makefile)))
     )))

;; Convert a command line as would be typed normally to run a script
;; into one that invokes an Emacs-enabled debugging session.
;; "--debugger" in inserted as the first switch.

(defun realgud:remake-massage-args (command-line)
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


(defun realgud:remake-customize ()
  "Use `customize' to edit the settings of the `remake' debugger."
  (interactive)
  (customize-group 'realgud:remake))

(provide-me "realgud:remake-")

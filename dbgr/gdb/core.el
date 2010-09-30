(eval-when-compile (require 'cl))
  
(require 'load-relative)
(require-relative-list '("../common/track" "../common/core") "dbgr-")

;; FIXME: I think the following could be generalized and moved to 
;; dbgr-... probably via a macro.
(defvar dbgr-gdb-minibuffer-history nil
  "minibuffer history list for the command `gdb'.")

(easy-mmode-defmap dbgr-gdb-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun dbgr-gdb-query-cmdline (&optional opt-debugger)
  (dbgr-query-cmdline 
   'dbgr-gdb-suggest-invocation
   dbgr-gdb-minibuffer-local-map
   'dbgr-gdb-minibuffer-history
   opt-debugger))

(defun dbgr-gdb-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ARGS should contain a tokenized list of the command line to run.

We return the a list containing
- the name of the debugger given (e.g. gdb) and its arguments - a list of strings
- the script name and its arguments - list of strings
- whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input 
  (map 'list 'symbol-name
   '(gdb --tty /dev/pts/1 --emacs ./gcd.py a b))

we might return:
   ((gdb --tty /dev/pts/1 --emacs) (./gcd.py a b) 't)

NOTE: the above should have each item listed in quotes.
"

  ;; Parse the following kind of pattern:
  ;;  [python python-options] gdb gdb-options script-name script-options
  (let (
	(args orig-args)
	(pair)          ;; temp return from 
	(gdb-opt-two-args '())
	;; Python doesn't have mandatory 2-arg options in our sense,
	;; since the two args can be run together, e.g. "-C/tmp" or "-C /tmp"
	;; 
	(python-two-args '())
	;; One dash is added automatically to the below, so
	;; h is really -h and -host is really --host.
	(gdb-two-args '("x" "-command" "b" "-exec" 
			"cd" "-pid"  "-core" "-directory"
			"-annotate"
			"se" "-symbols" "-tty"))
	(gdb-opt-two-args '())

	;; Things returned
	(script-name nil)
	(debugger-name nil)
	(debugger-args '())
	(script-args '())
	(annotate-p nil))

    (if (not (and args))
	;; Got nothing: return '(nil nil nil)
	(list debugger-args script-args annotate-p)
      ;; else
      (progn

	;; Remove "gdb" from "gdb --gdb-options script
	;; --script-options"
	(setq debugger-name (file-name-sans-extension
			     (file-name-nondirectory (car args))))
	(unless (string-match "^gdb.*" debugger-name)
	  (message 
	   "Expecting debugger name `%s' to be `gdb'"
	   debugger-name))
	(setq debugger-args (list (pop args)))
	
	;; Skip to the first non-option argument.
	(while (and args (not script-name))
	  (let ((arg (car args)))
	    (cond
	     ;; Annotation or emacs option with level number.
	     ((or (member arg '("--annotate" "-A"))
		  (equal arg "--emacs"))
	      (setq annotate-p t)
	      (nconc debugger-args (list (pop args) (pop args))))
	     ;; Combined annotation and level option.
	     ((string-match "^--annotate=[0-9]" arg)
	      (nconc debugger-args (list (pop args) (pop args)) )
	      (setq annotate-p t))
	     ;; Options with arguments.
	     ((string-match "^-" arg)
	      (setq pair (dbgr-parse-command-arg 
			  args gdb-two-args gdb-opt-two-args))
	      (nconc debugger-args (car pair))
	      (setq args (cadr pair)))
	     ;; Anything else must be the script to debug.
	     (t (setq script-name arg)
		(setq script-args args))
	     )))
	(list debugger-args script-args annotate-p)))))

(defvar dbgr-gdb-command-name)
(defun dbgr-gdb-suggest-invocation (debugger-name)
  "Suggest a gdb command invocation via `dbgr-suggest-invocaton'"
  (dbgr-suggest-invocation dbgr-gdb-command-name dbgr-gdb-minibuffer-history 
			   'dbgr-gdb-suggest-file)
)

(defun dbgr-gdb-suggest-file ()
    "Suggest a file to debug. Priority should be given the languages
that gdb supports: C, C++, Fortran, Java."
    '()
)


(defun dbgr-gdb-reset ()
  "Gdb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (gdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*gdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun gdb-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'gdb-debugger-support-minor-mode minor-mode-map-alist)
;; 	  gdb-debugger-support-minor-mode-map-when-deactive))


(defun dbgr-gdb-customize ()
  "Use `customize' to edit the settings of the `dbgr-gdb' debugger."
  (interactive)
  (customize-group 'dbgr-gdb))

(provide-me "dbgr-gdb-")

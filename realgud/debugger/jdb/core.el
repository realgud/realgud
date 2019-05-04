;; Copyright (C) 2014, 2016, 2018-2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; We use gud to handle the classpath-to-filename mapping
(require 'gud)

(require 'load-relative)
(require-relative-list '("../../common/track"
                         "../../common/core"
                         "../../common/file"
                         "../../common/lang"
                         "../../common/utils")
                       "realgud-")
(require-relative-list '("init") "realgud:jdb-")

(declare-function realgud:strip              'realgud-utils)
(declare-function realgud:expand-file-name-if-exists 'realgud-core)
(declare-function realgud-parse-command-arg  'realgud-core)
(declare-function realgud-query-cmdline      'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)
(declare-function realgud:file-loc-from-line 'realgud-file)
(declare-function realgud:find-file          'realgud-file)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud:jdb-minibuffer-history nil
  "minibuffer history list for the command `realgud:jdb'.")

(easy-mmode-defmap jdb-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of gud startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun realgud:jdb-query-cmdline (&optional opt-debugger)
  (realgud-query-cmdline
   'jdb-suggest-invocation
   jdb-minibuffer-local-map
   'realgud:jdb-minibuffer-history
   opt-debugger))

(defun realgud:jdb-dot-to-slash (str)
  "Change '.' to '/' in STR but chop off from the last . to the end. For example
ca.mgcill.rocky.snpEff.main => ca/mcgill/rocky/snpEff"
      ;;(setq str (replace-regexp-in-string "\\([^\\.]+\\.\\)[^\\.]+$" "\\1" str))
      ;;(setq str (replace-regexp-in-string "\\.$" "" str))
      (setq str (replace-regexp-in-string "\\." "/" str))
      str)

(defun realgud:jdb-find-file(marker filename directory)
  "A find-file specific for java/jdb. We use `gdb-jdb-find-source' to map a
name to a filename. Failing that we can add on .java to the name. Failing that
we will prompt for a mapping and save that the remap."
  (let* ((transformed-file)
	 (cmdbuf (realgud-get-cmdbuf))
	 (ignore-re-file-list (realgud-cmdbuf-ignore-re-file-list cmdbuf))
	 (filename-remap-alist (realgud-cmdbuf-filename-remap-alist))
	 (stripped-filename (realgud:strip filename))
	 (gud-jdb-filename (gud-jdb-find-source stripped-filename))
	 (remapped-filename
	  (assoc filename filename-remap-alist))
	)
    (cond
     ((and gud-jdb-filename (file-exists-p gud-jdb-filename))
      gud-jdb-filename)
     ((file-exists-p (setq transformed-file (concat stripped-filename ".java")))
      transformed-file)
     ((realgud:file-ignore filename ignore-re-file-list)
      (message "tracking ignored for %s" filename) nil)
     (t
      (if remapped-filename
	  (if (file-exists-p (cdr remapped-filename))
	      (cdr remapped-filename)
     	    ;; else remove from map since no find
     	    (and (realgud-cmdbuf-filename-remap-alist=
     		  (delq (assoc remapped-filename filename-remap-alist)
     			filename-remap-alist))
     		  nil))
	;; else
	(let ((remapped-filename)
	      (guess-filename (realgud:jdb-dot-to-slash filename)))
	  (setq remapped-filename
		(buffer-file-name
		 (realgud:find-file marker guess-filename
				    directory "%s.java")))
	  (when (and remapped-filename (file-exists-p remapped-filename))
     	    (realgud-cmdbuf-filename-remap-alist=
     	     (cons
     	      (cons filename remapped-filename)
     	      filename-remap-alist))
	    ))
	))
     )))

(defun realgud:jdb-loc-fn-callback(text filename lineno source-str
					cmd-mark directory)
  (realgud:file-loc-from-line filename lineno
			      cmd-mark source-str nil
			      'realgud:jdb-find-file directory))

(defun realgud:jdb-parse-cmd-args (orig-args)
  "Parse command line ARGS for the annotate level and name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing

* the command debugger (e.g. jdb)

* debugger command arguments if any - a list of strings

* the script name and its arguments - list of strings

For example for the following input
   '(\"jdb\" \"-classpath . ./TestMe.java a b\"))

we might return:
   (\"jdb\" nil \"TestMe\"))

Note that the script name path has been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  [ruby ruby-options] jdb jdb-options script-name script-options
  (let (
        (args orig-args)
	(interp-regexp
	 (if (member system-type (list 'windows-nt 'cygwin 'msdos))
	     "^jdb*\\(.exe\\)?$"
	   "^jdb*$"))
	(jdb-name)
        ;;
        ;; One dash is added automatically to the below, so
        ;; attach is really -attach
	(jdb-two-args '("attach" "sourcepath" "classpath" "dbgtrace"))

        ;; Things returned
        (debugger-args '())
        (program-args '()))

    (if (not (and args))
        ;; Got nothing: return '(nil nil nil)
        (list jdb-name nil debugger-args program-args)
      ;; else
      ;; Strip off optional "jdb" or "jdb.exe" etc.
      (when (string-match interp-regexp (car args))
	(setq jdb-name (car args))
        (setq program-args (nconc program-args (cdr args))))

      (list jdb-name debugger-args program-args))))

;; To silence Warning: reference to free variable
(defvar realgud:jdb-command-name)

(defun jdb-suggest-invocation (debugger-name)
  "Suggest a jdb command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation (or debugger-name realgud:jdb-command-name)
			      realgud:jdb-minibuffer-history
			      "java" "\\.java$" "jdb"))

(defun jdb-reset ()
  "Jdb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (jdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*jdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

;; (defun jdb-reset-keymaps()
;;   "This unbinds the special debugger keys of the source buffers."
;;   (interactive)
;;   (setcdr (assq 'jdb-debugger-support-minor-mode minor-mode-map-alist)
;;        jdb-debugger-support-minor-mode-map-when-deactive))


(defun realgud:jdb-customize ()
  "Use `customize' to edit the settings of the `jdb' debugger."
  (interactive)
  (customize-group 'realgud:jdb))

(provide-me "realgud:jdb-")

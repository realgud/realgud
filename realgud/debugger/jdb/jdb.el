;; Copyright (C) 2014-2017 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;  `jdb' Main interface to jdb via Emacs

(require 'gud) ;; For class-path and source-path handling

(require 'load-relative)
(require-relative-list '("../../common/run") "realgud:")
(require-relative-list '("../../common/helper" "../../common/utils")
		       "realgud-")
(require-relative-list '("core" "track-mode") "realgud:jdb-")

(declare-function realgud:jdb-query-cmdline  'realgud:jdb-core)
(declare-function realgud:jdb-parse-cmd-args 'realgud:jdb-core)
(declare-function realgud:run-process        'realgud:core)
(declare-function realgud:flatten            'realgud-utils)


;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:jdb nil
  "The realgud interface to the Java's jdb debugger"
  :group 'java
  :group 'realgud
  :version "24.3")

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:jdb-command-name
  ;;"jdb --emacs 3"
  "jdb"
  "File name for executing the Java debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:jdb)

;;;###autoload
(defun realgud:jdb (&optional opt-cmd-line no-reset)
  "Invoke the Java jdb debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `jdb-parse-cmd-args' and path elements found by that
are expanded using `expand-file-name'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)

  (setq gud-jdb-classpath nil)
  (setq gud-jdb-sourcepath nil)
  ;; Set gud-jdb-classpath from the CLASSPATH environment variable,
  ;; if CLASSPATH is set.
  (setq gud-jdb-classpath-string (or (getenv "CLASSPATH") "."))
  (if gud-jdb-classpath-string
      (setq gud-jdb-classpath
	    (gud-jdb-parse-classpath-string gud-jdb-classpath-string)))

  (setq gud-jdb-class-source-alist
	(gud-jdb-build-class-source-alist
	 (setq gud-jdb-source-files
	       (gud-jdb-build-source-files-list gud-jdb-directories
						"\\.java$"))))
  (fset 'gud-jdb-find-source 'gud-jdb-find-source-file)


  ;; reset for future invocations
  (setq gud-jdb-classpath-string nil)

  (let* (
	 (cmd-str (or opt-cmd-line (realgud:jdb-query-cmdline "jdb")))
	 (cmd-args (split-string-and-unquote cmd-str))
	 (parsed-args (realgud:jdb-parse-cmd-args cmd-args))
	 (script-args (caddr parsed-args))
	 (script-name (car script-args))
	 (parsed-cmd-args
	  (cl-remove-if 'nil (realgud:flatten parsed-args)))
	 (cmd-buf (realgud:run-process "jdb" script-name parsed-cmd-args
			 'realgud:jdb-track-mode-hook no-reset))
	 )
    (if cmd-buf
	(with-current-buffer cmd-buf
	  (set (make-local-variable 'realgud:jdb-file-remap)
	       (make-hash-table :test 'equal))
	  )
      )
    )
  )

(defalias 'jdb 'realgud:jdb)
(provide-me "realgud-")

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

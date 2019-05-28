;; Copyright (C) 2010-2011, 2014-2015 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;  `rdebug' Main interface to rdebug via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper"
			 "../../common/track") "realgud-")
(require-relative-list '("core" "track-mode") "realgud-rdebug-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 25.
(defgroup realgud:rdebug nil
  "The realgud interface to the Ruby debugger, rdebug"
  :group 'realgud
  :version "25.1")

(declare-function rdebug-query-cmdline   'realgud-rdebug-core)
(declare-function rdebug-parse-cmd-args  'realgud-rdebug-core)
(declare-function realgud:run-debugger   'realgud:run)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:rdebug-command-name
  ;;"rdebug --emacs 3"
  "rdebug"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:rdebug)

(declare-function rdebug-track-mode (bool))

;; -------------------------------------------------------------------
;; The end.
;;

(defun rdebug-get-script-name (args)
  "Parse command line ARGS.

ARGS is a list of strings containing the rdebug command name. We
return a list containing the script name, and whether the
annotate option was set is returned.

Initially annotate should be set to nil.  Argument ARGS contains
a tokenized list of the command line."
  ;; Parse the following:
  ;;
  ;;  [ruby ruby-options] rdebug rdebug-options script-name script-options
  (and args
       (let ((name nil)
             (annotate-p nil))
         ;; Strip of optional "ruby" or "ruby182" etc.
         (when (string-match "^ruby[0-9]*$"
                             (file-name-sans-extension
                              (file-name-nondirectory (car args))))
           (pop args)
           (while (and args
                       (string-match "^-" (car args)))
             (if (member (car args) '("-e" "-r" "-I" "-C" "-F" "-K"))
                 (pop args))
             (pop args)))
         ;; Remove "rdebug" from "rdebug --rdebug-options script
         ;; --script-options"
         (pop args)
         ;; Skip to the first non-option argument.
         (while (and args
                     (not name))
           (let ((arg (pop args)))
             (cond
              ;; Annotation or emacs option with level number.
              ((or (member arg '("--annotate" "-A"))
		   (equal arg "--emacs"))
               (setq annotate-p t)
               (pop args))
              ;; Combined annotation and level option.
              ((string-match "^--annotate=[0-9]" arg)
               (setq annotate-p t))
              ;; Options with arguments.
              ((member arg '("-h" "--host" "-p" "--port"
                             "-I" "--include" "-r" "--require"))
               (pop args))
              ((string-match "^-" arg)
               nil)
              (t
               (setq name arg)))))
         (and name
              (list name annotate-p)))))


;;;###autoload
(defun realgud:rdebug (&optional opt-cmd-line no-reset)
  "Invoke the rdebug Ruby debugger and start the Emacs user interface.

String OPT-CMD-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `trepan8-parse-cmd-args' and path elements found by that
are expanded using `realgud:expand-file-name-if-exists'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)
  (realgud:run-debugger "rdebug" 'rdebug-query-cmdline
			'rdebug-parse-cmd-args
			'realgud:rdebug-minibuffer-history
			opt-cmd-line no-reset)
  )


;;;###autoload
(defalias 'rdebug 'realgud:rdebug)
(provide-me "realgud-")

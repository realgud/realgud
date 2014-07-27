;;; Copyright (C) 2011, 2013-2014 Rocky Bernstein <rocky@gnu.org>
;;  `remake' Main interface to remake via Emacs
(require 'load-relative)
(require-relative-list '("../../common/helper") "realgud-")
(require-relative-list '("../../common/track")  "realgud-")
(require-relative-list '("../../common/run")    "realgud:")
(require-relative-list '("core" "track-mode") "realgud:remake-")
;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:remake nil
  "The realgud interface to the GNU Make debugger"
  :group 'realgud
  :group 'make
  :version "23.1")

(declare-function remake-query-cmdline  'realgud:remake-core)
(declare-function remake-parse-cmd-args 'realgud:remake-core)
(declare-function realgud:run-debugger  'realgud:run)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom realgud:remake-command-name
  ;;"remake --emacs 3"
  "remake"
  "File name for executing the GNU make debugger, remake, and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:remake)

;;;###autoload
(defun realgud:remake (&optional opt-cmd-line no-reset)
  "See `realgud:remake' for details"
  (interactive)
  (realgud:run-debugger "remake"
			'remake-query-cmdline
			'remake-parse-cmd-args
			'remake-track-mode-hook
			'realgud:remake-minibuffer-history
			opt-cmd-line no-reset)
  )

(defalias 'remake 'realgud:remake)

(provide-me "realgud-")
;;; remake.el ends here

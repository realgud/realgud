;;; Copyright (C) 2010-2013 Rocky Bernstein <rocky@gnu.org>
;;; Things related to loading and loading the dbgr package.
(require 'load-relative)

(defgroup realgud nil
  "The Grand Cathedral Debugger rewrite"
  :group 'processes
  :group 'tools
  :version "23.1")

;; FIXME: extend require-relative for "autoload".
(defun realgud-load-features()
  (require-relative-list
   '(
     "./realgud/common/track-mode"
     "./realgud/debugger/bashdb/bashdb"
     "./realgud/debugger/gdb/gdb"
     "./realgud/debugger/kshdb/kshdb"
     "./realgud/debugger/pdb/pdb"
     "./realgud/debugger/pydb/pydb"
     "./realgud/debugger/pydbgr/pydbgr"
     "./realgud/debugger/perldb/perldb"
     "./realgud/debugger/rdebug/rdebug"
     "./realgud/debugger/remake/remake"
     "./realgud/debugger/trepan/trepan"
     "./realgud/debugger/trepan3k/trepan3k"
     "./realgud/debugger/trepan.pl/trepanpl"
     "./realgud/debugger/trepanx/trepanx"
     "./realgud/debugger/trepan8/trepan8"
     "./realgud/debugger/zshdb/zshdb"
     ) "realgud-")
  )

;; Really should be part of GNU Emacs. But until then...
(defmacro realgud-string-starts-with(string prefix)
  "compare-strings on STRING anchored from the beginning and up
  to length(PREFIX)"
  (declare (indent 1) (debug t))
  `(compare-strings ,prefix 0 (length ,prefix)
		    ,string  0 (length ,prefix))
  )

(defun realgud-feature-starts-with(feature prefix)
  "realgud-strings-starts-with on stringified FEATURE and PREFIX."
  (declare (indent 1) (debug t))
  (realgud-string-starts-with (symbol-name feature) prefix)
  )

(defun realgud-loaded-features()
  "Return a list of loaded debugger features. These are the
features that start with 'realgud-' and also include standalone debugger features
like 'pydbgr'."
  (let ((result nil))
    (dolist (feature features result)
      (cond ((eq 't
		 (realgud-feature-starts-with feature "realgud-"))
	     (setq result (cons feature result)))
	    ((eq 't
		 (realgud-feature-starts-with feature "pydbgr"))
	     (setq result (cons feature result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "trepan".
		 (realgud-feature-starts-with feature "trepan"))
	     (setq result (cons feature result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "trepanx".
		 (realgud-feature-starts-with feature "trepanx"))
	     (setq result (cons feature result)))
	    ('t nil))
	)
      )
)

(defun realgud-unload-features()
  "Remove all features loaded from this package. Used in
`realgud-reload-features'. See that."
  (interactive "")
  (let ((result (realgud-loaded-features)))
    (dolist (feature result result)
      (unload-feature feature 't)))
  )

(defun realgud-reload-features()
  "Reload all features loaded from this package. Useful if have
changed some code or want to reload another version, say a newer
development version and you already have this package loaded."
  (interactive "")
  (realgud-unload-features)
  (realgud-load-features)
  )

;; Load everything.
(realgud-load-features)

(provide-me)

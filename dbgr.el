;;; Copyright (C) 2010, 2011 Rocky Bernstein <rocky@gnu.org>
;;; Things related to loading and loading the dbgr package.
(require 'load-relative)

(defgroup dbgr nil
  "The Grand Cathedral Debugger rewrite"
  :group 'processes
  :group 'tools
  :version "23.1")

;; FIXME: extend require-relative for "autoload".
(defun dbgr-load-features()
  (require-relative-list
   '(
     "./dbgr/common/track-mode"
     "./dbgr/debugger/bashdb/bashdb"
     "./dbgr/debugger/gdb/gdb"
     "./dbgr/debugger/kshdb/kshdb"
     "./dbgr/debugger/pydbgr/pydbgr"
     "./dbgr/debugger/perldb/perldb"
     "./dbgr/debugger/rdebug/rdebug"
     "./dbgr/debugger/remake/remake"
     "./dbgr/debugger/trepan/trepan"
     "./dbgr/debugger/trepanpl/trepanpl"
     "./dbgr/debugger/trepanx/trepanx"
     "./dbgr/debugger/trepan8/trepan8"
     "./dbgr/debugger/zshdb/zshdb"
     ) "dbgr-")
  )

;; Really should be part of GNU Emacs. But until then...
(defmacro dbgr-string-starts-with(string prefix)
  "compare-strings on STRING anchored from the beginning and up
  to length(PREFIX)"
  (declare (indent 1) (debug t))
  `(compare-strings ,prefix 0 (length ,prefix)
		    ,string  0 (length ,prefix))
  )
		    
(defun dbgr-feature-starts-with(feature prefix)
  "dbgr-strings-starts-with on stringified FEATURE and PREFIX."
  (declare (indent 1) (debug t))
  (dbgr-string-starts-with (symbol-name feature) prefix)
  )
		    
(defun dbgr-loaded-features()
  "Return a list of loaded debugger features. These are the
features that start with 'dbgr-' and also include standalone debugger features
like 'pydbgr'."
  (let ((result nil))
    (dolist (feature features result) 
      (cond ((eq 't 
		 (dbgr-feature-starts-with feature "dbgr-"))
	     (setq result (cons feature result)))
	    ((eq 't
		 (dbgr-feature-starts-with feature "pydbgr"))
	     (setq result (cons feature result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "trepan".
		 (dbgr-feature-starts-with feature "trepan")) 
	     (setq result (cons feature result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "trepanx".
		 (dbgr-feature-starts-with feature "trepanx")) 
	     (setq result (cons feature result)))
	    ('t nil))
	)
      )
)

(defun dbgr-unload-features()
  "Remove all features loaded from this package. Used in 
`dbgr-reload-features'. See that."
  (interactive "")
  (let ((result (dbgr-loaded-features)))
    (dolist (feature result result)
      (unload-feature feature 't)))
  )

(defun dbgr-reload-features()
  "Reload all features loaded from this package. Useful if have
changed some code or want to reload another version, say a newer
development version and you already have this package loaded."
  (interactive "")
  (dbgr-unload-features)
  (dbgr-load-features)
  )

;; Load everything.
(dbgr-load-features)

(provide-me)

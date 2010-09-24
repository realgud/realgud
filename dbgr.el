;;; Things related to loading and loading the dbgr package.
(require 'load-relative)
(require-relative-list
 '("./dbgr/common/track-mode"
   "./dbgr/rbdbgr/rbdbgr"
   "./dbgr/rdebug/rdebug"
   "./dbgr/trepan/trepan"
   "./dbgr/trepanx/trepanx"
   "./dbgr/gdb/gdb"
   "./dbgr/pydbgr/pydbgr") "dbgr-")


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
like 'rbdbgr', 'pydbgr'."
  (let ((result nil))
    (dolist (feature features result) 
      (cond ((eq 't 
		 (dbgr-feature-starts-with feature "dbgr-"))
	     (setq result (cons feature result)))
	    ((eq 't
		 (dbgr-feature-starts-with feature "pydbgr"))
	     (setq result (cons feature result)))
	    ((eq 't
		 ;; No trailing '-' to get a plain "rbdbgr".
		 (dbgr-feature-starts-with feature "rbdbgr")) 
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
  "Remove all features loaded from this package. Useful if you want to
reload another version, say a newer development version and you already have
this package loaded."
  (interactive "")
  (let ((result (dbgr-loaded-features)))
    (dolist (feature result result)
      (unload-feature feature 't)))
  )


(provide-me)

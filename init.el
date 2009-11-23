(require 'load-relative)

(defun load-debuggers()
  (let*
      ((dir (file-name-directory (__FILE__)))
       (debugger-dir (concat dir "init/"))
       (file-list (directory-files debugger-dir t "\.el$")))
    (dolist (file file-list)
      (load (file-name-sans-extension file) 't))))

(load-debuggers)

(provide-me "dbgr-")

;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)

(defface dbgr-line-number
  '((t :inherit font-lock-variable-name-face))
  "Face for displaying line numbers in."
  :group 'dbgr
  :version "23.1")

(defvar dbgr-line-number-face 'dbgr-line-number
  "Face name to use for line numbers.")

(defface dbgr-file-name
  '((t :inherit font-lock-preprocessor-face))
  "Face for displaying file names."
  :group 'dbgr
  :version "23.1")

(defface dbgr-backtrace-number
  '((t
     :foreground "black"
     :weight bold))
  "Fringe face for current position."
  :group 'dbgr)

(defvar dbgr-backtrace-number-face 'dbgr-backtrace-number
  "Face name to use for backtrace numbers.")

(defvar dbgr-file-name-face 'dbgr-file-name
  "Face name to use for file names.")

(defun load-debuggers()
  (let*
      ((dir (file-name-directory (__FILE__)))
       (debugger-dir (concat dir "init/"))
       (file-list (directory-files debugger-dir t "\.el$")))
    (dolist (file file-list)
      (load (file-name-sans-extension file) 't))))

(load-debuggers)

(provide-me "dbgr-")

;;  `rbdbgr' Main interface to rbdbgr via Emacs
(if (< emacs-major-version 22)
    (error
     "You need at least Emacs 22 or greater to run this - you have version %d"
     emacs-major-version))

(eval-when-compile
  (setq load-path (cons nil (cons ".." load-path)))
  (require 'cl)
  (load "rbdbgr-core")
  (setq load-path (cddr load-path)))

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 23.
(defgroup rbdbgr nil
  "The Ruby 1.9 debugger"
  :group 'processes
  :group 'tools)

;; -------------------------------------------------------------------
;; User definable variables
;;

(defcustom rbdbgr-command-name
  ;;"rbdbgr --emacs 3"
  "rbdbgr"
  "File name for executing the Ruby debugger and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'rbdbgr)


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rbdbgr)

;;; rbdbgr.el ends here


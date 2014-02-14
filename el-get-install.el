(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(eval-when-compile
  (defvar el-get-sources)
)

(declare-function el-get 'el-get)

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(setq
 el-get-sources
 '(el-get			; el-get is self-hosting
   load-relative		; load emacs lisp relative to emacs source
   test-simple			; simple test framework
   ))

;; install new packages and init already installed packages
(el-get 'sync)

;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "./bt-helper.el")
(load-file "./regexp-helper.el")
(load-file "../realgud/debugger/trepan2/init.el")

(declare-function setup-bt 'realgud-bt-helper)
(declare-function setup-regexp-vars 'regexp-helper)
(declare-function __FILE__ 'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar temp-bt)
  (defvar realgud-pat-bt)
  (defvar realgud:trepan2-pat-hash)
)

(setq temp-bt
      (setup-bt "trepan2"
		"->0 gcd(a=3, b=5) called from file '/test/gcd.py' at line 28
##1 <module> execfile() file '/test/gcd.py' at line 41
"))

(with-current-buffer temp-bt
  (switch-to-buffer temp-bt)
  (goto-char (point-min))
  (dolist (pair
	   '(
	     ("->" .    realgud-backtrace-number )
	     ("gc"    . font-lock-function-name-face )
	     ("("     . font-lock-variable-name-face )
	     ("/test" . realgud-file-name)
	     ("2"     . realgud-line-number)
	     ("##"    . realgud-backtrace-number)
	     ("/test" . realgud-file-name)
	     ("4"     . realgud-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  )


(setup-regexp-vars realgud:trepan2-pat-hash)
(setq realgud-pat-bt  (gethash "debugger-backtrace"
                               realgud:trepan2-pat-hash))


;; (let* ((triple
;;         (realgud:backtrace-add-text-properties
;;          realgud-pat-bt ""
;;          "->0 gcd(a=3, b=5) called from file '/test/gcd.py' at line 28
;; ##1 <module> exec() '/test/gcd.py' at line 41"
;;          "->"))
;;        (string-with-props (car triple)))
;;   (dolist (pair
;;            '(
;;              ("->0" . (0 . 28) )
;;              ("##1" . (1 . 41) )
;;              ))
;;     (string-match (car pair) string-with-props)
;;     (assert-equal (cddr pair)
;;                   (realgud-loc-line-number (get-text-property
;;                                             (match-beginning 0) 'loc
;;                                             string-with-props)))

;;     (assert-equal (cadr pair)
;;                   (get-text-property
;;                    (match-beginning 0) 'frame-num
;;                    string-with-props))))

(end-tests)

(load-file "./bt-helper.el")
(load-file "../realgud/debugger/zshdb/init.el")

(declare-function setup-bt 'realgud-bt-helper)

(test-simple-start)

(eval-when-compile
  (defvar temp-bt)
)

(setq temp-bt
      (setup-bt "zshdb"
		"->0 in file `/test/autogen.sh' at line 2
##1 /test/autogen.sh called from file `/usr/local/bin/zshdb' at line 121
"))
(with-current-buffer temp-bt
  (switch-to-buffer temp-bt)
  (goto-char (point-min))
  (dolist (pair
	   '(
	     ("->" .    realgud-backtrace-number )
	     ("/test" . realgud-file-name)
	     ("line " . realgud-line-number)
	     ))
    (search-forward (car pair))
    (assert-equal (cdr pair)
		  (get-text-property (point) 'face))
    )
  )

(end-tests)

(defvar realgud:dap-pat-hash (make-hash-table :test 'equal) )

(defconst realgud:dap-frame-file-regexp
  (format "\\(.+\\):%s" realgud:regexp-captured-num))

;; Regular expression that describes a lldb location generally shown
;; before a command prompt. NOTE: we assume annotate 1!
;; For example:
;; /src/build/ruby-2.1.5/main.c:24:454:beg:0x55555557659f
(setf (gethash "loc" realgud:dap-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^%s:%s:beg:0x\\([0-9a-f]+\\)"
		       realgud:dap-frame-file-regexp
		       realgud:regexp-captured-num)
       :file-group 1
       :line-group 2
       :char-offset-group 3))

;; Regular expression that describes a "breakpoint set" line
;; For example:
;;   Breakpoint 1, main (argc=1, argv=0x7fffffffdbd8) at main.c:24
(setf (gethash "brkpt-set" realgud:dap-pat-hash)
      (list
       (make-realgud-loc-pat
	:regexp (format
		 "^Breakpoint %s at 0x\\([0-9a-f]*\\): file \\(.+\\), line %s[.]\n"
			realgud:regexp-captured-num
			realgud:regexp-captured-num)
	:num 1
	:file-group 3
	:line-group 4)
       ;; Another breakpoint pattern seen
       (make-realgud-loc-pat
	:regexp (format "^Breakpoint %s, .* at \\(.+\\):%s[.]\n"
			realgud:regexp-captured-num
			realgud:regexp-captured-num)
	:num 1
	:file-group 2
	:line-group 3)
       ))

;; Regular expression that describes a debugger "delete" (breakpoint)
;; response.
;; For example:
;;   Deleted breakpoint 1
;;   Deleted breakpoints 1 2 3 4
(setf (gethash "brkpt-del" realgud:dap-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoints? \\(\\([0-9]+ *\\)+\\)\n"
       :num 1))

(defconst realgud:dap-frame-start-regexp
  "\\(?:^\\|\n\\)")

(defconst realgud:dap-frame-num-regexp
  (format "#%s " realgud:regexp-captured-num))

;; Regular expression that describes a gdb "backtrace" command line.
;; For example:
;; #0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
;; #1  0xb7e9f4a5 in *__GI___strdup (s=0xbffff760 "/tmp/remake/remake") at strdup.c:42
;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
;;    at main.c:952
;; #46 0xb7f51b87 in vm_call_cfunc (th=0x804d188, reg_cfp=0xb7ba9e88, num=0,
;;    recv=157798080, blockptr=0x0, me=0x80d12a0) at vm_insnhelper.c:410

(setf (gethash "debugger-backtrace" realgud:dap-pat-hash)
      (make-realgud-loc-pat
       :regexp	(concat realgud:dap-frame-start-regexp
			realgud:dap-frame-num-regexp
			"\\(?:.\\|\\(?:[\n] \\)\\)+[ ]+at "
			realgud:dap-frame-file-regexp
			)
       :num 1
       :file-group 2
       :line-group 3)
      )

;; FIXME breakpoints aren't locations. It should be a different structure
;; Regular expression that describes a gdb "info breakpoint" line
;; For example:
;; 1       breakpoint     keep y   0x0000000000401471 in vcdnav_get_entries at ctest.c:67

(setf (gethash "debugger-breakpoint" realgud:dap-pat-hash)
  (make-realgud-loc-pat
   :regexp (format
	    "^%s[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)[ \t]+\\([yn]\\)[ \t]+.*at \\(.+\\):%s"
		   realgud:regexp-captured-num
		   realgud:regexp-captured-num)
   :num 1
   :text-group 2  ;; misnamed Is "breakpoint" or "watchpoint"
   :string 3      ;; misnamed. Is "keep" or "del"
   ;; Enable is missing
   ;; Skipped address
   :file-group 5
   :line-group 6)
  )


(setf (gethash "font-lock-keywords" realgud:dap-pat-hash)
      '(
	;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
	;;    at main.c:952
	("[ \n]+at \\(.*\\):\\([0-9]+\\)"
	 (1 realgud-file-name-face)
	 (2 realgud-line-number-face))

	;; The frame number and first type name, if present.
	;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
	;;      ---^
	( "#\\(?:^\\|\n\\)\\([0-9]+\\)  "
	 (1 realgud-backtrace-number-face))
	))

(setf (gethash "font-lock-breakpoint-keywords" realgud:dap-pat-hash)
  '(
    ;; The breakpoint number, type and disposition
    ;; 1       breakpoint     keep y   0x0000000000401471 in vcdnav_get_entries at ctest.c:67
    ;; ^       ^^^^^^^^^^     ^^^^
    ("^\\([0-9]+\\)[ \t]+\\(breakpoint\\)[ \t]+\\(keep\\|del\\)"
     (1 realgud-breakpoint-number-face)
     (2 font-lock-function-name-face nil t)     ; t means optional.
     (3 font-lock-function-name-face nil t))
					; t means optional.

    ;; 1       breakpoint     keep y   0x0000000000401471 in vcdnav_get_entries at ctest.c:67
    ;;                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^
    (" in[ \t]+\\(.+*\\):\\([0-9]+\\)"
     (1 realgud-file-name-face)
     (2 realgud-line-number-face))
    ))

(setf (gethash "dap" realgud-pat-hash) realgud:dap-pat-hash)

;;  Prefix used in variable names (e.g. short-key-mode-map) for
;; this debugger

(setf (gethash "dap" realgud:variable-basename-hash) "dap")

(setf (gethash "dap" realgud-pat-hash) realgud:dap-pat-hash)

(provide-me "realgud-")

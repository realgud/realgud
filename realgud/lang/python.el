;;; Copyright (C) 2011, 2014-2015 Rocky Bernstein <rocky@gnu.org>
;;; Common Python constants and regular expressions.
(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track")
		       "realgud-")

(defconst realgud-python-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp "^[ \t]+File \"\\(.+\\)\", line \\([0-9]+\\)"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Python backtrace (or
traceback) line."  )

;;  Regular expression that pseudo-files in caller. For example:
;;    <string>
(defconst realgud-python-ignore-file-re "<string>"
  "Regular expression that pseudo-files of caller()")

(defun realgud-python-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{realgud-example-map-standard}"
  (define-key map (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)
  (define-key map (kbd "C-c !!") 'realgud:goto-lang-backtrace-line)
  )


;; Things common to the trepan Python debuggers

(defconst realgud:python-trepan-frame-start-regexp
  "\\(?:^\\|\n\\)\\(->\\|##\\)")

(defconst realgud:python-trepan-frame-num-regexp
  "\\([0-9]+\\)")

;; Regular expression that describes a trepan2/3k location generally shown
;; before a command prompt.
;;
;; For example:
;;   (/usr/bin/zonetab2pot.py:15 @10): <module>
;;   (/usr/bin/zonetab2pot.py:15 remapped <string>): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>

(defconst realgud:python-trepan-loc-pat
      (make-realgud-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\)\\(?: @[0-9]+\\)?\\(?: remapped .*?\\)?): \\(?:<module>\\)?\\(?:\n.. [0-9]+ \\(.*?\\)\n\\)?"
       :file-group 1
       :line-group 2
       :text-group 3
       :ignore-file-re  realgud-python-ignore-file-re)
      "A realgud-loc-pat struct that describes a Python trepan
      location line."  )

;; Regular expression that describes a trepan2/3k backtrace line.
;; For example:
;; ->0 get_distribution(dist='trepan==0.3.9')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 341
;; ##1 load_entry_point(dist='tr=0.3.9', group='console_scripts', name='tr')
;;     called from file '/python2.7/dist-packages/pkg_res.py' at line 351
;; ##2 <module> exec()

(defconst realgud:python-trepan-backtrace-pat
  (make-realgud-loc-pat
   :regexp (concat
	    realgud:python-trepan-frame-start-regexp
	    realgud:python-trepan-frame-num-regexp "[ ]"
	    "\\(?:.*?)\\)\\(?:[\n\t ]+?\\)"
	    "\\(?:called from file \\)?'\\([^:]+?\\)' at line \\([0-9]+\\)")
   :num 2
   :file-group 3
   :line-group 4
   :ignore-file-re  realgud-python-ignore-file-re)
      "A realgud-loc-pat struct that describes a Python trepan
      backtrace location line." )

;;  Regular expression that describes a "breakpoint set" line
(defconst realgud:python-trepan-brkpt-set-pat
  (make-realgud-loc-pat
   :regexp "^Breakpoint \\([0-9]+\\) set at line \\([0-9]+\\)[ \t\n]+of file \\(.+\\)\\(\n\\|$\\)"
   :num 1
   :file-group 3
   :line-group 2))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
(defconst realgud:python-trepan-brkpt-del-pat
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)\n"
       :num 1))

(defconst realgud:python-debugger-font-lock-keywords
  '(
    ;; The frame number and first type name, if present.
    ("^\\(->\\|##\\)\\([0-9]+\\) \\(<module>\\)? *\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\(.+\\))?"
     (2 realgud-backtrace-number-face)
     (4 font-lock-function-name-face nil t))     ; t means optional.

    ;; Parameter sequence, E.g. gcd(a=3, b=5)
    ;;                             ^^^^^^^^^
    ("(\\(.+\\))"
     (1 font-lock-variable-name-face))

    ;; File name. E.g  file '/test/gcd.py'
    ;;                 ------^^^^^^^^^^^^-
    ("[ \t]+file '\\([^ ]+*\\)'"
     (1 realgud-file-name-face))

    ;; Line number. E.g. at line 28
    ;;                  ---------^^
    ("[ \t]+at line \\([0-9]+\\)$"
     (1 realgud-line-number-face))

    ;; Function name.
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (1 font-lock-type-face)
     (2 font-lock-function-name-face))
    ;; (trepan2-frames-match-current-line
    ;;  (0 trepan2-frames-current-frame-face append))
    ))



(provide-me "realgud-lang-")

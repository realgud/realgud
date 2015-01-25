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
;;   (/usr/bin/zonetab2pot.py:15): <module>
;;   (/usr/bin/zonetab2pot.py:15 remapped <string>): <module>
;; or MS Windows:
;;   (c:\\mydirectory\\gcd.py:10): <module>

(defconst realgud:python-trepan-loc-pat
      (make-realgud-loc-pat
       :regexp "^(\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\)\\(?: remapped .*?\\)?): \\(?:<module>\\)?\\(?:\n.. [0-9]+ \\(.*?\\)\n\\)?"
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


(provide-me "realgud-lang-")

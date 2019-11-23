;; Copyright (C) 2016-2018 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
(require 'load-relative)
(require 'comint)
(require 'eshell)

(defun realgud:strip (str)
      "Remove leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)

;; From http://rosettacode.org/wiki/Flatten_a_list#Emacs_Lisp
(defun realgud:flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (realgud:flatten (car mylist)) (realgud:flatten (cdr mylist))))))

(if (or (< emacs-major-version 24)
	(and (= emacs-major-version 24) (<= emacs-minor-version 3)))
    ;; From
    ;; https://stackoverflow.com/questions/12999530/is-there-a-function-that-joins-a-string-into-a-delimited-string
    (defun realgud:join-string (list joiner)
      (mapconcat 'identity list joiner))
  (progn
    (require 'subr-x)
    (defalias 'realgud:join-string 'string-join)))

(defun realgud:canonic-major-mode()
  "Return
    - 'eshell if we are in eshell-mode,
    - 'comint if the major comint-mode or shell-mode
Or raise an error if neither."

  (cond ((derived-mode-p 'eshell-mode)
	 'eshell)
	((derived-mode-p 'comint-mode)
	 'comint)
	('t (error "We can only handle comint, shell, or eshell buffers"))
	))

(defun realgud:remove-ansi-schmutz-in-string (string)
  "Remove ASCII escape sequences from STRING"
  (replace-regexp-in-string
   ;; Strip ANSI escape codes, e.g. gdb produces ^[[?2004h and ^[[?2004l before
   ;; prompts. This regex handles these sequences, colors, and more. However, it
   ;; doesn't cover all. To cover all, we'd need something like
   ;; "\033\\[\\??[0-9;]*[a-zA-Z]" but this covers non-defined escape sequences and is
   ;; missing sequences that have multiple ending letters. The multi-letter ending
   ;; escape sequences probably won't occur because these are cursor movement
   ;; sequences. Examining the escape code spec, this regex should cover all cases
   ;; we'd hit from a debugger.
   ;; https://github.com/realgud/realgud/issues/257
   ;; https://en.wikipedia.org/wiki/ANSI_escape_code
   "\033\\[\\??[0-9;]*[CDGKJhlm]" "" string))

(defun realgud:remove-ansi-schmutz()
  "Remove ASCII escape sequences that node.js 'decorates' in
prompts and interactive output with"
  (interactive "")
  (add-to-list
   'comint-preoutput-filter-functions
   (lambda (output)
     (realgud:remove-ansi-schmutz-in-string output)))
  )


(provide-me "realgud-")

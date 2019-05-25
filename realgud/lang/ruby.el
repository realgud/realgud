;; Copyright (C) 2010, 2014, 2016, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Common Ruby constants and regular expressions.
(require 'load-relative)
(require-relative-list '("../common/regexp" "../common/loc" "../common/track")
		       "realgud-")

(declare-function realgud-goto-line-for-pt 'realgud-track)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defconst realgud-rails-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp "^\\([^:]+\\):\\([0-9]+\\)\\(?:[:]in `.*'\\)?"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Rails backtrace (or
traceback) line."  )

(defconst realgud-rspec-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp "^[ ]*# \\([^:]+\\):\\([0-9]+\\)\\(?:[:]in `.*'\\)?"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes an rspec backtrace (or
traceback) line."  )

;; Regular expression that describes a Ruby YARV 1.9 syntax error line.
;; SyntaxError: /tmp/columnize.rb:270: syntax error, unexpected $end, ...
(defconst realgud-ruby-YARV-syntax-error-pat
  (make-realgud-loc-pat
   :regexp "^SyntaxError: \\([^:]+\\):\\([0-9]+\\): syntax error"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Ruby YARV syntax error message")

(defconst realgud-ruby-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(?: in `.*'\\)?"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Ruby backtrace (or
traceback) line."  )

(defconst realgud-rubinius-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp "^\\(?:\\[0;3[1-4]m\\)?[ \t]+.* at \\([^:]+\\):\\([0-9]+\\)"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Rubinius backtrace (or
traceback) line."  )

(defconst realgud-rubinius-Xagent-backtrace-loc-pat
  (make-realgud-loc-pat
   :regexp "^\\(?:\\[0;3[1-4]m\\)?0x[a-f0-9]\\{8\\}: .* in \\([^:]+\\):\\([0-9]+\\) ([+][0-9]+)"
   :file-group 1
   :line-group 2)
  "A realgud-loc-pat struct that describes a Rubinius Xagent backtrace (or
traceback) line."  )

(defconst realgud-ruby-dollar-bang-loc-pat
      (make-realgud-loc-pat
       :regexp "^[ \t]*[[]?\\(.+\\):\\([0-9]+\\):in `.*'"
       :file-group 1
       :line-group 2)
  "A realgud-loc-pat that struct that describes a Ruby $! string."
)

;; FIXME: there is probably a less redundant way to do the following
;; FNS.
(defun realgud:rails-goto-backtrace-line (pt)
  "Display the location mentioned by the Rails backtrace line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "rails-backtrace"))

;; FIXME: there is probably a less redundant way to do the following
;; FNS.
(defun realgud:rspec-goto-backtrace-line (pt)
  "Display the location mentioned by the Rails backtrace line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "rspec-backtrace"))

(defun realgud:rubinius-goto-Xagent-backtrace-line (pt)
  "Display the location mentioned by the Rubinius Xagent- backtrace line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "rubinius-backtrace-Xagent"))

(defun realgud:ruby-goto-backtrace-line (pt)
  "Display the location mentioned by the Ruby backtrace line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "lang-backtrace"))

(defun realgud:ruby-goto-dollar-bang-line (pt)
  "Display the location mentioned by the Ruby backtrace line
described by PT."
  (interactive "d")
  (realgud-goto-line-for-pt pt "dollar-bang-backtrace"))

(defun realgud:ruby-populate-command-keys (&optional map)
  "Bind the debugger function key layout used by many debuggers.

\\{realgud-example-map-standard}"
  (define-key map (kbd "C-c !l") 'realgud:goto-lang-backtrace-line)
  (define-key map (kbd "C-c !!") 'realgud:ruby-goto-dollar-bang-line)
  (define-key map (kbd "C-c !b") 'realgud:goto-debugger-backtrace-line)
  (define-key map (kbd "C-c !r") 'realgud:rails-goto-backtrace-line)
  (define-key map (kbd "C-c !s") 'realgud:rspec-goto-backtrace-line)
  )


;; Some things common to all trepan debuggers (Rubinius and Ruby 1.9.2)
(defconst realgud:trepan-frame-start-regexp
  "\\(?:^\\|\n\\)\\(-->\\|   \\)")

(defconst realgud:trepan-frame-num-regexp
  "#\\([0-9]+\\)")

(defconst realgud:trepan-frame-line-regexp
  "[ \t\n]+at line \\([0-9]+\\)\\(?:\n\\|$\\)")


(provide-me "realgud-lang-")

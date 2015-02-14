;;; Copyright (C) 2010, 2015 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)

(defface realgud-line-number
  '((t :inherit font-lock-variable-name-face))
  "Face for displaying line numbers in."
  :group 'realgud
  :version "23.4")

(defvar realgud-line-number-face 'realgud-line-number
  "Face name to use for line numbers.")

(defface realgud-file-name
  '((t :inherit font-lock-preprocessor-face))
  "Face for displaying file names."
  :group 'realgud
  :version "23.4")

(defface realgud-backtrace-number
  '((t
     :foreground "black"
     :weight bold))
  "Fringe face for current position."
  :group 'realgud)

(defvar realgud-backtrace-number-face 'realgud-backtrace-number
  "Face name to use for backtrace numbers.")

(defvar realgud-file-name-face 'realgud-file-name
  "Face name to use for file names.")

(provide-me "realgud-")

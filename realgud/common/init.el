;; Copyright (C) 2010, 2015, 2019 Free Software Foundation, Inc

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

(defface realgud-line-number
  '((t :inherit font-lock-variable-name-face))
  "Face for displaying line numbers in."
  :group 'realgud
  :version "23.4")

(defvar realgud-address-face 'realgud-line-number
  "Face name to use for addresses.")

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

(defvar realgud-breakpoint-number-face 'realgud-backtrace-number
  "Face name to use for breakpoint numbers.")

(defvar realgud-file-name-face 'realgud-file-name
  "Face name to use for file names.")

(provide-me "realgud-")

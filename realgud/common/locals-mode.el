;;; Locals buffer
;; Copyright (C) 2021 Free Software Foundation, Inc

;; Author: 813gan

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(require 'load-relative)
(require-relative "helper" nil "realgud-")
(require-relative "buffer/locals" nil "realgud-buffer-")

(defvar realgud-locals-mode-map
  (let ((map  (realgud-populate-debugger-menu (make-sparse-keymap))))
    (suppress-keymap map)
    (realgud-populate-common-keys map)
    (define-key map "q" 'realgud:cmd-quit)
    (define-key map "L" 'realgud:window-locals)
    (define-key map "s" 'realgud:locals-show-all-values)
    (define-key map "h" 'realgud:locals-hide-all-values)
    map)
  )

(defun realgud-locals-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only 't)
  (setq major-mode 'realgud-locals-mode)
  (setq mode-name "Realgud Locals")
  (use-local-map realgud-locals-mode-map)
  )

(provide-me "realgud-")

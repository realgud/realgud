;; Copyright (C) 2015-2016, 2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Emacs Commands to associate or attach a source buffer to a command
;;; buffer and vice versa.

(eval-when-compile (require 'cl-lib))   ;For cl-remove-if-not.

(require 'load-relative)
(require-relative-list  '("buffer/command" "buffer/source")
			"realgud-buffer-")
(require-relative-list  '("shortkey") "realgud-")

(declare-function realgud-cmdbuf-add-srcbuf           'realgud-buffer-command)
(declare-function realgud-cmdbuf?                     'realgud-buffer-command)
(declare-function realgud-srcbuf-init-or-update       'realgud-source)
(declare-function realgud-short-key-mode-setup        'realgud-shortkey)

(defvar realgud:attach-cmdbuf-history nil "Attach command buffer history list'.")


;;;###autoload
(defun realgud:attach-source-buffer(srcbuf)
  "Associate a source buffer with the current command buffer"
  (interactive "bsource buffer: ")
  (unless (realgud-cmdbuf?)
    (error "The command only works inside a command buffer"))
  (unless (get-buffer-process (current-buffer))
    (warn "Can't find a process for command buffer %s" (current-buffer)))

  (let* ((cmdbuf (current-buffer))
	 (shortkey-mode? (realgud-sget 'cmdbuf-info 'src-shortkey?)))
    (if (stringp srcbuf) (setq srcbuf (get-buffer srcbuf)))
    (realgud-cmdbuf-add-srcbuf srcbuf)
    (realgud-srcbuf-init-or-update srcbuf cmdbuf)
    (if shortkey-mode?
	(with-current-buffer srcbuf
	  (realgud-short-key-mode-setup 't))
      )
    )
  )

;;;###autoload
(defun realgud:attach-cmd-buffer(cmdbuf)
  "Associate a command buffer with the current source buffer"

  (interactive
   (list
    (completing-read "Choose a realgud command buffer: "
		     (realgud:attach-list-command-buffers) nil t nil
		     'realgud:attach-cmdbuf-history nil)))
  (if (stringp cmdbuf) (setq cmdbuf (get-buffer cmdbuf)))
  (let* ((srcbuf (current-buffer))
	 (shortkey-mode?))
    (with-current-buffer cmdbuf
      (unless (realgud-cmdbuf?)
	(error "The buffer is not a command buffer"))
      (unless (get-buffer-process (current-buffer))
	(warn "Can't find a process for command buffer %s" (current-buffer)))
      (setq shortkey-mode? (realgud-sget 'cmdbuf-info 'src-shortkey?)))
    (add-to-list 'realgud:attach-cmdbuf-history (buffer-name cmdbuf))
    (realgud-cmdbuf-add-srcbuf srcbuf)
    (realgud-srcbuf-init-or-update srcbuf cmdbuf)
    (if shortkey-mode? (realgud-short-key-mode-setup 't)))
  )

(defun realgud:attach-list-command-buffers()
  (mapcar 'buffer-name (cl-remove-if-not 'realgud-cmdbuf? (buffer-list))))


(provide-me "realgud-")

;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
;;; Emacs Commands to associate or attach a source buffer to a command
;;; buffer and vice versa.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list  '("buffer/command" "buffer/source")
			"realgud-buffer-")
(require-relative-list  '("shortkey") "realgud-")

(declare-function realgud-cmdbuf-add-srcbuf           'realgud-buffer-command)
(declare-function realgud-cmdbuf?                     'realgud-buffer-command)
(declare-function realgud-srcbuf-init-or-update       'realgud-source)
(declare-function realgud-short-key-mode-setup        'realgud-shortkey)

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

(defun realgud:attach-command-buffer(cmdbuf)
  "Associate a command buffer with the current source buffer"

  (interactive "bcommand buffer: ")
  (if (stringp cmdbuf) (setq cmdbuf (get-buffer cmdbuf)))
  (let* ((srcbuf (current-buffer))
	 (shortkey-mode?))
    (with-current-buffer cmdbuf
      (unless (realgud-cmdbuf?)
	(error "The buffer is not a command buffer"))
      (unless (get-buffer-process (current-buffer))
	(warn "Can't find a process for command buffer %s" (current-buffer)))
      (setq shortkey-mode? (realgud-sget 'cmdbuf-info 'src-shortkey?)))
    (realgud-cmdbuf-add-srcbuf srcbuf)
    (realgud-srcbuf-init-or-update srcbuf cmdbuf)
    (if shortkey-mode? (realgud-short-key-mode-setup 't)))
  )

(provide-me "realgud-")

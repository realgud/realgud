;;; Copyright (C) 2010, 2014 Rocky Bernstein <rocky@gnu.org>
;; reset state on quit or restart

(require 'load-relative)
(require 'loc-changes)
(require-relative-list '("bp" "fringe" "helper") "realgud-")
(require-relative-list '("buffer/helper" "buffer/command" "buffer/source")
		       "realgud-buffer-")

(declare-function loc-changes-clear-buffer       'loc-changes)
(declare-function realgud-bp-remove-icons        'realgud-buffer-command)
(declare-function realgud-cmdbuf?                'realgud-buffer-command)
(declare-function realgud-get-cmdbuf-from-srcbuf 'realgud-buffer-helper)
(declare-function realgud-fringe-erase-history-arrows
		  'realgud-buffer-command)

(defun realgud:reset (&optional opt-buffer)
  "Reset state prior to quitting or restarting"
  (interactive)
  (let* ((buf (or opt-buffer (current-buffer)))
	 (cmdbuf
	  (cond ((realgud-cmdbuf? buf) buf)
		((realgud-get-cmdbuf-from-srcbuf buf))
		('t nil))))
    (if cmdbuf
	(with-current-buffer cmdbuf
	  (realgud-fringe-erase-history-arrows)
	  (let ((buf-list (realgud-sget 'cmdbuf-info 'srcbuf-list)))
	    (dolist (buf buf-list buf-list)
	      (with-current-buffer buf
		(loc-changes-clear-buffer)
		(realgud-fringe-erase-history-arrows)
		;; FIXME: iterate over specific breakpoints.
		(realgud-bp-remove-icons (point-min) (point-max))
		)
	      (message "buffer %s" buf)
	      ))
	  (setq realgud-cmdbuf-info nil)
	  )
      ;; else
      (error "Unable to find command buffer from %s" buf)
      ))
  )

(provide-me "realgud-")

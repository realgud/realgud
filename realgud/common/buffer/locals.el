(require 'load-relative)
(require-relative-list
 '("helper") "realgud-")
(require-relative-list
 '("command") "realgud-buffer-")

(make-variable-buffer-local (defvar realgud-buffer-type))

(cl-defstruct realgud-locals-info
  "debugger object/structure specific to a (top-level) program to be debugged."
  (cmdbuf    nil)  ;; buffer of the associated debugger process
  )
(make-variable-buffer-local (defvar realgud-locals-info))

(defun realgud-locals? ( &optional buffer)
  "Return true if BUFFER is a locals buffer.

If no BUFFER is given, current buffer is used."
  (with-current-buffer-safe (or buffer (current-buffer))
    (and (boundp 'realgud-buffer-type)
	 (equal realgud-buffer-type 'locals) )))

(defun realgud-run-command-get-output (cmd &rest args)
  "Run debugger command and split output into list.

First line (with command itself) is excluded.
CMD - command to be executed
ARGS - arguments for command"
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (let ((sleep-count 0)
	  (process (get-buffer-process (current-buffer)))
	  (realgud-track-divert-string nil))
      (realgud-cmdbuf-info-divert-output?= t)
      (if args
	  (apply cmd args)
	(funcall cmd))
      (while (and (eq 'run (process-status process))
		  (null realgud-track-divert-string)
		  (> 1000 (setq sleep-count (1+ sleep-count))))
	(sleep-for 0.001)
	)
      (if (>= sleep-count 1000)
	  (error "%s" "Timeout on running debugger command")
	(cdr (split-string realgud-track-divert-string "\n" t)) )) ))

(defun realgud-locals-init ()
  "Create locals buffer."
  (let ((cmdbuf (realgud-get-cmdbuf)))
    (with-current-buffer-safe cmdbuf
      (let ((locals-buffer (get-buffer-create
			    (format "*locals %s*"
				    (realgud-get-buffer-base-name
				     (buffer-name))))))
	(realgud-cmdbuf-info-locals-buf= locals-buffer)
	(with-current-buffer locals-buffer
	  (realgud-locals-mode)
	  (setq realgud-buffer-type 'locals)
	  (set (make-local-variable 'realgud-locals-info)
	       (make-realgud-locals-info
		:cmdbuf cmdbuf)) )
	(realgud-locals-register-reload)
	(realgud-locals-insert) ))))

(defun realgud-locals-get-variable-data (local-var-name)
  "Return list with type and value of variable, in that order.

LOCAL-VAR-NAME - variable to inspect"
  (list
   (car (realgud-run-command-get-output 'realgud:cmd-info-type local-var-name))
   (mapconcat 'identity (realgud-run-command-get-output 'realgud:cmd-info-value local-var-name) "\n") ))

(defun realgud-locals-register-reload ()
  "Get list of local variables and load values selected by user."
  (let* ((locals-names-list (realgud-run-command-get-output 'realgud:cmd-info-locals-name-list))
	 (frame-id 'frame_id_placeholder)
	 (locals-data-hash (realgud-get-info 'locals-data))
	 (frame-data-hash (gethash frame-id locals-data-hash))
	 (new-frame-data-hash (make-hash-table :test 'equal)))
    (dolist (local-var-name locals-names-list)
      (if (and frame-data-hash
	       (gethash local-var-name frame-data-hash))
	  (puthash local-var-name
		   (realgud-locals-get-variable-data local-var-name)
		   new-frame-data-hash)
	(puthash local-var-name nil new-frame-data-hash) ) )
    (puthash frame-id new-frame-data-hash locals-data-hash) )) ; TODO remove non-exising keys instead creating new hash?

(defun realgud-locals-toggle-value-visibility (local-var-name)
  "Update value of single variable in frame hash and update locale buffer.

LOCAL-VAR-NAME - variable to toggle"
  (interactive "sVariable: ")
  (let* ((frame-id 'frame_id_placeholder)
	 (locals-data-hash (realgud-get-info 'locals-data))
	 (frame-data-hash (gethash frame-id locals-data-hash))
	 (value nil))
    (unless (gethash local-var-name frame-data-hash)
      (setq value (realgud-locals-get-variable-data local-var-name)))
    (puthash local-var-name value frame-data-hash) )
  (realgud-locals-insert) )

(defun realgud-locals-insert ()
  "Serialize and format locales data."
  (let ((frame-data-hash
	 (gethash 'frame_id_placeholder (realgud-get-info 'locals-data)))
	(variable-data nil)
	(prev-buffer-end (point-min)) )
    (with-current-buffer (realgud-get-locals-buf)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (dolist (variable (hash-table-keys frame-data-hash))
	(setq variable-data (gethash variable frame-data-hash))
	(insert variable)
	(make-button prev-buffer-end (point-max)
		     'variable variable
		     'action (lambda (button)
			       (realgud-locals-toggle-value-visibility
				(button-get button 'variable) )) )
	(when variable-data
	      (insert " ")
	      (insert (nth 0 variable-data))
	      (insert " ")
	      (insert (nth 1 variable-data)) )
	(insert "\n")
	(setq prev-buffer-end (point-max)) )
      (setq buffer-read-only t) )) )

(provide-me "realgud-")

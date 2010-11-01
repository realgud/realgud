;;; Backtrace buffer
;;; Copyright (C) 2010 Rocky Bernstein <rocky@gnu.org>
(require 'load-relative)
(require-relative-list
 '("../send" "../track" "../key") "dbgr-")
(require-relative-list
 '("command") "dbgr-buffer-")

(defstruct dbgr-backtrace-info
  "debugger object/structure specific to a (top-level) Ruby file
to be debugged."
  cmdproc        ;; buffer of the associated debugger process
  cur-pos        ;; If not nil, frame we are at
)

(defvar dbgr-backtrace-info)
(make-variable-buffer-local 'dbgr-backtrace-info)



;: FIXME: not picked up from track. Why?
(defvar dbgr-track-divert-string nil)

(defvar dbgr-goto-entry-acc "")

(defvar dbgr-backtrace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [double-mouse-1] 'dbgr-goto-frame-mouse)
    (define-key map [mouse-2] 'dbgr-goto-frame-mouse)
    (define-key map [mouse-3] 'dbgr-goto-frame-mouse)
    (define-key map [(control m)] 'dbgr-goto-frame)
    (define-key map "0" 'dbgr-goto-frame-n)
    (define-key map "1" 'dbgr-goto-frame-n)
    (define-key map "2" 'dbgr-goto-frame-n)
    (define-key map "3" 'dbgr-goto-frame-n)
    (define-key map "4" 'dbgr-goto-frame-n)
    (define-key map "5" 'dbgr-goto-frame-n)
    (define-key map "6" 'dbgr-goto-frame-n)
    (define-key map "7" 'dbgr-goto-frame-n)
    (define-key map "8" 'dbgr-goto-frame-n)
    (define-key map "9" 'dbgr-goto-frame-n)
    (dbgr-populate-common-keys map)

    ;; ;; --------------------
    ;; ;; The "Stack window" submenu.
    ;; (let ((submenu (make-sparse-keymap)))
    ;;   (define-key-after map [menu-bar debugger stack]
    ;;     (cons "Stack window" submenu)
    ;;     'placeholder))

    ;; (define-key map [menu-bar debugger stack goto]
    ;;   '(menu-item "Goto frame" dbgr-goto-frame))
    map)
  "Keymap to navigate dbgr stack frames.")

(defun dbgr-remove-surrounding-stars(string)
  "Leading and ending * in string. For example:
   *shell<2>* -> shell<2>. 
   buffer.c -> buffer.c"
  (if (string-match "^[*]?\\([^*]+\\)[*]?$" string)
      (match-string 1 string)
    string
    )
)

(defun dbgr-backtrace-init ()
  (interactive)
  (let ((buffer (current-buffer))
  	(cmdbuf (dbgr-get-cmdbuf))
  	(process)
  	)
    (with-current-buffer-safe cmdbuf
      (setq process (get-buffer-process (current-buffer)))
      (dbgr-cmdbuf-info-in-srcbuf?= dbgr-cmdbuf-info 
    				   (not (dbgr-cmdbuf? buffer)))
      (dbgr-cmdbuf-info-divert-output?= dbgr-cmdbuf-info 't)
      (setq dbgr-track-divert-string nil)
      (dbgr-command "backtrace" nil nil 't)
      (while (and (eq 'run (process-status process))
		    (null dbgr-track-divert-string))
	  (sleep-for 0.001)
	  )
      ;; (message "+++4 %s" dbgr-track-divert-string)
      (let ((bt-buffer (get-buffer-create
			(format "*%s backtrace*" 
				(dbgr-remove-surrounding-stars (buffer-name)))))
	    (divert-string dbgr-track-divert-string)
	    )
	(with-current-buffer bt-buffer
	  (set (make-local-variable 'dbgr-backtrace-info)
	       (make-dbgr-backtrace-info
		:cmdproc cmdbuf))
	  (setq buffer-read-only nil)
	  (delete-region (point-min) (point-max))
	  (if divert-string 
	      (progn
		(insert divert-string)
		(dbgr-backtrace-mode cmdbuf)
		)
	    )
	  )
	)
    )
  )
)

(defun dbgr-backtrace-mode (&optional cmdbuf)
  "Major mode for displaying the stack frames.
\\{dbgr-frames-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only 't)
  (setq major-mode 'dbgr-backtrace-mode)
  (setq mode-name "dbgr Stack Frames")
  ;; (set (make-local-variable 'dbgr-secondary-buffer) t)
  (setq mode-line-process 'dbgr-mode-line-process)
  (use-local-map dbgr-backtrace-mode-map)

  ;; FIXME: make buffer specific
  (if cmdbuf
      (let* ((font-lock-keywords 
	      (with-current-buffer cmdbuf
		(dbgr-cmdbuf-pat "font-lock-keywords"))))
	(if font-lock-keywords
	    (set (make-local-variable 'font-lock-defaults)
		 (list font-lock-keywords)))
	))
  ;; (run-mode-hooks 'dbgr-backtrace-mode-hook)
  )

(defun dbgr-goto-frame-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq dbgr-goto-entry-acc (concat dbgr-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc dbgr-goto-entry-acc))
          (while (not (string= acc ""))
            (if (not (dbgr-goto-entry-try acc))
                (setq acc (substring acc 1))
              (dbgr-cmd-frame (string-to-number acc))
              ;; Break loop.
              (setq acc "")))))
    (message "`dbgr-goto-frame-n' must be bound to a number key")))

(defun dbgr-goto-entry-try (str)
  "See if thre is an entry with number STR.  If not return nil."
  (goto-char (point-min))
  (if (re-search-forward (concat "^[^0-9]*\\(" str "\\)[^0-9]") nil t)
      (progn
        (goto-char (match-end 1))
        t)
    nil))


;; The following is split in two to facilitate debugging.
(defun dbgr-goto-entry-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq dbgr-goto-entry-acc (concat dbgr-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc dbgr-goto-entry-acc)
              (p (point)))
          (while (not (string= acc ""))
            (if (not (dbgr-goto-entry-try acc))
                (setq acc (substring acc 1))
              (setq p (point))
              ;; Break loop.
              (setq acc "")))
          (goto-char p)))
    (message "`dbgr-goto-entry-n' must be bound to a number key")))


(defun dbgr-goto-entry-n ()
  "Go to an entry number.

Breakpoints, Display expressions and Stack Frames all have
numbers associated with them which are distinct from line
numbers.  In a secondary buffer, this function is usually bound to
a numeric key which will position you at that entry number.  To
go to an entry above 9, just keep entering the number.  For
example, if you press 1 and then 9, you should jump to entry
1 (if it exists) and then 19 (if that exists).  Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'dbgr-goto-entry-n))
      (setq dbgr-goto-entry-acc ""))
  (dbgr-goto-entry-n-internal (this-command-keys)))

(defun dbgr-goto-frame-n ()
  "Go to the frame number indicated by the accumulated numeric keys just entered.

This function is usually bound to a numeric key in a 'frame'
secondary buffer. To go to an entry above 9, just keep entering
the number. For example, if you press 1 and then 9, frame 1 is selected
\(if it exists) and then frame 19 (if that exists). Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'dbgr-goto-frame-n))
      (setq dbgr-goto-entry-acc ""))
  (dbgr-goto-frame-n-internal (this-command-keys)))

(provide-me "dbgr-buffer-")

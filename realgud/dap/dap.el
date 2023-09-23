;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'load-relative)
(require-relative-list '("../common/regexp")
		       "realgud-")
(require-relative-list '("../common/buffer/command")
		       "realgud-buffer-")


(defvar realgud:dap-pat-hash (make-hash-table :test 'equal)
  "dummy hash entry for compatibility")
(setf (gethash "loc" realgud:dap-pat-hash) (make-realgud-loc-pat))
(setf (gethash "prompt" realgud:dap-pat-hash) (make-realgud-loc-pat))
(setf (gethash "dap" realgud-pat-hash) realgud:dap-pat-hash)


(defun realgud--dap-headers-make-assoc (headers-str)
  (let* ((splitted (split-string headers-str ": " 't "[ \f\t\n\r\v]+")))
    `((,(nth 0 splitted) . ,(nth 1 splitted))) ))

(defun realgud--dap-parse-output ()
  "Parse DAP message.  Return 't if any message was found."
  (goto-char (point-min))
  (when (search-forward "\r\n\r\n" nil 't)
    (let* ((headers-end-point (match-end 0))
	   (headers (realgud--dap-headers-make-assoc
		     (buffer-substring (point-min)
				       (match-beginning 0))))
	   (len (string-to-number
		 (cdr (assoc "Content-Length" headers))))
	   (inhibit-read-only 't) )
      (when (>= (- (point-max) headers-end-point) len)
	(let* ((json-array-type 'list)
               (json-object-type 'hash-table)
               (json-false nil)
	       (body (json-read-from-string
		      (buffer-substring headers-end-point
					(+ headers-end-point len))))
	       (new-message (list :headers headers :body body)))
	  ;;let-alist cant be used as it's not place form
	  ;; (let-alist realgud-dap-message-que
	  ;;   (with-mutex .mutex
	  ;;     (if .que
	  ;; 	  (push new-message (cdr (last .que)) )
	  ;; 	(setf .que (list new-message)) )
	  ;;     (condition-notify .notify-var) )
	  ;;   )
	  (with-mutex (alist-get 'mutex realgud-dap-message-que)
	    (if (alist-get 'que realgud-dap-message-que)
		(push new-message (cdr (last (alist-get 'que realgud-dap-message-que))))
	      (setf (alist-get 'que realgud-dap-message-que) (list new-message)) )
	    (condition-notify (alist-get 'notify-var realgud-dap-message-que)) ) )
	(delete-region (point-min) (+ headers-end-point len))
	't) )) )

(defun realgud--dap-process-filter (proc string)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
	    (inhibit-read-only 't))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))

      (while (realgud--dap-parse-output) ) ; (thread-yield) ??
      )))

(defun realgud--dap-start-client (server port)
  ;; Create buffer with leading space, so it's hidden from user and have no undo history.
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Names.html
  (let* ((name (concat " *dap " server ":" (number-to-string port) "*"))
	 (netbuff (get-buffer-create name))
	 )
      (make-network-process
       :name name
       :buffer netbuff
       :filter 'realgud--dap-process-filter
       :host server
       :service port
       ; :filter-multibyte 't ??
       )))

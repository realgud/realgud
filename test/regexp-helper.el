(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")

(eval-when-compile
  (defvar helper-bps)
  (defvar helper-loc)
  (defvar helper-tb)
  (defvar prompt-pat)
)

(declare-function realgud-loc-pat-regexp 'realgud-backtrace-mode)
(declare-function realgud-cmdbuf-info-loc-regexp 'realgud-buffer-command)
(declare-function test-simple-start 'test-simple)


(defun setup-regexp-vars(pat-hash)
  (setq helper-bps    (gethash "brkpt-set" pat-hash))
  (setq helper-loc    (gethash "loc"       pat-hash))
  (setq helper-tb     (gethash "lang-backtrace" pat-hash))
)

(defun loc-match(text var)
  "Match TEXT against regexp field VAR"
  (string-match (realgud-loc-pat-regexp var) text)
)

(defun bp-loc-match(text)
  (string-match (realgud-loc-pat-regexp helper-bps) text)
)

(defun tb-loc-match(text)
  (string-match (realgud-loc-pat-regexp helper-tb) text)
)

(defun cmdbuf-loc-match(text dbgr)
  "Match TEXT against cmdbuf-info-loc field VAR"
  (string-match (realgud-cmdbuf-info-loc-regexp dbgr) text)
  )

(defun prompt-match(prompt-str &optional num-str fmt-str)
  (unless fmt-str (setq fmt-str "debugger prompt %s"))
  (assert-equal 0 (string-match (realgud-loc-pat-regexp prompt-pat)
				prompt-str)
		(format fmt-str prompt-str))
  (cond (num-str
	 (assert-equal num-str (substring prompt-str
				       (match-beginning 1) (match-end 1))))
	('t 't))
  )
(provide 'realgud-regexp-helper)

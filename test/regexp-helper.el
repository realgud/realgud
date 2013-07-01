(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")

(eval-when-compile
  (defvar bps)
  (defvar loc)
  (defvar tb)
  (defvar prompt-pat)
)

(defun setup-regexp-vars(pat-hash)
  (setq bps    (gethash "brkpt-set" pat-hash))
  (setq loc    (gethash "loc"       pat-hash))
  (setq tb     (gethash "lang-backtrace" pat-hash))
)

(defun loc-match(text var)
  "Match TEXT against regexp field VAR"
  (string-match (realgud-loc-pat-regexp var) text)
)

(defun bp-loc-match(text)
  (string-match (realgud-loc-pat-regexp bps) text)
)

(defun tb-loc-match(text)
  (string-match (realgud-loc-pat-regexp tb) text)
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
  (if num-str
      (assert-equal num-str (substring prompt-str
				       (match-beginning 1) (match-end 1))))
)

(defun setup-regexp-vars(pat-hash)
  (setq bps    (gethash "brkpt-set" pat-hash))
  (setq loc    (gethash "loc"       pat-hash))
  (setq tb     (gethash "lang-backtrace" pat-hash))
)

(defun loc-match(text var) 
  (string-match (dbgr-loc-pat-regexp var) text)
)

(defun prompt-match(prompt-str &optional num-str fmt-str) 
  (unless fmt-str (setq fmt-str "debugger prompt %s"))
  (assert-equal 0 (string-match (dbgr-loc-pat-regexp prompt-pat)
				prompt-str)
		(format fmt-str prompt-str))
  (if num-str
      (assert-equal num-str (substring prompt-str 
				       (match-beginning 1) (match-end 1))))
)

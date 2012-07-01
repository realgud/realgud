(defun setup-regexp-vars(pat-hash)
  (setq bps    (gethash "brkpt-set" pat-hash))
  (setq loc    (gethash "loc"       pat-hash))
  (setq tb     (gethash "lang-backtrace" pat-hash))
)

(defun loc-match(text var) 
  (string-match (dbgr-loc-pat-regexp var) text)
)


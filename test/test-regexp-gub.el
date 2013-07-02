(require 'test-simple)
(load-file "../realgud/common/buffer/command.el")
(load-file "../realgud/debugger/gub/init.el")
(load-file "./regexp-helper.el")

(test-simple-start)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "gub")

(note "prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud-gub-pat-hash))
(prompt-match "gub[0]: ")
(prompt-match "gub[20]: ")
(prompt-match "gub[32@5]: ")

(end-tests)

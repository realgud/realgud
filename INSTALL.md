* Have `test-simple`, `loc-changes`, `cl-lib` and `load-relative` installed.
* From inside emacs, evaluate:
```lisp
  (compile (format "EMACSLOADPATH=:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc"))))
```

* Have `test-simple`, `loc-changes`, `cl-lib` and `load-relative` installed.
* From inside emacs, evaluate:
```lisp
  (compile (format "EMACSLOADPATH=:%s:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc")) (file-name-directory (locate-library "realgud.elc")) ))
  ```

After this you should be able to run:

    $ make         # byte compile everything
    $ make check   # run unit tests
    $ make install # may need to prefix with sudo


Also you can run from the source directory by running `eval-current-buffer`
when inside to top level `realgud.el` (that's the one that is in this folder).

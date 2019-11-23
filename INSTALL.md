Have `test-simple`, `loc-changes`, `cl-lib` and `load-relative` installed.

    $ cd realgud  # where you placed the project
    $ emacs


From inside emacs, evaluate:

```lisp
  (compile (format "EMACSLOADPATH=:%s:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "loc-changes.elc")) default-directory) )
  ```


Optional: if you want to install to a custom location, use ./configure as shown.
Note, that --prefix does not prefix the lispdir, you must use --with-lispdir in
addition to --prefix.

    ./configure --prefix=INSTALL_DIR --with-lispdir=INSTALL_DIR/share/emacs/site-lisp


After this you should be able to run:

    $ make         # byte compile everything
    $ make check   # run unit tests
    $ make install # may need to prefix with sudo


Also you can run from the source directory by running `eval-current-buffer`
when inside to top level `realgud.el` (that's the one that is in this folder).

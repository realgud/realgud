#!/bin/sh
# An autoconf setup script.
# From inside emacs, make sure test-simple is installed and then
# Press C-x C-e at the end of the next line run this script
# (test-simple-run "EMACSLOADPATH=%s ./autogen.sh"  (mapconcat 'identity load-path ":"))
ln -fs README.md README
touch common.mk
autoreconf -vi && \
autoconf && {
  echo "Running configure with --enable-maintainer-mode $@"
  ./configure --enable-maintainer-mode $@
}

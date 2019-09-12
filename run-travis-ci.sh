#!/bin/bash

cd "$(dirname "$0")"

set_default () {
  eval "
if [ -z \$$1 ]; then
  $1=$2
fi
"
}

set_default EMACS "$(which emacs)"

echo "*** Emacs version ***"
echo "EMACS =" $(which $EMACS)
$EMACS --version
echo

cask
NO_CHECK_EMACS_PACKAGES=1 /bin/bash ./autogen.sh && cd test && make check-cask

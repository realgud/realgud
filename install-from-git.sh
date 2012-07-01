#!/bin/bash
# Install emacs-dbgr from git
run_cmd() {
    echo "--- Running command: $@"
    $@
    rc=$?
    echo "--- $@ exit status is $?"
    return $rc
}

# environment variable SUDO_CMD could be "sudo" or "su root -c"
if (( $(id -u) != 0)) ; then 
    if [[ -z "$SUDO_CMD" ]] ; then
	need_sudo='sudo'
	if which $need_sudo >/dev/null 2>&1 ; then
	    try_cmd=''
	else
	    need_sudo='su root -c'
	    try_cmd='su'
	fi
    else
	need_sudo="$SUDO_CMD"
    fi
else
    need_sudo=''
    try_cmd=''
fi
for program in git make $try_cmd ; do
    if ! which $program >/dev/null 2>&1 ; then
	echo "Cant find program $program in $PATH"
	exit 1
    fi
done

for pkg in emacs-{test-simple,load-relative,loc-changes,dbgr} ; do 
    echo '******************************************'
    echo Trying to install ${pkg}...
    echo '******************************************'
    run_cmd git clone http://github.com/rocky/${pkg}.git
    (cd $pkg && \
        run_cmd $SHELL ./autogen.sh && \
	run_cmd ./configure && \
	run_cmd make && \
	run_cmd make check && \
        run_cmd $need_sudo make install
    )
done

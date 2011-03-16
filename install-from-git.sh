#!/bin/bash
# Install emacs-dbgr from git
run_cmd() {
    echo "--- Running command: $@"
    $@
    rc=$?
    echo "--- $@ exit status is $?"
    return $rc
}

if (( $(id -u) != 0)) ; then 
    need_sudo='sudo'
else
    need_sudo=''
fi
for program in git make $need_sudo ; do
    if ! which $program >/dev/null 2>&1 ; then
	echo "Cant find program $program in $PATH"
	exit 1
    fi
done

for pkg in emacs-{test-unit,load-relative,loc-changes,dbgr} ; do 
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

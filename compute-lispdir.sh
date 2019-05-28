#!/bin/bash
# Figures out a reasonable --prefix
typeset -i rc=0
typeset -i DEBUG=${DEBUG:-0}
EMACS_PROG=${EMACS_PROG:-emacs}
list=$($EMACS_PROG --batch --no-splash --no-site-file --eval '(message (substring (format "%s" load-path) 1 -1))' 2>&1)
rc=$?
if (( rc != 0 )) ; then
    echo  >&2 "Something went wrong running $EMACS_PROG"
    exit $rc
$cmd
fi
for dir in $list ; do
    if [[ -d $dir ]] ; then
	case $dir in
	    */emacs/site-lisp)
		((DEBUG)) && echo "site lisp: $dir"
		echo "$dir"
		exit 0
		;;
	esac
    fi
done
for dir in $list ; do
    if [[ -d $dir ]] ; then
	case $dir in
	    */emacs/2[5-8]\.[0-9]/site-lisp)
		((DEBUG)) && echo "versioned site lisp: $dir"
		echo "$dir"
		exit 0
		;;
	esac
    fi
done
for dir in $list ; do
    if [[ -d $dir ]] ; then
	case $dir in
	    */emacs/2[5-8]\.[0-9]/site-lisp)
		((DEBUG)) && echo "versioned site lisp: $dir"
		echo "$dir"
		exit 0
		;;
	esac
    fi
done
exit 0

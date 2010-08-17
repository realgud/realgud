AC_INIT(emacs-dbgr, 0.02vc,)
AC_CONFIG_SRCDIR(dbgr.el)
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

AC_PATH_PROG(EMACS, emacs, no)
if test x$EMACS != xno ; then
  $EMACS -batch -q -no-site-file -eval \
  '(if (<= emacs-major-version 22)
     (progn
       (error "You need GNU Emacs 23 or better.")
       (kill-emacs 1)
     ))'
  if test $? -ne 0 ; then
    AC_MSG_ERROR([Can't continue with this version of Emacs])
  fi
fi

##
## Find out where to install the debugger emacs lisp files
##
AM_PATH_LISPDIR
lispdir_dbgr=$lispdir/dbgr
AC_SUBST([lispdir])
AC_SUBST([lispdir_dbgr])

AM_CONDITIONAL(INSTALL_EMACS_LISP, test "x$lispdir_dbgr" != "x")

AC_CONFIG_FILES([Makefile \
		 dbgr/Makefile \
		 dbgr/common/Makefile \
		 dbgr/common/init/Makefile \
		 dbgr/gdb/Makefile \
		 dbgr/pydbgr/Makefile \
		 dbgr/rbdbgr/Makefile \
		 dbgr/rdebug/Makefile \
		 test/Makefile \
		 ])
AC_OUTPUT

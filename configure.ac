AC_INIT(emacs-dbgr, 0.01vc,)
AC_CONFIG_SRCDIR(dbgr.el)
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

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
		 dbgr/python/Makefile \
		 dbgr/rbdbgr/Makefile \
		 dbgr/rdebug/Makefile \
		 test/Makefile \
		 ])
AC_OUTPUT

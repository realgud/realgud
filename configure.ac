AC_INIT(rbdbgr-emacs, 0.01vc,)
AC_CONFIG_SRCDIR(dbgr-loc.el)
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

##
## Find out where to install the debugger emacs lisp files
##
AM_PATH_LISPDIR
AM_CONDITIONAL(INSTALL_EMACS_LISP, test "x$lispdir" != "x")

AC_CONFIG_FILES([Makefile \
		 python/Makefile \
		 ruby/Makefile \
		 ])
AC_OUTPUT

AC_INIT(emacs-dbgr, 0.02vc,)
AC_CONFIG_SRCDIR(dbgr.el)
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

AC_PATH_PROG([EMACS], [emacs], [emacs])
AC_MSG_NOTICE("Checking emacs version and prerequiste packages")
$EMACS -batch -q -no-site-file -eval \
  '(if (<= emacs-major-version 22)
       (progn
         (error "You need GNU Emacs 23 or better.")
         (kill-emacs 1)
       )
     (dolist (package (quote (loc-changes load-relative test-unit)))
        (require package))
   )'
if test $? -ne 0 ; then
    AC_MSG_ERROR([Can't continue until above error is corrected.])
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
		 dbgr/trepan/Makefile \
		 dbgr/trepanx/Makefile \
		 test/Makefile \
		 ])
AC_OUTPUT

SUBDIRS = gdb init python rbdbgr rdebug 

lispdir = @lispdir_dbgr@
lisp_files := $(wildcard *.el)

EXTRA_DIST = $(lisp_files)
lisp_LISP = $(lisp_files)

test: check

check:
	$(MAKE) -C ../test $@




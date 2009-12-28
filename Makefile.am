SUBDIRS = dbgr

lispdir = @lispdir@
lisp_files := $(wildcard dbgr.el)
lisp_LISP = $(lisp_files)


if MAINTAINER_MODE

CL = ChangeLog
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

test: check





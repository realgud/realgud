SUBDIRS = dbgr test

lispdir = @lispdir@
lisp_files := $(wildcard dbgr.el)
lisp_LISP = $(lisp_files)

include common.mk

EXTRA_DIST=common.mk README.textile dbgr.el

if MAINTAINER_MODE

CL = ChangeLog
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

#: Run all tests
test: check

#: Run all tests without bloated output
check-short:
	$(MAKE) check 2>&1  | ruby make-check-filter.rb






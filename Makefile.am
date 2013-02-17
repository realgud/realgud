SUBDIRS = dbgr test

GIT2CL ?= git2cl
RUBY   ?= ruby

lispdir = @lispdir@
lisp_files := $(wildcard dbgr.el)
lisp_LISP = $(lisp_files)

include common.mk

EXTRA_DIST=common.mk README.textile dbgr.el

if MAINTAINER_MODE

CL = ChangeLog
#: Create a ChangeLog file from git logs
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

#: Run all tests
test: check

#: Run all tests without bloated output
check-short:
	$(MAKE) check 2>&1  | $(RUBY) make-check-filter.rb

#: Run "make install"
install-short:
	$(MAKE) install 2>&1  | $(RUBY) make-check-filter.rb



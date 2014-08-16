# Note: This makefile include remake-style target comments.
# These comments before the targets start with #:
# remake --tasks to shows the targets and the comments

SUBDIRS = realgud test

GIT2CL ?= git2cl
RUBY   ?= ruby

lispdir = @lispdir@

include common.mk

PHONY=check clean dist distclean test check-short check-terse install-short

EXTRA_DIST+=common.mk README.md THANKS

if MAINTAINER_MODE

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

#: Run all tests without and show just the failure lines
check-terse:
	$(MAKE) check 2>&1  | $(RUBY) make-check-filter.rb | grep failure

#: Run "make install"
install-short:
	$(MAKE) install 2>&1  | $(RUBY) make-check-filter.rb

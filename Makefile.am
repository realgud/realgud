SUBDIRS = realgud

lisp_files = realgud.el
lisp_LISP = $(lisp_files)
include common.mk

#: Run all tests
test: check

check check-short:
	$(MAKE) -C test $@

#: Run "make install" non-verbosely
install-short:
	$(MAKE) install 2>&1  | $(RUBY) test/make-check-filter.rb

.PHONY: test check check-short rmChangeLog

# FIXME fix up to use the git2cl in MAINT_MAKEFILE
GIT2CL = ./git2cl

#: create ChangeLog for distirbution from 'git log'
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

SUBDIRS = init
MOSTLYCLEANFILES = *.elc

lispdir = @lispdir_dbgr@/common
lisp_files := $(wildcard *.el)

EXTRA_DIST = $(lisp_files)
lisp_LISP = $(lisp_files)

test: check

check:
	$(MAKE) -C ../../test $@




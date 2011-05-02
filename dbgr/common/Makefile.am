SUBDIRS = buffer init
MOSTLYCLEANFILES = *.elc

include $(top_srcdir)/common.mk

lispdir = @lispdir_dbgr@/$(notdir $(subdir))
lisp_files := $(wildcard *.el)

EXTRA_DIST = $(lisp_files)
lisp_LISP = $(lisp_files)

SUBDIRS = bashdb gdb kshdb perldb pydbgr rdebug remake \
	trepan trepanpl trepanx trepan8 zshdb
EXTRA_DIST = common.mk

.PHONY: check all

check:
	$(MAKE) -C $(top_srcdir)/test $@

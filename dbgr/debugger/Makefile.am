SUBDIRS = gdb pydbgr rdebug trepan trepanx zshdb
EXTRA_DIST = common.mk

.PHONY: check all
all: check

check:
	$(MAKE) -C ../../test $@

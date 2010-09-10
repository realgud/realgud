lispdir = @lispdir_dbgr@/common/init
lisp_files := $(wildcard *.el)
lisp_LISP = $(lisp_files)
EXTRA_DIST = $(lisp_files) 
MOSTLYCLEANFILES = *.elc

%: 
	$(MAKE) -C .. $@


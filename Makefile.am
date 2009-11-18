SUBDIRS = init ruby python 

lispdir = @lispdir_dbgr@
lisp_files := $(wildcard *.el)
test_files := $(wildcard test/test-*.el)

EXTRA_DIST = $(lisp_files) $(test_files) test/behave.el

CHECK_FILES = $(notdir $(test_files:.el=.run))

lisp_LISP = $(lisp_files)

check: $(CHECK_FILES)


# FIGURE out how to DRY the run rules.
test-cmdtbuf.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-core.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-file.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-loc.run: 
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-lochist.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp-python.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp-ruby.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-srcbuf.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-send.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-track.run:
	(cd $(srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )

.PHONY: $(CHECK_FILES)

if MAINTAINER_MODE

CL = ChangeLog
ChangeLog:
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

test: check





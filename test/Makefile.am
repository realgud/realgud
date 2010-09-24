# Whatever it is you want to do, it should be forwarded to the 
# to top-level directories
PHONY=check test all
EXTRA_DIST=gcd.py gcd.rb

all: check

%: 

test_files := $(wildcard test-*.el)

CHECK_FILES = $(notdir $(test_files:.el=.run))

check: $(CHECK_FILES)

# FIGURE out how to DRY the run rules.
test-bp.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-cmdbuf.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-core.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-dbgr.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-file.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-loc.run: 
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-lochist.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-pydbgr.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-rdebug.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp-pydbgr.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp-rbdbgr.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp-trepan.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp-trepanx.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-regexp-rdebug.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-srcbuf.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-send.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-shortkey.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-track.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )
test-track-mode.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )

.PHONY: $(CHECK_FILES)

%: 
	$(MAKE) -C .. $@


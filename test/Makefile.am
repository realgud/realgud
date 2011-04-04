# Whatever it is you want to do, it should be forwarded to the 
# to top-level directories
PHONY=check test all
EXTRA_DIST=gcd.py gcd.rb

all: 

test: check

test_files := $(wildcard test-*.el)

CHECK_FILES = $(notdir $(test_files:.el=.run))

check: $(CHECK_FILES)

test-%.run:
	(cd $(top_srcdir)/test && $(EMACS) -batch -Q -l $(@:.run=.el) )

%: 
	$(MAKE) -C .. $@


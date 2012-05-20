include $(top_srcdir)/common.mk

PHONY=check test all
EXTRA_DIST=gcd.py gcd.rb

all: 

#: same thing as "check"
test: check

test_files := $(wildcard test-*.el)

CHECK_FILES = $(notdir $(test_files:.el=.run))

#: Run all tests
check: $(CHECK_FILES)

#: Run all tests with minimum verbosity
check-short:
	$(MAKE) check 2>&1  | ruby ../make-check-filter.rb

test-%.run:
	(cd $(top_srcdir)/test && $(EMACS) --batch --no-site-file --no-splash --load $(@:.run=.el))

# Whatever it is you want to do, it should be forwarded to the 
# to top-level directories
%: 
	$(MAKE) -C .. $@


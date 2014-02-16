include $(top_srcdir)/common.mk

PHONY=check test all check-elget test-elget
EXTRA_DIST += gcd.py gcd.rb

#: Give some basic help
help:
	@echo "Run all tests using targets \"test\", \"check\" or \"check-short\". For"
	@echo "example:"
	@echo "    make check"
	@echo "    make check-short"
	@echo
	@echo "You can run a specific test like test-srcbuf.el by changing \".el\" to"
	@echo "\".run\". For example:"
	@echo "    make test-srcbuf.run"
	@echo
	@echo "You can all tests via the Emacs el-get package which loads dependent emacs "
	@echo "packages, like load-relative, using targets, \"check-elget\" or \"test-elget\" or"
	@echo "\"check-short-elget\"."
	@echo
	@echo  "You can run a specific test like test-srcbuf.el via el-get by changing \".el\""
	@echo "to \".elrun\" For example:"
	@echo "    make test-srcbuf.elrun"


#: same thing as "check"
test: check

#: same thing as "check-elget"
test-elget: check-elget

test_files := $(wildcard test-*.el)

CHECK_FILES = $(notdir $(test_files:.el=.run))
EL_GET_CHECK_FILES = $(notdir $(test_files:.el=.elrun))

#: Run all tests
check: $(CHECK_FILES)

#: Run all tests via el-get
check-elget: $(EL_GET_CHECK_FILES)

#: Run all tests with minimum verbosity
check-short:
	$(MAKE) check 2>&1  | ruby ../make-check-filter.rb

#: Run all tests with minimum verbosity via el-get
check-short-elget:
	$(MAKE) check-elget 2>&1  | ruby ../make-check-filter.rb

test-%.run:
	(cd $(top_srcdir)/test && $(EMACS) --batch --no-site-file --no-splash --load $(@:.run=.el))

#: Run tests using el-get to specify external Lisp dependencies
test-%.elrun:
	(cd $(top_srcdir)/test && $(EMACS) --batch --no-site-file --no-splash --load ../el-get-install.el --load $(@:.elrun=.el))

# Whatever it is you want to do, it should be forwarded to the
# to top-level directories
# %:
# 	$(MAKE) -C .. $@

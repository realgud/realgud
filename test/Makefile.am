include $(top_srcdir)/common.mk

lisp_files = $(wildcard *.el)
lisp_LISP = $(lisp_files)
EXTRA_DIST = $(lisp_files)

PHONY=check test all check-elget test-elget help

#: overall help on running the make targets
help:
	@echo "The main function of this Makefile is to facilitate running tests."
	@echo
	@echo "To run all of the tests, use targets \"test\", \"check\" or \"check-short\"."
	@echo "For example:"
	@echo
	@echo "    make check"
	@echo "or:"
	@echo "    make check-short"
	@echo
	@echo "The -short target uses a filter written in Ruby to remove extreanous output."
	@echo
	@echo "To run a specific test like test-srcbuf.el, change \".el\" to"
	@echo "\".run\". For example:"
	@echo
	@echo "    make test-srcbuf.run"
	@echo
	@echo "Tests can also be run via the Emacs el-get package and this loads dependent emacs "
	@echo "package, like load-relative. To do this, use targets, \"check-elget\","
	@echo "\"test-elget\", or \"check-short-elget\"."
	@echo
	@echo  "To run a specific test like test-srcbuf.el via el-get change \".el\""
	@echo "to \".elrun\" For example:"
	@echo
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

#: Run all tests via cask
check-cask: $(EL_GET_CHECK_FILES)

#: Run all tests with minimum verbosity
check-short:
	$(MAKE) check 2>&1  | ruby make-check-filter.rb

#: Run all tests with minimum verbosity via el-get
check-short-elget:
	$(MAKE) check-elget 2>&1  | ruby make-check-filter.rb

test-%.run:
	(cd $(top_srcdir)/test && EMACSLOADPATH=$(EMACSLOADPATH) $(EMACS) --batch --no-site-file --no-splash --load $(@:.run=.el))

#: Run tests using cask to specify external Lisp dependencies
test-%.elrun:
	(cd $(top_srcdir)/test && $(EMACS) --batch --no-site-file --no-splash --load ../cask-install.el --load $(@:.elrun=.el))

install-lispLISP: $(lisp_LISP) $(ELCFILES)

# Whatever it is you want to do, it should be forwarded to the
# to top-level directories
# %:
# 	$(MAKE) -C .. $@

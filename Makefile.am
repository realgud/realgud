# Note: This makefile include remake-style target comments.
# These comments before the targets start with #:
# remake --tasks to shows the targets and the comments

SUBDIRS = realgud test

GIT2CL ?= git2cl
RUBY   ?= ruby

lispdir = @lispdir@

include common.mk

PHONY=check clean dist distclean test check-short check-terse install-short

EXTRA_DIST=common.mk README.md THANKS  $(lisp_files) test/gcd.py test/gcd.rb test/gdb

if MAINTAINER_MODE

#: Remove change log: ChangeLog
rmChangeLog:
	rm ChangeLog || true

#: Create a ChangeLog file from git logs
ChangeLog: rmChangeLog
	git log --pretty --numstat --summary | $(GIT2CL) > $@

ACLOCAL_AMFLAGS=-I .

endif

#: Run all tests
test: check

#: Run all tests without bloated output
check-short:
	$(MAKE) check 2>&1  | $(RUBY) test/make-check-filter.rb

#: Run all tests without and show just the failure lines
check-terse:
	$(MAKE) check 2>&1  | $(RUBY) tes/make-check-filter.rb | grep failure

#: Run "make install"
install-short:
	$(MAKE) install 2>&1  | $(RUBY) test/make-check-filter.rb

.PHONY: test check check-short rmChangeLog check_copyrights

CR_EXCEPTIONS=cr_exceptions.txt
#: Check that copyrights are assigned to FSF
check_copyrights:
	@echo "Compute exceptions >$(CR_EXCEPTIONS)~"
	@export LC_ALL=C;					    \
	(cd $(top_srcdir) &&					    \
	find . -name '.git' -prune -o				    \
	       -name 'test' -prune -o				    \
	       -name '*.el' -print0 |				    \
	    xargs -0 grep -L 'Free Software Foundation, Inc' |	    \
	    grep -v '\(\.dir-locals\|.-\(pkg\|autoloads\)\)\.el$$'; \
	find . -name '.git' -prune -o -name '*.el' -type f -print | \
	    while read f; do					    \
	        fquoted="$$(echo $$f|tr '|' '_')";		    \
	        sed -n -e '/[Cc]opyright.*, *[1-9][-0-9]*,\?$$/N'   \
	            -e '/Free Software Foundation/d'		    \
	            -e "s|^\\(.*[Cc]opyright\\)|$$fquoted:\\1|p"    \
	           "$$f";					    \
	    done) | sort >$(CR_EXCEPTIONS)~
	diff -u "$(CR_EXCEPTIONS)" "$(CR_EXCEPTIONS)~"

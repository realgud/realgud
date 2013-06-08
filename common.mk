lisp_files := $(wildcard *.el)
lisp_LISP = $(lisp_files)
EXTRA_DIST = $(lisp_files)
MOSTLYCLEANFILES = *.elc

short:
	$(MAKE) 2>&1 >/dev/null | ruby $(top_srcdir)/make-check-filter.rb

%.short:
	$(MAKE) $(@:.short=) 2>&1 >/dev/null

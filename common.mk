short:
	$(MAKE) 2>&1 >/dev/null | ruby $(top_srcdir)/make-check-filter.rb

%.short:
	$(MAKE) $(@:.short=) 2>&1 >/dev/null

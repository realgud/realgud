#!/usr/bin/env ruby
# Use this to cut out the crud from make check.
# Use like this:
#   make check 2>&1  | ruby ../make-check-filter.rb
# See Makefile.am
pats = ["^(?:Loading",
        'make\[',
        "Making check in",
        '\(cd \.\.',
        "make -C",
        "Test-Unit",
        "Fontifying",
        '\s*$'
       ].join('|') + ')'
# puts pats
skip_re = /#{pats}/

while gets()
  next if $_ =~ skip_re
  puts $_
end

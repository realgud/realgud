# Directories to run recursive makes
SUBDIRS = common debugger lang

# lispdir needs to get set here because it seems
# to automatically use the parent root. Not sure
# why that is since none of the other subdirectories have this
# problem.
lispdir = ${datarootdir}/emacs/site-lisp/realgud

# By default this is based off of $lispdir, but that
# is set funny, so we'll be explicit about lispdir_realgud,
# although in fact it isn't used.
lispdir_realgud = ${datarootdir}/emacs/site-lisp/realgud

# .nosearch isn't a lisp file, but we want it installed along with the
# lisp files.
lisp_LISP = .nosearch

# Overwrite clean-lisp target so it doesn't remove .nosearch
# Note: There are no lisp files in this directory
clean-lisp:

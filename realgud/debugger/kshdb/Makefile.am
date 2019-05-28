include ../common.mk

lisp_files = $(wildcard *.el)
lisp_LISP = $(lisp_files)
EXTRA_DIST = $(lisp_files)

kshdb.elc: core.elc track-mode.elc
track-mode.elc: core.elc init.elc

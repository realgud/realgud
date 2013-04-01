A modular GNU Emacs front-end for interacting with external debuggers.

Debuggers we currently support are:

|NAME                                          | INVOCATION          | WHAT |
----------------------------------------------------------------------------------------------------------------
|[bashdb](http://bashdb.sf.net)                | `bashdb`            | *bash* |
|[Devel::Trepan](https://github.com/rocky/Perl-Devel-Trepan/wiki)    | trepan.pl | Perl5 |
|gdb                                           | `realgud-gdb`       | *gdb*  |
|[kshdb](https://github.com/rocky/kshdb/wiki)  | `kshdb`             | Korn Shell 93 |
|perldb (Perl)                                 | `perldb`            | stock Perl5 debugger |
|pdb                                           | `pdb`               | Stock C Python debugger |
|[pydb](http://bashdb.sourceforge.net/pydb/)   | `pydb`              | slighly enhanced *pdb* for Python 2.x |
| [pydbgr](http://code.google.com/p/pydbgr/)   | `pydbgr`            | trepanning debugger for Python 2.x |
|[trepanning](https://github.com/rocky/rb-trepanning/wiki)| `trepan` | trepanning debugger for a patched Ruby 1.9 |
|[rb8-trepanning](https://github.com/rocky/rb8-trepanning/wiki)|`trepan8`| MRI Ruby 1.8 and an unpatched YARV 1.9)|
|[rbx-trepanning](https://github.com/rocky/rbx-trepanning/wiki)|`trepanx`|trepanning debugger for Rubinius Ruby|
|[remake](http://bashdb.sf.net/remake)         | `remake`            |GNU Make|
|[ruby-debug](http://bashdb.sf.net/ruby-debug) | `rdebug`            | Ruby |
|[zshdb](https://github.com/rocky/zshdb/wiki)  | `zshdb`             | Zsh  |

The debugger is run out of a *comint* process buffer, or you can use a
`M-x track-mode` inside an existing shell.

To install you'll need a couple of other Emacs packages installed. See
[the installation instructions](http://wiki.github.com/rocky/emacs-dbgr/how-to-install)
for details.

To get started using see the
[notes on using emacs-dbgr](http://wiki.github.com/rocky/emacs-dbgr/how-to-use)
.

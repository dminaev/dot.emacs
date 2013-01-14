dnl
dnl Autoconf script for hyperbole
dnl

AC_PREREQ([2.50])

dnl Remember to update the version
AC_INIT(hyperbole, 5.0)
AM_INIT_AUTOMAKE

dnl for cygwin windows environment
AC_PATH_PROGS(CYGPATH, cygpath)

AC_ARG_WITH(vm, [AS_HELP_STRING(--with-vm,[install support for vm mail reader])])
AM_CONDITIONAL([COND_WITH_VM], [test "$with_vm" = "yes"]) 

AM_PATH_LISPDIR

AC_CONFIG_FILES([Makefile man/Makefile kotl/Makefile])
AC_OUTPUT

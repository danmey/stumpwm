#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(Stump Window Manager, 0.9.8-git, sabetts@gmail.com)

AC_SUBST(CONTRIB_DIR)
AC_SUBST(LISP_PROGRAM)
AC_SUBST(LISP)
AC_SUBST(PPCRE_PATH)

# Checks for programs.
AC_ARG_WITH(lisp,    [  --with-lisp=IMPL        use the specified lisp (sbcl, clisp, ccl or ecl)], LISP=$withval, LISP="sbcl")
AC_ARG_WITH(sbcl,    [  --with-sbcl=PATH        specify location of sbcl], SBCL_PATH=$withval, SBCL_PATH="")
AC_ARG_WITH(clisp,   [  --with-clisp=PATH       specify location of clisp], CLISP_PATH=$withval, CLISP_PATH="")
AC_ARG_WITH(ccl,     [  --with-ccl=PATH         specify location of ccl], CCL_PATH=$withval, CCL_PATH="")
AC_ARG_WITH(ecl,     [  --with-ecl=PATH         specify location of ecl], ECL_PATH=$withval, ECL_PATH="")
AC_ARG_WITH(ppcre,   [  --with-ppcre=PATH       specify location of cl-ppcre], PPCRE_PATH=$withval, PPCRE_PATH="`pwd`/cl-ppcre")

AC_ARG_WITH(contrib-dir,
                     [  --with-contrib-dir=PATH specify location of contrib modules],
                     CONTRIB_DIR=$withval, CONTRIB_DIR="`pwd`/contrib")

if test -x "$SBCL_PATH"; then
   SBCL=$SBCL_PATH
   AC_MSG_CHECKING([for sbcl])
   AC_MSG_RESULT($SBCL)
else
   AC_PATH_PROG([SBCL], sbcl,"")
fi

if test -x "$CLISP_PATH"; then
   CLISP=$CLISP_PATH
   AC_MSG_CHECKING([for clisp])
   AC_MSG_RESULT($CLISP)
else
   AC_PATH_PROG([CLISP],clisp,"")
fi

if test -x "$CCL_PATH"; then
   CCL=$CCL_PATH
   AC_MSG_CHECKING([for ccl])
   AC_MSG_RESULT($CCL)
else
   AC_PATH_PROG([CCL],ccl,"")
fi

if test -x "$ECL_PATH"; then
   ECL=$ECL_PATH
   AC_MSG_CHECKING([for ecl])
   AC_MSG_RESULT($ECL)
else
   AC_PATH_PROG([ECL], ecl,"")
fi

if test "x$LISP" = "xclisp"; then
   if test "x$CLISP" = "x"; then
      LISP=sbcl
      LISP_PROGRAM=$SBCL
   else
      LISP_PROGRAM=$CLISP
   fi
elif test "x$LISP" = "xsbcl"; then
   if test "x$SBCL" = "x"; then
      LISP=clisp
      LISP_PROGRAM=$CLISP
   else
      LISP_PROGRAM=$SBCL
   fi
elif test "x$LISP" = "xccl"; then
   if test "x$CCL" = "x"; then
      LISP=sbcl
      LISP_PROGRAM=$SBCL
   else
      LISP_PROGRAM=$CCL
   fi
elif test "x$LISP" = "xecl"; then
   if test "x$ECL" = "x"; then
      LISP=sbcl
      LISP_PROGRAM=$SBCL
   else
      LISP_PROGRAM=$ECL
   fi

fi

if test "x$LISP_PROGRAM" = "x"; then
   AC_MSG_ERROR([*** No lisp is available.])
fi

AC_MSG_NOTICE([Using $LISP at $LISP_PROGRAM])

# check for makeinfo

AC_CHECK_PROG(MAKEINFO,makeinfo,yes,no)

if test "$MAKEINFO" = "no"; then
   AC_MSG_ERROR([Please install makeinfo for the manual.])
fi

AC_CHECK_PROG(XDPYINFO,xdpyinfo,yes,no)
if test "$XDPINFO" = "no"; then
   AC_MSG_WARN([xdpyinfo is needed for xinerama support.])
fi

if test "$LISP" = "clisp"; then
   AC_CHECK_FILE([$PPCRE_PATH/cl-ppcre.asd],,AC_MSG_ERROR([Cannot find ppcre. When using clisp you must specify its location using --with-ppcre]))
fi

# XXX How to do an OR ?
if test "$LISP" = "ecl"; then
   AC_CHECK_FILE([$PPCRE_PATH/cl-ppcre.asd],,AC_MSG_ERROR([Cannot find ppcre. When using ecl you must specify its location using --with-ppcre]))
fi

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_OUTPUT(Makefile)
AC_OUTPUT(make-image.lisp)
AC_OUTPUT(version.lisp)

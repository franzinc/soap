# $Id: Makefile,v 2.10 2009/03/17 18:48:48 mm Exp $

include ../../makefile.top
include ../../makefile.defs

ifeq ($(OS_NAME),windows)
windows_args = +s build.tmp +B +cn
else
unix_args = +s build.tmp
endif

mlisp = ../lisp $(run_lisp_args) $(windows_args) \
	-I dcl $(unix_args) -d build.out -batch -q

all: clean compile DIST

compile: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "build.cl")' >> build.tmp
	echo '(exit)' >> build.tmp
	$(mlisp)
	rm -f build.tmp

## contains the license check
LICENSE_FASL = ../private/addon2.fasl

## for ACL distributions
DIST: FORCE
	rm -fr DIST
	mkdir DIST \
	      DIST/examples \
	      DIST/examples/soap \
	      DIST/code
	cat $(LICENSE_FASL) soap.fasl > DIST/code/soap.fasl
	cp -p soapex.cl DIST/examples/soap
	cp -p soapval1.cl DIST/examples/soap
	cp -p bignum-server.cl DIST/examples/soap

soap.001 soap.002 soap.003 soap.004 soap.005 soap.006: clean compile
	rm -fr DIST DIST.src
	mkdir DIST \
	      DIST/code
	cp -p soap.fasl DIST/code/$@

clean: FORCE
	rm -fr *.fasl *.out DIST DIST.src soap[am].cl

FORCE:

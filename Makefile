# $Id: Makefile,v 2.3 2006/02/02 18:12:09 layer Exp $

include ../../makefile.top
include ../../makefile.defs

ifeq ($(OS_NAME),windows)
windows_args = +s build.tmp +B +cn
else
unix_args = +s build.tmp
endif

mlisp = ../lisp $(run_lisp_args) $(windows_args) \
	-I dcl $(unix_args) -d buildm.out -batch -q
alisp = ../lisp $(run_lisp_args) $(windows_args) \
	-I adcl $(unix_args) -d builda.out -batch -q

all: clean compile DIST

compile: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "build.cl")' >> build.tmp
	echo '(exit)' >> build.tmp
	$(mlisp)
	$(alisp)
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
	cp -p soap.fasl DIST/code/soap.fasl
	cat $(LICENSE_FASL) soapa.fasl > DIST/code/soapa.fasl
	cat $(LICENSE_FASL) soapm.fasl > DIST/code/soapm.fasl
	cp -p soapex.cl DIST/examples/soap
	cp -p soapval1.cl DIST/examples/soap

# Latest 7.0 patch:
DIST.004: clean compile
	rm -fr DIST DIST.src
	mkdir DIST \
	      DIST/code
	cp -p soap.fasl DIST/code/soap.004
	cat $(LICENSE_FASL) soapa.fasl > DIST/code/soapa.004
	cat $(LICENSE_FASL) soapm.fasl > DIST/code/soapm.004

# Latest 8.0 patch:
DIST.001: clean compile
	rm -fr DIST DIST.src
	mkdir DIST \
	      DIST/code
	cp -p soap.fasl DIST/code/soap.001
	cat $(LICENSE_FASL) soapa.fasl > DIST/code/soapa.001
	cat $(LICENSE_FASL) soapm.fasl > DIST/code/soapm.001

## 7.0 patch version 002:
old-patch-dist: clean compile
	rm -fr DIST DIST.src
	mkdir DIST DIST/code DIST/examples DIST/examples/soap
	cp -p soap.fasl DIST/code/soap.002
	cat $(LICENSE_FASL) soapa.fasl > DIST/code/soapa.002
	cat $(LICENSE_FASL) soapm.fasl > DIST/code/soapm.002
	cp -p soapex.cl DIST/examples/soap
	cp -p soapval1.cl DIST/examples/soap
	cp -p bignum-server.cl DIST/examples/soap

clean: FORCE
	rm -fr *.fasl *.out DIST DIST.src soap[am].cl

FORCE:

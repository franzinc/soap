# $Id: Makefile,v 2.1 2004/02/13 05:35:28 layer Exp $

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

patch: clean compile patch-dist

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

## 6.2 patch version 003:
patch-dist: FORCE
	rm -fr DIST DIST.src
	mkdir DIST \
	      DIST/code
	cp -p soap.fasl DIST/code/soap.003
	cat $(LICENSE_FASL) soapa.fasl > DIST/code/soapa.003
	cat $(LICENSE_FASL) soapm.fasl > DIST/code/soapm.003

## 6.2 patch version 002:
patch-dist.002: FORCE
	rm -fr DIST DIST.src
	mkdir DIST DIST.src \
	      DIST.src/examples \
	      DIST.src/examples/soap \
	      DIST.src/doc \
	      DIST/code
	cp -p soap.fasl DIST/code/soap.002
	cat $(LICENSE_FASL) soapa.fasl > DIST/code/soapa.002
	cat $(LICENSE_FASL) soapm.fasl > DIST/code/soapm.002
	cp -p ../../html6/soap.htm DIST.src/doc
	cp -p soapex.cl DIST.src/examples/soap
	cp -p soapval1.cl DIST.src/examples/soap

## 6.2 patch version 001:
patch-dist.001: FORCE
	rm -fr DIST DIST.src
	mkdir DIST DIST.src \
	      DIST.src/examples \
	      DIST.src/examples/soap \
	      DIST.src/doc \
	      DIST/code
	cp -p soap.fasl DIST/code/soap.001
	cat $(LICENSE_FASL) soapa.fasl > DIST/code/soapa.001
	cat $(LICENSE_FASL) soapm.fasl > DIST/code/soapm.001
	cp -p ../../html6/soap.htm DIST.src/doc
	cp -p soapex.cl DIST.src/examples/soap
	cp -p soapval1.cl DIST.src/examples/soap

## 6.2 patch version 000
patch-dist.000: FORCE
	rm -fr DIST DIST.src
	mkdir DIST DIST.src \
	      DIST.src/examples \
	      DIST.src/examples/soap \
	      DIST.src/doc \
	      DIST/code
	cp -p soap.fasl DIST/code/soap.fasl
	cat $(LICENSE_FASL) soapa.fasl > DIST/code/soapa.fasl
	cat $(LICENSE_FASL) soapm.fasl > DIST/code/soapm.fasl
	cp -p ../xmlutils/pxml.fasl DIST/code/pxml.001
	cp -p ../../html6/soap.htm DIST.src/doc
	cp -p soapex.cl DIST.src/examples/soap
	cp -p soapval1.cl DIST.src/examples/soap

clean: FORCE
	rm -fr *.fasl *.out DIST DIST.src

FORCE:

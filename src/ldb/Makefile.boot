# Makefile for generating the real Makefile.
# $Header: /project/cmucl/cvsroot/src/ldb/Attic/Makefile.boot,v 1.3 1991/11/08 00:22:04 wlott Exp $
#

Makefile: Makefile.orig
	/usr/cs/lib/cpp < Makefile.orig > Makefile.NEW
	mv Makefile.NEW Makefile
	make depend

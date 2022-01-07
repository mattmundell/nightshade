# $Header: /home/CVS-cmucl/src/lisp/GNUmakefile,v 1.13 1997/06/07 15:25:37 pw Exp $

all: nightshade.nm

CC = gcc

DEPEND_FLAGS =

include Config

ifndef GC_SRC
GC_SRC = gc.c
endif

SRCS = nightshade.c coreparse.c alloc.c monitor.c print.c interr.c \
	vars.c parse.c interrupt.c search.c validate.c globals.c \
	dynbind.c breakpoint.c regnames.c backtrace.c save.c purify.c \
	${ARCH_SRC} ${ASSEM_SRC} ${OS_SRC} ${GC_SRC}

OBJS = $(patsubst %.c,%.o,$(patsubst %.S,%.o,$(patsubst %.s,%.o,$(SRCS))))

### Don't look in RCS for the files, because we might not want the latest.
%: RCS/%,v

nightshade.nm: nightshade
	echo 'Map file for lisp version ' `cat version` > ,nightshade.nm
	$(NM) nightshade | grep -v " F \| U " >> ,nightshade.nm
	mv ,nightshade.nm nightshade.nm

nightshade: version.c ${OBJS} version
	echo '1 + ' `cat version` | bc > ,version
	mv ,version version
	$(CC) ${CFLAGS} -DVERSION=`cat version` -c $<
	$(CC) -g ${OS_LINK_FLAGS} -o ,nightshade \
		${OBJS} version.o \
		${OS_LIBS} -lm
	mv -f ,nightshade nightshade

version:
	echo 0 > version

internals.h:
	@echo "You must run genesis to create internals.h!"
	@false

clean:
	rm -f Depends *.o nightshade nightshade.nm core

depend: Depends

Depends: ${SRCS}
	$(CC) -MM -E ${DEPEND_FLAGS} ${CFLAGS} ${CPPFLAGS} $? > ,depends
	mv ,depends Depends

include Depends

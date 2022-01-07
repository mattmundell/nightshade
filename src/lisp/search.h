/*
 * $Header: /home/CVS-cmucl/src/lisp/search.h,v 1.1 1992/07/28 20:15:32 wlott Exp $
 */

#ifndef _SEARCH_H_
#define _SEARCH_H_

extern boolean search_for_type(int type, lispobj **start, int *count);
extern boolean search_for_symbol(char *name, lispobj **start, int *count);

#endif

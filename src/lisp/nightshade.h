#ifndef _NIGHTSHADE_H_
#define _NIGHTSHADE_H_

#define lowtag_Bits 3
#define lowtag_Mask ((1<<lowtag_Bits)-1)
#define LowtagOf(obj) ((obj)&lowtag_Mask)
#define type_Bits 8
#define type_Mask ((1<<type_Bits)-1)
#define TypeOf(obj) ((obj)&type_Mask)
#define HeaderValue(obj) ((unsigned long) ((obj)>>type_Bits))

#define Pointerp(obj) ((obj) & 0x01)
#define PTR(obj) ((obj)&~lowtag_Mask)

#define CONS(obj) ((struct cons *)((obj)-type_ListPointer))
#define SYMBOL(obj) ((struct symbol *)((obj)-type_OtherPointer))
#define FDEFN(obj) ((struct fdefn *)((obj)-type_OtherPointer))

#if !defined alpha
typedef unsigned long lispobj;

#if defined(__FreeBSD__) || defined(__linux__)
typedef unsigned int u32;
typedef signed int s32;
#endif

#else
typedef unsigned int u32;
typedef signed int s32;
typedef u32 lispobj;
#endif

#define make_fixnum(n) ((lispobj)((n)<<2))
#define fixnum_value(n) (((long)n)>>2)

#define boolean int
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define SymbolValue(sym) \
    (((struct symbol *)((sym)-type_OtherPointer))->value)
#define SetSymbolValue(sym,val) \
    (((struct symbol *)((sym)-type_OtherPointer))->value = (val))

/* This only works for static symbols. */
#define SymbolFunction(sym) \
    (((struct fdefn *)(SymbolValue(sym)-type_OtherPointer))->function)

#endif /* _NIGHTSHADE_H_ */

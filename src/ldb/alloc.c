/* $Header: /project/cmucl/cvsroot/src/ldb/Attic/alloc.c,v 1.7 1991/10/22 18:36:50 wlott Exp $ */
#include "lisp.h"
#include "ldb.h"
#include "alloc.h"
#include "globals.h"

#ifdef ibmrt
#define GET_FREE_POINTER() ((lispobj *)SymbolValue(ALLOCATION_POINTER))
#define SET_FREE_POINTER(new_value) \
    (SetSymbolValue(ALLOCATION_POINTER,(lispobj)(new_value)))
#define GET_GC_TRIGGER() ((lispobj *)SymbolValue(INTERNAL_GC_TRIGGER))
#define SET_GC_TRIGGER(new_value) \
    (SetSymbolValue(INTERNAL_GC_TRIGGER,(lispobj)(new_value)))
#else
#define GET_FREE_POINTER() current_dynamic_space_free_pointer
#define SET_FREE_POINTER(new_value) \
    (current_dynamic_space_free_pointer = (new_value))
#define GET_GC_TRIGGER() current_auto_gc_trigger
#define SET_GC_TRIGGER(new_value) \
    clear_auto_gc_trigger(); set_auto_gc_trigger(new_value);
#endif



/****************************************************************
Allocation Routines.
****************************************************************/

static lispobj *alloc(bytes)
int bytes;
{
    lispobj *result;

    /* Round to dual word boundry. */
    bytes = (bytes + lowtag_Mask) & ~lowtag_Mask;

    result = GET_FREE_POINTER();
    SET_FREE_POINTER(result + (bytes / sizeof(lispobj)));

    if (GET_GC_TRIGGER() && GET_FREE_POINTER() > GET_GC_TRIGGER()) {
	SET_GC_TRIGGER((char *)GET_FREE_POINTER()
		       - (char *)current_dynamic_space);
    }

    return result;
}

lispobj *alloc_unboxed(type, words)
int type, words;
{
    lispobj *result;

    result = alloc((1 + words) * sizeof(lispobj));

    *result = (lispobj) (words << type_Bits) | type;

    return result;
}

lispobj alloc_vector(type, length, size)
int type, length, size;
{
    struct vector *result;

    result = (struct vector *)alloc((2 + (length*size + 31) / 32) * sizeof(lispobj));

    result->header = type;
    result->length = fixnum(length);

    return ((lispobj)result)|type_OtherPointer;
}

lispobj alloc_cons(car, cdr)
lispobj car, cdr;
{
    struct cons *ptr = (struct cons *)alloc(sizeof(struct cons));

    ptr->car = car;
    ptr->cdr = cdr;

    return (lispobj)ptr | type_ListPointer;
}

lispobj alloc_number(n)
long n;
{
    struct bignum *ptr;

    if (-0x20000000 < n && n < 0x20000000)
        return fixnum(n);
    else {
        ptr = (struct bignum *)alloc_unboxed(type_Bignum, 1);

        ptr->digits[0] = n;

	return (lispobj) ptr | type_OtherPointer;
    }
}

lispobj alloc_string(str)
char *str;
{
    int len = strlen(str);
    lispobj result = alloc_vector(type_SimpleString, len+1, 8);
    struct vector *vec = (struct vector *)PTR(result);

    vec->length = fixnum(len);
    strcpy(vec->data, str);

    return result;
}

lispobj alloc_sap(ptr)
char *ptr;
{
    struct sap *sap = (struct sap *)alloc_unboxed(type_Sap, 1);

    sap->pointer = ptr;

    return (lispobj) sap | type_OtherPointer;
}


/* $Header: /home/CVS-cmucl/src/lisp/vars.c,v 1.2 1994/10/25 00:21:19 ram Exp $ */
#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>

#include "lisp.h"
#include "vars.h"
#include "os.h"

#define NAME_BUCKETS 31
#define OBJ_BUCKETS 31

static struct var *NameHash[NAME_BUCKETS], *ObjHash[OBJ_BUCKETS];
static int tempcntr = 1;

struct var {
    lispobj obj;
    lispobj (*update_fn)(struct var *var);
    char *name;
    long clock;
    boolean map_back, permanent;

    struct var *nnext; /* Next in name list */
    struct var *onext; /* Next in object list */
};

static int hash_name(char *name)
{
    unsigned long value = 0;

    while (*name != '\0') {
        value = (value << 1) ^ *(unsigned char *)(name++);
        value = (value & (1-(1<<24))) ^ (value >> 24);
    }

    return value % NAME_BUCKETS;
}

static int hash_obj(lispobj obj)
{
    return (unsigned long)obj % OBJ_BUCKETS;
}


void flush_vars()
{
    int index;
    struct var *var, *next, *perm = NULL;

    /* Note: all vars in the object hash table also appear in the name hash table, so if we free everything in the name hash table, we free everything in the object hash table. */

    for (index = 0; index < NAME_BUCKETS; index++)
        for (var = NameHash[index]; var != NULL; var = next) {
            next = var->nnext;
            if (var->permanent) {
                var->nnext = perm;
                perm = var;
            }
            else {
                free(var->name);
                free(var);
            }
        }
    bzero(NameHash, sizeof(NameHash));
    bzero(ObjHash, sizeof(ObjHash));
    tempcntr = 1;

    for (var = perm; var != NULL; var = next) {
        next = var->nnext;
        index = hash_name(var->name);
        var->nnext = NameHash[index];
        NameHash[index] = var;
        if (var->map_back) {
            index = hash_obj(var->obj);
            var->onext = ObjHash[index];
            ObjHash[index] = var;
        }
    }
}

struct var *lookup_by_name(name)
char *name;
{
    struct var *var;

    for (var = NameHash[hash_name(name)]; var != NULL; var = var->nnext)
        if (strcmp(var->name, name) == 0)
            return var;
    return NULL;
}

struct var *lookup_by_obj(obj)
lispobj obj;
{
    struct var *var;

    for (var = ObjHash[hash_obj(obj)]; var != NULL; var = var->onext)
        if (var->obj == obj)
            return var;
    return NULL;
}

static struct var *make_var(char *name, boolean perm)
{
    struct var *var = (struct var *)malloc(sizeof(struct var));
    char buffer[256];
    int index;

    if (name == NULL) {
        sprintf(buffer, "%d", tempcntr++);
        name = buffer;
    }
    var->name = (char *)malloc(strlen(name)+1);
    strcpy(var->name, name);
    var->clock = 0;
    var->permanent = perm;
    var->map_back = FALSE;
    
    index = hash_name(name);
    var->nnext = NameHash[index];
    NameHash[index] = var;

    return var;
}    

struct var *define_var(char *name, lispobj obj, boolean perm)
{
    struct var *var = make_var(name, perm);
    int index;

    var->obj = obj;
    var->update_fn = NULL;

    if (lookup_by_obj(obj) == NULL) {
        var->map_back = TRUE;
        index = hash_obj(obj);
        var->onext = ObjHash[index];
        ObjHash[index] = var;
    }

    return var;
}

struct var *define_dynamic_var(char *name, lispobj updatefn(struct var *),
			       boolean perm)
{
    struct var *var = make_var(name, perm);

    var->update_fn = updatefn;

    return var;
}

char *var_name(struct var *var)
{
    return var->name;
}

lispobj var_value(struct var *var)
{
    if (var->update_fn != NULL)
        var->obj = (*var->update_fn)(var);
    return var->obj;
}

long var_clock(struct var *var)
{
    return var->clock;
}

void var_setclock(struct var *var, long val)
{
    var->clock = val;
}

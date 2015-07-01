/*
Copyright (c) 2015, Sky Leonard
All rights reserved.
For more info see COPYING or http://opensource.org/licenses/BSD-3-Clause
 */

#include "partial.h"

#include <stdlib.h>
#include <ctype.h>

#include "ffi.h"

/**************************************
 * Default values
 **************************************/

static char * DEFAULT_STRUCT_NAME = "Unknown";
static char * DEFAULT_FUNCTION_NAME = "function";
static char * INDENT = "    ";

static char * PRIMITIVE_NAMES[] = {
    "void",
    "uint8_t",
    "int8_t",
    "uint16_t",
    "int16_t",
    "uint32_t",
    "int32_t",
    "uint64_t",
    "int64_t",
    "float",
    "double",
    "unsigned char",
    "signed char",
    "unsigned short",
    "signed short",
    "unsigned int",
    "signed int",
    "unsigned long",
    "signed long",
    "long double",
    "void *",
    "size_t",
};

static ffi_type * FFI_TYPES[] = {
    &ffi_type_void,
    &ffi_type_uint8,
    &ffi_type_sint8,
    &ffi_type_uint16,
    &ffi_type_sint16,
    &ffi_type_uint32,
    &ffi_type_sint32,
    &ffi_type_uint64,
    &ffi_type_sint64,
    &ffi_type_float,
    &ffi_type_double,
    &ffi_type_uchar,
    &ffi_type_schar,
    &ffi_type_ushort,
    &ffi_type_sshort,
    &ffi_type_uint,
    &ffi_type_sint,
    &ffi_type_ulong,
    &ffi_type_slong,
    &ffi_type_pointer,
#define CHECK_SIZE_T
#if SIZE_MAX && SIZE_MAX == UINT16_MAX
    &ffi_type_uint16,
#elif SIZE_MAX && SIZE_MAX == UINT32_MAX
    &ffi_type_uint32,
#elif SIZE_MAX && SIZE_MAX == UINT64_MAX
    &ffi_type_uint64,
#else
    NULL,
#    undef CHECK_SIZE_T
#    define CHECK_SIZE_T if (FFI_TYPES[PartialType_size_t] == NULL){\
        if (sizeof(size_t) == sizeof(uint16_t)){\
            FFI_TYPES[PartialType_size_t] = &ffi_type_uint16;\
        } else if (sizeof(size_t) == sizeof(uint32_t)){\
            FFI_TYPES[PartialType_size_t] = &ffi_type_uint32;\
        } else if (sizeof(size_t) == sizeof(uint64_t)){\
            FFI_TYPES[PartialType_size_t] = &ffi_type_uint64;\
        }\
    }
#endif
};

/**************************************
 * Types
 **************************************/

struct Partial {
    struct PartialFDef * targetdef;
    void * target;
    size_t nargs;
    union {
        uint64_t s;
        uint8_t * d;
    } argsset;
    union PartialArg * args;
    struct PartialFDef * extdef;
    void ** ffi_args;
    void * ffi_writable;
    void * ffi_exc;
};

struct PartialSDef {
    char * name;
    size_t nmemb;
    size_t imemb;
    enum PartialType * members;
    struct PartialSDef ** struct_members;
    ffi_type ffi_type_;
};

struct PartialFDef {
    char * name;
    enum PartialType returns;
    struct PartialSDef * returns_struct;
    size_t nargs;
    size_t iargs;
    enum PartialType * args;
    struct PartialSDef ** struct_args;
    int ffi_setup;
    ffi_cif ffi_cif_;
    ffi_type * ffi_rtype;
    ffi_type ** ffi_argtypes;
};


/**************************************
 * Utility
 **************************************/

#define ONE64 ((uint64_t) 1)

#define PARTIALSET_IN(p, n) ((p->nargs > 64) ?\
    (p->argsset.d[(n) >> 3] & (1 << ((n) & 0x7))) :\
    (p->argsset.s & (ONE64 << (n))))

#define PARTIALSET_SET(p, n) if (p->nargs > 64){\
        p->argsset.d[(n) >> 8] |= (1 << ((n) & 0xff));\
    } else {\
        p->argsset.s |= (ONE64 << (n));\
    }

static int types_compatible(enum PartialType a, enum PartialType b){
    return (FFI_TYPES[a] == FFI_TYPES[b]);
}

static enum PartialType char2type(char c){
    switch (c){
        case 'v':
            return PartialType_void;
        case 'B':
            return PartialType_uint8;
        case 'b':
            return PartialType_sint8;
        case 'H':
            return PartialType_uint16;
        case 'h':
            return PartialType_sint16;
        case 'J':
            return PartialType_uint32;
        case 'j':
            return PartialType_sint32;
        case 'Q':
            return PartialType_uint64;
        case 'q':
            return PartialType_sint64;
        case 'f':
            return PartialType_float;
        case 'd':
            return PartialType_double;
        case 'C':
            return PartialType_uchar;
        case 'c':
            return PartialType_schar;
        case 'S':
            return PartialType_ushort;
        case 's':
            return PartialType_sshort;
        case 'I':
            return PartialType_uint;
        case 'i':
            return PartialType_sint;
        case 'L':
            return PartialType_ulong;
        case 'l':
            return PartialType_slong;
        case 'p':
            return PartialType_pointer;
        case 'T':
            return PartialType_size_t;
        default:
            return PartialType_struct;  // used as error marker
    }
}

/**************************************
 * Structure definition operations
 **************************************/

struct PartialSDef * partial_sdef(size_t nmemb, ...){
    va_list members;
    struct PartialSDef * res;

    va_start(members, nmemb);
    res = partial_sdef_va(nmemb, &members);
    va_end(members);
    return res;
}

struct PartialSDef * partial_sdef_va(
        size_t nmemb, va_list * members){
    struct PartialSDef * self;
    enum PartialType type;
    struct PartialSDef * structmemb;
    size_t i;

    self = partial_sdef_empty(nmemb);
    if (self != NULL){
        for (i = 0; i < nmemb; ++i){
            type = va_arg(*members, enum PartialType);
            switch (type) {
                case PartialType_void:
                    partial_sdef_free(self);
                    return NULL;
                case PartialType_struct:
                    structmemb = va_arg(*members, struct PartialSDef *);
                    partial_sdef_nextmemb_struct(self, structmemb);
                    break;
                default:
                    partial_sdef_nextmemb(self, type);
                    break;
            }
        }
    }
    return self;
}

struct PartialSDef * partial_sdef_empty(size_t nmemb){
    struct PartialSDef * self;
    enum PartialType * members;
    struct PartialSDef ** struct_members;
    ffi_type ** ffi_elements;

    self = malloc(sizeof(struct PartialSDef));
    if (self == NULL){
        return NULL;
    }
    if (nmemb != 0){
        members = malloc(nmemb * sizeof(enum PartialType));
        if (members == NULL){
            free(self);
            return NULL;
        }
        struct_members = malloc(nmemb * sizeof(struct PartialSDef *));
        if (struct_members == NULL){
            free(members);
            free(self);
            return NULL;
        }
    }
    ffi_elements = malloc((nmemb + 1) * sizeof(ffi_type *));
    if (ffi_elements == NULL){
        free(struct_members);
        free(members);
        free(self);
        return NULL;
    }
    ffi_elements[nmemb] = NULL;
    self->name = DEFAULT_STRUCT_NAME;
    self->nmemb = nmemb;
    self->imemb = 0;
    self->members = members;
    self->struct_members = struct_members;
    self->ffi_type_.size = 0;
    self->ffi_type_.alignment = 0;
    self->ffi_type_.type = FFI_TYPE_STRUCT;
    self->ffi_type_.elements = ffi_elements;
    return self;
}

struct PartialSDef * partial_sdef_fromstr(char * format){
    struct PartialSDef * self;
    char c;
    char * fmt;
    size_t nmemb = 0;
    enum PartialType type;

    fmt = format;
    while ((c = *fmt++)){
        nmemb += 1;
    }
    self = partial_sdef_empty(nmemb);
    if (self != NULL){
        fmt = format;
        while ((c = *fmt++)){
            if (isspace(c)){
                continue;
            }
            type = char2type(c);
            switch (type) {
                case PartialType_struct:
                case PartialType_void:
                    partial_sdef_free(self);
                    return NULL;
                default:
                    partial_sdef_nextmemb(self, type);
                    break;
            }
        }
    }
    return self;
}

void partial_sdef_free(struct PartialSDef * self){
    free(self->ffi_type_.elements);
    if (self->nmemb != 0){
        free(self->struct_members);
        free(self->members);
    }
    free(self);
}

void partial_sdef_nextmemb(
        struct PartialSDef * self, enum PartialType type){
    CHECK_SIZE_T;
    self->members[self->imemb] = type;
    self->ffi_type_.elements[self->imemb] = FFI_TYPES[type];
    self->imemb += 1;
}

void partial_sdef_nextmemb_struct(
        struct PartialSDef * self, struct PartialSDef * type){
    self->members[self->imemb] = PartialType_struct;
    self->struct_members[self->imemb] = type;
    self->ffi_type_.elements[self->imemb] = &(type->ffi_type_);
    self->imemb += 1;
}

int partial_sdef_equal(
        struct PartialSDef * a, struct PartialSDef * b){
    size_t i;

    if (a->nmemb != b->nmemb){
        return 0;
    }
    for (i = 0; i < a->nmemb; ++i){
        if (!types_compatible(a->members[i], b->members[i])){
            return 0;
        }
        if (a->members[i] == PartialType_struct){
            if (!partial_sdef_equal(
                    a->struct_members[i], b->struct_members[i])){
                return 0;
            }
        }
    }
    return 1;
}

void partial_sdef_setname(struct PartialSDef * self, char * name){
    self->name = name;
}

void partial_sdef_print(struct PartialSDef * self){
    // TODO: maybe make this not a pass-through?
    partial_sdef_fprint(stdout, self);
}

void partial_sdef_fprint(FILE * stream, struct PartialSDef * self){
    size_t i;
    enum PartialType type;

    fprintf(stream, "struct %s {\n", self->name);
    for (i = 0; i < self->nmemb; ++i){
        type = self->members[i];
        if (type == PartialType_struct){
            fprintf(stream, "%sstruct %s;\n",
                    INDENT, self->struct_members[i]->name);
        } else {
            fprintf(stream, "%s%s;\n", INDENT, PRIMITIVE_NAMES[type]);
        }
    }
    fputs("};\n", stream);
}

void partial_sdef_snprint(
        char * buffer, size_t n, struct PartialSDef * self){
    // TODO
}

/**************************************
 * Function definition operations
 **************************************/

struct PartialFDef * partial_fdef(
        enum PartialType returns, size_t nargs, ...){
    va_list args;
    struct PartialFDef * res;

    va_start(args, nargs);
    res = partial_fdef_va(returns, nargs, &args);
    va_end(args);
    return res;
}

struct PartialFDef * partial_fdef_va(
        enum PartialType returns, size_t nargs, va_list * args){
    struct PartialFDef * self;
    enum PartialType type;
    struct PartialSDef * structarg;
    size_t i;

    if (returns == PartialType_struct){
        structarg = va_arg(*args, struct PartialSDef *);
        self = partial_fdef_empty_struct(structarg, nargs);
    } else {
        self = partial_fdef_empty(returns, nargs);
    }
    if (self != NULL){
        for (i = 0; i < nargs; ++i) {
            type = va_arg(*args, enum PartialType);
            switch (type) {
                case PartialType_void:
                    partial_fdef_free(self);
                    return NULL;
                case PartialType_struct:
                    structarg = va_arg(*args, struct PartialSDef *);
                    partial_fdef_nextarg_struct(self, structarg);
                    break;
                default:
                    partial_fdef_nextarg(self, type);
                    break;
            }
        }
    }
    return self;
}

static struct PartialFDef * function_def_empty_base(size_t nargs){
    struct PartialFDef * self;
    enum PartialType * args;
    struct PartialSDef ** struct_args;
    ffi_type ** ffi_argtypes;

    self = malloc(sizeof(struct PartialFDef));
    if (self == NULL){
        return NULL;
    }
    if (nargs != 0){
        args = malloc(nargs * sizeof(enum PartialType));
        if (args == NULL){
            free(self);
            return NULL;
        }
        struct_args = malloc(nargs * sizeof(struct PartialSDef *));
        if (struct_args == NULL){
            free(args);
            free(self);
            return NULL;
        }
        ffi_argtypes = malloc(nargs * sizeof(ffi_type *));
        if (ffi_argtypes == NULL){
            free(struct_args);
            free(args);
            free(self);
            return NULL;
        }
        self->args = args;
        self->struct_args = struct_args;
        self->ffi_argtypes = ffi_argtypes;
    }
    self->name = DEFAULT_FUNCTION_NAME;
    self->nargs = nargs;
    self->iargs = 0;
    self->ffi_setup = 0;
    return self;
}

struct PartialFDef * partial_fdef_empty(
        enum PartialType returns, size_t nargs){
    struct PartialFDef * self;

    CHECK_SIZE_T;

    if ((self = function_def_empty_base(nargs)) != NULL){
        self->returns = returns;
        self->ffi_rtype = FFI_TYPES[returns];
    }
    return self;
}

struct PartialFDef * partial_fdef_empty_struct(
        struct PartialSDef * returns, size_t nargs){
    struct PartialFDef * self;

    if ((self = function_def_empty_base(nargs)) != NULL){
        self->returns = PartialType_struct;
        self->returns_struct = returns;
        self->ffi_rtype = &(returns->ffi_type_);
    }
    return self;
}

struct PartialFDef * partial_fdef_fromstr(char * format){
    struct PartialFDef * self;
    char c;
    char * fmt;
    size_t nargs = 0;
    enum PartialType type;

    c = *format++;
    type = char2type(c);
    if (type == PartialType_struct) {
        return NULL;
    }
    fmt = format;
    while ((c = *fmt++)){
        if (!isspace(c)){
            nargs += 1;
        }
    }
    self = partial_fdef_empty(type, nargs);
    if (self != NULL){
        fmt = format;
        while ((c = *fmt++)){
            if (isspace(c)){
                continue;
            }
            type = char2type(c);
            switch (type) {
                case PartialType_struct:
                case PartialType_void:
                    partial_fdef_free(self);
                    return NULL;
                default:
                    partial_fdef_nextarg(self, type);
                    break;
            }
        }
    }
    return self;
}

void partial_fdef_free(struct PartialFDef * self){
    if (self->nargs != 0){
        free(self->ffi_argtypes);
        free(self->struct_args);
        free(self->args);
    }
    free(self);
}

void partial_fdef_nextarg(
        struct PartialFDef * self, enum PartialType type){
    CHECK_SIZE_T;
    self->args[self->iargs] = type;
    self->ffi_argtypes[self->iargs] = FFI_TYPES[type];
    self->iargs += 1;
}

void partial_fdef_nextarg_struct(
        struct PartialFDef * self, struct PartialSDef * type){
    self->args[self->iargs] = PartialType_struct;
    self->struct_args[self->iargs] = type;
    self->ffi_argtypes[self->iargs] = &(type->ffi_type_);
    self->iargs += 1;
}

int partial_fdef_equal(
        struct PartialFDef * a, struct PartialFDef * b){
    size_t i;

    if (a->returns != b->returns || a->nargs != b->nargs){
        return 0;
    }
    if (a->returns == PartialType_struct
            && !partial_sdef_equal(
                    a->returns_struct, b->returns_struct)){
        return 0;
    }
    for (i = 0; i < a->nargs; ++i){
        if (!types_compatible(a->args[i], b->args[i])){
            return 0;
        }
        if (a->args[i] == PartialType_struct){
            if (!partial_sdef_equal(
                    a->struct_args[i], b->struct_args[i])){
                return 0;
            }
        }
    }
    return 1;
}

void partial_fdef_setname(
        struct PartialFDef * self, char * name){
    self->name = name;
}

void partial_fdef_print(struct PartialFDef * self){
    // TODO: maybe make this not a pass-through?
    partial_fdef_fprint(stdout, self);
}

void partial_fdef_fprint(
        FILE * stream, struct PartialFDef * self){
    size_t i;
    enum PartialType type;

    if (self->returns == PartialType_struct){
        printf("struct %s %s(", self->returns_struct->name, self->name);
    } else {
        printf("%s %s(", PRIMITIVE_NAMES[self->returns], self->name);
    }
    i = 0;
    if (i < self->nargs){
        type = self->args[i];
        if (type == PartialType_struct){
            fprintf(stream, "struct %s", self->struct_args[i]->name);
        } else {
            fprintf(stream, "%s", PRIMITIVE_NAMES[type]);
        }
    }
    for (i += 1; i < self->nargs; ++i){
        type = self->args[i];
        if (type == PartialType_struct){
            fprintf(stream, ", struct %s", self->struct_args[i]->name);
        } else {
            fprintf(stream, ", %s", PRIMITIVE_NAMES[type]);
        }
    }
    fputs(");\n", stream);
}

void partial_fdef_snprint(
        char * buffer, size_t n, struct PartialFDef * self){
    // TODO
}

void partial_fdef_print_indent(struct PartialFDef * self){
    // TODO: maybe make this not a pass-through?
    partial_fdef_fprint_indent(stdout, self);
}

void partial_fdef_fprint_indent(
        FILE * stream, struct PartialFDef * self){
    size_t i;
    enum PartialType type;

    if (self->returns == PartialType_struct){
        printf("struct %s %s(", self->returns_struct->name, self->name);
    } else {
        printf("%s %s(", PRIMITIVE_NAMES[self->returns], self->name);
    }
    i = 0;
    if (i < self->nargs){
        type = self->args[i];
        if (type == PartialType_struct){
            fprintf(stream, "\n%sstruct %s",
                    INDENT, self->struct_args[i]->name);
        } else {
            fprintf(stream, "\n%s%s", INDENT, PRIMITIVE_NAMES[type]);
        }
    }
    for (i += 1; i < self->nargs; ++i){
        type = self->args[i];
        if (type == PartialType_struct){
            fprintf(stream, ",\n%sstruct %s",
                    INDENT, self->struct_args[i]->name);
        } else {
            fprintf(stream, ",\n%s%s", INDENT, PRIMITIVE_NAMES[type]);
        }
    }
    fputs(");\n", stream);
}

void partial_fdef_snprint_indent(
        char * buffer, size_t n, struct PartialFDef * self){
    // TODO
}

static int function_def_prep_cif(struct PartialFDef * self){
    ffi_status status;

    // TODO: ABI??
    status = ffi_prep_cif(
            &(self->ffi_cif_),
            FFI_DEFAULT_ABI,
            self->nargs,
            self->ffi_rtype,
            self->ffi_argtypes);
    if (status == FFI_OK){
        self->ffi_setup = 1;
        return 0;
    } else {
        return -1;
    }
}

/**************************************
 * Partial operations
 **************************************/

struct Partial * partial(
        struct PartialFDef * targetdef, void * target){
    struct Partial * self;
    size_t nargs;
    uint8_t * argsset;

    self = malloc(sizeof(struct Partial));
    if (self == NULL){
        return NULL;
    }
    self->targetdef = targetdef;
    self->target = target;
    self->nargs = nargs = targetdef->nargs;
    if (nargs != 0){
        self->args = malloc(nargs * sizeof(union PartialArg));
        if (self->args == NULL){
            free(self);
            return NULL;
        }
        self->ffi_args = malloc(nargs * sizeof(void *));
        if (self->ffi_args == NULL){
            free(self->args);
            free(self);
            return NULL;
        }
    }
    if (nargs > 64){
        argsset = calloc(((nargs >> 3) + 1), sizeof(uint8_t));
        if (argsset == NULL){
            free(self->ffi_args);
            free(self->args);
            free(self);
            return NULL;
        }
        self->argsset.d = argsset;
    } else {
        self->argsset.s = 0;
    }
    self->extdef = NULL;
    self->ffi_writable = NULL;
    return self;
}

void partial_free(struct Partial * self){
    if (self->nargs > 64){
        free(self->argsset.d);
    }
    if (self->extdef != NULL){
        partial_fdef_free(self->extdef);
    }
    if (self->ffi_writable != NULL){
        ffi_closure_free(self->ffi_writable);
    }
    if (self->nargs != 0){
        free(self->args);
        free(self->ffi_args);
    }
    free(self);
}

void partial_set_args(struct Partial * self, size_t start, size_t nargs, ...){
    va_list args;

    va_start(args, nargs);
    partial_set_args_va(self, start, nargs, &args);
    va_end(args);
}

#define sav_case(ct, pt) case PartialType_##pt:\
        value.a_##pt = va_arg(*args, ct);\
        break;

void partial_set_args_va(
        struct Partial * self, size_t start, size_t nargs, va_list * args){
    union PartialArg value;
    size_t i;

    for (i = start; i < start + nargs; ++i){
        switch (self->targetdef->args[i]){
            sav_case(int, uint8);
            sav_case(int, sint8);
            sav_case(int, uint16);
            sav_case(int, sint16);
            sav_case(uint32_t, uint32);
            sav_case(int32_t, sint32);
            sav_case(uint64_t, uint64);
            sav_case(int64_t, sint64);
            sav_case(double, float);
            sav_case(double, double);
            sav_case(int, uchar);
            sav_case(int, schar);
            sav_case(int, ushort);
            sav_case(int, sshort);
            sav_case(unsigned int, uint);
            sav_case(signed int, sint);
            sav_case(unsigned long, ulong);
            sav_case(signed long, slong);
            sav_case(void *, pointer);
            sav_case(size_t, size_t);
            default:
                // TODO: idk
                break;
        }
        PARTIALSET_SET(self, i);
        self->args[i] = value;
        self->ffi_args[i] = &(self->args[i]);
    }
}

void partial_set_arg(
        struct Partial * self, size_t arg, union PartialArg value){
    PARTIALSET_SET(self, arg);
    self->args[arg] = value;
    self->ffi_args[arg] = &(self->args[arg]);
}

struct PartialFDef * partial_getdef(struct Partial * self){
    struct PartialFDef * def;
    size_t i;
    size_t nargs;
    enum PartialType type;

    if (self->extdef != NULL){
        return self->extdef;
    }

    nargs = 0;
    for (i = 0; i < self->nargs; ++i){
        if (!PARTIALSET_IN(self, i)){
            nargs += 1;
        }
    }
    def = partial_fdef_empty(self->targetdef->returns, nargs);
    if (def == NULL){
        return NULL;
    }
    for (i = 0; i < self->nargs; ++i){
        if (!PARTIALSET_IN(self, i)) {
            type = self->targetdef->args[i];
            if (type == PartialType_struct){
                partial_fdef_nextarg_struct(
                        def, self->targetdef->struct_args[i]);
            } else {
                partial_fdef_nextarg(def, type);
            }
        }
    }
    self->extdef = def;
    return def;
}

static void closure_cb(ffi_cif * cif, void * ret, void ** args, void * ud){
    struct Partial * self;
    size_t i, argsi = 0;

    self = ud;
    for (i = 0; i < self->nargs; ++i){
        if (!PARTIALSET_IN(self, i)){
            self->ffi_args[i] = args[argsi++];
        }
    }
    ffi_call(
            &(self->targetdef->ffi_cif_),
            self->target,
            ret,
            self->ffi_args);
}

void * partial_compile(struct Partial * self){
    struct PartialFDef * extdef;
    void * writable, * code;

    if (!(self->targetdef->ffi_setup
            || !function_def_prep_cif(self->targetdef))){
        return NULL;
    }
    extdef = partial_getdef(self);
    if (extdef == NULL
            || !(extdef->ffi_setup || !function_def_prep_cif(extdef))){
        return NULL;
    }
    // TODO need more size?
    writable = ffi_closure_alloc(sizeof(ffi_closure), &code);
    if (writable == NULL){
        return NULL;
    }
    self->ffi_writable = writable;
    self->ffi_exc = code;
    ffi_prep_closure_loc(
            writable, &(extdef->ffi_cif_), &closure_cb, self, code);
    return code;
}

int partial_quick(
        union PartialArg * returnval,
        char * passfmt, void * pass,
        char * funcfmt, void * func,
        size_t nargs, ...){
    va_list args;
    int res;

    va_start(args, nargs);
    res = partial_quick_va(
            returnval, passfmt, pass, funcfmt, func, nargs, &args);
    va_end(args);
    return res;
}

int partial_quick_va(
        union PartialArg * returnval,
        char * passfmt, void * pass,
        char * funcfmt, void * func,
        size_t nargs, va_list * args){
    struct PartialFDef * passdef = NULL;
    struct PartialFDef * funcdef = NULL;
    struct Partial * passp = NULL;
    struct Partial * funcp = NULL;
    size_t passargifunc, passnargs;
    int paif_set;
    char * fmt, c;
    enum PartialType type;
    union PartialArg value;
    int res;
    void * func_exc;
    void * pass_exc;
    struct PartialFDef * pass_exc_def;


    funcdef = partial_fdef_fromstr(funcfmt);
    if (funcdef == NULL){
        goto error;
    }
    c = *passfmt++;
    type = char2type(c);
    if (type == PartialType_struct) {
        goto error;
    }
    fmt = passfmt;
    passnargs = 0;
    while ((c = *fmt++)){
        if (!isspace(c)){
            passnargs += 1;
        }
    }
    passdef = partial_fdef_empty(type, passnargs);
    if (passdef == NULL){
        goto error;
    }
    fmt = passfmt;
    passargifunc = 0;
    paif_set = 0;
    while ((c = *fmt++)){
        if (isspace(c)){
            continue;
        }
        if (c == '*'){
            paif_set = 1;
            c = 'p';
        }
        if (!paif_set){
            passargifunc += 1;
        }
        type = char2type(c);
        switch (type) {
            case PartialType_struct:
            case PartialType_void:
                goto error;
            default:
                partial_fdef_nextarg(passdef, type);
                break;
        }
    }
    if (passargifunc == passnargs){
        goto error;
    }
    funcp = partial(funcdef, func);
    if (funcp == NULL){
        goto error;
    }
    passp = partial(passdef, pass);
    if (passp == NULL){
        goto error;
    }

    if (passargifunc != 0){
        partial_set_args_va(passp, 0, passargifunc, args);
    }
    passnargs -= (passargifunc + 1);
    if (passnargs != 0){
        partial_set_args_va(passp, passargifunc + 1, passnargs, args);
    }
    partial_set_args_va(funcp, 0, nargs, args);
    func_exc = partial_compile(funcp);
    if (func_exc == NULL){
        goto error;
    }
    value.a_pointer = func_exc;
    partial_set_arg(passp, passargifunc, value);
    pass_exc = partial_compile(passp);
    if (pass_exc == NULL){
        goto error;
    }
    // don't check for an error here, would have already failed
    pass_exc_def = partial_getdef(passp);
    if (pass_exc_def->nargs != 0){  // assert
        goto error;
    }
    if (!(pass_exc_def->ffi_setup || !function_def_prep_cif(pass_exc_def))){
        goto error;
    }
    ffi_call(&(pass_exc_def->ffi_cif_), pass_exc, returnval, NULL);
    res = 0;
    goto cleanup;

error:
    res = -1;
cleanup:
    if (passdef != NULL){
        partial_fdef_free(passdef);
    }
    if (funcdef != NULL){
        partial_fdef_free(funcdef);
    }
    if (passp != NULL){
        partial_free(passp);
    }
    if (funcp != NULL){
        partial_free(funcp);
    }
    return res;
}

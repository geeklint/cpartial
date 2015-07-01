/*
Copyright (c) 2015, Sky Leonard
All rights reserved.
For more info see COPYING or http://opensource.org/licenses/BSD-3-Clause
 */

#ifndef PARTIAL_H_
#define PARTIAL_H_

#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>

/**************************************
 * Types
 **************************************/

/*
 * Partial Type.
 */
struct Partial;

/*
 * Structure definition type.
 */
struct PartialSDef;

/*
 * Function definition type.
 */
struct PartialFDef;

/*
 * Enum of potential return/argument/member types.
 */
enum PartialType {
	PartialType_void,	// 'v'
	PartialType_uint8,	// 'B'
	PartialType_sint8,	// 'b'
	PartialType_uint16,	// 'H'
	PartialType_sint16,	// 'h'
	PartialType_uint32,	// 'J'
	PartialType_sint32,	// 'j'
	PartialType_uint64,	// 'Q'
	PartialType_sint64,	// 'q'
	PartialType_float,	// 'f'
	PartialType_double,	// 'd'
	PartialType_uchar,	// 'C'
	PartialType_schar,	// 'c'
	PartialType_ushort,	// 'S'
	PartialType_sshort,	// 's'
	PartialType_uint,	// 'I'
	PartialType_sint,	// 'i'
	PartialType_ulong,	// 'L'
	PartialType_slong,	// 'l'
	PartialType_pointer,  // 'p'
	PartialType_size_t, // 'T'
	PartialType_struct,
};

/*
 * Union of argument types.
 */
union PartialArg {
	uint8_t a_uint8;
	int8_t a_sint8;
	uint16_t a_uint16;
	int16_t a_sint16;
	uint32_t a_uint32;
	int32_t a_sint32;
	uint64_t a_uint64;
	int64_t a_sint64;
	float a_float;
	double a_double;
	unsigned char a_uchar;
	signed char a_schar;
	unsigned short a_ushort;
	signed short a_sshort;
	unsigned int a_uint;
	signed int a_sint;
	unsigned long a_ulong;
	signed long a_slong;
	void * a_pointer;
	size_t a_size_t;
};

/**************************************
 * Structure definition operations
 **************************************/

/*
 * Constructors.
 */
struct PartialSDef * partial_sdef(size_t nmemb, ...);
struct PartialSDef * partial_sdef_va(
		size_t nmemb, va_list * members);
struct PartialSDef * partial_sdef_empty(size_t nmemb);
struct PartialSDef * partial_sdef_fromstr(char * format);

/*
 * Freer.
 */
void partial_sdef_free(struct PartialSDef *);

/*
 * On a definition created with _empty, set the next member.
 */
void partial_sdef_nextmemb(
		struct PartialSDef *, enum PartialType type);
void partial_sdef_nextmemb_struct(
		struct PartialSDef *, struct PartialSDef * type);

/*
 * Return 1 if the definitions are compatible, 0 if not.
 */
int partial_sdef_equal(
		struct PartialSDef * a, struct PartialSDef * b);

/*
 * Used for print operations. Defaults to "Unknown".
 */
void partial_sdef_setname(struct PartialSDef *, char * name);

/*
 * Print operations (probably for debugging)
 */
void partial_sdef_print(struct PartialSDef *);
void partial_sdef_fprint(FILE * stream, struct PartialSDef *);
void partial_sdef_snprint(
		char * buffer, size_t n, struct PartialSDef *);

/**************************************
 * Function definition operations
 **************************************/

/*
 * Constructor
 */
struct PartialFDef * partial_function_def(
		enum PartialType returns, size_t nargs, ...);
struct PartialFDef * partial_fdef_va(
		enum PartialType returns, size_t nargs, va_list * args);
struct PartialFDef * partial_fdef_empty(
		enum PartialType returns, size_t nargs);
struct PartialFDef * partial_fdef_empty_struct(
		struct PartialSDef * returns, size_t nargs);
struct PartialFDef * partial_fdef_fromstr(char * format);

/*
 * Freer
 */
void partial_fdef_free(struct PartialFDef *);

/*
 * On a definition created with _empty, set the next argument.
 */
void partial_fdef_nextarg(
		struct PartialFDef *, enum PartialType type);
void partial_fdef_nextarg_struct(
		struct PartialFDef *, struct PartialSDef * type);

/*
 * Return 1 if the definitions are compatible, 0 if not.
 */
int partial_fdef_equal(
		struct PartialFDef * a, struct PartialFDef * b);

/*
 * Used for print operations. Defaults to "function".
 */
void partial_fdef_setname(struct PartialFDef *, char * name);

/*
 * Print operations (probably for debugging)
 */
void partial_fdef_print(struct PartialFDef *);
void partial_fdef_fprint(FILE * stream, struct PartialFDef *);
void partial_fdef_snprint(
		char * buffer, size_t n, struct PartialFDef *);
void partial_fdef_print_indent(struct PartialFDef *);
void partial_fdef_fprint_indent(
		FILE * stream, struct PartialFDef *);
void partial_fdef_snprint_indent(
		char * buffer, size_t n, struct PartialFDef *);

/**************************************
 * Partial operations
 **************************************/

/*
 * Constructor.
 */
struct Partial * partial(struct PartialFDef *, void * function);

/*
 * Freer.
 */
void partial_free(struct Partial *);

/*
 * Set the values for many arguments at once.
 */
void partial_set_args(struct Partial *, size_t start, size_t nargs, ...);
void partial_set_args_va(
		struct Partial *, size_t start, size_t nargs, va_list * args);

/*
 * Set the value for a particular argument.
 */
void partial_set_arg(struct Partial *, size_t arg, union PartialArg value);

/*
 * Get the function definition with preset arguments removed
 */
struct PartialFDef * partial_getdef(struct Partial *);

/*
 * Compile the partial object and return a function pointer.
 */
void * partial_compile(struct Partial *);

/*
 * All-in-one operation. Returns 0 on success, -1 on failure.
 */
int partial_quick(
		union PartialArg * returnval,
		char * passfmt, void * pass,
		char * funcfmt, void * func,
		size_t nargs, ...);
int partial_quick_va(
		union PartialArg * returnval,
		char * passfmt, void * pass,
		char * funcfmt, void * func,
		size_t nargs, va_list * args);

#endif /* PARTIAL_H_ */

/*
 * Machine generated header file.  Do not edit.
 */

#ifndef _INTERNALS_H_
#define _INTERNALS_H_

#define type_EvenFixnum 0
#define type_FunctionPointer 1
#define type_OtherImmediate0 2
#define type_ListPointer 3
#define type_OddFixnum 4
#define type_InstancePointer 5
#define type_OtherImmediate1 6
#define type_OtherPointer 7
#define type_Bignum 10
#define type_Ratio 14
#define type_SingleFloat 18
#define type_DoubleFloat 22
#define type_Complex 26
#define type_ComplexSingleFloat 30
#define type_ComplexDoubleFloat 34
#define type_SimpleArray 38
#define type_SimpleString 42
#define type_SimpleBitVector 46
#define type_SimpleVector 50
#define type_SimpleArrayUnsignedByte2 54
#define type_SimpleArrayUnsignedByte4 58
#define type_SimpleArrayUnsignedByte8 62
#define type_SimpleArrayUnsignedByte16 66
#define type_SimpleArrayUnsignedByte32 70
#define type_SimpleArraySignedByte8 74
#define type_SimpleArraySignedByte16 78
#define type_SimpleArraySignedByte30 82
#define type_SimpleArraySignedByte32 86
#define type_SimpleArraySingleFloat 90
#define type_SimpleArrayDoubleFloat 94
#define type_SimpleArrayComplexSingleFloat 98
#define type_SimpleArrayComplexDoubleFloat 102
#define type_ComplexString 106
#define type_ComplexBitVector 110
#define type_ComplexVector 114
#define type_ComplexArray 118
#define type_CodeHeader 122
#define type_FunctionHeader 126
#define type_ClosureHeader 130
#define type_FuncallableInstanceHeader 134
#define type_ByteCodeFunction 138
#define type_ByteCodeClosure 142
#define type_DylanFunctionHeader 146
#define type_ClosureFunctionHeader 150
#define type_ReturnPcHeader 154
#define type_ValueCellHeader 158
#define type_SymbolHeader 162
#define type_BaseChar 166
#define type_Sap 170
#define type_UnboundMarker 174
#define type_WeakPointer 178
#define type_InstanceHeader 182
#define type_Fdefn 186
#define type_ScavengerHook 190

#define trap_Halt 8
#define trap_PendingInterrupt 9
#define trap_Error 10
#define trap_Cerror 11
#define trap_Breakpoint 12
#define trap_FunctionEndBreakpoint 13
#define trap_SingleStepBreakpoint 14
#define trap_ObjectNotList 16
#define trap_ObjectNotInstance 17

#define subtype_VectorNormal 0
#define subtype_VectorValidHashing 2
#define subtype_VectorMustRehash 3

#define tracetab_Normal 0
#define tracetab_CallSite 1
#define tracetab_FunctionPrologue 2
#define tracetab_FunctionEpilogue 3

#define sc_Constant 0
#define sc_FpConstant 1
#define sc_Immediate 2
#define sc_ControlStack 3
#define sc_SignedStack 4
#define sc_UnsignedStack 5
#define sc_BaseCharStack 6
#define sc_SapStack 7
#define sc_SingleStack 8
#define sc_DoubleStack 9
#define sc_ComplexSingleStack 10
#define sc_ComplexDoubleStack 11
#define sc_IgnoreMe 12
#define sc_AnyReg 13
#define sc_DescriptorReg 14
#define sc_BaseCharReg 15
#define sc_SapReg 16
#define sc_SignedReg 17
#define sc_UnsignedReg 18
#define sc_WordReg 19
#define sc_ByteReg 20
#define sc_SingleReg 21
#define sc_DoubleReg 22
#define sc_ComplexSingleReg 23
#define sc_ComplexDoubleReg 24
#define sc_CatchBlock 25

#define UNKNOWN_ERROR 0
#define OBJECT_NOT_FUNCTION_ERROR 1
#define OBJECT_NOT_LIST_ERROR 2
#define OBJECT_NOT_BIGNUM_ERROR 3
#define OBJECT_NOT_RATIO_ERROR 4
#define OBJECT_NOT_SINGLE_FLOAT_ERROR 5
#define OBJECT_NOT_DOUBLE_FLOAT_ERROR 6
#define OBJECT_NOT_SIMPLE_STRING_ERROR 7
#define OBJECT_NOT_SIMPLE_BIT_VECTOR_ERROR 8
#define OBJECT_NOT_SIMPLE_VECTOR_ERROR 9
#define OBJECT_NOT_FIXNUM_ERROR 10
#define OBJECT_NOT_FUNCTION_OR_SYMBOL_ERROR 11
#define OBJECT_NOT_VECTOR_ERROR 12
#define OBJECT_NOT_STRING_ERROR 13
#define OBJECT_NOT_BIT_VECTOR_ERROR 14
#define OBJECT_NOT_ARRAY_ERROR 15
#define OBJECT_NOT_NUMBER_ERROR 16
#define OBJECT_NOT_RATIONAL_ERROR 17
#define OBJECT_NOT_FLOAT_ERROR 18
#define OBJECT_NOT_REAL_ERROR 19
#define OBJECT_NOT_INTEGER_ERROR 20
#define OBJECT_NOT_CONS_ERROR 21
#define OBJECT_NOT_SYMBOL_ERROR 22
#define UNDEFINED_SYMBOL_ERROR 23
#define OBJECT_NOT_COERCABLE_TO_FUNCTION_ERROR 24
#define INVALID_ARGUMENT_COUNT_ERROR 25
#define BOGUS_ARGUMENT_TO_VALUES_LIST_ERROR 26
#define UNBOUND_SYMBOL_ERROR 27
#define OBJECT_NOT_SAP_ERROR 29
#define INVALID_UNWIND_ERROR 30
#define UNSEEN_THROW_TAG_ERROR 31
#define DIVISION_BY_ZERO_ERROR 32
#define OBJECT_NOT_TYPE_ERROR 33
#define ODD_KEYWORD_ARGUMENTS_ERROR 34
#define UNKNOWN_KEYWORD_ARGUMENT_ERROR 35
#define INVALID_ARRAY_INDEX_ERROR 38
#define WRONG_NUMBER_OF_INDICES_ERROR 39
#define OBJECT_NOT_SIMPLE_ARRAY_ERROR 40
#define OBJECT_NOT_SIGNED_BYTE_32_ERROR 41
#define OBJECT_NOT_UNSIGNED_BYTE_32_ERROR 42
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_2_ERROR 43
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_4_ERROR 44
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_8_ERROR 45
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_16_ERROR 46
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_32_ERROR 47
#define OBJECT_NOT_SIMPLE_ARRAY_SIGNED_BYTE_8_ERROR 48
#define OBJECT_NOT_SIMPLE_ARRAY_SIGNED_BYTE_16_ERROR 49
#define OBJECT_NOT_SIMPLE_ARRAY_SIGNED_BYTE_30_ERROR 50
#define OBJECT_NOT_SIMPLE_ARRAY_SIGNED_BYTE_32_ERROR 51
#define OBJECT_NOT_SIMPLE_ARRAY_SINGLE_FLOAT_ERROR 52
#define OBJECT_NOT_SIMPLE_ARRAY_DOUBLE_FLOAT_ERROR 53
#define OBJECT_NOT_SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_ERROR 54
#define OBJECT_NOT_SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_ERROR 55
#define OBJECT_NOT_COMPLEX_ERROR 56
#define OBJECT_NOT_COMPLEX_RATIONAL_ERROR 57
#define OBJECT_NOT_COMPLEX_FLOAT_ERROR 58
#define OBJECT_NOT_COMPLEX_SINGLE_FLOAT_ERROR 59
#define OBJECT_NOT_COMPLEX_DOUBLE_FLOAT_ERROR 60
#define OBJECT_NOT_WEAK_POINTER_ERROR 61
#define OBJECT_NOT_INSTANCE_ERROR 62
#define OBJECT_NOT_BASE_CHAR_ERROR 63
#define NIL_FUNCTION_RETURNED_ERROR 64
#define LAYOUT_INVALID_ERROR 65

#define ERRORS { \
    "Unknown.  System lossage.", \
    "Object is not of type FUNCTION.", \
    "Object is not of type LIST.", \
    "Object is not of type BIGNUM.", \
    "Object is not of type RATIO.", \
    "Object is not of type SINGLE-FLOAT.", \
    "Object is not of type DOUBLE-FLOAT.", \
    "Object is not of type SIMPLE-STRING.", \
    "Object is not of type SIMPLE-BIT-VECTOR.", \
    "Object is not of type SIMPLE-VECTOR.", \
    "Object is not of type FIXNUM.", \
    "Object is not of type FUNCTION or SYMBOL.", \
    "Object is not of type VECTOR.", \
    "Object is not of type STRING.", \
    "Object is not of type BIT-VECTOR.", \
    "Object is not of type ARRAY.", \
    "Object is not of type NUMBER.", \
    "Object is not of type RATIONAL.", \
    "Object is not of type FLOAT.", \
    "Object is not of type REAL.", \
    "Object is not of type INTEGER.", \
    "Object is not of type CONS.", \
    "Object is not of type SYMBOL.", \
    "Undefined symbol.", \
    "Object is not coercable to type FUNCTION.", \
    "Invalid argument count.", \
    "Bogus argument to VALUES-LIST.", \
    "Unbound symbol.", \
    "unused", \
    "Object is not a System Area Pointer (SAP).", \
    "Attempt to RETURN-FROM a block that no longer exists.", \
    "Attempt to THROW to a non-existent tag.", \
    "Attempt to divide by zero.", \
    "Object is of the wrong type.", \
    "Odd number of keyword arguments.", \
    "Unknown keyword.", \
    "unused", \
    "unused", \
    "Invalid array index.", \
    "Wrong number of indices.", \
    "Object is not of type SIMPLE-ARRAY.", \
    "Object is not of type (SIGNED-BYTE 32).", \
    "Object is not of type (UNSIGNED-BYTE 32).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 2) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 4) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 8) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 16) (*)).", \
    "Object is not of type (SIMPLE-ARRAY FIXNUM (*)).", \
    "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)).", \
    "Object is not of type (SIMPLE-ARRAY SINGLE-FLOAT (*)).", \
    "Object is not of type (SIMPLE-ARRAY DOUBLE-FLOAT (*)).", \
    "Object is not of type (SIMPLE-ARRAY (COMPLEX SINGLE-FLOAT) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)).", \
    "Object is not of type COMPLEX.", \
    "Object is not of type (COMPLEX RATIONAL).", \
    "Object is not of type (COMPLEX FLOAT).", \
    "Object is not of type (COMPLEX SINGLE-FLOAT).", \
    "Object is not of type (COMPLEX DOUBLE-FLOAT).", \
    "Object is not a WEAK-POINTER.", \
    "Object is not a INSTANCE.", \
    "Object is not of type BASE-CHAR.", \
    "Function with declared result type NIL returned.", \
    "Layout is invalid (instance obsolete.)", \
    NULL \
}

#ifndef LANGUAGE_ASSEMBLY

#define LISPOBJ(x) ((lispobj)x)

struct array {
    lispobj header;
    lispobj fill_pointer;
    lispobj fill_pointer_p;
    lispobj elements;
    lispobj data;
    lispobj displacement;
    lispobj displaced_p;
    lispobj dimensions[1];
};

struct bignum {
    lispobj header;
    long digits[1];
};

struct binding {
    lispobj value;
    lispobj symbol;
};

struct catch_block {
    struct unwind_block * current_uwp;
    lispobj * current_cont;
    lispobj entry_pc;
    lispobj tag;
    struct catch_block * previous_catch;
    lispobj size;
};

struct closure {
    lispobj header;
    lispobj function;
    lispobj info[1];
};

struct code {
    lispobj header;
    lispobj code_size;
    lispobj entry_points;
    lispobj debug_info;
    lispobj trace_table_offset;
    lispobj constants[1];
};

struct complex {
    lispobj header;
    lispobj real;
    lispobj imag;
};

struct complex_double_float {
    lispobj header;
    lispobj filler;
    double real;
    double imag;
};

struct complex_single_float {
    lispobj header;
    float real;
    float imag;
};

struct cons {
    lispobj car;
    lispobj cdr;
};

struct double_float {
    lispobj header;
    lispobj filler;
    double value;
};

struct fdefn {
    lispobj header;
    lispobj name;
    lispobj function;
    char * raw_addr;
};

struct funcallable_instance {
    lispobj header;
    lispobj function;
    lispobj lexenv;
    lispobj layout;
    lispobj info[1];
};

struct function {
    lispobj header;
    lispobj self;
    lispobj next;
    lispobj name;
    lispobj arglist;
    lispobj type;
    unsigned char code[1];
};

struct instance {
    lispobj header;
    lispobj slots[1];
};

struct ratio {
    lispobj header;
    lispobj numerator;
    lispobj denominator;
};

struct return_pc {
    lispobj header;
    unsigned char return_point[1];
};

struct sap {
    lispobj header;
    char * pointer;
};

struct scavenger_hook {
    lispobj header;
    lispobj value;
    lispobj function;
    struct scavenger_hook * next;
};

struct single_float {
    lispobj header;
    float value;
};

struct symbol {
    lispobj header;
    lispobj value;
    lispobj hash;
    lispobj plist;
    lispobj name;
    lispobj package;
};

struct unwind_block {
    struct unwind_block * current_uwp;
    lispobj * current_cont;
    lispobj entry_pc;
};

struct value_cell {
    lispobj header;
    lispobj value;
};

struct vector {
    lispobj header;
    lispobj length;
    unsigned long data[1];
};

struct weak_pointer {
    lispobj header;
    lispobj value;
    lispobj broken;
    struct weak_pointer * next;
};

#else /* LANGUAGE_ASSEMBLY */

#define LISPOBJ(thing) thing

#define ARRAY_FILL_POINTER_OFFSET -3
#define ARRAY_FILL_POINTER_P_OFFSET 1
#define ARRAY_ELEMENTS_OFFSET 5
#define ARRAY_DATA_OFFSET 9
#define ARRAY_DISPLACEMENT_OFFSET 13
#define ARRAY_DISPLACED_P_OFFSET 17
#define ARRAY_DIMENSIONS_OFFSET 21

#define BIGNUM_DIGITS_OFFSET -3

#define CLOSURE_FUNCTION_OFFSET 3
#define CLOSURE_INFO_OFFSET 7

#define CODE_CODE_SIZE_OFFSET -3
#define CODE_ENTRY_POINTS_OFFSET 1
#define CODE_DEBUG_INFO_OFFSET 5
#define CODE_TRACE_TABLE_OFFSET_OFFSET 9
#define CODE_CONSTANTS_OFFSET 13

#define COMPLEX_REAL_OFFSET -3
#define COMPLEX_IMAG_OFFSET 1

#define COMPLEX_DOUBLE_FLOAT_FILLER_OFFSET -3
#define COMPLEX_DOUBLE_FLOAT_REAL_OFFSET 1
#define COMPLEX_DOUBLE_FLOAT_IMAG_OFFSET 9

#define COMPLEX_SINGLE_FLOAT_REAL_OFFSET -3
#define COMPLEX_SINGLE_FLOAT_IMAG_OFFSET 1

#define CONS_CAR_OFFSET -3
#define CONS_CDR_OFFSET 1

#define DOUBLE_FLOAT_FILLER_OFFSET -3
#define DOUBLE_FLOAT_VALUE_OFFSET 1

#define FDEFN_NAME_OFFSET -3
#define FDEFN_FUNCTION_OFFSET 1
#define FDEFN_RAW_ADDR_OFFSET 5

#define FUNCALLABLE_INSTANCE_FUNCTION_OFFSET 3
#define FUNCALLABLE_INSTANCE_LEXENV_OFFSET 7
#define FUNCALLABLE_INSTANCE_LAYOUT_OFFSET 11
#define FUNCALLABLE_INSTANCE_INFO_OFFSET 15

#define FUNCTION_SELF_OFFSET 3
#define FUNCTION_NEXT_OFFSET 7
#define FUNCTION_NAME_OFFSET 11
#define FUNCTION_ARGLIST_OFFSET 15
#define FUNCTION_TYPE_OFFSET 19
#define FUNCTION_CODE_OFFSET 23

#define INSTANCE_SLOTS_OFFSET -1

#define RATIO_NUMERATOR_OFFSET -3
#define RATIO_DENOMINATOR_OFFSET 1

#define RETURN_PC_RETURN_POINT_OFFSET -3

#define SAP_POINTER_OFFSET -3

#define SCAVENGER_HOOK_VALUE_OFFSET -3
#define SCAVENGER_HOOK_FUNCTION_OFFSET 1
#define SCAVENGER_HOOK_NEXT_OFFSET 5

#define SINGLE_FLOAT_VALUE_OFFSET -3

#define SYMBOL_VALUE_OFFSET -3
#define SYMBOL_HASH_OFFSET 1
#define SYMBOL_PLIST_OFFSET 5
#define SYMBOL_NAME_OFFSET 9
#define SYMBOL_PACKAGE_OFFSET 13

#define VALUE_CELL_VALUE_OFFSET -3

#define VECTOR_LENGTH_OFFSET -3
#define VECTOR_DATA_OFFSET 1

#define WEAK_POINTER_VALUE_OFFSET -3
#define WEAK_POINTER_BROKEN_OFFSET 1
#define WEAK_POINTER_NEXT_OFFSET 5

#endif /* LANGUAGE_ASSEMBLY */

#define NIL LISPOBJ(0x2800000B)
#define T LISPOBJ(0x28000027)
#define LISP_ENVIRONMENT_LIST LISPOBJ(0x2800003F)
#define LISP_COMMAND_LINE_LIST LISPOBJ(0x28000057)
#define BATCH_MODE LISPOBJ(0x2800006F)
#define INITIAL_FDEFN_OBJECTS LISPOBJ(0x28000087)
#define INITIAL_FUNCTION LISPOBJ(0x2800009F)
#define MAYBE_GC LISPOBJ(0x280000B7)
#define INTERNAL_ERROR LISPOBJ(0x280000CF)
#define HANDLE_BREAKPOINT LISPOBJ(0x280000E7)
#define FDEFINITION_OBJECT LISPOBJ(0x280000FF)
#define READ_ONLY_SPACE_FREE_POINTER LISPOBJ(0x28000117)
#define STATIC_SPACE_FREE_POINTER LISPOBJ(0x2800012F)
#define INITIAL_DYNAMIC_SPACE_FREE_POINTER LISPOBJ(0x28000147)
#define CURRENT_CATCH_BLOCK LISPOBJ(0x2800015F)
#define CURRENT_UNWIND_PROTECT_BLOCK LISPOBJ(0x28000177)
#define EVAL_STACK_TOP LISPOBJ(0x2800018F)
#define ALIEN_STACK LISPOBJ(0x280001A7)
#define PSEUDO_ATOMIC_ATOMIC LISPOBJ(0x280001BF)
#define PSEUDO_ATOMIC_INTERRUPTED LISPOBJ(0x280001D7)
#define INTERRUPTS_ENABLED LISPOBJ(0x280001EF)
#define INTERRUPT_PENDING LISPOBJ(0x28000207)
#define FREE_INTERRUPT_CONTEXT_INDEX LISPOBJ(0x2800021F)
#define ALLOCATION_POINTER LISPOBJ(0x28000237)
#define BINDING_STACK_POINTER LISPOBJ(0x2800024F)
#define INTERNAL_GC_TRIGGER LISPOBJ(0x28000267)
#define FP_CONSTANT_0D0 LISPOBJ(0x2800027F)
#define FP_CONSTANT_1D0 LISPOBJ(0x28000297)
#define FP_CONSTANT_0S0 LISPOBJ(0x280002AF)
#define FP_CONSTANT_1S0 LISPOBJ(0x280002C7)
#define FP_CONSTANT_0L0 LISPOBJ(0x280002DF)
#define FP_CONSTANT_1L0 LISPOBJ(0x280002F7)
#define FP_CONSTANT_PI LISPOBJ(0x2800030F)
#define FP_CONSTANT_L2T LISPOBJ(0x28000327)
#define FP_CONSTANT_L2E LISPOBJ(0x2800033F)
#define FP_CONSTANT_LG2 LISPOBJ(0x28000357)
#define FP_CONSTANT_LN2 LISPOBJ(0x2800036F)
#define SCAVENGE_READ_ONLY_SPACE LISPOBJ(0x28000387)
#define CONTROL_STACKS LISPOBJ(0x2800039F)
#define SLOT_UNBOUND LISPOBJ(0x280003B7)
#define SPARE_10 LISPOBJ(0x280003CF)
#define SPARE_9 LISPOBJ(0x280003E7)
#define SPARE_8 LISPOBJ(0x280003FF)
#define SPARE_7 LISPOBJ(0x28000417)
#define SPARE_6 LISPOBJ(0x2800042F)
#define SPARE_5 LISPOBJ(0x28000447)
#define SPARE_4 LISPOBJ(0x2800045F)
#define SPARE_3 LISPOBJ(0x28000477)
#define SPARE_2 LISPOBJ(0x2800048F)
#define SPARE_1 LISPOBJ(0x280004A7)
#define X86_CGC_ACTIVE_P LISPOBJ(0x280004BF)
#define STATIC_BLUE_BAG LISPOBJ(0x280004D7)

#endif

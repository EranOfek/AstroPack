//
// mex_bitsetFlag32.c - Int32 implementation of Array(Flag) = bitset(Array(Flag), Bit, Value).
//
// Autohr: Chen Tishler, 04/2023
//=========================================================================

// __Type - Alias for the C data type that we compile for.
// In case of integers, use 'unsigned'.

typedef unsigned long long __Type;

// MEX_TYPE - Alias for the MATLAB data type that should match __Type
// For integers, set MEX_TYPE and MEX_UTYPE according to signed/unsigned
// MATLAB type. For single/double, set these values to the same
// value, the optimizer will ommit the duplicate code.
#define MEX_TYPE  mxINT64_CLASS
#define MEX_UTYPE mxUINT64_CLASS

// Include here the actual implementation that uses __Type, MEX_TYPE, UMEX_TYPE
#include "mex_bitsetFlag_include.cpp"

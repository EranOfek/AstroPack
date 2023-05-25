//
// mex_times8.cpp
//
// Autohr: Chen Tishler, 05/2023
//=========================================================================

// __Type - Alias for the C data type that we compile for.
// In case of integers, use 'unsigned'.

typedef int __Type;

// MEX_TYPE - Alias for the MATLAB data type that should match __Type
// For integers, set MEX_TYPE and MEX_UTYPE according to signed/unsigned
// MATLAB type. For single/double, set these values to the same
// value, the optimizer will ommit the duplicate code.
#define MEX_TYPE  mxINT32_CLASS
#define MEX_UTYPE mxUINT32_CLASS

// Include here the actual implementation that uses __Type, MEX_TYPE, UMEX_TYPE
#include "mex_times_include.cpp"

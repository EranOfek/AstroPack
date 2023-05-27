//
// mex_timesDouble_avx2.cpp
//
// Autohr: Chen Tishler, 05/2023
//
// Compile on Windows with Visual Studio:
//
//      mex mex_timesDouble_avx2.cpp COMPFLAGS="$COMPFLAGS /openmp /arch:AVX2"
//
// Compile on Linux:
//
//      mex -v CXXFLAGS='$CXXFLAGS -fopenmp -mavx2' LDFLAGS='$LDFLAGS -fopenmp' CXXOPTIMFLAGS='-O3 -DNDEBUG' mex_timesDouble_avx2.cpp
//=========================================================================

// __Type - Alias for the C data type that we compile for.
// In case of integers, use 'unsigned'.

typedef double __Type;

// MEX_TYPE - Alias for the MATLAB data type that should match __Type
// For integers, set MEX_TYPE and MEX_UTYPE according to signed/unsigned
// MATLAB type. For single/double, set these values to the same
// value, the optimizer will ommit the duplicate code.
#define MEX_TYPE  mxDOUBLE_CLASS
#define MEX_UTYPE mxDOUBLE_CLASS

// Include here the actual implementation that uses __Type, MEX_TYPE, UMEX_TYPE
#include "mex_times_include_avx2.cpp"

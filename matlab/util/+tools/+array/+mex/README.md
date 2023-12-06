# MEX Optimizations

## countVal

The MEX optimizations includes these files:

- countVal.m - MATLAB gateway function that calls the different MEX versions,
according to data type, etc.

In order to allow maximum optimizations for varios data types,
we compile separate MEX function for each type.


- mex_countVal_include.c - MEX implementation that supports custom data types
- mex_countVal16.c - MEX function for Int16 type
- mex_countVal32.c - MEX function for Int32 type
- mex_countValSingle.c - MEX function for Single type
- mex_countValDouble.c - MEX function for Double type

### mex_countVal32.c

    typedef unsigned int __Type;
    #define MEX_TYPE  mxINT32_CLASS
    #define MEX_UTYPE mxUINT32_CLASS

    #include "mex_countVal_include.c"


### mex_countVal32Double.c

    typedef double __Type;
    #define MEX_TYPE  mxDOUBLE_CLASS
    #define MEX_UTYPE mxDOUBLE_CLASS

    #include "mex_countVal_include.c"


unitTest.m


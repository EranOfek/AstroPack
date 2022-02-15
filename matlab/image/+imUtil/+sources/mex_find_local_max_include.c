// See: edit([matlabroot '/extern/examples/mex/explore.c']);
// See: edit([matlabroot '/extern/examples/refbook/matrixDivide.c']);
//
// Access element in 2D array:
//    val = input[ col*rows + row ];
//
// Access element in 3D array:
//    val = input[ col*rows + row + dep*cols*rows ];
//

#include <stdio.h>
#include "mex.h"
#include "matrix.h"

// For Ctrl-C detection http://www.caam.rice.edu/~wy1/links/mex_ctrl_c_trick/
#if defined (_WIN32)
#include <windows.h>
#elif defined (__linux__)
#include <unistd.h>
#endif

#ifdef __cplusplus
extern "C" bool utIsInterruptPending();
#else
extern bool utIsInterruptPending();
#endif

//-------------------------------------------------------------------------
// The gateway function
//
// mexFunction is not a routine you call. Rather, mexFunction is the name
// of the gateway function in C which every MEX function requires.
// When you invoke a MEX function, MATLAB finds and loads the corresponding
// MEX function of the same name. MATLAB then searches for a symbol named
// mexFunction within the MEX function. If it finds one, it calls the MEX
// function using the address of the mexFunction symbol. MATLAB displays
// an error message if it cannot find a routine named mexFunction inside
// the MEX function.
//
// Input Arguments
//
//    int nlhs              - Number of output arguments
//    mxArray* plhs[]       - Output arguments
//    int nrhs              - Number of input arguments
//    const mxArray* prhs[] - Input arguments
//

void mexFunction(
    int nlhs, mxArray *plhs[],          // Output arguments
    int nrhs, const mxArray *prhs[])    // Input arguments
{
    // Input
    __REAL* input;
    // not a pointer: ok ?
    __REAL val;
    __INT* outListInd,* outII,* outJJ, *outBW;
    int dim = 1;
    mwSize rows, cols;
    mwSize row, col;
    __REAL val1, val2, val3, val4;
    int Conn;
    __REAL Thresh;
    mxClassID     class_id;
    const char*   input_type;
    mwSize        input_ndims;
    const mwSize* input_size;
    __REAL AllocateFrac;

    // Output
    mwSize output_ndims =1;
    mwSize output_size[2] = {0,0};

    // Need at least one argument
    if (nrhs < 1) {
        return;
    }

    // Get dim
    if ((nrhs > 1) && mxIsScalar(prhs[1])) {
        Thresh = (__REAL)mxGetScalar(prhs[1]);
    }
    else {Thresh = 200.0;}


    if ((nrhs > 2) && mxIsScalar(prhs[2])) {
        Conn = (int)mxGetScalar(prhs[2]);
    }
    else {Conn = 4;}

    if ((nrhs > 3) && mxIsScalar(prhs[3])) {
        AllocateFrac = (__REAL)mxGetScalar(prhs[3]);
    }
    else {AllocateFrac = .1;}




    // Parse input matrix
    class_id    = mxGetClassID(prhs[0]);
    input       = (__REAL*)mxGetData(prhs[0]);
    input_ndims = mxGetNumberOfDimensions(prhs[0]);
    input_size  = mxGetDimensions(prhs[0]);
    input_type  = mxGetClassName(prhs[0]);

    //
    if (class_id != MEX_REAL)  {
        mexPrintf("Input matrix integer type does not match this function\n");
        return;
    }

    if (mxIsSparse(prhs[0])) {
        mexPrintf("Sparse array is not supported yet\n");
        return;
    }

    //mexPrintf("input_ndims: %d\n", input_ndims);
    //mexPrintf("input_size: %d, %d, %d\n", input_size[0], input_size[1], input_size[2]);
    //mexPrintf("input_type: %s\n", input_type);
    //mexPrintf("dim: %d\n", dim);

    // Validate input params
    if (input_ndims != 2)  {
        mexPrintf("Invalid ndims of input matrix, supported ndims is 2 or 3: %d\n", input_ndims);
        return;
    }

    //-------------------------------------------- Input 2D


    rows = input_size[0];
    cols = input_size[1];


    mwSize  SizeListInd[1] = {(mwSize)((__REAL)rows*(__REAL)cols*AllocateFrac)};
    plhs[0] = mxCreateNumericArray(output_ndims, SizeListInd, class_id, MEX_REAL);
    outListInd = (__INT*)mxGetData(plhs[0]);
    // Uri...


    
    // if (nlhs > 1) {
    plhs[1] = mxCreateNumericArray(output_ndims, SizeListInd, class_id, MEX_REAL);
    outII = (__INT*)mxGetData(plhs[1]);
    //}
    //if (nlhs > 2) {
    plhs[2] = mxCreateNumericArray(output_ndims, SizeListInd, class_id, MEX_REAL);
    outJJ = (__INT*)mxGetData(plhs[2]);
    //}
    //if (nlhs > 3) {

    output_ndims = 2;
    plhs[3] = mxCreateNumericArray(output_ndims, input_size, class_id, MEX_REAL);
    outBW = (__INT*)mxGetData(plhs[3]);
    //}
    //
    int count = 0;
    if (Conn==4) {
        for (col = 1;  col < cols - 1;  col++) {

            for (row = 1;  row < rows - 1;  row++) {
                val = input[ col*rows + row ];
                if (val>Thresh) {
                    val1 = input[ (col-1)*rows + row ];
                    val2 = input[ (col+1)*rows + row ];
                    val3 = input[ col*rows + row-1 ];
                    val4 = input[ col*rows + row+1 ];
                    if (val>val1 && val>val2 && val>val3 && val>val4) {
                        mwSize Indd = (__INT)col*(__INT)rows + (__INT)row;
                        
                        outListInd[count] = (__INT)Indd;
                        
                        if (nlhs > 2) {
                            outII[count] = (__INT)Indd%(__INT)rows+ 1;
                            outJJ[count] = (__INT)ceil((float)Indd/(float)rows);
                        }
                        if (nlhs > 3) {
                            for (int i =0;i<count;i++) {
                                outBW[outII[count], outJJ[count]] = 1;
                            }
                        }
                        count = count + 1;
                    }
                }
            }
        }
    }


    // 2D, dim=2: For each row, sum all column

}

//mexPrintf("output_ndims: %d\n", output_ndims);
//mexPrintf("output_size:  %d, %d, %d\n", output_size[0], output_size[1], output_size[2]);


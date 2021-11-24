// See: edit([matlabroot '/extern/examples/mex/explore.c']);
// See: edit([matlabroot '/extern/examples/refbook/matrixDivide.c']);

#include <stdio.h>
#include "mex.h"
#include "matrix.h"
#define _WIN32

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


#define FUNCNAME "MATLAB:mex_bitor_array:error"
    
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
        
        
// 
// mxIsClass(pm, "double");
// 
// is equivalent to calling either of these forms:
// 
// mxIsDouble(pm);
// strcmp(mxGetClassName(pm), "double")==0;
//
    
void mexFunction( 
        int nlhs, mxArray *plhs[],          // Output arguments
        int nrhs, const mxArray *prhs[])    // Input arguments
{
    // Input
    unsigned int* input, *inp, val;
    mxClassID  category;
    mwSize inputDimsNum;  
    const mwSize* inputDims;    
    const char *inputType;
    int argDim = 1;
    unsigned int* out;
    int i, j, k;
    int rows, cols;
    int row, col, z;
    
    // Output
    mwSize outputDimsNum;        
    mwSize outputDims[4] = {0,0,0,0};
    
    // Need at least one argument
    if (nrhs < 1) {
        return;
    }

    // Get dim
    if ((nrhs > 1) && mxIsScalar(prhs[1])) {
        argDim = (int)mxGetScalar(prhs[1]);
    }
    
    category = mxGetClassID(prhs[0]);        
    input = (int*)mxGetData(prhs[0]);
    inputDimsNum = mxGetNumberOfDimensions(prhs[0]);
    inputDims = mxGetDimensions(prhs[0]);      
    inputType = mxGetClassName(prhs[0]);
    
    if (mxIsSparse(prhs[0])) {
        mexPrintf("Sparse array is not supported yet\n", inputDimsNum);  
        return;        
    }
    
    //mexPrintf("inMatrixDimsNum: %d\n", inputDimsNum);  
    //mexPrintf("inMatrixDims: %d, %d, %d\n", inputDims[0], inputDims[1], inputDims[2]);
    //mexPrintf("inMatrixType: %s\n", inputType);
    //mexPrintf("Dim: %d\n", argDim);  
        
    // Validate input params
    bool Valid = false;
    if ((inputDimsNum == 1) || (inputDimsNum == 2) || (inputDimsNum == 3)) {
        Valid = true;
    }
    
    if (!Valid) {
        mexPrintf("Invalid dim of input matrix: %d\n", inputDimsNum);  
        return;
    }

    // Set output dims
    if (inputDimsNum == 1) {
        outputDimsNum = 1;
        outputDims[0] = 1;
        outputDims[1] = 1;
    }
    else if (inputDimsNum == 2) {
        outputDimsNum = 2;
        outputDims[0] = 1;
        outputDims[1] = inputDims[1];        
    }
    else if (inputDimsNum == 3) {
        if (argDim == 1) {
            outputDimsNum = 3;
            outputDims[0] = 1;
            outputDims[1] = inputDims[1];
            outputDims[2] = inputDims[2];
        }
        
        else if (argDim == 2) {
            outputDimsNum = 3;            
            outputDims[0] = inputDims[0];
            outputDims[1] = 1;
            outputDims[2] = inputDims[2];        
        }
        else {
            outputDimsNum = 2;                        
            outputDims[0] = inputDims[0];
            outputDims[1] = inputDims[1];            
        }
    }
    //mexPrintf("outputDimsNum: %d\n", outputDimsNum);  
    //mexPrintf("outputDims: %d, %d, %d\n", outputDims[0], outputDims[1], outputDims[2]);
    
    // Create output matrix
    plhs[0] = mxCreateNumericArray(outputDimsNum, outputDims, mxUINT32_CLASS, mxREAL);
    out = (int*)mxGetData(plhs[0]);
    
    // Set output dims
    if (inputDimsNum == 1) {
        out[0] = 0;
        for (row = 0;  row < inputDims[0];  row++) {
            //mexPrintf("input[%02d] = %04X\n", row, input[row]);  
            //out[0] |= input[row];
        }
    }
    else if (inputDimsNum == 2) {
        rows = inputDims[0];
        cols = inputDims[1];

        for (col = 0;  col < cols;  col++) {        
            out[col] = 0;
        }
            
        for (col = 0;  col < cols;  col++) {
            inp = input + col*rows;  //val = input[ col*rows + row ];
            for (row = 0;  row < rows;  row++) {
                val = *inp++;
                //val = input[ col*rows + row ];
                //mexPrintf("input[%02d, %02d] = %d\n", row+1, col+1, val);
                out[col] |= val;
            }            
        }
               
    }
    else if (inputDimsNum == 3) {
        if (argDim == 1) {
   
        }
        
        else if (argDim == 2) {
    
        }
        else {
              
        }
    }    
}

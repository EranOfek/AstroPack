//
// mex_bitsetFlag_include.cpp
//
// Author: Chen Tishler, May 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: int8, int16, int32, int64
//

#include <stdlib.h>
#include <omp.h>
#include "mex.h"
#include "matrix.h"

#ifdef never    
// Function to get dimensions of mxArray as a C-string
char* dimsStr(mwSize ndims, const mwSize *dims)
{
    static char s[256];
    char d[32];
    mwSize i;
    s[0] = '\0';
    for (i = 0; i < ndims; ++i) {
        if (i > 0) {
            strcat(s, " x ");
        }
        sprintf(d, "%d", (int)dims[i]);
        strcat(s, d);
    }
    return s;
}
#endif


void mexFunction( 
        int nlhs, mxArray *plhs[],          // Output arguments
        int nrhs, const mxArray *prhs[])    // Input arguments
{
#ifdef never    
    int* inp;
    int* out;
    bool* flag;
    int numel = 777;
    int mask = 16;
    
    //#pragma omp parallel
    {
        //#pragma omp for
        for (int i = 0; i < numel; ++i) {
            out[i] = inp[i];
            if (flag[i])
                out[i] |= mask;
        }
    }
#endif

            
    char s[256];
    
    mxClassID class_id;
    
    // Check the number of input and output arguments
    if (nrhs != 5) 
        mexErrMsgIdAndTxt("MATLAB:countVal:nargin", "Five input arguments required.");
    
    if (nlhs > 1) 
        mexErrMsgIdAndTxt("MATLAB:countVal:nargout", "Only one output argument allowed.");
    
    // Get and validate array
    const mxArray *arg_array = prhs[0];       
    const mxArray *arg_flag = prhs[1];       
        
    bool validate_args = false;
    if (validate_args) {
        // Array
        class_id = mxGetClassID(arg_array);            
        if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE)) 
            mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Input matrix type does not match this function");  
        if (mxIsSparse(arg_array)) 
            mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Array: Sparse array is not supported yet");  

        // Flag
        class_id = mxGetClassID(arg_flag);            
        if (class_id != mxLOGICAL_CLASS)
            mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Flag: Input matrix type does not match this function");  
        if (mxIsSparse(arg_flag)) 
            mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Flag: Sparse array is not supported yet");      

        // Bit
        if ((mxGetClassID(prhs[2]) != mxINT32_CLASS) || (mxGetNumberOfElements(prhs[2]) != 1))
            mexErrMsgIdAndTxt("MATLAB:bitsetFlag:Bit", "Bit: Wrong type or size");                  

        // Value
        if ((mxGetClassID(prhs[3]) != mxINT32_CLASS) || (mxGetNumberOfElements(prhs[3]) != 1))
            mexErrMsgIdAndTxt("MATLAB:bitsetFlag:Value", "Value: Wrong type or size");                  
        
        // UseMP
        if ((mxGetClassID(prhs[4]) != mxINT32_CLASS) || (mxGetNumberOfElements(prhs[4]) != 1))
            mexErrMsgIdAndTxt("MATLAB:bitsetFlag:UseMP", "UseMP: Wrong type or size");                  

        // Validate that Array and Flag are of the same size
        if (mxGetNumberOfElements(arg_array) != mxGetNumberOfElements(arg_flag))
            mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Input arrays must have the same dimensions: %d, %d", mxGetNumberOfElements(arg_array), mxGetNumberOfElements(arg_flag));       
    }
    
    // Get arguments
    int bit = *((int*)mxGetData(prhs[2]));    
    int value = *((__Type*)mxGetData(prhs[3]));    
    int usemp = *((int*)mxGetData(prhs[4]));            
    
    __Type mask = 1 << (bit-1);
    if (value == 0)
        mask = ~mask;   
    
    // Create output matrix with the same dims and size of the input
    __Type* inp = (__Type*)mxGetData(arg_array);    
    int numel = (int)mxGetNumberOfElements(arg_array);    
    mxLogical* flag = (mxLogical*)mxGetData(arg_flag);
    __Type* out;
    
    // Get the dimensions and size of the input array
    const mwSize* dims = mxGetDimensions(arg_array);
    mwSize ndims = mxGetNumberOfDimensions(arg_array);
    //size_t dataSize = mxGetElementSize(arg_array);    

    //sprintf(s, "mxCreateNumericArray: ndims: %d, dims: %s, numel: %d, class_id: %d, datasize: %d", (int)ndims, dimsStr(ndims, dims), (int)numel, (int)class_id, (int)dataSize);
    //msglog(s);
        
    mxArray *arg_output;
    bool useDuplicateArray = false;
    if (useDuplicateArray) {
        arg_output = mxDuplicateArray(arg_array);
        out = (__Type*)mxGetData(arg_output);
    }
    else {       
        class_id = mxGetClassID(arg_array);            
        arg_output = mxCreateNumericArray(ndims, dims, class_id, mxREAL);    
        out = (__Type*)mxGetData(arg_output);
        
        // Copy input to output
        //memcpy(out, inp, numel * dataSize);
    }
    
    // Process array elements
      
    if (value) {                                    
        // Parallelize the loop with OpenMP        
        if (usemp) {          
            #pragma omp parallel
            {
                #pragma omp for
                for (int i = 0; i < numel; ++i) {
                    out[i] = inp[i];
                    if (flag[i])
                        out[i] |= mask;
                }
            }
        }
        else {
            for (int i = 0; i < numel; ++i) {
                out[i] = inp[i];
                if (flag[i])
                    out[i] |= mask;
            }            
        }
    }
    else {       
        // Parallelize the loop with OpenMP        
        if (usemp) {                   
            #pragma omp parallel
            for (int i = 0; i < numel; ++i) {
                out[i] = inp[i];
                if (flag[i])
                    out[i] &= mask;        
            }
        }
        else {
            for (int i = 0; i < numel; ++i) {
                out[i] = inp[i];
                if (flag[i])
                    out[i] &= mask;                    
            }
        }
            
    }
    
    // Put result
    plhs[0] = arg_output;

}

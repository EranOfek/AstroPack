// See: edit([matlabroot '/extern/examples/mex/explore.c']);
// See: edit([matlabroot '/extern/examples/refbook/matrixDivide.c']);
//
// Access element in 2D array: 
//    val = input[ col*rows + row ];
//
// Access element in 3D array:
//    val = input[ col*rows + row + dep*cols*rows ];
//
//
// OpenMP:
//
//      https://www.mathworks.com/matlabcentral/answers/237411-can-i-make-use-of-openmp-in-my-matlab-mex-files
//
//

#include <stdio.h>
#include <omp.h>
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
    mxClassID class_id;
    
    // Check the number of input and output arguments
    if (nrhs != 4) 
        mexErrMsgIdAndTxt("MATLAB:countVal:nargin", "Four input arguments required.");
    
    if (nlhs > 1) 
        mexErrMsgIdAndTxt("MATLAB:countVal:nargout", "Only one output argument allowed.");
    
    // Get and validate array
    const mxArray *arg_array = prhs[0];       
    const mxArray *arg_flag = prhs[1];       
        
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
    int bit = *((int*)mxGetData(prhs[2]));
    
    // Value
    if ((mxGetClassID(prhs[3]) != mxINT32_CLASS) || (mxGetNumberOfElements(prhs[3]) != 1))
        mexErrMsgIdAndTxt("MATLAB:bitsetFlag:Value", "Value: Wrong type or size");                  
    int value = *((__Type*)mxGetData(prhs[3]));
    
    __Type mask = 1 << (bit-1);
    if (value == 0)
        mask = ~mask;
    
    // Validate that Array and Flag are of the same size
    // mexPrintf("numel: %d, %d\n", mxGetNumberOfElements(arg_array), mxGetNumberOfElements(arg_flag));
    if (mxGetNumberOfElements(arg_array) != mxGetNumberOfElements(arg_flag))
        mexErrMsgIdAndTxt("MATLAB:bitsetFlag", "Input arrays must have the same dimensions: %d, %d", mxGetNumberOfElements(arg_array), mxGetNumberOfElements(arg_flag));
    
    // Create output matrix with the same dims and size of the input
    __Type* inp = (__Type*)mxGetData(arg_array);    
    mwSize numel = mxGetNumberOfElements(arg_array);    
    mxLogical* flag = (mxLogical*)mxGetData(arg_flag);
    __Type* out;
    
    const mxArray *arg_output;
    bool useDuplicateArray = false;
    if (useDuplicateArray) {
        arg_output = mxDuplicateArray(arg_array);
        out = (__Type*)mxGetData(arg_output);
    }
    else {
        // Get the dimensions and size of the input array
        const mwSize* dims = mxGetDimensions(arg_array);
        mwSize ndims = mxGetNumberOfDimensions(arg_array);
        size_t dataSize = mxGetElementSize(arg_array);    
        
        arg_output = mxCreateNumericArray(ndims, dims, class_id, mxREAL);    
        out = (__Type*)mxGetData(arg_output);
        
        // Copy input to output
        //memcpy(out, inp, numel * dataSize);
    }

    // Process array elements
    //#ifdef never
    if (value) {
        
        // Parallelize the loop with OpenMP
        #pragma omp parallel for
        for (mwSize i = 0; i < numel; ++i)
            out[i] = inp[i];
            if (flag[i])
                out[i] |= mask;
    }
    else {
        // Parallelize the loop with OpenMP
        #pragma omp parallel for        
        for (mwSize i = 0; i < numel; ++i)
            out[i] = inp[i];
            if (flag[i])
                out[i] &= mask;        
    }
    //#endif
    
    // Put result
    plhs[0] = arg_output;
}

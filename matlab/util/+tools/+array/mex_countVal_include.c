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
    mxClassID class_id;
    
    // Check the number of input and output arguments
    if (nrhs < 2 || nrhs > 3) 
        mexErrMsgIdAndTxt("MATLAB:countVal:nargin", "Two or three input arguments required.");
    
    if (nlhs > 1) 
        mexErrMsgIdAndTxt("MATLAB:countVal:nargout", "Only one output argument allowed.");
    
    // Get and validate array
    const mxArray *array = prhs[0];       
        
    // Array
    class_id = mxGetClassID(array);            
    if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE)) 
        mexErrMsgIdAndTxt("MATLAB:countVal", "Input matrix type does not match this function");  
    if (mxIsSparse(prhs[0])) 
        mexErrMsgIdAndTxt("MATLAB:countVal", "Sparse array is not supported yet");  

    // Val
    if (((mxGetClassID(prhs[1]) != MEX_TYPE) && (mxGetClassID(prhs[1]) != MEX_UTYPE)) || (mxGetNumberOfElements(prhs[2]) != 1))
        mexErrMsgIdAndTxt("MATLAB:countVal:Val", "Val: Wrong type or size");                  
    __Type Val = *((__Type*)mxGetData(prhs[1]));
    
    // UseNot
    bool UseNot = false;
    if (nrhs == 3) {
        if ((mxGetClassID(prhs[2]) != mxLOGICAL_CLASS) || (mxGetNumberOfElements(prhs[2]) != 1))
            mexErrMsgIdAndTxt("MATLAB:countVal:UseNot", "UseNot: Wrong type or size");              
        
        UseNot = *((bool*)mxGetData(prhs[2]));
    }
    
    // Count elements equal or not equal to Val        
    __Type* array_data = (__Type*)mxGetData(array);
    mwSize numel = mxGetNumberOfElements(array);    
    long long count = 0; 
    if (UseNot) {
        for (mwSize i = 0; i < numel; ++i)
            if (array_data[i] != Val)
                count++;
    }
    else {
        for (mwSize i = 0; i < numel; ++i)
            if (array_data[i] == Val)
                count++;        
    }  
    
    // Put result
    plhs[0] = mxCreateNumericMatrix(1, 1, mxINT64_CLASS, mxREAL);
    *((long long*)mxGetData(plhs[0])) = count;
}

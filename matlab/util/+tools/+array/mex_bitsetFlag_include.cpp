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
      
#ifdef never
static int debug_bit = 1;
bool force=false;
void msglog(const char* s)
{
    if ((debug_bit > 0) || force) {
        mexPrintf("%s\n", s);
        FILE* fp = fopen("c:\\temp\\mex_msglog.log", "a");
        if (fp) {
            fwrite(s, 1, strlen(s), fp);
            fwrite("\n", 1, 1, fp);
            fclose(fp);
        }
    }
}

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

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

typedef unsigned long long __Int;

void mexFunction( 
        int nlhs, mxArray *plhs[],          // Output arguments
        int nrhs, const mxArray *prhs[])    // Input arguments
{
    // Input
    __Int* input, val;
    __Int* out;
    int dim = 1;    
    mwSize rows, cols, deps;
    mwSize row, col, dep;

    mxClassID     class_id;
    const char*   input_type;    
    mwSize        input_ndims;
    const mwSize* input_size;
   
    // Output
    mwSize output_ndims;
    mwSize output_size[4] = {0,0,0,0};
    
    // Need at least one argument
    if (nrhs < 1) {
        return;
    }

    // Get dim
    if ((nrhs > 1) && mxIsScalar(prhs[1])) {
        dim = (int)mxGetScalar(prhs[1]);
    }
    
    // Parse input matrix
    class_id    = mxGetClassID(prhs[0]);        
    input       = (__Int*)mxGetData(prhs[0]);
    input_ndims = mxGetNumberOfDimensions(prhs[0]);
    input_size  = mxGetDimensions(prhs[0]);      
    input_type  = mxGetClassName(prhs[0]);
    
    // 
    if ((class_id != mxINT64_CLASS) && (class_id != mxUINT64_CLASS)) {
        mexPrintf("Input matrix must be int64 or uint64, convert before calling\n");
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
    if ((input_ndims != 2) && (input_ndims != 3)) {
        mexPrintf("Invalid ndims of input matrix, supported ndims is 2 or 3: %d\n", input_ndims);  
        return;
    }
    
    //-------------------------------------------- Input 2D 
    if (input_ndims == 2) {
        
        rows = input_size[0];
        cols = input_size[1];
        
        // 2D, dim=1: For each column, sum all rows
        if (dim == 1) {
            // Create output matrix            
            output_ndims   = 2;
            output_size[0] = 1;
            output_size[1] = input_size[1];        
            plhs[0] = mxCreateNumericArray(output_ndims, output_size, class_id, mxREAL);
            out = (__Int*)mxGetData(plhs[0]);                    
                 
            //
            for (col = 0;  col < cols;  col++) {
                out[col] = 0;
                for (row = 0;  row < rows;  row++) {
                    val = input[ col*rows + row ];
                    out[col] |= val;
                }            
            }                        
        }
        
        // 2D, dim=2: For each row, sum all column        
        else if (dim == 2) {                    
            // Create output matrix            
            output_ndims   = 2;
            output_size[0] = input_size[0];
            output_size[1] = 1;        
            plhs[0] = mxCreateNumericArray(output_ndims, output_size, class_id, mxREAL);
            out = (__Int*)mxGetData(plhs[0]);                    

            //
            for (row = 0;  row < rows;  row++) {                        
                out[row] = 0;
                for (col = 0;  col < cols;  col++) {
                    val = input[ col*rows + row ];
                    out[row] |= val;
                }            
            }            
        }
    }

    //-------------------------------------------- Input 3D 
    else if (input_ndims == 3) {
        rows = input_size[0];
        cols = input_size[1];
        deps = input_size[2];
        
        #ifdef never
        for (dep = 0;  dep < deps;  dep++) {
            for (col = 0;  col < cols;  col++) {
                for (row = 0;  row < rows;  row++) {
                    val = input[ col*rows + row + dep*cols*rows];
                    mexPrintf("input[%02d, %02d, %02d] = %d\n", row+1, col+1, dep+1, val);
                }                                                
            }                                    
        }
        return;
        #endif
        
        //
        if (dim == 1) {
            output_ndims   = 3;
            output_size[0] = 1;
            output_size[1] = input_size[1];
            output_size[2] = input_size[2];
            plhs[0] = mxCreateNumericArray(output_ndims, output_size, class_id, mxREAL);
            out = (__Int*)mxGetData(plhs[0]);                                
            
            //
            for (dep = 0;  dep < deps;  dep++) {
                for (col = 0;  col < cols;  col++) {
                    out[dep*cols + col] = 0;
                    for (row = 0;  row < rows;  row++) {
                        val = input[ col*rows + row + dep*cols*rows];
                        out[dep*cols + col] |= val;
                    }            
                }                                    
            }
        }
        
        //
        else if (dim == 2) {
            output_ndims   = 3;            
            output_size[0] = input_size[0];
            output_size[1] = 1;
            output_size[2] = input_size[2];        
            plhs[0] = mxCreateNumericArray(output_ndims, output_size, class_id, mxREAL);
            out = (__Int*)mxGetData(plhs[0]);                                
            
            //
            for (dep = 0;  dep < deps;  dep++) {            
                for (row = 0;  row < rows;  row++) {                        
                    out[dep*rows + row] = 0;                                    
                    for (col = 0;  col < cols;  col++) {
                        val = input[ col*rows + row + dep*cols*rows];
                        out[dep*rows + row] |= val;
                    }            
                }
            }                        
        }
        
        //
        else if (dim == 3) {
            output_ndims   = 2;                        
            output_size[0] = input_size[0];
            output_size[1] = input_size[1];            
            plhs[0] = mxCreateNumericArray(output_ndims, output_size, class_id, mxREAL);
            out = (__Int*)mxGetData(plhs[0]);                                
            
            //
            for (row = 0;  row < rows;  row++) {                        
                for (col = 0;  col < cols;  col++) {
                    out[col*rows + row] = 0;                    
                    for (dep = 0;  dep < deps;  dep++) {                                    
                        val = input[ col*rows + row + dep*cols*rows ];
                        out[col*rows + row] |= val;
                    }
                }            
            }        
        }
    }
  
    //mexPrintf("output_ndims: %d\n", output_ndims);  
    //mexPrintf("output_size:  %d, %d, %d\n", output_size[0], output_size[1], output_size[2]); 
}


#ifdef never

    mwSize s1, s2, s3;
    mxArray   *Data;
    nDimNum = mxGetNumberOfDimensions(phi);
    pDims = mxGetDimensions(phi);
    Data = mxCreateNumericArray(nDimNum, pDims, mxDOUBLE_CLASS, mxReal);
    pD = (double *) mxGetPr(Data);
    s1 = pDims[0];
    s2 = pDims[1];
    s3 = pDims[2];
    for(i=0; i<s1; i++) {
       for(j=0; j<s2; j++) {
          for(k=0; k<s3; k++) {
             pD[i + j * s1 + k * (s1 * s2)] = i*100+j*10+k;
          }
       }
    }
        
#endif


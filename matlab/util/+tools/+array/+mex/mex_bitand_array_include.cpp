//
// mex_bitand_array_include.cpp
//
// Author: Chen Tishler, May 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: int8, int16, int32, int64
//

// Access element in 2D array: 
//    val = input[ col*rows + row ];
//
// Access element in 3D array:
//    val = input[ col*rows + row + dep*cols*rows ];
//

#include "mex.h"
#include "matrix.h"
	
void mexFunction( 
        int nlhs, mxArray *plhs[],          // Output arguments
        int nrhs, const mxArray *prhs[])    // Input arguments
{
    // Input
    __Type* input, val;
    __Type* out;
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
    input       = (__Type*)mxGetData(prhs[0]);
    input_ndims = mxGetNumberOfDimensions(prhs[0]);
    input_size  = mxGetDimensions(prhs[0]);      
    input_type  = mxGetClassName(prhs[0]);
    
    // 
    if ((class_id != MEX_TYPE) && (class_id != MEX_UTYPE)) {
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
            out = (__Type*)mxGetData(plhs[0]);                    
                 
            //
            for (col = 0;  col < cols;  col++) {
                out[col] = (__Type)(-1);
                for (row = 0;  row < rows;  row++) {
                    val = input[ col*rows + row ];
                    out[col] &= val;
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
            out = (__Type*)mxGetData(plhs[0]);                    

            //
            for (row = 0;  row < rows;  row++) {                        
                out[row] = (__Type)(-1);;
                for (col = 0;  col < cols;  col++) {
                    val = input[ col*rows + row ];
                    out[row] &= val;
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
            out = (__Type*)mxGetData(plhs[0]);                                
            
            //
            for (dep = 0;  dep < deps;  dep++) {
                for (col = 0;  col < cols;  col++) {
                    out[dep*cols + col] = (__Type)(-1);;
                    for (row = 0;  row < rows;  row++) {
                        val = input[ col*rows + row + dep*cols*rows];
                        out[dep*cols + col] &= val;
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
            out = (__Type*)mxGetData(plhs[0]);                                
            
            //
            for (dep = 0;  dep < deps;  dep++) {            
                for (row = 0;  row < rows;  row++) {                        
                    out[dep*rows + row] = (__Type)(-1);;                                    
                    for (col = 0;  col < cols;  col++) {
                        val = input[ col*rows + row + dep*cols*rows];
                        out[dep*rows + row] &= val;
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
            out = (__Type*)mxGetData(plhs[0]);                                
            
            //
            for (row = 0;  row < rows;  row++) {                        
                for (col = 0;  col < cols;  col++) {
                    out[col*rows + row] = (__Type)(-1);;                    
                    for (dep = 0;  dep < deps;  dep++) {                                    
                        val = input[ col*rows + row + dep*cols*rows ];
                        out[col*rows + row] &= val;
                    }
                }            
            }        
        }
    }
}

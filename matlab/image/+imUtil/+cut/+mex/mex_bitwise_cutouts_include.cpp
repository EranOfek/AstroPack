//
// mex_bitwise_cutouts_include.cpp
//
// Author: Dan Elhanati, August 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: int16, int32
//
// Function cuts stamps in a 2D image and performs bitwise (and/or) on each stamp
//
// Inputs:
// mat: 2D array (uint16 or uint32)
// x: x positions for stamp (double)
// y: y positions for stamp (double)
// half_size: half size of stamp, stamp size will be half_size*2+1 (double)
// bitwise_or: perform bitwise_or (true) / bitwise_and (false) 
//
// Example:
// Array32 = uint32(randi(2^32-1,100,100,1));
// res = imUtil.cut.mex.mex_bitwise_cutouts_int32(Array32,[23 34 76 12 76],[32 65 87 34 54],5,true)

#include "mex.h"

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 5) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts::InvalidInput", "Five input arguments are required.");
    }
    
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts:InvalidOutput", "One output argument is required.");
    }
        
    // Input arguments
    __Type *mat = (__Type*)mxGetData(prhs[0]);
    const double *x_coords = mxGetPr(prhs[1]);
    const double *y_coords = mxGetPr(prhs[2]);
    double HalfSize = mxGetScalar(prhs[3]);
    bool bitwise_or = mxIsLogicalScalarTrue(prhs[4]);

    // Check argument 1 (image) - must be numeric or boolean
    if (mxIsNumeric(prhs[0])==0 && mxIsLogical(prhs[0])==0) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts:inputNotNumeric", "Input 1 is not numeric nor logical!");
    }

    // Empty input matrix just returns an empty matrix
    if (mxIsEmpty(prhs[0])) {
        plhs[0] = mxDuplicateArray(prhs[0]);
        if (nlhs > 1) {
            plhs[1] = mxDuplicateArray(prhs[0]);
        }
        return;
    }

    // Get array dimensions
    mwSize nrows = mxGetM(prhs[0]);
    mwSize ncols = mxGetN(prhs[0]);
    long numel = mxGetNumberOfElements(prhs[1]);

    // Check image matrix side and dims
    if (ncols < 2 || nrows < 2 || mxGetNumberOfDimensions(prhs[0]) > 3) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts:inputNotMatrix", "Must input a 2D matrix to mex_bitwise_cutouts");
    }

    // Check argument 2 (x positions) - must be doubles
    if (mxIsNumeric(prhs[1])==0 || mxIsDouble(prhs[1])==0) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts:inputNotNumeric", "Input 2 to mexCutout is not numeric double precision...");
    }

    // Check argument 3 (y positions) - must be doubles
    if (mxIsNumeric(prhs[2])==0 || mxIsDouble(prhs[2])==0) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts:inputNotNumeric", "Input 3 to mexCutout is not numeric double precision...");
    }

    // Check argument 4 (half size) - must be numeric
    if (mxIsNumeric(prhs[3])==0) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts:inputNotNumeric", "Input 4 to mexCutout is not numeric...");
    }

    // Check argument 5 (bitewise_or) - must be boolean
    if (mxIsLogical(prhs[4])==0) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts:inputNotNumeric", "Input 5 is not logical!");
    }

    // Create output matrix
    plhs[0] = mxCreateNumericMatrix(1, numel, MEX_UTYPE, mxREAL);
    __Type *output = (__Type *)mxGetData(plhs[0]);
    
    // round HalfSize
    int HalfSize_int = static_cast<int>(HalfSize);

    // Loop through coordinates and cut stamps
    for (int i = 0; i < numel; i++) {
        int x = int(x_coords[i]);
        int y = int(y_coords[i]);

        // adjusting for matlab indices starting to 1 and not 0
        x--;
        y--;
        
        // check boundaries
        if (x - HalfSize_int < 0 || x + HalfSize_int >= ncols || y - HalfSize_int< 0 || y + HalfSize_int>= nrows) {
            mexWarnMsgIdAndTxt("MATLAB:util:img:mex_bitwise_cutouts:OutOfBounds", "Stamp at position (%d, %d) is out of bounds.", x, y);
            output[i] = 0;
            continue;
        }

        // stamp bitwise initialization
        __Type stamp_value;
        if (bitwise_or) {
            stamp_value = 0;
        }
        else {
            stamp_value = mat[x * nrows + y];
        }        

        // loop through stamp pixels and calculate bitwise
        for (int yy = y - HalfSize_int; yy <= y + HalfSize_int; yy++) {
            for (int xx = x - HalfSize_int; xx <= x + HalfSize_int; xx++) {
                 stamp_value = bitwise_or ? (stamp_value | mat[xx * nrows + yy]) : (stamp_value & mat[xx * nrows + yy]);
            }
        }
        output[i] = stamp_value;
    } 
}

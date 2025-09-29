#include "mex.h"
#include <omp.h> // For OpenMP
#include <immintrin.h> // For SIMD intrinsics

// Function to crop a matrix
void cropMat(const mxArray* Array, const double CCDSEC[4], mxArray** SubMat) {
    // Get dimensions of the input matrix
    mwSize numRows = mxGetM(Array);
    mwSize numCols = mxGetN(Array);
    
    // Extract CCDSEC values and convert to 0-based indexing
    int Jmin = static_cast<int>(CCDSEC[0]) - 1; // Column start index, convert 1-based to 0-based
    int Jmax = static_cast<int>(CCDSEC[1]) - 1; // Column end index, convert 1-based to 0-based
    int Imin = static_cast<int>(CCDSEC[2]) - 1; // Row start index, convert 1-based to 0-based
    int Imax = static_cast<int>(CCDSEC[3]) - 1; // Row end index, convert 1-based to 0-based
    
    // Validate CCDSEC indices
    if (Imin < 0 || Imax >= static_cast<int>(numRows) ||
        Jmin < 0 || Jmax >= static_cast<int>(numCols) ||
        Imin > Imax || Jmin > Jmax) {
        mexErrMsgIdAndTxt("MATLAB:cropMat:invalidIndices",
                          "Invalid indices in CCDSEC.");
    }
    
    // Calculate dimensions of the submatrix
    mwSize subNumRows = Imax - Imin + 1;
    mwSize subNumCols = Jmax - Jmin + 1;
    
    // Create a new mxArray for the submatrix
    *SubMat = mxCreateNumericMatrix(subNumRows, subNumCols, mxUINT16_CLASS, mxREAL);
    uint16_T* subMatData = static_cast<uint16_T*>(mxGetData(*SubMat));
    
    // Get the pointer to the input matrix data
    uint16_T* arrayData = static_cast<uint16_T*>(mxGetData(Array));
    
    // Copy data to the submatrix using a single loop with OpenMP
    #pragma omp parallel for
    for (mwSize col = 0; col < subNumCols; ++col) {
        uint16_T* subColStart = subMatData + col * subNumRows;
        uint16_T* arrayColStart = arrayData + (Jmin + col) * numRows + Imin;

        // Handle data copying with a fallback loop
        for (mwSize row = 0; row < subNumRows; ++row) {
            subColStart[row] = arrayColStart[row];
        }
    }
}

// Entry point for the MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check number of inputs and outputs
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:cropMat:numInputs", "Two inputs required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:cropMat:numOutputs", "One output required.");
    }
    
    // Get the input matrix and CCDSEC
    const mxArray* Array = prhs[0];
    const mxArray* CCDSECArray = prhs[1];
    
    // Validate input matrix
    if (!mxIsUint16(Array)) {
        mexErrMsgIdAndTxt("MATLAB:cropMat:invalidArray", "Input Array must be a uint16 matrix.");
    }
    if (!mxIsDouble(CCDSECArray) || mxGetNumberOfElements(CCDSECArray) != 4) {
        mexErrMsgIdAndTxt("MATLAB:cropMat:invalidCCDSEC", "CCDSEC must be a double precision array with 4 elements.");
    }
    
    // Extract CCDSEC values
    double* ccdsecData = mxGetPr(CCDSECArray);
    double CCDSEC[4];
    for (int i = 0; i < 4; ++i) {
        CCDSEC[i] = ccdsecData[i];
    }
    
    // Call cropMat function
    mxArray* SubMat = nullptr;
    cropMat(Array, CCDSEC, &SubMat);
    
    // Set output
    plhs[0] = SubMat;
}

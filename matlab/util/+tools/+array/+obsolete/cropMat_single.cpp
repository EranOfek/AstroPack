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
    *SubMat = mxCreateNumericMatrix(subNumRows, subNumCols, mxSINGLE_CLASS, mxREAL);
    float* subMatData = static_cast<float*>(mxGetData(*SubMat));
    
    // Get the pointer to the input matrix data
    float* arrayData = static_cast<float*>(mxGetData(Array));
    
    // Copy data to the submatrix using a single loop with OpenMP and SIMD
    #pragma omp parallel for
    for (mwSize col = 0; col < subNumCols; ++col) {
        float* subColStart = subMatData + col * subNumRows;
        float* arrayColStart = arrayData + (Jmin + col) * numRows + Imin;

        // Handle main loop with SIMD for chunks of 8 (float has 8-byte size for AVX)
        for (mwSize row = 0; row < subNumRows; row += 8) {
            if (row + 8 <= subNumRows) {
                __m256 values = _mm256_loadu_ps(arrayColStart + row);
                _mm256_storeu_ps(subColStart + row, values);
            } else {
                // Handle remaining elements
                for (mwSize i = row; i < subNumRows; ++i) {
                    subColStart[i] = arrayColStart[i];
                }
            }
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
    if (!mxIsSingle(Array)) {
        mexErrMsgIdAndTxt("MATLAB:cropMat:invalidArray", "Input Array must be a single precision matrix.");
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

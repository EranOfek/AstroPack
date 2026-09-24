#include "mex.h"
#include <omp.h> // For OpenMP
#include <immintrin.h> // For SIMD intrinsics

// mex CXXFLAGS="\$CXXFLAGS -fopenmp -mavx" LDFLAGS="\$LDFLAGS -fopenmp -mavx" cropMat4.cpp
// Array=(rand(1700,1700));
// CCDSEC=[101 ceil(rand(1,1).*1300+201),  201 ceil(rand(1,1).*1300+201)];tic; for I=1:1:1000, SA2=Array(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2)); end, toc, tic;for I=1:1:100, SA1=cropMat4(Array,CCDSEC);end,toc, max(abs(SA1-SA2),[],'all')


void cropMat(const mxArray* Array, const int CCDSEC[4], mxArray** SubMat) {
    // Get dimensions of the input matrix
    mwSize numRows = mxGetM(Array);
    mwSize numCols = mxGetN(Array);
    
    // Extract CCDSEC values and convert to 0-based indexing
    int Jmin = CCDSEC[0] - 1; // Column start index, convert 1-based to 0-based
    int Jmax = CCDSEC[1] - 1; // Column end index, convert 1-based to 0-based
    int Imin = CCDSEC[2] - 1; // Row start index, convert 1-based to 0-based
    int Imax = CCDSEC[3] - 1; // Row end index, convert 1-based to 0-based
    
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
    *SubMat = mxCreateNumericMatrix(subNumRows, subNumCols, mxDOUBLE_CLASS, mxREAL);
    double* subMatData = mxGetPr(*SubMat);
    
    // Get the pointer to the input matrix data
    double* arrayData = mxGetPr(Array);
    
    // Copy data to the submatrix using a single loop with OpenMP and SIMD
    #pragma omp parallel for
    for (mwSize col = 0; col < subNumCols; ++col) {
        double* subColStart = subMatData + col * subNumRows;
        double* arrayColStart = arrayData + (Jmin + col) * numRows + Imin;

        // Handle main loop with SIMD for chunks of 4
        for (mwSize row = 0; row < subNumRows; row += 4) {
            if (row + 4 <= subNumRows) {
                __m256d values = _mm256_loadu_pd(arrayColStart + row);
                _mm256_storeu_pd(subColStart + row, values);
            } else {
                // Handle remaining elements
                for (mwSize i = row; i < subNumRows; ++i) {
                    subColStart[i] = arrayColStart[i];
                }
            }
        }
    }
}

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
    if (!mxIsNumeric(Array) || mxIsComplex(Array)) {
        mexErrMsgIdAndTxt("MATLAB:cropMat:invalidArray", "Input Array must be a real numeric matrix.");
    }
    if (!mxIsNumeric(CCDSECArray) || mxGetNumberOfElements(CCDSECArray) != 4) {
        mexErrMsgIdAndTxt("MATLAB:cropMat:invalidCCDSEC", "CCDSEC must be a numeric array with 4 elements.");
    }
    
    // Extract CCDSEC values
    int CCDSEC[4];
    double* ccdsecData = mxGetPr(CCDSECArray);
    for (int i = 0; i < 4; ++i) {
        CCDSEC[i] = static_cast<int>(ccdsecData[i]);
    }
    
    // Call cropMat function
    mxArray* SubMat = nullptr;
    cropMat(Array, CCDSEC, &SubMat);
    
    // Set output
    plhs[0] = SubMat;
}

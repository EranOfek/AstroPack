// mex_check_nan_cells.cpp
// This MEX function checks if the contents of elements in a cell array are NaN.
// The function always uses OpenMP for parallel processing.
// mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" isnan_cell_mex.cpp
  
#include "mex.h"
#include <omp.h>
#include <cmath> // For std::isnan

// Gateway function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check number of inputs and outputs
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("MyToolbox:checkNaNCells:nrhs", "One input required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MyToolbox:checkNaNCells:nlhs", "One output required.");
    }

    // Ensure the input is a cell array
    if (!mxIsCell(prhs[0])) {
        mexErrMsgIdAndTxt("MyToolbox:checkNaNCells:notCell", "Input must be a cell array.");
    }

    // Get the input cell array and its dimensions
    const mxArray *cellArray = prhs[0];
    mwSize numCells = mxGetNumberOfElements(cellArray);
    const mwSize *dims = mxGetDimensions(cellArray);
    mwSize numDims = mxGetNumberOfDimensions(cellArray);

    // Create the output logical array with the same dimensions as the input
    plhs[0] = mxCreateLogicalArray(numDims, dims);
    mxLogical *outArray = mxGetLogicals(plhs[0]);

    // Iterate through each cell and check if the content is NaN using OpenMP
    #pragma omp parallel for
    for (mwSize i = 0; i < numCells; ++i) {
        const mxArray *cellElement = mxGetCell(cellArray, i);
        if (cellElement == nullptr || mxIsEmpty(cellElement)) {
            // If the cell is empty or null, it's not NaN
            outArray[i] = false;
        } else if (mxIsNumeric(cellElement) && !mxIsComplex(cellElement) && mxGetNumberOfElements(cellElement) == 1) {
            // Check if the cell contains a single numeric scalar (any
            // class - double, single, or an integer type) and if it is
            // NaN. Previously this only recognized mxIsDouble, silently
            // answering false for e.g. a single-precision or integer-
            // typed scalar cell (and mxGetPr on a non-double array would
            // have misread the underlying bytes anyway, had that branch
            // been taken) - see issue #1211. mxGetScalar is type-safe
            // for any numeric class.
            double value = mxGetScalar(cellElement);
            outArray[i] = std::isnan(value);
        } else {
            // Non-scalar numeric content or a non-numeric type: not NaN
            // by this function's contract (a scalar-or-NaN sentinel
            // check, not a general isnan()).
            outArray[i] = false;
        }
    }
}

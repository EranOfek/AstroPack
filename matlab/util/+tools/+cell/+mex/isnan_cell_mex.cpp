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
        } else if (mxIsDouble(cellElement) && mxGetNumberOfElements(cellElement) == 1) {
            // Check if the cell contains a single double and if it is NaN
            double value = *mxGetPr(cellElement);
            outArray[i] = std::isnan(value);
        } else {
            // If the cell does not contain a single double, it's not NaN
            outArray[i] = false;
        }
    }
}

// mex_check_empty_cells.cpp
// This MEX function checks if elements in a cell array are empty.
// The function always uses OpenMP for parallel processing.
// mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" isempty_cell_mex.cpp

#include "mex.h"
#include <omp.h>

// Gateway function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check number of inputs and outputs
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("MyToolbox:checkEmptyCells:nrhs", "One input required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MyToolbox:checkEmptyCells:nlhs", "One output required.");
    }

    // Ensure the input is a cell array
    if (!mxIsCell(prhs[0])) {
        mexErrMsgIdAndTxt("MyToolbox:checkEmptyCells:notCell", "Input must be a cell array.");
    }

    // Get the input cell array and its dimensions
    const mxArray *cellArray = prhs[0];
    mwSize numCells = mxGetNumberOfElements(cellArray);
    const mwSize *dims = mxGetDimensions(cellArray);
    mwSize numDims = mxGetNumberOfDimensions(cellArray);

    // Create the output logical array with the same dimensions as the input
    plhs[0] = mxCreateLogicalArray(numDims, dims);
    mxLogical *outArray = mxGetLogicals(plhs[0]);

    // Iterate through each cell and check if it's empty using OpenMP
    #pragma omp parallel for
    for (mwSize i = 0; i < numCells; ++i) {
        const mxArray *cellElement = mxGetCell(cellArray, i);
        outArray[i] = (cellElement == nullptr || mxIsEmpty(cellElement));
    }
}

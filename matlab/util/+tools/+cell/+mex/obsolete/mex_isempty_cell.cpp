//
// mex_isempty_cell_include.cpp
//
// Author: Dan Elhanati, June 2024
//
//
//

#include "mex.h"

// The gateway function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // Check for proper number of arguments
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MyToolbox:isEmptyCellArray:nrhs", "One input required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MyToolbox:isEmptyCellArray:nlhs", "One output required.");
    }
    
    // Ensure the input is a cell array
    if (!mxIsCell(prhs[0])) {
        mexErrMsgIdAndTxt("MyToolbox:isEmptyCellArray:notCell", "Input must be a cell array.");
    }
    
    // Get the input cell array
    const mxArray *cellArray = prhs[0];
    mwSize numCells = mxGetNumberOfElements(cellArray);
    bool useOpenMP = *((int*)mxGetData(prhs[1]));    

    // Create the output logical array
    plhs[0] = mxCreateLogicalMatrix(1, numCells);
    mxLogical *outArray = mxGetLogicals(plhs[0]);
    
    // Iterate through each cell and check if it's empty
    if (useOpenMP) {
        #pragma omp parallel for
        for (mwSize i = 0; i < numCells; ++i) {
            const mxArray *cellElement = mxGetCell(cellArray, i);
            if (cellElement == nullptr || mxIsEmpty(cellElement)) {
                outArray[i] = true;
            } else {
                outArray[i] = false;
            }
        }
    } else {
        for (mwSize i = 0; i < numCells; ++i) {
            const mxArray *cellElement = mxGetCell(cellArray, i);
            if (cellElement == nullptr || mxIsEmpty(cellElement)) {
                outArray[i] = true;
            } else {
                outArray[i] = false;
            }
        }
    }
}

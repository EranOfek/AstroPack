#include "mex.h"
#include <vector>
#include <cmath>

// Helper function to check if a value is NaN
bool isNaN(double value) {
    return std::isnan(value);
}

// The gateway function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Check for correct number of inputs and outputs
    if (nrhs != 1) {
        mexErrMsgTxt("One input required.");
    }
    if (nlhs != 1) {
        mexErrMsgTxt("One output required.");
    }

    // Input cell array
    const mxArray *cellArray = prhs[0];
    
    if (!mxIsCell(cellArray)) {
        mexErrMsgTxt("Input must be a cell array.");
    }

    // Get the number of cells
    mwSize numCells = mxGetNumberOfElements(cellArray);

    // Initialize output logical array
    mxLogical *flagArray = mxGetLogicals(plhs[0] = mxCreateLogicalMatrix(1, numCells));
    
    // Loop through each cell
    for (mwSize i = 0; i < numCells; i++) {
        mxArray *cellContent = mxGetCell(cellArray, i);
        
        // Check if the cell contains a numeric value
        if (cellContent != nullptr && mxIsDouble(cellContent)) {
            double *data = mxGetPr(cellContent);
            mwSize numElements = mxGetNumberOfElements(cellContent);
            
            // Check if any element in the cell is NaN
            bool containsNaN = false;
            for (mwSize j = 0; j < numElements; j++) {
                if (isNaN(data[j])) {
                    containsNaN = true;
                    break;
                }
            }
            
            flagArray[i] = containsNaN;
        } else {
            // If the cell does not contain numeric data, it cannot contain NaNs
            flagArray[i] = false;
        }
    }
}

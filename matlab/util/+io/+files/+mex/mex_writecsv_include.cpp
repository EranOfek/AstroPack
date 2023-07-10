//
// mex_writecsv_include.cpp
//
// Author: Chen Tishler, July 2023
//
//
// Flags for cmex.py:
//
//      $dtype: int8, int16, int32, int64, single, double
//

#include "mex.h"
#include <fstream>
#include <string>

typedef long long int64;

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    mxClassID class_id;

    // check number of arguments
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("MyToolbox:arrayProduct:nrhs",
                          "Three inputs required.");
    }
    if (nlhs != 0) {
        mexErrMsgIdAndTxt("MyToolbox:arrayProduct:nlhs",
                          "No output required.");
    }

    // first input: cell array of matrices
    if (!mxIsCell(prhs[0])) {
        mexErrMsgIdAndTxt("MyToolbox:inputNotCell",
                          "Input 1 must be a cell array.");
    }
    mwSize numMatrices = mxGetNumberOfElements(prhs[0]);

    // second input: cell array of headers
    if (!mxIsCell(prhs[1])) {
        mexErrMsgIdAndTxt("MyToolbox:inputNotCell",
                          "Input 2 must be a cell array.");
    }

    // third input: file name
    if (!mxIsChar(prhs[2])) {
        mexErrMsgIdAndTxt("MyToolbox:inputNotString",
                          "Input 3 must be a string.");
    }
    mwSize buflen = mxGetN(prhs[2])*sizeof(mxChar)+1;
    char* filename = mxArrayToString(prhs[2]);

    // write to CSV
    std::ofstream file;
    file.open(filename);
	char buffer[64];
	
    // write headers
    mwSize ncols = mxGetNumberOfElements(prhs[1]);
    for (mwSize j = 0; j < ncols; ++j) {
        mxArray *header = mxGetCell(prhs[1], j);
        file << mxArrayToString(header);
        if (j < ncols - 1) {
            file << ',';
        }
    }
    file << '\n';

    // write data
    for (mwSize k = 0; k < numMatrices; ++k) {
        mxArray *matrixCell = mxGetCell(prhs[0], k);
        __Type* inMatrix = (__Type*)mxGetPr(matrixCell);
        mwSize nrows = mxGetM(matrixCell);

        for (mwSize i = 0; i < nrows; ++i) {
            for (mwSize j = 0; j < ncols; ++j) {
                __Type value = inMatrix[j*nrows + i];

                // If the value is an integer, output it without decimal point
                // The optimizer will remove it of value is integer type.
                if (value == static_cast<int>(value)) {
                    file << static_cast<int>(value);
                } else {
					sprintf(buffer, "%.15g", value);
					file << buffer;
                }

                if (j < ncols - 1) {
                    file << ',';
                }
            }
            file << '\n';
        }
    }

    file.close();

    // free memory
    mxFree(filename);
}

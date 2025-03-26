#include "mex.h"
#include <string.h>
#include <math.h>
#include <omp.h>

# mex CFLAGS="\$CFLAGS -fopenmp -O3 -march=native" LDFLAGS="\$LDFLAGS -fopenmp"findAllInAll.cpp 

void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[]) {

    if (nrhs != 2)
        mexErrMsgIdAndTxt("find_string_indices:nrhs", "Two input arguments required.");

    if (!mxIsCell(prhs[0]) || !mxIsCell(prhs[1]))
        mexErrMsgIdAndTxt("find_string_indices:type", "Inputs must be cell arrays of strings.");

    mwSize nA = mxGetNumberOfElements(prhs[0]);
    mwSize nB = mxGetNumberOfElements(prhs[1]);

    // Pre-convert B to C strings
    char** B_strings = (char**)mxCalloc(nB, sizeof(char*));
    for (mwSize j = 0; j < nB; ++j) {
        mxArray* bStr = mxGetCell(prhs[1], j);
        B_strings[j] = (bStr && mxIsChar(bStr)) ? mxArrayToString(bStr) : NULL;
    }

    // Output array
    plhs[0] = mxCreateDoubleMatrix(nA, 1, mxREAL);
    double* out = mxGetPr(plhs[0]);

    #pragma omp parallel for
    for (mwSize i = 0; i < nA; ++i) {
        mxArray* aStr = mxGetCell(prhs[0], i);
        if (!mxIsChar(aStr)) {
            out[i] = mxGetNaN();
            continue;
        }

        char* aText = mxArrayToString(aStr);
        if (!aText) {
            out[i] = mxGetNaN();
            continue;
        }

        double index = mxGetNaN();
        for (mwSize j = 0; j < nB; ++j) {
            if (B_strings[j] && strcmp(aText, B_strings[j]) == 0) {
                index = (double)(j + 1); // MATLAB uses 1-based indexing
                break;
            }
        }
        out[i] = index;
        mxFree(aText);
    }

    // Free memory
    for (mwSize j = 0; j < nB; ++j) {
        if (B_strings[j])
            mxFree(B_strings[j]);
    }
    mxFree(B_strings);
}

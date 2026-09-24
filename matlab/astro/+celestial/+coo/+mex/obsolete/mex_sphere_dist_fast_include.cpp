//
// mex_sphere_dist_fast_include.cpp
//
// Author: Dan Elhanati, July 2023
//
//
// Flags for cmex.py:
//
// 		$dtype: single, double
//

#include "mex.h"
#include <math.h>

void computeDist(__Type* RA1, __Type* Dec1, __Type* RA2, __Type* Dec2, double* Dist, mwSize numPoints) {
    for (mwSize i = 0; i < numPoints; i++) {
        double sin_Dec1, cos_Dec1, sin_Dec2, cos_Dec2, cos_RA_diff;
        sincos(Dec1[i], &sin_Dec1, &cos_Dec1);
        sincos(Dec2[i], &sin_Dec2, &cos_Dec2);
        cos_RA_diff = cos(RA1[i] - RA2[i]);

        double dot_product = sin_Dec1 * sin_Dec2 + cos_Dec1 * cos_Dec2 * cos_RA_diff;

        if (dot_product > 1.0) {
            dot_product = 1.0;
        } else if (dot_product < -1.0) {
            dot_product = -1.0;
        }

        Dist[i] = acos(dot_product);
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("custom:usage", "Four input arguments are required.");
    }

    if (nlhs != 1) {
        mexErrMsgIdAndTxt("custom:usage", "One output argument is required.");
    }
    
    __Type *RA_1 = (__Type*)mxGetData(prhs[0]);
    __Type *Dec_1 = (__Type*)mxGetData(prhs[1]);
    __Type *RA_2 = (__Type*)mxGetData(prhs[2]);
    __Type *Dec_2 = (__Type*)mxGetData(prhs[3]);
    mwSize numPoints = mxGetNumberOfElements(prhs[0]);

    plhs[0] = mxCreateDoubleMatrix(1, numPoints, mxREAL);
    double* Dist = mxGetPr(plhs[0]);

    computeDist(RA_1, Dec_1, RA_2, Dec_2, Dist, numPoints);
}

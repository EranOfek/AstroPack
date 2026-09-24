#include "mex.h"
#include <cmath>
#include <omp.h>

void haversine_approx(const double lon1, const double lat1, const double* lon2, const double* lat2, double* distances, mwSize numPoints) {
    double cos_lat1 = cos(lat1); // Precompute cos(lat1)
    #pragma omp parallel for
    for (mwSize i = 0; i < numPoints; ++i) {
        double dlon = lon2[i] - lon1;
        double dlat = lat2[i] - lat1;

        double sin_dlat = sin(dlat / 2.0);
        double sin_dlon = sin(dlon / 2.0);
        double a = sin_dlat * sin_dlat + cos_lat1 * cos(lat2[i]) * sin_dlon * sin_dlon;

        // Approximation
        distances[i] = 2.0 * sqrt(a);
    }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumInputs", "Four inputs required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumOutputs", "One output required.");
    }

    double lon1 = mxGetScalar(prhs[0]);
    double lat1 = mxGetScalar(prhs[1]);
    double* lon2 = mxGetPr(prhs[2]);
    double* lat2 = mxGetPr(prhs[3]);
    mwSize numPoints = mxGetNumberOfElements(prhs[2]);

    if (mxGetNumberOfElements(prhs[2]) != mxGetNumberOfElements(prhs[3])) {
        mexErrMsgIdAndTxt("mexFunction:InvalidInputSize", "lon2 and lat2 must have the same number of elements.");
    }

    plhs[0] = mxCreateDoubleMatrix(numPoints, 1, mxREAL);
    double* distances = mxGetPr(plhs[0]);

    haversine_approx(lon1, lat1, lon2, lat2, distances, numPoints);
}

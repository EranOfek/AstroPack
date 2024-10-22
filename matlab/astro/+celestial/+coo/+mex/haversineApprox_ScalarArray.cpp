#include "mex.h"
#include <cmath>
#include <omp.h>

// mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" haversine_sa_app.cpp
// Function to compute the haversine distance using first-order Taylor expansion
void haversine_approx(const double lon1, const double lat1, const double* lon2, const double* lat2, double* distances, mwSize numPoints) {
    #pragma omp parallel for
    for (mwSize i = 0; i < numPoints; ++i) {
        double dlon = lon2[i] - lon1;
        double dlat = lat2[i] - lat1;

        // Haversine formula
        double sin_dlat = sin(dlat / 2.0);
        double sin_dlon = sin(dlon / 2.0);
        double a = sin_dlat * sin_dlat + cos(lat1) * cos(lat2[i]) * sin_dlon * sin_dlon;

        // Use sqrt(x) approximation instead of 2*atan2(sqrt(a), sqrt(1-a))
        distances[i] = 2.0 * sqrt(a);
    }
}

// The MEX function gateway
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Input validation
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumInputs", "Four inputs required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumOutputs", "One output required.");
    }

    // Get the inputs
    double lon1 = mxGetScalar(prhs[0]);
    double lat1 = mxGetScalar(prhs[1]);
    double* lon2 = mxGetPr(prhs[2]);
    double* lat2 = mxGetPr(prhs[3]);
    mwSize numPoints = mxGetNumberOfElements(prhs[2]);

    // Create the output array
    plhs[0] = mxCreateDoubleMatrix(numPoints, 1, mxREAL);
    double* distances = mxGetPr(plhs[0]);

    // Call the haversine function
    haversine_approx(lon1, lat1, lon2, lat2, distances, numPoints);
}

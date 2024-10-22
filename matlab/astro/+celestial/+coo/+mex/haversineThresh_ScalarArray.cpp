#include "mex.h"
#include <cmath>
#include <omp.h>
#include <limits>  // For std::numeric_limits

// mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" haversineApprox_ScalarArray.cpp
// Function to compute the haversine distance conditionally
void haversine_conditional(const double lon1, const double lat1, const double* lon2, const double* lat2, double* distances, mwSize numPoints, double threshold) {
    const double pi = 3.141592653589793;

    #pragma omp parallel for
    for (mwSize i = 0; i < numPoints; ++i) {
        double dlon = lon2[i] - lon1;
        double dlat = lat2[i] - lat1;

        // Condition: Perform calculation if dlon and dlat are within thresholds
        if (std::abs(dlon) < threshold && std::abs(dlat) < threshold && std::abs(dlon) <= pi && std::abs(dlat) <= pi/2) {
            // Haversine formula calculation
            double sin_dlat = sin(dlat / 2.0);
            double sin_dlon = sin(dlon / 2.0);
            double a = sin_dlat * sin_dlat + cos(lat1) * cos(lat2[i]) * sin_dlon * sin_dlon;
            distances[i] = 2.0 * atan2(sqrt(a), sqrt(1.0 - a));
        } else {
            // Set distance to NaN if condition is not met
            distances[i] = std::numeric_limits<double>::quiet_NaN();
        }
    }
}

// The MEX function gateway
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Input validation
    if (nrhs < 4 || nrhs > 5) {
        mexErrMsgIdAndTxt("mexFunction:InvalidNumInputs", "Four or five inputs required.");
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

    // Set default threshold if not provided
    double threshold = 0.001;
    if (nrhs == 5) {
        threshold = mxGetScalar(prhs[4]);
    }

    // Create the output array
    plhs[0] = mxCreateDoubleMatrix(numPoints, 1, mxREAL);
    double* distances = mxGetPr(plhs[0]);

    // Call the haversine function with conditional checks
    haversine_conditional(lon1, lat1, lon2, lat2, distances, numPoints, threshold);
}

#include "mex.h"
#include <cmath>
#include <omp.h>  // OpenMP for parallelization

// mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" haversine_sa3.cpp
// Function to compute spherical distance using the Haversine formula
void haversine(const double lon1, const double lat1, const double* lon2, const double* lat2, double* distance, mwSize num_points) {
    const double cosLat1 = cos(lat1);
    const double sinLat1 = sin(lat1);

    #pragma omp parallel for
    for (mwSize i = 0; i < num_points; ++i) {
        const double dLon = lon2[i] - lon1;
        const double dLat = lat2[i] - lat1;

        const double sinDLat2 = sin(dLat / 2.0);
        const double sinDLon2 = sin(dLon / 2.0);

        // Haversine formula
        const double a = sinDLat2 * sinDLat2 + cosLat1 * cos(lat2[i]) * sinDLon2 * sinDLon2;

        // Simplified to use atan instead of atan2
        distance[i] = 2.0 * atan(sqrt(a) / sqrt(1.0 - a));
    }
}

// MEX function entry point
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("MATLAB:haversine:invalidNumInputs", "Four inputs required.");
    }

    // Get scalar lon1, lat1
    const double lon1 = mxGetScalar(prhs[0]);
    const double lat1 = mxGetScalar(prhs[1]);

    // Get arrays lon2, lat2
    const double* lon2 = mxGetPr(prhs[2]);
    const double* lat2 = mxGetPr(prhs[3]);

    mwSize num_points = mxGetNumberOfElements(prhs[2]);

    // Create output array
    plhs[0] = mxCreateDoubleMatrix(mxGetM(prhs[2]), mxGetN(prhs[2]), mxREAL);
    double* distance = mxGetPr(plhs[0]);

    // Perform Haversine computation
    haversine(lon1, lat1, lon2, lat2, distance, num_points);
}

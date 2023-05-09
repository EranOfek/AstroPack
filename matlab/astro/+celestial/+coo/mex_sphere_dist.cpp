#include "mex.h"
#include <math.h>
#include <omp.h>
#include <stdbool.h>
#include <float.h>

void sphere_dist(double RA_1, double Dec_1, double RA_2, double Dec_2, double Threshold, double *Dist, bool *Flag) {
    double sin_Dec1, cos_Dec1, sin_Dec2, cos_Dec2;

    sincos(Dec_1, &sin_Dec1, &cos_Dec1);
    sincos(Dec_2, &sin_Dec2, &cos_Dec2);

    double cos_diff_RA = cos(RA_1 - RA_2);
    *Dist = acos(sin_Dec1 * sin_Dec2 + cos_Dec1 * cos_Dec2 * cos_diff_RA);

    *Flag = fabs(Dec_1 - Dec_2) > Threshold;
    if (*Flag) {
        *Dist = DBL_MAX; // Set to Inf
        // *Dist = 0.0 / 0.0; // Set to NaN
    }
}

void compute_distances(double *RA, double *Dec, double *result, bool *flags, int n, double Threshold) {
    #pragma omp parallel for
    for (int i = 0; i < n; ++i) {
        sphere_dist(RA[i], Dec[i], RA[i + 1], Dec[i + 1], Threshold, &result[i], &flags[i]);
    }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Validate input and output argument count
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("MATLAB:sphere_dist_mex:invalidNumInputs", "Three input arguments required.");
    }
    if (nlhs > 2) {
        mexErrMsgIdAndTxt("MATLAB:sphere_dist_mex:maxlhs", "Too many output arguments.");
    }

    // Get input arguments
    double *RA = mxGetPr(prhs[0]);
    double *Dec = mxGetPr(prhs[1]);
    double Threshold = mxGetScalar(prhs[2]);
    int n = mxGetNumberOfElements(prhs[0]);

    // Create output arrays
    plhs[0] = mxCreateDoubleMatrix(n - 1, 1, mxREAL);
    double *result = mxGetPr(plhs[0]);

    plhs[1] = mxCreateLogicalMatrix(n - 1, 1);
    bool *flags = mxGetLogicals(plhs[1]);

    // Call the compute_distances function
    compute_distances(RA, Dec, result, flags, n - 1, Threshold);
}

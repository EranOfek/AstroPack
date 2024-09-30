#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <omp.h>  // Include OpenMP header
#include "mex.h"

#define pi 3.141592653589793238462643383279502884197
#define twopi (2.0 * pi)
#define halfpi (pi / 2.0)

// Utility functions from the HEALPix library
int64_t isqrt64(int64_t v) {
    int64_t res = sqrt(v + 0.5);
    if (v < ((int64_t)(1) << 50)) return (long)res;
    if (res * res > v)
        --res;
    else if ((res + 1) * (res + 1) <= v)
        ++res;
    return (long)res;
}

// Function to convert pixel index to (theta, phi) in ring ordering
static void pix2ang_ring_z_phi64(int64_t nside_, int64_t pix, double *z, double *phi) {
    int64_t ncap_ = nside_ * (nside_ - 1) * 2;
    int64_t npix_ = 12 * nside_ * nside_;
    double fact2_ = 4.0 / npix_;

    if (pix < ncap_) {  // North Polar cap
        int64_t iring = (1 + isqrt64(1 + 2 * pix)) >> 1;  // counted from North pole
        int64_t iphi = (pix + 1) - 2 * iring * (iring - 1);

        *z = 1.0 - (iring * iring) * fact2_;
        *phi = (iphi - 0.5) * halfpi / iring;
    } else if (pix < (npix_ - ncap_)) {  // Equatorial region
        double fact1_ = (nside_ << 1) * fact2_;
        int64_t ip = pix - ncap_;
        int64_t iring = ip / (4 * nside_) + nside_;  // counted from North pole
        int64_t iphi = ip % (4 * nside_) + 1;

        // 1 if iring+nside is odd, 1/2 otherwise
        double fodd = ((iring + nside_) & 1) ? 1.0 : 0.5;
        int64_t nl2 = 2 * nside_;
        *z = (nl2 - iring) * fact1_;
        *phi = (iphi - fodd) * pi / nl2;
    } else {  // South Polar cap
        int64_t ip = npix_ - pix;
        int64_t iring = (1 + isqrt64(2 * ip - 1)) >> 1;  // counted from South pole
        int64_t iphi = 4 * iring + 1 - (ip - 2 * iring * (iring - 1));

        *z = -1.0 + (iring * iring) * fact2_;
        *phi = (iphi - 0.5) * halfpi / iring;
    }
}

// Main MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("pix2ang_ring_mex:nrhs", "Two inputs required: nside and pixel indices");
    }
    if (nlhs != 2) {
        mexErrMsgIdAndTxt("pix2ang_ring_mex:nlhs", "Two outputs required: longitude and latitude arrays");
    }

    // Input variables
    int64_t nside = (int64_t)mxGetScalar(prhs[0]);
    double *pixel_indices = mxGetPr(prhs[1]);

    // Get the number of elements in the input pixel array
    size_t num_elements = mxGetNumberOfElements(prhs[1]);

    // Prepare the output arrays with the same dimensions as the input
    plhs[0] = mxCreateDoubleMatrix(num_elements, 1, mxREAL); // Longitudes
    plhs[1] = mxCreateDoubleMatrix(num_elements, 1, mxREAL); // Latitudes
    double *longitudes = mxGetPr(plhs[0]);
    double *latitudes = mxGetPr(plhs[1]);

    // Parallelize the loop using OpenMP
    #pragma omp parallel for
    for (size_t i = 0; i < num_elements; i++) {
        int64_t ipix = (int64_t)pixel_indices[i];

        // Calculate z and phi from the pixel index in ring ordering
        double z, phi;
        pix2ang_ring_z_phi64(nside, ipix, &z, &phi);

        // Convert z to theta
        double theta = acos(z);

        // Convert theta to latitude and phi to longitude
        latitudes[i] = halfpi - theta; // Latitude
        longitudes[i] = phi; // Longitude
    }
}

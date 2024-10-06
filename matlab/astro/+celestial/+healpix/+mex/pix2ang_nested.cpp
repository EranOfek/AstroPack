#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <omp.h>  // Include OpenMP header
#include "mex.h"

#define pi 3.141592653589793238462643383279502884197
#define twopi (2.0 * pi)
#define halfpi (pi / 2.0)

// Define the jrll and jpll arrays as provided in the HEALPix C code
static const int jrll[] = { 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 };
static const int jpll[] = { 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 };

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

// Functions for bit manipulation (part of HEALPix library logic)
static int64_t compress_bits64(int64_t v) {
    static const short ctab[] = {
#define Z(a) a, a + 1, a + 256, a + 257
#define Y(a) Z(a), Z(a + 2), Z(a + 512), Z(a + 514)
#define X(a) Y(a), Y(a + 4), Y(a + 1024), Y(a + 1028)
        X(0), X(8), X(2048), X(2056)
#undef X
#undef Y
#undef Z
    };
    int64_t raw = v & 0x5555555555555555ull;
    raw |= raw >> 15;
    return ctab[raw & 0xff] | (ctab[(raw >> 8) & 0xff] << 4) |
           (ctab[(raw >> 32) & 0xff] << 16) | (ctab[(raw >> 40) & 0xff] << 20);
}

static void nest2xyf64(int64_t nside, int64_t pix, int *ix, int *iy, int *face_num) {
    int64_t npface_ = nside * nside;
    *face_num = pix / npface_;
    pix &= (npface_ - 1);
    *ix = compress_bits64(pix);
    *iy = compress_bits64(pix >> 1);
}

// Function to convert pixel index to (theta, phi) in nested ordering
static void pix2ang_nest_z_phi64(int64_t nside_, int64_t pix, double *z, double *phi) {
    int64_t nl4 = nside_ * 4;
    int64_t npix_ = 12 * nside_ * nside_;
    double fact2_ = 4.0 / npix_;
    int face_num, ix, iy;
    int64_t jr, nr, kshift, jp;

    // Convert nested index to face, ix, iy
    nest2xyf64(nside_, pix, &ix, &iy, &face_num);
    jr = (jrll[face_num] * nside_) - ix - iy - 1;

    if (jr < nside_) {
        double tmp;
        nr = jr;
        tmp = (nr * nr) * fact2_;
        *z = 1 - tmp;
        kshift = 0;
    } else if (jr > 3 * nside_) {
        double tmp;
        nr = nl4 - jr;
        tmp = (nr * nr) * fact2_;
        *z = tmp - 1;
        kshift = 0;
    } else {
        double fact1_ = (nside_ << 1) * fact2_;
        nr = nside_;
        *z = (2 * nside_ - jr) * fact1_;
        kshift = (jr - nside_) & 1;
    }

    jp = (jpll[face_num] * nr + ix - iy + 1 + kshift) / 2;
    if (jp > nl4) jp -= nl4;
    if (jp < 1) jp += nl4;

    *phi = (jp - (kshift + 1) * 0.5) * (halfpi / nr);
}

// Main MEX function
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("pix2ang_nest_mex:nrhs", "Two inputs required: nside and pixel indices");
    }
    if (nlhs != 2) {
        mexErrMsgIdAndTxt("pix2ang_nest_mex:nlhs", "Two outputs required: longitude and latitude arrays");
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

        // Calculate z and phi from the pixel index in nested ordering
        double z, phi;
        pix2ang_nest_z_phi64(nside, ipix, &z, &phi);

        // Convert z to theta
        double theta = acos(z);

        // Convert theta to latitude and phi to longitude
        latitudes[i] = halfpi - theta; // Latitude
        longitudes[i] = phi; // Longitude
    }
}

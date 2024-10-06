#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <omp.h>  // Include OpenMP header
#include "mex.h"

#define twothird (2.0/3.0)
#define pi 3.141592653589793238462643383279502884197
#define twopi (2.0 * pi)
#define halfpi (pi / 2.0)
#define inv_halfpi (2.0 / pi)

// ang2pix_nested(Nside, Long[rad], Lat[rad])
// mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" ang2pix_nested.cpp

// Function to spread bits for 64-bit indexing
static int64_t spread_bits64(int v) {
    static const int64_t utab[] = {
        0x00, 0x01, 0x04, 0x05, 0x10, 0x11, 0x14, 0x15,
        0x40, 0x41, 0x44, 0x45, 0x50, 0x51, 0x54, 0x55
    };
    return (int64_t)(utab[v & 0x0F]) |
           ((int64_t)(utab[(v >> 4) & 0x0F]) << 8) |
           ((int64_t)(utab[(v >> 8) & 0x0F]) << 16) |
           ((int64_t)(utab[(v >> 12) & 0x0F]) << 24);
}

// Function to convert (ix, iy, face_num) to the nested index in 64-bit
static int64_t xyf2nest64 (int64_t nside, int ix, int iy, int face_num) {
    return (face_num * nside * nside) + spread_bits64(ix) + (spread_bits64(iy) << 1);
}

// Main 64-bit version of ang2pix_nest function
int64_t ang2pix_nest_z_phi64 (int64_t nside, double z, double phi) {
    double za = fabs(z);
    double tt = fmod(phi, twopi) * inv_halfpi; /* in [0,4) */
    int face_num, ix, iy;

    if (za <= twothird) { /* Equatorial region */
        double temp1 = nside * (0.5 + tt);
        double temp2 = nside * (z * 0.75);
        int64_t jp = (int64_t)(temp1 - temp2); /* index of ascending edge line */
        int64_t jm = (int64_t)(temp1 + temp2); /* index of descending edge line */
        int64_t ifp = jp / nside;  /* in {0,4} */
        int64_t ifm = jm / nside;
        face_num = (ifp == ifm) ? (ifp | 4) : ((ifp < ifm) ? ifp : (ifm + 8));

        ix = jm & (nside - 1);
        iy = nside - (jp & (nside - 1)) - 1;
    } else { /* Polar region */
        int ntt = (int)tt, jp, jm;
        double tp, tmp;
        if (ntt >= 4) ntt = 3;
        tp = tt - ntt;
        tmp = nside * sqrt(3 * (1 - za));

        jp = (int64_t)(tp * tmp); /* increasing edge line index */
        jm = (int64_t)((1.0 - tp) * tmp); /* decreasing edge line index */
        if (jp >= nside) jp = nside - 1; /* for points too close to the boundary */
        if (jm >= nside) jm = nside - 1;
        if (z >= 0) {
            face_num = ntt;  /* in {0,3} */
            ix = nside - jm - 1;
            iy = nside - jp - 1;
        } else {
            face_num = ntt + 8; /* in {8,11} */
            ix = jp;
            iy = jm;
        }
    }

    return xyf2nest64(nside, ix, iy, face_num);
}

// Gateway function for the MEX interface with input (nside, long, lat arrays)
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("ang2pix_nest_mex:nrhs", "Three inputs required: nside, long, lat");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("ang2pix_nest_mex:nlhs", "One output required.");
    }

    // Input variables
    int64_t nside = (int64_t)mxGetScalar(prhs[0]);
    double *longitude = mxGetPr(prhs[1]);
    double *latitude = mxGetPr(prhs[2]);

    // Get the dimensions of the input arrays
    const mwSize *dims = mxGetDimensions(prhs[1]);
    size_t num_elements = mxGetNumberOfElements(prhs[1]);

    // Check that longitude and latitude have the same size
    if (num_elements != mxGetNumberOfElements(prhs[2])) {
        mexErrMsgIdAndTxt("ang2pix_nest_mex:inputSize", "Longitude and latitude arrays must be of the same size.");
    }

    // Prepare the output array with the same dimensions as the input
    plhs[0] = mxCreateNumericArray(mxGetNumberOfDimensions(prhs[1]), dims, mxINT64_CLASS, mxREAL);
    int64_t *output = (int64_t *)mxGetData(plhs[0]);

    // Parallelize the loop using OpenMP
    #pragma omp parallel for
    for (size_t i = 0; i < num_elements; i++) {
        // Convert lat and long to z and phi
        double theta = pi / 2.0 - latitude[i]; // Convert latitude to colatitude
        double z = cos(theta);
        double phi = longitude[i];

        // Calculate the nested index
        output[i] = ang2pix_nest_z_phi64(nside, z, phi);
    }
}

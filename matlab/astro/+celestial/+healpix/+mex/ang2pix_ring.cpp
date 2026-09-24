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

// Main 64-bit version of ang2pix_ring function
int64_t ang2pix_ring_z_phi64(int64_t nside, double z, double phi) {
    double za = fabs(z);
    double tt = fmod(phi, twopi) * inv_halfpi; /* in [0,4) */

    if (za <= twothird) { /* Equatorial region */
        double temp1 = nside * (0.5 + tt);
        double temp2 = nside * z * 0.75;
        int64_t jp = (int64_t)(temp1 - temp2); /* index of ascending edge line */
        int64_t jm = (int64_t)(temp1 + temp2); /* index of descending edge line */

        /* ring number counted from z=2/3 */
        int64_t ir = nside + 1 + jp - jm; /* in {1, 2n + 1} */
        int kshift = 1 - (ir & 1); /* kshift=1 if ir even, 0 otherwise */

        int64_t ip = (jp + jm - nside + kshift + 1) / 2; /* in {0, 4n - 1} */
        ip = (ip % (4 * nside) + 4 * nside) % (4 * nside);  // Ensure ip is non-negative

        return nside * (nside - 1) * 2 + (ir - 1) * 4 * nside + ip;
    } else { /* North & South polar caps */
        double tp = tt - (int)tt;
        double tmp = nside * sqrt(3 * (1 - za));

        int64_t jp = (int64_t)(tp * tmp); /* increasing edge line index */
        int64_t jm = (int64_t)((1.0 - tp) * tmp); /* decreasing edge line index */

        int64_t ir = jp + jm + 1; /* ring number counted from the closest pole */
        int64_t ip = (int64_t)(tt * ir); /* in {0, 4*ir - 1} */
        ip = (ip % (4 * ir) + 4 * ir) % (4 * ir);  // Ensure ip is non-negative

        if (z > 0) {
            return 2 * ir * (ir - 1) + ip;
        } else {
            return 12 * nside * nside - 2 * ir * (ir + 1) + ip;
        }
    }
}

// Gateway function for the MEX interface with input (nside, long, lat arrays)
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("ang2pix_ring_mex:nrhs", "Three inputs required: nside, long, lat");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("ang2pix_ring_mex:nlhs", "One output required.");
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
        mexErrMsgIdAndTxt("ang2pix_ring_mex:inputSize", "Longitude and latitude arrays must be of the same size.");
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

        // Calculate the ring index
        output[i] = ang2pix_ring_z_phi64(nside, z, phi);
    }
}

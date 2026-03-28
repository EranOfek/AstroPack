// mex coneSearch.cpp -I/home/kra/ExternalLib/Healpix_3.83/src/cxx/Healpix_cxx \
//                    -I/home/kra/ExternalLib/Healpix_3.83/src/cxx/cxxsupport \
//                    -L/home/kra/ExternalLib/Healpix_3.83/lib /home/kra/ExternalLib/Healpix_3.83/lib/libhealpix_cxx.a \
//                    -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
// important: the Healpix library must be compiled with the -fPIX option

#include "mex.h"
#include "healpix_base.h"
#include "pointing.h"
#include "rangeset.h"
#include <vector>
#include <cmath>
#include <cstring>

void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{
    if (nrhs < 4)
        mexErrMsgTxt("Usage: pix = celestial.healpix.mex.coneSearch(nside, ra, dec, radius_deg, [scheme])");

    // Inputs
    int64_t nside = (int64_t) mxGetScalar(prhs[0]);
    double ra  = mxGetScalar(prhs[1]);
    double dec = mxGetScalar(prhs[2]);
    double radius_deg = mxGetScalar(prhs[3]);
    
    Healpix_Ordering_Scheme scheme = NEST;
    if (nrhs >= 5) {
        char* str = mxArrayToString(prhs[4]);
        if (strcmp(str, "RING") == 0)
            scheme = RING;
        mxFree(str);
    }

    // Convert coordinates
    double theta = (90.0 - dec) * M_PI / 180.0;
    double phi   = ra * M_PI / 180.0;
    double radius = radius_deg * M_PI / 180.0;

    pointing center(theta, phi);

    // HEALPix object       
    Healpix_Base2 hp;
    hp.SetNside(nside, scheme);

    // Query
    std::vector<int64> pixels;
    hp.query_disc_inclusive(center, radius, pixels);

    // Output MATLAB array
    mwSize npix = pixels.size();
    plhs[0] = mxCreateNumericMatrix(npix, 1, mxINT64_CLASS, mxREAL);
    int64_t* out = (int64_t*) mxGetData(plhs[0]);

    for (mwSize i = 0; i < npix; i++)
        out[i] = pixels[i];
}

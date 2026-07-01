// mex rasterize_polygon.cpp -I/home/sasha/ExternalLib/Healpix_3.83/src/cxx/Healpix_cxx \
//                    -I/home/sasha/ExternalLib/Healpix_3.83/src/cxx/cxxsupport \
//                    -L/home/sasha/ExternalLib/Healpix_3.83/lib /home/sasha/ExternalLib/Healpix_3.83/lib/libhealpix_cxx.a \
//                    -lstdc++ CXXFLAGS="\$CXXFLAGS -std=c++11"
// important: the Healpix library must be compiled with the -fPIX option

#include "mex.h"
#include "healpix_base.h"
#include "pointing.h"
#include "rangeset.h"
#include <vector>
#include <cmath>
#include <cstring>
#include <strings.h>   // strcasecmp

// --- helper: nearest power of 2 ---
int64_t nearest_pow2(double x)
{
    int64_t p = 1;
    while (p < x) p <<= 1;
    int64_t prev = p >> 1;
    return (fabs(prev - x) < fabs(p - x)) ? prev : p;
}

void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{
    if (nrhs < 2)
        mexErrMsgTxt(
            "Usage: pix = celestial.healpix.mex.rasterize_polygon(P, value, [mode], [scheme]); \n"
            "or [pix, nside] = celestial.healpix.mex.rasterize_polygon(P, value, [mode], [scheme]) \n"
            "  value  : Nside (default) or resolution in arcsec, selected by mode \n"
            "  mode   : 'Nside' (default) or 'arcsec' \n"
            "  scheme : 'NEST' (default) or 'RING'"
        );

    // --- Input polygon ---
    
    if (!mxIsDouble(prhs[0])) {
    mexErrMsgTxt("Input polygon must be of type double");
    }
    
    const mxArray* P = prhs[0];
   
    if (mxGetN(P) != 2)
        mexErrMsgTxt("P must be an Nx2 matrix: [RA, Dec]");

    mwSize N = mxGetM(P);
    if (N < 3)
        mexErrMsgTxt("Polygon must have at least 3 vertices");

    double* data = mxGetPr(P);

    // define the radian
    double rad = 180.0/M_PI;
    // --- Second argument: Nside or resolution in arcsec (selected by mode) ---
    double value = mxGetScalar(prhs[1]);

    // --- Mode (optional, def. 'Nside') ---
    bool isArcsec = false;   // false -> value is Nside, true -> value is arcsec
    if (nrhs >= 3) {
        char* mstr = mxArrayToString(prhs[2]);
        if (strcasecmp(mstr, "arcsec")     == 0 ||
            strcasecmp(mstr, "arcsecond")  == 0 ||
            strcasecmp(mstr, "arcseconds") == 0 ||
            strcasecmp(mstr, "res")        == 0 ||
            strcasecmp(mstr, "resolution") == 0) {
            isArcsec = true;
        } else if (strcasecmp(mstr, "nside") == 0) {
            isArcsec = false;
        } else {
            mxFree(mstr);
            mexErrMsgTxt("Mode (3rd argument) must be 'Nside' or 'arcsec'");
        }
        mxFree(mstr);
    }

    // --- Scheme (optional, def. NESTED) ---
    Healpix_Ordering_Scheme scheme = NEST;
    if (nrhs >= 4) {
        char* str = mxArrayToString(prhs[3]);
        if (strcmp(str, "RING") == 0)
            scheme = RING;
        mxFree(str);
    }

    // --- Determine NSIDE ---
    int64_t nside;
    if (isArcsec) {
        // value is an angular resolution in arcsec -> compute the matching Nside
        if (value <= 0)
            mexErrMsgTxt("Resolution must be positive (arcsec)");
        double res_deg   = value / 3600.0;
        double nside_est = 58.63 / res_deg;
        nside = nearest_pow2(nside_est);
    } else {
        // value is Nside, given directly -> must be a positive power of 2
        int64_t nside_in = (int64_t) value;
        if ((double) nside_in != value || nside_in < 1 ||
            (nside_in & (nside_in - 1)) != 0)
            mexErrMsgTxt("Nside must be a positive power of 2");
        nside = nside_in;
    }

    // --- Convert polygon ---
    std::vector<pointing> verts;
    verts.reserve(N);

    for (mwSize i = 0; i < N; i++) {
        double ra  = data[i];
        double dec = data[i + N];

        double theta = (90.0 - dec) / rad;
        double phi   = ra / rad;

        verts.emplace_back(theta, phi);
    }

    // --- HEALPix object ---
    Healpix_Base2 hp;
    hp.SetNside(nside, scheme);

    // --- Query polygon ---
    rangeset<int64> pixset = hp.query_polygon(verts);

    std::vector<int64> pixels;
    pixset.toVector(pixels);

    mwSize npix = pixels.size();

    // --- Output pixel indices ---
    plhs[0] = mxCreateNumericMatrix(npix, 1, mxINT64_CLASS, mxREAL);
    int64_t* out = (int64_t*) mxGetData(plhs[0]);

    for (mwSize i = 0; i < npix; i++)
        out[i] = pixels[i];

    // --- Optional output: NSIDE ---
    if (nlhs >= 2) {
        plhs[1] = mxCreateNumericMatrix(1, 1, mxINT64_CLASS, mxREAL);
        *((int64_t*) mxGetData(plhs[1])) = nside;
    }
}

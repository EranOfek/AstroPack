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
            "Usage: pix = celestial.healpix.mex.rasterize_polygon(P, res_arcsec, [scheme]); \n"
            "or [pix, nside] = celestial.healpix.mex.rasterize_polygon(P, res_arcsec, [scheme])"
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
    // --- Resolution (arcsec) ---
    double res_arcsec = mxGetScalar(prhs[1]);
    if (res_arcsec <= 0)
        mexErrMsgTxt("Resolution must be positive (arcsec)");

    // --- Scheme ---
    Healpix_Ordering_Scheme scheme = NEST;
    if (nrhs >= 3) {
        char* str = mxArrayToString(prhs[2]);
        if (strcmp(str, "RING") == 0)
            scheme = RING;
        mxFree(str);
    }

    // --- Compute NSIDE ---
    double res_deg = res_arcsec / 3600.0;
    double nside_est = 58.63 / res_deg;
    int64_t nside = nearest_pow2(nside_est);

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

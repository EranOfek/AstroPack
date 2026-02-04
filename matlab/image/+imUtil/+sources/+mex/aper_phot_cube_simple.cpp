// aper_phot_cube_simple.cpp
// Hard-mask aperture photometry on stamp cube, with sorted radii (small->large).
//
// Compile:
// mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" aper_phot_cube_simple.cpp
//
// USAGE:
//   [Flux, Area] = aper_phot_cube_simple(Cube, Back, X1, Y1, AperR)
//
// INPUTS:
//   Cube  : MxK or MxKxN, real single/double, background INCLUDED.
//   Back  : scalar or length-N vector (single/double). Background per slice.
//   X1,Y1 : scalar or length-N vector (single/double).
//           Offsets RELATIVE TO STAMP CENTER (j/i directions respectively).
//   AperR : vector of aperture radii [pixels], MUST be sorted ascending.
//
// OUTPUTS (double):
//   Flux : N x NA. Flux(n,a) = sum_{r<=AperR(a)} Cube - Back(n)*Area.
//   Area : N x NA. Number of pixel-centers included in each aperture.
//
// Mask uses pixel centers with MATLAB 1-based coordinates:
//   cx = (K+1)/2 + X1(n),  cy = (M+1)/2 + Y1(n)
// Include pixel if (x-cx)^2+(y-cy)^2 <= R^2.
//
// Performance:
//   - Loops only over bounding box of largest radius.
//   - Per pixel: find first aperture that includes it (linear scan; NA small) and
//     update ONE bin; prefix-sum builds all apertures.

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include <algorithm>
#include <cstdio>

#if defined(_OPENMP)
  #include <omp.h>
#endif

static void die(const char* msg) {
    mexErrMsgIdAndTxt("aper_phot_cube_simple:err", "%s", msg);
}

static inline bool isRealSingleOrDouble(const mxArray* A) {
    return (mxIsSingle(A) || mxIsDouble(A)) && !mxIsComplex(A);
}

static void read_vec_double_allow_scalar(const mxArray* A, mwSize N, std::vector<double>& out, const char* name) {
    if (!A) die("Missing required input.");
    if (!mxIsNumeric(A) || mxIsComplex(A)) die("Back/X1/Y1 must be real numeric.");
    const mwSize nEl = mxGetNumberOfElements(A);
    if (!(nEl == 1 || nEl == N)) {
        char buf[256];
        std::snprintf(buf, sizeof(buf), "%s must be scalar or length N.", name);
        die(buf);
    }
    out.resize((size_t)N);

    if (mxIsDouble(A)) {
        const double* p = (const double*)mxGetData(A);
        if (nEl == 1) {
            const double v = p[0];
            for (mwSize i=0;i<N;++i) out[i]=v;
        } else {
            std::copy(p, p+N, out.begin());
        }
    } else {
        const float* p = (const float*)mxGetData(A);
        if (nEl == 1) {
            const double v = (double)p[0];
            for (mwSize i=0;i<N;++i) out[i]=v;
        } else {
            for (mwSize i=0;i<N;++i) out[i]=(double)p[i];
        }
    }
}

static void read_radii_sorted_assumed(const mxArray* RA, std::vector<double>& R2) {
    if (!RA) die("AperR is required.");
    if (!isRealSingleOrDouble(RA)) die("AperR must be real single/double.");
    const mwSize NA = mxGetNumberOfElements(RA);
    if (NA < 1) die("AperR must be non-empty.");

    R2.resize((size_t)NA);

    if (mxIsDouble(RA)) {
        const double* p = (const double*)mxGetData(RA);
        double prev = -1.0;
        for (mwSize a=0; a<NA; ++a) {
            const double R = p[a];
            if (!mxIsFinite(R) || R < 0.0) die("All AperR must be finite and >= 0.");
            if (a>0 && R < prev) die("AperR must be sorted ascending (small to large).");
            prev = R;
            R2[a] = R*R;
        }
    } else {
        const float* p = (const float*)mxGetData(RA);
        double prev = -1.0;
        for (mwSize a=0; a<NA; ++a) {
            const double R = (double)p[a];
            if (!mxIsFinite(R) || R < 0.0) die("All AperR must be finite and >= 0.");
            if (a>0 && R < prev) die("AperR must be sorted ascending (small to large).");
            prev = R;
            R2[a] = R*R;
        }
    }
}

template <typename T>
static void aperphot_cube_fast_sortedR(
    const T* cube, mwSize M, mwSize K, mwSize N,
    const std::vector<double>& Back,
    const std::vector<double>& X1,
    const std::vector<double>& Y1,
    const std::vector<double>& R2,  // sorted ascending
    double* outFlux, double* outArea
) {
    const mwSize stride = M*K;
    const mwSize NA = (mwSize)R2.size();
    const double R2max = R2[NA-1];
    const double Rmax  = std::sqrt(R2max);

#if defined(_OPENMP)
    #pragma omp parallel
#endif
    {
        // Per-thread bins: incSum[a0] accumulates pixels whose minimal containing aperture is a0.
        std::vector<double> incSum(NA);
        std::vector<double> incCnt(NA);

#if defined(_OPENMP)
        #pragma omp for schedule(static)
#endif
        for (mwSize n=0; n<N; ++n) {
            const double B  = Back[n];
            const double x1 = X1[n];
            const double y1 = Y1[n];

            if (!mxIsFinite(B) || !mxIsFinite(x1) || !mxIsFinite(y1)) {
                for (mwSize a=0; a<NA; ++a) {
                    outFlux[n + N*a] = mxGetNaN();
                    outArea[n + N*a] = mxGetNaN();
                }
                continue;
            }

            // center (MATLAB 1-based pixel centers)
            const double cx = 0.5 * ((double)K + 1.0) + x1;
            const double cy = 0.5 * ((double)M + 1.0) + y1;

            // bounding box for the largest aperture
            // x,y are 1..K / 1..M in MATLAB coords
            int xMin = (int)std::ceil(cx - Rmax);
            int xMax = (int)std::floor(cx + Rmax);
            int yMin = (int)std::ceil(cy - Rmax);
            int yMax = (int)std::floor(cy + Rmax);

            if (xMin < 1) xMin = 1;
            if (yMin < 1) yMin = 1;
            if (xMax > (int)K) xMax = (int)K;
            if (yMax > (int)M) yMax = (int)M;

            std::fill(incSum.begin(), incSum.end(), 0.0);
            std::fill(incCnt.begin(), incCnt.end(), 0.0);

            const T* img = cube + n*stride;

            // Column-major friendly: loop x then y (y contiguous).
            for (int xi = xMin; xi <= xMax; ++xi) {
                const double dx  = (double)xi - cx;
                const double dx2 = dx*dx;
                const mwSize base = (mwSize)M * (mwSize)(xi - 1);

                for (int yi = yMin; yi <= yMax; ++yi) {
                    const double dy = (double)yi - cy;
                    const double r2 = dx2 + dy*dy;
                    if (r2 > R2max) continue;

                    // Find smallest aperture index a0 such that r2 <= R2[a0]
                    // NA is small (typ 1..6) => linear scan is optimal.
                    mwSize a0 = 0;
                    while (a0 < NA && r2 > R2[a0]) ++a0;
                    if (a0 == NA) continue; // should not happen since r2<=R2max, but safe.

                    const double I = (double)img[base + (mwSize)(yi - 1)];
                    incSum[a0] += I;
                    incCnt[a0] += 1.0;
                }
            }

            // Prefix-sum to build totals for each aperture:
            // Pixels assigned to a0 contribute to apertures a>=a0.
            double runSum = 0.0;
            double runCnt = 0.0;
            for (mwSize a=0; a<NA; ++a) {
                runSum += incSum[a];
                runCnt += incCnt[a];
                outArea[n + N*a] = runCnt;
                outFlux[n + N*a] = runSum - B*runCnt;
            }
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 5) die("Usage: [Flux,Area] = aper_phot_cube_simple(Cube, Back, X1, Y1, AperR)");
    if (nlhs < 1 || nlhs > 2) die("One or two outputs: [Flux, Area].");

    const mxArray* CubeA = prhs[0];
    const mxArray* BackA = prhs[1];
    const mxArray* X1A   = prhs[2];
    const mxArray* Y1A   = prhs[3];
    const mxArray* RA    = prhs[4];

    if (!isRealSingleOrDouble(CubeA)) die("Cube must be real single or double.");

    const mwSize nd = mxGetNumberOfDimensions(CubeA);
    if (nd != 2 && nd != 3) die("Cube must be MxK or MxKxN.");

    const mwSize* dims = mxGetDimensions(CubeA);
    const mwSize M = dims[0];
    const mwSize K = dims[1];
    const mwSize N = (nd == 3) ? dims[2] : 1;

    std::vector<double> Back, X1, Y1;
    read_vec_double_allow_scalar(BackA, N, Back, "Back");
    read_vec_double_allow_scalar(X1A,   N, X1,   "X1");
    read_vec_double_allow_scalar(Y1A,   N, Y1,   "Y1");

    std::vector<double> R2;
    read_radii_sorted_assumed(RA, R2);
    const mwSize NA = (mwSize)R2.size();

    plhs[0] = mxCreateDoubleMatrix(N, NA, mxREAL);
    double* outFlux = (double*)mxGetData(plhs[0]);

    double* outArea = nullptr;
    std::vector<double> dummyArea;
    if (nlhs == 2) {
        plhs[1] = mxCreateDoubleMatrix(N, NA, mxREAL);
        outArea = (double*)mxGetData(plhs[1]);
    } else {
        dummyArea.resize((size_t)N*(size_t)NA);
        outArea = dummyArea.data();
    }

    if (mxIsDouble(CubeA)) {
        const double* cube = (const double*)mxGetData(CubeA);
        aperphot_cube_fast_sortedR<double>(cube, M, K, N, Back, X1, Y1, R2, outFlux, outArea);
    } else {
        const float* cube = (const float*)mxGetData(CubeA);
        aperphot_cube_fast_sortedR<float>(cube, M, K, N, Back, X1, Y1, R2, outFlux, outArea);
    }
}

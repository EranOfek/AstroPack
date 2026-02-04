// mom2_cube.cpp
// Input: Cube (includes background), Back (per-slice), X1, Y1, MaxRadius (if NaN, use all)
// Output: [X2,Y2,XY] (double)
// mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" mom2_cube.cpp

#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>

#if defined(_OPENMP)
  #include <omp.h>
#endif

static void die(const char* msg) {
    mexErrMsgIdAndTxt("mom2_cube:err", "%s", msg);
}

static inline bool isRealSingleOrDouble(const mxArray* A) {
    return (mxIsSingle(A) || mxIsDouble(A)) && !mxIsComplex(A);
}

static inline void print_help() {
    mexPrintf(
"mom2_cube  Responsibility-weighted 2nd central moments per slice (no external weight).\n"
"\n"
"USAGE:\n"
"  [X2,Y2,XY] = mom2_cube(Cube, Back, X1, Y1, MaxRadius)\n"
"\n"
"INPUTS:\n"
"  Cube      : MxK or MxKxN, real single/double. MUST include background.\n"
"  Back      : scalar or length-N vector (single/double), background level per slice (>=0).\n"
"  X1, Y1    : centers (MATLAB 1-based). Scalars or length-N vectors. Any real numeric.\n"
"  MaxRadius : scalar. If finite, include only pixels within radius around (X1,Y1).\n"
"              If NaN, include all pixels.\n"
"\n"
"OUTPUTS (double, N x 1):\n"
"  X2 = sum(s*dx^2) / sum(s)\n"
"  Y2 = sum(s*dy^2) / sum(s)\n"
"  XY = sum(s*dx*dy) / sum(s)\n"
"  where dx=(x-X1), dy=(y-Y1) and s is an 'effective source count' per pixel.\n"
"\n"
"RESPONSIBILITY-WEIGHTED SCHEME (no PSF model, no iteration):\n"
"  For observed pixel value n and background B (same units):\n"
"    r = max(1 - B/n, 0) for n>0, else r=0\n"
"    s = n*r = max(n - B, 0)\n"
"  This is a fast soft-assignment of counts to 'source' vs 'background' that avoids\n"
"  negative weights / cancellation from direct background subtraction.\n"
"\n"
"NOTES:\n"
"  - NaN/Inf pixels in Cube are ignored.\n"
"  - If sum(s)==0 (no pixels with n>B within radius), outputs are NaN for that slice.\n"
"  - MaxRadius is applied around the provided (X1,Y1) for EACH slice.\n"
"  - OpenMP parallel over slices.\n"
"\n"
    );
}

static void read_vec_to_double(const mxArray* A, mwSize N, std::vector<double>& out,
                              const char* name, bool allowScalar, bool requireFinite, bool nonneg)
{
    if (!A) mexErrMsgIdAndTxt("mom2_cube:err", "%s is required.", name);
    if (!mxIsNumeric(A) || mxIsComplex(A)) mexErrMsgIdAndTxt("mom2_cube:err", "%s must be real numeric.", name);

    const mwSize nEl = mxGetNumberOfElements(A);
    if (allowScalar) {
        if (!(nEl == 1 || nEl == N))
            mexErrMsgIdAndTxt("mom2_cube:err", "%s must be scalar or length N.", name);
    } else {
        if (nEl != N)
            mexErrMsgIdAndTxt("mom2_cube:err", "%s must be length N.", name);
    }

    out.resize((size_t)N);

    if (mxIsDouble(A)) {
        const double* p = (const double*)mxGetData(A);
        if (nEl == 1) {
            for (mwSize i=0;i<N;++i) out[i] = p[0];
        } else {
            std::copy(p, p+N, out.begin());
        }
    } else {
        const float* p = (const float*)mxGetData(A);
        if (nEl == 1) {
            const double v = (double)p[0];
            for (mwSize i=0;i<N;++i) out[i] = v;
        } else {
            for (mwSize i=0;i<N;++i) out[i] = (double)p[i];
        }
    }

    if (requireFinite || nonneg) {
        for (mwSize i=0;i<N;++i) {
            const double v = out[i];
            if (requireFinite && !mxIsFinite(v))
                mexErrMsgIdAndTxt("mom2_cube:err", "%s must be finite.", name);
            if (nonneg && v < 0.0)
                mexErrMsgIdAndTxt("mom2_cube:err", "%s must be >= 0.", name);
        }
    }
}

static inline double read_scalar_double(const mxArray* A, const char* name) {
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1)
        mexErrMsgIdAndTxt("mom2_cube:err", "%s must be a numeric scalar.", name);
    return mxGetScalar(A);
}

template <typename T>
static void compute_mom2_resp(
    const T* cube, mwSize M, mwSize K, mwSize N,
    const double* Back,
    const double* Xc, const double* Yc,
    bool useAll, double R2,
    double* outX2, double* outY2, double* outXY
){
    const mwSize stride = M * K;

#if defined(_OPENMP)
    #pragma omp parallel for schedule(static)
#endif
    for (mwSize n = 0; n < N; ++n) {
        const T* img = cube + n * stride;
        const double B = Back[n];
        const double X = Xc[n];
        const double Y = Yc[n];

        if (!mxIsFinite(X) || !mxIsFinite(Y) || !mxIsFinite(B) || B < 0.0) {
            outX2[n] = mxGetNaN();
            outY2[n] = mxGetNaN();
            outXY[n] = mxGetNaN();
            continue;
        }

        double sumS  = 0.0;
        double sumX2 = 0.0;
        double sumY2 = 0.0;
        double sumXY = 0.0;

        // Column-major friendly: y contiguous
        for (mwSize x = 0; x < K; ++x) {
            const double dx  = (double)(x + 1) - X;
            const double dx2 = dx * dx;
            const mwSize base = M * x;

            for (mwSize y = 0; y < M; ++y) {
                const double dy = (double)(y + 1) - Y;

                if (!useAll) {
                    const double r2 = dx2 + dy * dy;
                    if (r2 > R2) continue;
                }

                const double nobs = (double)img[base + y];
                if (!std::isfinite(nobs)) continue;

                // Responsibility-weighted effective source counts:
                // r = max(1 - B/n, 0) for n>0 else 0  => s = n*r = max(n-B, 0)
                if (nobs <= B) continue;       // s=0 (also covers nobs<=0 when B>=0)
                const double s = nobs - B;     // equivalent to nobs * max(1 - B/nobs, 0)

                sumS  += s;
                sumX2 += s * dx2;
                const double dy2 = dy * dy;
                sumY2 += s * dy2;
                sumXY += s * dx * dy;
            }
        }

        if (!(sumS > 0.0) || !mxIsFinite(sumS)) {
            outX2[n] = mxGetNaN();
            outY2[n] = mxGetNaN();
            outXY[n] = mxGetNaN();
        } else {
            const double inv = 1.0 / sumS;
            outX2[n] = sumX2 * inv;
            outY2[n] = sumY2 * inv;
            outXY[n] = sumXY * inv;
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs == 0) { print_help(); return; }

    if (nrhs != 5) die("Usage: [X2,Y2,XY] = mom2_cube(Cube, Back, X1, Y1, MaxRadius)");
    if (nlhs != 3) die("Require three outputs: [X2, Y2, XY].");

    const mxArray* CubeA = prhs[0];
    const mxArray* BackA = prhs[1];
    const mxArray* XA    = prhs[2];
    const mxArray* YA    = prhs[3];
    const mxArray* RA    = prhs[4];

    if (!isRealSingleOrDouble(CubeA)) die("Cube must be real single or double.");

    const mwSize nd = mxGetNumberOfDimensions(CubeA);
    if (nd != 2 && nd != 3) die("Cube must be 2-D (M x K) or 3-D (M x K x N).");

    const mwSize* dims = mxGetDimensions(CubeA);
    const mwSize M = dims[0];
    const mwSize K = dims[1];
    const mwSize N = (nd == 3) ? dims[2] : 1;

    // Read Back, X, Y (scalar or length N)
    std::vector<double> Back, Xc, Yc;
    read_vec_to_double(BackA, N, Back, "Back", true, true, true); // finite, nonneg
    read_vec_to_double(XA,    N, Xc,   "X1",   true, false, false);
    read_vec_to_double(YA,    N, Yc,   "Y1",   true, false, false);

    // Radius
    const double R = read_scalar_double(RA, "MaxRadius");
    bool useAll = false;
    double R2 = 0.0;

    if (std::isnan(R)) {
        useAll = true;
    } else {
        if (!mxIsFinite(R) || R < 0.0) die("MaxRadius must be NaN or a finite scalar >= 0.");
        useAll = false;
        R2 = R * R;
    }

    // Outputs
    plhs[0] = mxCreateDoubleMatrix(N, 1, mxREAL);
    plhs[1] = mxCreateDoubleMatrix(N, 1, mxREAL);
    plhs[2] = mxCreateDoubleMatrix(N, 1, mxREAL);

    double* outX2 = (double*)mxGetData(plhs[0]);
    double* outY2 = (double*)mxGetData(plhs[1]);
    double* outXY = (double*)mxGetData(plhs[2]);

    if (mxIsDouble(CubeA)) {
        const double* cube = (const double*)mxGetData(CubeA);
        compute_mom2_resp<double>(cube, M, K, N, Back.data(), Xc.data(), Yc.data(), useAll, R2, outX2, outY2, outXY);
    } else {
        const float* cube = (const float*)mxGetData(CubeA);
        compute_mom2_resp<float>(cube, M, K, N, Back.data(), Xc.data(), Yc.data(), useAll, R2, outX2, outY2, outXY);
    }
}

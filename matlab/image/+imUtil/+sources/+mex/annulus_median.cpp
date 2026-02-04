#include "mex.h"
#include <vector>
#include <cmath>
#include <algorithm>
#include <cstdint>

#if defined(_OPENMP)
  #include <omp.h>
#endif

static void die(const char* msg) {
    mexErrMsgIdAndTxt("annulus_sub_median:err", "%s", msg);
}

static inline bool isRealSingleOrDouble(const mxArray* A) {
    return (mxIsSingle(A) || mxIsDouble(A)) && !mxIsComplex(A);
}

static inline bool parseBoolScalar(const mxArray* A) {
    if (!A) return false;
    if (mxIsLogicalScalar(A)) return mxIsLogicalScalarTrue(A);
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1)
        die("Third argument must be logical or numeric scalar (0/1).");
    return mxGetScalar(A) != 0.0;
}

static void print_help() {
    mexPrintf(
"annulus_sub_median  Subtract per-slice annulus median background from image cube.\n"
"\n"
"USAGE:\n"
"  [OutCube, Bg, Std, Npix] = annulus_sub_median(Cube, Annulus, UseStdErr)\n"
"\n"
"INPUTS:\n"
"  Cube     : MxK or MxKxN, real single or double.\n"
"  Annulus  :\n"
"     - 2-element vector [Rin Rout] (single/double): inner/outer radius in pixels.\n"
"     - scalar W (single/double): annulus width. Then Rout is max possible radius\n"
"       from image center; Rin = max(0, Rout - W).\n"
"     Radii are measured from the image center using pixel-center coordinates\n"
"     in MATLAB 1-based indexing: center = ((K+1)/2, (M+1)/2).\n"
"  UseStdErr: logical or 0/1 scalar.\n"
"     - 0: Std is the sample standard deviation in the annulus.\n"
"     - 1: Std is divided by sqrt(Npix-1) (as requested).\n"
"\n"
"OUTPUTS:\n"
"  OutCube : same size/class as Cube. For each slice, subtract Bg(slice) from\n"
"            every pixel.\n"
"  Bg      : N x 1 double. Annulus median per slice.\n"
"  Std     : N x 1 double. Annulus sample std per slice (or scaled by sqrt(Npix-1)).\n"
"  Npix    : scalar double. Number of pixels in the annulus mask (same for all slices).\n"
"\n"
"NOTES / IMPLEMENTATION DETAILS (speed):\n"
"  - The annulus mask (linear indices into a slice) is precomputed once.\n"
"  - Median is computed using std::nth_element (O(Npix)). For even Npix, we\n"
"    average the two middle elements.\n"
"  - Std is computed with Welford's one-pass algorithm on the *finite* annulus\n"
"    samples (NaNs in annulus are ignored).\n"
"  - NaNs in Cube are preserved in OutCube (NaN - Bg = NaN).\n"
"  - Parallelization (if compiled with -fopenmp) is over slices.\n"
"\n"
    );
}

static inline void build_annulus_indices(
    mwSize M, mwSize K,
    double rin, double rout,
    std::vector<mwIndex>& idx
) {
    const double cx = 0.5 * ((double)K + 1.0); // 1-based
    const double cy = 0.5 * ((double)M + 1.0); // 1-based
    const double rin2  = rin  * rin;
    const double rout2 = rout * rout;

    idx.clear();
    idx.reserve((size_t)M * (size_t)K);

    for (mwSize x = 0; x < K; ++x) {
        const double dx = (double)(x + 1) - cx;
        const double dx2 = dx * dx;
        for (mwSize y = 0; y < M; ++y) {
            const double dy = (double)(y + 1) - cy;
            const double r2 = dx2 + dy * dy;
            if (r2 >= rin2 && r2 <= rout2) {
                // MATLAB column-major linear index within a slice:
                // lin = y + M*x
                idx.push_back((mwIndex)(y + M * x));
            }
        }
    }
}

// median from buffer[0..n-1], modifies buffer order
static inline double median_inplace(std::vector<double>& buf, size_t n) {
    if (n == 0) return mxGetNaN();
    const size_t mid = n / 2;

    std::nth_element(buf.begin(), buf.begin() + (ptrdiff_t)mid, buf.begin() + (ptrdiff_t)n);
    double m2 = buf[mid];

    if (n & 1) return m2; // odd
    // even: need lower middle too
    std::nth_element(buf.begin(), buf.begin() + (ptrdiff_t)(mid - 1), buf.begin() + (ptrdiff_t)mid);
    double m1 = buf[mid - 1];
    return 0.5 * (m1 + m2);
}

template <typename Tin, typename Tout>
static void process_cube(
    const Tin* in, Tout* out,
    mwSize M, mwSize K, mwSize N,
    const std::vector<mwIndex>& annIdx,
    bool useStdErr,
    double* Bg, double* Std
) {
    const mwSize sliceStride = M * K;
    const size_t Nmask = annIdx.size();
    const double NaN = mxGetNaN();

#if defined(_OPENMP)
    #pragma omp parallel
#endif
    {
        std::vector<double> buf;
        buf.resize(Nmask);

#if defined(_OPENMP)
        #pragma omp for schedule(static)
#endif
        for (mwSize n = 0; n < N; ++n) {
            const Tin* pin  = in  + n * sliceStride;
            Tout* pout      = out + n * sliceStride;

            // gather finite annulus pixels into buf[0..nf-1]
            size_t nf = 0;

            // Welford accumulators over finite values
            double mean = 0.0;
            double M2 = 0.0;

            for (size_t j = 0; j < Nmask; ++j) {
                const double v = (double)pin[annIdx[j]];
                if (!std::isfinite(v)) continue; // ignore NaN/Inf in annulus stats
                buf[nf++] = v;

                // Welford
                const double delta = v - mean;
                mean += delta / (double)nf;
                const double delta2 = v - mean;
                M2 += delta * delta2;
            }

            double bg = NaN;
            double sd = NaN;

            if (nf > 0) {
                bg = median_inplace(buf, nf);
                if (nf >= 2) {
                    sd = std::sqrt(M2 / (double)(nf - 1)); // sample std over finite values
                    if (useStdErr) {
                        // per request: divide by sqrt(Npix-1). They asked Npix, but NaNs are excluded.
                        // We use nf (finite count) to avoid dividing by sqrt of a count that includes NaNs.
                        sd /= std::sqrt((double)(nf - 1));
                    }
                }
            }

            Bg[n] = bg;
            Std[n] = sd;

            // subtract bg from whole slice (preserve NaNs: NaN - bg => NaN)
            // If bg is NaN, output becomes NaN everywhere (consistent with arithmetic).
            for (mwSize i = 0; i < sliceStride; ++i) {
                const double v = (double)pin[i];
                const double w = v - bg;
                pout[i] = (Tout)w;
            }
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs == 0) { print_help(); return; }

    if (nrhs != 3)
        die("Usage: [OutCube,Bg,Std,Npix]=annulus_sub_median(Cube, Annulus, UseStdErr)");
    if (nlhs != 4)
        die("Require four outputs: [OutCube, Bg, Std, Npix].");

    const mxArray* CubeA = prhs[0];
    const mxArray* AnnA  = prhs[1];
    const mxArray* UseA  = prhs[2];

    if (!isRealSingleOrDouble(CubeA)) die("Cube must be real single or double.");

    const mwSize nd = mxGetNumberOfDimensions(CubeA);
    if (nd != 2 && nd != 3) die("Cube must be 2-D (M x K) or 3-D (M x K x N).");

    const mwSize* dims = mxGetDimensions(CubeA);
    const mwSize M = dims[0];
    const mwSize K = dims[1];
    const mwSize N = (nd == 3) ? dims[2] : 1;

    if (!isRealSingleOrDouble(AnnA)) die("Annulus must be real single/double.");
    const mwSize nAnn = mxGetNumberOfElements(AnnA);
    if (!(nAnn == 1 || nAnn == 2)) die("Annulus must be scalar (width) or 2-element [Rin Rout].");

    // parse annulus radii
    double rin = 0.0, rout = 0.0;

    auto getAnnVal = [&](mwSize i)->double{
        if (mxIsDouble(AnnA)) return ((const double*)mxGetData(AnnA))[i];
        return (double)((const float*)mxGetData(AnnA))[i];
    };

    // compute maximum possible radius from center to any pixel center
    const double cx = 0.5 * ((double)K + 1.0);
    const double cy = 0.5 * ((double)M + 1.0);
    const double corners[4][2] = {
        {1.0, 1.0},
        {(double)K, 1.0},
        {1.0, (double)M},
        {(double)K, (double)M}
    };
    double rmax2 = 0.0;
    for (int t = 0; t < 4; ++t) {
        const double dx = corners[t][0] - cx;
        const double dy = corners[t][1] - cy;
        rmax2 = std::max(rmax2, dx*dx + dy*dy);
    }
    const double rmax = std::sqrt(rmax2);

    if (nAnn == 2) {
        rin = getAnnVal(0);
        rout = getAnnVal(1);
        if (!(mxIsFinite(rin) && mxIsFinite(rout))) die("Annulus radii must be finite.");
        if (rin < 0.0 || rout <= 0.0 || rout <= rin) die("Require 0 <= Rin < Rout.");
    } else {
        const double w = getAnnVal(0);
        if (!(mxIsFinite(w) && w > 0.0)) die("Annulus width must be finite and > 0.");
        rout = rmax;
        rin = rout - w;
        if (rin < 0.0) rin = 0.0;
    }

    const bool useStdErr = parseBoolScalar(UseA);

    // precompute annulus indices
    std::vector<mwIndex> annIdx;
    build_annulus_indices(M, K, rin, rout, annIdx);

    const mwSize Npix = (mwSize)annIdx.size();
    if (Npix == 0) die("Annulus mask is empty (no pixels). Check radii/width.");

    // outputs
    // OutCube: same size and class as input
    const mxClassID cid = mxGetClassID(CubeA);
    if (nd == 2) {
        plhs[0] = mxCreateNumericMatrix(M, K, cid, mxREAL);
    } else {
        plhs[0] = mxCreateNumericArray(3, dims, cid, mxREAL);
    }

    plhs[1] = mxCreateDoubleMatrix(N, 1, mxREAL); // Bg
    plhs[2] = mxCreateDoubleMatrix(N, 1, mxREAL); // Std
    plhs[3] = mxCreateDoubleScalar((double)Npix);

    double* Bg  = (double*)mxGetData(plhs[1]);
    double* Std = (double*)mxGetData(plhs[2]);

    if (cid == mxDOUBLE_CLASS) {
        const double* in = (const double*)mxGetData(CubeA);
        double* out = (double*)mxGetData(plhs[0]);
        process_cube<double,double>(in, out, M, K, N, annIdx, useStdErr, Bg, Std);
    } else {
        const float* in = (const float*)mxGetData(CubeA);
        float* out = (float*)mxGetData(plhs[0]);
        process_cube<float,float>(in, out, M, K, N, annIdx, useStdErr, Bg, Std);
    }
}

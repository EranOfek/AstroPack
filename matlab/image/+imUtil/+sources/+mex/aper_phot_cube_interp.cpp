// aper_phot_cube_pix_interp.cpp
//
// Hybrid patch method (Option #1):
//   1) Build a small output patch around stamp center with radius B = ceil(Rmax)
//   2) Shift ONLY that patch using separable Lanczos-3 (horizontal then vertical)
//   3) Sum shifted pixels inside circular apertures (sorted radii) using bin+prefix sums
//
// USAGE:
//   [Flux, Area] = aper_phot_cube_pix_interp(Cube, Back, X1, Y1, AperRadii)
//
// INPUTS:
//   Cube      : MxK or MxKxN, real single/double (background INCLUDED).
//   Back      : scalar or length-N vector (single/double). Background level per slice.
//   X1, Y1    : scalar or length-N vector (single/double). Offsets relative to stamp center.
//               Convention: we shift IMAGE by (-X1,-Y1), equivalent to shifting a fixed mask by (+X1,+Y1).
//   AperRadii : sorted ascending radii vector (single/double).
//
// OUTPUTS (double):
//   Flux : N x Na  background-subtracted aperture sums (ignoring non-finite shifted pixels).
//   Area : N x Na  effective area = count of finite shifted pixels included.
//
// NOTES on NaNs:
//   - Non-finite values in the SHIFTED patch are ignored in Flux/Area.
//   - If the input contains NaNs, Lanczos interpolation may propagate NaNs to neighbors;
//     those affected output pixels will be ignored (rare-case behavior).
//
// COMPILE:
//   mex -O CXXFLAGS="\$CXXFLAGS -O3 -std=c++17 -march=native -fopenmp" \
//       LDFLAGS="\$LDFLAGS -fopenmp" aper_phot_cube_pix_interp.cpp

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <cstdio>

#if defined(_OPENMP)
  #include <omp.h>
#endif

#include <immintrin.h>

static void die(const char* msg) {
    mexErrMsgIdAndTxt("aper_phot_cube_pix_interp:err", "%s", msg);
}

static inline bool isRealSingleOrDouble(const mxArray* A) {
    return (mxIsSingle(A) || mxIsDouble(A)) && !mxIsComplex(A);
}

// ---------------- Lanczos-3 kernel ----------------
static constexpr int A_LANCZOS = 3;
static constexpr int TAPS = 2 * A_LANCZOS;      // 6
static constexpr int OFF0 = -(A_LANCZOS - 1);   // -2
static constexpr int OFF1 =  (A_LANCZOS);       // +3
static constexpr double PI = 3.141592653589793238462643383279502884;

static inline double sinc_pi(double x) {
    if (x == 0.0) return 1.0;
    const double pix = PI * x;
    return std::sin(pix) / pix;
}
static inline double lanczos3(double x) {
    const double ax = std::abs(x);
    if (ax >= (double)A_LANCZOS) return 0.0;
    return sinc_pi(x) * sinc_pi(x / (double)A_LANCZOS);
}

// Convert shift d into integer shiftInt and fractional frac in [0,1) using t=-d.
// Matches your existing shifter convention.
static inline double frac_from_neg_shift(double d, int& shiftInt) {
    const double t = -d;
    const double ft = std::floor(t);
    shiftInt = (int)ft;
    return t - ft; // [0,1)
}

static inline void weights_lanczos3(double frac01, double w[TAPS]) {
    for (int i = 0; i < TAPS; ++i) {
        const int k = OFF0 + i; // -2..+3
        w[i] = lanczos3(frac01 - (double)k);
    }
}

// ---------- SIMD helpers: out[i] += w * in[i] ----------
static inline void axpy_f32(float* out, const float* in, int n, float w) {
#if defined(__AVX2__)
    __m256 vw = _mm256_set1_ps(w);
    int i = 0;
    for (; i + 8 <= n; i += 8) {
        __m256 vo = _mm256_loadu_ps(out + i);
        __m256 vi = _mm256_loadu_ps(in  + i);
    #if defined(__FMA__)
        vo = _mm256_fmadd_ps(vi, vw, vo);
    #else
        vo = _mm256_add_ps(vo, _mm256_mul_ps(vi, vw));
    #endif
        _mm256_storeu_ps(out + i, vo);
    }
    for (; i < n; ++i) out[i] += w * in[i];
#else
    for (int i = 0; i < n; ++i) out[i] += w * in[i];
#endif
}

static inline void axpy_f64(double* out, const double* in, int n, double w) {
#if defined(__AVX2__)
    __m256d vw = _mm256_set1_pd(w);
    int i = 0;
    for (; i + 4 <= n; i += 4) {
        __m256d vo = _mm256_loadu_pd(out + i);
        __m256d vi = _mm256_loadu_pd(in  + i);
    #if defined(__FMA__)
        vo = _mm256_fmadd_pd(vi, vw, vo);
    #else
        vo = _mm256_add_pd(vo, _mm256_mul_pd(vi, vw));
    #endif
        _mm256_storeu_pd(out + i, vo);
    }
    for (; i < n; ++i) out[i] += w * in[i];
#else
    for (int i = 0; i < n; ++i) out[i] += w * in[i];
#endif
}

template <typename T>
static inline void zero_vec(T* p, int n) {
    std::fill(p, p + n, (T)0);
}

// ---------------- Reading helpers ----------------
static void readVecToDoubleLenN(const mxArray* A, int N, std::vector<double>& out, const char* name) {
    if (!isRealSingleOrDouble(A)) {
        char buf[256];
        ::snprintf(buf, sizeof(buf), "%s must be real single/double.", name);
        die(buf);
    }
    const mwSize nEl = mxGetNumberOfElements(A);
    if (!((int)nEl == 1 || (int)nEl == N)) {
        char buf[256];
        ::snprintf(buf, sizeof(buf), "%s must be scalar or length N.", name);
        die(buf);
    }
    out.resize((size_t)N);

    if (mxIsDouble(A)) {
        const double* p = (const double*)mxGetData(A);
        if ((int)nEl == 1) {
            const double v = p[0];
            for (int i = 0; i < N; ++i) out[i] = v;
        } else {
            std::copy(p, p + N, out.begin());
        }
    } else {
        const float* p = (const float*)mxGetData(A);
        if ((int)nEl == 1) {
            const double v = (double)p[0];
            for (int i = 0; i < N; ++i) out[i] = v;
        } else {
            for (int i = 0; i < N; ++i) out[i] = (double)p[i];
        }
    }
}

static void readRadiiToDouble(const mxArray* A, std::vector<double>& R) {
    if (!isRealSingleOrDouble(A)) die("AperRadii must be real single/double vector.");
    const mwSize nEl = mxGetNumberOfElements(A);
    if (nEl < 1) die("AperRadii must have at least one element.");
    R.resize((size_t)nEl);

    if (mxIsDouble(A)) {
        const double* p = (const double*)mxGetData(A);
        std::copy(p, p + (ptrdiff_t)nEl, R.begin());
    } else {
        const float* p = (const float*)mxGetData(A);
        for (mwSize i = 0; i < nEl; ++i) R[(size_t)i] = (double)p[i];
    }

    for (mwSize i = 0; i < nEl; ++i) {
        if (!(R[i] >= 0.0) || !mxIsFinite(R[i])) die("All AperRadii must be finite and >= 0.");
        if (i > 0 && R[i] < R[i - 1]) die("AperRadii must be sorted ascending.");
    }
}

// -------------- Precompute pixels within Rmax and their "bin" --------------
struct OutPix {
    int x;   // 0..K-1
    int y;   // 0..M-1
    int bin; // 0..Na-1 (smallest aperture containing this pixel)
};

// Also returns patch bounds [x1..x2],[y1..y2] in 0-based coords.
static void precompute_outpix_and_patch(
    int M, int K,
    const std::vector<double>& R,
    std::vector<OutPix>& outpix,
    int& px1, int& px2, int& py1, int& py2
) {
    const int Na = (int)R.size();
    std::vector<double> R2((size_t)Na);
    for (int a = 0; a < Na; ++a) R2[(size_t)a] = R[(size_t)a] * R[(size_t)a];

    const double x0 = 0.5 * ((double)K + 1.0); // MATLAB center (1-based)
    const double y0 = 0.5 * ((double)M + 1.0);

    const double Rmax = R.back();
    const double Rmax2 = Rmax * Rmax;

    const int B = (int)std::ceil(Rmax);

    // Patch bounds in MATLAB 1-based coords then to 0-based
    int xMin1 = (int)std::ceil (x0 - (double)B);
    int xMax1 = (int)std::floor(x0 + (double)B);
    int yMin1 = (int)std::ceil (y0 - (double)B);
    int yMax1 = (int)std::floor(y0 + (double)B);

    if (xMin1 < 1) xMin1 = 1;
    if (yMin1 < 1) yMin1 = 1;
    if (xMax1 > K) xMax1 = K;
    if (yMax1 > M) yMax1 = M;

    px1 = xMin1 - 1;
    px2 = xMax1 - 1;
    py1 = yMin1 - 1;
    py2 = yMax1 - 1;

    outpix.clear();
    outpix.reserve((size_t)((px2 - px1 + 1) * (py2 - py1 + 1)));

    for (int x1 = xMin1; x1 <= xMax1; ++x1) {
        const double dx = (double)x1 - x0;
        const double dx2 = dx * dx;
        const int x = x1 - 1;

        for (int y1 = yMin1; y1 <= yMax1; ++y1) {
            const double dy = (double)y1 - y0;
            const double r2 = dx2 + dy * dy;
            if (r2 > Rmax2) continue;

            int a0 = 0;
            while (a0 < Na - 1 && r2 > R2[(size_t)a0]) ++a0;

            OutPix p;
            p.x = x;
            p.y = y1 - 1;
            p.bin = a0;
            outpix.push_back(p);
        }
    }
}

// -------------- Shift ONLY the patch (separable) --------------
template <typename T>
static inline void shift_patch_sep_lanczos3(
    const T* in, int M, int K,
    int px1, int px2, int py1, int py2,          // patch bounds (0-based) in OUTPUT
    double dx, double dy,                         // shift applied to IMAGE
    std::vector<T>& tmp,                           // tmp buffer (Htmp x Px), column-major
    std::vector<T>& outPatch                       // out patch (Py x Px), column-major
) {
    const int Px = px2 - px1 + 1;
    const int Py = py2 - py1 + 1;

    int sxInt, syInt;
    const double fx = frac_from_neg_shift(dx, sxInt);
    const double fy = frac_from_neg_shift(dy, syInt);

    double wx[TAPS], wy[TAPS];
    weights_lanczos3(fx, wx);
    weights_lanczos3(fy, wy);

    // We need tmp rows covering y in [py1+syInt+OFF0 .. py2+syInt+OFF1]
    int yTmp1 = py1 + syInt + OFF0;
    int yTmp2 = py2 + syInt + OFF1;
    if (yTmp1 < 0) yTmp1 = 0;
    if (yTmp2 > M - 1) yTmp2 = M - 1;
    const int Htmp = (yTmp2 >= yTmp1) ? (yTmp2 - yTmp1 + 1) : 0;

    tmp.resize((size_t)Htmp * (size_t)Px);
    outPatch.resize((size_t)Py * (size_t)Px);

    // ---- Horizontal pass: tmp(y, x_out) = sum wx * in(y, x_in) for y in [yTmp1..yTmp2]
    for (int xo = 0; xo < Px; ++xo) {
        const int x = px1 + xo;           // output x in full image (0-based)
        T* tmpCol = tmp.data() + (size_t)Htmp * (size_t)xo;
        zero_vec(tmpCol, Htmp);

        const int xbase = x + sxInt;      // x_in base for tap offsets

        for (int tix = 0; tix < TAPS; ++tix) {
            const int xx = xbase + (OFF0 + tix);  // -2..+3
            if ((unsigned)xx >= (unsigned)K) continue;

            const double w = wx[tix];
            if (w == 0.0) continue;

            const T* inCol = in + (mwSize)M * (mwSize)xx + (mwSize)yTmp1; // segment start

            if constexpr (std::is_same<T,float>::value) {
                axpy_f32((float*)tmpCol, (const float*)inCol, Htmp, (float)w);
            } else {
                axpy_f64((double*)tmpCol, (const double*)inCol, Htmp, (double)w);
            }
        }
    }

    // ---- Vertical pass: outPatch(y_out, x_out) = sum wy * tmp(y_in, x_out)
    // y_in = (y_out_full + syInt + offy) mapped into tmp row index by subtracting yTmp1.
    for (int xo = 0; xo < Px; ++xo) {
        const T* tmpCol = tmp.data() + (size_t)Htmp * (size_t)xo;
        T* outCol = outPatch.data() + (size_t)Py * (size_t)xo;
        zero_vec(outCol, Py);

        for (int tiy = 0; tiy < TAPS; ++tiy) {
            const double w = wy[tiy];
            if (w == 0.0) continue;

            const int offy = OFF0 + tiy;           // -2..+3

            // For output y_full in [py1..py2], y_in = y_full + syInt + offy must be in [yTmp1..yTmp2]
            // Compute y_full range that satisfies it:
            int yStartFull = py1;
            int yEndFull   = py2;

            const int minFull = yTmp1 - (syInt + offy);
            const int maxFull = yTmp2 - (syInt + offy);

            if (yStartFull < minFull) yStartFull = minFull;
            if (yEndFull   > maxFull) yEndFull   = maxFull;

            if (yEndFull < yStartFull) continue;

            const int len = yEndFull - yStartFull + 1;

            // Map to patch-local y (0..Py-1)
            const int dst0 = yStartFull - py1;

            // Map to tmp-local y (0..Htmp-1)
            const int srcFull0 = yStartFull + syInt + offy;
            const int src0 = srcFull0 - yTmp1;

            const T* src = tmpCol + src0;
            T* dst = outCol + dst0;

            if constexpr (std::is_same<T,float>::value) {
                axpy_f32((float*)dst, (const float*)src, len, (float)w);
            } else {
                axpy_f64((double*)dst, (const double*)src, len, (double)w);
            }
        }
    }
}

// -------------- Main: hybrid patch shift + aperture sum --------------
template <typename T>
static void aper_phot_cube_hybrid(
    const T* cube, int M, int K, int N,
    const double* Back,
    const double* X1, const double* Y1,
    const std::vector<OutPix>& outpix,
    int px1, int px2, int py1, int py2,
    int Na,
    double* outFlux, double* outArea   // N x Na, col-major
) {
    const mwSize stride = (mwSize)M * (mwSize)K;
    const int Px = px2 - px1 + 1;
    const int Py = py2 - py1 + 1;

#if defined(_OPENMP)
    #pragma omp parallel
#endif
    {
        std::vector<T> tmp, outPatch;
        std::vector<double> binFlux((size_t)Na);
        std::vector<double> binArea((size_t)Na);

#if defined(_OPENMP)
        #pragma omp for schedule(static)
#endif
        for (int n = 0; n < N; ++n) {
            const T* in = cube + (mwSize)n * stride;

            // shift IMAGE by (-X1,-Y1) so that fixed-center apertures behave like mask shifted by (+X1,+Y1)
            const double dx = -X1[n];
            const double dy = -Y1[n];

            // Build shifted patch
            shift_patch_sep_lanczos3<T>(in, M, K, px1, px2, py1, py2, dx, dy, tmp, outPatch);

            std::fill(binFlux.begin(), binFlux.end(), 0.0);
            std::fill(binArea.begin(), binArea.end(), 0.0);

            // Sum only pixels in circle list (precomputed) using shifted patch values
            for (const auto& p : outpix) {
                const int xo = p.x - px1;
                const int yo = p.y - py1;
                if ((unsigned)xo >= (unsigned)Px || (unsigned)yo >= (unsigned)Py) continue;

                const T vT = outPatch[(size_t)yo + (size_t)Py * (size_t)xo];
                const double v = (double)vT;

                if (!std::isfinite(v)) continue; // ignore NaNs/Infs in shifted values
                binFlux[(size_t)p.bin] += v;
                binArea[(size_t)p.bin] += 1.0;
            }

            // prefix sums and background subtraction
            double runFlux = 0.0;
            double runArea = 0.0;
            const double b = Back[n];

            for (int a = 0; a < Na; ++a) {
                runFlux += binFlux[(size_t)a];
                runArea += binArea[(size_t)a];
                outFlux[n + (mwSize)N * (mwSize)a] = runFlux - b * runArea;
                outArea[n + (mwSize)N * (mwSize)a] = runArea;
            }
        }
    }
}

// ----------------- Help -----------------
static void print_help() {
    mexPrintf(
"aper_phot_cube_pix_interp  Fast aperture photometry with hybrid patch Lanczos-3 shift.\n"
"\n"
"USAGE:\n"
"  [Flux, Area] = aper_phot_cube_pix_interp(Cube, Back, X1, Y1, AperRadii)\n"
"\n"
"INPUTS:\n"
"  Cube      : MxK or MxKxN, real single/double (background included).\n"
"  Back      : scalar or length-N vector. Background per slice.\n"
"  X1, Y1    : scalar or length-N vector. Offsets relative to stamp center.\n"
"              Implementation shifts IMAGE by (-X1,-Y1), equivalent to shifting a fixed mask by (+X1,+Y1).\n"
"  AperRadii : sorted ascending radii vector.\n"
"\n"
"OUTPUTS (double):\n"
"  Flux : N x Na. Flux = sum(shifted finite pixels in aperture) - Back*Area.\n"
"  Area : N x Na. Area = count of finite shifted pixels included.\n"
"\n"
"ALGORITHM:\n"
"  - Precompute pixels inside largest aperture (Rmax) and their smallest-radius bin.\n"
"  - For each slice, compute ONLY a (2*ceil(Rmax)+1)^2 patch centered on stamp center\n"
"    using separable Lanczos-3 (horizontal then vertical), with zero padding.\n"
"  - Sum shifted patch values over circle pixels, then prefix-sum bins to get all radii.\n"
"\n"
"NaN/Inf handling:\n"
"  - Non-finite values in the shifted patch are ignored (do not contribute to Flux/Area).\n"
"  - Input NaNs may propagate through interpolation; affected shifted pixels are ignored.\n"
"\n"
    );
}

// ----------------- Gateway -----------------
extern "C" void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs == 0) { print_help(); return; }

    if (nrhs != 5) die("Usage: [Flux,Area] = aper_phot_cube_pix_interp(Cube, Back, X1, Y1, AperRadii)");
    if (nlhs != 2) die("Require two outputs: [Flux, Area].");

    const mxArray* CubeA = prhs[0];
    const mxArray* BackA = prhs[1];
    const mxArray* X1A   = prhs[2];
    const mxArray* Y1A   = prhs[3];
    const mxArray* RadA  = prhs[4];

    if (!isRealSingleOrDouble(CubeA)) die("Cube must be real single or double.");

    const mwSize nd = mxGetNumberOfDimensions(CubeA);
    if (nd != 2 && nd != 3) die("Cube must be 2-D (M x K) or 3-D (M x K x N).");

    const mwSize* dims = mxGetDimensions(CubeA);
    const int M = (int)dims[0];
    const int K = (int)dims[1];
    const int N = (nd == 3) ? (int)dims[2] : 1;

    std::vector<double> Back, X1, Y1;
    readVecToDoubleLenN(BackA, N, Back, "Back");
    readVecToDoubleLenN(X1A,   N, X1,   "X1");
    readVecToDoubleLenN(Y1A,   N, Y1,   "Y1");

    std::vector<double> R;
    readRadiiToDouble(RadA, R);
    const int Na = (int)R.size();

    // Precompute circle pixels and patch bounds
    std::vector<OutPix> outpix;
    int px1, px2, py1, py2;
    precompute_outpix_and_patch(M, K, R, outpix, px1, px2, py1, py2);

    // Outputs are double matrices N x Na
    plhs[0] = mxCreateDoubleMatrix((mwSize)N, (mwSize)Na, mxREAL);
    plhs[1] = mxCreateDoubleMatrix((mwSize)N, (mwSize)Na, mxREAL);

    double* outFlux = (double*)mxGetData(plhs[0]);
    double* outArea = (double*)mxGetData(plhs[1]);

    if (mxIsDouble(CubeA)) {
        const double* cube = (const double*)mxGetData(CubeA);
        aper_phot_cube_hybrid<double>(cube, M, K, N, Back.data(), X1.data(), Y1.data(),
                                      outpix, px1, px2, py1, py2, Na, outFlux, outArea);
    } else {
        const float* cube = (const float*)mxGetData(CubeA);
        aper_phot_cube_hybrid<float>(cube, M, K, N, Back.data(), X1.data(), Y1.data(),
                                     outpix, px1, px2, py1, py2, Na, outFlux, outArea);
    }
}

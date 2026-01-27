#include "mex.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include <algorithm>

#if defined(_OPENMP)
  #include <omp.h>
#endif

#include <immintrin.h>

static void die(const char* msg) {
    mexErrMsgIdAndTxt("shift_cube_sinc_sep_simd:err", "%s", msg);
}

static inline bool isRealSingleOrDouble(const mxArray* A) {
    return (mxIsSingle(A) || mxIsDouble(A)) && !mxIsComplex(A);
}

static constexpr int A_LANCZOS = 3;
static constexpr int TAPS = 2 * A_LANCZOS;      // 6
static constexpr int K0 = -(A_LANCZOS - 1);     // -2
static constexpr int K1 = A_LANCZOS;            // +3
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

static inline double frac_from_neg_shift(double d, int& shiftInt) {
    const double t = -d;
    const double ft = std::floor(t);
    shiftInt = (int)ft;
    return t - ft; // in [0,1)
}

static inline void weights_lanczos3(double frac01, double w[TAPS]) {
    for (int i = 0; i < TAPS; ++i) {
        const int k = K0 + i; // -2..+3
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

static inline void zero_f32(float* p, int n)  { std::fill(p, p + n, 0.0f); }
static inline void zero_f64(double* p, int n) { std::fill(p, p + n, 0.0); }

// ---------- One image: separable, column-wise AXPY ----------
template <typename T>
static inline void shift_one_image_sep_simd(
    const T* in, T* out,
    int M, int K,
    double dx, double dy,
    T* tmp // M*K temp, same type as input to avoid casts
) {
    int sxInt, syInt;
    const double fx = frac_from_neg_shift(dx, sxInt);
    const double fy = frac_from_neg_shift(dy, syInt);

    double wxd[TAPS], wyd[TAPS];
    weights_lanczos3(fx, wxd);
    weights_lanczos3(fy, wyd);

    // ---- Pass 1: horizontal into tmp (column-wise) ----
    // tmp(:,x) = sum_i wx[i] * in(:, x+sxInt+ix) with zero padding
    for (int x = 0; x < K; ++x) {
        T* tmpCol = tmp + (mwSize)M * (mwSize)x;
        // tmpCol = 0
        if constexpr (std::is_same<T,float>::value) zero_f32((float*)tmpCol, M);
        else                                        zero_f64((double*)tmpCol, M);

        const int xbase = x + sxInt;

        for (int ti = 0; ti < TAPS; ++ti) {
            const int ix = K0 + ti;            // -2..+3
            const int xx = xbase + ix;
            if ((unsigned)xx >= (unsigned)K) continue;

            const T* inCol = in + (mwSize)M * (mwSize)xx;

            const double w = wxd[ti];
            if (w == 0.0) continue;

            if constexpr (std::is_same<T,float>::value) {
                axpy_f32((float*)tmpCol, (const float*)inCol, M, (float)w);
            } else {
                axpy_f64((double*)tmpCol, (const double*)inCol, M, (double)w);
            }
        }
    }

    // ---- Pass 2: vertical from tmp into out (segment AXPY, shifted) ----
    // out(:,x) = sum_i wy[i] * tmp(:,x) shifted by (syInt+iy), zero padding.
    for (int x = 0; x < K; ++x) {
        const T* tmpCol = tmp + (mwSize)M * (mwSize)x;
        T* outCol       = out + (mwSize)M * (mwSize)x;

        // outCol = 0
        if constexpr (std::is_same<T,float>::value) zero_f32((float*)outCol, M);
        else                                        zero_f64((double*)outCol, M);

        for (int ti = 0; ti < TAPS; ++ti) {
            const int iy = K0 + ti;            // -2..+3
            const int sh = syInt + iy;         // source index = y + sh

            const double w = wyd[ti];
            if (w == 0.0) continue;

            // y in [0..M-1], src = y+sh must be in [0..M-1]
            // => yStart = max(0, -sh), yEnd = min(M-1, M-1-sh)
            int yStart = (sh < 0) ? -sh : 0;
            int yEnd   = (sh > 0) ? (M - 1 - sh) : (M - 1);
            if (yEnd < yStart) continue;

            const int len = yEnd - yStart + 1;
            const T* src = tmpCol + (yStart + sh);
            T* dst = outCol + yStart;

            if constexpr (std::is_same<T,float>::value) {
                axpy_f32((float*)dst, (const float*)src, len, (float)w);
            } else {
                axpy_f64((double*)dst, (const double*)src, len, (double)w);
            }
        }
    }
}

template <typename T>
static void shift_cube_sep_simd(
    const T* cube, T* outCube,
    int M, int K, int N,
    const double* Dx, const double* Dy
) {
    const mwSize sliceStride = (mwSize)M * (mwSize)K;

#if defined(_OPENMP)
    #pragma omp parallel
#endif
    {
        std::vector<T> tmp((size_t)M * (size_t)K);

#if defined(_OPENMP)
        #pragma omp for schedule(static)
#endif
        for (int n = 0; n < N; ++n) {
            const T* in  = cube    + (mwSize)n * sliceStride;
            T* out       = outCube + (mwSize)n * sliceStride;

            shift_one_image_sep_simd<T>(in, out, M, K, Dx[n], Dy[n], tmp.data());
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 3) die("Usage: Out = shift_cube_sinc_sep_simd(Cube, Dx, Dy)");
    if (nlhs != 1) die("One output.");

    const mxArray* CubeA = prhs[0];
    const mxArray* DxA   = prhs[1];
    const mxArray* DyA   = prhs[2];

    if (!isRealSingleOrDouble(CubeA)) die("Cube must be real single or double.");
    if (!isRealSingleOrDouble(DxA) || !isRealSingleOrDouble(DyA)) die("Dx and Dy must be real single/double.");
    if (mxGetNumberOfDimensions(CubeA) != 3) die("Cube must be 3-D (M x K x N).");

    const mwSize* dims = mxGetDimensions(CubeA);
    const int M = (int)dims[0];
    const int K = (int)dims[1];
    const int N = (int)dims[2];

    if ((int)mxGetNumberOfElements(DxA) != N || (int)mxGetNumberOfElements(DyA) != N)
        die("Dx and Dy must have length N.");

    // Convert Dx/Dy to double (N~1000 => negligible)
    std::vector<double> Dx(N), Dy(N);
    if (mxIsDouble(DxA)) {
        const double* p = (const double*)mxGetData(DxA);
        std::copy(p, p + N, Dx.begin());
    } else {
        const float* p = (const float*)mxGetData(DxA);
        for (int i = 0; i < N; ++i) Dx[i] = (double)p[i];
    }
    if (mxIsDouble(DyA)) {
        const double* p = (const double*)mxGetData(DyA);
        std::copy(p, p + N, Dy.begin());
    } else {
        const float* p = (const float*)mxGetData(DyA);
        for (int i = 0; i < N; ++i) Dy[i] = (double)p[i];
    }

    const mxClassID cid = mxGetClassID(CubeA);
    plhs[0] = mxCreateNumericArray(3, dims, cid, mxREAL);

    if (cid == mxDOUBLE_CLASS) {
        const double* in = (const double*)mxGetData(CubeA);
        double* out = (double*)mxGetData(plhs[0]);
        shift_cube_sep_simd<double>(in, out, M, K, N, Dx.data(), Dy.data());
    } else {
        const float* in = (const float*)mxGetData(CubeA);
        float* out = (float*)mxGetData(plhs[0]);
        shift_cube_sep_simd<float>(in, out, M, K, N, Dx.data(), Dy.data());
    }
}

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include <algorithm>
#include <type_traits>

#if defined(_OPENMP)
  #include <omp.h>
#endif

#include <immintrin.h>

static void die(const char* msg) {
    mexErrMsgIdAndTxt("shift_sinc_sep_simd:err", "%s", msg);
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

// ============================================================
// SIMD AXPY kernels
// out[i] += w * in[i]
// Dispatch order:
//   AVX-512F -> AVX -> scalar
// AVX2 is naturally covered by the AVX path for this FP kernel.
// FMA is used whenever __FMA__ is available.
// ============================================================

static inline void axpy_f32(float* out, const float* in, int n, float w) {
#if defined(__AVX512F__)
    __m512 vw = _mm512_set1_ps(w);
    int i = 0;
    for (; i + 16 <= n; i += 16) {
        __m512 vo = _mm512_loadu_ps(out + i);
        __m512 vi = _mm512_loadu_ps(in  + i);
    #if defined(__FMA__)
        vo = _mm512_fmadd_ps(vi, vw, vo);
    #else
        vo = _mm512_add_ps(vo, _mm512_mul_ps(vi, vw));
    #endif
        _mm512_storeu_ps(out + i, vo);
    }

    __m256 vw256 = _mm256_set1_ps(w);
    for (; i + 8 <= n; i += 8) {
        __m256 vo = _mm256_loadu_ps(out + i);
        __m256 vi = _mm256_loadu_ps(in  + i);
    #if defined(__FMA__)
        vo = _mm256_fmadd_ps(vi, vw256, vo);
    #else
        vo = _mm256_add_ps(vo, _mm256_mul_ps(vi, vw256));
    #endif
        _mm256_storeu_ps(out + i, vo);
    }

    for (; i < n; ++i) out[i] += w * in[i];

#elif defined(__AVX__)
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
#if defined(__AVX512F__)
    __m512d vw = _mm512_set1_pd(w);
    int i = 0;
    for (; i + 8 <= n; i += 8) {
        __m512d vo = _mm512_loadu_pd(out + i);
        __m512d vi = _mm512_loadu_pd(in  + i);
    #if defined(__FMA__)
        vo = _mm512_fmadd_pd(vi, vw, vo);
    #else
        vo = _mm512_add_pd(vo, _mm512_mul_pd(vi, vw));
    #endif
        _mm512_storeu_pd(out + i, vo);
    }

    __m256d vw256 = _mm256_set1_pd(w);
    for (; i + 4 <= n; i += 4) {
        __m256d vo = _mm256_loadu_pd(out + i);
        __m256d vi = _mm256_loadu_pd(in  + i);
    #if defined(__FMA__)
        vo = _mm256_fmadd_pd(vi, vw256, vo);
    #else
        vo = _mm256_add_pd(vo, _mm256_mul_pd(vi, vw256));
    #endif
        _mm256_storeu_pd(out + i, vo);
    }

    for (; i < n; ++i) out[i] += w * in[i];

#elif defined(__AVX__)
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
    T* tmp
) {
    int sxInt, syInt;
    const double fx = frac_from_neg_shift(dx, sxInt);
    const double fy = frac_from_neg_shift(dy, syInt);

    double wxd[TAPS], wyd[TAPS];
    weights_lanczos3(fx, wxd);
    weights_lanczos3(fy, wyd);

    // ---- Pass 1: horizontal into tmp ----
    for (int x = 0; x < K; ++x) {
        T* tmpCol = tmp + (mwSize)M * (mwSize)x;
        if constexpr (std::is_same<T,float>::value) zero_f32((float*)tmpCol, M);
        else                                        zero_f64((double*)tmpCol, M);

        const int xbase = x + sxInt;

        for (int ti = 0; ti < TAPS; ++ti) {
            const int ix = K0 + ti;
            const int xx = xbase + ix;
            if ((unsigned)xx >= (unsigned)K) continue;

            const double w = wxd[ti];
            if (w == 0.0) continue;

            const T* inCol = in + (mwSize)M * (mwSize)xx;

            if constexpr (std::is_same<T,float>::value) {
                axpy_f32((float*)tmpCol, (const float*)inCol, M, (float)w);
            } else {
                axpy_f64((double*)tmpCol, (const double*)inCol, M, (double)w);
            }
        }
    }

    // ---- Pass 2: vertical from tmp into out ----
    for (int x = 0; x < K; ++x) {
        const T* tmpCol = tmp + (mwSize)M * (mwSize)x;
        T* outCol       = out + (mwSize)M * (mwSize)x;

        if constexpr (std::is_same<T,float>::value) zero_f32((float*)outCol, M);
        else                                        zero_f64((double*)outCol, M);

        for (int ti = 0; ti < TAPS; ++ti) {
            const int iy = K0 + ti;
            const int sh = syInt + iy; // source index = y + sh

            const double w = wyd[ti];
            if (w == 0.0) continue;

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
static void shift_stack_sep_simd(
    const T* inData, T* outData,
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
            const T* in  = inData  + (mwSize)n * sliceStride;
            T* out       = outData + (mwSize)n * sliceStride;
            shift_one_image_sep_simd<T>(in, out, M, K, Dx[n], Dy[n], tmp.data());
        }
    }
}

template <typename T>
static void shift_matrix_to_cube_sep_simd(
    const T* inData, T* outData,
    int M, int K, int L,
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
        for (int n = 0; n < L; ++n) {
            T* out = outData + (mwSize)n * sliceStride;
            shift_one_image_sep_simd<T>(inData, out, M, K, Dx[n], Dy[n], tmp.data());
        }
    }
}

static void readShiftVectorToDouble(const mxArray* A, std::vector<double>& out) {
    if (!isRealSingleOrDouble(A)) die("Dx/Dy must be real single or double.");
    const mwSize nEl = mxGetNumberOfElements(A);
    out.resize((size_t)nEl);

    if (mxIsDouble(A)) {
        const double* p = (const double*)mxGetData(A);
        std::copy(p, p + nEl, out.begin());
    } else {
        const float* p = (const float*)mxGetData(A);
        for (mwSize i = 0; i < nEl; ++i) out[(size_t)i] = (double)p[i];
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 3) die("Usage: Out = shift_sinc_sep_simd(ImageOrCube, Dx, Dy)");
    if (nlhs != 1) die("One output.");

    const mxArray* A   = prhs[0];
    const mxArray* DxA = prhs[1];
    const mxArray* DyA = prhs[2];

    if (!isRealSingleOrDouble(A)) die("First input must be real single or double.");

    const mwSize nd = mxGetNumberOfDimensions(A);
    if (nd != 2 && nd != 3) die("First input must be 2-D (M x K) or 3-D (M x K x N).");

    const mwSize* dims = mxGetDimensions(A);
    const int M = (int)dims[0];
    const int K = (int)dims[1];
    const int N = (nd == 3) ? (int)dims[2] : 1;

    std::vector<double> Dx, Dy;
    readShiftVectorToDouble(DxA, Dx);
    readShiftVectorToDouble(DyA, Dy);

    if (Dx.size() != Dy.size()) die("Dx and Dy must have the same number of elements.");
    const mwSize nShift = (mwSize)Dx.size();
    if (nShift == 0) die("Dx and Dy must not be empty.");

    const mxClassID cid = mxGetClassID(A);

    // 3-D input: one shift per slice
    if (nd == 3) {
        if ((int)nShift != N) die("For 3-D input, Dx and Dy must have length N (number of images).");

        plhs[0] = mxCreateNumericArray(3, dims, cid, mxREAL);

        if (cid == mxDOUBLE_CLASS) {
            const double* in = (const double*)mxGetData(A);
            double* out = (double*)mxGetData(plhs[0]);
            shift_stack_sep_simd<double>(in, out, M, K, N, Dx.data(), Dy.data());
        } else {
            const float* in = (const float*)mxGetData(A);
            float* out = (float*)mxGetData(plhs[0]);
            shift_stack_sep_simd<float>(in, out, M, K, N, Dx.data(), Dy.data());
        }
    }
    // 2-D input: scalar shift -> matrix, vector shifts -> cube
    else {
        if (nShift == 1) {
            plhs[0] = mxCreateNumericMatrix((mwSize)M, (mwSize)K, cid, mxREAL);

            if (cid == mxDOUBLE_CLASS) {
                const double* in = (const double*)mxGetData(A);
                double* out = (double*)mxGetData(plhs[0]);
                std::vector<double> tmp((size_t)M * (size_t)K);
                shift_one_image_sep_simd<double>(in, out, M, K, Dx[0], Dy[0], tmp.data());
            } else {
                const float* in = (const float*)mxGetData(A);
                float* out = (float*)mxGetData(plhs[0]);
                std::vector<float> tmp((size_t)M * (size_t)K);
                shift_one_image_sep_simd<float>(in, out, M, K, Dx[0], Dy[0], tmp.data());
            }
        } else {
            mwSize outDims[3];
            outDims[0] = (mwSize)M;
            outDims[1] = (mwSize)K;
            outDims[2] = nShift;
            plhs[0] = mxCreateNumericArray(3, outDims, cid, mxREAL);

            if (cid == mxDOUBLE_CLASS) {
                const double* in = (const double*)mxGetData(A);
                double* out = (double*)mxGetData(plhs[0]);
                shift_matrix_to_cube_sep_simd<double>(in, out, M, K, (int)nShift, Dx.data(), Dy.data());
            } else {
                const float* in = (const float*)mxGetData(A);
                float* out = (float*)mxGetData(plhs[0]);
                shift_matrix_to_cube_sep_simd<float>(in, out, M, K, (int)nShift, Dx.data(), Dy.data());
            }
        }
    }
}

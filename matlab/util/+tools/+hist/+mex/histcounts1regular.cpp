#include "mex.h"
#include <cstdint>
#include <cmath>
#include <cstring>

#ifdef _OPENMP
  #include <omp.h>
#endif

#if defined(_MSC_VER)
  #include <intrin.h>
  #include <immintrin.h>
#else
  #include <immintrin.h>
#endif

// Scalar helper
static inline double get_scalar(const mxArray* A, const char* name){
    if (mxGetNumberOfElements(A) != 1 || mxIsComplex(A))
        mexErrMsgIdAndTxt("hist1d:arg","%s must be a real scalar.", name);
    return mxGetScalar(A);
}

// ==================== Scalar kernels (fallback / int types) ====================
template <typename T>
static void hist_scalar(const T* __restrict x, mwSize n,
                        double firstEdge, double invStep,
                        double rightEdge, mwSize numBins,
                        uint32_t* __restrict H)
{
    #pragma omp parallel
    {
        uint32_t* local = new uint32_t[numBins](); // zeroed
        #pragma omp for schedule(static) nowait
        for (mwIndex i = 0; i < n; ++i){
            double v = (double)x[i];
            // range check (also rejects NaN)
            if (!(v >= firstEdge && v < rightEdge)) continue;
            double t = (v - firstEdge) * invStep;     // t >= 0
            mwSize bin = (mwSize)t;                  // trunc (floor)
            if (bin < numBins) local[bin]++;         // safety guard
        }
        #pragma omp critical
        {
            for (mwSize j = 0; j < numBins; ++j) H[j] += local[j];
        }
        delete[] local;
    }
}

// ==================== SIMD kernels (AVX2) ====================

// ---- DOUBLE (4-wide) ----
static void hist_simd_double(const double* __restrict x, mwSize n,
                             double firstEdge, double invStep,
                             double rightEdge, mwSize numBins,
                             uint32_t* __restrict H)
{
#if defined(__AVX2__)
    // If numBins won't fit into 32-bit indices, fall back.
    if (numBins > 0x7fffffff) { hist_scalar(x,n,firstEdge,invStep,rightEdge,numBins,H); return; }

    const __m256d vFirst = _mm256_set1_pd(firstEdge);
    const __m256d vRight = _mm256_set1_pd(rightEdge);
    const __m256d vInv   = _mm256_set1_pd(invStep);
    const __m256i vZeroI = _mm256_set1_epi32(0);
    const __m256i vMaxI  = _mm256_set1_epi32((int)numBins - 1);

    const mwSize V  = 4;
    const mwSize Nb = (n / V) * V;

    #pragma omp parallel
    {
        uint32_t* local = new uint32_t[numBins]();

        #pragma omp for schedule(static) nowait
        for (mwIndex i = 0; i < Nb; i += V){
            // Load 4 doubles
            __m256d xv = _mm256_loadu_pd(x + i);
            // Mask lanes that are in-range: firstEdge <= x < rightEdge
            __m256d ge = _mm256_cmp_pd(xv, vFirst, _CMP_GE_OQ);
            __m256d lt = _mm256_cmp_pd(xv, vRight, _CMP_LT_OQ);
            __m256d ok = _mm256_and_pd(ge, lt);
            int maskOk = _mm256_movemask_pd(ok);
            if (maskOk == 0) continue;

            // t = (x - firstEdge) * invStep   [>= 0 for valid lanes]
            __m256d t  = _mm256_mul_pd(_mm256_sub_pd(xv, vFirst), vInv);
            // convert to int via truncation (floor for t>=0)
            // _mm256_cvttpd_epi32 returns __m128i with 4 lanes
            __m128i idx128 = _mm256_cvttpd_epi32(t);
            // clamp idx to [0, numBins-1]
            __m256i idx256 = _mm256_castsi128_si256(idx128);
            idx256 = _mm256_max_epi32(idx256, vZeroI);
            idx256 = _mm256_min_epi32(idx256, vMaxI);
            // extract 4 ints
            alignas(16) int ii[4];
            _mm_storeu_si128((__m128i*)ii, _mm256_castsi256_si128(idx256));

            // scalar scatters guarded by mask bits b3..b0
            if (maskOk & 0x1) local[(mwSize)ii[0]]++;
            if (maskOk & 0x2) local[(mwSize)ii[1]]++;
            if (maskOk & 0x4) local[(mwSize)ii[2]]++;
            if (maskOk & 0x8) local[(mwSize)ii[3]]++;
        }

        // tail
        #pragma omp for schedule(static) nowait
        for (mwIndex i = Nb; i < n; ++i){
            double v = x[i];
            if (!(v >= firstEdge && v < rightEdge)) continue;
            double t = (v - firstEdge) * invStep;
            mwSize bin = (mwSize)t;
            if (bin < numBins) local[bin]++;
        }

        #pragma omp critical
        {
            for (mwSize j = 0; j < numBins; ++j) H[j] += local[j];
        }
        delete[] local;
    }
#else
    (void)rightEdge;
    hist_scalar(x,n,firstEdge,invStep,rightEdge,numBins,H);
#endif
}

// ---- FLOAT (8-wide) ----
static void hist_simd_float(const float* __restrict x, mwSize n,
                            float firstEdge, float invStep,
                            float rightEdge, mwSize numBins,
                            uint32_t* __restrict H)
{
#if defined(__AVX2__)
    if (numBins > 0x7fffffff) { // safety
        hist_scalar(x,n,(double)firstEdge,(double)invStep,(double)rightEdge,numBins,H);
        return;
    }

    const __m256 vFirst = _mm256_set1_ps(firstEdge);
    const __m256 vRight = _mm256_set1_ps(rightEdge);
    const __m256 vInv   = _mm256_set1_ps(invStep);
    const __m256i vZeroI= _mm256_set1_epi32(0);
    const __m256i vMaxI = _mm256_set1_epi32((int)numBins - 1);

    const mwSize V  = 8;
    const mwSize Nb = (n / V) * V;

    #pragma omp parallel
    {
        uint32_t* local = new uint32_t[numBins]();

        #pragma omp for schedule(static) nowait
        for (mwIndex i = 0; i < Nb; i += V){
            __m256 xv = _mm256_loadu_ps(x + i);
            __m256 ge = _mm256_cmp_ps(xv, vFirst, _CMP_GE_OQ);
            __m256 lt = _mm256_cmp_ps(xv, vRight, _CMP_LT_OQ);
            __m256 ok = _mm256_and_ps(ge, lt);
            int maskOk = _mm256_movemask_ps(ok);
            if (maskOk == 0) continue;

            __m256 t   = _mm256_mul_ps(_mm256_sub_ps(xv, vFirst), vInv);
            __m256i idx = _mm256_cvttps_epi32(t);
            idx = _mm256_max_epi32(idx, vZeroI);
            idx = _mm256_min_epi32(idx, vMaxI);

            alignas(32) int ii[8];
            _mm256_storeu_si256((__m256i*)ii, idx);

            // scatter by mask bits b7..b0
            if (maskOk & 0x01) local[(mwSize)ii[0]]++;
            if (maskOk & 0x02) local[(mwSize)ii[1]]++;
            if (maskOk & 0x04) local[(mwSize)ii[2]]++;
            if (maskOk & 0x08) local[(mwSize)ii[3]]++;
            if (maskOk & 0x10) local[(mwSize)ii[4]]++;
            if (maskOk & 0x20) local[(mwSize)ii[5]]++;
            if (maskOk & 0x40) local[(mwSize)ii[6]]++;
            if (maskOk & 0x80) local[(mwSize)ii[7]]++;
        }

        // tail
        #pragma omp for schedule(static) nowait
        for (mwIndex i = Nb; i < n; ++i){
            float v = x[i];
            if (!(v >= firstEdge && v < rightEdge)) continue;
            float t = (v - firstEdge) * invStep;
            mwSize bin = (mwSize)t;
            if (bin < numBins) local[bin]++;
        }

        #pragma omp critical
        {
            for (mwSize j = 0; j < numBins; ++j) H[j] += local[j];
        }
        delete[] local;
    }
#else
    (void)rightEdge;
    hist_scalar(x,n,(double)firstEdge,(double)invStep,(double)rightEdge,numBins,H);
#endif
}

// ==================== Entry point ====================
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 4)
        mexErrMsgIdAndTxt("hist1d:args","Usage: hist = hist1d_regular(Vector, firstEdge, binSize, numBins).");

    const mxArray* A = prhs[0];
    if (mxIsComplex(A))
        mexErrMsgIdAndTxt("hist1d:complex","Input array must be real.");

    const double firstEdge = get_scalar(prhs[1], "firstEdge");
    const double binSize   = get_scalar(prhs[2], "binSize");
    const double Nin       = get_scalar(prhs[3], "numBins");

    if (!(binSize > 0.0))
        mexErrMsgIdAndTxt("hist1d:binSize","binSize must be positive.");
    if (!mxIsFinite(Nin) || Nin < 1.0)
        mexErrMsgIdAndTxt("hist1d:numBins","numBins must be >= 1.");

    const mwSize numBins  = (mwSize)llround(Nin);
    if (numBins == 0) mexErrMsgIdAndTxt("hist1d:numBins","numBins invalid.");

    const mwSize n = (mwSize)mxGetNumberOfElements(A);
    const double invStepD = 1.0 / binSize;
    const double rightEdgeD = firstEdge + (double)numBins * binSize;

    // Output: uint32 column vector
    plhs[0] = mxCreateNumericMatrix(numBins, 1, mxUINT32_CLASS, mxREAL);
    uint32_t* H = (uint32_t*)mxGetData(plhs[0]);
    std::memset(H, 0, (size_t)numBins * sizeof(uint32_t));
    if (n == 0) return;

    const void* p = mxGetData(A);
    switch (mxGetClassID(A)) {
        case mxDOUBLE_CLASS:
            hist_simd_double((const double*)p, n, firstEdge, invStepD, rightEdgeD, numBins, H);
            break;
        case mxSINGLE_CLASS: {
            const float fFirst = (float)firstEdge;
            const float fInv   = (float)invStepD;
            const float fRight = (float)rightEdgeD;
            hist_simd_float((const float*)p, n, fFirst, fInv, fRight, numBins, H);
            break;
        }
        case mxUINT32_CLASS:
            hist_scalar((const uint32_T*)p, n, firstEdge, invStepD, rightEdgeD, numBins, H);
            break;
        case mxINT32_CLASS:
            hist_scalar((const int32_T*)p,  n, firstEdge, invStepD, rightEdgeD, numBins, H);
            break;
        default:
            mexErrMsgIdAndTxt("hist1d:type","Data must be single, double, uint32, or int32.");
    }
}

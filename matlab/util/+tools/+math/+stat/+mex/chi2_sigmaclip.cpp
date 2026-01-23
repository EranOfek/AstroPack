#include "mex.h"
#include <immintrin.h>
#include <cmath>
#include <cstdint>
#include <limits>

static void die(const char* msg) {
    mexErrMsgIdAndTxt("chi2_vec:err", "%s", msg);
}

static inline bool isVector(const mxArray* A) {
    return (mxGetM(A) == 1 || mxGetN(A) == 1);
}

static inline double hsum256_pd(__m256d v) {
    __m128d lo = _mm256_castpd256_pd128(v);
    __m128d hi = _mm256_extractf128_pd(v, 1);
    __m128d s  = _mm_add_pd(lo, hi);
    s = _mm_hadd_pd(s, s);
    return _mm_cvtsd_f64(s);
}

static inline double hsum256_ps(__m256 v) {
    __m128 lo = _mm256_castps256_ps128(v);
    __m128 hi = _mm256_extractf128_ps(v, 1);
    __m128 s  = _mm_add_ps(lo, hi);
    s = _mm_hadd_ps(s, s);
    s = _mm_hadd_ps(s, s);
    return (double)_mm_cvtss_f32(s);
}

static bool parseBoolScalar(const mxArray* A) {
    if (mxIsLogicalScalar(A)) return mxIsLogicalScalarTrue(A);
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1)
        die("IgnoreNaN must be a logical scalar or numeric scalar.");
    return (mxGetScalar(A) != 0.0);
}

// ---- Core: double ----
static double chi2_double_clip_count_nan(const double* o, const double* c,
                                         const double* s, bool sScalar, double s0,
                                         double nLow, double nHigh,
                                         bool ignoreNaN,
                                         mwSize n, uint64_t* outCount, bool* outAnyNaN)
{
    const mwSize V = 4;
    const mwSize V2 = 8;

    const __m256d vHigh = _mm256_set1_pd(nHigh);
    const __m256d vLow  = _mm256_set1_pd(-nLow);

    __m256d acc0 = _mm256_setzero_pd();
    __m256d acc1 = _mm256_setzero_pd();
    uint64_t cnt = 0;
    bool anyNaN = false;

    if (sScalar) {
        if (!ignoreNaN && std::isnan(s0)) anyNaN = true;
        const double invSig = 1.0 / s0;
        const __m256d vInvSig = _mm256_set1_pd(invSig);

        mwSize i = 0;
        for (; i + V2 <= n; i += V2) {
            __m256d o0 = _mm256_loadu_pd(o + i);
            __m256d c0 = _mm256_loadu_pd(c + i);
            __m256d z0 = _mm256_mul_pd(_mm256_sub_pd(c0, o0), vInvSig);

            if (!ignoreNaN) {
                __m256d nan0 = _mm256_cmp_pd(o0, o0, _CMP_UNORD_Q);
                nan0 = _mm256_or_pd(nan0, _mm256_cmp_pd(c0, c0, _CMP_UNORD_Q));
                nan0 = _mm256_or_pd(nan0, _mm256_cmp_pd(z0, z0, _CMP_UNORD_Q));
                if (_mm256_movemask_pd(nan0)) anyNaN = true;
            }

            __m256d m0 = _mm256_and_pd(
                _mm256_cmp_pd(z0, vHigh, _CMP_LE_OQ),
                _mm256_cmp_pd(z0, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256d ok0 = _mm256_cmp_pd(z0, z0, _CMP_ORD_Q);
                m0 = _mm256_and_pd(m0, ok0);
            }

            int mask0 = _mm256_movemask_pd(m0);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask0);

            __m256d z0m = _mm256_and_pd(z0, m0);
#ifdef __FMA__
            acc0 = _mm256_fmadd_pd(z0m, z0m, acc0);
#else
            acc0 = _mm256_add_pd(acc0, _mm256_mul_pd(z0m, z0m));
#endif

            __m256d o1 = _mm256_loadu_pd(o + i + V);
            __m256d c1 = _mm256_loadu_pd(c + i + V);
            __m256d z1 = _mm256_mul_pd(_mm256_sub_pd(c1, o1), vInvSig);

            if (!ignoreNaN) {
                __m256d nan1 = _mm256_cmp_pd(o1, o1, _CMP_UNORD_Q);
                nan1 = _mm256_or_pd(nan1, _mm256_cmp_pd(c1, c1, _CMP_UNORD_Q));
                nan1 = _mm256_or_pd(nan1, _mm256_cmp_pd(z1, z1, _CMP_UNORD_Q));
                if (_mm256_movemask_pd(nan1)) anyNaN = true;
            }

            __m256d m1 = _mm256_and_pd(
                _mm256_cmp_pd(z1, vHigh, _CMP_LE_OQ),
                _mm256_cmp_pd(z1, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256d ok1 = _mm256_cmp_pd(z1, z1, _CMP_ORD_Q);
                m1 = _mm256_and_pd(m1, ok1);
            }

            int mask1 = _mm256_movemask_pd(m1);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask1);

            __m256d z1m = _mm256_and_pd(z1, m1);
#ifdef __FMA__
            acc1 = _mm256_fmadd_pd(z1m, z1m, acc1);
#else
            acc1 = _mm256_add_pd(acc1, _mm256_mul_pd(z1m, z1m));
#endif
        }

        for (; i + V <= n; i += V) {
            __m256d vo = _mm256_loadu_pd(o + i);
            __m256d vc = _mm256_loadu_pd(c + i);
            __m256d z  = _mm256_mul_pd(_mm256_sub_pd(vc, vo), vInvSig);

            if (!ignoreNaN) {
                __m256d nan = _mm256_cmp_pd(vo, vo, _CMP_UNORD_Q);
                nan = _mm256_or_pd(nan, _mm256_cmp_pd(vc, vc, _CMP_UNORD_Q));
                nan = _mm256_or_pd(nan, _mm256_cmp_pd(z, z, _CMP_UNORD_Q));
                if (_mm256_movemask_pd(nan)) anyNaN = true;
            }

            __m256d m = _mm256_and_pd(
                _mm256_cmp_pd(z, vHigh, _CMP_LE_OQ),
                _mm256_cmp_pd(z, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256d ok = _mm256_cmp_pd(z, z, _CMP_ORD_Q);
                m = _mm256_and_pd(m, ok);
            }

            int mask = _mm256_movemask_pd(m);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask);

            __m256d zm = _mm256_and_pd(z, m);
#ifdef __FMA__
            acc0 = _mm256_fmadd_pd(zm, zm, acc0);
#else
            acc0 = _mm256_add_pd(acc0, _mm256_mul_pd(zm, zm));
#endif
        }

        double acc = hsum256_pd(_mm256_add_pd(acc0, acc1));
        for (; i < n; ++i) {
            double oi = o[i], ci = c[i];
            if (std::isnan(oi) || std::isnan(ci) || std::isnan(s0)) {
                if (!ignoreNaN) anyNaN = true;
                continue;
            }
            double z = (ci - oi) * invSig;
            if (std::isnan(z)) {
                if (!ignoreNaN) anyNaN = true;
                continue;
            }
            if (z <= nHigh && z >= -nLow) { acc += z * z; cnt++; }
        }

        *outCount = cnt;
        *outAnyNaN = anyNaN;
        return acc;

    } else {
        mwSize i = 0;
        for (; i + V2 <= n; i += V2) {
            __m256d o0 = _mm256_loadu_pd(o + i);
            __m256d c0 = _mm256_loadu_pd(c + i);
            __m256d s0v= _mm256_loadu_pd(s + i);
            __m256d z0 = _mm256_div_pd(_mm256_sub_pd(c0, o0), s0v);

            if (!ignoreNaN) {
                __m256d nan0 = _mm256_cmp_pd(o0, o0, _CMP_UNORD_Q);
                nan0 = _mm256_or_pd(nan0, _mm256_cmp_pd(c0, c0, _CMP_UNORD_Q));
                nan0 = _mm256_or_pd(nan0, _mm256_cmp_pd(s0v, s0v, _CMP_UNORD_Q));
                nan0 = _mm256_or_pd(nan0, _mm256_cmp_pd(z0, z0, _CMP_UNORD_Q));
                if (_mm256_movemask_pd(nan0)) anyNaN = true;
            }

            __m256d m0 = _mm256_and_pd(
                _mm256_cmp_pd(z0, vHigh, _CMP_LE_OQ),
                _mm256_cmp_pd(z0, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256d ok0 = _mm256_and_pd(_mm256_cmp_pd(z0, z0, _CMP_ORD_Q),
                                            _mm256_cmp_pd(s0v, s0v, _CMP_ORD_Q));
                m0 = _mm256_and_pd(m0, ok0);
            }

            int mask0 = _mm256_movemask_pd(m0);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask0);

            __m256d z0m = _mm256_and_pd(z0, m0);
#ifdef __FMA__
            acc0 = _mm256_fmadd_pd(z0m, z0m, acc0);
#else
            acc0 = _mm256_add_pd(acc0, _mm256_mul_pd(z0m, z0m));
#endif

            __m256d o1 = _mm256_loadu_pd(o + i + V);
            __m256d c1 = _mm256_loadu_pd(c + i + V);
            __m256d s1v= _mm256_loadu_pd(s + i + V);
            __m256d z1 = _mm256_div_pd(_mm256_sub_pd(c1, o1), s1v);

            if (!ignoreNaN) {
                __m256d nan1 = _mm256_cmp_pd(o1, o1, _CMP_UNORD_Q);
                nan1 = _mm256_or_pd(nan1, _mm256_cmp_pd(c1, c1, _CMP_UNORD_Q));
                nan1 = _mm256_or_pd(nan1, _mm256_cmp_pd(s1v, s1v, _CMP_UNORD_Q));
                nan1 = _mm256_or_pd(nan1, _mm256_cmp_pd(z1, z1, _CMP_UNORD_Q));
                if (_mm256_movemask_pd(nan1)) anyNaN = true;
            }

            __m256d m1 = _mm256_and_pd(
                _mm256_cmp_pd(z1, vHigh, _CMP_LE_OQ),
                _mm256_cmp_pd(z1, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256d ok1 = _mm256_and_pd(_mm256_cmp_pd(z1, z1, _CMP_ORD_Q),
                                            _mm256_cmp_pd(s1v, s1v, _CMP_ORD_Q));
                m1 = _mm256_and_pd(m1, ok1);
            }

            int mask1 = _mm256_movemask_pd(m1);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask1);

            __m256d z1m = _mm256_and_pd(z1, m1);
#ifdef __FMA__
            acc1 = _mm256_fmadd_pd(z1m, z1m, acc1);
#else
            acc1 = _mm256_add_pd(acc1, _mm256_mul_pd(z1m, z1m));
#endif
        }

        for (; i + 4 <= n; i += 4) {
            __m256d vo = _mm256_loadu_pd(o + i);
            __m256d vc = _mm256_loadu_pd(c + i);
            __m256d vs = _mm256_loadu_pd(s + i);
            __m256d z  = _mm256_div_pd(_mm256_sub_pd(vc, vo), vs);

            if (!ignoreNaN) {
                __m256d nan = _mm256_cmp_pd(vo, vo, _CMP_UNORD_Q);
                nan = _mm256_or_pd(nan, _mm256_cmp_pd(vc, vc, _CMP_UNORD_Q));
                nan = _mm256_or_pd(nan, _mm256_cmp_pd(vs, vs, _CMP_UNORD_Q));
                nan = _mm256_or_pd(nan, _mm256_cmp_pd(z, z, _CMP_UNORD_Q));
                if (_mm256_movemask_pd(nan)) anyNaN = true;
            }

            __m256d m = _mm256_and_pd(
                _mm256_cmp_pd(z, vHigh, _CMP_LE_OQ),
                _mm256_cmp_pd(z, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256d ok = _mm256_and_pd(_mm256_cmp_pd(z, z, _CMP_ORD_Q),
                                           _mm256_cmp_pd(vs, vs, _CMP_ORD_Q));
                m = _mm256_and_pd(m, ok);
            }

            int mask = _mm256_movemask_pd(m);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask);

            __m256d zm = _mm256_and_pd(z, m);
#ifdef __FMA__
            acc0 = _mm256_fmadd_pd(zm, zm, acc0);
#else
            acc0 = _mm256_add_pd(acc0, _mm256_mul_pd(zm, zm));
#endif
        }

        double acc = hsum256_pd(_mm256_add_pd(acc0, acc1));
        for (; i < n; ++i) {
            double oi = o[i], ci = c[i], si = s[i];
            if (std::isnan(oi) || std::isnan(ci) || std::isnan(si)) {
                if (!ignoreNaN) anyNaN = true;
                continue;
            }
            double z = (ci - oi) / si;
            if (std::isnan(z)) {
                if (!ignoreNaN) anyNaN = true;
                continue;
            }
            if (z <= nHigh && z >= -nLow) { acc += z * z; cnt++; }
        }

        *outCount = cnt;
        *outAnyNaN = anyNaN;
        return acc;
    }
}

// ---- Core: float ----
static double chi2_float_clip_count_nan(const float* o, const float* c,
                                        const float* s, bool sScalar, double s0,
                                        double nLow, double nHigh,
                                        bool ignoreNaN,
                                        mwSize n, uint64_t* outCount, bool* outAnyNaN)
{
    const mwSize V = 8;
    const mwSize V2 = 16;

    const __m256 vHigh = _mm256_set1_ps((float)nHigh);
    const __m256 vLow  = _mm256_set1_ps((float)(-nLow));

    __m256 acc0 = _mm256_setzero_ps();
    __m256 acc1 = _mm256_setzero_ps();
    uint64_t cnt = 0;
    bool anyNaN = false;

    if (sScalar) {
        if (!ignoreNaN && std::isnan(s0)) anyNaN = true;
        const float invSig = (float)(1.0 / s0);
        const __m256 vInvSig = _mm256_set1_ps(invSig);

        mwSize i = 0;
        for (; i + V2 <= n; i += V2) {
            __m256 o0 = _mm256_loadu_ps(o + i);
            __m256 c0 = _mm256_loadu_ps(c + i);
            __m256 z0 = _mm256_mul_ps(_mm256_sub_ps(c0, o0), vInvSig);

            if (!ignoreNaN) {
                __m256 nan0 = _mm256_cmp_ps(o0, o0, _CMP_UNORD_Q);
                nan0 = _mm256_or_ps(nan0, _mm256_cmp_ps(c0, c0, _CMP_UNORD_Q));
                nan0 = _mm256_or_ps(nan0, _mm256_cmp_ps(z0, z0, _CMP_UNORD_Q));
                if (_mm256_movemask_ps(nan0)) anyNaN = true;
            }

            __m256 m0 = _mm256_and_ps(
                _mm256_cmp_ps(z0, vHigh, _CMP_LE_OQ),
                _mm256_cmp_ps(z0, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256 ok0 = _mm256_cmp_ps(z0, z0, _CMP_ORD_Q);
                m0 = _mm256_and_ps(m0, ok0);
            }

            int mask0 = _mm256_movemask_ps(m0);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask0);

            __m256 z0m = _mm256_and_ps(z0, m0);
#ifdef __FMA__
            acc0 = _mm256_fmadd_ps(z0m, z0m, acc0);
#else
            acc0 = _mm256_add_ps(acc0, _mm256_mul_ps(z0m, z0m));
#endif

            __m256 o1 = _mm256_loadu_ps(o + i + V);
            __m256 c1 = _mm256_loadu_ps(c + i + V);
            __m256 z1 = _mm256_mul_ps(_mm256_sub_ps(c1, o1), vInvSig);

            if (!ignoreNaN) {
                __m256 nan1 = _mm256_cmp_ps(o1, o1, _CMP_UNORD_Q);
                nan1 = _mm256_or_ps(nan1, _mm256_cmp_ps(c1, c1, _CMP_UNORD_Q));
                nan1 = _mm256_or_ps(nan1, _mm256_cmp_ps(z1, z1, _CMP_UNORD_Q));
                if (_mm256_movemask_ps(nan1)) anyNaN = true;
            }

            __m256 m1 = _mm256_and_ps(
                _mm256_cmp_ps(z1, vHigh, _CMP_LE_OQ),
                _mm256_cmp_ps(z1, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256 ok1 = _mm256_cmp_ps(z1, z1, _CMP_ORD_Q);
                m1 = _mm256_and_ps(m1, ok1);
            }

            int mask1 = _mm256_movemask_ps(m1);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask1);

            __m256 z1m = _mm256_and_ps(z1, m1);
#ifdef __FMA__
            acc1 = _mm256_fmadd_ps(z1m, z1m, acc1);
#else
            acc1 = _mm256_add_ps(acc1, _mm256_mul_ps(z1m, z1m));
#endif
        }

        for (; i + V <= n; i += V) {
            __m256 vo = _mm256_loadu_ps(o + i);
            __m256 vc = _mm256_loadu_ps(c + i);
            __m256 z  = _mm256_mul_ps(_mm256_sub_ps(vc, vo), vInvSig);

            if (!ignoreNaN) {
                __m256 nan = _mm256_cmp_ps(vo, vo, _CMP_UNORD_Q);
                nan = _mm256_or_ps(nan, _mm256_cmp_ps(vc, vc, _CMP_UNORD_Q));
                nan = _mm256_or_ps(nan, _mm256_cmp_ps(z, z, _CMP_UNORD_Q));
                if (_mm256_movemask_ps(nan)) anyNaN = true;
            }

            __m256 m = _mm256_and_ps(
                _mm256_cmp_ps(z, vHigh, _CMP_LE_OQ),
                _mm256_cmp_ps(z, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256 ok = _mm256_cmp_ps(z, z, _CMP_ORD_Q);
                m = _mm256_and_ps(m, ok);
            }

            int mask = _mm256_movemask_ps(m);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask);

            __m256 zm = _mm256_and_ps(z, m);
#ifdef __FMA__
            acc0 = _mm256_fmadd_ps(zm, zm, acc0);
#else
            acc0 = _mm256_add_ps(acc0, _mm256_mul_ps(zm, zm));
#endif
        }

        double acc = hsum256_ps(_mm256_add_ps(acc0, acc1));
        for (; i < n; ++i) {
            float oi = o[i], ci = c[i];
            if (std::isnan(oi) || std::isnan(ci) || std::isnan((float)s0)) {
                if (!ignoreNaN) anyNaN = true;
                continue;
            }
            double z = ((double)ci - (double)oi) * (double)invSig;
            if (std::isnan(z)) {
                if (!ignoreNaN) anyNaN = true;
                continue;
            }
            if (z <= nHigh && z >= -nLow) { acc += z * z; cnt++; }
        }

        *outCount = cnt;
        *outAnyNaN = anyNaN;
        return acc;

    } else {
        mwSize i = 0;
        for (; i + V2 <= n; i += V2) {
            __m256 o0 = _mm256_loadu_ps(o + i);
            __m256 c0 = _mm256_loadu_ps(c + i);
            __m256 s0v= _mm256_loadu_ps(s + i);
            __m256 z0 = _mm256_div_ps(_mm256_sub_ps(c0, o0), s0v);

            if (!ignoreNaN) {
                __m256 nan0 = _mm256_cmp_ps(o0, o0, _CMP_UNORD_Q);
                nan0 = _mm256_or_ps(nan0, _mm256_cmp_ps(c0, c0, _CMP_UNORD_Q));
                nan0 = _mm256_or_ps(nan0, _mm256_cmp_ps(s0v, s0v, _CMP_UNORD_Q));
                nan0 = _mm256_or_ps(nan0, _mm256_cmp_ps(z0, z0, _CMP_UNORD_Q));
                if (_mm256_movemask_ps(nan0)) anyNaN = true;
            }

            __m256 m0 = _mm256_and_ps(
                _mm256_cmp_ps(z0, vHigh, _CMP_LE_OQ),
                _mm256_cmp_ps(z0, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256 ok0 = _mm256_and_ps(_mm256_cmp_ps(z0, z0, _CMP_ORD_Q),
                                           _mm256_cmp_ps(s0v, s0v, _CMP_ORD_Q));
                m0 = _mm256_and_ps(m0, ok0);
            }

            int mask0 = _mm256_movemask_ps(m0);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask0);

            __m256 z0m = _mm256_and_ps(z0, m0);
#ifdef __FMA__
            acc0 = _mm256_fmadd_ps(z0m, z0m, acc0);
#else
            acc0 = _mm256_add_ps(acc0, _mm256_mul_ps(z0m, z0m));
#endif

            __m256 o1 = _mm256_loadu_ps(o + i + V);
            __m256 c1 = _mm256_loadu_ps(c + i + V);
            __m256 s1v= _mm256_loadu_ps(s + i + V);
            __m256 z1 = _mm256_div_ps(_mm256_sub_ps(c1, o1), s1v);

            if (!ignoreNaN) {
                __m256 nan1 = _mm256_cmp_ps(o1, o1, _CMP_UNORD_Q);
                nan1 = _mm256_or_ps(nan1, _mm256_cmp_ps(c1, c1, _CMP_UNORD_Q));
                nan1 = _mm256_or_ps(nan1, _mm256_cmp_ps(s1v, s1v, _CMP_UNORD_Q));
                nan1 = _mm256_or_ps(nan1, _mm256_cmp_ps(z1, z1, _CMP_UNORD_Q));
                if (_mm256_movemask_ps(nan1)) anyNaN = true;
            }

            __m256 m1 = _mm256_and_ps(
                _mm256_cmp_ps(z1, vHigh, _CMP_LE_OQ),
                _mm256_cmp_ps(z1, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256 ok1 = _mm256_and_ps(_mm256_cmp_ps(z1, z1, _CMP_ORD_Q),
                                           _mm256_cmp_ps(s1v, s1v, _CMP_ORD_Q));
                m1 = _mm256_and_ps(m1, ok1);
            }

            int mask1 = _mm256_movemask_ps(m1);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask1);

            __m256 z1m = _mm256_and_ps(z1, m1);
#ifdef __FMA__
            acc1 = _mm256_fmadd_ps(z1m, z1m, acc1);
#else
            acc1 = _mm256_add_ps(acc1, _mm256_mul_ps(z1m, z1m));
#endif
        }

        for (; i + V <= n; i += V) {
            __m256 vo = _mm256_loadu_ps(o + i);
            __m256 vc = _mm256_loadu_ps(c + i);
            __m256 vs = _mm256_loadu_ps(s + i);
            __m256 z  = _mm256_div_ps(_mm256_sub_ps(vc, vo), vs);

            if (!ignoreNaN) {
                __m256 nan = _mm256_cmp_ps(vo, vo, _CMP_UNORD_Q);
                nan = _mm256_or_ps(nan, _mm256_cmp_ps(vc, vc, _CMP_UNORD_Q));
                nan = _mm256_or_ps(nan, _mm256_cmp_ps(vs, vs, _CMP_UNORD_Q));
                nan = _mm256_or_ps(nan, _mm256_cmp_ps(z, z, _CMP_UNORD_Q));
                if (_mm256_movemask_ps(nan)) anyNaN = true;
            }

            __m256 m = _mm256_and_ps(
                _mm256_cmp_ps(z, vHigh, _CMP_LE_OQ),
                _mm256_cmp_ps(z, vLow,  _CMP_GE_OQ)
            );
            if (ignoreNaN) {
                __m256 ok = _mm256_and_ps(_mm256_cmp_ps(z, z, _CMP_ORD_Q),
                                          _mm256_cmp_ps(vs, vs, _CMP_ORD_Q));
                m = _mm256_and_ps(m, ok);
            }

            int mask = _mm256_movemask_ps(m);
            cnt += (uint64_t)__builtin_popcount((unsigned)mask);

            __m256 zm = _mm256_and_ps(z, m);
#ifdef __FMA__
            acc0 = _mm256_fmadd_ps(zm, zm, acc0);
#else
            acc0 = _mm256_add_ps(acc0, _mm256_mul_ps(zm, zm));
#endif
        }

        double acc = hsum256_ps(_mm256_add_ps(acc0, acc1));
        for (; i < n; ++i) {
            float oi = o[i], ci = c[i], si = s[i];
            if (std::isnan(oi) || std::isnan(ci) || std::isnan(si)) {
                if (!ignoreNaN) anyNaN = true;
                continue;
            }
            double z = ((double)ci - (double)oi) / (double)si;
            if (std::isnan(z)) {
                if (!ignoreNaN) anyNaN = true;
                continue;
            }
            if (z <= nHigh && z >= -nLow) { acc += z * z; cnt++; }
        }

        *outCount = cnt;
        *outAnyNaN = anyNaN;
        return acc;
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // IgnoreNaN default = true => nrhs can be 4 or 5
    if (nrhs != 4 && nrhs != 5)
        die("Usage: [Chi2,Nused] = chi2_vec(Obs, Calc, Sigma, [NsigmaLow NsigmaHigh], IgnoreNaN=true)");

    if (nlhs < 1 || nlhs > 2) die("One or two outputs.");

    const mxArray* ObsA   = prhs[0];
    const mxArray* CalcA  = prhs[1];
    const mxArray* SigmaA = prhs[2];
    const mxArray* ThrA   = prhs[3];

    const bool ignoreNaN = (nrhs == 5) ? parseBoolScalar(prhs[4]) : true;

    if (mxIsComplex(ObsA) || mxIsComplex(CalcA) || mxIsComplex(SigmaA) || mxIsComplex(ThrA))
        die("Inputs must be real (non-complex).");

    if (!(mxIsSingle(ObsA) || mxIsDouble(ObsA)))
        die("Obs must be single or double.");
    if (mxGetClassID(CalcA) != mxGetClassID(ObsA))
        die("Obs and Calc must have the same class.");
    if (!isVector(ObsA) || !isVector(CalcA))
        die("Obs and Calc must be vectors (1xN or Nx1).");

    const mwSize n = mxGetNumberOfElements(ObsA);
    if (mxGetNumberOfElements(CalcA) != n)
        die("Obs and Calc must have the same length.");

    if (!((mxIsSingle(ThrA) || mxIsDouble(ThrA)) && mxGetNumberOfElements(ThrA) == 2))
        die("Threshold must be [NsigmaLow NsigmaHigh] (single/double, 2 elements).");

    double nLow = 0.0, nHigh = 0.0;
    if (mxIsDouble(ThrA)) {
        const double* t = (const double*)mxGetData(ThrA);
        nLow = t[0]; nHigh = t[1];
    } else {
        const float* t = (const float*)mxGetData(ThrA);
        nLow = (double)t[0]; nHigh = (double)t[1];
    }
    if (!(mxIsFinite(nLow) && mxIsFinite(nHigh))) die("Nsigma values must be finite.");
    if (nLow < 0 || nHigh < 0) die("NsigmaLow and NsigmaHigh must be >= 0.");

    const bool sigmaIsScalar = (mxGetNumberOfElements(SigmaA) == 1);
    if (!sigmaIsScalar) {
        if (!isVector(SigmaA)) die("Sigma must be a scalar or a vector.");
        if (mxGetNumberOfElements(SigmaA) != n) die("Sigma vector must match Obs length.");
        if (mxGetClassID(SigmaA) != mxGetClassID(ObsA))
            die("If Sigma is a vector, it must have the same class as Obs.");
    } else {
        if (!(mxIsSingle(SigmaA) || mxIsDouble(SigmaA)))
            die("Sigma scalar must be single or double.");
    }

    mxClassID cid = mxGetClassID(ObsA);
    plhs[0] = mxCreateNumericMatrix(1, 1, cid, mxREAL);
    if (nlhs == 2) plhs[1] = mxCreateDoubleScalar(0.0);

    void* outPtr = mxGetData(plhs[0]);
    const void* obsPtr = mxGetData(ObsA);
    const void* calPtr = mxGetData(CalcA);
    const void* sigPtr = mxGetData(SigmaA);

    uint64_t count = 0;
    bool anyNaN = false;

    if (cid == mxDOUBLE_CLASS) {
        double s0 = 0.0;
        if (sigmaIsScalar) s0 = mxIsSingle(SigmaA) ? (double)(*(const float*)sigPtr) : *(const double*)sigPtr;

        double chi2 = chi2_double_clip_count_nan((const double*)obsPtr, (const double*)calPtr,
                                                 (const double*)sigPtr, sigmaIsScalar, s0,
                                                 nLow, nHigh, ignoreNaN,
                                                 n, &count, &anyNaN);
        if (!ignoreNaN && anyNaN) chi2 = mxGetNaN();
        *(double*)outPtr = chi2;

    } else { // single
        double s0 = 0.0;
        if (sigmaIsScalar) s0 = mxIsSingle(SigmaA) ? (double)(*(const float*)sigPtr) : *(const double*)sigPtr;

        double chi2 = chi2_float_clip_count_nan((const float*)obsPtr, (const float*)calPtr,
                                                (const float*)sigPtr, sigmaIsScalar, s0,
                                                nLow, nHigh, ignoreNaN,
                                                n, &count, &anyNaN);

        float outv = (float)chi2;
        if (!ignoreNaN && anyNaN) outv = std::numeric_limits<float>::quiet_NaN();
        *(float*)outPtr = outv;
    }

    if (nlhs == 2) {
        *(double*)mxGetData(plhs[1]) = (double)count;
    }
}

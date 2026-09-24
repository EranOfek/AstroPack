#include "mex.h"
#include <immintrin.h>
#include <cmath>
#include <cstdint>
#include <algorithm>

#ifdef _OPENMP
  #include <omp.h>
#endif

static void die(const char* msg) {
    mexErrMsgIdAndTxt("sigma_clip_cube_fast:err", "%s", msg);
}

static inline bool isfinite_d(double x) { return mxIsFinite(x); }

// ------------------------ DOUBLE PATH ------------------------
static void pass1_double(const double* cube, double* sum, double* sumsq, double* cnt,
                         mwSize P, mwSize K)
{
    // Parallel over pixel blocks for contiguous memory
    const mwSize BLK = 1u << 15; // 32768 pixels (~256KB per array)
    #pragma omp parallel for schedule(static)
    for (mwIndex p0 = 0; p0 < (mwIndex)P; p0 += (mwIndex)BLK) {
        mwSize p1 = (mwSize)std::min<mwIndex>(p0 + (mwIndex)BLK, (mwIndex)P);

        for (mwSize k = 0; k < K; ++k) {
            const double* sl = cube + k * P;

            mwSize p = (mwSize)p0;
            const __m256d vone = _mm256_set1_pd(1.0);

            for (; p + 4 <= p1; p += 4) {
                __m256d x = _mm256_loadu_pd(sl + p);

                // valid mask: ordered => not NaN
                __m256d m = _mm256_cmp_pd(x, x, _CMP_ORD_Q);

                // xmasked = x if valid else 0
                __m256d xm = _mm256_and_pd(x, m);

                __m256d s  = _mm256_loadu_pd(sum + p);
                __m256d ss = _mm256_loadu_pd(sumsq + p);
                __m256d c  = _mm256_loadu_pd(cnt + p);

#ifdef __FMA__
                ss = _mm256_fmadd_pd(xm, xm, ss);
#else
                ss = _mm256_add_pd(ss, _mm256_mul_pd(xm, xm));
#endif
                s  = _mm256_add_pd(s, xm);

                // add 1.0 for valid lanes
                __m256d inc = _mm256_and_pd(m, vone);
                c = _mm256_add_pd(c, inc);

                _mm256_storeu_pd(sum + p, s);
                _mm256_storeu_pd(sumsq + p, ss);
                _mm256_storeu_pd(cnt + p, c);
            }

            for (; p < p1; ++p) {
                double x = sl[p];
                if (!std::isnan(x)) {
                    sum[p]   += x;
                    sumsq[p] += x * x;
                    cnt[p]   += 1.0;
                }
            }
        }
    }
}

static void mean_std_double(const double* sum, const double* sumsq, const double* cnt,
                            double* mean, double* sd, mwSize P)
{
    #pragma omp parallel for schedule(static)
    for (mwIndex p = 0; p < (mwIndex)P; ++p) {
        double c = cnt[p];
        if (c <= 0.0) {
            mean[p] = mxGetNaN();
            sd[p]   = mxGetNaN();
        } else if (c == 1.0) {
            mean[p] = sum[p];
            sd[p]   = 0.0;
        } else {
            double mu = sum[p] / c;
            double v  = (sumsq[p] - (sum[p]*sum[p]) / c) / (c - 1.0); // sample variance
            if (v < 0.0) v = 0.0; // numeric guard
            mean[p] = mu;
            sd[p]   = std::sqrt(v);
        }
    }
}

static void pass2_double(const double* cube,
                         const double* mean, const double* sd,
                         double* sum2, double* cnt2,
                         mwSize P, mwSize K,
                         double lowNs, double highNs)
{
    const mwSize BLK = 1u << 15;

    #pragma omp parallel for schedule(static)
    for (mwIndex p0 = 0; p0 < (mwIndex)P; p0 += (mwIndex)BLK) {
        mwSize p1 = (mwSize)std::min<mwIndex>(p0 + (mwIndex)BLK, (mwIndex)P);

        const __m256d vLowNs  = _mm256_set1_pd(lowNs);
        const __m256d vHighNs = _mm256_set1_pd(highNs);
        const __m256d vone    = _mm256_set1_pd(1.0);

        for (mwSize k = 0; k < K; ++k) {
            const double* sl = cube + k * P;

            mwSize p = (mwSize)p0;
            for (; p + 4 <= p1; p += 4) {
                __m256d x  = _mm256_loadu_pd(sl + p);
                __m256d mu = _mm256_loadu_pd(mean + p);
                __m256d s  = _mm256_loadu_pd(sd + p);

                // baseline stats exist? mu ordered
                __m256d mStats = _mm256_cmp_pd(mu, mu, _CMP_ORD_Q);

                // x not NaN
                __m256d mX = _mm256_cmp_pd(x, x, _CMP_ORD_Q);

                // bounds: [mu - lowNs*s, mu + highNs*s]
#ifdef __FMA__
                __m256d lo = _mm256_fnmadd_pd(vLowNs,  s, mu); // mu - lowNs*s
                __m256d hi = _mm256_fmadd_pd (vHighNs, s, mu); // mu + highNs*s
#else
                __m256d lo = _mm256_sub_pd(mu, _mm256_mul_pd(vLowNs, s));
                __m256d hi = _mm256_add_pd(mu, _mm256_mul_pd(vHighNs, s));
#endif
                __m256d mLo = _mm256_cmp_pd(x, lo, _CMP_GE_OQ);
                __m256d mHi = _mm256_cmp_pd(x, hi, _CMP_LE_OQ);

                __m256d m = _mm256_and_pd(mStats, _mm256_and_pd(mX, _mm256_and_pd(mLo, mHi)));

                __m256d xm = _mm256_and_pd(x, m);

                __m256d s2 = _mm256_loadu_pd(sum2 + p);
                __m256d c2 = _mm256_loadu_pd(cnt2 + p);

                s2 = _mm256_add_pd(s2, xm);
                __m256d inc = _mm256_and_pd(m, vone);
                c2 = _mm256_add_pd(c2, inc);

                _mm256_storeu_pd(sum2 + p, s2);
                _mm256_storeu_pd(cnt2 + p, c2);
            }

            for (; p < p1; ++p) {
                double x = sl[p];
                if (std::isnan(x)) continue;

                double mu = mean[p];
                if (std::isnan(mu)) continue;

                double sdev = sd[p];
                double lo = mu - lowNs * sdev;
                double hi = mu + highNs * sdev;

                if (x >= lo && x <= hi) {
                    sum2[p] += x;
                    cnt2[p] += 1.0;
                }
            }
        }
    }
}

// ------------------------ FLOAT PATH ------------------------
// Keep accumulators in float for speed; counts are float but exact for typical K.
static void pass1_float(const float* cube, float* sum, float* sumsq, float* cnt,
                        mwSize P, mwSize K)
{
    const mwSize BLK = 1u << 15;

    #pragma omp parallel for schedule(static)
    for (mwIndex p0 = 0; p0 < (mwIndex)P; p0 += (mwIndex)BLK) {
        mwSize p1 = (mwSize)std::min<mwIndex>(p0 + (mwIndex)BLK, (mwIndex)P);

        for (mwSize k = 0; k < K; ++k) {
            const float* sl = cube + k * P;

            mwSize p = (mwSize)p0;
            const __m256 vone = _mm256_set1_ps(1.0f);

            for (; p + 8 <= p1; p += 8) {
                __m256 x = _mm256_loadu_ps(sl + p);
                __m256 m = _mm256_cmp_ps(x, x, _CMP_ORD_Q);
                __m256 xm = _mm256_and_ps(x, m);

                __m256 s  = _mm256_loadu_ps(sum + p);
                __m256 ss = _mm256_loadu_ps(sumsq + p);
                __m256 c  = _mm256_loadu_ps(cnt + p);

#ifdef __FMA__
                ss = _mm256_fmadd_ps(xm, xm, ss);
#else
                ss = _mm256_add_ps(ss, _mm256_mul_ps(xm, xm));
#endif
                s  = _mm256_add_ps(s, xm);

                __m256 inc = _mm256_and_ps(m, vone);
                c = _mm256_add_ps(c, inc);

                _mm256_storeu_ps(sum + p, s);
                _mm256_storeu_ps(sumsq + p, ss);
                _mm256_storeu_ps(cnt + p, c);
            }

            for (; p < p1; ++p) {
                float x = sl[p];
                if (!std::isnan((double)x)) {
                    sum[p]   += x;
                    sumsq[p] += x * x;
                    cnt[p]   += 1.0f;
                }
            }
        }
    }
}

static void mean_std_float(const float* sum, const float* sumsq, const float* cnt,
                           float* mean, float* sd, mwSize P)
{
    #pragma omp parallel for schedule(static)
    for (mwIndex p = 0; p < (mwIndex)P; ++p) {
        float c = cnt[p];
        if (c <= 0.0f) {
            mean[p] = (float)mxGetNaN();
            sd[p]   = (float)mxGetNaN();
        } else if (c == 1.0f) {
            mean[p] = sum[p];
            sd[p]   = 0.0f;
        } else {
            double cd = (double)c;
            double s  = (double)sum[p];
            double ss = (double)sumsq[p];
            double mu = s / cd;
            double v  = (ss - (s*s)/cd) / (cd - 1.0); // sample variance
            if (v < 0.0) v = 0.0;
            mean[p] = (float)mu;
            sd[p]   = (float)std::sqrt(v);
        }
    }
}

static void pass2_float(const float* cube,
                        const float* mean, const float* sd,
                        float* sum2, float* cnt2,
                        mwSize P, mwSize K,
                        float lowNs, float highNs)
{
    const mwSize BLK = 1u << 15;

    #pragma omp parallel for schedule(static)
    for (mwIndex p0 = 0; p0 < (mwIndex)P; p0 += (mwIndex)BLK) {
        mwSize p1 = (mwSize)std::min<mwIndex>(p0 + (mwIndex)BLK, (mwIndex)P);

        const __m256 vLowNs  = _mm256_set1_ps(lowNs);
        const __m256 vHighNs = _mm256_set1_ps(highNs);
        const __m256 vone    = _mm256_set1_ps(1.0f);

        for (mwSize k = 0; k < K; ++k) {
            const float* sl = cube + k * P;

            mwSize p = (mwSize)p0;
            for (; p + 8 <= p1; p += 8) {
                __m256 x  = _mm256_loadu_ps(sl + p);
                __m256 mu = _mm256_loadu_ps(mean + p);
                __m256 s  = _mm256_loadu_ps(sd + p);

                __m256 mStats = _mm256_cmp_ps(mu, mu, _CMP_ORD_Q);
                __m256 mX     = _mm256_cmp_ps(x,  x,  _CMP_ORD_Q);

#ifdef __FMA__
                __m256 lo = _mm256_fnmadd_ps(vLowNs,  s, mu);
                __m256 hi = _mm256_fmadd_ps (vHighNs, s, mu);
#else
                __m256 lo = _mm256_sub_ps(mu, _mm256_mul_ps(vLowNs, s));
                __m256 hi = _mm256_add_ps(mu, _mm256_mul_ps(vHighNs, s));
#endif
                __m256 mLo = _mm256_cmp_ps(x, lo, _CMP_GE_OQ);
                __m256 mHi = _mm256_cmp_ps(x, hi, _CMP_LE_OQ);

                __m256 m = _mm256_and_ps(mStats, _mm256_and_ps(mX, _mm256_and_ps(mLo, mHi)));

                __m256 xm = _mm256_and_ps(x, m);

                __m256 s2 = _mm256_loadu_ps(sum2 + p);
                __m256 c2 = _mm256_loadu_ps(cnt2 + p);

                s2 = _mm256_add_ps(s2, xm);
                __m256 inc = _mm256_and_ps(m, vone);
                c2 = _mm256_add_ps(c2, inc);

                _mm256_storeu_ps(sum2 + p, s2);
                _mm256_storeu_ps(cnt2 + p, c2);
            }

            for (; p < p1; ++p) {
                float x = sl[p];
                if (std::isnan((double)x)) continue;

                float mu = mean[p];
                if (std::isnan((double)mu)) continue;

                float sdev = sd[p];
                float lo = mu - lowNs * sdev;
                float hi = mu + highNs * sdev;

                if (x >= lo && x <= hi) {
                    sum2[p] += x;
                    cnt2[p] += 1.0f;
                }
            }
        }
    }
}

// ------------------------ MEX ENTRY ------------------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 2) die("Usage: [Img,Nused] = sigma_clip_cube_fast(Cube, [LowNsigma HighNsigma])");
    if (nlhs < 1 || nlhs > 2) die("One or two outputs.");

    const mxArray* CubeA = prhs[0];
    const mxArray* ThrA  = prhs[1];

    if (mxIsComplex(CubeA) || mxIsComplex(ThrA))
        die("Inputs must be real (non-complex).");

    if (!(mxIsSingle(CubeA) || mxIsDouble(CubeA)))
        die("Cube must be single or double.");

    if (!((mxIsSingle(ThrA) || mxIsDouble(ThrA)) && mxGetNumberOfElements(ThrA) == 2))
        die("Threshold must be [LowNsigma HighNsigma] (single/double, 2 elements).");

    if (mxGetNumberOfDimensions(CubeA) != 3)
        die("Cube must be 3D: size(Cube) = [M N K].");

    const mwSize* dims = mxGetDimensions(CubeA);
    const mwSize M = dims[0], N = dims[1], K = dims[2];
    if (K < 1) die("Third dimension K must be >= 1.");

    double lowNs=0.0, highNs=0.0;
    if (mxIsDouble(ThrA)) {
        const double* t = (const double*)mxGetData(ThrA);
        lowNs  = t[0];
        highNs = t[1];
    } else {
        const float* t = (const float*)mxGetData(ThrA);
        lowNs  = (double)t[0];
        highNs = (double)t[1];
    }
    if (!isfinite_d(lowNs) || !isfinite_d(highNs)) die("Nsigma values must be finite.");
    if (lowNs < 0.0 || highNs < 0.0) die("LowNsigma and HighNsigma must be >= 0.");

    const mwSize P = M * N;

    // Outputs
    mxClassID cid = mxGetClassID(CubeA);
    plhs[0] = mxCreateNumericMatrix(M, N, cid, mxREAL);
    if (nlhs == 2) {
        plhs[1] = mxCreateDoubleMatrix(M, N, mxREAL);
    }

    void* outImg = mxGetData(plhs[0]);
    double* outN = (nlhs == 2) ? (double*)mxGetData(plhs[1]) : nullptr;

    // If Nused not requested, still compute and discard
    double* tmpN = outN ? outN : (double*)mxCalloc(P, sizeof(double));

    const void* cubePtr = mxGetData(CubeA);

    if (cid == mxDOUBLE_CLASS) {
        // allocate accum arrays (double)
        double* sum   = (double*)mxCalloc(P, sizeof(double));
        double* sumsq = (double*)mxCalloc(P, sizeof(double));
        double* cnt   = (double*)mxCalloc(P, sizeof(double));
        double* mean  = (double*)mxCalloc(P, sizeof(double));
        double* sd    = (double*)mxCalloc(P, sizeof(double));
        double* sum2  = (double*)mxCalloc(P, sizeof(double));
        double* cnt2  = (double*)mxCalloc(P, sizeof(double));

        pass1_double((const double*)cubePtr, sum, sumsq, cnt, P, K);
        mean_std_double(sum, sumsq, cnt, mean, sd, P);
        pass2_double((const double*)cubePtr, mean, sd, sum2, cnt2, P, K, lowNs, highNs);

        // output
        double* out = (double*)outImg;
        #pragma omp parallel for schedule(static)
        for (mwIndex p = 0; p < (mwIndex)P; ++p) {
            double c2 = cnt2[p];
            tmpN[p] = c2;
            out[p] = (c2 > 0.0) ? (sum2[p] / c2) : mxGetNaN();
        }

        mxFree(sum); mxFree(sumsq); mxFree(cnt);
        mxFree(mean); mxFree(sd);
        mxFree(sum2); mxFree(cnt2);

    } else { // single
        float lowF  = (float)lowNs;
        float highF = (float)highNs;

        float* sum   = (float*)mxCalloc(P, sizeof(float));
        float* sumsq = (float*)mxCalloc(P, sizeof(float));
        float* cnt   = (float*)mxCalloc(P, sizeof(float));
        float* mean  = (float*)mxCalloc(P, sizeof(float));
        float* sd    = (float*)mxCalloc(P, sizeof(float));
        float* sum2  = (float*)mxCalloc(P, sizeof(float));
        float* cnt2  = (float*)mxCalloc(P, sizeof(float));

        pass1_float((const float*)cubePtr, sum, sumsq, cnt, P, K);
        mean_std_float(sum, sumsq, cnt, mean, sd, P);
        pass2_float((const float*)cubePtr, mean, sd, sum2, cnt2, P, K, lowF, highF);

        float* out = (float*)outImg;
        #pragma omp parallel for schedule(static)
        for (mwIndex p = 0; p < (mwIndex)P; ++p) {
            float c2 = cnt2[p];
            tmpN[p] = (double)c2;
            out[p] = (c2 > 0.0f) ? (sum2[p] / c2) : (float)mxGetNaN();
        }

        mxFree(sum); mxFree(sumsq); mxFree(cnt);
        mxFree(mean); mxFree(sd);
        mxFree(sum2); mxFree(cnt2);
    }

    if (!outN) mxFree(tmpN);
}

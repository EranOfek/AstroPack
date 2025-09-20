// ps_mex_md.cpp
// P = ps_mex_md(T, M, FreqVec)
// T: [N x 1] or [1 x N] real (single/double)
// M: [N x K] real, size(M,1)=numel(T)
// FreqVec: [Nf x 1] or [1 x Nf] real (same class as T/M)
// Output P: [Nf x K], P(j,k) = |sum_valid M(i,k)*exp(-j*2*pi*T(i)*f_j)|^2 / Nvalid(j,k)
// Notes: NaNs in T or M(:,k) are ignored. sin/cos computed once per freq.

#include "mex.h"
#include <cmath>
#include <cstring>
#include <vector>
#include <cstdint>
#ifdef _OPENMP
  #include <omp.h>
#endif

static inline bool isRealFloatOrDouble(const mxArray* a){
    return !mxIsComplex(a) && (mxIsDouble(a) || mxIsSingle(a));
}
static inline bool isVector(const mxArray* a){
    return mxGetNumberOfDimensions(a)==2 && (mxGetM(a)==1 || mxGetN(a)==1);
}

// ---------------- single-precision path (accumulate in double) ----------------
static void compute_ps_single(const float* __restrict T,
                              const float* __restrict M, mwSize N, mwSize K,
                              const float* __restrict F, mwSize Nf,
                              float* __restrict P) // [Nf x K], column-major
{
    std::vector<uint8_t> tvalid(N);
    for (mwSize i=0;i<N;++i) tvalid[i] = !mxIsNaN((double)T[i]);

    const double twoPi = 6.28318530717958647692;

    #pragma omp parallel for if (Nf > 32) schedule(static)
    for (mwSize j=0;j<Nf;++j){
        const double w = -twoPi * (double)F[j];

        // Precompute sin/cos once for this frequency
        std::vector<float> s_tbl(N), c_tbl(N);
        for (mwSize i=0;i<N;++i){
            if (tvalid[i]){
                const float phi = (float)(w * (double)T[i]);
            #if defined(__GLIBC__) || (defined(__GNUC__) && !defined(__APPLE__))
                ::sincosf(phi, &s_tbl[i], &c_tbl[i]);
            #else
                s_tbl[i] = std::sinf(phi);
                c_tbl[i] = std::cosf(phi);
            #endif
            } else {
                s_tbl[i] = 0.0f; c_tbl[i] = 0.0f;
            }
        }

        for (mwSize k=0;k<K;++k){
            const float* __restrict Mk = M + (size_t)k * N;
            double sumR=0.0, sumI=0.0;
            mwSize nvalid=0;

            #pragma omp simd reduction(+:sumR,sumI,nvalid)
            for (mwSize i=0;i<N;++i){
                const float mm = Mk[i];
                if (tvalid[i] && !mxIsNaN((double)mm)){
                    sumR += (double)mm * (double)c_tbl[i];
                    sumI -= (double)mm * (double)s_tbl[i];
                    nvalid++;
                }
            }
            P[j + (size_t)k * Nf] = (nvalid>0) ? (float)((sumR*sumR + sumI*sumI)/(double)nvalid) : 0.0f;
        }
    }
}

// ---------------- double-precision path ----------------
static void compute_ps_double(const double* __restrict T,
                              const double* __restrict M, mwSize N, mwSize K,
                              const double* __restrict F, mwSize Nf,
                              double* __restrict P) // [Nf x K], column-major
{
    std::vector<uint8_t> tvalid(N);
    for (mwSize i=0;i<N;++i) tvalid[i] = !mxIsNaN(T[i]);

    const double twoPi = 6.28318530717958647692;

    #pragma omp parallel for if (Nf > 16) schedule(static)
    for (mwSize j=0;j<Nf;++j){
        const double w = -twoPi * F[j];

        // Precompute sin/cos once for this frequency
        std::vector<double> s_tbl(N), c_tbl(N);
        #pragma omp simd
        for (mwSize i=0;i<N;++i){
            if (tvalid[i]){
                const double phi = w * T[i];
                s_tbl[i] = std::sin(phi);
                c_tbl[i] = std::cos(phi);
            } else {
                s_tbl[i] = 0.0; c_tbl[i] = 0.0;
            }
        }

        for (mwSize k=0;k<K;++k){
            const double* __restrict Mk = M + (size_t)k * N;
            double sumR=0.0, sumI=0.0;
            mwSize nvalid=0;

            #pragma omp simd reduction(+:sumR,sumI,nvalid)
            for (mwSize i=0;i<N;++i){
                const double mm = Mk[i];
                if (tvalid[i] && !mxIsNaN(mm)){
                    sumR += mm * c_tbl[i];
                    sumI -= mm * s_tbl[i];
                    nvalid++;
                }
            }
            P[j + (size_t)k * Nf] = (nvalid>0) ? ((sumR*sumR + sumI*sumI)/(double)nvalid) : 0.0;
        }
    }
}

// ---------------- mex entry ----------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 3) mexErrMsgIdAndTxt("ps_mex_md:nrhs","Usage: P = ps_mex_md(T, M, FreqVec)");
    if (nlhs != 1) mexErrMsgIdAndTxt("ps_mex_md:nlhs","One output required: P (Nf x K).");

    const mxArray* T = prhs[0];
    const mxArray* M = prhs[1];
    const mxArray* F = prhs[2];

    if (!isRealFloatOrDouble(T) || !isRealFloatOrDouble(M) || !isRealFloatOrDouble(F))
        mexErrMsgIdAndTxt("ps_mex_md:type","Inputs must be real single or double.");
    if (mxGetClassID(T)!=mxGetClassID(M) || mxGetClassID(T)!=mxGetClassID(F))
        mexErrMsgIdAndTxt("ps_mex_md:class","All inputs must be the same class.");
    if (!isVector(T) || !isVector(F))
        mexErrMsgIdAndTxt("ps_mex_md:shape","T and FreqVec must be vectors.");
    if (mxGetNumberOfDimensions(M) != 2)
        mexErrMsgIdAndTxt("ps_mex_md:shapeM","M must be 2-D.");

    const mwSize N  = mxGetNumberOfElements(T);
    const mwSize Nf = mxGetNumberOfElements(F);
    if (mxGetM(M) != N)
        mexErrMsgIdAndTxt("ps_mex_md:len","size(M,1) must equal numel(T).");
    const mwSize K = mxGetN(M);

    mwSize dims[2] = { Nf, K };

    if (mxIsDouble(T)){
        plhs[0] = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);
        double* P = mxGetPr(plhs[0]);
        compute_ps_double(mxGetPr(T), mxGetPr(M), N, K, mxGetPr(F), Nf, P);
    } else {
        plhs[0] = mxCreateNumericArray(2, dims, mxSINGLE_CLASS, mxREAL);
        float* P = reinterpret_cast<float*>(mxGetData(plhs[0]));
        const float* Tf = reinterpret_cast<const float*>(mxGetData(T));
        const float* Mf = reinterpret_cast<const float*>(mxGetData(M));
        const float* Ff = reinterpret_cast<const float*>(mxGetData(F));
        compute_ps_single(Tf, Mf, N, K, Ff, Nf, P);
    }
}

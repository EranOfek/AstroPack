// ps_mex.cpp
// PS = ps_mex(T, M, FreqVec)
// Computes power at each frequency:
//   PS(f) = |sum_i M_i * exp(-j*2*pi*T_i*f)|^2 / N
// Returns Nf x 1 vector of powers (single or double depending on inputs).
// This is the "previous" simple kernel (no uniform-T optimizations).

#include "mex.h"
#include <cmath>
#include <cstring>

#ifdef _OPENMP
  #include <omp.h>
#endif

static inline bool isRealFloatOrDouble(const mxArray* a) {
    return !mxIsComplex(a) && (mxIsDouble(a) || mxIsSingle(a));
}
static inline bool isVector(const mxArray* a) {
    return mxGetNumberOfDimensions(a) == 2 && (mxGetM(a) == 1 || mxGetN(a) == 1);
}

// ---------------- single-precision path ----------------
static void compute_ps_single(const float* __restrict T,
                              const float* __restrict M,
                              mwSize N,
                              const float* __restrict F,
                              mwSize Nf,
                              float* __restrict outPow)    // size Nf
{
    const double twoPi = 6.28318530717958647692;

    #pragma omp parallel for if (Nf > 64) schedule(static)
    for (mwSize j = 0; j < Nf; ++j) {
        const double w = -twoPi * (double)F[j];   // -2*pi*freq
        double sumR = 0.0, sumI = 0.0;            // accumulate in double for accuracy
        for (mwSize i = 0; i < N; ++i) {
            const double phi = w * (double)T[i];
        #if defined(__GLIBC__) || (defined(__GNUC__) && !defined(__APPLE__))
            float s, c;
            ::sincosf((float)phi, &s, &c);       // shared argument reduction (fast)
        #else
            const float s = std::sin((float)phi);
            const float c = std::cos((float)phi);
        #endif
            const double mm = (double)M[i];
            sumR += mm * (double)c;              // real += M * cos(phi)
            sumI -= mm * (double)s;              // imag -= M * sin(phi)
        }
        const double p = (sumR*sumR + sumI*sumI) / (double)N;
        outPow[j] = (float)p;
    }
}

// ---------------- double-precision path ----------------
static void compute_ps_double(const double* __restrict T,
                              const double* __restrict M,
                              mwSize N,
                              const double* __restrict F,
                              mwSize Nf,
                              double* __restrict outPow)   // size Nf
{
    const double twoPi = 6.28318530717958647692;

    #pragma omp parallel for if (Nf > 32) schedule(static)
    for (mwSize j = 0; j < Nf; ++j) {
        const double w = -twoPi * F[j];     // -2*pi*freq
        double sumR = 0.0, sumI = 0.0;

        // Encourage vectorization of sin/cos pairs
        #pragma omp simd reduction(+:sumR,sumI)
        for (mwSize i = 0; i < N; ++i) {
            const double phi = w * T[i];
            const double s = std::sin(phi);
            const double c = std::cos(phi);
            const double mm = M[i];
            sumR += mm * c;
            sumI -= mm * s;
        }
        outPow[j] = (sumR*sumR + sumI*sumI) / (double)N;
    }
}

// ---------------- mex entry ----------------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 3)
        mexErrMsgIdAndTxt("ps_mex:nrhs", "Usage: PS = ps_mex(T, M, FreqVec)");
    if (nlhs != 1)
        mexErrMsgIdAndTxt("ps_mex:nlhs", "One output required: power (Nf x 1).");

    const mxArray* T = prhs[0];
    const mxArray* M = prhs[1];
    const mxArray* F = prhs[2];

    if (!isRealFloatOrDouble(T) || !isRealFloatOrDouble(M) || !isRealFloatOrDouble(F))
        mexErrMsgIdAndTxt("ps_mex:type", "Inputs must be real single or double.");
    if (mxGetClassID(T) != mxGetClassID(M) || mxGetClassID(T) != mxGetClassID(F))
        mexErrMsgIdAndTxt("ps_mex:class", "All inputs must be the same class (all single or all double).");
    if (!isVector(T) || !isVector(M) || !isVector(F))
        mexErrMsgIdAndTxt("ps_mex:shape", "All inputs must be vectors.");

    const mwSize N  = mxGetNumberOfElements(T);
    const mwSize Nm = mxGetNumberOfElements(M);
    const mwSize Nf = mxGetNumberOfElements(F);
    if (Nm != N)
        mexErrMsgIdAndTxt("ps_mex:length", "T and M must have the same length.");

    mwSize dims[2] = { Nf, 1 };

    if (mxIsDouble(T)) {
        plhs[0] = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);
        double* outPow = mxGetPr(plhs[0]);
        const double* Td = mxGetPr(T);
        const double* Md = mxGetPr(M);
        const double* Fd = mxGetPr(F);
        compute_ps_double(Td, Md, N, Fd, Nf, outPow);
    } else { // single
        plhs[0] = mxCreateNumericArray(2, dims, mxSINGLE_CLASS, mxREAL);
        float* outPow = reinterpret_cast<float*>(mxGetData(plhs[0]));
        const float* Tf = reinterpret_cast<const float*>(mxGetData(T));
        const float* Mf = reinterpret_cast<const float*>(mxGetData(M));
        const float* Ff = reinterpret_cast<const float*>(mxGetData(F));
        compute_ps_single(Tf, Mf, N, Ff, Nf, outPow);
    }
}

#include "mex.h"
#include <cmath>
#include <cstdint>

#if defined(_MSC_VER)
  #define RESTRICT __restrict
#else
  #define RESTRICT __restrict__
#endif

// Uncomment to enable OpenMP; add -fopenmp in mex command
// #include <omp.h>

static inline double two_pi_d() { return 6.283185307179586476925286766559005768; }
static inline float  two_pi_f() { return 6.2831855f; }

template<typename T>
struct MathTraits {};

template<> struct MathTraits<double> {
    static inline double atan2T(double y, double x) { return std::atan2(y,x); }
    static inline double sqrtT (double v)          { return std::sqrt(v); }
    static inline double two_pi()                  { return two_pi_d(); }
};

template<> struct MathTraits<float> {
    static inline float atan2T(float y, float x)   { return ::atan2f(y,x); }
    static inline float sqrtT (float v)           { return ::sqrtf(v); }
    static inline float two_pi()                  { return two_pi_f(); }
};

template<typename T>
static void kernel(const T* RESTRICT cd1,
                   const T* RESTRICT cd2,
                   const T* RESTRICT cd3,
                   T* RESTRICT lon,
                   T* RESTRICT lat,
                   const std::size_t N)
{
    const T TWO_PI = (T)MathTraits<T>::two_pi();

    // Parallel AND vectorized loop
    // Adjust threshold to avoid thread overhead for tiny N
    // #pragma omp parallel for simd if(N >= 200000) schedule(static)
    #pragma omp simd
    for (ptrdiff_t i = 0; i < (ptrdiff_t)N; ++i) {
        const T x = cd1[i];
        const T y = cd2[i];
        const T z = cd3[i];

        // Longitude in [0, 2π)
        T L = (T)MathTraits<T>::atan2T(y, x);  // (-π, π]
        if (L < (T)0) L += TWO_PI;
        lon[i] = L;

        // Small-angle “latitude”: atan2(z, sqrt(x^2+y^2))
        const T sll = (T)MathTraits<T>::sqrtT(x*x + y*y);
        lat[i] = (T)MathTraits<T>::atan2T(z, sll); // handles poles & z==0 cleanly
    }
}

static void checkSameShape(const mxArray* a, const mxArray* b, const mxArray* c)
{
    if (mxGetNumberOfDimensions(a) != mxGetNumberOfDimensions(b) ||
        mxGetNumberOfDimensions(a) != mxGetNumberOfDimensions(c)) {
        mexErrMsgIdAndTxt("cosined2coo:dimMismatch", "Inputs must have identical sizes.");
    }
    const mwSize nd = mxGetNumberOfDimensions(a);
    const mwSize* da = mxGetDimensions(a);
    const mwSize* db = mxGetDimensions(b);
    const mwSize* dc = mxGetDimensions(c);
    for (mwSize k=0; k<nd; ++k) {
        if (da[k]!=db[k] || da[k]!=dc[k]) {
            mexErrMsgIdAndTxt("cosined2coo:dimMismatch", "Inputs must have identical sizes.");
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 3) mexErrMsgIdAndTxt("cosined2coo:nrhs","Need 3 inputs: CD1, CD2, CD3.");
    if (nlhs != 2) mexErrMsgIdAndTxt("cosined2coo:nlhs","Need 2 outputs: [Long, Lat].");

    for (int i=0;i<3;++i) {
        if (!mxIsNumeric(prhs[i]) || mxIsComplex(prhs[i]))
            mexErrMsgIdAndTxt("cosined2coo:class","Inputs must be real numeric.");
    }

    const mxClassID cls = mxGetClassID(prhs[0]);
    if ((cls != mxDOUBLE_CLASS && cls != mxSINGLE_CLASS) ||
        mxGetClassID(prhs[1])!=cls || mxGetClassID(prhs[2])!=cls) {
        mexErrMsgIdAndTxt("cosined2coo:classMismatch",
                          "All inputs must be the same class (double or single).");
    }

    checkSameShape(prhs[0], prhs[1], prhs[2]);

    const mwSize nd   = mxGetNumberOfDimensions(prhs[0]);
    const mwSize* ds  = mxGetDimensions(prhs[0]);
    const size_t  N   = (size_t)mxGetNumberOfElements(prhs[0]);

    plhs[0] = mxCreateNumericArray(nd, ds, cls, mxREAL); // Long
    plhs[1] = mxCreateNumericArray(nd, ds, cls, mxREAL); // Lat

    if (cls == mxDOUBLE_CLASS) {
        const double* cd1 = (const double*)mxGetData(prhs[0]);
        const double* cd2 = (const double*)mxGetData(prhs[1]);
        const double* cd3 = (const double*)mxGetData(prhs[2]);
        double* lon = (double*)mxGetData(plhs[0]);
        double* lat = (double*)mxGetData(plhs[1]);
        kernel<double>(cd1, cd2, cd3, lon, lat, N);
    } else {
        const float* cd1 = (const float*)mxGetData(prhs[0]);
        const float* cd2 = (const float*)mxGetData(prhs[1]);
        const float* cd3 = (const float*)mxGetData(prhs[2]);
        float* lon = (float*)mxGetData(plhs[0]);
        float* lat = (float*)mxGetData(plhs[1]);
        kernel<float>(cd1, cd2, cd3, lon, lat, N);
    }
}

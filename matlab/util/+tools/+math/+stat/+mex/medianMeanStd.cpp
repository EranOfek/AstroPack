#include "mex.h"
#include <vector>
#include <algorithm>
#include <limits>
#include <type_traits>
#include <cstring>
#include <cmath>

#if defined(_OPENMP)
  #include <omp.h>
#endif

// mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -Ofast -march=native -fopenmp -DNDEBUG" LDFLAGS="\$LDFLAGS -fopenmp" median_dim_mex.cpp

// ===== Options & utils =====
enum NanMode { OMITNAN=0, INCLUDENAN=1 };

template<typename T> static inline bool is_nan_fast(T v) { return v != v; } // NaN != NaN
template<typename T> static inline T   nan_value()       { return std::numeric_limits<T>::quiet_NaN(); }

// --- small-N helpers ---
template<typename T>
static inline void sort_net_3(T* a) {
    if (a[1] < a[0]) std::swap(a[1], a[0]);
    if (a[2] < a[1]) std::swap(a[2], a[1]);
    if (a[1] < a[0]) std::swap(a[1], a[0]);
}
template<typename T>
static inline void sort_net_5(T* a) {
    if (a[1] < a[0]) std::swap(a[1], a[0]);
    if (a[3] < a[2]) std::swap(a[3], a[2]);
    if (a[2] < a[0]) std::swap(a[2], a[0]);
    if (a[3] < a[1]) std::swap(a[3], a[1]);
    if (a[2] < a[1]) std::swap(a[2], a[1]);
    if (a[4] < a[3]) std::swap(a[4], a[3]);
    if (a[3] < a[2]) std::swap(a[3], a[2]);
    if (a[2] < a[1]) std::swap(a[2], a[1]);
}
template<typename T>
static inline void sort_net_7(T* a) {
    if (a[1] < a[0]) std::swap(a[1], a[0]);
    if (a[3] < a[2]) std::swap(a[3], a[2]);
    if (a[5] < a[4]) std::swap(a[5], a[4]);
    if (a[2] < a[0]) std::swap(a[2], a[0]);
    if (a[3] < a[1]) std::swap(a[3], a[1]);
    if (a[6] < a[4]) std::swap(a[6], a[4]);
    if (a[4] < a[2]) std::swap(a[4], a[2]);
    if (a[5] < a[3]) std::swap(a[5], a[3]);
    if (a[2] < a[1]) std::swap(a[2], a[1]);
    if (a[4] < a[3]) std::swap(a[4], a[3]);
    if (a[6] < a[5]) std::swap(a[6], a[5]);
    if (a[1] < a[0]) std::swap(a[1], a[0]);
    if (a[3] < a[2]) std::swap(a[3], a[2]);
    if (a[5] < a[4]) std::swap(a[5], a[4]);
}
template<typename T>
static inline void insertion_sort(T* a, mwSize n) {
    for (mwSize i=1;i<n;++i) {
        T key=a[i]; mwSize j=i;
        while (j>0 && key < a[j-1]) { a[j]=a[j-1]; --j; }
        a[j]=key;
    }
}
template<typename T>
static inline T median_sorted(T* a, mwSize n) {
    if (n & 1) return a[n/2];
    return static_cast<T>((a[n/2-1] + a[n/2]) / (T)2);
}
template<typename T>
static inline T median_of(std::vector<T>& buf, mwSize m) {
    if (m <= 1) return buf[0];
    if (m <= 3) { sort_net_3(buf.data()); return median_sorted(buf.data(), m); }
    if (m <= 5) { sort_net_5(buf.data()); return median_sorted(buf.data(), m); }
    if (m <= 7) { sort_net_7(buf.data()); return median_sorted(buf.data(), m); }
    if (m <= 32){ insertion_sort(buf.data(), m); return median_sorted(buf.data(), m); }
    const mwSize mid = m/2;
    T* begin = buf.data(); T* midIt = begin + mid;
    std::nth_element(begin, midIt, begin + m);
    if (m & 1) return *midIt;
    T loMax = *std::max_element(begin, midIt);
    return static_cast<T>((loMax + *midIt) / (T)2);
}

// ===== Path A: Dim == 1 (contiguous slices) =====
template<typename T>
void median_mean_std_dim1(const T* in, T* outMed, T* outMean, T* outStd,
                          const mwSize* dims, mwSize nd, NanMode mode)
{
    const mwSize m = dims[0];                // slice length
    mwSize nOuter = 1; for (mwSize i=1;i<nd;++i) nOuter *= dims[i];

    const size_t WORK = static_cast<size_t>(m) * static_cast<size_t>(nOuter);
    const size_t SERIAL_TH = 1ull << 18;

    auto do_line = [&](mwIndex idx){
        const T* src = in + static_cast<size_t>(idx)*m;

        if (mode == INCLUDENAN){
            // If any NaN -> all outputs NaN. Else compute all in one pass.
            bool hasNaN=false;
            // Pre-size buffer to avoid push_back overhead
            std::vector<T> buf; buf.resize(m);
            long double mean=0.0L, M2=0.0L; mwSize n=0;
            for (mwSize k=0;k<m;++k){
                T v=src[k];
                if (is_nan_fast(v)){ hasNaN=true; break; }
                buf[k]=v;
                // Welford
                ++n;
                long double delta  = (long double)v - mean;
                mean += delta / (long double)n;
                long double delta2 = (long double)v - mean;
                M2 += delta * delta2;
            }
            if (hasNaN){
                outMed[idx]=nan_value<T>(); outMean[idx]=nan_value<T>(); outStd[idx]=nan_value<T>();
                return;
            }
            outMed[idx]  = median_of(buf, m);
            outMean[idx] = (T)mean;
            outStd[idx]  = (m>1) ? (T)std::sqrt((double)(M2/(long double)(m-1))) : (T)0;
        } else {
            // OMITNAN: filter into buf, Welford over valid values only
            std::vector<T> buf; buf.reserve(m);
            long double mean=0.0L, M2=0.0L; mwSize n=0;
            for (mwSize k=0;k<m;++k){
                T v=src[k];
                if (is_nan_fast(v)) continue;
                buf.push_back(v);
                ++n;
                long double delta  = (long double)v - mean;
                mean += delta / (long double)n;
                long double delta2 = (long double)v - mean;
                M2 += delta * delta2;
            }
            if (buf.empty()){
                outMed[idx]=nan_value<T>(); outMean[idx]=nan_value<T>(); outStd[idx]=nan_value<T>();
                return;
            }
            outMed[idx]  = median_of(buf, (mwSize)buf.size());
            outMean[idx] = (T)mean;
            outStd[idx]  = (n>1) ? (T)std::sqrt((double)(M2/(long double)(n-1))) : (T)0;
        }
    };

    if (WORK < SERIAL_TH
#if defined(_OPENMP)
        || omp_get_max_threads()==1
#endif
    ) {
        for (mwIndex idx=0; idx<(mwIndex)nOuter; ++idx) do_line(idx);
        return;
    }

#if defined(_OPENMP)
    #pragma omp parallel
    {
        #pragma omp for schedule(static)
        for (mwIndex idx=0; idx<(mwIndex)nOuter; ++idx) do_line(idx);
    }
#endif
}

// ===== Path B: Dim > 1 — treat target Dim as “virtual last” (no permute) =====
template<typename T>
void median_mean_std_dim_as_last(const T* in, T* outMed, T* outMean, T* outStd,
                                 const mwSize* dims, mwSize nd, mwSize dim, NanMode mode)
{
    const mwSize L = dims[dim];            // length along Dim
    mwSize stride = 1; for (mwSize i=0;i<dim; ++i) stride *= dims[i];
    const mwSize block = L * stride;
    mwSize nOuter = 1; for (mwSize i=dim+1;i<nd; ++i) nOuter *= dims[i];
    const mwSize nCols = nOuter * stride;

    const size_t WORK = static_cast<size_t>(nCols) * static_cast<size_t>(L);
    const size_t SERIAL_TH = 1ull << 18;

    auto do_col = [&](mwIndex c){
        const mwSize inner = static_cast<mwSize>(c) % stride;
        const mwSize outer = static_cast<mwSize>(c) / stride;
        const mwSize base  = outer * block + inner;

        if (mode == INCLUDENAN){
            bool hasNaN=false;
            std::vector<T> buf; buf.resize(L);
            long double mean=0.0L, M2=0.0L; mwSize n=0;
            for (mwSize k=0;k<L;++k){
                T v=in[base + k*stride];
                if (is_nan_fast(v)){ hasNaN=true; break; }
                buf[k]=v;
                ++n;
                long double delta  = (long double)v - mean;
                mean += delta / (long double)n;
                long double delta2 = (long double)v - mean;
                M2 += delta * delta2;
            }
            if (hasNaN){
                outMed[c]=nan_value<T>(); outMean[c]=nan_value<T>(); outStd[c]=nan_value<T>();
                return;
            }
            outMed[c]  = median_of(buf, L);
            outMean[c] = (T)mean;
            outStd[c]  = (L>1) ? (T)std::sqrt((double)(M2/(long double)(L-1))) : (T)0;
        } else {
            std::vector<T> buf; buf.reserve(L);
            long double mean=0.0L, M2=0.0L; mwSize n=0;
            for (mwSize k=0;k<L;++k){
                T v=in[base + k*stride];
                if (is_nan_fast(v)) continue;
                buf.push_back(v);
                ++n;
                long double delta  = (long double)v - mean;
                mean += delta / (long double)n;
                long double delta2 = (long double)v - mean;
                M2 += delta * delta2;
            }
            if (buf.empty()){
                outMed[c]=nan_value<T>(); outMean[c]=nan_value<T>(); outStd[c]=nan_value<T>();
                return;
            }
            outMed[c]  = median_of(buf, (mwSize)buf.size());
            outMean[c] = (T)mean;
            outStd[c]  = (n>1) ? (T)std::sqrt((double)(M2/(long double)(n-1))) : (T)0;
        }
    };

    if (WORK < SERIAL_TH
#if defined(_OPENMP)
        || omp_get_max_threads()==1
#endif
    ) {
        for (mwIndex c=0; c<(mwIndex)nCols; ++c) do_col(c);
        return;
    }

#if defined(_OPENMP)
    #pragma omp parallel
    {
        #pragma omp for schedule(static)
        for (mwIndex c=0; c<(mwIndex)nCols; ++c) do_col(c);
    }
#endif
}

// ===== MEX entry =====
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2)
        mexErrMsgIdAndTxt("median_mixdim_mex:usage",
            "Usage: [Median,Mean,Std] = median_mixdim_mex(A, Dim [, 'omitnan'|'includenan'])");
    if (nlhs != 3)
        mexErrMsgIdAndTxt("median_mixdim_mex:nlhs","Exactly three outputs required: [Median, Mean, Std].");

    const mxArray* A = prhs[0];
    if (!(mxIsDouble(A) || mxIsSingle(A)))
        mexErrMsgIdAndTxt("median_mixdim_mex:type","A must be single or double.");
    if (mxIsComplex(A))
        mexErrMsgIdAndTxt("median_mixdim_mex:complex","Complex inputs are not supported.");

    if (!mxIsDouble(prhs[1]) || mxIsComplex(prhs[1]))
        mexErrMsgIdAndTxt("median_mixdim_mex:dim","Dim must be a real scalar.");
    mwSize dim = (mwSize)mxGetScalar(prhs[1]);
    if (dim < 1) mexErrMsgIdAndTxt("median_mixdim_mex:dim","Dim must be >= 1.");
    dim -= 1; // convert to 0-based

    NanMode mode = INCLUDENAN; // default
    if (nrhs >= 3) {
        if (!mxIsChar(prhs[2])) mexErrMsgIdAndTxt("median_mixdim_mex:arg","Third argument must be a string.");
        char opt[32]; mxGetString(prhs[2], opt, sizeof(opt));
        if      (!std::strcmp(opt,"omitnan"))    mode = OMITNAN;
        else if (!std::strcmp(opt,"includenan")) mode = INCLUDENAN;
        else mexErrMsgIdAndTxt("median_mixdim_mex:arg","Use 'omitnan' or 'includenan'.");
    }

    const mwSize nd = mxGetNumberOfDimensions(A);
    const mwSize* dims = mxGetDimensions(A);
    if (dim >= nd) mexErrMsgIdAndTxt("median_mixdim_mex:dim","Dim exceeds ndims(A).");

    // Output dims: same as input, but size(Dim)=1
    std::vector<mwSize> outDims(dims, dims+nd);
    outDims[dim] = 1;

    if (mxIsDouble(A)) {
        const double* in = mxGetPr(A);
        plhs[0] = mxCreateNumericArray(nd, outDims.data(), mxDOUBLE_CLASS, mxREAL);
        plhs[1] = mxCreateNumericArray(nd, outDims.data(), mxDOUBLE_CLASS, mxREAL);
        plhs[2] = mxCreateNumericArray(nd, outDims.data(), mxDOUBLE_CLASS, mxREAL);
        double* outMed  = mxGetPr(plhs[0]);
        double* outMean = mxGetPr(plhs[1]);
        double* outStd  = mxGetPr(plhs[2]);

        if (dim == 0) median_mean_std_dim1(in, outMed, outMean, outStd, dims, nd, mode);
        else          median_mean_std_dim_as_last(in, outMed, outMean, outStd, dims, nd, dim, mode);

    } else { // single
        const float* in  = reinterpret_cast<const float*>(mxGetData(A));
        plhs[0] = mxCreateNumericArray(nd, outDims.data(), mxSINGLE_CLASS, mxREAL);
        plhs[1] = mxCreateNumericArray(nd, outDims.data(), mxSINGLE_CLASS, mxREAL);
        plhs[2] = mxCreateNumericArray(nd, outDims.data(), mxSINGLE_CLASS, mxREAL);
        float* outMed  = reinterpret_cast<float*>(mxGetData(plhs[0]));
        float* outMean = reinterpret_cast<float*>(mxGetData(plhs[1]));
        float* outStd  = reinterpret_cast<float*>(mxGetData(plhs[2]));

        if (dim == 0) median_mean_std_dim1(in, outMed, outMean, outStd, dims, nd, mode);
        else          median_mean_std_dim_as_last(in, outMed, outMean, outStd, dims, nd, dim, mode);
    }
}

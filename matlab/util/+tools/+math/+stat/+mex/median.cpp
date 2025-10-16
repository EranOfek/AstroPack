#include "mex.h"
#include <vector>
#include <algorithm>
#include <limits>
#include <type_traits>
#include <cstring>

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
void median_dim1(const T* in, T* out, const mwSize* dims, mwSize nd, NanMode mode)
{
    const mwSize m = dims[0];                // slice length
    mwSize nOuter = 1; for (mwSize i=1;i<nd;++i) nOuter *= dims[i];

    const size_t WORK = static_cast<size_t>(m) * static_cast<size_t>(nOuter);
    const size_t SERIAL_TH = 1ull << 18;

    if (WORK < SERIAL_TH
#if defined(_OPENMP)
        || omp_get_max_threads()==1
#endif
    ) {
        std::vector<T> buf; buf.reserve(m);
        for (mwIndex idx=0; idx<(mwIndex)nOuter; ++idx) {
            const T* src = in + static_cast<size_t>(idx)*m;
            if (mode == INCLUDENAN) {
                bool any=false; buf.resize(m);
                for (mwSize k=0;k<m;++k) { T v=src[k]; if (is_nan_fast(v)){any=true;break;} buf[k]=v; }
                out[idx] = any ? nan_value<T>() : median_of(buf, m);
            } else {
                buf.clear();
                for (mwSize k=0;k<m;++k) { T v=src[k]; if (!is_nan_fast(v)) buf.push_back(v); }
                out[idx] = buf.empty()? nan_value<T>() : median_of(buf, (mwSize)buf.size());
            }
        }
        return;
    }

#if defined(_OPENMP)
    #pragma omp parallel
    {
        std::vector<T> buf; buf.reserve(m);
        #pragma omp for schedule(static)
        for (mwIndex idx=0; idx<(mwIndex)nOuter; ++idx) {
            const T* src = in + static_cast<size_t>(idx)*m;
            if (mode == INCLUDENAN) {
                bool any=false; buf.resize(m);
                for (mwSize k=0;k<m;++k) { T v=src[k]; if (is_nan_fast(v)){any=true;break;} buf[k]=v; }
                out[idx] = any ? nan_value<T>() : median_of(buf, m);
            } else {
                buf.clear();
                for (mwSize k=0;k<m;++k) { T v=src[k]; if (!is_nan_fast(v)) buf.push_back(v); }
                out[idx] = buf.empty()? nan_value<T>() : median_of(buf, (mwSize)buf.size());
            }
        }
    }
#endif
}

// ===== Path B: Dim > 1 — treat target Dim as “virtual last” (no permute) =====
// We iterate over all columns formed by the other dims. For column 'c':
//   inner = c % stride;  outer = c / stride;
//   base = outer * block + inner;   (block = L * stride, L = dims[dim])
template<typename T>
void median_dim_as_last(const T* in, T* out, const mwSize* dims, mwSize nd, mwSize dim, NanMode mode)
{
    const mwSize L = dims[dim];            // length along Dim
    mwSize stride = 1; for (mwSize i=0;i<dim; ++i) stride *= dims[i];
    const mwSize block = L * stride;
    mwSize nOuter = 1; for (mwSize i=dim+1;i<nd; ++i) nOuter *= dims[i];
    const mwSize nCols = nOuter * stride;

    const size_t WORK = static_cast<size_t>(nCols) * static_cast<size_t>(L);
    const size_t SERIAL_TH = 1ull << 18;

    if (WORK < SERIAL_TH
#if defined(_OPENMP)
        || omp_get_max_threads()==1
#endif
    ) {
        std::vector<T> buf; buf.reserve(L);
        for (mwIndex c=0; c<(mwIndex)nCols; ++c) {
            const mwSize inner = static_cast<mwSize>(c) % stride;
            const mwSize outer = static_cast<mwSize>(c) / stride;
            const mwSize base  = outer * block + inner;

            if (mode == INCLUDENAN) {
                bool any=false; buf.resize(L);
                for (mwSize k=0;k<L;++k) { T v=in[base + k*stride]; if (is_nan_fast(v)){any=true;break;} buf[k]=v; }
                out[c] = any ? nan_value<T>() : median_of(buf, L);
            } else {
                buf.clear();
                for (mwSize k=0;k<L;++k) { T v=in[base + k*stride]; if (!is_nan_fast(v)) buf.push_back(v); }
                out[c] = buf.empty()? nan_value<T>() : median_of(buf, (mwSize)buf.size());
            }
        }
        return;
    }

#if defined(_OPENMP)
    #pragma omp parallel
    {
        std::vector<T> buf; buf.reserve(L);
        #pragma omp for schedule(static)
        for (mwIndex c=0; c<(mwIndex)nCols; ++c) {
            const mwSize inner = static_cast<mwSize>(c) % stride;
            const mwSize outer = static_cast<mwSize>(c) / stride;
            const mwSize base  = outer * block + inner;

            if (mode == INCLUDENAN) {
                bool any=false; buf.resize(L);
                for (mwSize k=0;k<L;++k) { T v=in[base + k*stride]; if (is_nan_fast(v)){any=true;break;} buf[k]=v; }
                out[c] = any ? nan_value<T>() : median_of(buf, L);
            } else {
                buf.clear();
                for (mwSize k=0;k<L;++k) { T v=in[base + k*stride]; if (!is_nan_fast(v)) buf.push_back(v); }
                out[c] = buf.empty()? nan_value<T>() : median_of(buf, (mwSize)buf.size());
            }
        }
    }
#endif
}

// ===== MEX entry =====
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 1)
        mexErrMsgIdAndTxt("median_mixdim_mex:usage",
            "Usage: M = median_mixdim_mex(A [, Dim] [, 'omitnan'|'includenan'])");

    const mxArray* A = prhs[0];
    if (!(mxIsDouble(A) || mxIsSingle(A)))
        mexErrMsgIdAndTxt("median_mixdim_mex:type","A must be single or double.");

    // Defaults
    mwSize dim0_based = 0;            // default Dim = 1 -> 0-based = 0
    NanMode mode = INCLUDENAN;        // <-- default per your request

    // Parse optional arguments:
    if (nrhs >= 2) {
        if (mxIsChar(prhs[1])) {
            // Only mode provided: median_mixdim_mex(A, 'omitnan'|'includenan')
            char opt[32]; mxGetString(prhs[1], opt, sizeof(opt));
            if      (!std::strcmp(opt,"omitnan"))    mode = OMITNAN;
            else if (!std::strcmp(opt,"includenan")) mode = INCLUDENAN;
            else mexErrMsgIdAndTxt("median_mixdim_mex:arg","Use 'omitnan' or 'includenan'.");
        } else {
            // Dim provided (and maybe mode as 3rd arg)
            if (!mxIsDouble(prhs[1]) || mxIsComplex(prhs[1]))
                mexErrMsgIdAndTxt("median_mixdim_mex:dim","Dim must be a real scalar.");
            mwSize dim = (mwSize)mxGetScalar(prhs[1]);
            if (dim < 1) mexErrMsgIdAndTxt("median_mixdim_mex:dim","Dim must be >= 1.");
            dim0_based = dim - 1;

            if (nrhs >= 3) {
                if (!mxIsChar(prhs[2])) mexErrMsgIdAndTxt("median_mixdim_mex:arg","Third argument must be a string.");
                char opt[32]; mxGetString(prhs[2], opt, sizeof(opt));
                if      (!std::strcmp(opt,"omitnan"))    mode = OMITNAN;
                else if (!std::strcmp(opt,"includenan")) mode = INCLUDENAN;
                else mexErrMsgIdAndTxt("median_mixdim_mex:arg","Use 'omitnan' or 'includenan'.");
            }
        }
    }

    const mwSize nd = mxGetNumberOfDimensions(A);
    const mwSize* dims = mxGetDimensions(A);
    if (dim0_based >= nd) mexErrMsgIdAndTxt("median_mixdim_mex:dim","Dim exceeds ndims(A).");

    // Output dims: same as input, but size(Dim)=1
    std::vector<mwSize> outDims(dims, dims+nd);
    outDims[dim0_based] = 1;

    if (mxIsDouble(A)) {
        const double* in = mxGetPr(A);
        plhs[0] = mxCreateNumericArray(nd, outDims.data(), mxDOUBLE_CLASS, mxREAL);
        double* out = mxGetPr(plhs[0]);

        if (dim0_based == 0) median_dim1(in, out, dims, nd, mode);
        else                 median_dim_as_last(in, out, dims, nd, dim0_based, mode);

    } else { // single
        const float* in  = reinterpret_cast<const float*>(mxGetData(A));
        plhs[0] = mxCreateNumericArray(nd, outDims.data(), mxSINGLE_CLASS, mxREAL);
        float* out = reinterpret_cast<float*>(mxGetData(plhs[0]));

        if (dim0_based == 0) median_dim1(in, out, dims, nd, mode);
        else                 median_dim_as_last(in, out, dims, nd, dim0_based, mode);
    }
}

#include "mex.h"
#include <cmath>
#include <vector>
#include <limits>

#ifdef _OPENMP
  #include <omp.h>
#endif

// ---- math helpers ----
template<typename T> inline T PI();
template<> inline double PI<double>() { return 3.141592653589793238462643383279502884; }
template<> inline float  PI<float>()  { return 3.14159265358979323846264f; }
template<typename T> inline T TWO_PI(){ return T(2) * PI<T>(); }

template<typename T>
inline T mod_pos(T a, T m){ return a - m * std::floor(a/m); }   // MATLAB-like mod

template<typename T>
inline T wrapToPi(T x){ return mod_pos(x + PI<T>(), TWO_PI<T>()) - PI<T>(); }

// first non-singleton (1-based like MATLAB)
static mwSize firstNonSingletonDim(const mwSize* dims, mwSize nd){
    for (mwSize j=0; j<nd; ++j) if (dims[j] > 1) return j+1;
    return 1;
}

static mwSize prodDims(const mwSize* dims, mwSize nd){
    mwSize p = 1; for (mwSize j=0; j<nd; ++j) p *= (dims[j] ? dims[j] : 1); return p;
}

static void buildStrides(const mwSize* dims, mwSize nd, std::vector<mwSize>& stride){
    stride.assign(nd,1);
    mwSize s = 1;
    for (mwSize j=0; j<nd; ++j){ stride[j] = s; s *= (dims[j] ? dims[j] : 1); }
}

// ---- core unwrappers (fast path for dim==0; generic otherwise) ----
template<typename T>
static void unwrap_real_dim1(const T* in, T* out, const mwSize* dims, mwSize nd, T tol){
    const mwSize M = dims[0];
    const mwSize numel = prodDims(dims, nd);
    const mwSize lines = (M>0) ? numel / M : 0;

    #pragma omp parallel for if(lines > 64)
    for (mwIndex line = 0; line < lines; ++line){
        const mwIndex base = line * M;
        T prev = in[base];
        out[base] = prev;
        T shift = T(0);
        const T pi = PI<T>();

        for (mwSize k=1; k<M; ++k){
            const mwIndex i = base + k;
            const T p = in[i];

            T dp = p - prev;
            T dp_mod = wrapToPi<T>(dp);
            if (dp_mod == -pi && dp > T(0)) dp_mod = pi;

            T corr = dp_mod - dp;
            if (std::fabs(dp) < tol) corr = T(0);

            shift += corr;
            out[i] = p + shift;
            prev = p;
        }
    }
}

template<typename T>
static void unwrap_cplx_dim1(const mxComplexDouble* cD, const mxComplexSingle* cS,
                             T* out, const mwSize* dims, mwSize nd, T tol){
    const mwSize M = dims[0];
    const mwSize numel = prodDims(dims, nd);
    const mwSize lines = (M>0) ? numel / M : 0;

    #pragma omp parallel for if(lines > 64)
    for (mwIndex line = 0; line < lines; ++line){
        const mwIndex base = line * M;

        auto getAng = [&](mwIndex i)->T{
            if (std::is_same<T,double>::value){
                return std::atan2(cD[i].imag, cD[i].real);
            } else {
                return std::atan2(cS[i].imag, cS[i].real);
            }
        };

        T prev = getAng(base);
        out[base] = prev;
        T shift = T(0);
        const T pi = PI<T>();

        for (mwSize k=1; k<M; ++k){
            const mwIndex i = base + k;
            const T p = getAng(i);

            T dp = p - prev;
            T dp_mod = wrapToPi<T>(dp);
            if (dp_mod == -pi && dp > T(0)) dp_mod = pi;

            T corr = dp_mod - dp;
            if (std::fabs(dp) < tol) corr = T(0);

            shift += corr;
            out[i] = p + shift;
            prev = p;
        }
    }
}

template<typename T>
static void unwrap_real_generic(const T* in, T* out,
                                const mwSize* dims, mwSize nd, mwSize dim, T tol){
    std::vector<mwSize> stride; buildStrides(dims, nd, stride);
    const mwSize M = dims[dim];
    // Build odometer over all dims except 'dim'
    std::vector<mwSize> odims; odims.reserve(nd?nd-1:0);
    std::vector<mwSize> ostr;  ostr.reserve(nd?nd-1:0);
    for (mwSize j=0;j<nd;++j){ if (j==dim) continue; odims.push_back(dims[j]?dims[j]:1); ostr.push_back(stride[j]); }

    mwSize lines = 1; for (size_t t=0;t<odims.size();++t) lines *= odims[t];
    std::vector<mwSize> idx(odims.size(),0);
    mwIndex base = 0;

    const T pi = PI<T>();
    for (mwIndex line=0; line<lines; ++line){
        // process one line starting at 'base'
        mwIndex i0 = base;
        T prev = in[i0];
        out[i0] = prev;
        T shift = T(0);
        const mwSize step = stride[dim];

        for (mwSize k=1;k<M;++k){
            mwIndex i = base + k*step;
            T p = in[i];

            T dp = p - prev;
            T dp_mod = wrapToPi<T>(dp);
            if (dp_mod == -pi && dp > T(0)) dp_mod = pi;

            T corr = dp_mod - dp;
            if (std::fabs(dp) < tol) corr = T(0);

            shift += corr;
            out[i] = p + shift;
            prev = p;
        }

        // advance odometer (no divisions)
        for (size_t t=0; t<idx.size(); ++t){
            ++idx[t];
            base += ostr[t];
            if (idx[t] < odims[t]) break;
            // carry
            base -= odims[t]*ostr[t];
            idx[t] = 0;
        }
    }
}

template<typename T>
static void unwrap_cplx_generic(const mxComplexDouble* cD, const mxComplexSingle* cS,
                                T* out, const mwSize* dims, mwSize nd, mwSize dim, T tol){
    std::vector<mwSize> stride; buildStrides(dims, nd, stride);
    const mwSize M = dims[dim];

    std::vector<mwSize> odims; odims.reserve(nd?nd-1:0);
    std::vector<mwSize> ostr;  ostr.reserve(nd?nd-1:0);
    for (mwSize j=0;j<nd;++j){ if (j==dim) continue; odims.push_back(dims[j]?dims[j]:1); ostr.push_back(stride[j]); }

    mwSize lines = 1; for (size_t t=0;t<odims.size();++t) lines *= odims[t];
    std::vector<mwSize> idx(odims.size(),0);
    mwIndex base = 0;

    auto getAng = [&](mwIndex i)->T{
        if (std::is_same<T,double>::value){
            return std::atan2(cD[i].imag, cD[i].real);
        } else {
            return std::atan2(cS[i].imag, cS[i].real);
        }
    };

    const T pi = PI<T>();
    const mwSize step = stride[dim];

    for (mwIndex line=0; line<lines; ++line){
        mwIndex i0 = base;
        T prev = getAng(i0);
        out[i0] = prev;
        T shift = T(0);

        for (mwSize k=1;k<M;++k){
            mwIndex i = base + k*step;
            T p = getAng(i);

            T dp = p - prev;
            T dp_mod = wrapToPi<T>(dp);
            if (dp_mod == -pi && dp > T(0)) dp_mod = pi;

            T corr = dp_mod - dp;
            if (std::fabs(dp) < tol) corr = T(0);

            shift += corr;
            out[i] = p + shift;
            prev = p;
        }

        // advance odometer
        for (size_t t=0; t<idx.size(); ++t){
            ++idx[t];
            base += ostr[t];
            if (idx[t] < odims[t]) break;
            base -= odims[t]*ostr[t];
            idx[t] = 0;
        }
    }
}

// ---- mex entry ----
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]){
    if (nrhs < 1 || nrhs > 3)
        mexErrMsgIdAndTxt("unwrap_mex:nrhs","Usage: U = unwrap_mex(P [, tol] [, dim])");
    if (nlhs > 1)
        mexErrMsgIdAndTxt("unwrap_mex:nlhs","One output.");

    const mxArray* P = prhs[0];
    if (!mxIsNumeric(P))
        mexErrMsgIdAndTxt("unwrap_mex:type","P must be numeric (real or complex), single or double.");

    const mwSize nd = mxGetNumberOfDimensions(P);
    const mwSize* dims = mxGetDimensions(P);

    // tol
    bool tol_given = (nrhs >= 2 && !mxIsEmpty(prhs[1]));
    double tol_d = PI<double>();
    if (tol_given) tol_d = mxGetScalar(prhs[1]);
    if (!std::isfinite(tol_d) || tol_d < 0)
        mexErrMsgIdAndTxt("unwrap_mex:tol","tol must be finite & >=0.");

    // dim (1-based) default = first non-singleton
    mwSize dim1 = (nrhs >= 3 && !mxIsEmpty(prhs[2])) ?
                  static_cast<mwSize>(mxGetScalar(prhs[2])) :
                  firstNonSingletonDim(dims, nd);
    if (dim1 < 1) mexErrMsgIdAndTxt("unwrap_mex:dim","dim must be >= 1.");
    const mwSize dim0 = dim1 - 1;

    // class
    mxClassID cls = mxGetClassID(P);
    if (cls != mxDOUBLE_CLASS && cls != mxSINGLE_CLASS)
        mexErrMsgIdAndTxt("unwrap_mex:class","Only single/double supported.");

    // allocate output (real)
    plhs[0] = mxCreateNumericArray(nd, dims, cls, mxREAL);

    const bool is_cx = mxIsComplex(P);
    const mwSize M = (dim0 < nd) ? dims[dim0] : 1;

    if (cls == mxDOUBLE_CLASS){
        double* out = mxGetDoubles(plhs[0]);
        const double tol = static_cast<double>(tol_d);

        if (!is_cx){
            const double* in = mxGetDoubles(P);
            if (dim0 >= nd || M <= 1){
                // trivial: copy
                const mwSize numel = prodDims(dims, nd);
                for (mwIndex i=0;i<numel;++i) out[i] = in[i];
            } else if (dim0 == 0){
                unwrap_real_dim1<double>(in, out, dims, nd, tol);
            } else {
                unwrap_real_generic<double>(in, out, dims, nd, dim0, tol);
            }
        } else {
            const mxComplexDouble* cD = mxGetComplexDoubles(P);
            if (dim0 >= nd || M <= 1){
                const mwSize numel = prodDims(dims, nd);
                for (mwIndex i=0;i<numel;++i) out[i] = std::atan2(cD[i].imag, cD[i].real);
            } else if (dim0 == 0){
                unwrap_cplx_dim1<double>(cD, nullptr, out, dims, nd, tol);
            } else {
                unwrap_cplx_generic<double>(cD, nullptr, out, dims, nd, dim0, tol);
            }
        }
    } else { // single
        float* out = mxGetSingles(plhs[0]);
        const float tol = tol_given ? static_cast<float>(tol_d) : PI<float>();

        if (!is_cx){
            const float* in = mxGetSingles(P);
            if (dim0 >= nd || M <= 1){
                const mwSize numel = prodDims(dims, nd);
                for (mwIndex i=0;i<numel;++i) out[i] = in[i];
            } else if (dim0 == 0){
                unwrap_real_dim1<float>(in, out, dims, nd, tol);
            } else {
                unwrap_real_generic<float>(in, out, dims, nd, dim0, tol);
            }
        } else {
            const mxComplexSingle* cS = mxGetComplexSingles(P);
            if (dim0 >= nd || M <= 1){
                const mwSize numel = prodDims(dims, nd);
                for (mwIndex i=0;i<numel;++i) out[i] = std::atan2(cS[i].imag, cS[i].real);
            } else if (dim0 == 0){
                unwrap_cplx_dim1<float>(nullptr, cS, out, dims, nd, tol);
            } else {
                unwrap_cplx_generic<float>(nullptr, cS, out, dims, nd, dim0, tol);
            }
        }
    }
}

#include "mex.h"
#include <cmath>
#include <vector>
#include <limits>

#ifdef _OPENMP
  #include <omp.h>
#endif

// ===== Helpers =====
template<typename T> inline T PI();
template<> inline double PI<double>() { return 3.141592653589793238462643383279502884; }
template<> inline float  PI<float>()  { return 3.14159265358979323846264f; }
template<typename T> inline T TWO_PI(){ return T(2) * PI<T>(); }
template<typename T> inline T mod_pos(T a, T m){ return a - m*std::floor(a/m); } // MATLAB-like mod
template<typename T> inline T wrapToPi(T x){ return mod_pos(x + PI<T>(), TWO_PI<T>()) - PI<T>(); }

static inline mwSize prodDims(const mwSize* dims, mwSize nd){ mwSize p=1; for(mwSize j=0;j<nd;++j) p*=(dims[j]?dims[j]:1); return p; }
static inline void buildStrides(const mwSize* dims, mwSize nd, std::vector<mwSize>& s){ s.assign(nd,1); mwSize k=1; for(mwSize j=0;j<nd;++j){ s[j]=k; k*=(dims[j]?dims[j]:1);} }
static inline mwSize firstNonSingletonDim(const mwSize* dims, mwSize nd){ for(mwSize j=0;j<nd;++j) if(dims[j]>1) return j+1; return 1; }

// ----- fused sin/cos (GNU/Linux) -----
#if defined(__GNUC__)
inline void sincos_f(double x, double& s, double& c){ __builtin_sincos(x, &s, &c); }
inline void sincos_f(float  x, float & s, float & c){ __builtin_sincosf(x, &s, &c); }
#else
inline void sincos_f(double x, double& s, double& c){ s=std::sin(x); c=std::cos(x); }
inline void sincos_f(float  x, float & s, float & c){ s=std::sin(x); c=std::cos(x); }
#endif

// ----- angle getters -----
template<typename T>
inline T get_angle_complex(const mxComplexDouble* cD, const mxComplexSingle* cS, mwIndex i){
    if (std::is_same<T,double>::value) return (T)std::atan2(cD[i].imag, cD[i].real);
    else                               return (T)std::atan2(cS[i].imag, cS[i].real);
}
template<typename T>
inline T get_angle_real(const T* r, mwIndex i){
    const T v = r[i];
    if (v != v) return v;                 // NaN
    return (v >= T(0)) ? T(0) : PI<T>();  // 0 or +pi
}

// ===== DIM=1 FAST PATH =====
// -- DY scalar (or line-constant case handled separately in dyarray kernels)
template<typename T>
static void kernel_dim1_dyscalar(bool Ycplx,
                                 const mxComplexDouble* cD, const mxComplexSingle* cS,
                                 const T* rY,
                                 T DY, T tol,
                                 const mwSize* dimsY, mwSize nd,
                                 void* outRaw, bool outIsDouble)
{
    const mwSize M = dimsY[0];
    const mwSize numel = prodDims(dimsY, nd);
    const mwSize lines = (M>0) ? numel / M : 0;
    auto outD = static_cast<mxComplexDouble*>(outRaw);
    auto outS = static_cast<mxComplexSingle*>(outRaw);
    const T pi = PI<T>();

    #pragma omp parallel for if(lines > 64)
    for (mwIndex line=0; line<lines; ++line){
        const mwIndex base = line * M;

        // k=0
        T p_prev = Ycplx ? get_angle_complex<T>(cD, cS, base) : get_angle_real<T>(rY, base);
        T x_prev = DY * p_prev;
        if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[base].real = cd; outD[base].imag = sd; }
        else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[base].real = cf; outS[base].imag = sf; }

        for (mwSize k=1; k<M; ++k){
            const mwIndex i = base + k;
            T p = Ycplx ? get_angle_complex<T>(cD, cS, i) : get_angle_real<T>(rY, i);

            // NaN barrier: reset incremental chain
            if ((p != p) || (p_prev != p_prev)){
                x_prev = DY * p;
                if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[i].real = cd; outD[i].imag = sd; }
                else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[i].real = cf; outS[i].imag = sf; }
                p_prev = p;
                continue;
            }

            T dp = p - p_prev;
            T dp_mod = wrapToPi<T>(dp);
            if (dp_mod == -pi && dp > T(0)) dp_mod = pi;
            // Δp_unwrapped: either dp (if |dp|<tol) or dp_mod
            T dpu = (std::fabs(dp) < tol) ? dp : dp_mod;

            x_prev += DY * dpu;     // incremental phase
            if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[i].real = cd; outD[i].imag = sd; }
            else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[i].real = cf; outS[i].imag = sf; }
            p_prev = p;
        }
    }
}

// -- DY array/broadcast along dim1 (handles both constant and varying DY across k)
template<typename T>
static void kernel_dim1_dyarray(bool Ycplx,
                                const mxComplexDouble* cD, const mxComplexSingle* cS,
                                const T* rY,
                                const T* DY, const mwSize* dimsDY, T tol,
                                const mwSize* dimsY, mwSize nd,
                                void* outRaw, bool outIsDouble)
{
    const mwSize M = dimsY[0];
    const mwSize numel = prodDims(dimsY, nd);
    const mwSize lines = (M>0) ? numel / M : 0;

    // strides for DY (padded to nd)
    std::vector<mwSize> strideDY; buildStrides(dimsDY, nd, strideDY);
    // Build odometer over dims != 0 (higher dims)
    std::vector<mwSize> odims; odims.reserve(nd?nd-1:0);
    std::vector<mwSize> ostr;  ostr.reserve(nd?nd-1:0);
    { std::vector<mwSize> strideY; buildStrides(dimsY, nd, strideY);
      for (mwSize j=1;j<nd;++j){ odims.push_back(dimsY[j]?dimsY[j]:1); ostr.push_back(strideY[j]); } }

    auto outD = static_cast<mxComplexDouble*>(outRaw);
    auto outS = static_cast<mxComplexSingle*>(outRaw);
    const T pi = PI<T>();
    const bool dyConstK = (dimsDY[0] == 1); // constant along k
    const mwIndex dyStep = dyConstK ? 0 : 1;

    // Manual odometer
    std::vector<mwSize> idx(odims.size(),0);
    mwIndex baseY = 0, baseDY = 0;

    for (mwIndex line=0; line<lines; ++line){
        // k=0
        T p_prev = Ycplx ? get_angle_complex<T>(cD, cS, baseY) : get_angle_real<T>(rY, baseY);
        T x_prev;
        if (dyConstK){
            const T dy = DY[baseDY];
            x_prev = dy * p_prev;
            if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[baseY].real = cd; outD[baseY].imag = sd; }
            else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[baseY].real = cf; outS[baseY].imag = sf; }
            // k=1..M-1 (incremental x)
            mwIndex iY = baseY; for (mwSize k=1; k<M; ++k){
                iY += 1;
                T p = Ycplx ? get_angle_complex<T>(cD, cS, iY) : get_angle_real<T>(rY, iY);
                if ((p != p) || (p_prev != p_prev)){
                    x_prev = dy * p;
                    if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[iY].real = cd; outD[iY].imag = sd; }
                    else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[iY].real = cf; outS[iY].imag = sf; }
                    p_prev = p; continue;
                }
                T dp = p - p_prev;
                T dp_mod = wrapToPi<T>(dp);
                if (dp_mod == -pi && dp > T(0)) dp_mod = pi;
                T dpu = (std::fabs(dp) < tol) ? dp : dp_mod;
                x_prev += dy * dpu;
                if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[iY].real = cd; outD[iY].imag = sd; }
                else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[iY].real = cf; outS[iY].imag = sf; }
                p_prev = p;
            }
        } else {
            // varying DY along k → fall back to shift method
            T shift = T(0);
            {
                const T x = DY[baseDY] * p_prev;
                if (outIsDouble){ double sd, cd; sincos_f((double)x, sd, cd); outD[baseY].real = cd; outD[baseY].imag = sd; }
                else             { float  sf, cf; sincos_f((float )x, sf, cf); outS[baseY].real = cf; outS[baseY].imag = sf; }
            }
            mwIndex iY = baseY, iDY = baseDY;
            for (mwSize k=1; k<M; ++k){
                iY  += 1;
                iDY += dyStep;
                T p = Ycplx ? get_angle_complex<T>(cD, cS, iY) : get_angle_real<T>(rY, iY);
                if ((p != p) || (p_prev != p_prev)){
                    const T x = DY[iDY] * p;
                    if (outIsDouble){ double sd, cd; sincos_f((double)x, sd, cd); outD[iY].real = cd; outD[iY].imag = sd; }
                    else             { float  sf, cf; sincos_f((float )x, sf, cf); outS[iY].real = cf; outS[iY].imag = sf; }
                    p_prev = p; shift = T(0); continue;
                }
                T dp = p - p_prev;
                T dp_mod = wrapToPi<T>(dp);
                if (dp_mod == -pi && dp > T(0)) dp_mod = pi;
                T corr = (std::fabs(dp) < tol) ? T(0) : (dp_mod - dp);
                shift += corr;
                const T x = DY[iDY] * (p + shift);
                if (outIsDouble){ double sd, cd; sincos_f((double)x, sd, cd); outD[iY].real = cd; outD[iY].imag = sd; }
                else             { float  sf, cf; sincos_f((float )x, sf, cf); outS[iY].real = cf; outS[iY].imag = sf; }
                p_prev = p;
            }
        }

        // advance higher-dim odometer
        for (size_t t=0; t<idx.size(); ++t){
            ++idx[t];
            baseY  += ostr[t];
            if (dimsDY[t+1] != 1) baseDY += strideDY[t+1];
            if (idx[t] < odims[t]) break;
            baseY  -= odims[t]*ostr[t];
            if (dimsDY[t+1] != 1) baseDY -= odims[t]*strideDY[t+1];
            idx[t] = 0;
        }
    }
}

// ===== GENERIC (dim != 1) =====

// -- DY scalar
template<typename T>
static void kernel_generic_dyscalar(bool Ycplx,
                                    const mxComplexDouble* cD, const mxComplexSingle* cS,
                                    const T* rY,
                                    T DY, T tol,
                                    const mwSize* dimsY, mwSize nd, mwSize dim,
                                    void* outRaw, bool outIsDouble)
{
    std::vector<mwSize> strideY; buildStrides(dimsY, nd, strideY);
    const mwSize M = dimsY[dim];
    const mwSize step = strideY[dim];

    std::vector<mwSize> odims; odims.reserve(nd?nd-1:0);
    std::vector<mwSize> ostr;  ostr.reserve(nd?nd-1:0);
    for (mwSize j=0;j<nd;++j){ if (j==dim) continue; odims.push_back(dimsY[j]?dimsY[j]:1); ostr.push_back(strideY[j]); }
    mwSize lines = 1; for (size_t t=0;t<odims.size();++t) lines *= odims[t];

    auto outD = static_cast<mxComplexDouble*>(outRaw);
    auto outS = static_cast<mxComplexSingle*>(outRaw);
    const T pi = PI<T>();

    std::vector<mwSize> idx(odims.size(),0);
    mwIndex base = 0;

    for (mwIndex line=0; line<lines; ++line){
        mwIndex i0 = base;
        T p_prev = Ycplx ? get_angle_complex<T>(cD, cS, i0) : get_angle_real<T>(rY, i0);
        T x_prev = DY * p_prev;
        if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[i0].real = cd; outD[i0].imag = sd; }
        else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[i0].real = cf; outS[i0].imag = sf; }

        for (mwSize k=1;k<M;++k){
            mwIndex i = base + k*step;
            T p = Ycplx ? get_angle_complex<T>(cD, cS, i) : get_angle_real<T>(rY, i);
            if ((p != p) || (p_prev != p_prev)){
                x_prev = DY * p;
                if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[i].real = cd; outD[i].imag = sd; }
                else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[i].real = cf; outS[i].imag = sf; }
                p_prev = p; continue;
            }
            T dp = p - p_prev;
            T dp_mod = wrapToPi<T>(dp);
            if (dp_mod == -pi && dp > T(0)) dp_mod = pi;
            T dpu = (std::fabs(dp) < tol) ? dp : dp_mod;
            x_prev += DY * dpu;
            if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[i].real = cd; outD[i].imag = sd; }
            else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[i].real = cf; outS[i].imag = sf; }
            p_prev = p;
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

// -- DY array/broadcast (generic dim). Uses incremental x when DY is constant along the unwrap dim.
template<typename T>
static void kernel_generic_dyarray(bool Ycplx,
                                   const mxComplexDouble* cD, const mxComplexSingle* cS,
                                   const T* rY,
                                   const T* DY, const std::vector<mwSize>& dimsDYv,
                                   T tol,
                                   const mwSize* dimsY, mwSize nd, mwSize dim,
                                   void* outRaw, bool outIsDouble)
{
    std::vector<mwSize> strideY;  buildStrides(dimsY,  nd, strideY);
    std::vector<mwSize> strideDY; buildStrides(dimsDYv.data(), nd, strideDY);
    const mwSize M = dimsY[dim];
    const mwSize stepY  = strideY[dim];
    const bool dyConstK = (dimsDYv[dim] == 1);
    const mwSize stepDY = dyConstK ? 0 : strideDY[dim];

    // odometer over dims != dim
    std::vector<mwSize> odims; odims.reserve(nd?nd-1:0);
    std::vector<mwSize> ostrY; ostrY.reserve(nd?nd-1:0);
    std::vector<mwSize> ostrDY; ostrDY.reserve(nd?nd-1:0);
    for (mwSize j=0;j<nd;++j){
        if (j==dim) continue;
        odims.push_back(dimsY[j]?dimsY[j]:1);
        ostrY.push_back(strideY[j]);
        ostrDY.push_back(dimsDYv[j]==1 ? 0 : strideDY[j]);
    }
    mwSize lines = 1; for (size_t t=0;t<odims.size();++t) lines *= odims[t];

    auto outD = static_cast<mxComplexDouble*>(outRaw);
    auto outS = static_cast<mxComplexSingle*>(outRaw);
    const T pi = PI<T>();

    std::vector<mwSize> idx(odims.size(),0);
    mwIndex baseY = 0, baseDY = 0;

    for (mwIndex line=0; line<lines; ++line){
        mwIndex iY0  = baseY;
        mwIndex iDY0 = baseDY;

        T p_prev = Ycplx ? get_angle_complex<T>(cD, cS, iY0) : get_angle_real<T>(rY, iY0);

        if (dyConstK){
            const T dy = DY[iDY0];
            T x_prev = dy * p_prev;
            if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[iY0].real = cd; outD[iY0].imag = sd; }
            else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[iY0].real = cf; outS[iY0].imag = sf; }

            for (mwSize k=1; k<M; ++k){
                const mwIndex iY = baseY + k*stepY;
                T p = Ycplx ? get_angle_complex<T>(cD, cS, iY) : get_angle_real<T>(rY, iY);
                if ((p != p) || (p_prev != p_prev)){
                    x_prev = dy * p;
                    if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[iY].real = cd; outD[iY].imag = sd; }
                    else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[iY].real = cf; outS[iY].imag = sf; }
                    p_prev = p; continue;
                }
                T dp = p - p_prev;
                T dp_mod = wrapToPi<T>(dp);
                if (dp_mod == -pi && dp > T(0)) dp_mod = pi;
                T dpu = (std::fabs(dp) < tol) ? dp : dp_mod;
                x_prev += dy * dpu;
                if (outIsDouble){ double sd, cd; sincos_f((double)x_prev, sd, cd); outD[iY].real = cd; outD[iY].imag = sd; }
                else             { float  sf, cf; sincos_f((float )x_prev, sf, cf); outS[iY].real = cf; outS[iY].imag = sf; }
                p_prev = p;
            }
        } else {
            // varying DY along k → shift method
            T shift = T(0);
            const T x0 = DY[iDY0] * p_prev;
            if (outIsDouble){ double sd, cd; sincos_f((double)x0, sd, cd); outD[iY0].real = cd; outD[iY0].imag = sd; }
            else             { float  sf, cf; sincos_f((float )x0, sf, cf); outS[iY0].real = cf; outS[iY0].imag = sf; }

            for (mwSize k=1; k<M; ++k){
                const mwIndex iY  = baseY + k*stepY;
                const mwIndex iDY = baseDY + k*stepDY;
                T p = Ycplx ? get_angle_complex<T>(cD, cS, iY) : get_angle_real<T>(rY, iY);
                if ((p != p) || (p_prev != p_prev)){
                    const T x = DY[iDY] * p;
                    if (outIsDouble){ double sd, cd; sincos_f((double)x, sd, cd); outD[iY].real = cd; outD[iY].imag = sd; }
                    else             { float  sf, cf; sincos_f((float )x, sf, cf); outS[iY].real = cf; outS[iY].imag = sf; }
                    p_prev = p; shift = T(0); continue;
                }
                T dp = p - p_prev;
                T dp_mod = wrapToPi<T>(dp);
                if (dp_mod == -pi && dp > T(0)) dp_mod = pi;
                T corr = (std::fabs(dp) < tol) ? T(0) : (dp_mod - dp);
                shift += corr;
                const T x = DY[iDY] * (p + shift);
                if (outIsDouble){ double sd, cd; sincos_f((double)x, sd, cd); outD[iY].real = cd; outD[iY].imag = sd; }
                else             { float  sf, cf; sincos_f((float )x, sf, cf); outS[iY].real = cf; outS[iY].imag = sf; }
                p_prev = p;
            }
        }

        // advance odometer
        for (size_t t=0; t<idx.size(); ++t){
            ++idx[t];
            baseY  += ostrY[t];
            baseDY += ostrDY[t];
            if (idx[t] < odims[t]) break;
            baseY  -= odims[t]*ostrY[t];
            baseDY -= odims[t]*ostrDY[t];
            idx[t] = 0;
        }
    }
}

// ===== MEX entry =====
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]){
    // Result = exp_i_DY_unwrap_angle_fused_ultra(DY, OperY [, tol [, dim]])
    if (nrhs < 2 || nrhs > 4) mexErrMsgIdAndTxt("exp_ultra:nrhs","Usage: Result = exp_i_DY_unwrap_angle_fused_ultra(DY, OperY [, tol [, dim]])");
    if (nlhs > 1)            mexErrMsgIdAndTxt("exp_ultra:nlhs","One output only.");

    const mxArray* DY    = prhs[0];
    const mxArray* OperY = prhs[1];
    if (!mxIsNumeric(OperY) || !(mxIsSingle(OperY) || mxIsDouble(OperY)))
        mexErrMsgIdAndTxt("exp_ultra:OperY","OperY must be single/double (real or complex).");
    if (!mxIsNumeric(DY) || mxIsComplex(DY) || !(mxIsSingle(DY) || mxIsDouble(DY)))
        mexErrMsgIdAndTxt("exp_ultra:DY","DY must be real single/double.");

    // tol
    bool haveTol = (nrhs >= 3 && !mxIsEmpty(prhs[2]));
    double tol_d = PI<double>(); if (haveTol) tol_d = mxGetScalar(prhs[2]);
    if (!std::isfinite(tol_d) || tol_d < 0) mexErrMsgIdAndTxt("exp_ultra:tol","tol must be finite and >= 0.");

    // dim (1-based default first non-singleton)
    const mwSize nd  = mxGetNumberOfDimensions(OperY);
    const mwSize* dimsY = mxGetDimensions(OperY);
    mwSize dim1 = (nrhs >= 4 && !mxIsEmpty(prhs[3])) ? (mwSize)mxGetScalar(prhs[3])
                                                     : firstNonSingletonDim(dimsY, nd);
    if (dim1 < 1) mexErrMsgIdAndTxt("exp_ultra:dim","dim must be >= 1.");
    const mwSize dim0 = dim1 - 1;
    const mwSize M = (dim0<nd)?dimsY[dim0]:1;

    // class policy: require same class for exactness
    const bool isDouble = mxIsDouble(OperY);
    if ((isDouble && !mxIsDouble(DY)) || (!isDouble && !mxIsSingle(DY)))
        mexErrMsgIdAndTxt("exp_ultra:class","DY and OperY must be the same class.");

    // Broadcast compatibility: pad DY dims to nd
    const mwSize ndDY = mxGetNumberOfDimensions(DY);
    const mwSize* dDY = mxGetDimensions(DY);
    std::vector<mwSize> dimsDYv(nd, 1);
    for (mwSize j=0; j<nd && j<ndDY; ++j) dimsDYv[j] = dDY[j];
    for (mwSize j=0; j<nd; ++j)
        if (!(dimsDYv[j] == 1 || dimsDYv[j] == dimsY[j]))
            mexErrMsgIdAndTxt("exp_ultra:broadcast","DY size not compatible with OperY for implicit expansion.");

    // Allocate output (complex)
    plhs[0] = mxCreateNumericArray(nd, dimsY, isDouble ? mxDOUBLE_CLASS : mxSINGLE_CLASS, mxCOMPLEX);

    const bool Ycplx = mxIsComplex(OperY);
    const bool DY_scalar = (mxGetNumberOfElements(DY) == 1);

    if (isDouble){
        const mxComplexDouble* cD = Ycplx ? mxGetComplexDoubles(OperY) : nullptr;
        const double* rY = Ycplx ? nullptr : mxGetDoubles(OperY);
        const double tol = (double)tol_d;

        if (dim0 >= nd || M <= 1){
            // Unwrap is a no-op along this dim ⇒ just angle then exp(i*DY*·) with broadcasting
            const mwSize numel = prodDims(dimsY, nd);
            const double DYv = DY_scalar ? *mxGetDoubles(DY) : 0.0;
            const double* DYp = DY_scalar ? nullptr : mxGetDoubles(DY);
            auto out = mxGetComplexDoubles(plhs[0]);
            for (mwIndex i=0;i<numel;++i){
                double p = Ycplx ? std::atan2(cD[i].imag, cD[i].real) : ((mxGetDoubles(OperY)[i] >= 0.0) ? 0.0 : PI<double>());
                double dy = DY_scalar ? DYv : DYp[i];
                double s,c; sincos_f(dy*p, s, c);
                out[i].real = c; out[i].imag = s;
            }
            return;
        }

        if (DY_scalar){
            if (dim0 == 0)
                kernel_dim1_dyscalar<double>(Ycplx, cD, nullptr, rY, *mxGetDoubles(DY), tol, dimsY, nd, mxGetComplexDoubles(plhs[0]), true);
            else
                kernel_generic_dyscalar<double>(Ycplx, cD, nullptr, rY, *mxGetDoubles(DY), tol, dimsY, nd, dim0, mxGetComplexDoubles(plhs[0]), true);
        } else {
            const double* DYp = mxGetDoubles(DY);
            if (dim0 == 0)
                kernel_dim1_dyarray<double>(Ycplx, cD, nullptr, rY, DYp, dimsDYv.data(), tol, dimsY, nd, mxGetComplexDoubles(plhs[0]), true);
            else
                kernel_generic_dyarray<double>(Ycplx, cD, nullptr, rY, DYp, dimsDYv, tol, dimsY, nd, dim0, mxGetComplexDoubles(plhs[0]), true);
        }
    } else { // single
        const mxComplexSingle* cS = Ycplx ? mxGetComplexSingles(OperY) : nullptr;
        const float* rY = Ycplx ? nullptr : mxGetSingles(OperY);
        const float tol = haveTol ? (float)tol_d : PI<float>();

        if (dim0 >= nd || M <= 1){
            const mwSize numel = prodDims(dimsY, nd);
            const float DYv = DY_scalar ? *mxGetSingles(DY) : 0.0f;
            const float* DYp = DY_scalar ? nullptr : mxGetSingles(DY);
            auto out = mxGetComplexSingles(plhs[0]);
            for (mwIndex i=0;i<numel;++i){
                float p = Ycplx ? std::atan2(cS[i].imag, cS[i].real) : ((mxGetSingles(OperY)[i] >= 0.0f) ? 0.0f : PI<float>());
                float dy = DY_scalar ? DYv : DYp[i];
                float s,c; sincos_f(dy*p, s, c);
                out[i].real = c; out[i].imag = s;
            }
            return;
        }

        if (DY_scalar){
            if (dim0 == 0)
                kernel_dim1_dyscalar<float>(Ycplx, nullptr, cS, rY, *mxGetSingles(DY), tol, dimsY, nd, mxGetComplexSingles(plhs[0]), false);
            else
                kernel_generic_dyscalar<float>(Ycplx, nullptr, cS, rY, *mxGetSingles(DY), tol, dimsY, nd, dim0, mxGetComplexSingles(plhs[0]), false);
        } else {
            const float* DYp = mxGetSingles(DY);
            if (dim0 == 0)
                kernel_dim1_dyarray<float>(Ycplx, nullptr, cS, rY, DYp, dimsDYv.data(), tol, dimsY, nd, mxGetComplexSingles(plhs[0]), false);
            else
                kernel_generic_dyarray<float>(Ycplx, nullptr, cS, rY, DYp, dimsDYv, tol, dimsY, nd, dim0, mxGetComplexSingles(plhs[0]), false);
        }
    }
}

#include "mex.h"
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <limits>
#include <cmath>

#if defined(_OPENMP)
  #include <omp.h>
#endif

// ---------- saturating cast (round-to-nearest) ----------
template<typename T, typename Enable=void> struct SatCast;

template<typename T>
struct SatCast<T, typename std::enable_if<std::is_floating_point<T>::value>::type> {
    static inline T run(double v) { return static_cast<T>(v); }
};
template<typename T>
struct SatCast<T, typename std::enable_if<std::is_integral<T>::value && std::is_signed<T>::value>::type> {
    static inline T run(double v) {
        long long r = std::llround(v);
        if (r < (long long)std::numeric_limits<T>::min()) r = (long long)std::numeric_limits<T>::min();
        if (r > (long long)std::numeric_limits<T>::max()) r = (long long)std::numeric_limits<T>::max();
        return static_cast<T>(r);
    }
};
template<typename T>
struct SatCast<T, typename std::enable_if<std::is_integral<T>::value && !std::is_signed<T>::value>::type> {
    static inline T run(double v) {
        long long r = std::llround(v);
        if (r < 0) r = 0;
        if ((unsigned long long)r > (unsigned long long)std::numeric_limits<T>::max())
            r = (long long)std::numeric_limits<T>::max();
        return static_cast<T>(r);
    }
};
template<> struct SatCast<mxLogical, void> { static inline mxLogical run(double v){ return (v!=0.0)?1:0; } };
template<> struct SatCast<mxChar,    void> { static inline mxChar    run(double v){
    long long r = std::llround(v);
    if (r < 0) r = 0;
    if (r > 65535) r = 65535;
    return static_cast<mxChar>(r);
}};

// ---------- A-readers that return A[i] as double (MATLAB promotion semantics) ----------
struct AReaderDouble { const double* p; inline double operator()(size_t i) const { return p[i]; } };
struct AReaderSingle { const float*  p; inline double operator()(size_t i) const { return (double)p[i]; } };
template<typename T> struct AReaderCast  { const T* p; inline double operator()(size_t i) const { return (double)p[i]; } };
struct AReaderLogical{ const mxLogical* p; inline double operator()(size_t i) const { return (double)p[i]; } };
struct AReaderChar   { const mxChar*    p; inline double operator()(size_t i) const { return (double)p[i]; } };

// ---------- fused single-pass loops ----------
template<typename MType, typename AReader>
static inline void loop_real(const MType* __restrict__ Min,
                             MType*       __restrict__ Mout,
                             size_t N, AReader aread, double B, MType v)
{
#if defined(_OPENMP)
    #pragma omp parallel for if(N >= (1u<<18)) schedule(static)
#endif
    for (mwIndex i=0;i<(mwIndex)N;++i){
        MType x = Min[i];
        const double a = aread((size_t)i);
        // use branchless form to help vectorizers
        Mout[i] = (a > B) ? v : x;
    }
}

template<typename Cx, typename AReader, typename FT>
static inline void loop_cplx(const Cx* __restrict__ Min,
                             Cx*       __restrict__ Mout,
                             size_t N, AReader aread, double B, FT Vr, FT Vi)
{
#if defined(_OPENMP)
    #pragma omp parallel for if(N >= (1u<<18)) schedule(static)
#endif
    for (mwIndex i=0;i<(mwIndex)N;++i){
        Cx z = Min[i];
        const double a = aread((size_t)i);
        if (a > B){ z.real = Vr; z.imag = Vi; }
        Mout[i] = z;
    }
}

// ---------- helpers ----------
static inline void require_same_size(const mxArray* A, const mxArray* B){
    const mwSize ndA = mxGetNumberOfDimensions(A);
    const mwSize ndB = mxGetNumberOfDimensions(B);
    if (ndA != ndB) mexErrMsgIdAndTxt("conditionalReplace:size","M and A must have the same size.");
    const mwSize* dA = mxGetDimensions(A);
    const mwSize* dB = mxGetDimensions(B);
    for (mwSize i=0;i<ndA;++i) if (dA[i]!=dB[i]) mexErrMsgIdAndTxt("conditionalReplace:size","M and A must have the same size.");
}
static inline double real_part_scalar(const mxArray* V){
    if (!mxIsComplex(V)) return mxGetScalar(V);
    if (mxGetClassID(V)==mxDOUBLE_CLASS) return mxGetComplexDoubles(V)[0].real;
    if (mxGetClassID(V)==mxSINGLE_CLASS) return (double)mxGetComplexSingles(V)[0].real;
    return mxGetScalar(V);
}

// ---------- MEX entry ----------
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 4)
        mexErrMsgIdAndTxt("conditionalReplace:usage","Usage: Mout = conditionalReplace(M, A, B, V)");
    if (nlhs > 1)
        mexErrMsgIdAndTxt("conditionalReplace:usage","One output only.");

    const mxArray* M = prhs[0];
    const mxArray* A = prhs[1];
    const mxArray* B = prhs[2];
    const mxArray* V = prhs[3];

    if (mxIsSparse(M)) mexErrMsgIdAndTxt("conditionalReplace:type","Sparse M not supported.");
    if (mxIsComplex(A)) mexErrMsgIdAndTxt("conditionalReplace:A","A must be real.");
    if (mxGetNumberOfElements(B)!=1 || mxIsComplex(B)) mexErrMsgIdAndTxt("conditionalReplace:B","B must be a real scalar.");
    if (mxGetNumberOfElements(V)!=1) mexErrMsgIdAndTxt("conditionalReplace:V","V must be a scalar.");
    require_same_size(M, A);

    const mwSize nd    = mxGetNumberOfDimensions(M);
    const mwSize* dims = mxGetDimensions(M);
    const size_t N     = (size_t)mxGetNumberOfElements(M);
    const mxClassID cM = mxGetClassID(M);
    const bool M_is_c  = mxIsComplex(M);
    const double B_d   = mxGetScalar(B);

    // Create fresh output (same class & complexity as M)
    plhs[0] = mxCreateUninitNumericArray(nd, const_cast<mwSize*>(dims), cM, M_is_c ? mxCOMPLEX : mxREAL);

    // Prepare A-reader
    mxClassID cA = mxGetClassID(A);

    // -------- REAL M --------
    if (!M_is_c){
        const double V_real = real_part_scalar(V);
        switch (cM){
            case mxDOUBLE_CLASS: {
                const double* Min = mxGetDoubles(M);
                double*       Mout= mxGetDoubles(plhs[0]);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, (double)V_real); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, (double)V_real); break;
                    case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, (double)V_real); break;
                    case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, (double)V_real); break;
                    case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, (double)V_real); break;
                    case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d,(double)V_real); break;
                    case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, (double)V_real); break;
                    case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d,(double)V_real); break;
                    case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, (double)V_real); break;
                    case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d,(double)V_real); break;
                    case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, (double)V_real); break;
                    case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, (double)V_real); break;
                    default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                }
            } break;

            case mxSINGLE_CLASS: {
                const float* Min = mxGetSingles(M);
                float*       Mout= mxGetSingles(plhs[0]);
                float v = SatCast<float>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                    case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                    case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                    case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                    case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                    case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                    case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                    default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                }
            } break;

            case mxINT8_CLASS:   {
                const int8_T* Min = (const int8_T*)mxGetData(M);
                int8_T*       Mout= (int8_T*)mxGetData(plhs[0]);
                int8_T v = SatCast<int8_T>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                    case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                    case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                    case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                    case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                    case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                    case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                    default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                }
            } break;

            // The rest of integer/logical/char mirror the same pattern:
            case mxUINT8_CLASS:  { const uint8_T* Min=(const uint8_T*)mxGetData(M); uint8_T* Mout=(uint8_T*)mxGetData(plhs[0]);
                uint8_T v = SatCast<uint8_T>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                    case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                    case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                    case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                    case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                    case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                    case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                    default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                }
            } break;

            case mxINT16_CLASS:  { const int16_T* Min=(const int16_T*)mxGetData(M); int16_T* Mout=(int16_T*)mxGetData(plhs[0]);
                int16_T v = SatCast<int16_T>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                    case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                    case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                    case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                    case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                    case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                    case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                    default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                }
            } break;

            case mxUINT16_CLASS: { const uint16_T* Min=(const uint16_T*)mxGetData(M); uint16_T* Mout=(uint16_T*)mxGetData(plhs[0]);
                uint16_T v = SatCast<uint16_T>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                    case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                    case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                    case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                    case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                    case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                    case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                    case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                    default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                }
            } break;

            case mxINT32_CLASS:  { const int32_T* Min=(const int32_T*)mxGetData(M); int32_T* Mout=(int32_T*)mxGetData(plhs[0]);
                int32_T v = SatCast<int32_T>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    default: { // rest
                        switch (cA){
                            case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                            case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                            case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                            case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                            case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                            case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                            case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                            default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                        }
                    }
                }
            } break;

            case mxUINT32_CLASS: { const uint32_T* Min=(const uint32_T*)mxGetData(M); uint32_T* Mout=(uint32_T*)mxGetData(plhs[0]);
                uint32_T v = SatCast<uint32_T>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    default: { // rest (same pattern)
                        switch (cA){
                            case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                            case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                            case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                            case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                            case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                            case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                            case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                            default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                        }
                    }
                }
            } break;

            case mxINT64_CLASS:  { const int64_T* Min=(const int64_T*)mxGetData(M); int64_T* Mout=(int64_T*)mxGetData(plhs[0]);
                int64_T v = SatCast<int64_T>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    default: { // rest
                        switch (cA){
                            case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                            case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                            case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                            case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                            case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                            case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                            case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                            default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                        }
                    }
                }
            } break;

            case mxUINT64_CLASS: { const uint64_T* Min=(const uint64_T*)mxGetData(M); uint64_T* Mout=(uint64_T*)mxGetData(plhs[0]);
                uint64_T v = SatCast<uint64_T>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    default: { // rest
                        switch (cA){
                            case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                            case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                            case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                            case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                            case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                            case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                            case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                            default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                        }
                    }
                }
            } break;

            case mxLOGICAL_CLASS:{
                const mxLogical* Min = mxGetLogicals(M);
                mxLogical*       Mout= mxGetLogicals(plhs[0]);
                mxLogical v = SatCast<mxLogical>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    default: { // rest
                        switch (cA){
                            case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                            case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                            case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                            case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                            case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                            case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                            case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                            default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                        }
                    }
                }
            } break;

            case mxCHAR_CLASS:   {
                const mxChar* Min = mxGetChars(M);
                mxChar*       Mout= mxGetChars(plhs[0]);
                mxChar v = SatCast<mxChar>::run(V_real);
                switch (cA){
                    case mxDOUBLE_CLASS: loop_real(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, v); break;
                    case mxSINGLE_CLASS: loop_real(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, v); break;
                    default: { // rest
                        switch (cA){
                            case mxINT8_CLASS:   loop_real(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, v); break;
                            case mxUINT8_CLASS:  loop_real(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, v); break;
                            case mxINT16_CLASS:  loop_real(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT16_CLASS: loop_real(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, v); break;
                            case mxINT32_CLASS:  loop_real(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT32_CLASS: loop_real(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, v); break;
                            case mxINT64_CLASS:  loop_real(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, v); break;
                            case mxUINT64_CLASS: loop_real(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, v); break;
                            case mxLOGICAL_CLASS:loop_real(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},          B_d, v); break;
                            case mxCHAR_CLASS:   loop_real(Min, Mout, N, AReaderChar   {mxGetChars(A)},             B_d, v); break;
                            default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
                        }
                    }
                }
            } break;

            default: mexErrMsgIdAndTxt("conditionalReplace:Mtype","Unsupported class for M.");
        }
        return;
    }

    // -------- COMPLEX M (single/double only) --------
    if (cM!=mxDOUBLE_CLASS && cM!=mxSINGLE_CLASS)
        mexErrMsgIdAndTxt("conditionalReplace:Mcomplex","Complex M must be single or double.");

    const bool V_is_c = mxIsComplex(V);

    if (cM==mxDOUBLE_CLASS){
        const mxComplexDouble* Min = mxGetComplexDoubles(M);
        mxComplexDouble*       Mout= mxGetComplexDoubles(plhs[0]);
        double Vr = real_part_scalar(V);
        double Vi = V_is_c ? mxGetComplexDoubles(V)[0].imag : 0.0;

        switch (cA){
            case mxDOUBLE_CLASS: loop_cplx(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, Vr, Vi); break;
            case mxSINGLE_CLASS: loop_cplx(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, Vr, Vi); break;
            case mxINT8_CLASS:   loop_cplx(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, Vr, Vi); break;
            case mxUINT8_CLASS:  loop_cplx(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxINT16_CLASS:  loop_cplx(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxUINT16_CLASS: loop_cplx(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxINT32_CLASS:  loop_cplx(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxUINT32_CLASS: loop_cplx(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxINT64_CLASS:  loop_cplx(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxUINT64_CLASS: loop_cplx(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxLOGICAL_CLASS:loop_cplx(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},              B_d, Vr, Vi); break;
            case mxCHAR_CLASS:   loop_cplx(Min, Mout, N, AReaderChar   {mxGetChars(A)},                 B_d, Vr, Vi); break;
            default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
        }
    } else {
        const mxComplexSingle* Min = mxGetComplexSingles(M);
        mxComplexSingle*       Mout= mxGetComplexSingles(plhs[0]);
        float Vr = (float)real_part_scalar(V);
        float Vi = V_is_c ? mxGetComplexSingles(V)[0].imag : 0.0f;

        switch (cA){
            case mxDOUBLE_CLASS: loop_cplx(Min, Mout, N, AReaderDouble{mxGetDoubles(A)}, B_d, Vr, Vi); break;
            case mxSINGLE_CLASS: loop_cplx(Min, Mout, N, AReaderSingle{mxGetSingles(A)}, B_d, Vr, Vi); break;
            case mxINT8_CLASS:   loop_cplx(Min, Mout, N, AReaderCast<int8_T>{(const int8_T*)mxGetData(A)},  B_d, Vr, Vi); break;
            case mxUINT8_CLASS:  loop_cplx(Min, Mout, N, AReaderCast<uint8_T>{(const uint8_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxINT16_CLASS:  loop_cplx(Min, Mout, N, AReaderCast<int16_T>{(const int16_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxUINT16_CLASS: loop_cplx(Min, Mout, N, AReaderCast<uint16_T>{(const uint16_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxINT32_CLASS:  loop_cplx(Min, Mout, N, AReaderCast<int32_T>{(const int32_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxUINT32_CLASS: loop_cplx(Min, Mout, N, AReaderCast<uint32_T>{(const uint32_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxINT64_CLASS:  loop_cplx(Min, Mout, N, AReaderCast<int64_T>{(const int64_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxUINT64_CLASS: loop_cplx(Min, Mout, N, AReaderCast<uint64_T>{(const uint64_T*)mxGetData(A)},B_d, Vr, Vi); break;
            case mxLOGICAL_CLASS:loop_cplx(Min, Mout, N, AReaderLogical{mxGetLogicals(A)},              B_d, Vr, Vi); break;
            case mxCHAR_CLASS:   loop_cplx(Min, Mout, N, AReaderChar   {mxGetChars(A)},                 B_d, Vr, Vi); break;
            default: mexErrMsgIdAndTxt("conditionalReplace:Atype","Unsupported class for A.");
        }
    }
}

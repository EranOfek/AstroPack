#include "mex.h"
#include <cstdint>
#include <cstddef>
#include <cstring>
#include <vector>
#include <cmath>
#include <limits>

static void check_index_vector(const mxArray* IndFlag)
{
    if (mxIsComplex(IndFlag)) {
        mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag must be real.");
    }
    if (!mxIsNumeric(IndFlag)) {
        mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag must be a numeric vector.");
    }
    if (mxGetNumberOfDimensions(IndFlag) > 2) {
        mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag must be a vector.");
    }

    const mwSize M = mxGetM(IndFlag);
    const mwSize N = mxGetN(IndFlag);
    if (!(M == 1 || N == 1 || mxGetNumberOfElements(IndFlag) == 0)) {
        mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag must be a vector.");
    }
}

static int get_max_bit(mxClassID Id)
{
    switch (Id) {
        case mxUINT8_CLASS:  return 8;
        case mxUINT16_CLASS: return 16;
        case mxUINT32_CLASS: return 32;
        case mxUINT64_CLASS: return 64;
        default: return 0;
    }
}

template <typename TInd>
static inline mwIndex convert_integer_index_checked(TInd x, mwSize Nmask)
{
    using Lim = std::numeric_limits<TInd>;

    if constexpr (Lim::is_signed) {
        const int64_t v = static_cast<int64_t>(x);
        if (v < 1) {
            mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag contains an out-of-range index.");
        }
        if (static_cast<uint64_t>(v) > static_cast<uint64_t>(Nmask)) {
            mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag contains an out-of-range index.");
        }
        return static_cast<mwIndex>(v - 1);
    } else {
        const uint64_t v = static_cast<uint64_t>(x);
        if (v < 1) {
            mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag contains an out-of-range index.");
        }
        if (v > static_cast<uint64_t>(Nmask)) {
            mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag contains an out-of-range index.");
        }
        return static_cast<mwIndex>(v - 1);
    }
}

template <typename TFloat>
static inline mwIndex convert_float_index_checked(TFloat x, mwSize Nmask)
{
    const double xd = static_cast<double>(x);

    if (!std::isfinite(xd)) {
        mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag contains NaN or Inf.");
    }

    const double r = std::round(xd);
    if (r != xd) {
        mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag must contain integer-valued indices.");
    }

    if (r < 1.0 || r > static_cast<double>(Nmask)) {
        mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag contains an out-of-range index.");
    }

    return static_cast<mwIndex>(r - 1.0);
}

template <typename TMask, typename TInd>
static void apply_indices_integer(TMask* Out,
                                  const TInd* Ind,
                                  mwSize Nind,
                                  mwSize Nmask,
                                  unsigned Bit0,
                                  bool SetOne)
{
    const TMask M    = static_cast<TMask>(TMask(1) << Bit0);
    const TMask Keep = static_cast<TMask>(~M);

    for (mwIndex i = 0; i < Nind; ++i) {
        const mwIndex idx0 = convert_integer_index_checked<TInd>(Ind[i], Nmask);
        if (SetOne) {
            Out[idx0] = static_cast<TMask>(Out[idx0] | M);
        } else {
            Out[idx0] = static_cast<TMask>(Out[idx0] & Keep);
        }
    }
}

template <typename TMask, typename TFloat>
static void apply_indices_float(TMask* Out,
                                const TFloat* Ind,
                                mwSize Nind,
                                mwSize Nmask,
                                unsigned Bit0,
                                bool SetOne)
{
    const TMask M    = static_cast<TMask>(TMask(1) << Bit0);
    const TMask Keep = static_cast<TMask>(~M);

    for (mwIndex i = 0; i < Nind; ++i) {
        const mwIndex idx0 = convert_float_index_checked<TFloat>(Ind[i], Nmask);
        if (SetOne) {
            Out[idx0] = static_cast<TMask>(Out[idx0] | M);
        } else {
            Out[idx0] = static_cast<TMask>(Out[idx0] & Keep);
        }
    }
}

template <typename TMask>
static void dispatch_indices(const mxArray* IndFlag,
                             TMask* Out,
                             mwSize Nmask,
                             unsigned Bit0,
                             bool SetOne)
{
    const mxClassID Id = mxGetClassID(IndFlag);
    const mwSize Nind = static_cast<mwSize>(mxGetNumberOfElements(IndFlag));

    switch (Id) {
        case mxINT8_CLASS:
            apply_indices_integer<TMask,int8_T>(Out, static_cast<const int8_T*>(mxGetData(IndFlag)), Nind, Nmask, Bit0, SetOne);
            break;
        case mxUINT8_CLASS:
            apply_indices_integer<TMask,uint8_T>(Out, static_cast<const uint8_T*>(mxGetData(IndFlag)), Nind, Nmask, Bit0, SetOne);
            break;
        case mxINT16_CLASS:
            apply_indices_integer<TMask,int16_T>(Out, static_cast<const int16_T*>(mxGetData(IndFlag)), Nind, Nmask, Bit0, SetOne);
            break;
        case mxUINT16_CLASS:
            apply_indices_integer<TMask,uint16_T>(Out, static_cast<const uint16_T*>(mxGetData(IndFlag)), Nind, Nmask, Bit0, SetOne);
            break;
        case mxINT32_CLASS:
            apply_indices_integer<TMask,int32_T>(Out, static_cast<const int32_T*>(mxGetData(IndFlag)), Nind, Nmask, Bit0, SetOne);
            break;
        case mxUINT32_CLASS:
            apply_indices_integer<TMask,uint32_T>(Out, static_cast<const uint32_T*>(mxGetData(IndFlag)), Nind, Nmask, Bit0, SetOne);
            break;
        case mxINT64_CLASS:
            apply_indices_integer<TMask,int64_T>(Out, static_cast<const int64_T*>(mxGetData(IndFlag)), Nind, Nmask, Bit0, SetOne);
            break;
        case mxUINT64_CLASS:
            apply_indices_integer<TMask,uint64_T>(Out, static_cast<const uint64_T*>(mxGetData(IndFlag)), Nind, Nmask, Bit0, SetOne);
            break;
        case mxSINGLE_CLASS:
            apply_indices_float<TMask,float>(Out, mxGetSingles(IndFlag), Nind, Nmask, Bit0, SetOne);
            break;
        case mxDOUBLE_CLASS:
            apply_indices_float<TMask,double>(Out, mxGetDoubles(IndFlag), Nind, Nmask, Bit0, SetOne);
            break;
        default:
            mexErrMsgIdAndTxt("bitsetInd:index", "IndFlag must be int/uint/single/double.");
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("bitsetInd:usage",
                          "Usage: NewMask = bitsetInd(Mask, IndFlag, BitNumber, Value)");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("bitsetInd:usage", "One output only.");
    }

    const mxArray* MaskA    = prhs[0];
    const mxArray* IndFlagA = prhs[1];
    const mxArray* BitA     = prhs[2];
    const mxArray* ValA     = prhs[3];

    if (mxIsComplex(MaskA)) {
        mexErrMsgIdAndTxt("bitsetInd:type", "Mask must be real.");
    }

    const mxClassID MaskId = mxGetClassID(MaskA);
    if (!(MaskId == mxUINT8_CLASS || MaskId == mxUINT16_CLASS ||
          MaskId == mxUINT32_CLASS || MaskId == mxUINT64_CLASS)) {
        mexErrMsgIdAndTxt("bitsetInd:type", "Mask must be uint8/uint16/uint32/uint64.");
    }

    check_index_vector(IndFlagA);

    if (mxIsComplex(BitA) || mxGetNumberOfElements(BitA) != 1) {
        mexErrMsgIdAndTxt("bitsetInd:bit", "BitNumber must be a real scalar.");
    }
    const double b = mxGetScalar(BitA);
    if (!std::isfinite(b)) {
        mexErrMsgIdAndTxt("bitsetInd:bit", "BitNumber must be finite.");
    }
    const int Bit1 = static_cast<int>((b >= 0.0) ? (b + 0.5) : (b - 0.5));

    const int MaxBit = get_max_bit(MaskId);
    if (Bit1 < 1 || Bit1 > MaxBit) {
        mexErrMsgIdAndTxt("bitsetInd:bit", "BitNumber out of range for Mask class.");
    }
    const unsigned Bit0 = static_cast<unsigned>(Bit1 - 1);

    if (mxIsComplex(ValA) || mxGetNumberOfElements(ValA) != 1) {
        mexErrMsgIdAndTxt("bitsetInd:value", "Value must be a real or logical scalar.");
    }
    const bool SetOne = (mxGetScalar(ValA) != 0.0);

    const mwSize Nd    = mxGetNumberOfDimensions(MaskA);
    const mwSize* Dims = mxGetDimensions(MaskA);
    const mwSize Nmask = static_cast<mwSize>(mxGetNumberOfElements(MaskA));

    #if defined(mxCreateUninitNumericArray)
      std::vector<size_t> DimsCopy(static_cast<size_t>(Nd));
      for (mwSize k = 0; k < Nd; ++k) {
          DimsCopy[static_cast<size_t>(k)] = static_cast<size_t>(Dims[k]);
      }
      plhs[0] = mxCreateUninitNumericArray(static_cast<size_t>(Nd), DimsCopy.data(), MaskId, mxREAL);
    #else
      plhs[0] = mxCreateNumericArray(Nd, Dims, MaskId, mxREAL);
    #endif

    std::memcpy(mxGetData(plhs[0]), mxGetData(MaskA), Nmask * mxGetElementSize(MaskA));

    switch (MaskId) {
        case mxUINT8_CLASS:
            dispatch_indices<uint8_T>(IndFlagA, static_cast<uint8_T*>(mxGetData(plhs[0])), Nmask, Bit0, SetOne);
            break;
        case mxUINT16_CLASS:
            dispatch_indices<uint16_T>(IndFlagA, static_cast<uint16_T*>(mxGetData(plhs[0])), Nmask, Bit0, SetOne);
            break;
        case mxUINT32_CLASS:
            dispatch_indices<uint32_T>(IndFlagA, static_cast<uint32_T*>(mxGetData(plhs[0])), Nmask, Bit0, SetOne);
            break;
        case mxUINT64_CLASS:
            dispatch_indices<uint64_T>(IndFlagA, static_cast<uint64_T*>(mxGetData(plhs[0])), Nmask, Bit0, SetOne);
            break;
        default:
            mexErrMsgIdAndTxt("bitsetInd:type", "Unsupported Mask class.");
    }
}

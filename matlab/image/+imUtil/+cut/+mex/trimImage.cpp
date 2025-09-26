// trim_crop_mex.cpp
// out = trim_crop_mex(Image, CCDSEC)
// 2D: out = Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2))
// 3D: out = Image(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2), :)
//
// - Supports ANY built-in class (incl. uint64) and complex (interleaved).
// - Extremely fast: memcpy per column per plane; OpenMP across cols×planes.
// - Portable CCDSEC reader (no mxGetDoubles dependency).
//
// Compile (Linux, OpenMP):
// mex -O CXXFLAGS="$CXXFLAGS -std=c++11 -O3 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" trim_crop_mex.cpp
// Compile (Linux, no OpenMP):
// mex -O CXXFLAGS="$CXXFLAGS -std=c++11 -O3 -march=native" trim_crop_mex.cpp
// Compile (Windows MSVC, OpenMP):
// mex -O COMPFLAGS="$COMPFLAGS /O2 /GL /arch:AVX2 /openmp" LINKFLAGS="$LINKFLAGS /LTCG" trim_crop_mex.cpp
// Compile (Windows MSVC, no OpenMP):
// mex -O COMPFLAGS="$COMPFLAGS /O2 /GL /arch:AVX2" LINKFLAGS="$LINKFLAGS /LTCG" trim_crop_mex.cpp

#include "mex.h"
#include <cstring>
#include <cstdint>
#include <cmath>

#ifdef _OPENMP
  #include <omp.h>
#endif

static inline double get_elem_as_double(const mxArray* A, mwIndex idx) {
    const void* p = mxGetData(A);
    switch (mxGetClassID(A)) {
        case mxDOUBLE_CLASS: return static_cast<const double*>(p)[idx];
        case mxSINGLE_CLASS: return static_cast<const float*>(p)[idx];
        case mxINT8_CLASS:   return static_cast<double>(static_cast<const int8_T*>(p)[idx]);
        case mxUINT8_CLASS:  return static_cast<double>(static_cast<const uint8_T*>(p)[idx]);
        case mxINT16_CLASS:  return static_cast<double>(static_cast<const int16_T*>(p)[idx]);
        case mxUINT16_CLASS: return static_cast<double>(static_cast<const uint16_T*>(p)[idx]);
        case mxINT32_CLASS:  return static_cast<double>(static_cast<const int32_T*>(p)[idx]);
        case mxUINT32_CLASS: return static_cast<double>(static_cast<const uint32_T*>(p)[idx]);
#if defined(mxINT64_CLASS)
        case mxINT64_CLASS:  return static_cast<double>(static_cast<const int64_T*>(p)[idx]);
        case mxUINT64_CLASS: return static_cast<double>(static_cast<const uint64_T*>(p)[idx]);
#endif
        case mxLOGICAL_CLASS:return static_cast<double>(static_cast<const mxLogical*>(p)[idx]);
        default:
            mexErrMsgIdAndTxt("trim:CCDSEC:Type", "Unsupported CCDSEC class.");
            return 0.0;
    }
}

static inline mwSize to_index_checked(double v, const char* name) {
    if (!mxIsFinite(v)) mexErrMsgIdAndTxt("trim:CCDSEC:NaNInf", "%s must be finite.", name);
    const double r = std::round(v);
    if (std::abs(v - r) != 0.0) mexErrMsgIdAndTxt("trim:CCDSEC:NonInteger", "%s must be integer-valued.", name);
    if (r < 1.0) mexErrMsgIdAndTxt("trim:CCDSEC:Range", "%s must be >= 1.", name);
    return static_cast<mwSize>(r);
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2) mexErrMsgIdAndTxt("trim:Args", "Usage: out = trim_crop_mex(Image, CCDSEC).");

    const mxArray* A   = prhs[0];
    const mxArray* CCD = prhs[1];

    if (mxIsSparse(A)) mexErrMsgIdAndTxt("trim:Sparse", "Image must be full (not sparse).");
    const mwSize nd = mxGetNumberOfDimensions(A);
    if (nd < 2 || nd > 3) mexErrMsgIdAndTxt("trim:Dim", "Image must be 2-D or 3-D.");

    if ((!mxIsNumeric(CCD) && !mxIsLogical(CCD)) || mxIsComplex(CCD) || mxGetNumberOfElements(CCD)!=4)
        mexErrMsgIdAndTxt("trim:CCDSEC", "CCDSEC must be real numeric/logical vector [x1 x2 y1 y2].");

    // Read CCDSEC (1-based inclusive)
    const mwSize x1 = to_index_checked(get_elem_as_double(CCD, 0), "CCDSEC(1)");
    const mwSize x2 = to_index_checked(get_elem_as_double(CCD, 1), "CCDSEC(2)");
    const mwSize y1 = to_index_checked(get_elem_as_double(CCD, 2), "CCDSEC(3)");
    const mwSize y2 = to_index_checked(get_elem_as_double(CCD, 3), "CCDSEC(4)");
    if (x2 < x1) mexErrMsgIdAndTxt("trim:Order", "CCDSEC(2) must be >= CCDSEC(1).");
    if (y2 < y1) mexErrMsgIdAndTxt("trim:Order", "CCDSEC(4) must be >= CCDSEC(3).");

    // Sizes
    const mwSize* dims = mxGetDimensions(A);
    const mwSize inRows = dims[0];
    const mwSize inCols = dims[1];
    const mwSize inPages = (nd==3) ? dims[2] : 1;

    if (x1>inCols || x2>inCols || y1>inRows || y2>inRows)
        mexErrMsgIdAndTxt("trim:Bounds", "CCDSEC is out of bounds.");

    const mwSize outRows = y2 - y1 + 1;
    const mwSize outCols = x2 - x1 + 1;
    const mwSize outDims[3] = { outRows, outCols, inPages };

    // Create output (same class & complexity)
    const mxClassID cls  = mxGetClassID(A);
    const bool isComplex = mxIsComplex(A);
    if (nd==2)
        plhs[0] = mxCreateNumericMatrix(outRows, outCols, cls, isComplex ? mxCOMPLEX : mxREAL);
    else
        plhs[0] = mxCreateNumericArray(3, outDims, cls, isComplex ? mxCOMPLEX : mxREAL);

    // Raw pointers and strides (column-major)
    const size_t elemSize = mxGetElementSize(A);
    const unsigned char* src = reinterpret_cast<const unsigned char*>(mxGetData(A));
    unsigned char*       dst = reinterpret_cast<unsigned char*>(mxGetData(plhs[0]));

    const size_t inRowStride   = 1;                         // elements
    const size_t inColStride   = static_cast<size_t>(inRows);
    const size_t inPageStride  = static_cast<size_t>(inRows) * static_cast<size_t>(inCols);

    const size_t outColStride  = static_cast<size_t>(outRows);
    const size_t outPageStride = static_cast<size_t>(outRows) * static_cast<size_t>(outCols);

    const mwSize x0 = x1 - 1;   // 0-based
    const mwSize y0 = y1 - 1;

    if (nd == 2) {
        // Parallel over columns
        #pragma omp parallel for if(outCols > 32) schedule(static)
        for (mwSize j = 0; j < outCols; ++j) {
            const size_t srcOffEl = static_cast<size_t>(x0 + j) * inColStride + y0;
            const size_t dstOffEl = static_cast<size_t>(j) * outColStride;
            std::memcpy(dst + dstOffEl * elemSize,
                        src + srcOffEl * elemSize,
                        static_cast<size_t>(outRows) * elemSize);
        }
    } else {
        // Parallel over (columns × pages)
        const mwSize P = inPages;
        // Use collapse(2) when available to distribute 2D iteration space.
        #if defined(_OPENMP) && (_OPENMP >= 200805) // OpenMP 3.0+
        #pragma omp parallel for collapse(2) if(static_cast<size_t>(outCols)*static_cast<size_t>(P) > 64) schedule(static)
        #endif
        for (mwSize k = 0; k < P; ++k) {
            for (mwSize j = 0; j < outCols; ++j) {
                const size_t srcOffEl =
                    static_cast<size_t>(k) * inPageStride +
                    static_cast<size_t>(x0 + j) * inColStride +
                    static_cast<size_t>(y0) * inRowStride;

                const size_t dstOffEl =
                    static_cast<size_t>(k) * outPageStride +
                    static_cast<size_t>(j) * outColStride;

                std::memcpy(dst + dstOffEl * elemSize,
                            src + srcOffEl * elemSize,
                            static_cast<size_t>(outRows) * elemSize);
            }
        }
    }
}

// bitand_dim.cpp
#include "mex.h"
#include <cstdint>
#include <cstring>
#include <vector>
#include <algorithm>
#include <cmath>

static void die(const char* msg) {
    mexErrMsgIdAndTxt("bitand_dim:err", "%s", msg);
}

static inline mwSize prod(const mwSize* d, mwSize k) {
    mwSize p = 1;
    for (mwSize i = 0; i < k; ++i) p *= d[i];
    return p;
}

static int parseDim(const mxArray* A, mwSize ndA) {
    if (A == nullptr) return 1;
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxGetNumberOfElements(A) != 1)
        die("dim must be a real numeric scalar.");

    double dv = mxGetScalar(A);
    if (!std::isfinite(dv) || dv < 1.0) die("dim must be a positive integer (1-based).");

    int dim = (int)dv;
    if ((double)dim != dv) die("dim must be a positive integer (1-based).");
    if (dim > (int)ndA) die("dim exceeds number of dimensions of A.");
    return dim;
}

// MATLAB squeeze special-casing for 2D:
// - if input is 2D, keep it 2D after reduction (i.e., 1xN or Mx1), not Nx1.
static void buildOutDimsLikeSqueeze(const mwSize* inDims, mwSize ndIn, int dim1based,
                                   std::vector<mwSize>& outDims)
{
    const int dim0 = dim1based - 1;

    std::vector<mwSize> redDims(inDims, inDims + ndIn);
    redDims[dim0] = 1;

    outDims.clear();

    if (ndIn == 2) {
        outDims.push_back(redDims[0]);
        outDims.push_back(redDims[1]);
        return;
    }

    for (mwSize i = 0; i < ndIn; ++i) {
        if (redDims[i] != 1) outDims.push_back(redDims[i]);
    }
    if (outDims.empty()) outDims.push_back(1); // scalar
}

template <typename T>
static void bitand_dim_core(const T* A, T* Y,
                            const mwSize* dims, mwSize nd, int dim1based)
{
    const int dim0 = dim1based - 1;

    const mwSize nAlong = dims[dim0];
    const mwSize inner  = prod(dims, (mwSize)dim0);
    const mwSize outer  = (dim0 + 1 < (int)nd) ? prod(dims + dim0 + 1, nd - dim0 - 1) : 1;

    for (mwSize o = 0; o < outer; ++o) {
        const mwSize outerBase = o * nAlong * inner;
        T* yBlock = Y + o * inner;

        for (mwSize i = 0; i < inner; ++i) {
            const T* p = A + outerBase + i;

            // AND reduction identity:
            // start with first element in the reduction axis (fast, avoids max-value init)
            T acc = p[0];

            mwSize k = 1;
            for (; k + 4 <= nAlong; k += 4) {
                acc &= p[(k + 0) * inner];
                acc &= p[(k + 1) * inner];
                acc &= p[(k + 2) * inner];
                acc &= p[(k + 3) * inner];
            }
            for (; k < nAlong; ++k) {
                acc &= p[k * inner];
            }
            yBlock[i] = acc;
        }
    }
}

template <typename T>
static void copyReducedToSqueezed(const T* R, T* S,
                                  const std::vector<mwSize>& outDims)
{
    mwSize nEl = 1;
    for (mwSize d : outDims) nEl *= d;
    std::memcpy(S, R, (size_t)nEl * sizeof(T));
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 1 && nrhs != 2) die("Usage: Y = bitand_dim(A, dim=1)");
    if (nlhs != 1) die("One output.");

    const mxArray* A = prhs[0];
    if (mxIsComplex(A)) die("A must be real.");
    if (!(mxIsNumeric(A) || mxIsLogical(A))) die("A must be an integer/uint array (logical allowed).");
    if (mxIsSparse(A)) die("Sparse input not supported.");

    mxClassID cid = mxGetClassID(A);
    const bool isLogical = mxIsLogical(A);

    if (!isLogical) {
        if (cid == mxSINGLE_CLASS || cid == mxDOUBLE_CLASS)
            die("A must be an integer/uint type (or logical), not single/double.");
    }

    const mwSize ndA = mxGetNumberOfDimensions(A);
    const mwSize* dimsA = mxGetDimensions(A);
    const int dim = parseDim((nrhs == 2) ? prhs[1] : nullptr, ndA);

    std::vector<mwSize> outDims;
    buildOutDimsLikeSqueeze(dimsA, ndA, dim, outDims);

    const mwSize nAlong = dimsA[dim - 1];
    if (nAlong == 0) {
        // MATLAB-like behavior for AND over empty: identity is all-ones.
        // For logical: true. For integers: all bits set (i.e., -1 for signed, max for unsigned).
        mxClassID outClass = isLogical ? mxLOGICAL_CLASS : cid;
        plhs[0] = mxCreateNumericArray((mwSize)outDims.size(), outDims.data(), outClass, mxREAL);

        const mwSize nOut = (mwSize)mxGetNumberOfElements(plhs[0]);

        if (isLogical) {
            mxLogical* out = (mxLogical*)mxGetData(plhs[0]);
            std::memset(out, 1, (size_t)nOut * sizeof(mxLogical)); // true
        } else {
            // Fill with all-ones bytes => bit pattern 0xFF..FF
            void* out = mxGetData(plhs[0]);
            std::memset(out, 0xFF, (size_t)nOut * mxGetElementSize(plhs[0]));
        }
        return;
    }

    std::vector<mwSize> redDims(dimsA, dimsA + ndA);
    redDims[dim - 1] = 1;

    mxClassID outClass = isLogical ? mxLOGICAL_CLASS : cid;
    mxArray* Rarr = mxCreateNumericArray(ndA, redDims.data(), outClass, mxREAL);
    plhs[0] = mxCreateNumericArray((mwSize)outDims.size(), outDims.data(), outClass, mxREAL);

    if (isLogical) {
        const mxLogical* in = (const mxLogical*)mxGetData(A);
        mxLogical* R = (mxLogical*)mxGetData(Rarr);
        bitand_dim_core<mxLogical>(in, R, dimsA, ndA, dim);

        mxLogical* out = (mxLogical*)mxGetData(plhs[0]);
        copyReducedToSqueezed<mxLogical>(R, out, outDims);
    } else {
        const void* inV = mxGetData(A);
        void* RV = mxGetData(Rarr);
        void* outV = mxGetData(plhs[0]);

        switch (cid) {
            case mxINT8_CLASS:
                bitand_dim_core<int8_t>((const int8_t*)inV, (int8_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<int8_t>((const int8_t*)RV, (int8_t*)outV, outDims);
                break;
            case mxUINT8_CLASS:
                bitand_dim_core<uint8_t>((const uint8_t*)inV, (uint8_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<uint8_t>((const uint8_t*)RV, (uint8_t*)outV, outDims);
                break;
            case mxINT16_CLASS:
                bitand_dim_core<int16_t>((const int16_t*)inV, (int16_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<int16_t>((const int16_t*)RV, (int16_t*)outV, outDims);
                break;
            case mxUINT16_CLASS:
                bitand_dim_core<uint16_t>((const uint16_t*)inV, (uint16_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<uint16_t>((const uint16_t*)RV, (uint16_t*)outV, outDims);
                break;
            case mxINT32_CLASS:
                bitand_dim_core<int32_t>((const int32_t*)inV, (int32_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<int32_t>((const int32_t*)RV, (int32_t*)outV, outDims);
                break;
            case mxUINT32_CLASS:
                bitand_dim_core<uint32_t>((const uint32_t*)inV, (uint32_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<uint32_t>((const uint32_t*)RV, (uint32_t*)outV, outDims);
                break;
            case mxINT64_CLASS:
                bitand_dim_core<int64_t>((const int64_t*)inV, (int64_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<int64_t>((const int64_t*)RV, (int64_t*)outV, outDims);
                break;
            case mxUINT64_CLASS:
                bitand_dim_core<uint64_t>((const uint64_t*)inV, (uint64_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<uint64_t>((const uint64_t*)RV, (uint64_t*)outV, outDims);
                break;
            default:
                mxDestroyArray(Rarr);
                die("Unsupported class (expected int/uint or logical).");
        }
    }

    mxDestroyArray(Rarr);
}

// bitor_dim.cpp
#include "mex.h"
#include <cstdint>
#include <cstring>
#include <vector>
#include <algorithm>
#include <cmath>   // <-- FIX: std::floor / std::isfinite

static void die(const char* msg) {
    mexErrMsgIdAndTxt("bitor_dim:err", "%s", msg);
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

static void buildOutDimsSqueezed(const mwSize* inDims, mwSize ndIn, int dim1based,
                                std::vector<mwSize>& outDims) {
    const int dim0 = dim1based - 1;

    std::vector<mwSize> redDims(inDims, inDims + ndIn);
    redDims[dim0] = 1;

    outDims.clear();
    for (mwSize i = 0; i < ndIn; ++i) {
        if (redDims[i] != 1) outDims.push_back(redDims[i]);
    }
    if (outDims.empty()) outDims.push_back(1); // scalar
}

template <typename T>
static void bitor_dim_core(const T* A, T* Y,
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
            T acc = (T)0;

            mwSize k = 0;
            for (; k + 4 <= nAlong; k += 4) {
                acc |= p[(k + 0) * inner];
                acc |= p[(k + 1) * inner];
                acc |= p[(k + 2) * inner];
                acc |= p[(k + 3) * inner];
            }
            for (; k < nAlong; ++k) {
                acc |= p[k * inner];
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
    if (nrhs != 1 && nrhs != 2) die("Usage: Y = bitor_dim(A, dim=1)");
    if (nlhs != 1) die("One output.");

    const mxArray* A = prhs[0];
    if (mxIsComplex(A)) die("A must be real.");
    if (!(mxIsNumeric(A) || mxIsLogical(A))) die("A must be an integer/uint array (logical allowed).");
    if (mxIsSparse(A)) die("Sparse input not supported.");

    mxClassID cid = mxGetClassID(A);
    bool isLogical = mxIsLogical(A);

    if (!isLogical) {
        if (cid == mxSINGLE_CLASS || cid == mxDOUBLE_CLASS)
            die("A must be an integer/uint type (or logical), not single/double.");
    }

    const mwSize ndA = mxGetNumberOfDimensions(A);
    const mwSize* dimsA = mxGetDimensions(A);
    const int dim = parseDim((nrhs == 2) ? prhs[1] : nullptr, ndA);

    std::vector<mwSize> outDims;
    buildOutDimsSqueezed(dimsA, ndA, dim, outDims);

    const mwSize nAlong = dimsA[dim - 1];
    if (nAlong == 0) {
        plhs[0] = mxCreateNumericArray((mwSize)outDims.size(), outDims.data(),
                                      isLogical ? mxUINT8_CLASS : cid, mxREAL);
        void* out = mxGetData(plhs[0]);
        std::memset(out, 0, (size_t)mxGetNumberOfElements(plhs[0]) *
                            (isLogical ? 1 : mxGetElementSize(plhs[0])));
        return;
    }

    const mwSize nElA = (mwSize)mxGetNumberOfElements(A);
    const mwSize nElR = nElA / nAlong;

    std::vector<mwSize> redDims(dimsA, dimsA + ndA);
    redDims[dim - 1] = 1;

    mxArray* Rarr = mxCreateNumericArray(ndA, redDims.data(),
                                        isLogical ? mxUINT8_CLASS : cid, mxREAL);
    plhs[0] = mxCreateNumericArray((mwSize)outDims.size(), outDims.data(),
                                  isLogical ? mxUINT8_CLASS : cid, mxREAL);

    if (isLogical) {
        const uint8_t* in = (const uint8_t*)mxGetData(A);
        uint8_t* R = (uint8_t*)mxGetData(Rarr);
        bitor_dim_core<uint8_t>(in, R, dimsA, ndA, dim);

        uint8_t* out = (uint8_t*)mxGetData(plhs[0]);
        copyReducedToSqueezed<uint8_t>(R, out, outDims);
    } else {
        const void* inV = mxGetData(A);
        void* RV = mxGetData(Rarr);
        void* outV = mxGetData(plhs[0]);

        switch (cid) {
            case mxINT8_CLASS:
                bitor_dim_core<int8_t>((const int8_t*)inV, (int8_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<int8_t>((const int8_t*)RV, (int8_t*)outV, outDims);
                break;
            case mxUINT8_CLASS:
                bitor_dim_core<uint8_t>((const uint8_t*)inV, (uint8_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<uint8_t>((const uint8_t*)RV, (uint8_t*)outV, outDims);
                break;
            case mxINT16_CLASS:
                bitor_dim_core<int16_t>((const int16_t*)inV, (int16_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<int16_t>((const int16_t*)RV, (int16_t*)outV, outDims);
                break;
            case mxUINT16_CLASS:
                bitor_dim_core<uint16_t>((const uint16_t*)inV, (uint16_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<uint16_t>((const uint16_t*)RV, (uint16_t*)outV, outDims);
                break;
            case mxINT32_CLASS:
                bitor_dim_core<int32_t>((const int32_t*)inV, (int32_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<int32_t>((const int32_t*)RV, (int32_t*)outV, outDims);
                break;
            case mxUINT32_CLASS:
                bitor_dim_core<uint32_t>((const uint32_t*)inV, (uint32_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<uint32_t>((const uint32_t*)RV, (uint32_t*)outV, outDims);
                break;
            case mxINT64_CLASS:
                bitor_dim_core<int64_t>((const int64_t*)inV, (int64_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<int64_t>((const int64_t*)RV, (int64_t*)outV, outDims);
                break;
            case mxUINT64_CLASS:
                bitor_dim_core<uint64_t>((const uint64_t*)inV, (uint64_t*)RV, dimsA, ndA, dim);
                copyReducedToSqueezed<uint64_t>((const uint64_t*)RV, (uint64_t*)outV, outDims);
                break;
            default:
                mxDestroyArray(Rarr);
                die("Unsupported class (expected int/uint or logical).");
        }
    }

    mxDestroyArray(Rarr);
}

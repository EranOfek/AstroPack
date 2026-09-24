#include "mex.h"
#include <cstdint>
#include <cstring>
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

static inline mwSize checkedIndex(double v, mwSize minv, mwSize maxv, const char *name)
{
    if (!mxIsFinite(v)) {
        mexErrMsgIdAndTxt("cube2image:CCDSEC",
                          "%s contains NaN or Inf.", name);
    }

    if (std::floor(v) != v) {
        mexErrMsgIdAndTxt("cube2image:CCDSEC",
                          "%s must contain integer values.", name);
    }

    if (v < static_cast<double>(minv) || v > static_cast<double>(maxv)) {
        mexErrMsgIdAndTxt("cube2image:CCDSEC",
                          "%s is out of bounds.", name);
    }

    return static_cast<mwSize>(v);
}

static inline void checkInputs(int nlhs, int nrhs, const mxArray *prhs[])
{
    if (nrhs != 4) {
        mexErrMsgIdAndTxt("cube2image:Input",
                          "Four inputs required: Cube, Sub_CCDSEC, NoOverlapCCDSEC, NewNoOverlapCCDSEC.");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("cube2image:Output",
                          "One output only.");
    }

    if (mxGetNumberOfDimensions(prhs[0]) != 3) {
        mexErrMsgIdAndTxt("cube2image:Cube",
                          "Cube must be a 3D array.");
    }

    if (mxIsSparse(prhs[0])) {
        mexErrMsgIdAndTxt("cube2image:Cube",
                          "Sparse Cube is not supported.");
    }

    if (mxIsComplex(prhs[0])) {
        mexErrMsgIdAndTxt("cube2image:Cube",
                          "Complex Cube is not supported.");
    }

    for (int i = 1; i <= 3; ++i) {
        if (!mxIsDouble(prhs[i]) || mxIsComplex(prhs[i])) {
            mexErrMsgIdAndTxt("cube2image:CCDSEC",
                              "Sub_CCDSEC, NoOverlapCCDSEC, and NewNoOverlapCCDSEC must be real double matrices.");
        }

        if (mxGetN(prhs[i]) != 4) {
            mexErrMsgIdAndTxt("cube2image:CCDSEC",
                              "Each CCDSEC input must be N x 4.");
        }
    }

    const mwSize N1 = mxGetM(prhs[1]);
    const mwSize N2 = mxGetM(prhs[2]);
    const mwSize N3 = mxGetM(prhs[3]);

    if (N1 != N2 || N1 != N3) {
        mexErrMsgIdAndTxt("cube2image:CCDSEC",
                          "Sub_CCDSEC, NoOverlapCCDSEC, and NewNoOverlapCCDSEC must have the same number of rows.");
    }

    const mwSize *CubeDims = mxGetDimensions(prhs[0]);
    const mwSize Nim = CubeDims[2];

    if (N1 != Nim) {
        mexErrMsgIdAndTxt("cube2image:SizeMismatch",
                          "Number of CCDSEC rows must equal size(Cube,3).");
    }
}

template <typename T>
void cube2imageTyped(const mxArray *CubeArray,
                     const mxArray *SubCCDSECArray,
                     const mxArray *NoOverlapCCDSECArray,
                     const mxArray *NewNoOverlapCCDSECArray,
                     mxArray *&OutArray)
{
    const T *Cube = static_cast<const T *>(mxGetData(CubeArray));

    const double *SubCCDSEC          = mxGetPr(SubCCDSECArray);
    const double *NoOverlapCCDSEC    = mxGetPr(NoOverlapCCDSECArray);
    const double *NewNoOverlapCCDSEC = mxGetPr(NewNoOverlapCCDSECArray);

    const mwSize *CubeDims = mxGetDimensions(CubeArray);
    const mwSize SubNy = CubeDims[0];
    const mwSize SubNx = CubeDims[1];
    const mwSize Nim   = CubeDims[2];
    const mwSize PlaneSize = SubNy * SubNx;

    // Infer full image size from Sub_CCDSEC
    mwSize FullNx = 0;
    mwSize FullNy = 0;

    const double *SubXmax = SubCCDSEC + Nim;
    const double *SubYmax = SubCCDSEC + 3 * Nim;

    for (mwSize i = 0; i < Nim; ++i) {
        if (!mxIsFinite(SubXmax[i]) || !mxIsFinite(SubYmax[i])) {
            mexErrMsgIdAndTxt("cube2image:CCDSEC",
                              "Sub_CCDSEC contains NaN or Inf.");
        }
        if (std::floor(SubXmax[i]) != SubXmax[i] || std::floor(SubYmax[i]) != SubYmax[i]) {
            mexErrMsgIdAndTxt("cube2image:CCDSEC",
                              "Sub_CCDSEC must contain integer values.");
        }

        const mwSize Xmax = static_cast<mwSize>(SubXmax[i]);
        const mwSize Ymax = static_cast<mwSize>(SubYmax[i]);

        if (Xmax > FullNx) FullNx = Xmax;
        if (Ymax > FullNy) FullNy = Ymax;
    }

    if (FullNx == 0 || FullNy == 0) {
        mwSize Dims[2] = {0, 0};
        if (mxIsLogical(CubeArray)) {
            OutArray = mxCreateLogicalArray(2, Dims);
        } else {
            OutArray = mxCreateNumericArray(2, Dims, mxGetClassID(CubeArray), mxREAL);
        }
        return;
    }

    mwSize OutDims[2] = {FullNy, FullNx};
    if (mxIsLogical(CubeArray)) {
        OutArray = mxCreateLogicalArray(2, OutDims);
    } else {
        OutArray = mxCreateNumericArray(2, OutDims, mxGetClassID(CubeArray), mxREAL);
    }

    T *Out = static_cast<T *>(mxGetData(OutArray));

    const double *NoXmin = NoOverlapCCDSEC;
    const double *NoXmax = NoOverlapCCDSEC + Nim;
    const double *NoYmin = NoOverlapCCDSEC + 2 * Nim;
    const double *NoYmax = NoOverlapCCDSEC + 3 * Nim;

    const double *NewXmin = NewNoOverlapCCDSEC;
    const double *NewXmax = NewNoOverlapCCDSEC + Nim;
    const double *NewYmin = NewNoOverlapCCDSEC + 2 * Nim;
    const double *NewYmax = NewNoOverlapCCDSEC + 3 * Nim;

    mwSize *DstBase = static_cast<mwSize *>(mxMalloc(Nim * sizeof(mwSize)));
    mwSize *SrcBase = static_cast<mwSize *>(mxMalloc(Nim * sizeof(mwSize)));
    mwSize *CopyNx  = static_cast<mwSize *>(mxMalloc(Nim * sizeof(mwSize)));
    mwSize *CopyNy  = static_cast<mwSize *>(mxMalloc(Nim * sizeof(mwSize)));

    for (mwSize i = 0; i < Nim; ++i) {
        const mwSize DXmin = checkedIndex(NoXmin[i], 1, FullNx, "NoOverlap Xmin");
        const mwSize DXmax = checkedIndex(NoXmax[i], 1, FullNx, "NoOverlap Xmax");
        const mwSize DYmin = checkedIndex(NoYmin[i], 1, FullNy, "NoOverlap Ymin");
        const mwSize DYmax = checkedIndex(NoYmax[i], 1, FullNy, "NoOverlap Ymax");

        const mwSize SXmin = checkedIndex(NewXmin[i], 1, SubNx, "NewNoOverlap Xmin");
        const mwSize SXmax = checkedIndex(NewXmax[i], 1, SubNx, "NewNoOverlap Xmax");
        const mwSize SYmin = checkedIndex(NewYmin[i], 1, SubNy, "NewNoOverlap Ymin");
        const mwSize SYmax = checkedIndex(NewYmax[i], 1, SubNy, "NewNoOverlap Ymax");

        if (DXmax < DXmin || DYmax < DYmin) {
            mxFree(DstBase);
            mxFree(SrcBase);
            mxFree(CopyNx);
            mxFree(CopyNy);
            mexErrMsgIdAndTxt("cube2image:CCDSEC",
                              "Invalid NoOverlapCCDSEC row.");
        }

        if (SXmax < SXmin || SYmax < SYmin) {
            mxFree(DstBase);
            mxFree(SrcBase);
            mxFree(CopyNx);
            mxFree(CopyNy);
            mexErrMsgIdAndTxt("cube2image:CCDSEC",
                              "Invalid NewNoOverlapCCDSEC row.");
        }

        const mwSize NXd = DXmax - DXmin + 1;
        const mwSize NYd = DYmax - DYmin + 1;
        const mwSize NXs = SXmax - SXmin + 1;
        const mwSize NYs = SYmax - SYmin + 1;

        if (NXd != NXs || NYd != NYs) {
            mxFree(DstBase);
            mxFree(SrcBase);
            mxFree(CopyNx);
            mxFree(CopyNy);
            mexErrMsgIdAndTxt("cube2image:SizeMismatch",
                              "NoOverlapCCDSEC and NewNoOverlapCCDSEC sizes must match row by row.");
        }

        DstBase[i] = (DXmin - 1) * FullNy + (DYmin - 1);
        SrcBase[i] = i * PlaneSize + (SXmin - 1) * SubNy + (SYmin - 1);
        CopyNx[i]  = NXd;
        CopyNy[i]  = NYd;
    }

    #ifdef _OPENMP
    #pragma omp parallel for if(Nim > 8)
    #endif
    for (mwSignedIndex i = 0; i < static_cast<mwSignedIndex>(Nim); ++i) {
        T *DstBasePtr = Out + DstBase[i];
        const T *SrcBasePtr = Cube + SrcBase[i];
        const mwSize NX = CopyNx[i];
        const mwSize NY = CopyNy[i];

        for (mwSize ix = 0; ix < NX; ++ix) {
            T *Dst = DstBasePtr + ix * FullNy;
            const T *Src = SrcBasePtr + ix * SubNy;
            std::memcpy(Dst, Src, NY * sizeof(T));
        }
    }

    mxFree(DstBase);
    mxFree(SrcBase);
    mxFree(CopyNx);
    mxFree(CopyNy);
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    checkInputs(nlhs, nrhs, prhs);

    switch (mxGetClassID(prhs[0])) {
        case mxDOUBLE_CLASS:
            cube2imageTyped<double>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxSINGLE_CLASS:
            cube2imageTyped<float>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxINT8_CLASS:
            cube2imageTyped<int8_T>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxUINT8_CLASS:
            cube2imageTyped<uint8_T>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxINT16_CLASS:
            cube2imageTyped<int16_T>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxUINT16_CLASS:
            cube2imageTyped<uint16_T>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxINT32_CLASS:
            cube2imageTyped<int32_T>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxUINT32_CLASS:
            cube2imageTyped<uint32_T>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxINT64_CLASS:
            cube2imageTyped<int64_T>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxUINT64_CLASS:
            cube2imageTyped<uint64_T>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        case mxLOGICAL_CLASS:
            cube2imageTyped<mxLogical>(prhs[0], prhs[1], prhs[2], prhs[3], plhs[0]);
            break;
        default:
            mexErrMsgIdAndTxt("cube2image:Type",
                              "Unsupported Cube class.");
    }
}

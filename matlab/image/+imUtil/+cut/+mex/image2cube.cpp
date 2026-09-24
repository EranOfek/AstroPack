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
        mexErrMsgIdAndTxt("image2subImages:CCDSEC",
                          "%s contains NaN or Inf.", name);
    }

    if (std::floor(v) != v) {
        mexErrMsgIdAndTxt("image2subImages:CCDSEC",
                          "%s must contain integer values.", name);
    }

    if (v < static_cast<double>(minv) || v > static_cast<double>(maxv)) {
        mexErrMsgIdAndTxt("image2subImages:CCDSEC",
                          "%s is out of bounds.", name);
    }

    return static_cast<mwSize>(v);
}

static inline void checkInputs(int nlhs, int nrhs, const mxArray *prhs[])
{
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("image2subImages:Input",
                          "Two inputs required: Image, Sub_CCDSEC.");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("image2subImages:Output",
                          "One output only.");
    }

    if (mxGetNumberOfDimensions(prhs[0]) != 2) {
        mexErrMsgIdAndTxt("image2subImages:Image",
                          "Image must be a 2D matrix.");
    }

    if (mxIsSparse(prhs[0])) {
        mexErrMsgIdAndTxt("image2subImages:Image",
                          "Sparse images are not supported.");
    }

    if (mxIsComplex(prhs[0])) {
        mexErrMsgIdAndTxt("image2subImages:Image",
                          "Complex images are not supported.");
    }

    if (!mxIsDouble(prhs[1]) || mxIsComplex(prhs[1])) {
        mexErrMsgIdAndTxt("image2subImages:SubCCDSEC",
                          "Sub_CCDSEC must be a real double matrix.");
    }

    if (mxGetN(prhs[1]) != 4) {
        mexErrMsgIdAndTxt("image2subImages:SubCCDSEC",
                          "Sub_CCDSEC must be N x 4.");
    }
}

template <typename T>
void extractSubImagesTyped(const mxArray *ImageArray,
                           const mxArray *SubCCDSECArray,
                           mxArray *&OutArray)
{
    const T *Image = static_cast<const T *>(mxGetData(ImageArray));
    const double *SubCCDSEC = mxGetPr(SubCCDSECArray);

    const mwSize Ny   = mxGetM(ImageArray);
    const mwSize Nx   = mxGetN(ImageArray);
    const mwSize Nsec = mxGetM(SubCCDSECArray);

    if (Nsec == 0) {
        mwSize Dims[3] = {0, 0, 0};
        if (mxIsLogical(ImageArray)) {
            OutArray = mxCreateLogicalArray(3, Dims);
        } else {
            OutArray = mxCreateNumericArray(3, Dims, mxGetClassID(ImageArray), mxREAL);
        }
        return;
    }

    const double *XminCol = SubCCDSEC;
    const double *XmaxCol = SubCCDSEC + Nsec;
    const double *YminCol = SubCCDSEC + 2 * Nsec;
    const double *YmaxCol = SubCCDSEC + 3 * Nsec;

    // Validate first row and get common size
    const mwSize Xmin1 = checkedIndex(XminCol[0], 1, Nx, "Xmin");
    const mwSize Xmax1 = checkedIndex(XmaxCol[0], 1, Nx, "Xmax");
    const mwSize Ymin1 = checkedIndex(YminCol[0], 1, Ny, "Ymin");
    const mwSize Ymax1 = checkedIndex(YmaxCol[0], 1, Ny, "Ymax");

    if (Xmax1 < Xmin1 || Ymax1 < Ymin1) {
        mexErrMsgIdAndTxt("image2subImages:CCDSEC",
                          "Invalid first CCDSEC.");
    }

    const mwSize SubNx = Xmax1 - Xmin1 + 1;
    const mwSize SubNy = Ymax1 - Ymin1 + 1;
    const mwSize PlaneSize = SubNx * SubNy;

    mwSize Dims[3] = {SubNy, SubNx, Nsec};

    if (mxIsLogical(ImageArray)) {
        OutArray = mxCreateLogicalArray(3, Dims);
    } else {
        OutArray = mxCreateNumericArray(3, Dims, mxGetClassID(ImageArray), mxREAL);
    }

    T *Out = static_cast<T *>(mxGetData(OutArray));

    // Precompute source base pointers (as offsets in elements)
    mwSize *BaseOffset = static_cast<mwSize *>(mxMalloc(Nsec * sizeof(mwSize)));

    for (mwSize Isec = 0; Isec < Nsec; ++Isec) {
        const mwSize Xmin = checkedIndex(XminCol[Isec], 1, Nx, "Xmin");
        const mwSize Xmax = checkedIndex(XmaxCol[Isec], 1, Nx, "Xmax");
        const mwSize Ymin = checkedIndex(YminCol[Isec], 1, Ny, "Ymin");
        const mwSize Ymax = checkedIndex(YmaxCol[Isec], 1, Ny, "Ymax");

        if (Xmax < Xmin || Ymax < Ymin) {
            mxFree(BaseOffset);
            mexErrMsgIdAndTxt("image2subImages:CCDSEC",
                              "Invalid CCDSEC row.");
        }

        if ((Xmax - Xmin + 1) != SubNx || (Ymax - Ymin + 1) != SubNy) {
            mxFree(BaseOffset);
            mexErrMsgIdAndTxt("image2subImages:SizeMismatch",
                              "All CCDSEC rows must have the same size.");
        }

        const mwSize X0 = Xmin - 1;
        const mwSize Y0 = Ymin - 1;
        BaseOffset[Isec] = X0 * Ny + Y0;
    }

    // Parallel over cutouts
    #ifdef _OPENMP
    #pragma omp parallel for if(Nsec > 8)
    #endif
    for (mwSignedIndex Isec = 0; Isec < static_cast<mwSignedIndex>(Nsec); ++Isec) {
        const T *SrcBase = Image + BaseOffset[Isec];
        T *OutPlane = Out + static_cast<mwSize>(Isec) * PlaneSize;

        for (mwSize Ix = 0; Ix < SubNx; ++Ix) {
            const T *Src = SrcBase + Ix * Ny;
            T *Dst = OutPlane + Ix * SubNy;
            std::memcpy(Dst, Src, SubNy * sizeof(T));
        }
    }

    mxFree(BaseOffset);
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    checkInputs(nlhs, nrhs, prhs);

    switch (mxGetClassID(prhs[0])) {
        case mxDOUBLE_CLASS:
            extractSubImagesTyped<double>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxSINGLE_CLASS:
            extractSubImagesTyped<float>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxINT8_CLASS:
            extractSubImagesTyped<int8_T>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxUINT8_CLASS:
            extractSubImagesTyped<uint8_T>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxINT16_CLASS:
            extractSubImagesTyped<int16_T>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxUINT16_CLASS:
            extractSubImagesTyped<uint16_T>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxINT32_CLASS:
            extractSubImagesTyped<int32_T>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxUINT32_CLASS:
            extractSubImagesTyped<uint32_T>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxINT64_CLASS:
            extractSubImagesTyped<int64_T>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxUINT64_CLASS:
            extractSubImagesTyped<uint64_T>(prhs[0], prhs[1], plhs[0]);
            break;
        case mxLOGICAL_CLASS:
            extractSubImagesTyped<mxLogical>(prhs[0], prhs[1], plhs[0]);
            break;
        default:
            mexErrMsgIdAndTxt("image2subImages:Type",
                              "Unsupported image class.");
    }
}

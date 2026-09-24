#include "mex.h"
#include <string>
#include <algorithm>
#include <cctype>
#include <vector>
#include <limits>
#include <cmath>

static std::string toLower(const std::string& S) {
    std::string Out = S;
    std::transform(Out.begin(), Out.end(), Out.begin(),
                   [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return Out;
}

static mxClassID getClassID(const std::string& TypeStr, bool& IsLogical) {
    IsLogical = false;
    std::string T = toLower(TypeStr);

    if (T == "double") return mxDOUBLE_CLASS;
    if (T == "single") return mxSINGLE_CLASS;
    if (T == "int8")   return mxINT8_CLASS;
    if (T == "uint8")  return mxUINT8_CLASS;
    if (T == "int16")  return mxINT16_CLASS;
    if (T == "uint16") return mxUINT16_CLASS;
    if (T == "int32")  return mxINT32_CLASS;
    if (T == "uint32") return mxUINT32_CLASS;
    if (T == "int64")  return mxINT64_CLASS;
    if (T == "uint64") return mxUINT64_CLASS;

    if (T == "logical" || T == "bool" || T == "boolean") {
        IsLogical = true;
        return mxLOGICAL_CLASS;
    }

    mexErrMsgIdAndTxt("allocateUninit:UnsupportedType",
                      "Unsupported type.");
    return mxDOUBLE_CLASS;
}

static std::string getStringFromMxArray(const mxArray* Arr) {
    if (!mxIsChar(Arr)) {
        mexErrMsgIdAndTxt("allocateUninit:TypeInput",
                          "Second input must be a char array, e.g. 'double'.");
    }

    char* Buf = mxArrayToString(Arr);
    if (Buf == NULL) {
        mexErrMsgIdAndTxt("allocateUninit:TypeConversion",
                          "Failed to convert type input to C string.");
    }

    std::string Result(Buf);
    mxFree(Buf);
    return Result;
}

static std::vector<mwSize> parseSizeVector(const mxArray* Arr) {
    if (!mxIsDouble(Arr) || mxIsComplex(Arr) || mxIsSparse(Arr)) {
        mexErrMsgIdAndTxt("allocateUninit:SizeInput",
                          "Size must be a real, full double vector.");
    }

    mwSize NumElem = mxGetNumberOfElements(Arr);
    if (NumElem < 2) {
        mexErrMsgIdAndTxt("allocateUninit:SizeLength",
                          "Size must contain at least two elements.");
    }

    const double* Ptr = mxGetPr(Arr);
    std::vector<mwSize> Dims(NumElem);

    for (mwSize I = 0; I < NumElem; ++I) {
        double V = Ptr[I];

        if (!(V >= 0.0) || V != floor(V)) {
            mexErrMsgIdAndTxt("allocateUninit:InvalidDimension",
                              "All dimensions must be non-negative integers.");
        }

        if (V > static_cast<double>(std::numeric_limits<mwSize>::max())) {
            mexErrMsgIdAndTxt("allocateUninit:DimensionTooLarge",
                              "A dimension exceeds mwSize range.");
        }

        Dims[I] = static_cast<mwSize>(V);
    }

    return Dims;
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("allocateUninit:NumInputs",
                          "Usage: Data = allocateUninit(Size, 'type')");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("allocateUninit:NumOutputs",
                          "One output only.");
    }

    std::vector<mwSize> Dims = parseSizeVector(prhs[0]);
    std::string TypeStr = getStringFromMxArray(prhs[1]);

    bool IsLogical = false;
    mxClassID ClassID = getClassID(TypeStr, IsLogical);

    if (IsLogical) {
        plhs[0] = mxCreateLogicalArray(static_cast<mwSize>(Dims.size()), Dims.data());
        return;
    }

#if defined(MX_HAS_INTERLEAVED_COMPLEX)
    plhs[0] = mxCreateUninitNumericArray(static_cast<mwSize>(Dims.size()),
                                         Dims.data(),
                                         ClassID,
                                         mxREAL);
#else
    plhs[0] = mxCreateNumericArray(static_cast<mwSize>(Dims.size()),
                                   Dims.data(),
                                   ClassID,
                                   mxREAL);
#endif

    if (plhs[0] == NULL) {
        mexErrMsgIdAndTxt("allocateUninit:AllocationFailed",
                          "Allocation failed.");
    }
}

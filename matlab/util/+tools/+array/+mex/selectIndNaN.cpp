#include "mex.h"
#include <cmath>
#include <cstdint>
#include <limits>
#include <type_traits>

template <typename T>
inline double ToDouble(T Val) {
    return static_cast<double>(Val);
}

template <>
inline double ToDouble<float>(float Val) {
    return static_cast<double>(Val);
}

template <>
inline double ToDouble<double>(double Val) {
    return Val;
}

template <typename T>
bool IsIntegerValued(T Val) {
    double D = static_cast<double>(Val);
    return std::floor(D) == D;
}

template <>
bool IsIntegerValued<double>(double Val) {
    return std::floor(Val) == Val;
}

template <>
bool IsIntegerValued<float>(float Val) {
    return std::floor(static_cast<double>(Val)) == static_cast<double>(Val);
}

template <typename TInd>
void ParseIndices(const TInd* IndPtr,
                  mwSize M,
                  mwSize Nrow,
                  mwIndex* RowInd,
                  bool* IsNaNRow)
{
    for (mwSize I = 0; I < M; ++I) {
        double V = ToDouble(IndPtr[I]);

        if constexpr (std::is_floating_point<TInd>::value) {
            if (std::isnan(V)) {
                IsNaNRow[I] = true;
                RowInd[I] = 0;
                continue;
            }
        }

        if (!IsIntegerValued(IndPtr[I])) {
            mexErrMsgIdAndTxt("selectIndNaN:InvalidIndex",
                              "All finite elements in Ind must be integer-valued.");
        }

        if (V < 1.0 || V > static_cast<double>(Nrow)) {
            mexErrMsgIdAndTxt("selectIndNaN:IndexOutOfRange",
                              "Indices in Ind must be between 1 and size(Matrix,1).");
        }

        IsNaNRow[I] = false;
        RowInd[I] = static_cast<mwIndex>(V - 1.0);  // zero-based
    }
}

template <typename TMat, typename TOut>
void CopyRowsToOutput(const TMat* MatrixPtr,
                      mwSize Nrow,
                      mwSize Ncol,
                      const mwIndex* RowInd,
                      const bool* IsNaNRow,
                      mwSize M,
                      TOut* OutPtr)
{
    const TOut NaNVal = std::numeric_limits<TOut>::quiet_NaN();

    for (mwSize Col = 0; Col < Ncol; ++Col) {
        const TMat* ColPtrIn = MatrixPtr + Col * Nrow;
        TOut* ColPtrOut = OutPtr + Col * M;

        for (mwSize I = 0; I < M; ++I) {
            ColPtrOut[I] = IsNaNRow[I] ? NaNVal
                                       : static_cast<TOut>(ColPtrIn[RowInd[I]]);
        }
    }
}

template <typename TInd>
void DispatchMatrixType(const mxArray* Matrix,
                        const mxArray* Ind,
                        mxArray*& Out)
{
    const mwSize Nrow = mxGetM(Matrix);
    const mwSize Ncol = mxGetN(Matrix);
    const mwSize M    = mxGetNumberOfElements(Ind);

    mwIndex* RowInd = static_cast<mwIndex*>(mxCalloc(M, sizeof(mwIndex)));
    bool* IsNaNRow  = static_cast<bool*>(mxCalloc(M, sizeof(bool)));

    ParseIndices(static_cast<const TInd*>(mxGetData(Ind)), M, Nrow, RowInd, IsNaNRow);

    mxClassID ClassID = mxGetClassID(Matrix);

    switch (ClassID) {
        case mxDOUBLE_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const double*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxSINGLE_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxSINGLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const float*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<float*>(mxGetData(Out)));
            break;
        }

        case mxINT8_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const int8_T*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxUINT8_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const uint8_T*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxINT16_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const int16_T*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxUINT16_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const uint16_T*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxINT32_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const int32_T*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxUINT32_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const uint32_T*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxINT64_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const int64_T*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxUINT64_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const uint64_T*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        case mxLOGICAL_CLASS: {
            Out = mxCreateNumericMatrix(M, Ncol, mxDOUBLE_CLASS, mxREAL);
            CopyRowsToOutput(static_cast<const mxLogical*>(mxGetData(Matrix)),
                             Nrow, Ncol, RowInd, IsNaNRow, M,
                             static_cast<double*>(mxGetData(Out)));
            break;
        }

        default:
            mxFree(RowInd);
            mxFree(IsNaNRow);
            mexErrMsgIdAndTxt("selectIndNaN:UnsupportedMatrixType",
                              "Matrix must be a real numeric or logical 2D array.");
    }

    mxFree(RowInd);
    mxFree(IsNaNRow);
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("selectIndNaN:NumInputs",
                          "Two inputs required: selectIndNaN(Matrix, Ind).");
    }

    if (nlhs > 1) {
        mexErrMsgIdAndTxt("selectIndNaN:NumOutputs",
                          "One output only.");
    }

    const mxArray* Matrix = prhs[0];
    const mxArray* Ind    = prhs[1];

    if (mxGetNumberOfDimensions(Matrix) != 2) {
        mexErrMsgIdAndTxt("selectIndNaN:MatrixDim",
                          "Matrix must be 2D.");
    }

    if (mxIsComplex(Matrix) || !(mxIsNumeric(Matrix) || mxIsLogical(Matrix))) {
        mexErrMsgIdAndTxt("selectIndNaN:MatrixType",
                          "Matrix must be a real numeric or logical array.");
    }

    if (mxIsComplex(Ind) || !mxIsNumeric(Ind)) {
        mexErrMsgIdAndTxt("selectIndNaN:IndType",
                          "Ind must be a real numeric array.");
    }

    mxArray* Out = nullptr;

    switch (mxGetClassID(Ind)) {
        case mxDOUBLE_CLASS:
            DispatchMatrixType<double>(Matrix, Ind, Out);
            break;

        case mxSINGLE_CLASS:
            DispatchMatrixType<float>(Matrix, Ind, Out);
            break;

        case mxINT8_CLASS:
            DispatchMatrixType<int8_T>(Matrix, Ind, Out);
            break;

        case mxUINT8_CLASS:
            DispatchMatrixType<uint8_T>(Matrix, Ind, Out);
            break;

        case mxINT16_CLASS:
            DispatchMatrixType<int16_T>(Matrix, Ind, Out);
            break;

        case mxUINT16_CLASS:
            DispatchMatrixType<uint16_T>(Matrix, Ind, Out);
            break;

        case mxINT32_CLASS:
            DispatchMatrixType<int32_T>(Matrix, Ind, Out);
            break;

        case mxUINT32_CLASS:
            DispatchMatrixType<uint32_T>(Matrix, Ind, Out);
            break;

        case mxINT64_CLASS:
            DispatchMatrixType<int64_T>(Matrix, Ind, Out);
            break;

        case mxUINT64_CLASS:
            DispatchMatrixType<uint64_T>(Matrix, Ind, Out);
            break;

        default:
            mexErrMsgIdAndTxt("selectIndNaN:UnsupportedIndType",
                              "Ind must be single, double, or integer/unsigned integer.");
    }

    plhs[0] = Out;
}

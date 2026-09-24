#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace {

inline double NaNVal() {
    return std::numeric_limits<double>::quiet_NaN();
}

struct ArgsType {
    double Ninit         = 1000.0;
    double QuantileLow   = 0.1;
    double QuantileHigh  = 0.9;
    double Nlarge        = 1.0e4;
    double Nbin          = 20.0;
    bool   IntegerData   = true;
    bool   FitOnlyPeak   = false;
    double MaxVar        = 1.0e4;
};

inline bool getFieldIfExists(const mxArray* S, const char* Name, const mxArray*& Field) {
    if (S == nullptr || !mxIsStruct(S)) {
        Field = nullptr;
        return false;
    }
    Field = mxGetField(S, 0, Name);
    return (Field != nullptr);
}

inline bool getScalarField(const mxArray* S, const char* Name, double& Val) {
    const mxArray* F = nullptr;
    if (!getFieldIfExists(S, Name, F)) {
        return false;
    }
    if (!mxIsNumeric(F) || mxIsComplex(F) || mxGetNumberOfElements(F) != 1) {
        return false;
    }
    Val = mxGetScalar(F);
    return std::isfinite(Val);
}

inline bool getLogicalField(const mxArray* S, const char* Name, bool& Val) {
    const mxArray* F = nullptr;
    if (!getFieldIfExists(S, Name, F)) {
        return false;
    }

    if (mxIsLogical(F) && mxGetNumberOfElements(F) == 1) {
        Val = mxIsLogicalScalarTrue(F);
        return true;
    }

    if (mxIsNumeric(F) && !mxIsComplex(F) && mxGetNumberOfElements(F) == 1) {
        Val = (mxGetScalar(F) != 0.0);
        return true;
    }

    return false;
}

inline bool parseQuantileRange(const mxArray* S, double& Q1, double& Q2) {
    const mxArray* F = nullptr;
    if (!getFieldIfExists(S, "QuantileRange", F)) {
        return true;
    }
    if (!mxIsNumeric(F) || mxIsComplex(F) || mxGetNumberOfElements(F) != 2) {
        return false;
    }
    const double* P = mxGetPr(F);
    if (P == nullptr) {
        return false;
    }
    Q1 = P[0];
    Q2 = P[1];
    return std::isfinite(Q1) && std::isfinite(Q2);
}

inline bool loadArgs(const mxArray* S, ArgsType& A) {
    if (S == nullptr) {
        return true;
    }
    if (!mxIsStruct(S) || mxGetNumberOfElements(S) < 1) {
        return false;
    }

    double Tmp;
    bool   Btmp;

    if (getScalarField(S, "Ninit", Tmp))         A.Ninit       = Tmp;
    if (getScalarField(S, "Nlarge", Tmp))        A.Nlarge      = Tmp;
    if (getScalarField(S, "Nbin", Tmp))          A.Nbin        = Tmp;
    if (getScalarField(S, "MaxVar", Tmp))        A.MaxVar      = Tmp;
    if (getLogicalField(S, "IntegerData", Btmp)) A.IntegerData = Btmp;
    if (getLogicalField(S, "FitOnlyPeak", Btmp)) A.FitOnlyPeak = Btmp;
    if (!parseQuantileRange(S, A.QuantileLow, A.QuantileHigh)) {
        return false;
    }

    if (!(std::isfinite(A.Ninit) && A.Ninit > 0.0)) return false;
    if (!(std::isfinite(A.Nlarge) && A.Nlarge > 0.0)) return false;
    if (!(std::isfinite(A.Nbin) && A.Nbin > 0.0)) return false;
    if (!(std::isfinite(A.MaxVar) && A.MaxVar > 0.0)) return false;
    if (!(std::isfinite(A.QuantileLow) && std::isfinite(A.QuantileHigh))) return false;
    if (!(A.QuantileLow >= 0.0 && A.QuantileLow <= 1.0)) return false;
    if (!(A.QuantileHigh >= 0.0 && A.QuantileHigh <= 1.0)) return false;
    if (!(A.QuantileLow <= A.QuantileHigh)) return false;

    return true;
}

template <typename T>
inline double getValue(const T* X, mwSize I) {
    return static_cast<double>(X[I]);
}

inline double selectOrderStatistic(std::vector<double>& V, mwSize K) {
    std::nth_element(V.begin(), V.begin() + static_cast<std::ptrdiff_t>(K), V.end());
    return V[K];
}

inline double interpQuantileNth(const std::vector<double>& V, double Q) {
    const mwSize N = static_cast<mwSize>(V.size());
    if (N == 0) {
        return NaNVal();
    }
    if (N == 1) {
        return V[0];
    }
    if (Q <= 0.0) {
        return *std::min_element(V.begin(), V.end());
    }
    if (Q >= 1.0) {
        return *std::max_element(V.begin(), V.end());
    }

    const double Pos = Q * static_cast<double>(N - 1);
    const mwSize I0 = static_cast<mwSize>(std::floor(Pos));
    const mwSize I1 = static_cast<mwSize>(std::ceil(Pos));
    const double Frac = Pos - static_cast<double>(I0);

    std::vector<double> Work(V);

    const double V0 = selectOrderStatistic(Work, I0);
    if (I0 == I1) {
        return V0;
    }

    const double V1 = selectOrderStatistic(Work, I1);
    return (1.0 - Frac) * V0 + Frac * V1;
}

inline double medianInPlace(std::vector<double>& V) {
    const mwSize N = static_cast<mwSize>(V.size());
    if (N == 0) return NaNVal();
    const mwSize K = N / 2;
    std::nth_element(V.begin(), V.begin() + static_cast<std::ptrdiff_t>(K), V.end());
    double M = V[K];
    if ((N & 1U) == 0U) {
        const double V2 = *std::max_element(V.begin(), V.begin() + static_cast<std::ptrdiff_t>(K));
        M = 0.5 * (M + V2);
    }
    return M;
}

inline bool solve3x3(
    double A00, double A01, double A02,
    double A10, double A11, double A12,
    double A20, double A21, double A22,
    double B0,  double B1,  double B2,
    double& X0, double& X1, double& X2)
{
    double M[3][4] = {
        {A00, A01, A02, B0},
        {A10, A11, A12, B1},
        {A20, A21, A22, B2}
    };

    for (int K = 0; K < 3; ++K) {
        int Pivot = K;
        double MaxAbs = std::fabs(M[K][K]);
        for (int I = K + 1; I < 3; ++I) {
            const double AbsVal = std::fabs(M[I][K]);
            if (AbsVal > MaxAbs) {
                MaxAbs = AbsVal;
                Pivot = I;
            }
        }

        if (!(MaxAbs > 0.0) || !std::isfinite(MaxAbs)) {
            return false;
        }

        if (Pivot != K) {
            for (int J = K; J < 4; ++J) {
                std::swap(M[K][J], M[Pivot][J]);
            }
        }

        const double Diag = M[K][K];
        for (int J = K; J < 4; ++J) {
            M[K][J] /= Diag;
        }

        for (int I = 0; I < 3; ++I) {
            if (I == K) continue;
            const double F = M[I][K];
            for (int J = K; J < 4; ++J) {
                M[I][J] -= F * M[K][J];
            }
        }
    }

    X0 = M[0][3];
    X1 = M[1][3];
    X2 = M[2][3];

    return std::isfinite(X0) && std::isfinite(X1) && std::isfinite(X2);
}

template <typename T>
bool runCore(
    const T* Array,
    mwSize Npt,
    const ArgsType& Args,
    double& Mode,
    double& Var,
    double& Method)
{
    Mode = NaNVal();
    Var = NaNVal();
    Method = NaNVal();

    if (Array == nullptr || Npt == 0) {
        return false;
    }

    const mwSize StepSampleSmall = std::max<mwSize>(1, static_cast<mwSize>(std::floor(static_cast<double>(Npt) / Args.Ninit)));
    const mwSize StepSampleFinal = std::max<mwSize>(1, static_cast<mwSize>(std::floor(static_cast<double>(Npt) / Args.Nlarge)));

    const mwSize Nsmall = (Npt + StepSampleSmall - 1) / StepSampleSmall;
    std::vector<double> SmallArray(Nsmall);

    int AllFiniteSmall = 1;
#ifdef _OPENMP
#pragma omp parallel for reduction(&:AllFiniteSmall)
#endif
    for (mwSignedIndex I = 0; I < static_cast<mwSignedIndex>(Nsmall); ++I) {
        const mwSize Idx = static_cast<mwSize>(I) * StepSampleSmall;
        const double V = getValue(Array, Idx);
        SmallArray[static_cast<mwSize>(I)] = V;
        AllFiniteSmall = AllFiniteSmall & static_cast<int>(std::isfinite(V));
    }

    if (!AllFiniteSmall) {
        return false;
    }

    const double Bound1 = interpQuantileNth(SmallArray, Args.QuantileLow);
    const double Bound2 = interpQuantileNth(SmallArray, Args.QuantileHigh);

    if (!(std::isfinite(Bound1) && std::isfinite(Bound2))) {
        return false;
    }

    double Xmin, Xmax, HistStep;

    if (Args.IntegerData) {
        Xmin = std::floor(Bound1) - 0.5;
        Xmax = std::ceil(Bound2) + 0.5;

        if ((Xmax - Xmin) < 3.0) {
            Xmin -= 2.0;
            Xmax += 2.0;
        }

        HistStep = std::max(1.0, std::floor((Xmax - Xmin) / Args.Nbin));
    } else {
        Xmin = Bound1;
        Xmax = Bound2;
        HistStep = (Xmax - Xmin) / Args.Nbin;
    }

    if (!(std::isfinite(Xmin) && std::isfinite(Xmax) && std::isfinite(HistStep))) {
        return false;
    }
    if (!(Xmax > Xmin) || !(HistStep > 0.0)) {
        return false;
    }

    const double InvHistStep = 1.0 / HistStep;
    const double Span = Xmax - Xmin;
    mwSize Nedges = static_cast<mwSize>(std::floor(Span * InvHistStep)) + 1;
    if (Nedges < 2) {
        return false;
    }

    const double LastEdge = Xmin + HistStep * static_cast<double>(Nedges - 1);
    if (LastEdge < Xmax) {
        ++Nedges;
    }

    if (Nedges < 2 || Nedges > static_cast<mwSize>(1e8)) {
        return false;
    }

    const mwSize Nbins = Nedges - 1;
    std::vector<double> Nhist(Nbins, 0.0);

    mwSize ImaxHist = 0;

#ifdef _OPENMP
    const int Nthreads = omp_get_max_threads();
    std::vector< std::vector<double> > Hpriv(static_cast<mwSize>(Nthreads), std::vector<double>(Nbins, 0.0));
    int AllFiniteHist = 1;

#pragma omp parallel reduction(&:AllFiniteHist)
    {
        const int Tid = omp_get_thread_num();
        std::vector<double>& H = Hpriv[static_cast<mwSize>(Tid)];

#pragma omp for
        for (mwSignedIndex I = 0; I < static_cast<mwSignedIndex>(Npt); I += static_cast<mwSignedIndex>(StepSampleFinal)) {
            const double V = getValue(Array, static_cast<mwSize>(I));
            AllFiniteHist = AllFiniteHist & static_cast<int>(std::isfinite(V));
            if (!std::isfinite(V)) {
                continue;
            }

            if (V < Xmin || V > Xmax) {
                continue;
            }

            mwSize Bin;
            if (V == Xmax) {
                Bin = Nbins - 1;
            } else {
                const double F = (V - Xmin) * InvHistStep;
                if (!(F >= 0.0)) {
                    continue;
                }
                Bin = static_cast<mwSize>(std::floor(F));
                if (Bin >= Nbins) {
                    continue;
                }
            }
            H[Bin] += 1.0;
        }
    }

    if (!AllFiniteHist) {
        return false;
    }

    double VmaxHist = -1.0;
    for (mwSize I = 0; I < Nbins; ++I) {
        double Sum = 0.0;
        for (int Thr = 0; Thr < Nthreads; ++Thr) {
            Sum += Hpriv[static_cast<mwSize>(Thr)][I];
        }
        Nhist[I] = Sum;
        if (Sum > VmaxHist) {
            VmaxHist = Sum;
            ImaxHist = I;
        }
    }
#else
    double VmaxHist = -1.0;
    for (mwSize I = 0; I < Npt; I += StepSampleFinal) {
        const double V = getValue(Array, I);
        if (!std::isfinite(V)) {
            return false;
        }

        if (V < Xmin || V > Xmax) {
            continue;
        }

        mwSize Bin;
        if (V == Xmax) {
            Bin = Nbins - 1;
        } else {
            const double F = (V - Xmin) * InvHistStep;
            if (!(F >= 0.0)) {
                continue;
            }
            Bin = static_cast<mwSize>(std::floor(F));
            if (Bin >= Nbins) {
                continue;
            }
        }

        Nhist[Bin] += 1.0;
    }

    for (mwSize I = 0; I < Nbins; ++I) {
        if (Nhist[I] > VmaxHist) {
            VmaxHist = Nhist[I];
            ImaxHist = I;
        }
    }
#endif

    auto fallbackMethod2 = [&](void) -> bool {
        Method = 2.0;

        const mwSize Nfinal = (Npt + StepSampleFinal - 1) / StepSampleFinal;
        std::vector<double> FinalArray;
        FinalArray.reserve(Nfinal);

        for (mwSize I = 0; I < Npt; I += StepSampleFinal) {
            const double V = getValue(Array, I);
            if (!std::isfinite(V)) {
                return false;
            }
            FinalArray.push_back(V);
        }

        if (FinalArray.empty()) {
            return false;
        }

        std::vector<double> Tmp;
        Tmp.reserve(FinalArray.size());
        for (mwSize I = 0; I < FinalArray.size(); ++I) {
            const double V = FinalArray[I];
            if (V > Xmin && V < Xmax) {
                Tmp.push_back(V);
            }
        }

        if (Tmp.empty()) {
            Tmp = FinalArray;
        }

        Mode = medianInPlace(Tmp);
        if (!std::isfinite(Mode)) {
            return false;
        }

        std::vector<double> AbsDev(FinalArray.size());
        const mwSize Nf = static_cast<mwSize>(FinalArray.size());
        const double* PtrIn = FinalArray.data();
        double* PtrOut = AbsDev.data();
        const double M = Mode;

#ifdef _OPENMP
#pragma omp parallel for
#endif
        for (mwSignedIndex I = 0; I < static_cast<mwSignedIndex>(Nf); ++I) {
            PtrOut[I] = std::fabs(PtrIn[I] - M);
        }

        const double MAD = medianInPlace(AbsDev);
        if (!std::isfinite(MAD)) {
            return false;
        }

        const double Std = 1.482602218505602 * MAD;
        Var = Std * Std;
        return std::isfinite(Var);
    };

    mwSize Nflag = 0;
    double Mode0 = NaNVal();

    if (Args.FitOnlyPeak) {
        const mwSize I1 = (ImaxHist > 2) ? (ImaxHist - 2) : 0;
        const mwSize I2 = std::min<mwSize>(Nbins - 1, ImaxHist + 2);

        double CmaxSel = -1.0;
        for (mwSize I = I1; I <= I2; ++I) {
            const double C = Nhist[I];
            if (C <= 0.0) {
                continue;
            }
            ++Nflag;
            if (C > CmaxSel) {
                CmaxSel = C;
                Mode0 = Xmin + HistStep * (static_cast<double>(I) + 0.5);
            }
        }

        if (Nflag < 5 || !std::isfinite(Mode0)) {
            return fallbackMethod2();
        }

        double S0 = 0.0, S1 = 0.0, S2 = 0.0, S3 = 0.0, S4 = 0.0;
        double T0 = 0.0, T1 = 0.0, T2 = 0.0;

        for (mwSize I = I1; I <= I2; ++I) {
            const double C = Nhist[I];
            if (C <= 0.0) {
                continue;
            }

            const double X  = (Xmin + HistStep * (static_cast<double>(I) + 0.5)) - Mode0;
            const double Y  = std::log(C);
            const double X2 = X * X;

            S0 += 1.0;
            S1 += X;
            S2 += X2;
            S3 += X2 * X;
            S4 += X2 * X2;

            T0 += Y;
            T1 += X * Y;
            T2 += X2 * Y;
        }

        double P0, P1, P2;
        if (!solve3x3(
                S0, S1, S2,
                S1, S2, S3,
                S2, S3, S4,
                T0, T1, T2,
                P0, P1, P2)) {
            return fallbackMethod2();
        }

        if (!(std::isfinite(P2)) || P2 == 0.0) {
            return fallbackMethod2();
        }

        Mode = Mode0 - 0.5 * P1 / P2;
        Var  = -0.5 / P2;
        Method = 1.0;
    } else {
        double CmaxSel = -1.0;
        for (mwSize I = 0; I < Nbins; ++I) {
            const double C = Nhist[I];
            if (C <= 0.0) {
                continue;
            }
            ++Nflag;
            if (C > CmaxSel) {
                CmaxSel = C;
                Mode0 = Xmin + HistStep * (static_cast<double>(I) + 0.5);
            }
        }

        if (Nflag < 5 || !std::isfinite(Mode0)) {
            return fallbackMethod2();
        }

        double S0 = 0.0, S1 = 0.0, S2 = 0.0, S3 = 0.0, S4 = 0.0;
        double T0 = 0.0, T1 = 0.0, T2 = 0.0;

#ifdef _OPENMP
#pragma omp parallel for reduction(+:S0,S1,S2,S3,S4,T0,T1,T2)
#endif
        for (mwSignedIndex I = 0; I < static_cast<mwSignedIndex>(Nbins); ++I) {
            const mwSize Ui = static_cast<mwSize>(I);
            const double C = Nhist[Ui];
            if (C <= 0.0) {
                continue;
            }

            const double X  = (Xmin + HistStep * (static_cast<double>(Ui) + 0.5)) - Mode0;
            const double Y  = std::log(C);
            const double X2 = X * X;

            S0 += 1.0;
            S1 += X;
            S2 += X2;
            S3 += X2 * X;
            S4 += X2 * X2;

            T0 += Y;
            T1 += X * Y;
            T2 += X2 * Y;
        }

        double P0, P1, P2;
        if (!solve3x3(
                S0, S1, S2,
                S1, S2, S3,
                S2, S3, S4,
                T0, T1, T2,
                P0, P1, P2)) {
            return fallbackMethod2();
        }

        if (!(std::isfinite(P2)) || P2 == 0.0) {
            return fallbackMethod2();
        }

        Mode = Mode0 - 0.5 * P1 / P2;
        Var  = -0.5 / P2;
        Method = 1.0;
    }

    if (!(std::isfinite(Mode) && std::isfinite(Var))) {
        return fallbackMethod2();
    }

    if (Var < 0.0 || Var > Args.MaxVar) {
        return fallbackMethod2();
    }

    return true;
}

void setFailureOutputs(int Nlhs, mxArray* Plhs[]) {
    const double Nan = NaNVal();

    if (Nlhs >= 1) {
        Plhs[0] = mxCreateDoubleScalar(Nan);
    }
    if (Nlhs >= 2) {
        Plhs[1] = mxCreateDoubleScalar(Nan);
    }
    if (Nlhs >= 3) {
        Plhs[2] = mxCreateDoubleScalar(Nan);
    }
}

} // namespace

void mexFunction(int Nlhs, mxArray* Plhs[], int Nrhs, const mxArray* Prhs[])
{
    if (Nrhs < 1 || Nrhs > 2) {
        mexErrMsgIdAndTxt("modeVar_SampleHist_mex:Input",
                          "Usage: [Mode, Var, Method] = modeVar_SampleHist_mex(Array, Args)");
    }

    if (Nlhs > 3) {
        mexErrMsgIdAndTxt("modeVar_SampleHist_mex:Output",
                          "Too many output arguments.");
    }

    const mxArray* ArrayIn = Prhs[0];
    if (!mxIsNumeric(ArrayIn) || mxIsComplex(ArrayIn) || mxIsSparse(ArrayIn)) {
        setFailureOutputs(Nlhs, Plhs);
        return;
    }

    const mxClassID ClassID = mxGetClassID(ArrayIn);
    if (!(ClassID == mxDOUBLE_CLASS || ClassID == mxSINGLE_CLASS ||
          ClassID == mxINT8_CLASS   || ClassID == mxUINT8_CLASS  ||
          ClassID == mxINT16_CLASS  || ClassID == mxUINT16_CLASS ||
          ClassID == mxINT32_CLASS  || ClassID == mxUINT32_CLASS ||
          ClassID == mxINT64_CLASS  || ClassID == mxUINT64_CLASS)) {
        setFailureOutputs(Nlhs, Plhs);
        return;
    }

    ArgsType Args;
    if (Nrhs >= 2) {
        if (!loadArgs(Prhs[1], Args)) {
            setFailureOutputs(Nlhs, Plhs);
            return;
        }
    }

    const mwSize Npt = mxGetNumberOfElements(ArrayIn);
    double Mode = NaNVal();
    double Var = NaNVal();
    double Method = NaNVal();
    bool Ok = false;

    switch (ClassID) {
        case mxDOUBLE_CLASS:
            Ok = runCore(static_cast<const double*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxSINGLE_CLASS:
            Ok = runCore(static_cast<const float*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxINT8_CLASS:
            Ok = runCore(static_cast<const int8_T*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxUINT8_CLASS:
            Ok = runCore(static_cast<const uint8_T*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxINT16_CLASS:
            Ok = runCore(static_cast<const int16_T*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxUINT16_CLASS:
            Ok = runCore(static_cast<const uint16_T*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxINT32_CLASS:
            Ok = runCore(static_cast<const int32_T*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxUINT32_CLASS:
            Ok = runCore(static_cast<const uint32_T*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxINT64_CLASS:
            Ok = runCore(static_cast<const int64_T*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        case mxUINT64_CLASS:
            Ok = runCore(static_cast<const uint64_T*>(mxGetData(ArrayIn)), Npt, Args, Mode, Var, Method);
            break;
        default:
            Ok = false;
            break;
    }

    if (!Ok) {
        setFailureOutputs(Nlhs, Plhs);
        return;
    }

    if (Nlhs >= 1) {
        Plhs[0] = mxCreateDoubleScalar(Mode);
    }
    if (Nlhs >= 2) {
        Plhs[1] = mxCreateDoubleScalar(Var);
    }
    if (Nlhs >= 3) {
        Plhs[2] = mxCreateDoubleScalar(Method);
    }
}

#include "mex.h"
#include <cmath>
#include <vector>
#include <algorithm>
#include <limits>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace {

template <typename T>
inline bool isFiniteT(T x) {
    return std::isfinite(static_cast<double>(x));
}

template <typename T>
inline T NaNVal() {
    return static_cast<T>(mxGetNaN());
}

enum class Mode {
    Empty,
    Scalar,
    Vector,
    Cube
};

struct Size3 {
    mwSize Ny{0}, Nx{0}, Nim{0}, Npix{0};
};

inline Size3 getImageSize(const mxArray* A) {
    if (!mxIsSingle(A) && !mxIsDouble(A)) {
        mexErrMsgIdAndTxt("wcoaddRobust_mex:Type", "Image must be single or double.");
    }

    mwSize nd = mxGetNumberOfDimensions(A);
    const mwSize* dims = mxGetDimensions(A);

    Size3 S;
    if (nd == 2) {
        S.Ny = dims[0];
        S.Nx = dims[1];
        S.Nim = 1;
    } else if (nd == 3) {
        S.Ny = dims[0];
        S.Nx = dims[1];
        S.Nim = dims[2];
    } else {
        mexErrMsgIdAndTxt("wcoaddRobust_mex:Dim", "Image must be 2D or 3D.");
    }

    S.Npix = S.Ny * S.Nx;
    return S;
}

template <typename T>
T getScalarAny(const mxArray* A) {
    if (!mxIsNumeric(A) || mxIsComplex(A) || mxIsSparse(A) || mxIsEmpty(A) || mxGetNumberOfElements(A) != 1) {
        mexErrMsgIdAndTxt("wcoaddRobust_mex:Scalar", "Expected numeric scalar.");
    }
    return static_cast<T>(mxGetScalar(A));
}

template <typename T>
struct InputAccessor {
    Mode mode{Mode::Empty};
    const T* ptr{nullptr};
    T scalar{0};
    mwSize Nim{0};
    mwSize Npix{0};

    inline T get(mwSize p, mwSize k) const {
        switch (mode) {
            case Mode::Empty:
                return static_cast<T>(0);
            case Mode::Scalar:
                return scalar;
            case Mode::Vector:
                return ptr[k];
            case Mode::Cube:
                return ptr[p + k * Npix];
        }
        return static_cast<T>(0);
    }
};

template <typename T>
InputAccessor<T> parseScalarVectorCube(const mxArray* A, const Size3& S, const char* name, bool allowEmpty, T defaultScalar, bool useDefaultIfEmpty) {
    InputAccessor<T> out;
    out.Nim = S.Nim;
    out.Npix = S.Npix;

    if (A == nullptr || mxIsEmpty(A)) {
        if (allowEmpty) {
            out.mode = Mode::Empty;
            return out;
        }
        if (useDefaultIfEmpty) {
            out.mode = Mode::Scalar;
            out.scalar = defaultScalar;
            return out;
        }
        mexErrMsgIdAndTxt("wcoaddRobust_mex:Empty", "%s cannot be empty.", name);
    }

    if (!mxIsNumeric(A) || mxIsComplex(A) || mxIsSparse(A)) {
        mexErrMsgIdAndTxt("wcoaddRobust_mex:Numeric", "%s must be a full real numeric array.", name);
    }

    if (mxGetNumberOfElements(A) == 1) {
        out.mode = Mode::Scalar;
        out.scalar = static_cast<T>(mxGetScalar(A));
        return out;
    }

    mxClassID cid = mxGetClassID(A);
    if constexpr (std::is_same<T, float>::value) {
        if (cid != mxSINGLE_CLASS) {
            mexErrMsgIdAndTxt("wcoaddRobust_mex:Type", "%s must be single when Image is single.", name);
        }
        out.ptr = static_cast<const T*>(mxGetData(A));
    } else {
        if (cid != mxDOUBLE_CLASS) {
            mexErrMsgIdAndTxt("wcoaddRobust_mex:Type", "%s must be double when Image is double.", name);
        }
        out.ptr = static_cast<const T*>(mxGetData(A));
    }

    mwSize nd = mxGetNumberOfDimensions(A);
    const mwSize* dims = mxGetDimensions(A);
    mwSize ne = mxGetNumberOfElements(A);

    if (nd <= 2 && ne == S.Nim) {
        out.mode = Mode::Vector;
        return out;
    }

    if (nd == 3 && dims[0] == S.Ny && dims[1] == S.Nx && dims[2] == S.Nim) {
        out.mode = Mode::Cube;
        return out;
    }

    mexErrMsgIdAndTxt("wcoaddRobust_mex:Shape", "%s must be scalar, vector of length Nim, or cube of size Ny x Nx x Nim.", name);
    return out;
}

template <typename T>
struct FAccessor {
    bool deriveFromZP{false};
    InputAccessor<T> F;
    InputAccessor<T> ZP;
    T ZP0{static_cast<T>(25)};

    inline T get(mwSize p, mwSize k) const {
        if (!deriveFromZP) {
            return F.get(p, k);
        } else {
            T zp = ZP.get(p, k);
            return static_cast<T>(std::pow(10.0, 0.4 * static_cast<double>(ZP0 - zp)));
        }
    }
};

template <typename T>
FAccessor<T> parseFandZP(const mxArray* A_F, const mxArray* A_ZP, const mxArray* A_ZP0, const Size3& S) {
    FAccessor<T> out;
    out.ZP0 = (A_ZP0 == nullptr || mxIsEmpty(A_ZP0)) ? static_cast<T>(25) : getScalarAny<T>(A_ZP0);

    if (A_F != nullptr && !mxIsEmpty(A_F)) {
        out.deriveFromZP = false;
        out.F = parseScalarVectorCube<T>(A_F, S, "F", false, static_cast<T>(0), false);
    } else {
        out.deriveFromZP = true;
        if (A_ZP == nullptr || mxIsEmpty(A_ZP)) {
            out.ZP.mode = Mode::Scalar;
            out.ZP.scalar = static_cast<T>(25);
            out.ZP.Nim = S.Nim;
            out.ZP.Npix = S.Npix;
        } else {
            out.ZP = parseScalarVectorCube<T>(A_ZP, S, "ZP", false, static_cast<T>(25), false);
            if (out.ZP.mode == Mode::Cube) {
                mexErrMsgIdAndTxt("wcoaddRobust_mex:Shape", "ZP must be scalar or vector of length Nim.");
            }
        }
    }
    return out;
}

template <typename T>
inline void insertionSortPairs(T* x, T* w, mwSize n) {
    for (mwSize i = 1; i < n; ++i) {
        T keyX = x[i];
        T keyW = w[i];
        mwSize j = i;
        while (j > 0 && x[j - 1] > keyX) {
            x[j] = x[j - 1];
            w[j] = w[j - 1];
            --j;
        }
        x[j] = keyX;
        w[j] = keyW;
    }
}

template <typename T>
inline T weightedMedianInPlace(T* x, T* w, mwSize n) {
    insertionSortPairs(x, w, n);

    double sumW = 0.0;
    for (mwSize i = 0; i < n; ++i) {
        sumW += static_cast<double>(w[i]);
    }
    double halfW = 0.5 * sumW;

    double cumW = 0.0;
    for (mwSize i = 0; i < n; ++i) {
        cumW += static_cast<double>(w[i]);
        if (cumW >= halfW) {
            return x[i];
        }
    }

    return x[n - 1];
}

template <typename T>
inline mwSize removeMinMax(T* val, unsigned char* keep, mwSize Nim, mwSize nvalid) {
    if (nvalid == 0) return 0;

    bool any = false;
    mwSize imin = 0;
    mwSize imax = 0;
    T vmin = NaNVal<T>();
    T vmax = NaNVal<T>();

    for (mwSize k = 0; k < Nim; ++k) {
        if (!keep[k]) continue;

        if (!any) {
            any = true;
            imin = k;
            imax = k;
            vmin = val[k];
            vmax = val[k];
        } else {
            if (val[k] < vmin) {
                vmin = val[k];
                imin = k;
            }
            if (val[k] > vmax) {
                vmax = val[k];
                imax = k;
            }
        }
    }

    if (!any) return 0;

    if (keep[imin]) {
        keep[imin] = 0;
        --nvalid;
    }

    if (imax != imin && keep[imax]) {
        keep[imax] = 0;
        --nvalid;
    }

    return nvalid;
}

template <typename T>
inline bool computeWeightedMean(const T* val, const T* wgt, const unsigned char* keep, mwSize Nim, T& mean, T& sumW) {
    double sw = 0.0;
    double swx = 0.0;

    for (mwSize k = 0; k < Nim; ++k) {
        if (!keep[k]) continue;
        sw += static_cast<double>(wgt[k]);
        swx += static_cast<double>(wgt[k]) * static_cast<double>(val[k]);
    }

    if (sw == 0.0) {
        mean = NaNVal<T>();
        sumW = static_cast<T>(0);
        return false;
    }

    sumW = static_cast<T>(sw);
    mean = static_cast<T>(swx / sw);
    return true;
}

template <typename T>
inline bool computeWeightedMeanAndStdMethod1(const T* val,
                                             const T* wgt,
                                             const unsigned char* keep,
                                             mwSize Nim,
                                             T& meanWeighted,
                                             T& sumW,
                                             T& stdUnweighted) {
    double sw  = 0.0;
    double swx = 0.0;
    double sx  = 0.0;
    double sx2 = 0.0;
    mwSize n   = 0;

    for (mwSize k = 0; k < Nim; ++k) {
        if (!keep[k]) continue;

        double x = static_cast<double>(val[k]);
        double w = static_cast<double>(wgt[k]);

        sw  += w;
        swx += w * x;
        sx  += x;
        sx2 += x * x;
        ++n;
    }

    if (sw == 0.0) {
        meanWeighted  = NaNVal<T>();
        sumW          = static_cast<T>(0);
        stdUnweighted = NaNVal<T>();
        return false;
    }

    meanWeighted = static_cast<T>(swx / sw);
    sumW         = static_cast<T>(sw);

    if (n <= 1) {
        stdUnweighted = NaNVal<T>();
    } else {
        double var = (sx2 - (sx * sx) / static_cast<double>(n)) / static_cast<double>(n - 1);
        if (var < 0.0 && var > -1e-12) {
            var = 0.0;
        }
        stdUnweighted = static_cast<T>(std::sqrt(var));
    }

    return true;
}

template <typename T>
inline T computeStdMethod2(const T* val, const unsigned char* keep, mwSize Nim, T center) {
    mwSize n = 0;
    double mad = 0.0;

    for (mwSize k = 0; k < Nim; ++k) {
        if (!keep[k]) continue;
        mad += std::abs(static_cast<double>(val[k] - center));
        ++n;
    }

    if (n == 0) return NaNVal<T>();

    return static_cast<T>(1.253 * (mad / static_cast<double>(n)));
}

template <typename T>
inline bool buildCompactValid(const T* val, const T* wgt, const unsigned char* keep, mwSize Nim, T* xbuf, T* wbuf, mwSize& nvalid) {
    nvalid = 0;
    for (mwSize k = 0; k < Nim; ++k) {
        if (!keep[k]) continue;
        xbuf[nvalid] = val[k];
        wbuf[nvalid] = wgt[k];
        ++nvalid;
    }
    return nvalid > 0;
}

template <typename T>
inline T computeStdMethod3(const T* val, const T* wgt, const unsigned char* keep, mwSize Nim, T& center, T* xbuf, T* wbuf, T* abuf) {
    mwSize nvalid = 0;

    if (!buildCompactValid(val, wgt, keep, Nim, xbuf, wbuf, nvalid)) {
        center = NaNVal<T>();
        return NaNVal<T>();
    }

    center = weightedMedianInPlace(xbuf, wbuf, nvalid);

    for (mwSize i = 0; i < nvalid; ++i) {
        abuf[i] = static_cast<T>(std::abs(static_cast<double>(xbuf[i] - center)));
    }

    for (mwSize i = 0; i < nvalid; ++i) {
        xbuf[i] = abuf[i];
    }

    return static_cast<T>(1.4826 * static_cast<double>(weightedMedianInPlace(xbuf, wbuf, nvalid)));
}

template <typename T>
void runKernelSimple(const T* Image,
                     const Size3& S,
                     const InputAccessor<T>& Back,
                     const InputAccessor<T>& Var,
                     const FAccessor<T>& Facc,
                     T* Coadd,
                     T* CoaddVar) {
    #pragma omp parallel for schedule(static)
    for (mwIndex p = 0; p < static_cast<mwIndex>(S.Npix); ++p) {
        double sw = 0.0;
        double swx = 0.0;

        for (mwSize k = 0; k < S.Nim; ++k) {
            T I  = Image[p + k * S.Npix];
            T B  = (Back.mode == Mode::Empty) ? static_cast<T>(0) : Back.get(p, k);
            T Fv = Facc.get(p, k);
            T V  = Var.get(p, k);

            if (!(isFiniteT(I) && isFiniteT(B) && isFiniteT(Fv) && isFiniteT(V))) {
                continue;
            }

            double X = static_cast<double>(Fv) * static_cast<double>(I - B);
            double W = 1.0 / (static_cast<double>(Fv) * static_cast<double>(Fv) * static_cast<double>(V));

            if (!(std::isfinite(X) && std::isfinite(W))) {
                continue;
            }

            sw  += W;
            swx += W * X;
        }

        if (sw == 0.0) {
            Coadd[p] = NaNVal<T>();
            if (CoaddVar) CoaddVar[p] = NaNVal<T>();
        } else {
            Coadd[p] = static_cast<T>(swx / sw);
            if (CoaddVar) CoaddVar[p] = static_cast<T>(1.0 / sw);
        }
    }
}

template <typename T>
void runKernelGeneral(const T* Image,
                      const Size3& S,
                      const InputAccessor<T>& Back,
                      const InputAccessor<T>& Var,
                      const FAccessor<T>& Facc,
                      bool RemoveMinMax,
                      int Niter,
                      int StdMethod,
                      T SigmaLow,
                      T SigmaHigh,
                      T* Coadd,
                      T* CoaddVar) {
    #pragma omp parallel
    {
        std::vector<T> val(S.Nim);
        std::vector<T> wgt(S.Nim);
        std::vector<unsigned char> keep(S.Nim);
        std::vector<T> xbuf(S.Nim);
        std::vector<T> wbuf(S.Nim);
        std::vector<T> abuf(S.Nim);

        #pragma omp for schedule(static)
        for (mwIndex p = 0; p < static_cast<mwIndex>(S.Npix); ++p) {
            mwSize nvalid = 0;

            for (mwSize k = 0; k < S.Nim; ++k) {
                T I = Image[p + k * S.Npix];
                T B = (Back.mode == Mode::Empty) ? static_cast<T>(0) : Back.get(p, k);
                T Fv = Facc.get(p, k);
                T V = Var.get(p, k);

                T X = NaNVal<T>();
                T W = NaNVal<T>();
                unsigned char K = 0;

                if (isFiniteT(I) && isFiniteT(B) && isFiniteT(Fv) && isFiniteT(V)) {
                    X = static_cast<T>(Fv * (I - B));
                    W = static_cast<T>(1.0 / (static_cast<double>(Fv) * static_cast<double>(Fv) * static_cast<double>(V)));
                    K = (isFiniteT(X) && isFiniteT(W)) ? 1 : 0;
                }

                val[k] = X;
                wgt[k] = W;
                keep[k] = K;
                nvalid += static_cast<mwSize>(K);
            }

            if (nvalid == 0) {
                Coadd[p] = NaNVal<T>();
                if (CoaddVar) CoaddVar[p] = NaNVal<T>();
                continue;
            }

            if (RemoveMinMax) {
                nvalid = removeMinMax(val.data(), keep.data(), S.Nim, nvalid);
                if (nvalid == 0) {
                    Coadd[p] = NaNVal<T>();
                    if (CoaddVar) CoaddVar[p] = NaNVal<T>();
                    continue;
                }
            }

            bool haveFinalStats = false;
            T finalCenter = NaNVal<T>();
            T finalSumW = static_cast<T>(0);

            for (int iter = 0; iter < Niter; ++iter) {
                T center = NaNVal<T>();
                T sumW   = static_cast<T>(0);
                T stdI   = NaNVal<T>();
                T clipCenter = NaNVal<T>();

                if (StdMethod == 1) {
                    if (!computeWeightedMeanAndStdMethod1(val.data(), wgt.data(), keep.data(), S.Nim,
                                                          center, sumW, stdI)) {
                        break;
                    }
                    clipCenter = center;

                } else if (StdMethod == 2) {
                    if (!computeWeightedMean(val.data(), wgt.data(), keep.data(), S.Nim, center, sumW)) {
                        break;
                    }
                    clipCenter = center;
                    stdI = computeStdMethod2(val.data(), keep.data(), S.Nim, clipCenter);

                } else if (StdMethod == 3) {
                    if (!computeWeightedMean(val.data(), wgt.data(), keep.data(), S.Nim, center, sumW)) {
                        break;
                    }
                    stdI = computeStdMethod3(val.data(), wgt.data(), keep.data(), S.Nim,
                                             clipCenter, xbuf.data(), wbuf.data(), abuf.data());

                } else {
                    mexErrMsgIdAndTxt("wcoaddRobust_mex:StdMethod", "StdMethod must be 1, 2, or 3.");
                }

                bool changed = false;
                for (mwSize k = 0; k < S.Nim; ++k) {
                    if (!keep[k]) continue;
                    double z = (static_cast<double>(val[k]) - static_cast<double>(clipCenter)) / static_cast<double>(stdI);
                    if (z < static_cast<double>(SigmaLow) || z > static_cast<double>(SigmaHigh)) {
                        keep[k] = 0;
                        --nvalid;
                        changed = true;
                    }
                }

                if (!changed) {
                    finalCenter = center;
                    finalSumW = sumW;
                    haveFinalStats = true;
                    break;
                }

                if (nvalid == 0) {
                    break;
                }
            }

            if (nvalid == 0) {
                Coadd[p] = NaNVal<T>();
                if (CoaddVar) CoaddVar[p] = NaNVal<T>();
                continue;
            }

            if (haveFinalStats) {
                Coadd[p] = finalCenter;
                if (CoaddVar) {
                    CoaddVar[p] = static_cast<T>(1.0 / static_cast<double>(finalSumW));
                }
                continue;
            }

            T center = NaNVal<T>();
            T sumW = static_cast<T>(0);

            if (!computeWeightedMean(val.data(), wgt.data(), keep.data(), S.Nim, center, sumW)) {
                Coadd[p] = NaNVal<T>();
                if (CoaddVar) CoaddVar[p] = NaNVal<T>();
            } else {
                Coadd[p] = center;
                if (CoaddVar) {
                    CoaddVar[p] = static_cast<T>(1.0 / static_cast<double>(sumW));
                }
            }
        }
    }
}

template <typename T>
void runKernel(const mxArray* prhs[], int nrhs, mxArray* plhs[], bool wantVar) {
    const mxArray* AImage = prhs[0];
    Size3 S = getImageSize(AImage);

    const T* Image = static_cast<const T*>(mxGetData(AImage));

    const mxArray* ABack = (nrhs > 1) ? prhs[1] : nullptr;
    const mxArray* AVar  = (nrhs > 2) ? prhs[2] : nullptr;
    const mxArray* AF    = (nrhs > 3) ? prhs[3] : nullptr;
    const mxArray* AZP   = (nrhs > 4) ? prhs[4] : nullptr;
    const mxArray* AZP0  = (nrhs > 5) ? prhs[5] : nullptr;
    const mxArray* ARem  = (nrhs > 6) ? prhs[6] : nullptr;
    const mxArray* ANit  = (nrhs > 7) ? prhs[7] : nullptr;
    const mxArray* ASig  = (nrhs > 8) ? prhs[8] : nullptr;
    const mxArray* AStd  = (nrhs > 9) ? prhs[9] : nullptr;

    InputAccessor<T> Back = parseScalarVectorCube<T>(ABack, S, "Back", true, static_cast<T>(0), false);
    InputAccessor<T> Var  = parseScalarVectorCube<T>(AVar,  S, "Var", false, static_cast<T>(1), true);
    FAccessor<T> Facc     = parseFandZP<T>(AF, AZP, AZP0, S);

    bool RemoveMinMax = (ARem == nullptr || mxIsEmpty(ARem)) ? true : (mxGetScalar(ARem) != 0.0);
    int Niter = (ANit == nullptr || mxIsEmpty(ANit)) ? 1 : static_cast<int>(mxGetScalar(ANit));
    int StdMethod = (AStd == nullptr || mxIsEmpty(AStd)) ? 2 : static_cast<int>(mxGetScalar(AStd));

    T SigmaLow = static_cast<T>(-3);
    T SigmaHigh = static_cast<T>(3);
    if (ASig != nullptr && !mxIsEmpty(ASig)) {
        if (mxGetNumberOfElements(ASig) != 2) {
            mexErrMsgIdAndTxt("wcoaddRobust_mex:SigmaClip", "SigmaClip must have 2 elements.");
        }
        double* sp = mxGetPr(ASig);
        SigmaLow  = static_cast<T>(-std::abs(sp[0]));
        SigmaHigh = static_cast<T>( std::abs(sp[1]));
    }

    mwSize odims[2] = {S.Ny, S.Nx};
    mxClassID outClass = std::is_same<T, float>::value ? mxSINGLE_CLASS : mxDOUBLE_CLASS;

    plhs[0] = mxCreateNumericArray(2, odims, outClass, mxREAL);
    T* Coadd = static_cast<T*>(mxGetData(plhs[0]));

    T* CoaddVar = nullptr;
    if (wantVar) {
        plhs[1] = mxCreateNumericArray(2, odims, outClass, mxREAL);
        CoaddVar = static_cast<T*>(mxGetData(plhs[1]));
    }

    if (!RemoveMinMax && Niter == 0) {
        runKernelSimple<T>(Image, S, Back, Var, Facc, Coadd, CoaddVar);
    } else {
        runKernelGeneral<T>(Image, S, Back, Var, Facc, RemoveMinMax, Niter, StdMethod, SigmaLow, SigmaHigh, Coadd, CoaddVar);
    }
}

} // namespace

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs < 1 || nrhs > 10) {
        mexErrMsgIdAndTxt("wcoaddRobust_mex:Inputs",
            "Usage: [Coadd, CoaddVar] = wcoaddRobust_mex(Image, Back, Var, F, ZP, ZP0, RemoveMinMax, Niter, SigmaClip, StdMethod)");
    }

    if (nlhs > 2) {
        mexErrMsgIdAndTxt("wcoaddRobust_mex:Outputs", "Too many output arguments.");
    }

    bool wantVar = (nlhs >= 2);

    if (mxIsSingle(prhs[0])) {
        runKernel<float>(prhs, nrhs, plhs, wantVar);
    } else if (mxIsDouble(prhs[0])) {
        runKernel<double>(prhs, nrhs, plhs, wantVar);
    } else {
        mexErrMsgIdAndTxt("wcoaddRobust_mex:Type", "Image must be single or double.");
    }
}

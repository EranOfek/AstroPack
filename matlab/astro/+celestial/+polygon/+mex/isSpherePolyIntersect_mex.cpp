#include "mex.h"
#include <cmath>
#include <vector>
#include <limits>
#include <algorithm>
#include <type_traits>

#if defined(__GNUC__) || defined(__clang__)
    #define HAS_SINCOS 1
#else
    #define HAS_SINCOS 0
#endif

template <typename T>
struct Vec3 {
    T x, y, z;
};

template <typename T>
struct PolygonData {
    std::vector<T> Vx;
    std::vector<T> Vy;
    std::vector<T> Vz;
    std::vector<T> Vxn;
    std::vector<T> Vyn;
    std::vector<T> Vzn;
    std::vector<T> Nx;
    std::vector<T> Ny;
    std::vector<T> Nz;
    bool Valid = false;
};

template <typename T>
static inline bool isNanT(T x) {
    return std::isnan(static_cast<double>(x));
}

template <typename T>
static inline T dot3(T Ax, T Ay, T Az, T Bx, T By, T Bz) {
    return Ax * Bx + Ay * By + Az * Bz;
}

template <typename T>
static inline Vec3<T> cross3(const Vec3<T>& A, const Vec3<T>& B) {
    Vec3<T> C;
    C.x = A.y * B.z - A.z * B.y;
    C.y = A.z * B.x - A.x * B.z;
    C.z = A.x * B.y - A.y * B.x;
    return C;
}

template <typename T>
static inline T norm3(const Vec3<T>& A) {
    return std::sqrt(dot3(A.x, A.y, A.z, A.x, A.y, A.z));
}

template <typename T>
static inline bool normalize3(Vec3<T>& A) {
    T N = norm3(A);
    if (N <= T(0)) {
        return false;
    }
    T Inv = T(1) / N;
    A.x *= Inv;
    A.y *= Inv;
    A.z *= Inv;
    return true;
}

template <typename T>
static inline void sincosT(T x, T* s, T* c);

template <>
inline void sincosT<double>(double x, double* s, double* c) {
#if HAS_SINCOS
    ::sincos(x, s, c);
#else
    *s = std::sin(x);
    *c = std::cos(x);
#endif
}

template <>
inline void sincosT<float>(float x, float* s, float* c) {
#if HAS_SINCOS
    ::sincosf(x, s, c);
#else
    *s = std::sinf(x);
    *c = std::cosf(x);
#endif
}

template <typename T>
static inline Vec3<T> lonLatToUnitVec(T Lon, T Lat, bool IsDeg) {
    const T Deg2Rad = T(3.141592653589793238462643383279502884L / 180.0L);

    if (IsDeg) {
        Lon *= Deg2Rad;
        Lat *= Deg2Rad;
    }

    T SinLon, CosLon, SinLat, CosLat;
    sincosT<T>(Lon, &SinLon, &CosLon);
    sincosT<T>(Lat, &SinLat, &CosLat);

    Vec3<T> P;
    P.x = CosLat * CosLon;
    P.y = CosLat * SinLon;
    P.z = SinLat;
    return P;
}

static bool getBoolScalar(const mxArray* A, bool DefaultValue) {
    if (A == nullptr) {
        return DefaultValue;
    }

    if (mxIsLogical(A)) {
        if (mxGetNumberOfElements(A) != 1) {
            mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input", "Flag inputs must be scalar.");
        }
        return mxIsLogicalScalarTrue(A);
    }

    if (!mxIsDouble(A) && !mxIsSingle(A)) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input", "Flag inputs must be logical, single, or double scalars.");
    }

    if (mxGetNumberOfElements(A) != 1) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input", "Flag inputs must be scalar.");
    }

    return (mxGetScalar(A) != 0.0);
}

template <typename T>
static bool pointOnArc(const Vec3<T>& P,
                       const Vec3<T>& A,
                       const Vec3<T>& B,
                       const Vec3<T>& N,
                       T Tol,
                       bool IncludeEdge)
{
    T OnGC = std::abs(dot3(N.x, N.y, N.z, P.x, P.y, P.z));
    if (OnGC > Tol) {
        return false;
    }

    Vec3<T> AP = cross3(A, P);
    Vec3<T> PB = cross3(P, B);

    T S1 = dot3(AP.x, AP.y, AP.z, N.x, N.y, N.z);
    T S2 = dot3(PB.x, PB.y, PB.z, N.x, N.y, N.z);

    if (IncludeEdge) {
        return (S1 >= -Tol) && (S2 >= -Tol);
    } else {
        return (S1 > Tol) && (S2 > Tol);
    }
}

template <typename T>
static bool pointInsidePolygon(const Vec3<T>& P,
                               const PolygonData<T>& Poly,
                               T Tol,
                               bool IncludeEdge)
{
    const size_t Nedge = Poly.Nx.size();
    for (size_t e = 0; e < Nedge; ++e) {
        T S = dot3(Poly.Nx[e], Poly.Ny[e], Poly.Nz[e], P.x, P.y, P.z);
        if (IncludeEdge) {
            if (S < -Tol) {
                return false;
            }
        } else {
            if (S <= Tol) {
                return false;
            }
        }
    }
    return true;
}

template <typename T>
static PolygonData<T> preprocessPolygonColumn(const T* Lon,
                                              const T* Lat,
                                              mwSize Nvert,
                                              mwSize Col,
                                              bool IsDeg)
{
    PolygonData<T> Out;
    Out.Valid = false;

    std::vector<Vec3<T>> Vert;
    Vert.reserve(Nvert);

    const mwSize Offset = Col * Nvert;
    for (mwSize r = 0; r < Nvert; ++r) {
        T L = Lon[Offset + r];
        T B = Lat[Offset + r];
        if (isNanT(L) || isNanT(B)) {
            continue;
        }
        Vert.push_back(lonLatToUnitVec<T>(L, B, IsDeg));
    }

    if (Vert.size() < 3) {
        return Out;
    }

    {
        std::vector<Vec3<T>> Clean;
        Clean.reserve(Vert.size());
        Clean.push_back(Vert[0]);

        T DupTol = std::is_same<T,double>::value ? T(1e-28) : T(1e-12);

        for (size_t i = 1; i < Vert.size(); ++i) {
            T Dx = Vert[i].x - Clean.back().x;
            T Dy = Vert[i].y - Clean.back().y;
            T Dz = Vert[i].z - Clean.back().z;
            T D2 = Dx*Dx + Dy*Dy + Dz*Dz;
            if (D2 > DupTol) {
                Clean.push_back(Vert[i]);
            }
        }

        if (Clean.size() >= 2) {
            T Dx = Clean.front().x - Clean.back().x;
            T Dy = Clean.front().y - Clean.back().y;
            T Dz = Clean.front().z - Clean.back().z;
            T D2 = Dx*Dx + Dy*Dy + Dz*Dz;
            if (D2 <= DupTol) {
                Clean.pop_back();
            }
        }

        Vert.swap(Clean);
    }

    if (Vert.size() < 3) {
        return Out;
    }

    Vec3<T> Center{T(0), T(0), T(0)};
    for (size_t i = 0; i < Vert.size(); ++i) {
        Center.x += Vert[i].x;
        Center.y += Vert[i].y;
        Center.z += Vert[i].z;
    }

    if (!normalize3(Center)) {
        return Out;
    }

    const size_t Nv = Vert.size();
    Out.Vx.resize(Nv);
    Out.Vy.resize(Nv);
    Out.Vz.resize(Nv);
    Out.Vxn.resize(Nv);
    Out.Vyn.resize(Nv);
    Out.Vzn.resize(Nv);
    Out.Nx.reserve(Nv);
    Out.Ny.reserve(Nv);
    Out.Nz.reserve(Nv);

    for (size_t i = 0; i < Nv; ++i) {
        Out.Vx[i] = Vert[i].x;
        Out.Vy[i] = Vert[i].y;
        Out.Vz[i] = Vert[i].z;
    }

    for (size_t i = 0; i < Nv; ++i) {
        size_t i2 = (i + 1) % Nv;
        Out.Vxn[i] = Vert[i2].x;
        Out.Vyn[i] = Vert[i2].y;
        Out.Vzn[i] = Vert[i2].z;

        Vec3<T> N = cross3(Vert[i], Vert[i2]);
        T NN = norm3(N);
        if (NN <= T(1e-20)) {
            continue;
        }

        T Inv = T(1) / NN;
        N.x *= Inv;
        N.y *= Inv;
        N.z *= Inv;

        if (dot3(N.x, N.y, N.z, Center.x, Center.y, Center.z) < T(0)) {
            N.x = -N.x;
            N.y = -N.y;
            N.z = -N.z;
        }

        Out.Nx.push_back(N.x);
        Out.Ny.push_back(N.y);
        Out.Nz.push_back(N.z);
    }

    if (Out.Nx.size() < 3) {
        Out = PolygonData<T>();
        return Out;
    }

    Out.Valid = true;
    return Out;
}

template <typename T>
static bool polygonsIntersect(const PolygonData<T>& P1,
                              const PolygonData<T>& P2,
                              T Tol,
                              bool IncludeEdge)
{
    if (!P1.Valid || !P2.Valid) {
        return false;
    }

    for (size_t i = 0; i < P1.Vx.size(); ++i) {
        Vec3<T> P{P1.Vx[i], P1.Vy[i], P1.Vz[i]};
        if (pointInsidePolygon(P, P2, Tol, IncludeEdge)) {
            return true;
        }
    }

    for (size_t i = 0; i < P2.Vx.size(); ++i) {
        Vec3<T> P{P2.Vx[i], P2.Vy[i], P2.Vz[i]};
        if (pointInsidePolygon(P, P1, Tol, IncludeEdge)) {
            return true;
        }
    }

    for (size_t e1 = 0; e1 < P1.Vx.size(); ++e1) {
        Vec3<T> U1{P1.Vx[e1],  P1.Vy[e1],  P1.Vz[e1]};
        Vec3<T> U2{P1.Vxn[e1], P1.Vyn[e1], P1.Vzn[e1]};
        Vec3<T> N1{U1.y * U2.z - U1.z * U2.y,
                   U1.z * U2.x - U1.x * U2.z,
                   U1.x * U2.y - U1.y * U2.x};

        T N1n = norm3(N1);
        if (N1n <= T(1e-20)) {
            continue;
        }
        T Inv1 = T(1) / N1n;
        N1.x *= Inv1; N1.y *= Inv1; N1.z *= Inv1;

        for (size_t e2 = 0; e2 < P2.Vx.size(); ++e2) {
            Vec3<T> V1{P2.Vx[e2],  P2.Vy[e2],  P2.Vz[e2]};
            Vec3<T> V2{P2.Vxn[e2], P2.Vyn[e2], P2.Vzn[e2]};
            Vec3<T> N2{V1.y * V2.z - V1.z * V2.y,
                       V1.z * V2.x - V1.x * V2.z,
                       V1.x * V2.y - V1.y * V2.x};

            T N2n = norm3(N2);
            if (N2n <= T(1e-20)) {
                continue;
            }
            T Inv2 = T(1) / N2n;
            N2.x *= Inv2; N2.y *= Inv2; N2.z *= Inv2;

            Vec3<T> X = cross3(N1, N2);
            T Xn = norm3(X);
            if (Xn <= Tol) {
                continue;
            }

            T InvX = T(1) / Xn;
            X.x *= InvX; X.y *= InvX; X.z *= InvX;

            if (pointOnArc(X, U1, U2, N1, Tol, IncludeEdge) &&
                pointOnArc(X, V1, V2, N2, Tol, IncludeEdge)) {
                return true;
            }

            X.x = -X.x;
            X.y = -X.y;
            X.z = -X.z;

            if (pointOnArc(X, U1, U2, N1, Tol, IncludeEdge) &&
                pointOnArc(X, V1, V2, N2, Tol, IncludeEdge)) {
                return true;
            }
        }
    }

    return false;
}

template <typename T>
static void runMex(int nlhs, mxArray* plhs[],
                   int nrhs, const mxArray* prhs[])
{
    if (nrhs < 4 || nrhs > 6) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "Usage: Flag = sphericalPolygonsIntersectMex(LonPoly1, LatPoly1, LonPoly2, LatPoly2, [IsDeg], [IncludeEdge])");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Output", "One output only.");
    }

    const mxArray* LonPoly1A = prhs[0];
    const mxArray* LatPoly1A = prhs[1];
    const mxArray* LonPoly2A = prhs[2];
    const mxArray* LatPoly2A = prhs[3];

    if (mxGetClassID(LonPoly1A) != mxGetClassID(LatPoly1A) ||
        mxGetClassID(LonPoly1A) != mxGetClassID(LonPoly2A) ||
        mxGetClassID(LonPoly1A) != mxGetClassID(LatPoly2A)) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "All four numeric inputs must have the same class.");
    }

    if (!(mxIsDouble(LonPoly1A) || mxIsSingle(LonPoly1A))) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "Numeric inputs must be single or double.");
    }

    if (mxIsComplex(LonPoly1A) || mxIsComplex(LatPoly1A) ||
        mxIsComplex(LonPoly2A) || mxIsComplex(LatPoly2A)) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "Inputs must be real.");
    }

    mwSize Nvert1 = mxGetM(LonPoly1A);
    mwSize Npoly1 = mxGetN(LonPoly1A);
    mwSize Nvert2 = mxGetM(LonPoly2A);
    mwSize Npoly2 = mxGetN(LonPoly2A);

    if (mxGetM(LatPoly1A) != Nvert1 || mxGetN(LatPoly1A) != Npoly1) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "LonPoly1 and LatPoly1 must have identical size.");
    }
    if (mxGetM(LatPoly2A) != Nvert2 || mxGetN(LatPoly2A) != Npoly2) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "LonPoly2 and LatPoly2 must have identical size.");
    }

    if (Nvert1 < 3 || Nvert2 < 3) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "Each polygon must have at least 3 rows.");
    }

    if (!(Npoly1 == Npoly2 || Npoly1 == 1 || Npoly2 == 1)) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "Number of columns must be equal, or one side must have exactly one column.");
    }

    bool IsDeg = true;
    bool IncludeEdge = true;
    if (nrhs >= 5) {
        IsDeg = getBoolScalar(prhs[4], true);
    }
    if (nrhs >= 6) {
        IncludeEdge = getBoolScalar(prhs[5], true);
    }

    const T* LonPoly1 = static_cast<const T*>(mxGetData(LonPoly1A));
    const T* LatPoly1 = static_cast<const T*>(mxGetData(LatPoly1A));
    const T* LonPoly2 = static_cast<const T*>(mxGetData(LonPoly2A));
    const T* LatPoly2 = static_cast<const T*>(mxGetData(LatPoly2A));

    mwSize Ncmp = (Npoly1 > Npoly2 ? Npoly1 : Npoly2);

    plhs[0] = mxCreateLogicalMatrix(Ncmp, 1);
    mxLogical* Out = mxGetLogicals(plhs[0]);

    std::vector< PolygonData<T> > Polys1(Npoly1);
    std::vector< PolygonData<T> > Polys2(Npoly2);

    for (mwSize j = 0; j < Npoly1; ++j) {
        Polys1[j] = preprocessPolygonColumn<T>(LonPoly1, LatPoly1, Nvert1, j, IsDeg);
    }
    for (mwSize j = 0; j < Npoly2; ++j) {
        Polys2[j] = preprocessPolygonColumn<T>(LonPoly2, LatPoly2, Nvert2, j, IsDeg);
    }

    T Tol = std::is_same<T,double>::value ? T(1e-12) : T(1e-6);

    for (mwSize Icmp = 0; Icmp < Ncmp; ++Icmp) {
        mwSize Ipoly1 = (Npoly1 == 1 ? 0 : Icmp);
        mwSize Ipoly2 = (Npoly2 == 1 ? 0 : Icmp);

        bool Flag = polygonsIntersect<T>(Polys1[Ipoly1], Polys2[Ipoly2], Tol, IncludeEdge);
        Out[Icmp] = Flag ? mxLogical(1) : mxLogical(0);
    }
}

void mexFunction(int nlhs, mxArray* plhs[],
                 int nrhs, const mxArray* prhs[])
{
    if (nrhs < 4 || nrhs > 6) {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "Usage: Flag = sphericalPolygonsIntersectMex(LonPoly1, LatPoly1, LonPoly2, LatPoly2, [IsDeg], [IncludeEdge])");
    }

    mxClassID ClassID = mxGetClassID(prhs[0]);

    if (ClassID == mxDOUBLE_CLASS) {
        runMex<double>(nlhs, plhs, nrhs, prhs);
    } else if (ClassID == mxSINGLE_CLASS) {
        runMex<float>(nlhs, plhs, nrhs, prhs);
    } else {
        mexErrMsgIdAndTxt("sphericalPolygonsIntersectMex:Input",
            "Inputs must be single or double.");
    }
}

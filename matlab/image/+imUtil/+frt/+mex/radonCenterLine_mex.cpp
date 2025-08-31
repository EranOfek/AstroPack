// Build (Linux, GCC/Clang):
// mex -v -O \
//   CXXFLAGS="$CXXFLAGS -O3 -march=native -ffast-math -fopenmp -std=c++17" \
//   CXXOPTIMFLAGS="$CXXOPTIMFLAGS -O3 -march=native -ffast-math -fopenmp -std=c++17" \
//   LDFLAGS="$LDFLAGS -fopenmp" \
//   -largeArrayDims radonCenterLine_step1_mex.cpp

#include "mex.h"
#include <cmath>
#include <vector>
#include <limits>
#ifdef _OPENMP
  #include <omp.h>
#endif

// ---------- Zero-padded samplers ----------
template<typename Tin>
inline double GetPix(const Tin* I, mwSize M, mwSize /*N*/, mwIndex x, mwIndex y)
{
    // column-major: (y-1) + (x-1)*M
    return static_cast<double>( I[(mwIndex)(y-1) + (mwIndex)(x-1)*M] );
}

inline bool InBounds(mwIndex x, mwIndex y, mwSize M, mwSize N)
{
    return (x >= 1 && x <= (mwIndex)N && y >= 1 && y <= (mwIndex)M);
}

template<typename Tin>
inline double BilinearSample(const Tin* I, mwSize M, mwSize N, double x, double y)
{
    // 1-based continuous coords; outside → 0
    const double x0d = std::floor(x), y0d = std::floor(y);
    const mwIndex x0 = static_cast<mwIndex>(x0d);
    const mwIndex y0 = static_cast<mwIndex>(y0d);
    const mwIndex x1 = x0 + 1;
    const mwIndex y1 = y0 + 1;

    const double dx = x - x0d;
    const double dy = y - y0d;

    const double w00 = (1.0 - dx)*(1.0 - dy);
    const double w10 =        dx *(1.0 - dy);
    const double w01 = (1.0 - dx)*       dy ;
    const double w11 =        dx *       dy ;

    double s = 0.0;
    if (InBounds(x0,y0,M,N)) s += w00 * GetPix(I,M,N,x0,y0);
    if (InBounds(x1,y0,M,N)) s += w10 * GetPix(I,M,N,x1,y0);
    if (InBounds(x0,y1,M,N)) s += w01 * GetPix(I,M,N,x0,y1);
    if (InBounds(x1,y1,M,N)) s += w11 * GetPix(I,M,N,x1,y1);
    return s;
}

template<typename Tin>
inline double NearestSample(const Tin* I, mwSize M, mwSize N, double x, double y)
{
    const mwIndex xi = static_cast<mwIndex>(std::llround(x));
    const mwIndex yi = static_cast<mwIndex>(std::llround(y));
    if (!InBounds(xi,yi,M,N)) return 0.0;
    return GetPix(I,M,N,xi,yi);
}

// ---------- Core (unit step = 1 along arc-length) ----------
template<typename Tin, typename Tout>
void Compute(const Tin* Image, mwSize M, mwSize N,
             const double* Theta, mwSize L,
             bool UseDegrees, bool UseBilinear, bool UseNormalAngle,
             Tout* P)
{
    const double Xc = (static_cast<double>(N) + 1.0)*0.5;  // columns
    const double Yc = (static_cast<double>(M) + 1.0)*0.5;  // rows

    #pragma omp parallel for schedule(static)
    for (mwIndex k = 0; k < L; ++k)
    {
        const double th = UseDegrees ? (Theta[k] * M_PI / 180.0) : Theta[k];

        // Direction of the line:
        // - if UseNormalAngle=false (default): direction = [cos th, sin th]
        // - if UseNormalAngle=true  (MATLAB radon): direction ⟂ normal → [-sin th, cos th]
        const double dx = UseNormalAngle ? -std::sin(th) : std::cos(th);
        const double dy = UseNormalAngle ?  std::cos(th) : std::sin(th);
        // (dx,dy) is unit-length

        // Intersect parametric line (Xc,Yc) + t*(dx,dy) with image box:
        // x in [0.5, N+0.5], y in [0.5, M+0.5]
        double tL = -INFINITY, tU = INFINITY;
        if (std::abs(dx) > 0.0) {
            const double tx1 = (0.5   - Xc)/dx;
            const double tx2 = (N+0.5 - Xc)/dx;
            tL = std::max(tL, std::min(tx1,tx2));
            tU = std::min(tU, std::max(tx1,tx2));
        }
        if (std::abs(dy) > 0.0) {
            const double ty1 = (0.5   - Yc)/dy;
            const double ty2 = (M+0.5 - Yc)/dy;
            tL = std::max(tL, std::min(ty1,ty2));
            tU = std::min(tU, std::max(ty1,ty2));
        }
        if (!(std::isfinite(tL) && std::isfinite(tU)) || tL > tU) {
            P[k] = static_cast<Tout>(0);
            continue;
        }

        // Unit arc-length spacing → sample at integer t with t=0 at center
        const long long n0 = static_cast<long long>(std::ceil(tL));
        const long long n1 = static_cast<long long>(std::floor(tU));

        double sum = 0.0;
        if (UseBilinear) {
            for (long long n = n0; n <= n1; ++n) {
                const double x = Xc + static_cast<double>(n)*dx;
                const double y = Yc + static_cast<double>(n)*dy;
                sum += BilinearSample(Image, M, N, x, y);
            }
        } else {
            for (long long n = n0; n <= n1; ++n) {
                const double x = Xc + static_cast<double>(n)*dx;
                const double y = Yc + static_cast<double>(n)*dy;
                sum += NearestSample(Image, M, N, x, y);
            }
        }

        P[k] = static_cast<Tout>(sum);
    }
}

// ---------- MEX entry ----------
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    // Usage:
    //   P = radonCenterLine_step1_mex(Image, Theta, UseDegrees, UseBilinear, UseNormalAngle)
    //     Image         : MxN single/double, real
    //     Theta         : vector single/double, real
    //     UseDegrees    : logical (default true)   – true: Theta in degrees, false: radians
    //     UseBilinear   : logical (default true)   – true: bilinear, false: nearest
    //     UseNormalAngle: logical (default false)  – true: Theta is NORMAL angle; false: DIRECTION angle

    if (nrhs < 2) {
        mexErrMsgIdAndTxt("radonCenterLine_step1_mex:args",
            "Need at least Image and Theta.");
    }

    const mxArray* ImageMx = prhs[0];
    const mxArray* ThetaMx = prhs[1];

    if (mxGetNumberOfDimensions(ImageMx) != 2)
        mexErrMsgIdAndTxt("radonCenterLine_step1_mex:image","Image must be 2-D.");
    if (mxIsComplex(ImageMx))
        mexErrMsgIdAndTxt("radonCenterLine_step1_mex:image","Image must be real.");
    if (!(mxIsSingle(ImageMx) || mxIsDouble(ImageMx)))
        mexErrMsgIdAndTxt("radonCenterLine_step1_mex:image","Image must be single or double.");

    if (mxIsComplex(ThetaMx))
        mexErrMsgIdAndTxt("radonCenterLine_step1_mex:theta","Theta must be real.");
    if (!(mxIsSingle(ThetaMx) || mxIsDouble(ThetaMx)))
        mexErrMsgIdAndTxt("radonCenterLine_step1_mex:theta","Theta must be single or double.");

    bool UseDegrees     = true;
    bool UseBilinear    = true;
    bool UseNormalAngle = false;
    if (nrhs >= 3) UseDegrees     = mxIsLogicalScalarTrue(prhs[2]);
    if (nrhs >= 4) UseBilinear    = mxIsLogicalScalarTrue(prhs[3]);
    if (nrhs >= 5) UseNormalAngle = mxIsLogicalScalarTrue(prhs[4]);

    const mwSize L = (mwSize)mxGetNumberOfElements(ThetaMx);
    const mxClassID OutClass = mxGetClassID(ImageMx);

    // Output vector (same class as Image)
    plhs[0] = mxCreateNumericMatrix(L, 1, OutClass, mxREAL);

    const mwSize* D = mxGetDimensions(ImageMx);
    const mwSize M = D[0];
    const mwSize N = D[1];

    // Theta as double buffer (for trig)
    std::vector<double> Th(L);
    if (mxIsDouble(ThetaMx)) {
        const double* T = static_cast<const double*>(mxGetData(ThetaMx));
        std::copy(T, T+L, Th.begin());
    } else {
        const float* T = static_cast<const float*>(mxGetData(ThetaMx));
        for (mwIndex i=0; i<L; ++i) Th[i] = static_cast<double>(T[i]);
    }

    if (mxIsDouble(ImageMx)) {
        const double* Im = static_cast<const double*>(mxGetData(ImageMx));
        double* P = static_cast<double*>(mxGetData(plhs[0]));
        Compute<double,double>(Im, M, N, Th.data(), L,
                               UseDegrees, UseBilinear, UseNormalAngle, P);
    } else {
        const float* Im = static_cast<const float*>(mxGetData(ImageMx));
        float* P = static_cast<float*>(mxGetData(plhs[0]));
        Compute<float,float>(Im, M, N, Th.data(), L,
                             UseDegrees, UseBilinear, UseNormalAngle, P);
    }
}

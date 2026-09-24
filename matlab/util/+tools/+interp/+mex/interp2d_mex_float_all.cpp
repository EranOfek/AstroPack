#include "mex.h"
#include <algorithm>
#include <cmath>
#include <cstring>

#ifdef _OPENMP
  #include <omp.h>
#endif

// mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -fno-exceptions -fno-rtti -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" interp2d_mex_float_all.cpp                                 
// ===================== Utilities =====================

enum Method : int { NEAREST=0, LINEAR=1, CUBIC=2 };

static Method parse_method(const mxArray* m) {
    if (!mxIsChar(m)) mexErrMsgIdAndTxt("interp2d:method","method must be a string");
    char buf[16]; buf[0]='\0';
    mxGetString(m, buf, sizeof(buf));
    if (!std::strcmp(buf,"nearest")) return NEAREST;
    if (!std::strcmp(buf,"linear"))  return LINEAR;
    if (!std::strcmp(buf,"cubic"))   return CUBIC;
    mexErrMsgIdAndTxt("interp2d:method","Unknown method. Use 'nearest'|'linear'|'cubic'.");
    return LINEAR;
}

template <typename T> inline int clampi(T v, int lo, int hi){
    return (v<lo) ? lo : ((v>hi) ? hi : (int)v);
}

template <typename T>
inline bool is_strict_increasing(const T* G, int n){
    for (int i=0;i<n-1;++i) if (!(G[i] < G[i+1])) return false;
    return true;
}

// Near-uniform grid check (ascending). Returns origin, step, inv_step (as double).
template <typename T>
inline bool uniform_grid(const T* G, int n, double& g0, double& dg, double& inv_dg){
    g0 = static_cast<double>(G[0]);
    dg = (static_cast<double>(G[n-1]) - static_cast<double>(G[0])) / (double)(n-1);
    if (dg == 0.0) return false;
    const double tol = std::abs(dg) * 1e-12; // tight for double-precision match
    for (int k=1;k<n;++k){
        double pred = g0 + dg*k;
        if (std::abs(static_cast<double>(G[k]) - pred) > tol) return false;
    }
    inv_dg = 1.0 / dg;
    return true;
}

// Minimal index advance so that X[ix] <= x <= X[ix+1] (ascending X).
template <typename T>
inline int advance_ix(const T* X, int nx, int ix, double x){
    // step forward/backward minimally (no per-pixel binary search)
    while (ix + 1 < nx && static_cast<double>(X[ix+1]) <= x) ++ix;
    while (ix > 0 && static_cast<double>(X[ix]) > x) --ix;
    if (ix >= nx-1) ix = nx-2;
    return ix;
}

// ===================== Cubic kernel (MATLAB-compatible) =====================

// Keys cubic convolution kernel, a = -0.5, piecewise |s| form (stable & matches MATLAB).
inline double keys_h(double s){
    const double a = -0.5;
    s = std::abs(s);
    if (s <= 1.0) {
        return ((a+2.0)*s - (a+3.0))*s*s + 1.0;        // ((a+2)s - (a+3)) s^2 + 1
    } else if (s < 2.0) {
        return ((a*s - 5.0*a)*s + 8.0*a)*s - 4.0*a;    // a s^3 - 5a s^2 + 8a s - 4a
    } else {
        return 0.0;
    }
}

// Build 4 weights for position t in [0,1] at taps -1,0,1,2.
inline void keys_weights_piecewise(double t, double w[4]){
    w[0] = keys_h(t + 1.0);
    w[1] = keys_h(t + 0.0);
    w[2] = keys_h(1.0 - t);
    w[3] = keys_h(2.0 - t);
}

// ===================== Interpolation kernels =====================

template <typename GX, typename GY, typename ZT, typename OUT>
inline OUT bilinear_at(double x, double y,
                       const GX* X, const GY* Y, const ZT* Z,
                       int nx, int ny, int ix, int iy)
{
    const double xL = static_cast<double>(X[ix]);
    const double xR = static_cast<double>(X[ix+1]);
    const double yB = static_cast<double>(Y[iy]);
    const double yT = static_cast<double>(Y[iy+1]);
    const double inv_dx = (xR!=xL)? 1.0/(xR-xL) : 0.0;
    const double inv_dy = (yT!=yB)? 1.0/(yT-yB) : 0.0;

    double tx = (x - xL)*inv_dx; if (tx<0) tx=0; else if (tx>1) tx=1;
    double ty = (y - yB)*inv_dy; if (ty<0) ty=0; else if (ty>1) ty=1;

    const ZT* c0 = Z + ix*ny;
    const ZT* c1 = c0 + ny;

    const double z00 = static_cast<double>(c0[iy]);
    const double z01 = static_cast<double>(c0[iy+1]);
    const double z10 = static_cast<double>(c1[iy]);
    const double z11 = static_cast<double>(c1[iy+1]);

    const double zL = z00 + ty*(z01 - z00);
    const double zR = z10 + ty*(z11 - z10);
    return static_cast<OUT>(zL + tx*(zR - zL));
}

template <typename GX, typename GY, typename ZT, typename OUT>
inline OUT nearest_at(double x, double y,
                      const GX* X, const GY* Y, const ZT* Z,
                      int nx, int ny, int ix, int iy)
{
    int ixn = (std::fabs(static_cast<double>(X[ix]) - x) < std::fabs(static_cast<double>(X[ix+1]) - x)) ? ix : ix+1;
    int iyn = (std::fabs(static_cast<double>(Y[iy]) - y) < std::fabs(static_cast<double>(Y[iy+1]) - y)) ? iy : iy+1;
    ixn = clampi(ixn,0,nx-1);
    iyn = clampi(iyn,0,ny-1);
    return static_cast<OUT>(Z[iyn + ixn*ny]);
}

// Bicubic on a uniform grid with replicated borders.
// X0, inv_dX, Y0, inv_dY map physical x,y -> grid coords.
template <typename ZT, typename OUT>
inline OUT bicubic_uniform_at(double x, double y,
                              const ZT* Z, int nx, int ny,
                              double X0, double inv_dX, double Y0, double inv_dY)
{
    const double gx = (x - X0) * inv_dX;
    const double gy = (y - Y0) * inv_dY;

    int ix = (int)std::floor(gx);
    int iy = (int)std::floor(gy);
    double tx = gx - ix; if (tx < 0) tx = 0; else if (tx > 1) tx = 1;
    double ty = gy - iy; if (ty < 0) ty = 0; else if (ty > 1) ty = 1;

    auto clamp = [](int v, int lo, int hi){ return v<lo?lo:(v>hi?hi:v); };
    const int ixm = clamp(ix-1, 0, nx-1), ix0 = clamp(ix,   0, nx-1),
              ix1 = clamp(ix+1, 0, nx-1), ix2 = clamp(ix+2, 0, nx-1);
    const int iym = clamp(iy-1, 0, ny-1), iy0 = clamp(iy,   0, ny-1),
              iy1 = clamp(iy+1, 0, ny-1), iy2 = clamp(iy+2, 0, ny-1);

    const ZT* c0 = Z + ixm*ny;
    const ZT* c1 = Z + ix0*ny;
    const ZT* c2 = Z + ix1*ny;
    const ZT* c3 = Z + ix2*ny;

    double wx[4], wy[4];
    keys_weights_piecewise(tx, wx);
    keys_weights_piecewise(ty, wy);

    const double gy0 = wy[0]*static_cast<double>(c0[iym]) + wy[1]*static_cast<double>(c0[iy0])
                     + wy[2]*static_cast<double>(c0[iy1]) + wy[3]*static_cast<double>(c0[iy2]);
    const double gy1 = wy[0]*static_cast<double>(c1[iym]) + wy[1]*static_cast<double>(c1[iy0])
                     + wy[2]*static_cast<double>(c1[iy1]) + wy[3]*static_cast<double>(c1[iy2]);
    const double gy2 = wy[0]*static_cast<double>(c2[iym]) + wy[1]*static_cast<double>(c2[iy0])
                     + wy[2]*static_cast<double>(c2[iy1]) + wy[3]*static_cast<double>(c2[iy2]);
    const double gy3 = wy[0]*static_cast<double>(c3[iym]) + wy[1]*static_cast<double>(c3[iy0])
                     + wy[2]*static_cast<double>(c3[iy1]) + wy[3]*static_cast<double>(c3[iy2]);

    return static_cast<OUT>(wx[0]*gy0 + wx[1]*gy1 + wx[2]*gy2 + wx[3]*gy3);
}

// ===================== Driver (templated over types) =====================

template <typename GX, typename GY, typename ZT, typename OUT>
static void interp2d_mixed_core(const mxArray* Xarr, const mxArray* Yarr, const mxArray* Zarr,
                                const mxArray* XIarr, const mxArray* YIarr,
                                Method method, mxArray* ZOarr)
{
    const GX* X  = reinterpret_cast<const GX*>(mxGetData(Xarr));
    const GY* Y  = reinterpret_cast<const GY*>(mxGetData(Yarr));
    const ZT* Z  = reinterpret_cast<const ZT*>(mxGetData(Zarr));
    const GX* XI = reinterpret_cast<const GX*>(mxGetData(XIarr)); // allow XI type = GX or GY? We’ll assume same type as X for pointer; safe since we only read as double
    const GY* YI = reinterpret_cast<const GY*>(mxGetData(YIarr));

    const int nx = (int)mxGetNumberOfElements(Xarr);
    const int ny = (int)mxGetNumberOfElements(Yarr);

    const mwSize Rq = mxGetM(XIarr);
    const mwSize Cq = mxGetN(XIarr);

    OUT* ZO = reinterpret_cast<OUT*>(mxGetData(ZOarr));

    // Grid checks (ascending monotonic)
    if (!is_strict_increasing(X, nx) || !is_strict_increasing(Y, ny))
        mexErrMsgIdAndTxt("interp2d:monotonic","X and Y must be strictly increasing.");

    // Box limits (as double)
    const double xMin = static_cast<double>(X[0]);
    const double xMax = static_cast<double>(X[nx-1]);
    const double yMin = static_cast<double>(Y[0]);
    const double yMax = static_cast<double>(Y[ny-1]);

    // NaN of output type
    const double NaNd = mxGetNaN();
    const OUT NaNout = static_cast<OUT>(NaNd);

    // Uniform detection (for both fast mapping and cubic)
    double X0=0, dX=0, inv_dX=0, Y0=0, dY=0, inv_dY=0;
    const bool Xu = uniform_grid(X, nx, X0, dX, inv_dX);
    const bool Yu = uniform_grid(Y, ny, Y0, dY, inv_dY);

    // Parallelize over columns
    #pragma omp parallel for schedule(static)
    for (mwSize c=0; c<Cq; ++c){
        // Seed from top pixel
        const mwSize idx0 = 0 + c*Rq;
        const double x0 = static_cast<double>(XI[idx0]);
        const double y0 = static_cast<double>(YI[idx0]);

        int ix, iy;
        if (Xu) ix = clampi(std::floor((x0 - X0)*inv_dX), 0, nx-2);
        else {
            const GX* it = std::upper_bound(X, X+nx, static_cast<GX>(x0));
            ix = clampi((int)(it - X) - 1, 0, nx-2);
        }
        if (Yu) iy = clampi(std::floor((y0 - Y0)*inv_dY), 0, ny-2);
        else {
            const GY* it = std::upper_bound(Y, Y+ny, static_cast<GY>(y0));
            iy = clampi((int)(it - Y) - 1, 0, ny-2);
        }

        for (mwSize r=0; r<Rq; ++r){
            const mwSize idx = r + c*Rq;
            const double x = static_cast<double>(XI[idx]);
            const double y = static_cast<double>(YI[idx]);

            if (x < xMin || x > xMax || y < yMin || y > yMax) {
                ZO[idx] = NaNout; continue;
            }

            // Update indices (uniform -> O(1), else minimal advance)
            ix = Xu ? clampi(std::floor((x - X0)*inv_dX), 0, nx-2) : advance_ix(X, nx, ix, x);
            iy = Yu ? clampi(std::floor((y - Y0)*inv_dY), 0, ny-2) : advance_ix(Y, ny, iy, y);

            OUT v;
            if (method == NEAREST) {
                v = nearest_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy);
            } else if (method == LINEAR) {
                v = bilinear_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy);
            } else { // CUBIC
                if (Xu && Yu) {
                    v = bicubic_uniform_at<ZT,OUT>(x,y,Z,nx,ny,X0,inv_dX,Y0,inv_dY);
                } else {
                    // MATLAB behavior with non-uniform vectors: use linear
                    v = bilinear_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy);
                }
            }
            ZO[idx] = v;
        }
    }
}

// ===================== MEX entry =====================

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 6)
        mexErrMsgIdAndTxt("interp2d:nrhs","Six inputs: X, Y, V, XI, YI, method");

    // Validate real arrays
    for (int k : {0,1,2,3,4})
        if (mxIsComplex(prhs[k]))
            mexErrMsgIdAndTxt("interp2d:complex","Inputs must be real.");

    const mxArray* Xarr  = prhs[0];
    const mxArray* Yarr  = prhs[1];
    const mxArray* Zarr  = prhs[2];
    const mxArray* XIarr = prhs[3];
    const mxArray* YIarr = prhs[4];

    // Z size: Ny x Nx
    const mwSize ny = mxGetNumberOfElements(Yarr);
    const mwSize nx = mxGetNumberOfElements(Xarr);
    if (mxGetM(Zarr) != ny || mxGetN(Zarr) != nx)
        mexErrMsgIdAndTxt("interp2d:Zsize","V must be Ny-by-Nx (numel(Y) x numel(X)).");

    // XI,YI same size
    if (mxGetM(XIarr) != mxGetM(YIarr) || mxGetN(XIarr) != mxGetN(YIarr))
        mexErrMsgIdAndTxt("interp2d:Qsize","XI and YI must be the same size.");

    // Method
    Method method = parse_method(prhs[5]);

    // Output class = class(V)
    mxClassID zClass = mxGetClassID(Zarr);
    if (!(zClass == mxDOUBLE_CLASS || zClass == mxSINGLE_CLASS))
        mexErrMsgIdAndTxt("interp2d:classV","V must be single or double.");

    plhs[0] = mxCreateNumericMatrix(mxGetM(XIarr), mxGetN(XIarr), zClass, mxREAL);

    // Dispatch on types of X,Y, XI, YI, and Z.
    mxClassID xClass  = mxGetClassID(Xarr);
    mxClassID yClass  = mxGetClassID(Yarr);
    mxClassID xiClass = mxGetClassID(XIarr);
    mxClassID yiClass = mxGetClassID(YIarr);

    // We support X/XI in {single,double} and Y/YI in {single,double}.
    // For simplicity (and speed), require X and XI have the same class, and Y and YI have the same class.
    // (This is how MATLAB typically uses them; if you want cross-mixing, we can add conversions.)
    if (xClass != xiClass || yClass != yiClass)
        mexErrMsgIdAndTxt("interp2d:classMismatch","X and XI must have same class; Y and YI must have same class (single or double).");

    // Route to the correct instantiation:
    if (zClass == mxDOUBLE_CLASS) {
        if (xClass == mxDOUBLE_CLASS && yClass == mxDOUBLE_CLASS) {
            interp2d_mixed_core<double,double,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        } else if (xClass == mxSINGLE_CLASS && yClass == mxSINGLE_CLASS) {
            interp2d_mixed_core<float,float,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        } else if (xClass == mxDOUBLE_CLASS && yClass == mxSINGLE_CLASS) {
            interp2d_mixed_core<double,float,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        } else { // x single, y double
            interp2d_mixed_core<float,double,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        }
    } else { // V is single -> output single
        if (xClass == mxDOUBLE_CLASS && yClass == mxDOUBLE_CLASS) {
            interp2d_mixed_core<double,double,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        } else if (xClass == mxSINGLE_CLASS && yClass == mxSINGLE_CLASS) {
            interp2d_mixed_core<float,float,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        } else if (xClass == mxDOUBLE_CLASS && yClass == mxSINGLE_CLASS) {
            interp2d_mixed_core<double,float,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        } else { // x single, y double
            interp2d_mixed_core<float,double,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        }
    }
}

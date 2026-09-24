#include "mex.h"
#include <algorithm>
#include <cmath>
#include <cstring>

#ifdef _OPENMP
  #include <omp.h>
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -fno-exceptions -fno-rtti -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" interp2d_mex_float_wlanczos.cpp
// ===================== Utilities =====================

enum Method : int { NEAREST=0, LINEAR=1, CUBIC=2, LANCZOS2=3, LANCZOS3=4 };

static Method parse_method(const mxArray* m) {
    if (!mxIsChar(m)) mexErrMsgIdAndTxt("interp2d:method","method must be a string");
    char buf[32]; buf[0]='\0';
    mxGetString(m, buf, sizeof(buf));
    if (!std::strcmp(buf,"nearest"))   return NEAREST;
    if (!std::strcmp(buf,"linear"))    return LINEAR;
    if (!std::strcmp(buf,"cubic"))     return CUBIC;
    if (!std::strcmp(buf,"lanczos2"))  return LANCZOS2;
    if (!std::strcmp(buf,"lanczos3"))  return LANCZOS3;
    mexErrMsgIdAndTxt("interp2d:method","Unknown method. Use 'nearest'|'linear'|'cubic'|'lanczos2'|'lanczos3'.");
    return LINEAR;
}

template <typename T> inline bool is_strict_increasing(const T* G, int n){
    for (int i=0;i<n-1;++i) if (!(G[i] < G[i+1])) return false;
    return true;
}

template <typename T>
inline bool uniform_grid(const T* G, int n, double& g0, double& dg, double& inv_dg){
    g0 = static_cast<double>(G[0]);
    dg = (static_cast<double>(G[n-1]) - static_cast<double>(G[0])) / (double)(n-1);
    if (dg == 0.0) return false;
    const double tol = std::abs(dg) * 1e-12;
    for (int k=1;k<n;++k){
        const double pred = g0 + dg*k;
        if (std::abs(static_cast<double>(G[k]) - pred) > tol) return false;
    }
    inv_dg = 1.0 / dg;
    return true;
}

inline int clampi(int v, int lo, int hi){ return v<lo?lo:(v>hi?hi:v); }

// Minimal index advance so that X[ix] <= x <= X[ix+1] (ascending)
template <typename T>
inline int advance_ix(const T* X, int nx, int ix, double x){
    while (ix + 1 < nx && static_cast<double>(X[ix+1]) <= x) ++ix;
    while (ix > 0    && static_cast<double>(X[ix])   >  x) --ix;
    if (ix >= nx-1) ix = nx-2;
    return ix;
}

template <typename T> inline double get_as_double(const T* a, mwSize idx){ return static_cast<double>(a[idx]); }

// ===================== Kernels (shared) =====================

// Bilinear (two lerps)
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

// Nearest
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

// Keys/Catmull–Rom (a=-0.5) — MATLAB 'cubic'
inline double keys_h(double s){
    const double a = -0.5;
    s = std::abs(s);
    if (s <= 1.0) return ((a+2.0)*s - (a+3.0))*s*s + 1.0;
    if (s <  2.0) return ((a*s - 5.0*a)*s + 8.0*a)*s - 4.0*a;
    return 0.0;
}
inline void keys_weights(double t, double w[4]){
    w[0] = keys_h(t + 1.0);
    w[1] = keys_h(t + 0.0);
    w[2] = keys_h(1.0 - t);
    w[3] = keys_h(2.0 - t);
}

// Bicubic on uniform grid with replicated borders
template <typename ZT, typename OUT>
inline OUT bicubic_uniform_at(double x, double y,
                              const ZT* Z, int nx, int ny,
                              double X0, double inv_dX, double Y0, double inv_dY)
{
    const double gx = (x - X0) * inv_dX;
    const double gy = (y - Y0) * inv_dY;

    int ix = (int)std::floor(gx);
    int iy = (int)std::floor(gy);
    double tx = gx - ix; if (tx<0) tx=0; else if (tx>1) tx=1;
    double ty = gy - iy; if (ty<0) ty=0; else if (ty>1) ty=1;

    auto clamp = [](int v, int lo, int hi){ return v<lo?lo:(v>hi?hi:v); };
    int ixm = clamp(ix-1, 0, nx-1), ix0 = clamp(ix,   0, nx-1),
        ix1 = clamp(ix+1, 0, nx-1), ix2 = clamp(ix+2, 0, nx-1);
    int iym = clamp(iy-1, 0, ny-1), iy0 = clamp(iy,   0, ny-1),
        iy1 = clamp(iy+1, 0, ny-1), iy2 = clamp(iy+2, 0, ny-1);

    const ZT* c0 = Z + ixm*ny;
    const ZT* c1 = Z + ix0*ny;
    const ZT* c2 = Z + ix1*ny;
    const ZT* c3 = Z + ix2*ny;

    double wx[4], wy[4];
    keys_weights(tx, wx);
    keys_weights(ty, wy);

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

// ---------- Lanczos (uniform grids) ----------
inline double sinc_pi(double x){ if (x==0.0) return 1.0; const double px = M_PI*x; return std::sin(px)/(px); }
inline double lanczos_lobe(double x, int a){
    x = std::abs(x);
    if (x >= a) return 0.0;
    return sinc_pi(x) * sinc_pi(x / (double)a);
}

// General separable Lanczos-a on uniform grid with replicated borders
template <typename ZT, typename OUT>
inline OUT lanczos_uniform_at(double x, double y,
                              const ZT* Z, int nx, int ny,
                              double X0, double inv_dX, double Y0, double inv_dY,
                              int a)
{
    const int taps = 2*a;

    const double gx = (x - X0) * inv_dX;
    const double gy = (y - Y0) * inv_dY;

    const int ix0 = (int)std::floor(gx) - (a - 1);
    const int iy0 = (int)std::floor(gy) - (a - 1);

    double wx[6]; // supports a up to 3
    double wy[6];

    for (int n=0; n<taps; ++n){
        const double kx = (double)(ix0 + n);
        const double ky = (double)(iy0 + n);
        wx[n] = lanczos_lobe(gx - kx, a);
        wy[n] = lanczos_lobe(gy - ky, a);
    }

    // Normalize weights (helps near edges)
    double sx=0, sy=0;
    for (int n=0;n<taps;++n){ sx += wx[n]; sy += wy[n]; }
    if (sx != 0.0){ for (int n=0;n<taps;++n) wx[n] /= sx; }
    if (sy != 0.0){ for (int n=0;n<taps;++n) wy[n] /= sy; }

    auto clamp = [](int v, int lo, int hi){ return v<lo?lo:(v>hi?hi:v); };

    double gcol[6] = {0,0,0,0,0,0};
    for (int n=0; n<taps; ++n){
        const int ix = clamp(ix0 + n, 0, nx-1);
        const ZT* col = Z + ix*ny;
        double s = 0.0;
        for (int m=0; m<taps; ++m){
            const int iy = clamp(iy0 + m, 0, ny-1);
            s += wy[m]*static_cast<double>(col[iy]);
        }
        gcol[n] = s;
    }
    double out = 0.0;
    for (int n=0; n<taps; ++n) out += wx[n]*gcol[n];

    return static_cast<OUT>(out);
}

// ===================== Core driver (templated) =====================

template <typename GX, typename GY, typename ZT, typename OUT, typename QX, typename QY>
static void interp2d_core(const mxArray* Xarr, const mxArray* Yarr, const mxArray* Zarr,
                          const mxArray* XIarr, const mxArray* YIarr,
                          Method method, mxArray* ZOarr)
{
    const GX* X  = reinterpret_cast<const GX*>(mxGetData(Xarr));
    const GY* Y  = reinterpret_cast<const GY*>(mxGetData(Yarr));
    const ZT* Z  = reinterpret_cast<const ZT*>(mxGetData(Zarr));
    const QX* XI = reinterpret_cast<const QX*>(mxGetData(XIarr));
    const QY* YI = reinterpret_cast<const QY*>(mxGetData(YIarr));

    const int nx = (int)mxGetNumberOfElements(Xarr);
    const int ny = (int)mxGetNumberOfElements(Yarr);
    const mwSize Rq = mxGetM(XIarr);
    const mwSize Cq = mxGetN(XIarr);

    OUT* ZO = reinterpret_cast<OUT*>(mxGetData(ZOarr));

    if (!is_strict_increasing(X, nx) || !is_strict_increasing(Y, ny))
        mexErrMsgIdAndTxt("interp2d:monotonic","X and Y must be strictly increasing.");

    const double xMin = static_cast<double>(X[0]);
    const double xMax = static_cast<double>(X[nx-1]);
    const double yMin = static_cast<double>(Y[0]);
    const double yMax = static_cast<double>(Y[ny-1]);

    // uniform detect (for cubic & lanczos paths)
    double X0=0, dX=0, inv_dX=0, Y0=0, dY=0, inv_dY=0;
    const bool Xu = uniform_grid(X, nx, X0, dX, inv_dX);
    const bool Yu = uniform_grid(Y, ny, Y0, dY, inv_dY);

    // Parallel over columns (contiguous writes)
    #pragma omp parallel for schedule(static)
    for (mwSize c = 0; c < Cq; ++c) {
        // Seed left/bottom cell once from column top
        const mwSize idx0 = 0 + c*Rq;
        double x_top = static_cast<double>(XI[idx0]);
        double y_top = static_cast<double>(YI[idx0]);

        int ix, iy;
        if (Xu) { ix = clampi((int)std::floor((x_top - X0)*inv_dX), 0, nx-2); }
        else { const GX* it = std::upper_bound(X, X+nx, static_cast<GX>(x_top));
               ix = clampi((int)(it - X) - 1, 0, nx-2); }
        if (Yu) { iy = clampi((int)std::floor((y_top - Y0)*inv_dY), 0, ny-2); }
        else { const GY* it = std::upper_bound(Y, Y+ny, static_cast<GY>(y_top));
               iy = clampi((int)(it - Y) - 1, 0, ny-2); }

        for (mwSize r = 0; r < Rq; ++r) {
            const mwSize idx = r + c*Rq;
            const double x = static_cast<double>(XI[idx]);
            const double y = static_cast<double>(YI[idx]);

            if (x < xMin || x > xMax || y < yMin || y > yMax) {
                ZO[idx] = static_cast<OUT>(mxGetNaN());
                continue;
            }

            // Update indices for bilinear/nearest
            ix = Xu ? clampi((int)std::floor((x - X0)*inv_dX), 0, nx-2) : advance_ix(X, nx, ix, x);
            iy = Yu ? clampi((int)std::floor((y - Y0)*inv_dY), 0, ny-2) : advance_ix(Y, ny, iy, y);

            OUT v;
            switch (method) {
                case NEAREST:
                    v = nearest_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy); break;
                case LINEAR:
                    v = bilinear_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy); break;
                case CUBIC:
                    if (Xu && Yu) v = bicubic_uniform_at<ZT,OUT>(x,y,Z,nx,ny,X0,inv_dX,Y0,inv_dY);
                    else          v = bilinear_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy);
                    break;
                case LANCZOS2:
                    if (Xu && Yu) v = lanczos_uniform_at<ZT,OUT>(x,y,Z,nx,ny,X0,inv_dX,Y0,inv_dY,2);
                    else          v = bilinear_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy);
                    break;
                case LANCZOS3:
                    if (Xu && Yu) v = lanczos_uniform_at<ZT,OUT>(x,y,Z,nx,ny,X0,inv_dX,Y0,inv_dY,3);
                    else          v = bilinear_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy);
                    break;
                default:
                    v = bilinear_at<GX,GY,ZT,OUT>(x,y,X,Y,Z,nx,ny,ix,iy); break;
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
    for (int k : {0,1,2,3,4})
        if (mxIsComplex(prhs[k]))
            mexErrMsgIdAndTxt("interp2d:complex","Inputs must be real.");

    const mxArray* Xarr  = prhs[0];
    const mxArray* Yarr  = prhs[1];
    const mxArray* Zarr  = prhs[2];
    const mxArray* XIarr = prhs[3];
    const mxArray* YIarr = prhs[4];
    Method method = parse_method(prhs[5]);

    const mwSize ny = mxGetNumberOfElements(Yarr);
    const mwSize nx = mxGetNumberOfElements(Xarr);
    if (mxGetM(Zarr) != ny || mxGetN(Zarr) != nx)
        mexErrMsgIdAndTxt("interp2d:Zsize","V must be Ny-by-Nx (numel(Y) x numel(X)).");
    if (mxGetM(XIarr) != mxGetM(YIarr) || mxGetN(XIarr) != mxGetN(YIarr))
        mexErrMsgIdAndTxt("interp2d:Qsize","XI and YI must be the same size.");

    mxClassID zC = mxGetClassID(Zarr);
    if (!(zC == mxDOUBLE_CLASS || zC == mxSINGLE_CLASS))
        mexErrMsgIdAndTxt("interp2d:classV","V must be single or double.");

    // Output: same size as XI/YI, class = class(V)
    plhs[0] = mxCreateNumericMatrix(mxGetM(XIarr), mxGetN(XIarr), zC, mxREAL);

    mxClassID xC  = mxGetClassID(Xarr);
    mxClassID yC  = mxGetClassID(Yarr);
    mxClassID xiC = mxGetClassID(XIarr);
    mxClassID yiC = mxGetClassID(YIarr);

    if (zC == mxDOUBLE_CLASS) {
        if (xC==mxDOUBLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<double,double,double,double,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<double,double,double,double,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<double,float,double,double,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<double,float,double,double,float,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<double,float,double,double,double,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<double,float,double,double,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<float,double,double,double,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<float,double,double,double,float,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<float,double,double,double,double,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<float,double,double,double,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<float,float,double,double,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<float,float,double,double,float,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<float,float,double,double,double,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<float,float,double,double,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else
            mexErrMsgIdAndTxt("interp2d:classes","X,Y,XI,YI must be single or double.");
    } else { // output single (V single)
        if (xC==mxDOUBLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<double,double,float,float,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<double,double,float,float,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<double,float,float,float,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<double,float,float,float,float,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<double,float,float,float,double,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<double,float,float,float,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<float,double,float,float,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<float,double,float,float,float,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<float,double,float,float,double,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<float,double,float,float,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<float,float,float,float,double,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS)
            interp2d_core<float,float,float,float,float,double>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<float,float,float,float,double,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS && xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS)
            interp2d_core<float,float,float,float,float,float>(Xarr,Yarr,Zarr,XIarr,YIarr,method,plhs[0]);
        else
            mexErrMsgIdAndTxt("interp2d:classes","X,Y,XI,YI must be single or double.");
    }
}

#include "mex.h"
#include <algorithm>
#include <cmath>

#ifdef _OPENMP
  #include <omp.h>
#endif
// mex -R2018a CXXFLAGS="\$CXXFLAGS -O3 -march=native -ffast-math -fno-exceptions -fno-rtti -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" interp2d_mex_uint32_nearest.cpp
// ========================= Utility =========================

template <typename T>
inline bool strictly_increasing(const T* a, int n){
    for (int i=0; i<n-1; ++i) if (!(a[i] < a[i+1])) return false;
    return true;
}

// detect near-uniform grid (ascending). Returns origin, step, inv_step (double)
template <typename T>
inline bool uniform_grid(const T* G, int n, double& g0, double& dg, double& inv_dg){
    g0 = static_cast<double>(G[0]);
    dg = (static_cast<double>(G[n-1]) - static_cast<double>(G[0])) / (double)(n-1);
    if (dg == 0.0) return false;
    const double tol = std::abs(dg) * 1e-12; // tight for double-accuracy match
    for (int k=1; k<n; ++k){
        double pred = g0 + dg*k;
        if (std::abs(static_cast<double>(G[k]) - pred) > tol) return false;
    }
    inv_dg = 1.0 / dg;
    return true;
}

inline int clampi(int v, int lo, int hi){ return v<lo?lo:(v>hi?hi:v); }

// minimal advance of lower-cell index so that X[ix] <= x <= X[ix+1]
template <typename T>
inline int advance_ix(const T* X, int nx, int ix, double x){
    while (ix + 1 < nx && static_cast<double>(X[ix+1]) <= x) ++ix;
    while (ix > 0 && static_cast<double>(X[ix]) > x) --ix;
    if (ix >= nx-1) ix = nx-2;
    return ix;
}

// read helpers for query arrays
template <typename T> inline double qget(const T* a, mwSize idx){ return static_cast<double>(a[idx]); }

// ========================= Core (templated) =========================

template <typename GX, typename GY, typename QX, typename QY>
static void nn_u32_core(const mxArray* Xarr, const mxArray* Yarr, const mxArray* Zarr,
                        const mxArray* XIarr, const mxArray* YIarr, mxArray* ZOarr)
{
    const GX* X  = reinterpret_cast<const GX*>(mxGetData(Xarr));
    const GY* Y  = reinterpret_cast<const GY*>(mxGetData(Yarr));
    const QX* XI = reinterpret_cast<const QX*>(mxGetData(XIarr));
    const QY* YI = reinterpret_cast<const QY*>(mxGetData(YIarr));
    const uint32_T* Z = reinterpret_cast<const uint32_T*>(mxGetData(Zarr));

    const int nx = (int)mxGetNumberOfElements(Xarr);
    const int ny = (int)mxGetNumberOfElements(Yarr);
    const mwSize Rq = mxGetM(XIarr);
    const mwSize Cq = mxGetN(XIarr);

    uint32_T* ZO = reinterpret_cast<uint32_T*>(mxGetData(ZOarr));

    if (!strictly_increasing(X, nx) || !strictly_increasing(Y, ny))
        mexErrMsgIdAndTxt("nn_u32:monotonic","X and Y must be strictly increasing.");

    // bounds
    const double xMin = static_cast<double>(X[0]);
    const double xMax = static_cast<double>(X[nx-1]);
    const double yMin = static_cast<double>(Y[0]);
    const double yMax = static_cast<double>(Y[ny-1]);

    // fill value for out-of-bounds (change if you prefer another sentinel)
    const uint32_T FILL_VALUE = 0;

    // uniform detection
    double X0=0, dX=0, inv_dX=0, Y0=0, dY=0, inv_dY=0;
    const bool Xu = uniform_grid(X, nx, X0, dX, inv_dX);
    const bool Yu = uniform_grid(Y, ny, Y0, dY, inv_dY);

    // Parallelize over columns (contiguous writes per thread)
    #pragma omp parallel for schedule(static)
    for (mwSize c = 0; c < Cq; ++c) {

        // Seed lower indices from the top pixel of this column
        const mwSize idx0 = 0 + c*Rq;
        const double x0 = qget(XI, idx0);
        const double y0 = qget(YI, idx0);

        int ix, iy;
        if (Xu) {
            ix = clampi((int)std::llround((x0 - X0) * inv_dX), 0, nx-1);
        } else {
            const GX* itx = std::upper_bound(X, X+nx, static_cast<GX>(x0));
            int il = clampi((int)(itx - X) - 1, 0, nx-2);
            // nearest candidate index (left or right)
            ix = (std::abs(qget(X, il) - x0) <= std::abs(qget(X, il+1) - x0)) ? il : il+1;
        }

        if (Yu) {
            iy = clampi((int)std::llround((y0 - Y0) * inv_dY), 0, ny-1);
        } else {
            const GY* ity = std::upper_bound(Y, Y+ny, static_cast<GY>(y0));
            int jl = clampi((int)(ity - Y) - 1, 0, ny-2);
            iy = (std::abs(qget(Y, jl) - y0) <= std::abs(qget(Y, jl+1) - y0)) ? jl : jl+1;
        }

        for (mwSize r = 0; r < Rq; ++r) {
            const mwSize idx = r + c*Rq;
            const double x = qget(XI, idx);
            const double y = qget(YI, idx);

            // out of box -> fill
            if (x < xMin || x > xMax || y < yMin || y > yMax) {
                ZO[idx] = FILL_VALUE; continue;
            }

            int ixn, iyn;
            if (Xu) {
                ixn = clampi((int)std::llround((x - X0) * inv_dX), 0, nx-1);
            } else {
                // keep a lower index that tracks x (no full binary search)
                int il = advance_ix(X, nx, std::min(std::max(ix-1,0),nx-2), x); // allow step back by 1 for symmetry
                ixn = (std::abs(qget(X, il) - x) <= std::abs(qget(X, il+1) - x)) ? il : il+1;
                ix = ixn;
            }

            if (Yu) {
                iyn = clampi((int)std::llround((y - Y0) * inv_dY), 0, ny-1);
            } else {
                int jl = advance_ix(Y, ny, std::min(std::max(iy-1,0),ny-2), y);
                iyn = (std::abs(qget(Y, jl) - y) <= std::abs(qget(Y, jl+1) - y)) ? jl : jl+1;
                iy = iyn;
            }

            ZO[idx] = Z[iyn + ixn * ny];
        }
    }
}

// ========================= MEX entry =========================

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 5)
        mexErrMsgIdAndTxt("nn_u32:nrhs","Five inputs: X, Y, V(uint32), XI, YI");

    // real-only
    for (int k : {0,1,2,3,4})
        if (mxIsComplex(prhs[k]))
            mexErrMsgIdAndTxt("nn_u32:complex","Inputs must be real.");

    // V must be uint32 and sized Ny x Nx
    if (mxGetClassID(prhs[2]) != mxUINT32_CLASS)
        mexErrMsgIdAndTxt("nn_u32:Vclass","V must be uint32.");
    const mwSize ny = mxGetNumberOfElements(prhs[1]);
    const mwSize nx = mxGetNumberOfElements(prhs[0]);
    if (mxGetM(prhs[2]) != ny || mxGetN(prhs[2]) != nx)
        mexErrMsgIdAndTxt("nn_u32:Zsize","V must be Ny-by-Nx (numel(Y) x numel(X)).");

    // XI, YI same size
    if (mxGetM(prhs[3]) != mxGetM(prhs[4]) || mxGetN(prhs[3]) != mxGetN(prhs[4]))
        mexErrMsgIdAndTxt("nn_u32:Qsize","XI and YI must have the same size.");

    // Output (uint32), same size as XI/YI
    plhs[0] = mxCreateNumericMatrix(mxGetM(prhs[3]), mxGetN(prhs[3]), mxUINT32_CLASS, mxREAL);

    // Dispatch on types of X,Y,XI,YI (single/double combos)
    mxClassID xC = mxGetClassID(prhs[0]);
    mxClassID yC = mxGetClassID(prhs[1]);
    mxClassID xiC= mxGetClassID(prhs[3]);
    mxClassID yiC= mxGetClassID(prhs[4]);

    // helper lambdas to route
    auto run_dd_dd = [&](){
        nn_u32_core<double,double,double,double>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]);
    };
    auto run_dd_sd = [&](){ nn_u32_core<double,double,float,double>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_dd_ds = [&](){ nn_u32_core<double,double,double,float>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_dd_ss = [&](){ nn_u32_core<double,double,float,float>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };

    auto run_sd_dd = [&](){ nn_u32_core<float,double,double,double>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_sd_sd = [&](){ nn_u32_core<float,double,float,double>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_sd_ds = [&](){ nn_u32_core<float,double,double,float>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_sd_ss = [&](){ nn_u32_core<float,double,float,float>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };

    auto run_ds_dd = [&](){ nn_u32_core<double,float,double,double>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_ds_sd = [&](){ nn_u32_core<double,float,float,double>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_ds_ds = [&](){ nn_u32_core<double,float,double,float>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_ds_ss = [&](){ nn_u32_core<double,float,float,float>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };

    auto run_ss_dd = [&](){ nn_u32_core<float,float,double,double>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_ss_sd = [&](){ nn_u32_core<float,float,float,double>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_ss_ds = [&](){ nn_u32_core<float,float,double,float>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };
    auto run_ss_ss = [&](){ nn_u32_core<float,float,float,float>(prhs[0],prhs[1],prhs[2],prhs[3],prhs[4],plhs[0]); };

    // Route by all combinations
    if (xC==mxDOUBLE_CLASS && yC==mxDOUBLE_CLASS) {
        if (xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS) run_dd_dd();
        else if (xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS) run_dd_sd();
        else if (xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS) run_dd_ds();
        else if (xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS) run_dd_ss();
        else mexErrMsgIdAndTxt("nn_u32:XIYI","XI,YI must be single or double.");
    } else if (xC==mxSINGLE_CLASS && yC==mxDOUBLE_CLASS) {
        if (xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS) run_sd_dd();
        else if (xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS) run_sd_sd();
        else if (xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS) run_sd_ds();
        else if (xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS) run_sd_ss();
        else mexErrMsgIdAndTxt("nn_u32:XIYI","XI,YI must be single or double.");
    } else if (xC==mxDOUBLE_CLASS && yC==mxSINGLE_CLASS) {
        if (xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS) run_ds_dd();
        else if (xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS) run_ds_sd();
        else if (xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS) run_ds_ds();
        else if (xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS) run_ds_ss();
        else mexErrMsgIdAndTxt("nn_u32:XIYI","XI,YI must be single or double.");
    } else if (xC==mxSINGLE_CLASS && yC==mxSINGLE_CLASS) {
        if (xiC==mxDOUBLE_CLASS && yiC==mxDOUBLE_CLASS) run_ss_dd();
        else if (xiC==mxSINGLE_CLASS && yiC==mxDOUBLE_CLASS) run_ss_sd();
        else if (xiC==mxDOUBLE_CLASS && yiC==mxSINGLE_CLASS) run_ss_ds();
        else if (xiC==mxSINGLE_CLASS && yiC==mxSINGLE_CLASS) run_ss_ss();
        else mexErrMsgIdAndTxt("nn_u32:XIYI","XI,YI must be single or double.");
    } else {
        mexErrMsgIdAndTxt("nn_u32:XYclass","X and Y must be single or double.");
    }
}

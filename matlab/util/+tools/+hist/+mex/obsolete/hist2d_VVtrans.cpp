// hist2d_VVtrans_match.cpp
// [H2, VecX, VecY] = hist2d_VVtrans_match(Xcat, Ycat, Xref, Yref, FlipX, FlipY, RangeX, StepX, RangeY, StepY)
//
// Matches your MATLAB result directly:
//     H2 == histcounts2(Dx(:), Dy(:), EdgesX, EdgesY)
// with Dx = Xcat - FlipX.*Xref.' , Dy = Ycat - FlipY.*Yref.'
// EdgesX = RangeX(1):StepX:RangeX(2)   (X = columns)
// EdgesY = RangeY(1):StepY:RangeY(2)   (Y = rows)
//
// Layout HERE: rows = X bins (Dx), cols = Y bins (Dy)  -> linear idx = bx + Nx*by
// (This is the transpose of MATLAB’s documented layout, but matches your observed output.)
// Bins are half-open; last bin inclusive.
// Inputs: real, full, single/double column vectors. Output H2 is double.
//
// Build (OpenMP):
//   clear mex; clear hist2d_VVtrans_match
//   mex -O CXXFLAGS="-std=c++17 -O3 -march=native -fopenmp" LDFLAGS="$LDFLAGS -fopenmp" hist2d_VVtrans_match.cpp
// Build (no OpenMP):
//   mex -O CXXFLAGS="-std=c++17 -O3 -march=native" hist2d_VVtrans_match.cpp

#include "mex.h"
#include <cmath>
#include <cstdint>
#include <vector>
#include <algorithm>
#include <limits>

#if defined(_OPENMP)
  #include <omp.h>
#endif

// ---- helpers ----
static inline void mustRealVec(const mxArray* A, const char* n){
    if ((!mxIsSingle(A) && !mxIsDouble(A)) || mxIsComplex(A) || mxIsSparse(A))
        mexErrMsgIdAndTxt("hist2d_VVtrans_match:Type","%s must be real single/double (full).", n);
    if (mxGetNumberOfElements(A)==0)
        mexErrMsgIdAndTxt("hist2d_VVtrans_match:Empty","%s is empty.", n);
}
static inline double getScalarDouble(const mxArray* A, const char* n){
    if (mxGetNumberOfElements(A)!=1 || (!mxIsSingle(A) && !mxIsDouble(A)))
        mexErrMsgIdAndTxt("hist2d_VVtrans_match:Scalar","%s must be real scalar.", n);
    return mxIsDouble(A) ? *(const double*)mxGetData(A) : (double)(*(const float*)mxGetData(A));
}
static inline void getRangeStep(const mxArray* R, const mxArray* S, const char* rn, const char* sn,
                                double& r1, double& r2, double& step, mwSize& nbin)
{
    if (!((mxIsSingle(R)||mxIsDouble(R)) && mxGetNumberOfElements(R)==2))
        mexErrMsgIdAndTxt("hist2d_VVtrans_match:Range","%s must be 2-element real vector.", rn);
    if (!mxIsSingle(S) && !mxIsDouble(S))
        mexErrMsgIdAndTxt("hist2d_VVtrans_match:Step","%s must be real scalar.", sn);

    if (mxIsDouble(R)) { const double* p=(const double*)mxGetData(R); r1=p[0]; r2=p[1]; }
    else               { const float*  p=(const float*) mxGetData(R); r1=p[0]; r2=p[1]; }
    step = mxIsDouble(S) ? *(const double*)mxGetData(S) : (double)(*(const float*)mxGetData(S));

    if (!(std::isfinite(r1)&&std::isfinite(r2)&&std::isfinite(step)) || step<=0.0 || r2<=r1)
        mexErrMsgIdAndTxt("hist2d_VVtrans_match:RangeStep","Invalid %s/%s.", rn, sn);

    const double nraw = std::floor((r2 - r1)/step); // nbin bins, edges k=0..nbin
    if (nraw < 1.0) mexErrMsgIdAndTxt("hist2d_VVtrans_match:Bins","Range too small.");
    nbin = (mwSize)nraw;
}

// MATLAB bin policy: last bin inclusive
template <typename T>
static inline int bin_idx_inclusive(T v, T invS, T off, mwSize nbin, T lastEdge){
    if (!std::isfinite(v)) return -1;
    long b = (long)std::floor((double)(v*invS + off)); // off = -R1*invS
    if (b >= 0 && b < (long)nbin) return (int)b;
    const T tol = (T)(8.0 * std::numeric_limits<double>::epsilon()) *
                  (T)std::max<T>(1, std::abs((double)lastEdge));
    if (v <= lastEdge + tol && v >= lastEdge - tol) return (int)nbin - 1;
    return -1;
}

// ---- core (rows=X, cols=Y) ----
template <typename T>
static void run_core(const T* __restrict__ xc, const T* __restrict__ yc, size_t Ncat,
                     const T* __restrict__ xrS, const T* __restrict__ yrS, size_t Nref,
                     T invSx, T offX, mwSize Nx, T lastEdgeX,
                     T invSy, T offY, mwSize Ny, T lastEdgeY,
                     double* __restrict__ H2)   // idx = bx + Nx*by
{
    const size_t gridSize = (size_t)Nx * (size_t)Ny;

    int nT = 1;
#if defined(_OPENMP)
    nT = omp_get_max_threads();
#endif
    const size_t bytesPer = gridSize * sizeof(uint32_t);
    const size_t cap = 256ull * 1024ull * 1024ull;
    const bool useLocal = (nT>1) && (bytesPer*(size_t)nT <= cap) && gridSize>0;

    if (useLocal){
        std::vector<uint32_t*> loc(nT,nullptr);
        for(int t=0;t<nT;++t){
            loc[t]=(uint32_t*)mxCalloc(gridSize, sizeof(uint32_t));
            if(!loc[t]) mexErrMsgIdAndTxt("hist2d_VVtrans_match:Alloc","Local histogram alloc failed.");
        }

        #if defined(_OPENMP)
        #pragma omp parallel for schedule(static, 1<<14)
        #endif
        for (ptrdiff_t ii=0; ii<(ptrdiff_t)Ncat; ++ii){
            int tid=0;
            #if defined(_OPENMP)
            tid = omp_get_thread_num();
            #endif
            uint32_t* __restrict__ L = loc[tid];
            const T xi = xc[ii], yi = yc[ii];

            for (size_t j=0; j<Nref; ++j){
                const T dx = xi - xrS[j];
                const T dy = yi - yrS[j];
                const int bx = bin_idx_inclusive(dx, invSx, offX, Nx, lastEdgeX);
                if (bx < 0) continue;
                const int by = bin_idx_inclusive(dy, invSy, offY, Ny, lastEdgeY);
                if (by < 0) continue;
                L[(size_t)bx + (size_t)Nx*(size_t)by] += 1u; // rows=X, cols=Y
            }
        }
        for(int t=0;t<nT;++t){
            uint32_t* L = loc[t];
            for(size_t k=0;k<gridSize;++k) H2[k] += (double)L[k];
            mxFree(L);
        }
    }else{
        #if defined(_OPENMP)
        #pragma omp parallel for schedule(static, 1<<14)
        #endif
        for (ptrdiff_t ii=0; ii<(ptrdiff_t)Ncat; ++ii){
            const T xi = xc[ii], yi = yc[ii];
            for (size_t j=0; j<Nref; ++j){
                const T dx = xi - xrS[j];
                const T dy = yi - yrS[j];
                const int bx = bin_idx_inclusive(dx, invSx, offX, Nx, lastEdgeX);
                if (bx < 0) continue;
                const int by = bin_idx_inclusive(dy, invSy, offY, Ny, lastEdgeY);
                if (by < 0) continue;
                const size_t idx = (size_t)bx + (size_t)Nx*(size_t)by; // rows=X, cols=Y
                #if defined(_OPENMP)
                #pragma omp atomic
                #endif
                H2[idx] += 1.0;
            }
        }
    }
}

// ---- MEX entry ----
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 10)
        mexErrMsgIdAndTxt("hist2d_VVtrans_match:Args",
            "Usage: [H2, VecX, VecY]=hist2d_VVtrans_match(Xcat, Ycat, Xref, Yref, FlipX, FlipY, RangeX, StepX, RangeY, StepY)");

    const mxArray *XcatA=prhs[0], *YcatA=prhs[1], *XrefA=prhs[2], *YrefA=prhs[3];
    mustRealVec(XcatA,"Xcat"); mustRealVec(YcatA,"Ycat");
    mustRealVec(XrefA,"Xref"); mustRealVec(YrefA,"Yref");

    const size_t Ncat = (size_t)mxGetNumberOfElements(XcatA);
    const size_t Nref = (size_t)mxGetNumberOfElements(XrefA);
    if (mxGetNumberOfElements(YcatA)!=Ncat) mexErrMsgIdAndTxt("hist2d_VVtrans_match:Size","Ycat size mismatch.");
    if (mxGetNumberOfElements(YrefA)!=Nref) mexErrMsgIdAndTxt("hist2d_VVtrans_match:Size","Yref size mismatch.");

    const bool useDouble = mxIsDouble(XcatA) || mxIsDouble(YcatA) || mxIsDouble(XrefA) || mxIsDouble(YrefA);

    const double FlipX = getScalarDouble(prhs[4],"FlipX");
    const double FlipY = getScalarDouble(prhs[5],"FlipY");

    double Rx1,Rx2,Sx; mwSize Nx; getRangeStep(prhs[6], prhs[7], "RangeX","StepX", Rx1,Rx2,Sx, Nx);
    double Ry1,Ry2,Sy; mwSize Ny; getRangeStep(prhs[8], prhs[9], "RangeY","StepY", Ry1,Ry2,Sy, Ny);

    // Output: rows = X bins (Nx), cols = Y bins (Ny)
    mwSize dims[2] = { Nx, Ny };
    plhs[0] = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);
    double* H2 = (double*)mxGetData(plhs[0]);
    std::fill(H2, H2 + (size_t)Nx*(size_t)Ny, 0.0);

    // Bin centers
    plhs[1] = mxCreateDoubleMatrix(Nx,1,mxREAL); // centers for Dx (X)
    double* VecX = (double*)mxGetData(plhs[1]);
    for (mwSize j=0;j<Nx;++j) VecX[j] = Rx1 + ((double)j + 0.5) * Sx;

    plhs[2] = mxCreateDoubleMatrix(Ny,1,mxREAL); // centers for Dy (Y)
    double* VecY = (double*)mxGetData(plhs[2]);
    for (mwSize i=0;i<Ny;++i) VecY[i] = Ry1 + ((double)i + 0.5) * Sy;

    // Precompute reciprocals/offsets and last edges
    const double invSx = 1.0/Sx, offX = -Rx1*invSx, lastEdgeX = Rx1 + (double)Nx*Sx;
    const double invSy = 1.0/Sy, offY = -Ry1*invSy, lastEdgeY = Ry1 + (double)Ny*Sy;

    if (useDouble){
        const double *xc=(const double*)mxGetData(XcatA);
        const double *yc=(const double*)mxGetData(YcatA);
        const double *xr=(const double*)mxGetData(XrefA);
        const double *yr=(const double*)mxGetData(YrefA);

        std::vector<double> xrS(Nref), yrS(Nref);
        for (size_t j=0;j<Nref;++j){ xrS[j]=FlipX*xr[j]; yrS[j]=FlipY*yr[j]; }

        run_core<double>(xc,yc,Ncat,
                         xrS.data(),yrS.data(),Nref,
                         (double)invSx,(double)offX,Nx,(double)lastEdgeX,
                         (double)invSy,(double)offY,Ny,(double)lastEdgeY,
                         H2);
    }else{
        const float *xc=(const float*)mxGetData(XcatA);
        const float *yc=(const float*)mxGetData(YcatA);
        const float *xr=(const float*)mxGetData(XrefA);
        const float *yr=(const float*)mxGetData(YrefA);

        const float FlipXf=(float)FlipX, FlipYf=(float)FlipY;
        std::vector<float> xrS(Nref), yrS(Nref);
        for (size_t j=0;j<Nref;++j){ xrS[j]=FlipXf*xr[j]; yrS[j]=FlipYf*yr[j]; }

        run_core<float>(xc,yc,Ncat,
                        xrS.data(),yrS.data(),Nref,
                        (float)invSx,(float)offX,Nx,(float)lastEdgeX,
                        (float)invSy,(float)offY,Ny,(float)lastEdgeY,
                        H2);
    }
}

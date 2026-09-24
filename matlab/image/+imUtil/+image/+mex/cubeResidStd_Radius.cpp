#include "mex.h"
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

// Optional restrict macro for better optimization
#if defined(__GNUC__) || defined(__clang__)
    #define RESTRICT __restrict__
#elif defined(_MSC_VER)
    #define RESTRICT __restrict
#else
    #define RESTRICT
#endif

// -------------------------------------------------------------------------
// Core templated kernel: T is float (single) or double.
// Inputs:
//   VecXrel: N
//   VecYrel: N
//   DX, DY : M
//   Resid  : N x N x M
//   Std1x1xM: 1 x 1 x M  (we treat it as vector length M)
// Outputs:
//   Flag       : logical N x N x M
//   ResidStdOut: same size as Resid, type T
//
// Computes (equivalent to MATLAB):
//   MatX   = permute(VecXrel - DX(:), [3 2 1]);  % 1 x N x M
//   MatY   = permute(VecYrel - DY(:), [2 3 1]);  % N x 1 x M
//   MatR2  = MatX.^2 + MatY.^2;                  % N x N x M
//   Flag   = MatR2 < FitRadius2;
//   ResidStd = Flag .* Resid ./ Std;            % Std: 1 x 1 x M
// -------------------------------------------------------------------------
template <typename T>
void core_kernel(const T* RESTRICT VecXrel,
                 const T* RESTRICT VecYrel,
                 const T* RESTRICT DX,
                 const T* RESTRICT DY,
                 const T* RESTRICT Resid,
                 const T* RESTRICT Std1x1xM,
                 mxLogical* RESTRICT Flag,
                 T* RESTRICT ResidStdOut,
                 mwSize N,
                 mwSize M,
                 T FitRadius2)
{
    const mwSize N2 = N * N;

    // Parallelize over k (third dim)
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for (mwIndex k = 0; k < M; ++k)
    {
        const T dx = DX[k];
        const T dy = DY[k];
        const T s  = Std1x1xM[k];    // Std(1,1,k)
        const mwSize base_k = static_cast<mwSize>(k) * N2;

        for (mwIndex j = 0; j < N; ++j)
        {
            const T vx = VecXrel[j];
            const mwSize base = base_k + static_cast<mwSize>(j) * N;

            for (mwIndex i = 0; i < N; ++i)
            {
                const T vy = VecYrel[i];

                const T dxr = vx - dx;
                const T dyr = vy - dy;
                const T r2  = dxr * dxr + dyr * dyr;

                const mwSize idx = base + static_cast<mwSize>(i);

                const bool inside = (r2 < FitRadius2);
                Flag[idx] = inside ? 1 : 0;

                if (inside)
                {
                    // ResidStd = Resid ./ Std where Flag is true, 0 otherwise
                    ResidStdOut[idx] = Resid[idx] / s;
                }
                else
                {
                    ResidStdOut[idx] = static_cast<T>(0);
                }
            }
        }
    }
}

// -------------------------------------------------------------------------
// MEX gateway
// -------------------------------------------------------------------------
void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{
    // 1. Check inputs/outputs
    if (nrhs != 7)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:nrhs",
                          "Seven inputs required: VecXrel, VecYrel, DX, DY, Resid, Std, FitRadius2.");

    if (nlhs != 2)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:nlhs",
                          "Two outputs required: Flag, ResidStd.");

    const mxArray *mxVecXrel = prhs[0];
    const mxArray *mxVecYrel = prhs[1];
    const mxArray *mxDX      = prhs[2];
    const mxArray *mxDY      = prhs[3];
    const mxArray *mxResid   = prhs[4];
    const mxArray *mxStd     = prhs[5];
    const mxArray *mxFitR2   = prhs[6];

    // Type determined by VecXrel
    mxClassID cls = mxGetClassID(mxVecXrel);
    if (cls != mxDOUBLE_CLASS && cls != mxSINGLE_CLASS)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:Type",
                          "VecXrel must be single or double.");

    // VecXrel
    if (mxIsComplex(mxVecXrel))
        mexErrMsgIdAndTxt("cubeResidStd_Radius:VecXrel",
                          "VecXrel must be real.");
    const mwSize N = mxGetNumberOfElements(mxVecXrel);

    // VecYrel
    if (mxGetClassID(mxVecYrel) != cls || mxIsComplex(mxVecYrel))
        mexErrMsgIdAndTxt("cubeResidStd_Radius:VecYrel",
                          "VecYrel must be real and the same class as VecXrel.");
    if (mxGetNumberOfElements(mxVecYrel) != N)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:VecSizeMismatch",
                          "VecXrel and VecYrel must have the same number of elements.");

    // DX
    if (mxGetClassID(mxDX) != cls || mxIsComplex(mxDX))
        mexErrMsgIdAndTxt("cubeResidStd_Radius:DX",
                          "DX must be real and the same class as VecXrel.");
    const mwSize M = mxGetNumberOfElements(mxDX);

    // DY
    if (mxGetClassID(mxDY) != cls || mxIsComplex(mxDY))
        mexErrMsgIdAndTxt("cubeResidStd_Radius:DY",
                          "DY must be real and the same class as VecXrel.");
    if (mxGetNumberOfElements(mxDY) != M)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:DSizeMismatch",
                          "DX and DY must have the same number of elements.");

    // Resid: N x N x M
    if (mxGetClassID(mxResid) != cls || mxIsComplex(mxResid))
        mexErrMsgIdAndTxt("cubeResidStd_Radius:Resid",
                          "Resid must be real and the same class as VecXrel.");
    const mwSize *dimsResid = mxGetDimensions(mxResid);
    const mwSize ndimsResid = mxGetNumberOfDimensions(mxResid);
    if (ndimsResid != 3)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:ResidDims",
                          "Resid must be a 3-D array of size [N, N, M].");
    if (dimsResid[0] != N || dimsResid[1] != N || dimsResid[2] != M)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:ResidSize",
                          "Resid must be of size [numel(VecXrel), numel(VecXrel), numel(DX)].");

    // Std: 1 x 1 x M
    if (mxGetClassID(mxStd) != cls || mxIsComplex(mxStd))
        mexErrMsgIdAndTxt("cubeResidStd_Radius:Std",
                          "Std must be real and the same class as VecXrel.");
    const mwSize *dimsStd = mxGetDimensions(mxStd);
    const mwSize ndimsStd = mxGetNumberOfDimensions(mxStd);
    if (ndimsStd != 3 || dimsStd[0] != 1 || dimsStd[1] != 1 || dimsStd[2] != M)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:StdSize",
                          "Std must be of size [1, 1, numel(DX)].");

    // FitRadius2: scalar
    if (mxIsComplex(mxFitR2) || mxGetNumberOfElements(mxFitR2) != 1)
        mexErrMsgIdAndTxt("cubeResidStd_Radius:FitRadius2",
                          "FitRadius2 must be a real scalar.");

    double FitRadius2_double = mxGetScalar(mxFitR2);

    // 2. Create outputs: Flag (logical), ResidStd (same class as inputs)
    plhs[0] = mxCreateLogicalArray(ndimsResid, dimsResid);
    plhs[1] = mxCreateNumericArray(ndimsResid, dimsResid, cls, mxREAL);

    mxLogical *Flag = mxGetLogicals(plhs[0]);

    void *VecXrelData   = mxGetData(mxVecXrel);
    void *VecYrelData   = mxGetData(mxVecYrel);
    void *DXData        = mxGetData(mxDX);
    void *DYData        = mxGetData(mxDY);
    void *ResidData     = mxGetData(mxResid);
    void *StdData       = mxGetData(mxStd);
    void *ResidStdData  = mxGetData(plhs[1]);

    // 3. Dispatch to single / double core
    if (cls == mxDOUBLE_CLASS)
    {
        double *VecXrel      = static_cast<double*>(VecXrelData);
        double *VecYrel      = static_cast<double*>(VecYrelData);
        double *DX           = static_cast<double*>(DXData);
        double *DY           = static_cast<double*>(DYData);
        double *Resid        = static_cast<double*>(ResidData);
        double *Std1x1xM     = static_cast<double*>(StdData);
        double *ResidStdOut  = static_cast<double*>(ResidStdData);

        const double FitRadius2 = static_cast<double>(FitRadius2_double);

        core_kernel<double>(VecXrel, VecYrel,
                            DX, DY,
                            Resid, Std1x1xM,
                            Flag, ResidStdOut,
                            N, M,
                            FitRadius2);
    }
    else // mxSINGLE_CLASS
    {
        float *VecXrel       = static_cast<float*>(VecXrelData);
        float *VecYrel       = static_cast<float*>(VecYrelData);
        float *DX            = static_cast<float*>(DXData);
        float *DY            = static_cast<float*>(DYData);
        float *Resid         = static_cast<float*>(ResidData);
        float *Std1x1xM      = static_cast<float*>(StdData);
        float *ResidStdOut   = static_cast<float*>(ResidStdData);

        const float FitRadius2 = static_cast<float>(FitRadius2_double);

        core_kernel<float>(VecXrel, VecYrel,
                           DX, DY,
                           Resid, Std1x1xM,
                           Flag, ResidStdOut,
                           N, M,
                           FitRadius2);
    }
}

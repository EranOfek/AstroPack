#include "mex.h"
#include <cmath>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <omp.h>

void mfind_bin(const double* X, mwSize N, const double* Vals, mwSize Nvals, std::vector<uint32_t>& Im, bool useOpenMP) {
    std::vector<uint32_t> I1(Nvals, 1);
    std::vector<uint32_t> I2(Nvals, N);
    Im.assign(Nvals, static_cast<uint32_t>(std::floor(0.5 * (1 + N))));
    std::vector<uint32_t> PrevIm(Nvals, 0);
    
    const double halfN = 0.5 * (1 + N);
    bool converged = false;

    while (!converged) {
        converged = true;

        #pragma omp parallel for if(useOpenMP) reduction(&& : converged)
        for (mwSize i = 0; i < Nvals; ++i) {
            if (Vals[i] > X[Im[i] - 1]) {
                I1[i] = Im[i];
            } else {
                I2[i] = Im[i];
            }
            uint32_t newIm = static_cast<uint32_t>(std::floor(0.5 * (I1[i] + I2[i])));
            if (newIm != Im[i]) {
                Im[i] = newIm;
                converged = false;
            }
        }
    }
}

void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    if (nrhs != 2) {
        mexErrMsgIdAndTxt("MATLAB:mex_mfind_bin_double1:invalidNumInputs", "Two inputs required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:mex_mfind_bin_double1:invalidNumOutputs", "One output required.");
    }

    mwSize N = mxGetNumberOfElements(prhs[0]);
    mwSize Nvals = mxGetNumberOfElements(prhs[1]);

    const double* X = static_cast<const double*>(mxGetData(prhs[0]));
    const double* Vals = static_cast<const double*>(mxGetData(prhs[1]));

    plhs[0] = mxCreateNumericMatrix(1, Nvals, mxUINT32_CLASS, mxREAL);
    uint32_t* Im = static_cast<uint32_t*>(mxGetData(plhs[0]));

    std::vector<uint32_t> ImVec(Nvals);

    mfind_bin(X, N, Vals, Nvals, ImVec, true);

    std::copy(ImVec.begin(), ImVec.end(), Im);
}

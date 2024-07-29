//
// mex_mfind_bin_include.cpp
//
// Author: Dan Elhanati, July 2024
//
//
// Flags for cmex.py:
//
// 		$dtype: single, double
//

#include "mex.h"
#include <vector>
#include <cmath>
#include <algorithm>
#include <omp.h> // Include OpenMP header

// Function to perform the binary search
void mfind_bin(const __Type* X, mwSize N, const __Type* Vals, mwSize Nvals, std::vector<uint32_t>& Im, bool useOpenMP) {
    std::vector<uint32_t> I1(Nvals, 1);
    std::vector<uint32_t> I2(Nvals, N);
    Im.assign(Nvals, static_cast<uint32_t>(std::floor(0.5 * (1 + N))));
    std::vector<uint32_t> PrevIm(Nvals, 0);

    if (N < 2) {
        if (N == 0) {
            Im.clear();
        } else {
            Im.assign(Nvals, 1);
        }
        return;
    }

    bool converged = false;
    while (!converged) {
        converged = true;

        if (useOpenMP) {
            #pragma omp parallel for
            for (mwSize i = 0; i < Nvals; ++i) {
                if (Im[i] != PrevIm[i]) {
                    converged = false;
                }
            }

            #pragma omp parallel for
            for (mwSize i = 0; i < Nvals; ++i) {
                if (Vals[i] > X[Im[i] - 1]) {
                    I1[i] = Im[i];
                } else {
                    I2[i] = Im[i];
                }
            }
        } else {
            for (mwSize i = 0; i < Nvals; ++i) {
                if (Im[i] != PrevIm[i]) {
                    converged = false;
                }
            }

            for (mwSize i = 0; i < Nvals; ++i) {
                if (Vals[i] > X[Im[i] - 1]) {
                    I1[i] = Im[i];
                } else {
                    I2[i] = Im[i];
                }
            }
        }

        PrevIm = Im;

        if (useOpenMP) {
            #pragma omp parallel for
            for (mwSize i = 0; i < Nvals; ++i) {
                Im[i] = static_cast<uint32_t>(std::floor(0.5 * (I1[i] + I2[i])));
            }
        } else {
            for (mwSize i = 0; i < Nvals; ++i) {
                Im[i] = static_cast<uint32_t>(std::floor(0.5 * (I1[i] + I2[i])));
            }
        }
    }
}
// The MEX gateway function
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]) {
    // Check for the correct number of arguments
    if (nrhs != 3) {
        mexErrMsgIdAndTxt("mfind_bin:nrhs", "Two inputs required.");
    }
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("mfind_bin:nlhs", "One output required.");
    }

    // Get the input arguments
    const __Type* X = (__Type*) mxGetData(prhs[0]);
    const __Type* Vals = (__Type*) mxGetData(prhs[1]);
    bool useOpenMP = *((int*)mxGetData(prhs[2]));

    // Get the dimensions of the input arguments
    mwSize N = mxGetM(prhs[0]);
    mwSize Nvals = mxGetN(prhs[1]);

    // Create the output array
    plhs[0] = mxCreateNumericMatrix(1, Nvals, mxUINT32_CLASS, mxREAL);
    uint32_t* Im = (uint32_t*)mxGetData(plhs[0]);

    // Call the binary search function
    std::vector<uint32_t> ImVec;
    mfind_bin(X, N, Vals, Nvals, ImVec, useOpenMP);

    // Copy the results to the output array
    std::copy(ImVec.begin(), ImVec.end(), Im);
}

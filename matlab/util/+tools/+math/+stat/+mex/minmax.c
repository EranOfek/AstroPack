#include "mex.h"
#include "matrix.h"
#include <float.h>
#include <omp.h>

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 1) {
        mexErrMsgTxt("One input required.");
    }
    
    const mxArray *input = prhs[0];
    mwSize num_elements = mxGetNumberOfElements(input);

    if (mxIsDouble(input)) {
        double *data = mxGetPr(input);
        double global_min = DBL_MAX;
        double global_max = -DBL_MAX;
        mwSize global_min_idx = 0, global_max_idx = 0;

        #pragma omp parallel
        {
            double local_min = DBL_MAX;
            double local_max = -DBL_MAX;
            mwSize local_min_idx = 0, local_max_idx = 0;

            #pragma omp for
            for (mwSize i = 0; i < num_elements; ++i) {
                double val = data[i];
                if (val < local_min) {
                    local_min = val;
                    local_min_idx = i;
                }
                if (val > local_max) {
                    local_max = val;
                    local_max_idx = i;
                }
            }

            #pragma omp critical
            {
                if (local_min < global_min) {
                    global_min = local_min;
                    global_min_idx = local_min_idx;
                }
                if (local_max > global_max) {
                    global_max = local_max;
                    global_max_idx = local_max_idx;
                }
            }
        }

        plhs[0] = mxCreateDoubleScalar(global_min);
        plhs[1] = mxCreateDoubleScalar(global_max);
        plhs[2] = mxCreateDoubleScalar((double)(global_min_idx + 1));  // MATLAB uses 1-based indexing
        plhs[3] = mxCreateDoubleScalar((double)(global_max_idx + 1));

    } else if (mxIsSingle(input)) {
        float *data = (float *)mxGetData(input);
        float global_min = FLT_MAX;
        float global_max = -FLT_MAX;
        mwSize global_min_idx = 0, global_max_idx = 0;

        #pragma omp parallel
        {
            float local_min = FLT_MAX;
            float local_max = -FLT_MAX;
            mwSize local_min_idx = 0, local_max_idx = 0;

            #pragma omp for
            for (mwSize i = 0; i < num_elements; ++i) {
                float val = data[i];
                if (val < local_min) {
                    local_min = val;
                    local_min_idx = i;
                }
                if (val > local_max) {
                    local_max = val;
                    local_max_idx = i;
                }
            }

            #pragma omp critical
            {
                if (local_min < global_min) {
                    global_min = local_min;
                    global_min_idx = local_min_idx;
                }
                if (local_max > global_max) {
                    global_max = local_max;
                    global_max_idx = local_max_idx;
                }
            }
        }

        plhs[0] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
        *(float *)mxGetData(plhs[0]) = global_min;
        plhs[1] = mxCreateNumericMatrix(1, 1, mxSINGLE_CLASS, mxREAL);
        *(float *)mxGetData(plhs[1]) = global_max;
        plhs[2] = mxCreateDoubleScalar((double)(global_min_idx + 1));  // MATLAB uses 1-based indexing
        plhs[3] = mxCreateDoubleScalar((double)(global_max_idx + 1));
    } else {
        mexErrMsgTxt("Input must be of type single or double.");
    }
}

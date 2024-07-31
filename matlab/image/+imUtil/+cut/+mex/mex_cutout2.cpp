#include "mex.h"
#include <cstring>
#include <cstdio>

#define SRC_POS(x,y,p) ((x) + (y) * N + (p) * num_in_elements)
#define DST_POS(c,x,y,p) ((c) + (x) * num_cuts + (y) * num_cuts * cut_size + (p) * num_out_elements)

void msglog(const char* msg, bool is_error = false) {
    if (is_error) {
        mexErrMsgIdAndTxt("mex_cutout1:log", "%s", msg);
    } else {
        mexPrintf("%s\n", msg);
    }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 7) {
        mexErrMsgIdAndTxt("mex_cutout1:invalidNumInputs", "Seven inputs required.");
    }

    // Get input data
    unsigned char *in_array = (unsigned char*) mxGetData(prhs[0]);
    int N = mxGetScalar(prhs[1]);
    int cut_size = mxGetScalar(prhs[2]);
    int num_cuts = mxGetScalar(prhs[3]);
    int *pos_x = (int*) mxGetData(prhs[4]);
    int *pos_y = (int*) mxGetData(prhs[5]);
    unsigned char replace_value = (unsigned char) mxGetScalar(prhs[6]);

    // Initialize output array
    const mwSize dims[] = {static_cast<mwSize>(cut_size), static_cast<mwSize>(cut_size), static_cast<mwSize>(num_cuts)};
    plhs[0] = mxCreateNumericArray(3, dims, mxUINT8_CLASS, mxREAL);
    unsigned char *out_array = (unsigned char*) mxGetData(plhs[0]);

    int num_bytes = mxGetElementSize(prhs[0]);
    int pages = mxGetNumberOfDimensions(prhs[0]) > 2 ? mxGetDimensions(prhs[0])[2] : 1;
    int num_in_elements = mxGetDimensions(prhs[0])[0] * mxGetDimensions(prhs[0])[1];
    int num_out_elements = mxGetNumberOfElements(plhs[0]);

    unsigned char *in_array_end = in_array + num_in_elements * pages * num_bytes;
    unsigned char *out_array_end = out_array + num_out_elements * num_bytes;

    int cut_size_x = cut_size;
    int cut_size_y = cut_size;
    int push_x = 0;
    int push_y = 0;
    int count = 0;
    char s[256];

    for (int c = 0; c < num_cuts; c++) {
        int x1 = pos_x[c] - (cut_size / 2);
        int y1 = pos_y[c] - (cut_size / 2);

        for (int p = 0; p < pages; p++) {
            for (int j = push_x; j < cut_size_x; j++) {
                for (int i = push_y; i < cut_size_y; i++) {
                    unsigned char *a = in_array + SRC_POS(x1 + j, y1 + i, p) * num_bytes;
                    unsigned char *b = out_array + DST_POS(c, j, i, p) * num_bytes;

                    // Validate destination pointer is within range
                    if (b >= out_array && b < out_array_end) {
                        if (a >= in_array && a < in_array_end) {
                            if (count < 10) {
                                sprintf(s, "  %0d: dst ok          : c=%d, p=%d, j=%d, i=%d, N=%d, cut_size_x: %d, cut_size_y: %d, push_x: %d, push_y: %d, ptr: %p", count, c, p, j, i, N, cut_size_x, cut_size_y, push_x, push_y, (void*)b);
                                msglog(s);
                            }
                        } else {
                            sprintf(s, "  %0d: dst OUT OF RANGE: c=%d, p=%d, j=%d, i=%d, N=%d, cut_size_x: %d, cut_size_y: %d, push_x: %d, push_y: %d, ptr: %p", count, c, p, j, i, N, cut_size_x, cut_size_y, push_x, push_y, (void*)b);
                            msglog(s, true);
                        }

                        // Validate both pointers are in range
                        if ((a >= in_array && a < in_array_end) && (b >= out_array && b < out_array_end)) {
                            memcpy(b, a, num_bytes);
                        }
                        count++;
                    }
                }
            }
        }
    }

    msglog("Done copying input matrix");
}

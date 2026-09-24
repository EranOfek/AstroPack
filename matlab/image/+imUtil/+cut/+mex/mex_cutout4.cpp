#include "mex.h"
#include "matrix.h"
#include <math.h>
#include <string.h>

// Calculate position of low corner
inline int low_corner(int center, int size) {
    int half_size = size / 2;
    return center - half_size;
}

// Calculate position of specified cut in input matrix
#define SRC_POS(col, row, page) ((rows * cols * page + col * rows + row) * num_bytes)

// Calculate position of specified cut in output matrix
#define DST_POS(cut_idx, col, row, page) ((cut_idx * cut_size * cut_size * pages + page * cut_size * cut_size + col * cut_size + row) * num_bytes)

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Validate number of arguments
    if (nrhs < 3) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:invalidNumInputs", "USAGE: [cutouts, image_subtracted] = mexCutout(image, positions, cut_size, pad_value=0, replace_value=0, debug_bit=0, use_memcpy=1");
        return;
    }

    // Check argument 1 (image) - must be numeric or boolean
    if (!mxIsNumeric(prhs[0]) && !mxIsLogical(prhs[0])) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 1 to mexCutout is not numeric nor logical!");
    }

    // Empty input matrix just returns an empty matrix
    if (mxIsEmpty(prhs[0])) {
        plhs[0] = mxDuplicateArray(prhs[0]);
        if (nlhs > 1) {
            plhs[1] = mxDuplicateArray(prhs[0]);
        }
        return;
    }

    // Check image matrix side and dims
    if (mxGetN(prhs[0]) < 2 || mxGetM(prhs[0]) < 2 || mxGetNumberOfDimensions(prhs[0]) > 3) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotMatrix", "Must input a (2D or 3D) matrix to mexCutout!");
    }

    // Check argument 2 (positions matrix) - must be doubles and 2D
    if (!mxIsNumeric(prhs[1]) || !mxIsDouble(prhs[1]) || mxGetN(prhs[1]) != 2) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputInvalid", "Second input must be a Nx2 matrix of x and y center positions in double precision.");
    }

    // Check argument 3 (cut_size) - must be numeric
    if (!mxIsNumeric(prhs[2])) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 3 to mexCutout is not numeric...");
    }

    // Get cut_size
    int cut_size = (int)mxGetScalar(prhs[2]);

    // cut_size must be ODD VALUE (@Chen)
    if ((cut_size % 2) != 1) {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputWrongSize", "cut_size (argument 3) must be ODD value...");
        return;
    }

    // Get argument 4 (pad_value) - must be numeric
    double pad_value = 0;
    if (nrhs > 3 && !mxIsEmpty(prhs[3])) {
        if (!mxIsNumeric(prhs[3])) {
            mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 4 to mexCutout is not numeric...");
        }
        pad_value = mxGetScalar(prhs[3]);
    }

    // Check argument 5 (replace_value) - must be numeric
    double replace_value = 0;
    if (nrhs > 4 && !mxIsEmpty(prhs[4])) {
        if (!mxIsNumeric(prhs[4])) {
            mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 5 to mexCutout is not numeric...");
        }
        replace_value = mxGetScalar(prhs[4]);
    }

    // Check argument 6 (debug_bit) - must be numeric
    int debug_bit = 0;
    if (nrhs > 5 && !mxIsEmpty(prhs[5])) {
        if (!mxIsNumeric(prhs[5])) {
            mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 6 to mexCutout is not numeric...");
        }
        debug_bit = (int)mxGetScalar(prhs[5]);
    }

    // Check argument 7 (use_memcpy) - must be numeric (boolean)
    int use_memcpy = 0;
    if (nrhs > 6 && !mxIsEmpty(prhs[6])) {
        if (!mxIsNumeric(prhs[6])) {
            mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputNotNumeric", "Input 7 to mexCutout is not numeric...");
        }
        use_memcpy = (int)mxGetScalar(prhs[6]);
    }

    // Get input image size
    size_t rows = mxGetM(prhs[0]);
    size_t cols = mxGetN(prhs[0]);
    size_t pages = 1;

    // When input image is 3D, set pages=
    if (mxGetNumberOfDimensions(prhs[0]) > 2) {
        const mwSize *dims = mxGetDimensions(prhs[0]);
        rows = dims[0];
        cols = dims[1];
        pages = dims[2];
    }

    // Get num_cuts which is the number of rows in positions[] argument
    size_t num_cuts = mxGetM(prhs[1]);

    // Get the cut positions into an array:
    double *pos_ptr = (double*) mxGetPr(prhs[1]);

    // Allocate pos - the x,y of each cut
    int *pos_x, *pos_y;
    pos_x = (int*) mxCalloc(num_cuts, sizeof(int));
    pos_y = (int*) mxCalloc(num_cuts, sizeof(int));

    // Prepare pos array
    for (int i = 0; i < num_cuts; i++) {
        // Minus one for conversion between Matlab indices and C indices
        pos_x[i] = (int)round(pos_ptr[num_cuts * 0 + i]) - 1;
        pos_y[i] = (int)round(pos_ptr[num_cuts * 1 + i]) - 1;
    }

    // Prepare output matrix
    const size_t out_dims[4] = { (size_t)cut_size, (size_t)cut_size, (size_t)pages, (size_t)num_cuts };

    size_t num_in_elements = (size_t)(rows * cols * pages);
    size_t num_out_elements = (size_t)(cut_size * cut_size * pages * num_cuts);

    unsigned char pad_value_bytes[8] = {0};       // can contain any value type
    unsigned char replace_value_bytes[8] = {0};   // can contain any value type
    int num_bytes = 0;                            // bytes per pixel

    // Input image is uint16
    if (mxIsClass(prhs[0], "uint16")) {
        // Cannot have NaN or negative values in the uint16 matrix
        if (isnan(pad_value) || pad_value < 0) {
            pad_value = 0;
        }

        if (isnan(replace_value) || replace_value < 0) {
            replace_value = 0;
        }

        // Allocate
        num_bytes = sizeof(unsigned short int);
        plhs[0] = mxCreateNumericArray(4, out_dims, mxUINT16_CLASS, mxREAL);

        // Only copy the first 2 bytes into this array...
        unsigned short int pad_value_uint16 = (unsigned short int)pad_value;
        memcpy(&pad_value_bytes, &pad_value_uint16, num_bytes);

        unsigned short int replace_value_uint16 = (unsigned short int)replace_value;
        memcpy(&replace_value_bytes, &replace_value_uint16, num_bytes);
    }
    // Input image is double
    else if (mxIsClass(prhs[0], "double")) {
        // Allocate
        num_bytes = sizeof(double);
        plhs[0] = mxCreateNumericArray(4, out_dims, mxDOUBLE_CLASS, mxREAL);

        memcpy(&pad_value_bytes, &pad_value, num_bytes);
        memcpy(&replace_value_bytes, &replace_value, num_bytes);
    }
    // Input image is single
    else if (mxIsClass(prhs[0], "single")) {
        // Allocate
        num_bytes = sizeof(float);
        plhs[0] = mxCreateNumericArray(4, out_dims, mxSINGLE_CLASS, mxREAL);

        float pad_value_single = (float)pad_value;
        memcpy(&pad_value_bytes, &pad_value_single, num_bytes);

        float replace_value_single = (float)replace_value;
        memcpy(&replace_value_bytes, &replace_value_single, num_bytes);
    }
    // Input image is logical
    else if (mxIsClass(prhs[0], "logical")) {
        // Cannot have NaN or negative values in the logical matrix
        if (isnan(pad_value) || pad_value < 0) {
            pad_value = 0;
        }

        if (isnan(replace_value) || replace_value < 0) {
            replace_value = 0;
        }

        // Allocate
        num_bytes = sizeof(bool);
        plhs[0] = mxCreateLogicalArray(4, out_dims);

        bool pad_value_logical = (bool)pad_value;
        memcpy(&pad_value_bytes, &pad_value_logical, num_bytes);

        bool replace_value_logical = (bool)replace_value;
        memcpy(&replace_value_bytes, &replace_value_logical, num_bytes);
    }
    // Unsupported input type
    else {
        mexErrMsgIdAndTxt("MATLAB:util:img:mexCutout:inputInvalidType", "Input image must be uint16, double, single, or logical!");
    }

    // Image array
    unsigned char *in_array = (unsigned char*)mxGetData(prhs[0]);
    unsigned char *out_array = (unsigned char*)mxGetData(plhs[0]);
    unsigned char *in_array_end = in_array + (num_in_elements * num_bytes);
    unsigned char *out_array_end = out_array + (num_out_elements * num_bytes);

    // Pad the out_array with pad_value (@Chen)
    for (int p = 0; p < pages; p++) {
        for (int j = 0; j < cut_size; j++) {
            for (int i = 0; i < cut_size; i++) {
                for (int c = 0; c < num_cuts; c++) {
                    memcpy(&out_array[DST_POS(c, j, i, p)], &pad_value_bytes, num_bytes);
                }
            }
        }
    }

    // Main processing loop
    unsigned char *src, *dst;
    int push_x, push_y, cut_size_x, cut_size_y;
    for (int c = 0; c < num_cuts; c++) {
        int x1 = low_corner(pos_x[c], cut_size);
        int y1 = low_corner(pos_y[c], cut_size);

        // Handle boundaries and adjust push_x, push_y, cut_size_x, cut_size_y
        push_x = push_y = 0;
        cut_size_x = cut_size;
        cut_size_y = cut_size;

        if (x1 < 0) {
            push_x = -x1;
            x1 = 0;
        }

        if (y1 < 0) {
            push_y = -y1;
            y1 = 0;
        }

        if ((x1 + cut_size_x) > cols) {
            cut_size_x = cols - x1;
        }

        if ((y1 + cut_size_y) > rows) {
            cut_size_y = rows - y1;
        }

        for (int p = 0; p < pages; p++) {
            for (int j = push_x; j < cut_size_x; j++) {
                src = &in_array[SRC_POS(x1, y1, p) + j * rows * num_bytes];
                dst = &out_array[DST_POS(c, j, push_y, p)];
                int N = (cut_size_y - push_y) * num_bytes;

                if (use_memcpy) {
                    memcpy(dst, src, N);
                } else {
                    for (int i = push_y; i < cut_size_y; i++) {
                        unsigned char *a = &src[i * num_bytes];
                        unsigned char *b = &dst[i * num_bytes];
                        if (a >= in_array && a < in_array_end && b >= out_array && b < out_array_end) {
                            memcpy(b, a, num_bytes);
                        }
                    }
                }
            }
        }
    }

    // Replace values if requested
    if (nrhs > 4 && !mxIsEmpty(prhs[4])) {
        unsigned char *in_array_copy = (unsigned char*)mxDuplicateArray(prhs[0]);
        for (int c = 0; c < num_cuts; c++) {
            int x1 = low_corner(pos_x[c], cut_size);
            int y1 = low_corner(pos_y[c], cut_size);
            push_x = push_y = 0;
            cut_size_x = cut_size;
            cut_size_y = cut_size;

            if (x1 < 0) {
                push_x = -x1;
                x1 = 0;
            }

            if (y1 < 0) {
                push_y = -y1;
                y1 = 0;
            }

            if ((x1 + cut_size_x) > cols) {
                cut_size_x = cols - x1;
            }

            if ((y1 + cut_size_y) > rows) {
                cut_size_y = rows - y1;
            }

            for (int p = 0; p < pages; p++) {
                for (int j = push_x; j < cut_size_x; j++) {
                    src = &in_array[SRC_POS(x1, y1, p) + j * rows * num_bytes];
                    int N = (cut_size_y - push_y) * num_bytes;
                    for (int i = push_y; i < cut_size_y; i++) {
                        unsigned char *a = &src[i * num_bytes];
                        if (a >= in_array && a < in_array_end) {
                            memcpy(a, &replace_value_bytes, num_bytes);
                        }
                    }
                }
            }
        }

        if (nlhs > 1) {
            plhs[1] = mxDuplicateArray(prhs[0]);
        } else {
            mxFree(in_array_copy);
        }
    }

    // Free allocated memory
    mxFree(pos_x);
    mxFree(pos_y);
}

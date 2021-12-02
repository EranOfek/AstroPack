//    Use the following command to build this MEX file:
//        mex -R2018a 'CFLAGS=-mavx' tdma.c

#include "mex.h"
#include "matrix.h"
#include "matlab_version.c"
#include "matlab_version.h"
#include "threadpool.c"
#include <math.h>
#include <immintrin.h>

mxArray* tridiag_r(const mxArray* prhs[], mwSize settings[]);

mxArray* tridiag_c(const mxArray* prhs[], mwSize settings[]);

// y = tdma(main, lower, upper, f, direction);
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (TARGET_API_VERSION == R2017b && matlab_version() < 0x2018a) {
        mexErrMsgIdAndTxt("tdma:InvalidCompileOption",
                          "MEX routine must be compiled with -R2018a option!");
    }
    
    if (nrhs != 4 && nrhs != 5) {
        mexErrMsgIdAndTxt("tdma:InvalidInput",
                          "Need four or five inputs!");
    }
    
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("tdma:InvalidOutput",
                          "Only one output allowed!");
    }
    
    mwSize n = mxGetNumberOfElements(prhs[0]);
    mwSize nu = mxGetNumberOfElements(prhs[1]);
    mwSize nl = mxGetNumberOfElements(prhs[2]);
    mwSize nf = mxGetNumberOfElements(prhs[3]);
    
    if (nu != n || nl != n || nf != n) {
        mexErrMsgIdAndTxt("tdma:InvalidInput",
                          "Input matrices must have the same number of elements!");
    }
    
    const int ndims = mxGetNumberOfDimensions(prhs[0]);
    const mwSize* dims = mxGetDimensions(prhs[0]);
    
    int direction;
    mwSize settings[6];
    
    const mwSize ne = mxGetNumberOfElements(prhs[0]);
    const mwSize nr = dims[0];
    const mwSize nc = dims[1];
    
    if (nrhs < 5) {
        direction = 1;
    }
    else {
        direction = (int) *mxGetDoubles(prhs[nrhs-1]);
    }
    
    for (int i = 1; i < 3; i++) {
        const mwSize* dims2 = mxGetDimensions(prhs[i]);
        for (int j = 0; j < ndims; j++) {
            if (dims[j] != dims2[j]) {
                mexErrMsgIdAndTxt("tdma:InvalidInput",
                                  "Input matrices must all have the same dimensions!");
            }
        }
    }
    
    if (ndims == 2 && direction == 3) {
        mexErrMsgIdAndTxt("tdma:InvalidInput",
                          "You want to find solutions down the third dimension, but your input data only has two dimensions.  The calculation will still run, but it will give you garbage data, so here's an error!");
    }
    
    if (ndims == 2 && nr == 1 && direction == 1) {
        mexErrMsgIdAndTxt("tdma:InvalidInput",
                          "You're trying to solve down a row vector when you should be going across.  This will technically run, but will give you garbage data.  Set direction to be 2.");
    }
    
    if (ndims == 2 && nc == 1 && direction == 2) {
        mexErrMsgIdAndTxt("tdma:InvalidInput",
                          "You're trying to solve across a column vector when you should be going down.  This will technically run, but will give you garbage data.  Set direction to be 1.");
    }
    
    if (direction < 1 || direction > 3) {
        mexErrMsgIdAndTxt("tdma:InvalidInput",
                          "Direction has to be either 1, 2, or 3.");
    }
    
    switch(direction) {
        case 1:
            settings[0] = 1; settings[1] = nr; settings[2] = 1;
            settings[3] = 1; settings[4] = ne; settings[5] = nr;
            break;
        case 2:
            settings[0] = nr*nc; settings[1] = 1; settings[2] = nr;
            settings[3] = ne; settings[4] = nr; settings[5] = nr*nc;
            break;
        case 3:
            settings[0] = 1; settings[1] = 1; settings[2] = nr*nc;
            settings[3] = nr*nc; settings[4] = 1; settings[5] = ne;
            break;
    }
    
    if (mxIsComplex(prhs[0]) && mxIsComplex(prhs[1]) && mxIsComplex(prhs[2]) && mxIsComplex(prhs[3])) {
        plhs[0] = tridiag_c(prhs, settings);
    }
    else if (!mxIsComplex(prhs[0]) && !mxIsComplex(prhs[1]) && !mxIsComplex(prhs[2]) && !mxIsComplex(prhs[3])){
        plhs[0] = tridiag_r(prhs, settings);
    }
    else {
        mexErrMsgIdAndTxt("tdma:InvalidInput",
                          "All inputs must be either purely real or complex!");
    }
}

// ------------------------------------------------------------------------
// Real TDMA:

struct inputs_r {
    mwSize* settings;
    mwSize n;
    mxDouble* main;
    mxDouble* upper;
    mxDouble* lower;
    mxDouble* f;
    mxDouble* v;
    mxDouble* y;
};

void tridiag_r_multithread(void*);
// unsigned __stdcall tridiag_r_multithread(void*);

mxArray* tridiag_r(const mxArray* prhs[], mwSize settings[])
{
    const mwSize istep = settings[0];
    const mwSize jstep = settings[1];
    const mwSize kstep = settings[2];
    const mwSize imax = settings[3];
    const mwSize jmax = settings[4];
    const mwSize kmax = settings[5];
    
    const mwSize n = mxGetNumberOfElements(prhs[0]);
    const mwSize ndims = mxGetNumberOfDimensions(prhs[0]);
    const mwSize* dims = mxGetDimensions(prhs[0]);
    
    mxDouble* main = mxGetDoubles(prhs[0]);
    mxDouble* lower = mxGetDoubles(prhs[1]);
    mxDouble* upper = mxGetDoubles(prhs[2]);
    mxDouble* f = mxGetDoubles(prhs[3]);
    
    mxArray* tmp = mxCreateNumericArray(ndims, dims, mxDOUBLE_CLASS, mxREAL);
    mxDouble* v = mxGetDoubles(tmp);
    
    mxArray* output = mxCreateNumericArray(ndims, dims, mxDOUBLE_CLASS, mxREAL);
    mxDouble* y = mxGetDoubles(output);
    
    const bool multithread = (jmax == n) && (dims[0] > 1) && (dims[1] > 1);
        
    if (multithread) {
//         mxArray* nt[1];
//         mexCallMATLAB(1, nt, 0, NULL, "maxNumCompThreads");
        int nthreads = GetNumThreads(); // (int) *mxGetDoubles(nt[0]);
    
        if (nthreads > dims[1]) {
            nthreads = (int) dims[1];
        }
        
        struct inputs_r args[nthreads];
//         HANDLE threads[nthreads];
        
        mwSize nc;
        if (ndims == 3) {
            nc = (mwSize) ceil((mxDouble) dims[1] * dims[2] / nthreads);
        }
        else {
            nc = (mwSize) ceil((mxDouble) dims[1] / nthreads);
        }
        mwSize numel = nc * dims[0];
        
        mwSize idx = 0;
        for (int i = 0; i < nthreads; i++) {
            if (idx + numel > n) {
                numel = n - idx;
            }
            args[i].main = &main[idx];
            args[i].upper = &upper[idx];
            args[i].lower = &lower[idx];
            args[i].f = &f[idx];
            args[i].v = &v[idx];
            args[i].y = &y[idx];
            
            args[i].settings = &settings[0];
            args[i].n = numel;
            idx += numel;
            
            AddThreadPoolJob(&tridiag_r_multithread, &args[i]);
//             threads[i] = (HANDLE) _beginthreadex(NULL, 0, &tridiag_r_multithread, &args[i], 0, NULL);
//             
//             bool threaderr = (threads[i] == 0);
//             if (threaderr) {
//                 mexErrMsgIdAndTxt("tdma:ThreadError", "Error creating new thread!");
//             }
        }
        SynchronizeThreads();
//         WaitForMultipleObjects(nthreads, threads, TRUE, INFINITE);
//         for (int i = 0; i < nthreads; i++) {
//             CloseHandle(threads[i]);
//         }
        
        mxDestroyArray(tmp);
        return output;
    }
    
    mxDouble w;
    mwIndex i, j, k;
    for (i = 0; i < imax; i += istep) {
        for (j = i; j < i + jmax; j += jstep) {
            w = main[j];
            y[j] = f[j] / w;
            
            for (k = j + kstep; k < j + kmax; k += kstep) {
                
                v[k-kstep] = upper[k-kstep] / w;
                w = main[k] - lower[k] * v[k-kstep];
                
                y[k] = (f[k] - lower[k] * y[k-kstep]) / w;
            }
        }
    }
    
    mwIndex m;
    for (i = 0; i < imax; i += istep) {
        for (j = i; j < i + jmax; j += jstep) { 
            for (k = j + kstep; k < j + kmax; k += kstep) {
                m = n - 1 - k;
                y[m] = y[m] - v[m] * y[m+kstep];
            }
        }
    }
    
    mxDestroyArray(tmp);
    return output;
}

void tridiag_r_multithread(void* args)
{
    mwSize* settings = ((struct inputs_r*) args) -> settings;
    const mwSize istep = settings[0];
    const mwSize jstep = settings[1];
    const mwSize kstep = settings[2];
    const mwSize imax = settings[3];
    const mwSize kmax = settings[5];
    
    const mwSize n = ((struct inputs_r*) args) -> n;
    const mwSize jmax = n;
    
    mxDouble* main = ((struct inputs_r*) args) -> main;
    mxDouble* upper = ((struct inputs_r*) args) -> upper;
    mxDouble* lower = ((struct inputs_r*) args) -> lower;
    mxDouble* f = ((struct inputs_r*) args) -> f;
    
    mxDouble* v = ((struct inputs_r*) args) -> v;
    mxDouble* y = ((struct inputs_r*) args) -> y;
    
    mxDouble w;
    mwIndex i, j, k;
    for (i = 0; i < imax; i += istep) {
        for (j = i; j < i + jmax; j += jstep) {
            w = main[j];
            y[j] = f[j] / w;
            
            for (k = j + kstep; k < j + kmax; k += kstep) {
                
                v[k-kstep] = upper[k-kstep] / w;
                w = main[k] - lower[k] * v[k-kstep];
                
                y[k] = (f[k] - lower[k] * y[k-kstep]) / w;
            }
        }
    }
    
    mwIndex m;
    for (i = 0; i < imax; i += istep) {
        for (j = i; j < i + jmax; j += jstep) { 
            for (k = j + kstep; k < j + kmax; k += kstep) {
                m = n - 1 - k;
                y[m] = y[m] - v[m] * y[m+kstep];
            }
        }
    }
    
//     _endthreadex(0);
//     return 0;
}

// ------------------------------------------------------------------------
// Complex TDMA:

// real part of (a + jb) / (c + jd)
mxDouble cdiv_r(mxDouble a, mxDouble b, mxDouble c, mxDouble d)
{
    return (a*c + b*d) / (c*c + d*d);
}

// imaginary part of (a + jb) / (c + jd)
mxDouble cdiv_i(mxDouble a, mxDouble b, mxDouble c, mxDouble d)
{
    return (b*c - a*d) / (c*c + d*d);
}

// real part of (a + jb) * (c + jd)
mxDouble ctimes_r(mxDouble a, mxDouble b, mxDouble c, mxDouble d)
{
    return a*c - b*d;
}

// imaginary part of (a + jb) * (c + jd)
mxDouble ctimes_i(mxDouble a, mxDouble b, mxDouble c, mxDouble d)
{
    return a*d + b*c;
}

struct inputs_c {
    mwSize* settings;
    mwSize n;
    mxComplexDouble* main;
    mxComplexDouble* upper;
    mxComplexDouble* lower;
    mxComplexDouble* f;
    mxComplexDouble* v;
    mxComplexDouble* y;
};

void tridiag_c_multithread(void*);
// unsigned __stdcall tridiag_c_multithread(void*);

mxArray* tridiag_c(const mxArray* prhs[], mwSize settings[]) {
    const mwSize istep = settings[0];
    const mwSize jstep = settings[1];
    const mwSize kstep = settings[2];
    const mwSize imax = settings[3];
    const mwSize jmax = settings[4];
    const mwSize kmax = settings[5];
    
    const mwSize n = mxGetNumberOfElements(prhs[0]);
    const mwSize ndims = mxGetNumberOfDimensions(prhs[0]);
    const mwSize* dims = mxGetDimensions(prhs[0]);
    
    mxComplexDouble* main = mxGetComplexDoubles(prhs[0]);
    mxComplexDouble* lower = mxGetComplexDoubles(prhs[1]);
    mxComplexDouble* upper = mxGetComplexDoubles(prhs[2]);
    mxComplexDouble* f = mxGetComplexDoubles(prhs[3]);
    
    mxArray* tmp = mxCreateNumericArray(ndims, dims, mxDOUBLE_CLASS, mxCOMPLEX);
    mxComplexDouble* v = mxGetComplexDoubles(tmp);
    
    mxArray* output = mxCreateNumericArray(ndims, dims, mxDOUBLE_CLASS, mxCOMPLEX);
    mxComplexDouble* y = mxGetComplexDoubles(output);
    
    // will only multithread if there are multiple TDMA problems to be
    // solve at once, and inputs are oriented such that direction == 1;
    // that is oriented to solve down columns.
    const bool multithread = (jmax == n) && (dims[0] > 1) && (dims[1] > 1);
    
    if (multithread) {
//         mxArray* nt[1];
//         mexCallMATLAB(1, nt, 0, NULL, "maxNumCompThreads");
        int nthreads = GetNumThreads(); // (int) *mxGetDoubles(nt[0]);
        
        if (nthreads > dims[1]) {
            nthreads = (int) dims[1];
        }
        
        struct inputs_c args[nthreads];
//         HANDLE threads[nthreads];
        
        mwSize nc;
        if (ndims == 3) {
            nc = (mwSize) ceil((mxDouble) dims[1] * dims[2] / nthreads);
        }
        else {
            nc = (mwSize) ceil((mxDouble) dims[1] / nthreads);
        }
        mwSize numel = nc * dims[0];
        
        mwSize idx = 0;
        for (int i = 0; i < nthreads; i++) {
            if (idx + numel > n) {
                numel = n - idx;
            }
            args[i].main = &main[idx];
            args[i].upper = &upper[idx];
            args[i].lower = &lower[idx];
            args[i].f = &f[idx];
            args[i].v = &v[idx];
            args[i].y = &y[idx];
            
            args[i].settings = &settings[0];
            args[i].n = numel;
            idx += numel;
            
            AddThreadPoolJob(&tridiag_c_multithread, &args[i]);
//             threads[i] = (HANDLE) _beginthreadex(NULL, 0, &tridiag_c_multithread, &args[i], 0, NULL);
//             
//             bool threaderr = (threads[i] == 0);
//             if (threaderr) {
//                 mexErrMsgIdAndTxt("tdma:ThreadError", "Error creating new thread!");
//             }
        }
        SynchronizeThreads();
//         WaitForMultipleObjects(nthreads, threads, TRUE, INFINITE);
//         for (int i = 0; i < nthreads; i++) {
//             CloseHandle(threads[i]);
//         }
        
        mxDestroyArray(tmp);
        return output;
    }
    
    mxComplexDouble w;
    mxComplexDouble x;
    
    mwIndex i, j, k;
    for (i = 0; i < imax; i += istep) {
        for (j = i; j < i + jmax; j += jstep) {
            
            w.real = main[j].real;
            w.imag = main[j].imag;
//          y[j] = f[j] / w;
            y[j].real = cdiv_r(f[j].real, f[j].imag, w.real, w.imag);
            y[j].imag = cdiv_i(f[j].real, f[j].imag, w.real, w.imag);
            
            for (k = j + kstep; k < j + kmax; k += kstep) {
                
//              v[k-kstep] = upper[k-kstep] / w;
                v[k-kstep].real = cdiv_r(upper[k-kstep].real, upper[k-kstep].imag, w.real, w.imag);
                v[k-kstep].imag = cdiv_i(upper[k-kstep].real, upper[k-kstep].imag, w.real, w.imag);
                
//              w = main[k] - lower[k] * v[k-kstep];
                w.real = main[k].real - ctimes_r(lower[k].real, lower[k].imag, v[k-kstep].real, v[k-kstep].imag);
                w.imag = main[k].imag - ctimes_i(lower[k].real, lower[k].imag, v[k-kstep].real, v[k-kstep].imag);
                
//              y[k] = (f[k] - lower[k] * y[k-kstep]) / w;
                x.real = f[k].real - ctimes_r(lower[k].real, lower[k].imag, y[k-kstep].real, y[k-kstep].imag);
                x.imag = f[k].imag - ctimes_i(lower[k].real, lower[k].imag, y[k-kstep].real, y[k-kstep].imag);
                y[k].real = cdiv_r(x.real, x.imag, w.real, w.imag);
                y[k].imag = cdiv_i(x.real, x.imag, w.real, w.imag);
            }
        }
    }
    
    mwIndex m;
    for (i = 0; i < imax; i += istep) {
        for (j = i; j < i + jmax; j += jstep) { 
            for (k = j + kstep; k < j + kmax; k += kstep) {
                m = n - 1 - k;
                y[m].real = y[m].real - ctimes_r(v[m].real, v[m].imag, y[m+kstep].real, y[m+kstep].imag);
                y[m].imag = y[m].imag - ctimes_i(v[m].real, v[m].imag, y[m+kstep].real, y[m+kstep].imag);
            }
        }
    }
    
    mxDestroyArray(tmp);
    return output;
}

// output = a * b
mxComplexDouble ctimes(mxComplexDouble a, mxComplexDouble b)
{
    __m256d row1 = _mm256_set_pd(a.real, a.imag, b.real, b.imag);
    __m256d row2 = _mm256_set_pd(b.real, -b.imag, a.imag, a.real);
    
    row1 = _mm256_mul_pd(row1, row2);
    row1 = _mm256_hadd_pd(row1, row1);
    
    mxDouble tmp[4];
    _mm256_store_pd(tmp, row1);
    
    mxComplexDouble output;
    output.real = tmp[2];
    output.imag = tmp[0];
    return output;
}

// output = a / b
mxComplexDouble cdiv(mxComplexDouble a, mxComplexDouble b)
{
    __m256d row1 = _mm256_set_pd(a.real, a.imag, b.real, b.imag);
    __m256d row2 = _mm256_set_pd(b.real, b.imag, a.imag, -a.real);
    
    row1 = _mm256_mul_pd(row1, row2);
    row1 = _mm256_hadd_pd(row1, row1);
    
    mxDouble tmp[4];
    _mm256_store_pd(tmp, row1);
    
    mxDouble invmag = 1 / (b.real*b.real + b.imag*b.imag);
    
    mxComplexDouble output;
    output.real = tmp[2] * invmag;
    output.imag = tmp[0] * invmag;
    return output;
}

void tridiag_c_multithread(void* args)
// unsigned __stdcall tridiag_c_multithread(void* args)
{
    mwSize* settings = ((struct inputs_c*) args) -> settings;
    const mwSize jstep = settings[1];
    const mwSize kstep = settings[2];
    const mwSize kmax = settings[5];
    
    const mwSize n = ((struct inputs_c*) args) -> n;
    const mwSize jmax = n;
    
    mxComplexDouble* main = ((struct inputs_c*) args) -> main;
    mxComplexDouble* upper = ((struct inputs_c*) args) -> upper;
    mxComplexDouble* lower = ((struct inputs_c*) args) -> lower;
    mxComplexDouble* f = ((struct inputs_c*) args) -> f;
    
    mxComplexDouble* v = ((struct inputs_c*) args) -> v;
    mxComplexDouble* y = ((struct inputs_c*) args) -> y;
    
    mxComplexDouble w;
    mxComplexDouble x;
    
    mwIndex j, k;
    for (j = 0; j < jmax; j += jstep) {
        
        w = main[j];
//      y[j] = f[j] / w;
        y[j] = cdiv(f[j], w);
        
        for (k = j + kstep; k < j + kmax; k += kstep) {
            
//          v[k-kstep] = upper[k-kstep] / w;
            v[k-kstep] = cdiv(upper[k-kstep], w);
            
//          w = main[k] - lower[k] * v[k-kstep];
            x = ctimes(lower[k], v[k-kstep]);
            w.real = main[k].real - x.real;
            w.imag = main[k].imag - x.imag;
            
//          y[k] = (f[k] - lower[k] * y[k-kstep]) / w;
            x = ctimes(lower[k], y[k-kstep]);
            x.real = f[k].real - x.real;
            x.imag = f[k].imag - x.imag;
            y[k] = cdiv(x, w);
        }
    }
    
    mwIndex m;
    for (j = 0; j < jmax; j += jstep) {
        for (k = j + kstep; k < j + kmax; k += kstep) {
            m = n - 1 - k;
            
//          y[m] = y[m] - v[m] * y[m+kstep];
            x = ctimes(v[m], y[m+kstep]);
            y[m].real = y[m].real - x.real;
            y[m].imag = y[m].imag - x.imag;
        }
    }
}

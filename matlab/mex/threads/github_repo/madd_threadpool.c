// function c = MADD(a, b)
//   MADD  Adds two vectors, a and b, using multiple threads. With inline
//   comments to help myself and other people figure out what's going on
//   with multi-threaded MEX functions in MATLAB.
//   
//   Use the following command to build this MEX file:
//       mex -R2018a madd_threadpool.c
//
// This is just a companion file to help demonstrate the difference between
// using the threadpool and basic multithreading.
#include "mex.h"
#include "matrix.h"
#include "math.h"
#include "threadpool.c"

void multithread_add(void*);

struct parameters {
    mxDouble* input1;
    mxDouble* input2;
    mwSize numel;
    mxDouble* output;
};

// c = madd(a, b)
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs < 2) {
        mexErrMsgIdAndTxt("madd:InvalidInput", "Not enough input arguments!");
    }
    if (!mxIsDouble(prhs[0]) || mxIsComplex(prhs[0]) || mxIsSparse(prhs[0])) {
        mexErrMsgIdAndTxt("madd:InvalidInput", "Inputs must be real doubles, and not sparse.");
    }
    if (mxGetNumberOfElements(prhs[0]) != mxGetNumberOfElements(prhs[1])) {
        mexErrMsgIdAndTxt("madd:InvalidInput", "Inputs have to be the same size!");
    }

    mxDouble* input1 = mxGetDoubles(prhs[0]);
    mxDouble* input2 = mxGetDoubles(prhs[1]);
    
    const mwSize numel = mxGetNumberOfElements(prhs[0]);
    const mwSize ndims = mxGetNumberOfDimensions(prhs[0]);
    const mwSize* dims = mxGetDimensions(prhs[0]);
    
    plhs[0] = mxCreateNumericArray(ndims, dims, mxDOUBLE_CLASS, mxREAL);
    mxDouble* output = mxGetDoubles(plhs[0]);
    
    mxArray* n[1];
    mexCallMATLAB(1, n, 0, NULL, "maxNumCompThreads");
    int nthreads = (int) *mxGetDoubles(n[0]);
    
    int offset = floor((mxDouble) numel / nthreads);
        
    struct parameters args[256];  // @Chen nthreads];
    SynchronizeThreads();
    for (int i = 0; i < nthreads; i++) {
        args[i].input1 = &input1[i * offset];
        args[i].input2 = &input2[i * offset];
        args[i].output = &output[i * offset];
        args[i].numel = offset;
        
        AddThreadPoolJob(&multithread_add, &args[i]);
    }
    
    struct parameters lastarg;
    lastarg.input1 = &input1[nthreads * offset];
    lastarg.input2 = &input2[nthreads * offset];
    lastarg.output = &output[nthreads * offset];
    lastarg.numel = numel - nthreads * offset;
        
    multithread_add(&lastarg);
    
    SynchronizeThreads();
}

void multithread_add(void* args)
{
    mxDouble* input1 = ((struct parameters*) args) -> input1;
    mxDouble* input2 = ((struct parameters*) args) -> input2;
    mxDouble* output = ((struct parameters*) args) -> output;
    mwSize imax = ((struct parameters*) args) -> numel;
    
    for (mwIndex i = 0; i < imax; i++) {
        output[i] = input1[i] + input2[i];
    }
}

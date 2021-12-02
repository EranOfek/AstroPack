// function c = MADD(a, b)
//   MADD  Adds two vectors, a and b, using multiple threads. With inline
//   comments to help myself and other people figure out what's going on
//   with multi-threaded MEX functions in MATLAB.
//   
//   Use the following command to build this MEX file:
//       mex -R2018a madd.cpp
//   
//   Since this is used only for learning purposes, if the total number of
//   elements in a and b aren't a multiples of the number of threads
//   available, then mod(numel(a), nthreads) elements will be left as 0.
//   
//   I figured out a lot of this stuff by reading through Yair Altman's
//   article on multi-threaded mex functions:
//   
//     https://undocumentedmatlab.com/blog/multi-threaded-mex
//   
//   Everything here is for Windows, but if you want to use non-Windows you
//   can take a look at pthreads, e.g. as described in the above article or
//   elsewhere throughout the internet.  Also, after running through some
//   performance testing: always always always figure out how to vectorize
//   your code when you can.  Good MATLAB code is always faster than
//   terrible C code.
//   
//   Relevant Microsoft documentation for weird functions and their
//   requirements can be found via the internet:
//   
//     https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/beginthread-beginthreadex
//
//     https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/endthread-endthreadex
//
//     https://docs.microsoft.com/en-us/windows/desktop/api/synchapi/nf-synchapi-waitformultipleobjects
//
//     https://docs.microsoft.com/en-us/windows/desktop/api/synchapi/nf-synchapi-waitforsingleobject
//
//     https://docs.microsoft.com/en-us/windows/desktop/api/handleapi/nf-handleapi-closehandle
//
//   More examples of multithreading can be found with MATLAB answers:
//
//     https://www.mathworks.com/matlabcentral/answers/101658-is-it-possible-to-start-new-threads-from-a-c-mex-file
//
//   Where they also discuss how the MEX API is not thread safe, so you
//   shouldn't use things like, e.g. mxCreateNumericArray, within threads 
//   as the whole thing will explode.

#include "mex.h"
#include "matrix.h"
#include "math.h"
#include <windows.h>
#include <process.h>

// A function declaration that looks like this is required when creating
// new threads with _beginthreadex (and other functions).
// 
// It has to return an unsigned integer, and it has to use the __stdcall
// calling convention.
// 
// It has to accept a void pointer, which I've seen used as a pointer to
// pointers or arrays or structs, proper casting of which is taken care of
// in the function so that data can be used.
unsigned __stdcall multithread_add(void*);

// When sending information into multithreaded functions (e.g. as declared
// above), I prefer to use structs instead of pointers to pointers to
// references of arrays of pointers.  It makes the code easier to follow as
// all of the inputs/outputs to each thread can be kept track of in an
// array of such structs.
//
// This struct, parameters, contains pointers to the data that needs to be
// used, as well as any other information required by each thread; and then
// finally a pointer to the section of the output array that will be
// written into.
struct parameters {
    mxDouble* input1;
    mxDouble* input2;
    mwSize numel;
    mxDouble* output;
};

// c = madd(a, b)
void mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    // Basic error checking for when we mess up our testing.  In a real 
    // function we'd do more error checking to make sure that the inputs 
    // and outputs are properly defined for our problem.
    if (nrhs < 2) {
        mexErrMsgIdAndTxt("madd:InvalidInput", "Not enough input arguments!");
    }
    if (!mxIsDouble(prhs[0]) || mxIsComplex(prhs[0]) || mxIsSparse(prhs[0])) {
        mexErrMsgIdAndTxt("madd:InvalidInput", "Inputs must be real doubles, and not sparse.");
    }
    if (mxGetNumberOfElements(prhs[0]) != mxGetNumberOfElements(prhs[1])) {
        mexErrMsgIdAndTxt("madd:InvalidInput", "Inputs have to be the same size!");
    }

    // Grabbing pointers to the actual data for both of our input matrices.
    mxDouble* input1 = mxGetDoubles(prhs[0]);
    mxDouble* input2 = mxGetDoubles(prhs[1]);
    
    // Figuring out some constants so that we can properly define what the
    // output will look like.
    const mwSize numel = mxGetNumberOfElements(prhs[0]);
    const mwSize ndims = mxGetNumberOfDimensions(prhs[0]);
    const mwSize* dims = mxGetDimensions(prhs[0]);
    
    // Creating the output array, and then grabbing a pointer to the actual
    // data for the output array.
    plhs[0] = mxCreateNumericArray(ndims, dims, mxDOUBLE_CLASS, mxREAL);
    mxDouble* output = mxGetDoubles(plhs[0]);
    
    // Determining the number of threads available by making a call from
    // this mex function back into MATLAB.
    mxArray* n[1];
    mexCallMATLAB(1, n, 0, NULL, "maxNumCompThreads");
    int nthreads = (int) *mxGetDoubles(n[0]);

    // A terrible way to, "logically," figure out how many data points will
    // be accessed in each thread.  Figuring out proper multi-threaded
    // algorithms is the hardest part, right?
    int offset = ceil((mxDouble) numel / nthreads);
    
    // This creates an array of parameters structs, args, a single one of
    // which we will pass in as an argument to our multithreaded function
    // each time we create and begin a new thread.  Thankfully parameter
    // and argument are synonyms for eachother in English language
    // programming; though this naming is still mildly confusing.
    struct parameters args[256];  // @Chen nthreads];

    // This is where we set up how things are divided up into our multiple
    // threads.  Each element of the args array is a parameters struct that
    // contains pointers to the beginning of the data each thread will use.
    //
    // Notice that we're not creating new mxArrays or matrices to help us
    // divide up our data, but just pointers to sections of the data that
    // each thread will use.  This probably violates a few best practices
    // as if you're not careful in your multithreaded function, you could
    // write over sections of data that are being used by other threads.
    //
    // For more complicated functions, I usually create new arrays for
    // everything and then combine them at the end.  However, activity like 
    // that can eliminate any performance gains from naive multi-threading.
    for (int i = 0; i < nthreads; i++) {
        
        // pointers to the location of each segment of data to be used
        // by individual threads.
        args[i].input1 = &input1[i * offset];
        args[i].input2 = &input2[i * offset];
        args[i].output = &output[i * offset];
        
        // total number of elements allowed to be accessed by each thread.
        args[i].numel = offset;
    }
    // If I wanted to create a separate array, e.g. for a section of the
    // output, I might try something like:
    //
    //   mwSize adims[2] = {offset, 1};
    //   args[i].output = mxGetDoubles(mxCreateNumericArray(2, &dims[0], mxDOUBLE_CLASS, mxREAL));
    //
    // But then you'd have to figure out how to piece it all back together 
    // again so that your final output makes sense for the problem at hand.
    // And creating a temp array like this makes it hard to properly
    // destroy the array after you're finished.

    // This creates an array of HANDLEs (weird generic pointers) that will
    // help us keep track of our threads.  This is a mandatory input to the
    // functions that are used to wait for and properly terminate threads.
    HANDLE threads[256];  // @Chen nthreads];
    
    // This loop creates and begins each thread.  I don't really understand
    // all of the input parameters for _beginthreadex, but the most 
    // important ones are the pointer to the function each thread will be 
    // processing (as declared above), and then a pointer to wherever the 
    // argumets for that thread sit in memory.  
    //
    // That's the trickiest part for me, because the input argument needs
    // to be a void pointer (void*), which as long as it's the same size
    // as any other pointer on your system (e.g. double* or mxDouble*), 
    // shouldn't be an issue.  But then de-referencing things again I find
    // hard to follow, which is why I like to pass in structs. In this case
    // we are passing in the address (which gets casted to a void*) to each 
    // parameters struct contained in the struct array args[].
    // 
    // _beginthreadex returns a uintptr_t, which we have to cast to a
    // HANDLE in order to properly manage waiting for and closing down
    // threads.
    for (int i = 0; i < nthreads; i++) {
        threads[i] = (HANDLE) _beginthreadex(NULL, 0, &multithread_add, &args[i], 0, NULL);
    }
    
    // This just waits for each thread to complete.  TRUE means wait for
    // all threads to complete, and INFINITE means wait until the end of
    // time for all threads to complete.  You can get fun results if you
    // don't have this here (or a loop of WaitForSingleObject calls), as
    // this function will keep going, discarding all of the work done by
    // threads that take longer to execute.
    WaitForMultipleObjects(nthreads, threads, TRUE, INFINITE);
    
    // This just closes the HANDLEs for each thread, I'm not sure what that
    // means, but I think it's a Windows thing.
    for (int i = 0; i < nthreads; i++) {
        CloseHandle(threads[i]);
    }
}

// This is the function that actually does the real work within each
// separate thread that was created and launched with the _beginthreadex
// function.  It is declared in this way because that's what's required
// to properly launch things with _beginthreadex.
//
// Reminder: don't use MEX API functions in this function; anything that
// calls back to MATLAB, including printf or mexPrintf.  They're not thread
// according to MATLAB support and smart people on the internet.
unsigned __stdcall multithread_add(void* args)
{
    // Since args comes into our thread as a void*, in order to use it we
    // have to translate it back into what it really is: a pointer to a
    // parameters struct that contains references to the data that we need.
    // If you're not using structs, you'll have to figure out a different
    // way to get access to the actual data that each thread needs.
    //
    // In order use this data, we have to cast the void* to a pointer for a
    // parameters struct, and then use the weird arrow notation to access
    // struct elements directly from a pointer to a struct.
    mxDouble* input1 = ((struct parameters*) args) -> input1;
    mxDouble* input2 = ((struct parameters*) args) -> input2;
    mxDouble* output = ((struct parameters*) args) -> output;
    mwSize imax = ((struct parameters*) args) -> numel;
    
    // This is the loop that adds the numbers from each array together;
    // finally!
    //
    // Remember: each thread is taking data directly from the input arrays
    // and then placing them directly into the output array without using
    // any temporary or intermediary variables / arrays.  This is probably
    // a bad idea for more complex problems.
    for (mwIndex i = 0; i < imax; i++) {
        output[i] = input1[i] + input2[i];
    }
    
    // This just terminates the thread properly, freeing up resources.
    _endthreadex(0);
    return 0;
}
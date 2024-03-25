#include "mex.h"
#include <thread>
#include <chrono>
#include <iostream>

// This is the function that will be executed by the worker thread.
void workerFunction() {
    std::this_thread::sleep_for(std::chrono::milliseconds(100)); // Simulate work by sleeping
    mexPrintf("Worker thread has completed its task.\n");
}

// The gateway function for the MEX file
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    // Start the worker thread
    std::thread worker(workerFunction);

    // Immediately detach the thread to allow it to run independently
    worker.detach();

    mexPrintf("The worker thread has been started and detached.\n");
}



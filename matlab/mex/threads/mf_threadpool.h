// THREADPOOL   Persistent thread pool for MATLAB MEX files so that you
// don't have to suffer the overhead of creating / destroying threads
// every time you run a MEX file.  Can significantly speed up execution
// of MEX files, especially if you're solving a lot of smaller problems
// in a loop, or calling into the MEX file a lot in a loop.
//
//   To use this in your MEX file, you need to:
//      #include "threadpool.c"
//
//   From there, the only two commands you need to know are:
//      - AddThreadPoolJob(void (*jobfunc)(void*), void* jobargs);
//      - SynchronizeThreads();
//
//   AddThreadPoolJob should be used in place of creating HANDLEs or
//   threads or whatever fun stuff is needed to create a standalone multi-
//   threaded MEX file. The first argument that needs to be passed is an 
//   address to the function that you'll be running on your data.  The 
//   function signature must be:
//
//      void jobfunc(void* jobargs)
//
//   ... but with jobfunc or jobargs called whatever you want.
//   The second argument has to be a pointer to an array or a struct that 
//   can be cast to a void poitner (void*), and that allows you to access 
//   the data that needs to be manipulated.  Since the function can't  
//   return anything, this should include pointers to where your results
//   need to go.
//
//   SynchronizeThreads should be used before returning to MATLAB from your
//   MEX file otherwise there will be fun results.
//
//   And then compile your MEX file with:
//      mex -R2018a yourmex.c
// 
//   Where yourmex.c should be replaced with the name of the mex file you
//   wrote.
//
//   To destroy the thread pool and free up memory, all you need to do is
//   call CLEAR ALL (not just clear) from the MATLAB command prompt or in
//   your script or function.  Memory will also be cleared properly upon
//   exiting MATLAB.
//
//   See also MADD_THREADPOOL.c as an example on how to use things.
//
//   This might work on MATLAB versions older than R2018a, but I only use
//   -R2018a to compile my MEX files 'cause that's the way I like it so
//   I can only say it works on R2018a and above.

#pragma once

#include "mex.h"
#include "mf_thread.h"

// Main functions used in the MEX file you'd like to write:
// 
// AddThreadPoolJob adds jobs to the ThreadPool, and if needed initializes
// everything, setting up the ThreadPool so that it seamlessly persists
// between calls to other MEX files.
//
// The first argument that needs to be passed is an address to the function
// that you'll be running on your data.  The function signature must be:
//
//      void jobfunc(void* jobargs)
//
// ... but with jobfunc or jobargs called whatever you want.
//
// The second argument has to be a pointer to an array or a struct that 
// can be cast to a void poitner (void*), and that allows you to access the
// data that needs to be manipulated.  Since the function can't return 
// anything, this should include pointers to where your results need to go.
//
// See also MADD_THREADPOOL.c and MADD.c with examples on using the
// thread pool vs regular multithreading.
void AddThreadPoolJob(void (*jobfunc)(void*), void* jobargs);

// Call this before leaving your MEX function to go back to MATLAB.  This
// makes sure that all of the threads have finished doing whatever they
// were doing and any output data is ready to be returned.  If you don't
// use this, you're gonna get some very interesting race conditions going
// on.  It's actually kinda neat to see your MATLAB workspace change in
// real time.
void SynchronizeThreads();

// Use this instead of figuring out how many threads you want to use.  Will
// initialize the Thread Pool if not already.  Can significantly cut down
// on MEX file runtime if you're using maxNumCompThreads to determine the
// number of threads to use.
int GetNumThreads();

// Next two functions are used for importing / exporting the memory address
// of the Thread Pool so that you can re-use it amongst multiple MEX files:

// SetThreadPool(prhs[n])
void SetThreadPool();
// plhs[n] = GetThreadPool();
mxArray* GetThreadPool();

//--- Everything below here is not needed by the end user. ---

struct Queue {
    struct Queue* front;
    struct Queue* next;
    struct Queue* back;
    void* data;
};


struct ThreadPool {
	
    // To be our array of handles to threads.
    Threads* threads;
	
    // Signals when Queue is free to be modified.
    Mutex* QueueMutex;
	
    // Event signals when Queue is not empty or memory needs to be cleaned.
    EventSignal* JobReadyOrCleanup;

    // Event signals when a job has finished.
    EventSignal* JobFinished;
    
    Mutex* ThreadPoolMutex; // Mutex allows checking of vars:
    bool isalive;
    int jobsremaining;
    
    int nthreads;
};

// Global variables to make sure that they can properly persist and be
// cleaned up.  Are also needed to access everything, so prety useful!
extern struct Queue* QueuePtr;
extern struct ThreadPool* ThreadPool;

struct Job {
    void (*jobfunc)(void*);
    void* jobargs;
};

// I'm pretty sure QueuePtr, Push and Pop make a pretty standard queue
// implementation, so with very little change can be pulled out of here
// and put somewhere else.
void Push(struct Queue*);
void* Pop();

// Separate function to initialize the ThreadPool simply to make code a
// little bit easier to read.
struct ThreadPool* InitThreadPool();

// Next three files are used to clean up memory when CLEAR ALL is called 
// from MATLAB.
void FreeQueue();
void FreeThreadPool();
void CleanupMemory();

// This declaration is needed by _beginthreadex.
unsigned __stdcall ThreadFunction(void*);

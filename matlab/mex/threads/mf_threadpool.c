// Full API documentation included in threadpool.h.
//
//   To use this in your MEX file, you need to:
//      #include "threadpool.h"
//
//   And then compile your MEX file with:
//      mex -R2018a yourmex.c threadpool.c
//
//   Where yourmex.c should be replaced with the name of the mex file you
//   wrote.  From there, the only two commands you need to know are:
//      - AddThreadPoolJob(void (*jobfunc)(void*), void* jobargs);
//      - SynchronizeThreads();
//
//   SynchronizeThreads needs to run before returning to MATLAB from your
//   MEX file otherwise there will be fun results.
//
//   This might work on MATLAB versions older than R2018a, but I only use
//   -R2018a to compile my MEX files 'cause that's the way I like it so
//   I can only say it works on R2018a and above.

#include "threadpool.h"
#include <process.h>

#ifndef _WIN32
#define INFINITE 10000000
#endif


// Defining previously declared globals:
struct Queue* QueuePtr = NULL;
struct ThreadPool* ThreadPool = NULL;

//
void AddThreadPoolJob(void (*jobfunc)(void*), void* jobargs)
{
    struct Job* newjob = (struct Job*) mxCalloc(1, sizeof(struct Job));
    newjob -> jobfunc = jobfunc;
    newjob -> jobargs = jobargs;
    mexMakeMemoryPersistent(newjob);
    
    struct Queue* jobnode = (struct Queue*) mxCalloc(1, sizeof(struct Queue));
    jobnode -> data = newjob;
    mexMakeMemoryPersistent(jobnode);
    
    if (ThreadPool == NULL) {
        InitThreadPool();
    }
    
    ThreadPool -> ThreadPoolMutex->lock();
    ThreadPool -> jobsremaining++;
    ResetEvent(ThreadPool -> JobFinished);
    ReleaseMutex(ThreadPool -> ThreadPoolMutex);
    
    ThreadPool->QueueMutex->lock();    
    Push(jobnode);    
    ThreadPool -> QueueMutex->unlock();
}


struct ThreadPool* InitThreadPool()
{
    ThreadPool = (struct ThreadPool*) mxCalloc(1, sizeof(struct ThreadPool));
    mexMakeMemoryPersistent(ThreadPool);
    
    // Creates mutex, doesn't need to take immediate ownership of it since
    // we need to do that anyway before pushing anything onto the queue.
    ThreadPool -> QueueMutex = new Mutex();
    if (ThreadPool -> QueueMutex == NULL) {
        mexErrMsgIdAndTxt("AddThreadPoolJob:MutexError", "Error creating mutex!");
    }
    
    // Auto-reset event, initial state is non-signaled since the queue
    // doesn't have any jobs yet--hasn't even been created!
    ThreadPool -> JobReadyOrCleanup = new EventSignal();  //CreateEvent(NULL, false, false, NULL);
    if (ThreadPool -> JobReadyOrCleanup == NULL) {
        mexErrMsgIdAndTxt("AddThreadPoolJob:EventError", "Error creating event!");
    }
    
    // Auto-reset event, initial state is non-signaled since no jobs have
    // been created yet, let alone finished.
    ThreadPool -> JobFinished = new EventSignal();  //CreateEvent(NULL, false, false, NULL);
    if (ThreadPool -> JobFinished == NULL) {
        mexErrMsgIdAndTxt("AddThreadPoolJob:EventError", "Error creating event!");
    }
    
    // Creates mutex and immediately takes ownership of it so that we can
    // make sure the threads don't start working until everything is
    // properly initalized.
    ThreadPool -> ThreadPoolMutex = new Mutex();
    if (ThreadPool -> ThreadPoolMutex == NULL) {
        mexErrMsgIdAndTxt("AddThreadPoolJob:MutexError", "Error creating mutex!");
    }
    
    ThreadPool -> isalive = true;
    ThreadPool -> jobsremaining = 0;
    
    // Thread pool set to have the number of threads determined by the
    // MATLAB command maxNumCompThreads.  Just 'cause.
    mxArray* n[1];
    mexCallMATLAB(1, n, 0, NULL, "maxNumCompThreads");
    int nthreads = ((int) *mxGetDoubles(n[0]));
    ThreadPool -> nthreads = nthreads;
    
    ThreadPool -> threads = (HANDLE*) mxCalloc(nthreads, sizeof(Thread*));
    mexMakeMemoryPersistent(ThreadPool -> threads);
    
    for (int i = 0; i < nthreads; i++) {
		ThreadPool -> threads[i] = new Thread();
        ThreadPool -> threads[i].start();
        if (ThreadPool -> threads[i] == 0) {
            mexErrMsgIdAndTxt("AddThreadPoolJob:ThreadError", "Error creating thread!");
        }
    }
    ThreadPool -> ThreadPoolMutex->unlock();
    mexAtExit(CleanupMemory);
    
    return ThreadPool;
}


int GetNumThreads()
{
    if (ThreadPool == NULL) {
        InitThreadPool();
    }
    return ThreadPool -> nthreads;
}


// SetThreadPool(prhs[n]);
void SetThreadPool(const mxArray* input)
{
    ThreadPool = (struct ThreadPool*) *mxGetUint64s(input);
}


// plhs[n] = GetThreadPool();
mxArray* GetThreadPool()
{
    if (ThreadPool == NULL) {
        mexErrMsgIdAndTxt("GetThreadPool:InvalidThreadPool", "Thread Pool has yet to be initialized.");
    }
    
    mwSize dims[2] = {1, 1};
    mxArray* output = mxCreateNumericArray(2, &dims[0], mxUINT64_CLASS, mxREAL);
    *mxGetUint64s(output) = (long long int) ThreadPool;
    return output;
}

//===========================================================================
//
//===========================================================================
//
void Push(struct Queue* newnode)
{
    if (QueuePtr == NULL) {
        QueuePtr = (struct Queue*) mxCalloc(1, sizeof(struct Queue));
        mexMakeMemoryPersistent(QueuePtr);
        
        QueuePtr -> front = newnode;
        QueuePtr -> back = newnode;
        QueuePtr -> next = newnode;
        
        SetEvent(ThreadPool -> JobReadyOrCleanup);
    }
    else {
        QueuePtr -> back -> next = newnode;
        QueuePtr -> back = newnode;
        QueuePtr -> next = QueuePtr -> front -> next;
    }
    newnode -> front = QueuePtr -> front;
    newnode -> next = newnode;
    newnode -> back = 0;
}


//
void* Pop()
{
    if (QueuePtr == NULL) {
        mexErrMsgIdAndTxt("Pop:EmptyQueue", "Can't pop, queue is empty!");
    }
    
    struct Queue* temp = QueuePtr -> front;
    void* result = QueuePtr -> front -> data;
    
    if (QueuePtr -> front == QueuePtr -> next) {
        mxFree(QueuePtr);
        QueuePtr = NULL;
    }
    else {
        QueuePtr -> front = QueuePtr -> next;
        QueuePtr -> next = QueuePtr -> front -> next;
        
        SetEvent(ThreadPool -> JobReadyOrCleanup);
    }
    
    if (temp != NULL) {
        mxFree(temp);
    }
        
    return result;
}


//
void FreeQueue()
{
    while (QueuePtr != NULL) {
        void* data = Pop();
        if (data != NULL) {
            mxFree(data);
        }
    }
}


//
void FreeThreadPool()
{
    if (ThreadPool != NULL) {
        mxFree(ThreadPool);
        ThreadPool = NULL;
    }
}


void CleanupMemory()
{
    ThreadPool ->ThreadPoolMutex->lock();
    ThreadPool -> isalive = false;
    SetEvent(ThreadPool -> JobReadyOrCleanup);
    delete ThreadPool -> ThreadPoolMutex;
    
    WaitForMultipleObjects(ThreadPool -> nthreads, ThreadPool -> threads, TRUE, INFINITE);
    for (int i = 0; i < ThreadPool -> nthreads; i++) {
        CloseHandle(ThreadPool -> threads[i]);
    }
    
    delete ThreadPool ->JobFinished;
    delete ThreadPool ->JobReadyOrCleanup;
    delete ThreadPool ->QueueMutex;
    delete ThreadPool ->ThreadPoolMutex;
    
    FreeQueue();
    FreeThreadPool();
}

void SynchronizeThreads()
{
    if (ThreadPool == NULL) {
        return;
    }
    
    while (true) {
        // This is signaled every time a thread finishes a job, allowing
        // us to see how many jobs remain to be completed.
        ThreadPool -> JobFinished->waitFor(INFINITE);
        
        ThreadPool -> ThreadPoolMutex->unlock();
        if (ThreadPool -> jobsremaining == 0) {
            break;
        }
        delete ThreadPool -> ThreadPoolMutex;
    }
    delete ThreadPool -> ThreadPoolMutex;
}

//
unsigned __stdcall ThreadFunction(void* empty)
{
    while (true)
    {
        // JobReadyOrCleanup is signaled when the queue isn't empty 
        // after popping a job off (or pushing one on), AND in order to 
        // clean up memory--otherwise all threads will just sit here.
        ThreadPool -> JobReadyOrCleanup->waitFor(INFINITE);
        
        WaitForSingleObject(ThreadPool -> ThreadPoolMutex, INFINITE);
        if (!(ThreadPool -> isalive)) {
            break;
        }
        ThreadPool -> ThreadPoolMutex->unlock();
        
        WaitForSingleObject(ThreadPool -> QueueMutex, INFINITE);
        struct Job* job;
        if (QueuePtr != NULL) {
            job = Pop();
        }
        ThreadPool -> QueueMutex->unlock();
        
        if (job != NULL) {
            // void jobfunc(void* jobargs) { ... }
            (job -> jobfunc)(job -> jobargs);
            mxFree(job);
            job = NULL;
            
            WaitForSingleObject(ThreadPool -> ThreadPoolMutex, INFINITE);
            ThreadPool -> jobsremaining--;
            SetEvent(ThreadPool -> JobFinished);
            ThreadPool -> ThreadPoolMutex->unlock();
        }
    }
    // Since CleanupMemory only sets this event to signalled once, meaning
    // only one thread exits its infinite loop, each thread also sets this
    // event to signalled so that other threads can exit and end--one at a
    // time.
    SetEvent(ThreadPool -> JobReadyOrCleanup);
    
    ThreadPool -> ThreadPoolMutex->unlock();
    
    _endthreadex(0);
    return 0;
}

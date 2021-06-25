// call_mmap.c - A sample of python embedding 
// (calling python functions 
// from within C code)
#include <Python.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>

#ifdef WIN32    // Windows includes
#include <Windows.h>
#include <process.h>
#define sleep(x) Sleep(1000*x)
HANDLE hFile, handle, map;
#else        // POSIX includes
#include <pthread.h>
#include <sys/mman.h>
pthread_t mythread;
#endif

void myThread(void*);

#define NUM_ARGUMENTS 5
typedef struct 
{
   int argc;
   char *argv[NUM_ARGUMENTS]; 
} CMD_LINE_STRUCT;

int main(int argc, char *argv[])
{
    int i;
    char* indata = NULL;
    CMD_LINE_STRUCT cmd;
    
    cmd.argc = argc;
    for( i = 0; i < NUM_ARGUMENTS; i++ )
    {
        cmd.argv[i] = argv[i];
    }

    if (argc < 4) 
    {
        fprintf(stderr, "Usage: " + 
          "exe_name python_file class_name function_name\n");
        return 1;
    }

    /////////////////////////////////////////////////////////
    // Create a MMAP
    /////////////////////////////////////////////////////////
#ifdef WIN32
    // Create a memory-mapped file (MMF)
    hFile = CreateFile((LPCTSTR) "input.dat", 
        GENERIC_WRITE | GENERIC_READ, 
        FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, 
        OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    
    if(hFile == INVALID_HANDLE_VALUE) 
    {
    // Failed to create file 
    return 1;
    }

    map = CreateFileMapping(hFile, NULL, 
             PAGE_READWRITE, 0, 1024, "MMAPShmem");
    indata = (char *) MapViewOfFile (map, 
             FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0); 
#else
    int fd;
    if((fd = open("input.dat", O_RDWR)) == -1)
    {
        printf("Couldn't open 'input.data'\n");
    }
    indata = mmap(    NULL, 1024, 
        PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
#endif

    if(indata != NULL)
    {    
        printf("Wrapper has created a MMAP " + 
                         "for file 'input.data'\n");
    }
 
    ///////////////////////////////////////////////
    // Create a thread
    ///////////////////////////////////////////////
#ifdef WIN32
    // Windows code
    handle = (HANDLE) _beginthread( myThread, 0, &cmd);
#else
    // POSIX code
    pthread_create( &mythread, NULL, myThread, 
                                         (void*)&cmd );
#endif

    // Random testing code
    for(i = 0; i < 10; i++)
    {
    memset(indata, 0, 1024);
    sprintf(indata, "%d", i);
    indata[3] = '\n';
      printf("The Main thread has writen %d to MMAP.\n", i);
    sleep(1);
    }

    printf("Main thread waiting for " + 
                "Python thread to complete...\n");

    // Join and wait for the created thread to complete...
#ifdef WIN32
    // Windows code
    WaitForSingleObject(handle,INFINITE);
    // Clean up
    UnmapViewOfFile(indata);
    CloseHandle(map);
    CloseHandle(hFile);
#else
    // POSIX code
    pthread_join(mythread, NULL);
    // Clean up
    munmap(indata, 1024);
    close(fd);
#endif
    
    printf("Main thread finished gracefully.\n");
    return 0;
}

void myThread( void *data )
{
    PyObject *pName, *pModule, *pDict, 
                            *pClass, *pInstance;
    PyThreadState *mainThreadState, 
                     *myThreadState, *tempState;
    PyInterpreterState *mainInterpreterState;
    
    CMD_LINE_STRUCT* arg = (CMD_LINE_STRUCT*)data;

    // Initialize python inerpreter
    Py_Initialize();
        
    // Initialize thread support
    PyEval_InitThreads();

    // Save a pointer to the main PyThreadState object
    mainThreadState = PyThreadState_Get();

    // Get a reference to the PyInterpreterState
    mainInterpreterState = mainThreadState->interp;

    // Create a thread state object for this thread
    myThreadState = PyThreadState_New(mainInterpreterState);

    // Swap in my thread state
    tempState = PyThreadState_Swap(myThreadState);

    // Now execute some python code (call python functions)
    pName = PyString_FromString(arg->argv[1]);
    pModule = PyImport_Import(pName);

    // pDict and pFunc are borrowed references 
    pDict = PyModule_GetDict(pModule);

    // Build the name of a callable class 
    pClass = PyDict_GetItemString(pDict, arg->argv[2]);

    // Create an instance of the class
    if (PyCallable_Check(pClass))
    {
    pInstance = PyObject_CallObject(pClass, NULL); 
    }

    // Call a method of the class with no parameters
    PyObject_CallMethod(pInstance, arg->argv[3], NULL);

    // Swap out the current thread
    PyThreadState_Swap(tempState);

    // Clean up thread state
    PyThreadState_Clear(myThreadState);
    PyThreadState_Delete(myThreadState);

    // Clean up
    Py_DECREF(pModule);
    Py_DECREF(pName);

    Py_Finalize();
    printf("My thread is finishing...\n");

    // Exiting the thread
#ifdef WIN32
    // Windows code
    _endthread();
#else
    // POSIX code
    pthread_exit(NULL);
#endif
}


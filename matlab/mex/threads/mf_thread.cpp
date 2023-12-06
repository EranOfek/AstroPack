#include "mf_thread.h"

//
// On Linux, must add and the end of CMakeLists.txt
//
//    TARGET_LINK_LIBRARIES(dro
//      pthread)
//

#ifdef _WIN32
    #include <windows.h>
    #include <thread>
#else
    #include <pthread.h>
    #include <unistd.h>
    #include <sys/time.h>
#endif

#include <iostream>

// Define for debug printouts
//#define _DBG
#ifdef _DBG
#include "mf_perfcounter.h"
#endif

//
bool Thread::_isDebuggerPresent = false;
bool Thread::_isDebuggerPresentInit = false;

// Global
bool linuxIsDebuggerPresent = false;

#ifdef _WIN32
void Thread::initIsDebuggerPresent()
{
	_isDebuggerPresentInit = true;
	_isDebuggerPresent = ::IsDebuggerPresent();
}
#else
void Thread::initIsDebuggerPresent()
{
	_isDebuggerPresentInit = true;

	// Set if from systemConfig
	_isDebuggerPresent = linuxIsDebuggerPresent;
}
#endif

//===========================================================================
//								CritSect
//===========================================================================
CritSect::CritSect()
{
#ifdef _WIN32
	pCriticalSection = new CRITICAL_SECTION;
	InitializeCriticalSection((CRITICAL_SECTION*)pCriticalSection);
#else
	pCriticalSection = (void*)new pthread_mutex_t;
	pthread_mutex_init((pthread_mutex_t*)pCriticalSection, NULL);
#endif
}


CritSect::~CritSect()
{
	unlock();

#ifdef _WIN32
	DeleteCriticalSection((CRITICAL_SECTION*)pCriticalSection);
#else
	pthread_mutex_destroy((pthread_mutex_t*)pCriticalSection);
	delete (pthread_mutex_t*)pCriticalSection;
#endif
}


void CritSect::lock()
{
#ifdef _WIN32
	EnterCriticalSection((CRITICAL_SECTION*)pCriticalSection);
#else
	pthread_mutex_lock((pthread_mutex_t*)pCriticalSection);
#endif
}


void CritSect::unlock()
{
#ifdef _WIN32
	LeaveCriticalSection((CRITICAL_SECTION*)pCriticalSection);
#else
	pthread_mutex_unlock((pthread_mutex_t*)pCriticalSection);
#endif
}

//===========================================================================
//								EventSignal
//===========================================================================
#ifdef _WIN32
EventSignal::EventSignal()
{
	handle = CreateEvent(NULL, FALSE, FALSE, NULL);
}


EventSignal::~EventSignal()
{ 
	CloseHandle(handle);	
}


EventSignalWaitResult EventSignal::waitFor(unsigned int timeoutMs)
{
	EventSignalWaitResult result;
	DWORD Res = WaitForSingleObject(handle, timeoutMs);

	if (Res == WAIT_ABANDONED)
		result = eswrAbandoned;
	else if (Res >= WAIT_OBJECT_0)
		result = eswrSignaled;
	else if (Res == WAIT_TIMEOUT)
		result = eswrTimeout;
	else if (Res == WAIT_FAILED) {
		result = eswrError;
	}
	else result = eswrError;

	return result;
}


void EventSignal::set()
{
	SetEvent(handle);
}


void EventSignal::clear()
{
	ResetEvent(handle);
}
#else	// _WIN32

struct EventSignalData {
	bool signalled;
	pthread_mutex_t mutex;
	pthread_cond_t cond;
};


EventSignal::EventSignal()
{
	EventSignalData* _data = new EventSignalData;
	data = (void*)_data;
	_data->signalled = false;
	pthread_mutex_init(&_data->mutex, NULL);
	pthread_cond_init(&_data->cond, NULL);
}


EventSignal::~EventSignal()
{
	EventSignalData* _data = (EventSignalData*)data;

	pthread_mutex_destroy(&_data->mutex);
	pthread_cond_destroy(&_data->cond);
	delete (EventSignalData*)data;
}


EventSignalWaitResult EventSignal::waitFor(unsigned int timeoutMs)
{
#ifdef _DBG
	std::cout << "EventSignal::waitFor started, timeoutMs: " << timeoutMs << std::endl;
	PerfCounter perf;
#endif

	EventSignalData* _data = (EventSignalData*)data;
	EventSignalWaitResult result;

	// Prepare timeout
	unsigned int toSec = timeoutMs / 1000;
	unsigned int toMs = timeoutMs % 1000;
	struct timespec timeToWait;
	clock_gettime(CLOCK_REALTIME, &timeToWait);
	timeToWait.tv_sec += toSec;
	timeToWait.tv_nsec += 1000000UL * toMs;
	int rt;

	rt = pthread_mutex_lock(&_data->mutex);
	if (rt) std::cout << "EventSignal::waitFor - pthread_mutex_lock failed: " << rt << std::endl;

	rt = pthread_cond_timedwait(&_data->cond, &_data->mutex, &timeToWait);
	if (rt)	std::cout << "EventSignal::waitFor - pthread_cond_timedwait failed: " << rt << std::endl;

	rt = pthread_mutex_unlock(&_data->mutex);
	if (rt) std::cout << "EventSignal::waitFor - pthread_mutex_unlock failed: " << rt << std::endl;

	if (_data->signalled)
		result = eswrSignaled;
	else 
		result = eswrTimeout;

#ifdef _DBG
	double timems = perf.stopms();
	std::cout << "EventSignal::waitFor done, result: " << (int)result << ", perf: " << timems << " ms" << std::endl;
#endif

	return result;
}


void EventSignal::set()
{
#ifdef _DBG
	std::cout << "EventSignal::set" << std::endl;
#endif

	EventSignalData* _data = (EventSignalData*)data;
	int rt;

	rt = pthread_mutex_lock(&_data->mutex);
	if (rt) std::cout << "EventSignal::set - pthread_mutex_lock failed: " << rt << std::endl;

	_data->signalled = true;
	rt = pthread_mutex_unlock(&_data->mutex);
	if (rt) std::cout << "EventSignal::set - pthread_mutex_unlock failed: " << rt << std::endl;

	rt = pthread_cond_signal(&_data->cond);
	if (rt) std::cout << "EventSignal::set - pthread_cond_signal failed: " << rt << std::endl;
}


void EventSignal::clear()
{
#ifdef _DBG
	std::cout << "EventSignal::clear" << std::endl;
#endif

	EventSignalData* _data = (EventSignalData*)data;
	int rt;

	rt = pthread_mutex_lock(&_data->mutex);
	if (rt) std::cout << "EventSignal::clear - pthread_mutex_lock failed: " << rt << std::endl;

	_data->signalled = false;
	rt = pthread_mutex_unlock(&_data->mutex);
	if (rt) std::cout << "EventSignal::clear - pthread_mutex_unlock failed: " << rt << std::endl;

	rt = pthread_cond_signal(&_data->cond);
	if (rt) std::cout << "EventSignal::clear - pthread_cond_signal failed: " << rt << std::endl;
}
#endif  // Linux

//===========================================================================
//									Thread
//===========================================================================

#ifdef _WIN32
// CPU Thread entry point (Windows)
static void thread_entry(Thread* thread)
{
#ifdef _DBG
	std::cout << PerfLog::now() << "Thread entry started" << std::endl;
#endif

	thread->run();

#ifdef _DBG
	std::cout << PerfLog::now() << "Thread entry done" << std::endl;
#endif
}
#else
// Thread entry point (Linux)
static void* thread_entry(void* _thread)
{
	Thread* thread = (Thread*)_thread;
#ifdef _DBG
	std::cout << PerfLog::now() << "Thread entry started" << std::endl;
#endif

	thread->run();

#ifdef _DBG
	std::cout << PerfLog::now() << "Thread entry done" << std::endl;
#endif

	return NULL;
}
#endif


Thread::Thread()
{
#ifdef _DBG
	std::cout << PerfLog::now() << "Thread created" << std::endl;
#endif
}


Thread::Thread(const Thread&) : pthread(NULL)
{
#ifdef _DBG
	std::cout << PerfLog::now() << "Thread copy constructor" << std::endl;
#endif
}


Thread::~Thread()
{
#ifdef _DBG
	std::cout << PerfLog::now() << "Thread destructor" << std::endl;
#endif
}

#ifdef _WIN32
static DWORD WINAPI ThreadFunc(void* data)
{
    thread_entry((Thread*)data);
    return 0;
}
#endif


void Thread::start()
{
	terminated = false;

	// Start new CPU test thread
#ifdef _WIN32
    HANDLE thread = CreateThread(NULL, 0, ThreadFunc, this, 0, NULL);
#elif _WIN32
	std::thread* thread = new std::thread(thread_entry, this);
	pthread = (void*)thread;
	thread->detach();
#else
	pthread_t thread;
	/*int ret =*/ pthread_create(&thread, NULL, thread_entry, (void*)this);
	pthread = (void*)thread;
#endif
}


void Thread::kill()
{
	terminated = true;
	sleep(1000);

#ifdef _WIN32
	//std::thread* thread = (std::thread*)pthread;
#else
	//pthread_t thread = (pthread_t)pthread;
#endif
}


void Thread::terminate()
{
	terminated = true;
}


void Thread::run()
{
	while (!terminated) {

		// Pop front item
		ThreadOper* oper = NULL;
		{
			CritSectLocker Locker(critSect);
			if (!list.empty()) {
				oper = list.front();
				list.pop_front();
			}
		}

		// Execute oper
		if (oper) {
#ifdef _DBG
			std::cout << "*THREAD* " << PerfLog::now() << "Thread::run - oper->run()" << std::endl;
#endif
			oper->run();

#ifdef _DBG
			std::cout << "*THREAD* " << PerfLog::now() << "Thread::run - oper->run() done" << std::endl;
#endif
			delete oper;
		}

		// Might be replaced by event signal
		sleep(1);
	}
}


void Thread::sleep(int ms)
{
    if (ms > 0) {
        #ifdef _WIN32
    	Sleep(ms);
        #else
	    usleep(1000*ms);
        #endif
    }
}


bool Thread::wait(bool* flag, int timeoutMs)
{
	int elapsedMs = 0;
	while (!*flag && (elapsedMs < timeoutMs)) {
		Thread::sleep(1);
		elapsedMs++;
	}
	return *flag;
}

//===========================================================================
//									ThreadPool 
//===========================================================================
#ifdef never

ThreadPool::ThreadPool()
{
#ifdef _DBG
	std::cout << PerfLog::now() << "ThreadPool created" << std::endl;
#endif
}


ThreadPool::ThreadPool(const ThreadPool&)
{
#ifdef _DBG
	std::cout << PerfLog::now() << "ThreadPool copy constructor" << std::endl;
#endif
}


ThreadPool::~ThreadPool()
{
#ifdef _DBG
	std::cout << PerfLog::now() << "ThreadPool destructor" << std::endl;
#endif
}

#endif

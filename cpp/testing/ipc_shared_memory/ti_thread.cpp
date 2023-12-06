
#include "opencv2/core/ti_thread.hpp"
#include "opencv2/core/ti_perfcounter.hpp"

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

static TiThread* backgroundThread = NULL;

// Return pointer to background thread
TiThread* backthGetThread()
{
	if (!backgroundThread) {
		backgroundThread = new TiThread;
		backgroundThread->start();
	}

	return backgroundThread;
}


void backthAddOper(TiThreadOper* oper)
{
	backthGetThread()->addOper(oper);
}

//===========================================================================
//								TiCritSect
//===========================================================================
TiCritSect::TiCritSect()
{
#ifdef _WIN32
	pCriticalSection = new CRITICAL_SECTION;
	InitializeCriticalSection((CRITICAL_SECTION*)pCriticalSection);
#else
	pCriticalSection = (void*)new pthread_mutex_t;
	pthread_mutex_init((pthread_mutex_t*)pCriticalSection, NULL);
#endif
}


TiCritSect::~TiCritSect()
{
	unlock();

#ifdef _WIN32
	DeleteCriticalSection((CRITICAL_SECTION*)pCriticalSection);
#else
	pthread_mutex_destroy((pthread_mutex_t*)pCriticalSection);
	delete (pthread_mutex_t*)pCriticalSection;
#endif
}


void TiCritSect::lock()
{
#ifdef _WIN32
	EnterCriticalSection((CRITICAL_SECTION*)pCriticalSection);
#else
	pthread_mutex_lock((pthread_mutex_t*)pCriticalSection);
#endif
}


void TiCritSect::unlock()
{
#ifdef _WIN32
	LeaveCriticalSection((CRITICAL_SECTION*)pCriticalSection);
#else
	pthread_mutex_unlock((pthread_mutex_t*)pCriticalSection);
#endif
}

//===========================================================================
//								TiEventSignal
//===========================================================================
#ifdef _WIN32
TiEventSignal::TiEventSignal()
{
	handle = CreateEvent(NULL, FALSE, FALSE, NULL);
}


TiEventSignal::~TiEventSignal() 
{ 
	CloseHandle(handle);	
}


TiEventSignalWaitResult TiEventSignal::waitFor(unsigned int timeoutMs)
{
	TiEventSignalWaitResult result;
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


void TiEventSignal::set()
{
	SetEvent(handle);
}


void TiEventSignal::clear()
{
	ResetEvent(handle);
}
#else	// _WIN32

struct TiEventSignalData {
	bool signalled;
	pthread_mutex_t mutex;
	pthread_cond_t cond;
};


TiEventSignal::TiEventSignal()
{
	TiEventSignalData* _data = new TiEventSignalData;
	data = (void*)_data;
	_data->signalled = false;
	pthread_mutex_init(&_data->mutex, NULL);
	pthread_cond_init(&_data->cond, NULL);
}


TiEventSignal::~TiEventSignal()
{
	TiEventSignalData* _data = (TiEventSignalData*)data;

	pthread_mutex_destroy(&_data->mutex);
	pthread_cond_destroy(&_data->cond);
	delete (TiEventSignalData*)data;
}


TiEventSignalWaitResult TiEventSignal::waitFor(unsigned int timeoutMs)
{
#ifdef _DBG
	std::cout << "TiEventSignal::waitFor started, timeoutMs: " << timeoutMs << std::endl;
	PerfCounter perf;
#endif

	TiEventSignalData* _data = (TiEventSignalData*)data;
	TiEventSignalWaitResult result;

	// Prepare timeout
	unsigned int toSec = timeoutMs / 1000;
	unsigned int toMs = timeoutMs % 1000;
	struct timespec timeToWait;
	clock_gettime(CLOCK_REALTIME, &timeToWait);
	timeToWait.tv_sec += toSec;
	timeToWait.tv_nsec += 1000000UL * toMs;
	int rt;

	rt = pthread_mutex_lock(&_data->mutex);
	if (rt) std::cout << "TiEventSignal::waitFor - pthread_mutex_lock failed: " << rt << std::endl;

	rt = pthread_cond_timedwait(&_data->cond, &_data->mutex, &timeToWait);
	if (rt)	std::cout << "TiEventSignal::waitFor - pthread_cond_timedwait failed: " << rt << std::endl;

	rt = pthread_mutex_unlock(&_data->mutex);
	if (rt) std::cout << "TiEventSignal::waitFor - pthread_mutex_unlock failed: " << rt << std::endl;

	if (_data->signalled)
		result = eswrSignaled;
	else 
		result = eswrTimeout;

#ifdef _DBG
	double timems = perf.stopms();
	std::cout << "TiEventSignal::waitFor done, result: " << (int)result << ", perf: " << timems << " ms" << std::endl;
#endif

	return result;
}


void TiEventSignal::set()
{
#ifdef _DBG
	std::cout << "TiEventSignal::set" << std::endl;
#endif

	TiEventSignalData* _data = (TiEventSignalData*)data;
	int rt;

	rt = pthread_mutex_lock(&_data->mutex);
	if (rt) std::cout << "TiEventSignal::set - pthread_mutex_lock failed: " << rt << std::endl;

	_data->signalled = true;
	rt = pthread_mutex_unlock(&_data->mutex);
	if (rt) std::cout << "TiEventSignal::set - pthread_mutex_unlock failed: " << rt << std::endl;

	rt = pthread_cond_signal(&_data->cond);
	if (rt) std::cout << "TiEventSignal::set - pthread_cond_signal failed: " << rt << std::endl;
}


void TiEventSignal::clear()
{
#ifdef _DBG
	std::cout << "TiEventSignal::clear" << std::endl;
#endif

	TiEventSignalData* _data = (TiEventSignalData*)data;
	int rt;

	rt = pthread_mutex_lock(&_data->mutex);
	if (rt) std::cout << "TiEventSignal::clear - pthread_mutex_lock failed: " << rt << std::endl;

	_data->signalled = false;
	rt = pthread_mutex_unlock(&_data->mutex);
	if (rt) std::cout << "TiEventSignal::clear - pthread_mutex_unlock failed: " << rt << std::endl;

	rt = pthread_cond_signal(&_data->cond);
	if (rt) std::cout << "TiEventSignal::clear - pthread_cond_signal failed: " << rt << std::endl;
}
#endif  // Linux

//===========================================================================
//									TiThread
//===========================================================================

#ifdef _WIN32
// CPU Thread entry point (Windows)
static void thread_entry(TiThread* thread)
{
#ifdef _DBG
	std::cout << PerfLog::now() << "TiThread entry started" << std::endl;
#endif

	thread->run();

#ifdef _DBG
	std::cout << PerfLog::now() << "TiThread entry done" << std::endl;
#endif
}
#else
// Thread entry point (Linux)
static void* thread_entry(void* _thread)
{
	TiThread* thread = (TiThread*)_thread;
#ifdef _DBG
	std::cout << PerfLog::now() << "TiThread entry started" << std::endl;
#endif

	thread->run();

#ifdef _DBG
	std::cout << PerfLog::now() << "TiThread entry done" << std::endl;
#endif

	return NULL;
}
#endif


TiThread::TiThread()
{
#ifdef _DBG
	std::cout << PerfLog::now() << "TiThread created" << std::endl;
#endif
}


TiThread::TiThread(const TiThread&) : pthread(NULL)
{
#ifdef _DBG
	std::cout << PerfLog::now() << "TiThread copy constructor" << std::endl;
#endif
}


TiThread::~TiThread()
{
#ifdef _DBG
	std::cout << PerfLog::now() << "TiThread destructor" << std::endl;
#endif
}


void TiThread::start()
{
	terminated = false;

	// Start new CPU test thread
#ifdef _WIN32
	std::thread* thread = new std::thread(thread_entry, this);
	pthread = (void*)thread;
	thread->detach();
#else
	pthread_t thread;
	/*int ret =*/ pthread_create(&thread, NULL, thread_entry, (void*)this);
	pthread = (void*)thread;
#endif
}


void TiThread::kill()
{
	terminated = true;
	sleep(1000);

#ifdef _WIN32
	//std::thread* thread = (std::thread*)pthread;
#else
	//pthread_t thread = (pthread_t)pthread;
#endif
}


void TiThread::terminate()
{
	terminated = true;
}


void TiThread::run()
{
	while (!terminated) {

		// Pop front item
		TiThreadOper* oper = NULL;
		{
			TiCritSectLocker Locker(critSect);
			if (!list.empty()) {
				oper = list.front();
				list.pop_front();
			}
		}

		// Execute oper
		if (oper) {
#ifdef _DBG
			std::cout << "*THREAD* " << PerfLog::now() << "TiThread::run - oper->run()" << std::endl;
#endif
			oper->run();

#ifdef _DBG
			std::cout << "*THREAD* " << PerfLog::now() << "TiThread::run - oper->run() done" << std::endl;
#endif
			delete oper;
		}

		// Might be replaced by event signal
		sleep(1);
	}
}


void TiThread::sleep(int ms)
{
#ifdef _WIN32
	Sleep(ms);
#else
	usleep(1000*ms);
#endif
}


bool TiThread::wait(bool* flag, int timeoutMs)
{
	int elapsedMs = 0;
	while (!*flag && (elapsedMs < timeoutMs)) {
		TiThread::sleep(1);
		elapsedMs++;
	}
	return *flag;
}


bool TiThread::isDebuggerPresent()
{
#ifdef _WIN32
	return IsDebuggerPresent();
#else
	return false;
#endif
}


void TiThread::addOper(TiThreadOper* oper)
{
	TiCritSectLocker locker(critSect);
	list.push_back(oper);
}


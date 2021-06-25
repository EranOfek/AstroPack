

#ifndef OPENCV_CORE_TI_THREAD_HPP
#define OPENCV_CORE_TI_THREAD_HPP

#include "opencv2/core.hpp"
#include <stdlib.h>
#include <list>


// Simple CriticalSection
struct CV_EXPORTS_W TiCritSect {

	// Constructor
	TiCritSect();

	// Destructor
	~TiCritSect();

	// Lock
	void lock();

	// Unlock
	void unlock();

	// Pointer to O/S object
	void* pCriticalSection;			
};


// Auto locker for TiCritSect
class CV_EXPORTS_W TiCritSectLocker {
public:
	// Constructor - lock
	TiCritSectLocker(TiCritSect& _critSect, bool _lock = true) : critSect(_critSect), lock(_lock) { if (lock) critSect.lock(); }

	// Destructor - unlock
	~TiCritSectLocker() { if (lock) critSect.unlock(); }

private:
	TiCritSect& critSect;
	bool lock;
};

//===========================================================================

enum TiEventSignalWaitResult { eswrNone=0 , eswrSignaled, eswrTimeout, eswrAbandoned, eswrError };

// Simple event signal - Windows version
class CV_EXPORTS_W TiEventSignal {
public:
	// Constructor
	TiEventSignal();

	// Destructor
	~TiEventSignal();

	// For for specified timeout, return 
	TiEventSignalWaitResult waitFor(unsigned int timeoutMs);

	// Set signal
	void set();

	// Clear signal
	void clear();

#ifdef _WIN32
	void* handle;
#else
	void* data;
#endif

};

//===========================================================================
//									TiThreadOper
//===========================================================================
// Base class for thread operation
class CV_EXPORTS_W TiThreadOper {
public:

	// Constructor
	TiThreadOper() {}

	// Copy constructor - required by std::vector
	TiThreadOper(const TiThreadOper&) {}

	// Destructor
	virtual ~TiThreadOper() {}

	// Required by std::vector
	TiThreadOper& operator= (const TiThreadOper&)
	{
		return *this;
	}

	// Execute the operation
	virtual void run() {}

};

//===========================================================================
//								TiThread
//===========================================================================
// Simple thread that executes background operations
class CV_EXPORTS_W TiThread {
public:

	// Default constructor
	TiThread();

	// Copy constructor
	TiThread(const TiThread&);

	// Destructor - release 
	~TiThread();

	// Required by std::vector
	TiThread& operator= (const TiThread&)
	{
		return *this;
	}

	// Start the thread
	void start();

	// Kill thread and release resources
	void kill();

	// Signal thread to terminate
	void terminate();

	// Thread worker function
	void run();

	// Sleep for milliseconds
	static void sleep(int ms);

	// Simple wait until flag is set or timeout has elapsed
	// Time is measured roughly using sleep(1)
	static bool wait(bool* flag, int timeoutMs);

	// Return true if running under debugger
	static bool isDebuggerPresent();

	// Add new operation to list
	void addOper(TiThreadOper* oper);

	std::list<TiThreadOper*> list;		// List of TiThreadOper or derived
	bool terminated;

	void* pthread;
	TiCritSect critSect;
};

//===========================================================================

// Return pointer to background thread
CV_EXPORTS_W TiThread* backthGetThread();

// Add new operation to list
CV_EXPORTS_W void backthAddOper(TiThreadOper* oper);


#endif

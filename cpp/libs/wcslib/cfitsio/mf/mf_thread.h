#ifndef mf_threadH
#define mf_threadH

//
// Note that under Linux you have to link with 'pthread' library
// CMake:
// TARGET_LINK_LIBRARIES(app
//    ...
//    pthread)
//

#include <stdlib.h>
#include <list>


// Simple Critical-Section
struct CritSect {

	// Constructor
	CritSect();

	// Destructor
	~CritSect();

	// Lock
	void lock();

	// Unlock
	void unlock();

	// Pointer to O/S object
	void* pCriticalSection;			
};


// Auto locker for CritSect
class CritSectLocker {
public:
	// Constructor - lock
	CritSectLocker(CritSect& _critSect, bool _lock = true) : 
		critSect(&_critSect)
	{ 
		if (critSect)
			critSect->lock(); 
	}

	// Constructor - lock, it specified poinetr is NULL, it does nothing
	CritSectLocker(CritSect* _critSect) : 
		critSect(_critSect)
	{ 
		if (critSect)
			critSect->lock(); 
	}

	// Destructor - unlock
	~CritSectLocker() 
	{ 
		if (critSect)
			critSect->unlock(); 
	}

private:
	CritSect* critSect;
};

//===========================================================================

// Event signal result
enum EventSignalWaitResult { 
	eswrNone=0,			//
	eswrSignaled,		//
	eswrTimeout,		//
	eswrAbandoned,		//
	eswrError };		//


// Simple event signal - Windows version
class EventSignal {
public:
	// Constructor
	EventSignal();

	// Destructor
	~EventSignal();

	// Wait for for specified timeout in milliseconds
	EventSignalWaitResult waitFor(unsigned int timeoutMs);

	// Set signal
	void set();

	// Clear signal
	void clear();

#ifdef _WIN32
	void* handle;		// Handle to Windows object
#else
	void* data;			// Handle to Linux object
#endif

};


// Base class for thread operation
class ThreadOper {
public:

	// Constructor
	ThreadOper() {}

	// Copy constructor - required by std::vector
	ThreadOper(const ThreadOper&) {}

	// Destructor
	virtual ~ThreadOper() {}

	// Required by std::vector
	ThreadOper& operator= (const ThreadOper&)
	{
		return *this;
	}

	// Execute the operation
	virtual void run() {}
};


// Simple thread that executes background operations by derived class
class Thread {
public:

	// Constructor
	Thread();

	// Copy constructor
	Thread(const Thread&);

	// Destructor - release 
	~Thread();

	// Required by std containers
	Thread& operator= (const Thread&)
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
	virtual void run();

	// Sleep for milliseconds
	static void sleep(int ms);

	// Simple wait until flag is set or timeout has elapsed
	// Time is measured roughly using sleep(1)
	static bool wait(bool* flag, int timeoutMs);

	// Return true if running under debugger
	static bool isDebuggerPresent()
	{
		if (!_isDebuggerPresentInit) 
			initIsDebuggerPresent();

		return _isDebuggerPresent;
	}

	// Initialize _isDebuggerPresent
	static void initIsDebuggerPresent();

	// Return true if build with debug or release
	static bool isDebugBuild()
	{
		// For Linux, need to run: cmake -DCMAKE_BUILD_TYPE=Debug ...
#ifdef _DEBUG
		return true;
#else
		return false;
#endif
	}

	// Add new operation to list
	void addOper(ThreadOper* oper)
	{
		CritSectLocker locker(critSect);
		list.push_back(oper);
	}

	std::list<ThreadOper*> list;		// List of TiThreadOper or derived
	bool terminated;					//
	void* pthread;						//
	CritSect critSect;					//
	static bool _isDebuggerPresent;		// Set on startup
	static bool _isDebuggerPresentInit;	// Set on startup
};

#endif

#ifndef mf_componentH
#define mf_componentH

#include "mf_def.h"
#include "mf_thread.h"

// Optional thread if required by component
class ComponentThread;

// Exception class
class Exception {
public:
	// Constructor
    Exception(const std::string& _message) :
        message(_message) {}

    std::string message;		// Message text
};


// Base class for all components
// Support log and access to configuration data, and optional thread
class Component {
public:
	// Constructor
	Component() : debugMode(true), logColor(0), thread(NULL), pCritSect(NULL), threadInterval(1) {}

	// Destructor
	virtual ~Component();

    // Start thread with specified sleep interval [milliseconds]
	virtual bool startThread(int threadInterval = 1);

	// Worker function called from thread
	virtual void threadProcess() {}

    // Set log color
    virtual void setLogColor(int _color) { logColor = _color; }

	// Log message, with optional text color
	virtual void log(const std::string& msg, int color = 0);

    // Log message with format
    virtual void logf(const char* format, ...);

    // Log exception
    virtual void logEx(Exception& ex, const std::string& msg = "");

    // Data
    bool	debugMode;				// True for debug mode
    int		logColor;               // Current log color
	int		threadInterval;         // Thread sleep interval [milliseconds]
    ComponentThread* thread;		// Optional thread
    CritSect* pCritSect;			// Optional critical-section when using thread
};


// Component Thread
class ComponentThread : public Thread {
public:
    // Constructor
    ComponentThread(Component* _comp) : Thread(), comp(_comp) {}

	// Destructor
	virtual ~ComponentThread() {}

	// Thread worker function, calls Component::threadProcess()
	virtual void run();

    // Data
    Component* comp;        // Pointer to owner compoenent
};

#endif

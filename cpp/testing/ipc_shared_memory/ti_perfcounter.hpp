
#ifndef TI_CORE_PERFCOUNTER_HPP
#define TI_CORE_PERFCOUNTER_HPP

// Set when compiling with OpenCV
#define HAVE_OPENCV

#ifdef HAVE_OPENCV
#include "opencv2/core.hpp"
#else
#define CV_EXPORTS_W
#endif


#include <iostream>
#include <sstream>

#ifdef _WIN32
#define PERFCOUNTER_HAVE_CHRONO
#elif __cplusplus >= 201103L
#define PERFCOUNTER_HAVE_CHRONO
#endif

#ifdef PERFCOUNTER_HAVE_CHRONO
#include <chrono>
#endif

// Simple performance counter to measure timing in microseconds resolution
class PerfCounter {
public:

#ifdef PERFCOUNTER_HAVE_CHRONO
	typedef std::chrono::high_resolution_clock::time_point Time;
#else
	typedef long long Time;
#endif

	PerfCounter() : microseconds(0) { start(); }

	// Reset counter
	void start() {
#ifdef PERFCOUNTER_HAVE_CHRONO
		startTime = std::chrono::high_resolution_clock::now();
		stopTime = startTime;
#else
		startTime = 0;
		stopTime = 0;
#endif
	}

	// Stop and auto-restart counter, calculates elapsed time since start
	// Returns microseconds since start
	long long stop() {
#ifdef PERFCOUNTER_HAVE_CHRONO
		stopTime = std::chrono::high_resolution_clock::now();
		microseconds = std::chrono::duration_cast<std::chrono::microseconds>(stopTime - startTime).count();
#else
		stopTime = 0;
		microseconds = 0;
#endif
		start();
		return microseconds;
	}

	// Stop as milliseconds
	double stopms() { return (double)stop() / 1000.0; }
		
	// Returns elapsed time
	long long get() { return microseconds; }


	Time startTime;
	Time stopTime;
	long long microseconds;
};


// Performance logger
class CV_EXPORTS_W PerfCounterLog {
public:

	// Constructor, optionally log "started" on beginning
	PerfCounterLog(const std::string& _caption, bool logStarted = false);

	// Destructor
	~PerfCounterLog();

	// Start
	void start(const std::string& _caption, bool logStarted = false);

	// Stop
	void stop();

	// Log elapsed time since started or since last log()
	void log(const std::string& text);

	// Set enabled
	static void setEnabled(bool _enabled, bool _dumpToFile, bool _dumpToConsole);

	// Set useStream
	static void setUseStream(bool _useStream);

	// Dump internal stream to std::cout
	static void dump();

	// Clear internal stream
	static void clear();

	// Get stream
	static std::ostringstream& getStream();

	// Get current time as hh:nn:ss.mil
	static std::string now();

	// Perform self performance test
	static void selfTest();
	
	// Members
	PerfCounter perf;
	PerfCounter::Time startTime;		// Save perf.startTime
	std::string caption;
	bool isSection;						// True when caption starts with "---"
	bool started;						// Set by start(), cleared by stop()

	// Static
	static bool enabled;				// True to enable the logger
	static bool dumpToConsole;			// True - dump to console
	static bool dumpToFile;				// True - dump to file
	static bool useStream;				// True to use internal string-stream until dump()
	static std::string fileName;		// Log file name, used by 
	static std::ostringstream stream;	// Used when useStream = true
};

// Alias
typedef PerfCounterLog PerfLog;


// PerfLog with bytes-per-second
class CV_EXPORTS_W PerfLogBytesPerSec {
public:

	// Constructor
	PerfLogBytesPerSec(const std::string& _caption) : caption(_caption) {}

	// Print results
	void stop(long long bytes)
	{
		double stopms = perf.stopms();
		double mbps = ((double)bytes / (1024.0*1024.0)) / (stopms / 1000.0);
		double mbpms = mbps / 1000.0;
		std::cout << caption.c_str() << ": " << stopms << " ms = " << 
			mbps << " MB/sec = " << mbpms << " MB/ms" << std::endl;
	}

	PerfCounter perf;
	std::string caption;
};



// Utility class to count loop iterations
class CV_EXPORTS_W PerfLogLoop {
public:
    // Constructor
    PerfLogLoop(const std::string& _text) : enabled(true), perf(_text), it(0) {}

    // Destructor - log
    ~PerfLogLoop()
    {
        if (enabled) {
            char s[512];
            sprintf(s, "%s - Iters: %u", perf.caption.c_str(), it);
            perf.log(s);
        }
    }

    // Increment counter
    void iter() { it++; }

    bool enabled;
    PerfLog perf;
    uint it;
};


namespace cv {

// Set PerfLog::enabled
void CV_EXPORTS_W perfLogSetEnabled(bool enabled, bool dumpToFile = true, bool dumpToConsole = true);

// Dump log
void CV_EXPORTS_W perfLogDump();

}  // cv

#endif

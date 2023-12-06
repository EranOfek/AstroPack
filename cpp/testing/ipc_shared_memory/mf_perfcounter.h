#ifndef mf_perfcounterH
#define mf_perfcounterH

#include <iostream>
#include <sstream>

// Starting from C++11 we have std::chrono, otherwise we use our partial implementation
#define PERFCOUNTER_HAVE_CHRONO
#ifdef PERFCOUNTER_HAVE_CHRONO
    #include <chrono>
#endif

// Forward declaration, see LogFile.h
class LogFile;

// Simple performance counter to measure timing in microseconds resolution
class PerfCounter {
public:

#ifdef PERFCOUNTER_HAVE_CHRONO
	// Time is int64_t
	typedef std::chrono::high_resolution_clock::time_point Time;
#else
	typedef int64_t Time;
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
	int64_t stop() {
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
		
	// Returns elapsed time in microseconds
	int64_t get() { return microseconds; }

	Time startTime;			// Start time
	Time stopTime;			// Stop time
	int64_t microseconds;	// Elapsed microseconds, set by stop()
};


// Performance logger
class PerfLog {
public:

	// Constructor with per-iteration calculation
	PerfLog(const std::string& _caption, uint64_t _count = 0);

	// Constructor, optionally log "started" on beginning
	PerfLog(LogFile* _logfile, const std::string& _caption, bool logStarted = false);
	PerfLog(LogFile& _logfile, const std::string& _caption, bool logStarted = false);

	// Destructor
	~PerfLog() { stop(); }

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

	// Get current time as "dd/mm/yyyy hh:nn:ss.mil"
	static std::string now();

	// Perform self performance test
	static void selfTest();
	
	// Members
	PerfCounter			perf;			//
	PerfCounter::Time	startTime;		// Save perf.startTime
	std::string			caption;		//
	bool				isSection;		// True when caption starts with "---"
	bool				started;		// Set by start(), cleared by stop()
	uint64_t			count;			// Number of iterations
	LogFile*			logfile;		// Pointer to logfile

	// Static data
	static bool enabled;				// True to enable the logger
	static bool dumpToConsole;			// True - dump to console
	static bool dumpToFile;				// True - dump to file
	static bool useStream;				// True to use internal string-stream until dump()
	static bool debugBuild;				// True on debug build, false on release
	static std::string fileName;		// Log file name, used by 
	static std::ostringstream stream;	// Used when useStream = true
};


// PerfLog with bytes-per-second
class PerfLogBytesPerSec {
public:

	// Constructor
	PerfLogBytesPerSec(const std::string& _caption) : caption(_caption) {}

	// Print results
	void stop(int64_t bytes)
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
class PerfLogLoop {
public:
    // Constructor
    PerfLogLoop(const std::string& _text) : enabled(true), perf(_text), it(0) {}

    // Destructor - log
    ~PerfLogLoop()
    {
        if (enabled) {
            char s[512];
            sprintf(s, "%s - Iters: %lld", perf.caption.c_str(), (long long int)it);
            perf.log(s);
        }
    }

    // Increment counter
    void iter() { it++; }

    bool			enabled;
    PerfLog			perf;
	int64_t		    it;
};

#endif

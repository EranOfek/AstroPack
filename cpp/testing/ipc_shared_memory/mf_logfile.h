#ifndef mf_logfileH
#define mf_logfileH

#include "mf_def.h"
#include "mf_file.h"
#include "mf_thread.h"
#include "mf_perfcounter.h"
#include <algorithm>


// Colors, relevent only for GUI version
enum TColor { clBlack=0, clBlue, clRed, clGreen, clMaroon, clPurple, clFuchsia };

// Callback function, optional to allow directing the log message
typedef void (*LogFileCallback)(const std::string& msg, const std::string& timestr, int color);

//
extern bool globalQuietMode;

// Log file with optional thread (to reduce overhead)
class  LogFile {
public:

	// Constructor
	LogFile() : callback(NULL), print(false) {}

    // Constructor
    LogFile(const std::string& _filename, const std::string& _prefix = "", LogFileCallback _callback = NULL, bool logStart = false);

	// Destructor
    ~LogFile();

	// Initialize log file
	void init(const std::string& _filename, const std::string& _prefix = "", LogFileCallback _callback = NULL, bool logStart = false);

	// Write message to log
	void log(const std::string& msg, int color = 0);

    // Write message lto log
    void logf(const char* format, ...);

	// Dump data buffer
    void dump(void* data, int len, const std::string& msg);

	// Flush log to file
	void flush();

    // Dump data buffer to string
	static std::string dumpStr(void* data, int len, const std::string& msg);

	//
	std::string			prefix;		    // Line prefix
	LogFileCallback		callback;       // Optional callback function
    CritSect      		critSect;       // Thread-safe locker
	bool				print;			// True for console print

#ifdef _WIN32
	FileWin				fp;
#else
	FileLinux			fp;
#endif
};


// Default global system logs
void mfLog(const std::string& str, int color = clBlack);

// Default log files
extern LogFile* sysLogFile;
extern LogFile* errLogFile;


// LogFile stream wrapper
class LogStream {
public:

	// Constrctor
	LogStream(LogFile* _logfile) : logfile(_logfile) {}
	
	// Destructor
	virtual ~LogStream() {}

	// Log string
	void logStr(char* sz);

	// Log string
	void logStream(char* sz);

	// Set optional prompt
	void setPrompt(char* sz);

	// Operators
	virtual LogStream& operator<<(char const* str);
	virtual LogStream& operator<<(int i);
	virtual LogStream& operator<<(unsigned u);
	virtual LogStream& operator<<(int64_t i64);
	virtual LogStream& operator<<(double d);
	virtual LogStream& operator<<(void* p);

	//virtual LogStream& operator<<(TColor _color);
	//void setDefaultColor(TColor _color = clBlack);

	//
	LogFile* logfile;

	//int defaultColor;
	//int 			color;
	//char 			szPrompt[128];

};


// Thread-safe list of LogFiles
class LogFilesList {
public:
	// Constructor
	LogFilesList() {}

	// Destructor, flush logs
	~LogFilesList() { flushAll(); }

	// Add logfile to list
	void add(LogFile* logfile)
	{
		CritSectLocker locker(critSect);
		list.push_back(logfile);
	}

	// Flush all logfiles
	void flushAll()
	{
		PerfLog perf("LogFilesList::flush");

		CritSectLocker locker(critSect);
		for (auto it : list) {
			LogFile* logfile = it;
			logfile->fp.flushWriteQueue();
		}
	}
	
	// Flush specified log file
	void flush(LogFile* logfile)
	{
		CritSectLocker locker(critSect);
		logfile->fp.flushWriteQueue();
	}

	// Flush specified log file
	void flushAndRemove(LogFile* logfile)
	{
		CritSectLocker locker(critSect);
		logfile->fp.flushWriteQueue();
		auto it = std::find(list.begin(), list.end(),logfile);
		if (it != list.end()) {
			list.erase(it);
		}
	}

	std::vector<LogFile*> list;
	CritSect critSect;
};


// Global LogFiles thread
class LogFileThread : public Thread {
public:
	// Constructor
	LogFileThread(LogFilesList* _list, int _flushInterval) :
		list(_list), flushInterval(_flushInterval) {}

	// Destructor
	virtual ~LogFileThread() {}

	// Thread worker function
	virtual void run()
	{
		while (!terminated) {
			Thread::sleep(flushInterval);
			list->flushAll();
		}
	}

	LogFilesList* list;
	int flushInterval;
};


// Auto-flush wrapper
class FlushLogFilesOnExit {
public:
	// Constructor
	FlushLogFilesOnExit(LogFilesList* _list, int _flushInterval) : 
		thread(_list, _flushInterval) { thread.start(); }

	// Destructor - flush all log files
	~FlushLogFilesOnExit() { flush(); }

	// Flush all log files
	void flush() { thread.list->flushAll(); }

	LogFileThread thread;
};

// Flush all log files
void flushLogFiles();

#endif

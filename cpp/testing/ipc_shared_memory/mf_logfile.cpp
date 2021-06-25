#include "mf_def.h"
#include "mf_time.h"
#include "mf_system.h"
#include "mf_logfile.h"
#include "mf_thread.h"
#include "mf_perfcounter.h"
#include <iostream>

#ifndef _WIN32
#include <stdarg.h>
#endif

bool globalQuietMode = false;

// Default log files
LogFile* sysLogFile = NULL;

// List with auto-flush on termination
LogFilesList logFilesList;

// This call creates a thread
FlushLogFilesOnExit flushLogFilesOnExit(&logFilesList, 10000);

void flushLogFiles()
{
	flushLogFilesOnExit.flush();
}
//===========================================================================
LogFile::LogFile(const std::string& _filename, const std::string& _prefix, LogFileCallback _callback, bool logStart) :
	callback(_callback), print(false)
{
	init(_filename, _prefix, _callback, logStart);
}


LogFile::~LogFile()
{
	logFilesList.flushAndRemove(this);
}


void LogFile::init(const std::string& _filename, const std::string& _prefix, LogFileCallback _callback, bool logStart)
{
    // Optional thread to reduce I/O overhead
	fp.filename = _filename;
	fp.useWriteQueue = true;
	logFilesList.add(this);

    // Log start message
    if (logStart)
        log("*** LOG STARTED ***");
}


void LogFile::log(const std::string& msg, int color)
{
	// Nothing to do if log file is not initialized
	//if (fp.filename.empty())
	//	return;

    // Thread-safe locker
    CritSectLocker locker(critSect);

	// Empty line, log without prefix
	if (msg.size() == 0) {
		if (!fp.filename.empty())
			fp.openAppendClose(fp.filename, "\n");

		if (print && (color > -1) && !globalQuietMode) {
			std::cout << "\n";
		}
		return;
	}

    // Get current timestamp string
    std::string timestr = mfNowStr(true);

	// Optional callback
	if (callback) {
		callback(prefix + msg, timestr, color);
	}

    // Prepare line and append to file write-queue
    std::string str = prefix + timestr + std::string(" ") + msg + "\n";
	if (!fp.filename.empty())
		fp.openAppendClose("", str);

	// Console print
	if (print && (color > -1) && !globalQuietMode) {
		std::cout << str;
	}
}


void LogFile::logf(const char* format, ...)
{
    char s[65536];
    va_list paramList;
    va_start(paramList, format);
	vsprintf(s, format, paramList);
    log(s);
    va_end(paramList);
}


void LogFile::dump(void* data, int len, const std::string& msg)
{
    std::string str = dumpStr(data, len, msg);
    log(str);
}


void LogFile::flush()
{
	// Thread-safe locker
	CritSectLocker locker(critSect);
	fp.flushWriteQueue();

	// @Todo: do we need it here?
	// Close file
	// fp.close();
}


std::string LogFile::dumpStr(void* data, int len, const std::string& msg)
{
    // Limit buffer length
    if (len > 1000)
        len = 1000;

    // Convert bytes to hex representation
    std::string str = msg;
    static const char Hex[] = "0123456789ABCDEF";
    char sz[] = { 0, 0, ' ', 0 };
    const uint8_t* p = (const uint8_t*)data;
    for (int i=0;  i < len;  i++) {
        sz[0] = Hex[*p >> 4 ];
        sz[1] = Hex[*p & 0x0F ];
        str += std::string(sz);
        p++;
    }
    return str;
}


void mfLog(const std::string& str, int color)
{
	if (sysLogFile) {
		sysLogFile->log(str, color);
	}
}

//===========================================================================
LogStream& LogStream::operator<<(char const* sz)
{
	logStr((char*)sz);
	return *this;
}


LogStream& LogStream::operator<<(int i)
{
	char sz[256];
	sprintf(sz, "%d", i);
	logStr(sz);
	return *this;
}


LogStream& LogStream::operator<<(unsigned u)
{
	char sz[256];
	sprintf(sz, "%u (0x%08X)", u, u);
	logStr(sz);
	return *this;
}


LogStream& LogStream::operator<<(int64_t i64)
{
	char sz[256];
	sprintf(sz, "%lld", (long long int)i64);
	logStr(sz);
	return *this;
}


LogStream& LogStream::operator<<(double d)
{
	char sz[256];
	sprintf(sz, "%0.6lf", d);
	logStr(sz);
	return *this;
}


LogStream& LogStream::operator<<(void* p)
{
	char sz[256];
	sprintf(sz, "%p", p);
	logStr(sz);
	return *this;
}

#ifdef never
LogStream& LogStream::operator<<(TColor _color)
{
	color = _color;
	return *this;
}


void LogStream::setPrompt(char* sz)
{
	strncpy(szPrompt, sz, sizeof(szPrompt) - 1);
	szPrompt[sizeof(szPrompt) - 1] = 0;
}


void LogStream::setDefaultColor(TColor _color)
{
	defaultColor = _color;
	color = _color;
}

#endif

void LogStream::logStr(char* sz)
{
	logStream(sz);
}


void LogStream::logStream(char* sz)
{
	mfLog(sz);
}

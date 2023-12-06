#include "mf_perfcounter.h"
#include <fstream>
#include <ctime>
#include <time.h>
#include <stdio.h>
#include <string>
#include "mf_logfile.h"
#include "mf_string.h"
#include "mf_thread.h"

// Define to create output file name with process-id, this allows
// unique log file per execution
// #define FNAME_WITH_PID

// Static members
#ifdef _WIN32

// Windows
#include <process.h>

bool PerfLog::enabled = true;

#ifdef FNAME_WITH_PID
std::string PerfLog::fileName = "c:\\temp\\PerfLog_" + std::to_string(getpid()) + ".txt";
#else
std::string PerfLog::fileName = "c:\\temp\\PerfLog.txt";
#endif

#else

// Linux
#include <sys/types.h>
#include <unistd.h>

bool PerfLog::enabled = true;

#ifdef FNAME_WITH_PID
std::string PerfLog::fileName = "/tmp/PerfLog_" + std::to_string(getpid()) + ".txt";
#else
std::string PerfLog::fileName = "/tmp/PerfLog.txt";
#endif

#endif


// Static data
bool PerfLog::useStream = false;
bool PerfLog::dumpToConsole = true;
bool PerfLog::dumpToFile = false;
std::ostringstream PerfLog::stream;
bool PerfLog::debugBuild = Thread::isDebugBuild();


// Return true if s starts with prefix
static bool startsWith(const std::string& s, const std::string& prefix) 
{
	return (s.size() >= prefix.size()) && (s.compare(0, prefix.size(), prefix) == 0);
}


PerfLog::PerfLog(const std::string& _caption, uint64_t _count)
{
	started = false;
	count = _count;
	logfile = NULL;
	start(_caption, false);
}


PerfLog::PerfLog(LogFile* _logfile, const std::string& _caption, bool logStarted)
{
	started = false;
	count = 0;
	logfile = _logfile;
	start(_caption, logStarted);
}


PerfLog::PerfLog(LogFile& _logfile, const std::string& _caption, bool logStarted)
{
	started = false;
	count = 0;
	logfile = &_logfile;
	start(_caption, logStarted);
}


void PerfLog::start(const std::string& _caption, bool logStarted)
{
	if (!PerfLog::enabled)
		return;

	caption = _caption;
	started = true;

	// Save start time
	startTime = perf.startTime;
	isSection = startsWith(caption, "---") || startsWith(caption, "===") || startsWith(caption, "***");

	// Optional log start message
	if (logStarted) {
		std::string msg = caption + " started";
		if (logfile) {
			logfile->log(msg);
			return;
		}
		mfLog(msg);
		return;

		std::ostream* outstr = PerfLog::useStream ? &PerfLog::stream : &std::cout;
		if (isSection)
			*outstr << std::endl;

		*outstr << msg << std::endl;
	}
}


void PerfLog::stop()
{
	if (!PerfLog::enabled || !started)
		return;

	// Get elapsed time since constructor was invoked
	perf.startTime = startTime;
	double stopms = perf.stopms();
	started = false;

	std::string msg;
	if (debugBuild) {
		if (count > 0)
			msg = Asprintf("%s: %0.3lf ms, count: %lld, per: %0.6lf microsec (debug build)", caption.c_str(), stopms, count, stopms / (double)count * 1000.0);
		else
			msg = Asprintf("%s: %0.3lf ms (debug build)", caption.c_str(), stopms);
	}
	else {
		if (count > 0)
			msg = Asprintf("%s: %0.3lf ms, count: %lld, per: %0.6lf microsec", caption.c_str(), stopms, count, stopms / (double)count * 1000.0);
		else
			msg = Asprintf("%s: %0.3lf ms", caption.c_str(), stopms);
	}

	if (logfile) {
		logfile->log(msg);
	}
	else {
		mfLog(msg);
	}
	return;

	// Log end message
	std::ostream* outstr = PerfLog::useStream ? &PerfLog::stream : &std::cout;
	*outstr << msg << std::endl;

	if (isSection)
		*outstr << std::endl << std::endl;
}


void PerfLog::log(const std::string& text)
{
	if (!PerfLog::enabled)
		return;

	double stopms = perf.stopms();
	std::string msg = Asprintf("%s: %0.3 ms", text.c_str(), stopms);
	if (logfile) {
		logfile->log(msg);
	}
	mfLog(msg);
	return;

	std::ostream* outstr = PerfLog::useStream ? &PerfLog::stream : &std::cout;
	*outstr << msg << std::endl;
}


void PerfLog::setEnabled(bool _enabled, bool _dumpToFile, bool _dumpToConsole)
{
	enabled = _enabled;
	dumpToFile = _dumpToFile;
	dumpToConsole = _dumpToConsole;
}


void PerfLog::setUseStream(bool _useStream)
{
	useStream = _useStream;
}


void PerfLog::dump()
{
	if (PerfLog::enabled) {

		// Dump to file
		if (PerfLog::dumpToFile) {
			std::string fname = PerfLog::fileName;

			std::cout << "PerfLog::dump - " << fname.c_str() << std::endl;
			std::ofstream ofs;
			ofs.open(fname.c_str(), std::ofstream::out | std::ofstream::app);
			ofs << "*** PerfLog::dump ***" << std::endl << std::endl;
			ofs << PerfLog::stream.str();
			std::cout << "PerfLog::dump - file written" << std::endl;
		}

		// Dump to console
		if (PerfLog::dumpToConsole) {
			std::cout << PerfLog::stream.str();
		}
	}

	clear();
}


void PerfLog::clear()
{
	PerfLog::stream.str(std::string());
}


std::ostringstream& PerfLog::getStream()
{
	return PerfLog::stream;
}


std::string PerfLog::now()
{
#ifdef PERFCOUNTER_HAVE_CHRONO
	using namespace std::chrono;

	// get current time
	auto _now = system_clock::now();

	// get number of milliseconds for the current second
	// (remainder after division into seconds)
	auto ms = duration_cast<milliseconds>(_now.time_since_epoch()) % 1000;

	// convert to std::time_t in order to convert to std::tm (broken time)
	auto timer = system_clock::to_time_t(_now);

	// convert to broken time
	std::tm bt = *std::localtime(&timer);

	char buf[80];
    sprintf(buf, "%02d/%2d/%d %02d:%2d:%02d.%03d ", (int)bt.tm_mday, (int)bt.tm_mon, (int)bt.tm_year, (int)bt.tm_hour, (int)bt.tm_min, (int)bt.tm_sec, (int)ms.count());
	return std::string(buf);
#else
	time_t     now = time(0);
	struct tm  tstruct;
	char       buf[80];
	tstruct = *localtime(&now);
	strftime(buf, sizeof(buf), "%d/%m/%Y %H:%M:%S ", &tstruct);
	return std::string(buf);
#endif
}


void PerfLog::selfTest()
{
	std::cout << "PerfLog self performance started, enabled: " << PerfLog::enabled << ", useStream: " << PerfLog::useStream << std::endl;

	int iters = 1000;
	PerfCounter perf;
	for (int iter = 0; iter < iters; iter++) {
		PerfLog plog("test", true);
		plog.log("elapsed");
	}

	double stop = perf.stopms();
	std::cout << "PerfLog self performance: " << stop / (double)iters << " ms" << std::endl;
	PerfLog::clear();
	PerfLog::dump();
}

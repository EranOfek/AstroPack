
// Set when compiling with OpenCV
#define HAVE_OPENCV

// Set to create file name with process-id
#define FNAME_WITH_PID


#ifdef HAVE_OPENCV
#include "opencv2/core/ti_perfcounter.hpp"
#else
#include "ti_perfcounter.hpp"
#endif

#include <fstream>
#include <ctime>
#include <time.h>
#include <stdio.h>

// Static members
#ifdef _WIN32

// Windows
#include <process.h>

bool PerfCounterLog::enabled = true;

#ifdef FNAME_WITH_PID
std::string PerfCounterLog::fileName = "c:\\temp\\PerfLog_" + std::to_string(getpid()) + ".txt";
#else
std::string PerfCounterLog::fileName = "c:\\temp\\PerfLog.txt";	
#endif

#else  // Windows

// Linux
#include <sys/types.h>
#include <unistd.h>

bool PerfCounterLog::enabled = false;

#ifdef FNAME_WITH_PID
std::string PerfCounterLog::fileName = "/tmp/PerfLog_" + std::to_string(getpid()) + ".txt";
#else
std::string PerfCounterLog::fileName = "/tmp/PerfLog.txt";
#endif

#endif  // Linux


// Static data
bool PerfCounterLog::useStream = true;
bool PerfCounterLog::dumpToConsole = false;
bool PerfCounterLog::dumpToFile = true;
std::ostringstream PerfCounterLog::stream;


// Auto call to PerfCounterLog::dump() on termination
struct PerfCounterLogAutoDump {
	PerfCounterLogAutoDump() {}
	~PerfCounterLogAutoDump() { PerfCounterLog::dump(); }
};

// Will call dump() on termination
static PerfCounterLogAutoDump autoDump;


static bool startsWith(const std::string& s, const std::string& prefix) 
{
	return s.size() >= prefix.size() && s.compare(0, prefix.size(), prefix) == 0;
}

PerfCounterLog::PerfCounterLog(const std::string& _caption, bool logStarted) 
{
	started = false;
	start(_caption, logStarted);
}

PerfCounterLog::~PerfCounterLog()
{
	stop();
}

void PerfCounterLog::start(const std::string& _caption, bool logStarted)
{
	if (!PerfCounterLog::enabled)
		return;

	caption = _caption;
	started = true;

	// Save start time
	startTime = perf.startTime;
	isSection = startsWith(caption, "---") || startsWith(caption, "===") || startsWith(caption, "***");

	// Optional log start message
	if (logStarted) {		
		std::ostream& outstr = PerfCounterLog::useStream ? PerfCounterLog::stream : std::cout;
		if (isSection)
			outstr << std::endl;

		outstr << caption.c_str() << " started" << std::endl;		
	}
}


void PerfCounterLog::stop()
{
	if (!PerfCounterLog::enabled || !started)
		return;

	// Get elapsed time since constructor was invoked
	perf.startTime = startTime;
	double stopms = perf.stopms();
	started = false;

	// Log end message
	std::ostream& outstr = PerfCounterLog::useStream ? PerfCounterLog::stream : std::cout;
	outstr << caption.c_str() << ": " << stopms << " ms" << std::endl;

	if (isSection)
		outstr << std::endl << std::endl;
}



void PerfCounterLog::log(const std::string& text)
{
	if (!PerfCounterLog::enabled)
		return;

	double stopms = perf.stopms();
	std::ostream& outstr = PerfCounterLog::useStream ? PerfCounterLog::stream : std::cout;
	outstr << text.c_str() << ": " << stopms << " ms" << std::endl;
}

void PerfCounterLog::setEnabled(bool _enabled, bool _dumpToFile, bool _dumpToConsole)
{
	enabled = _enabled;
	dumpToFile = _dumpToFile;
	dumpToConsole = _dumpToConsole;
}

void PerfCounterLog::setUseStream(bool _useStream) 
{
	useStream = _useStream;
}

void PerfCounterLog::dump()
{
	if (PerfCounterLog::enabled) {

		// Dump to file
		if (PerfCounterLog::dumpToFile) {
			std::string fname = PerfCounterLog::fileName;

			std::cout << "PerfLog::dump - " << fname.c_str() << std::endl;
			std::ofstream ofs;
			ofs.open(fname.c_str(), std::ofstream::out | std::ofstream::app);
			ofs << "*** PerfCounterLog::dump ***" << std::endl << std::endl;
			ofs << PerfCounterLog::stream.str();
			std::cout << "PerfLog::dump - file written" << std::endl;
		}

		// Dump to console
		if (PerfCounterLog::dumpToConsole) {
			std::cout << PerfCounterLog::stream.str();
		}
	}

	clear();
}

void PerfCounterLog::clear()
{	
	PerfCounterLog::stream.str(std::string());
}

std::ostringstream& PerfCounterLog::getStream()
{
	return PerfCounterLog::stream;
}

std::string PerfCounterLog::now()
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
	sprintf(buf, "%02d:%2d:%02d.%03d ", (int)bt.tm_hour, (int)bt.tm_min, (int)bt.tm_sec, (int)ms.count());
	return std::string(buf);
#else
	time_t     now = time(0);
	struct tm  tstruct;
	char       buf[80];
	tstruct = *localtime(&now);
	strftime(buf, sizeof(buf), "%H:%M:%S ", &tstruct);
	return std::string(buf);
#endif
}

void PerfCounterLog::selfTest()
{
	std::cout << "PerfCounterLog self performance started, enabled: " << PerfCounterLog::enabled << ", useStream: " << PerfCounterLog::useStream << std::endl;

	int iters = 1000;
	PerfCounter perf;
	for (int iter = 0; iter < iters; iter++) {
		PerfCounterLog plog("test", true);
		plog.log("elapsed");
	}

	double stop = perf.stopms();
	std::cout << "PerfCounterLog self performance: " << stop / (double)iters << " ms" << std::endl;
	PerfCounterLog::clear();
	PerfCounterLog::dump();
}


namespace cv {

void perfLogSetEnabled(bool _enabled, bool _dumpToFile, bool _dumpToConsole)
{
	PerfLog::setEnabled(_enabled, _dumpToFile, _dumpToConsole);
}


void perfLogDump()
{
	PerfLog::dump();
}

}  // cv

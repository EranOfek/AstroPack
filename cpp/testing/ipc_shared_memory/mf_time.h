#ifndef mf_timeH
#define mf_timeH

#include <stdint.h>
#include <time.h>
#include <string>

#ifdef _WIN32
	// For timeval
	#include <winsock2.h>
#endif

// Tick data type
typedef uint64_t Tick;
        
// Wrapper for 'struct timeval'
struct TimeVal {
    timeval tv;

    // Constructor - clear
	TimeVal() { clear(); }

    // Clear
    void clear() { tv.tv_sec = 0; tv.tv_usec = 0; }

    // Return true if not empty
    bool isSet() { return (tv.tv_sec != 0) || (tv.tv_usec != 0); }

    // Return true if empty
    bool empty() { return !isSet(); }

    // Add
    TimeVal& operator +=(const TimeVal& b)
    {
        tv.tv_sec += b.tv.tv_sec;
        tv.tv_usec += b.tv.tv_usec;
        if (tv.tv_usec >= 1000000) {
            tv.tv_sec++;
            tv.tv_usec -= 1000000;
        }
        return *this;
    }

    // Sub
    TimeVal& operator -=(const TimeVal& b)
    {
        tv.tv_sec -= b.tv.tv_sec;
        tv.tv_usec -= b.tv.tv_usec;
        if (tv.tv_usec < 0) {
            tv.tv_sec--;
            tv.tv_usec += 1000000;
        }
        return *this;
    }

    // Operators
    bool operator ==(const TimeVal& b) const { return (tv.tv_sec == b.tv.tv_sec) && (tv.tv_usec == b.tv.tv_usec); }
    bool operator != (const TimeVal& b) const { return !(operator == (b)); }

    bool operator <(const TimeVal& b) const { return (tv.tv_sec == b.tv.tv_sec) ? (tv.tv_usec < b.tv.tv_usec) : (tv.tv_sec < b.tv.tv_sec); }
    bool operator >(const TimeVal& b) const { return (tv.tv_sec == b.tv.tv_sec) ? (tv.tv_usec > b.tv.tv_usec) : (tv.tv_sec > b.tv.tv_sec); }

};


// Get current timestamp in microseconds
uint64_t mfGetTimeUsec();

// Get current system tick in milliseconds
Tick mfTick();

// Get current timestamp
TimeVal mfNow();

// Convert timestamp to string, optional milliseconds
std::string mfTimeStr(const TimeVal& dt, bool ms = false);

// Get date/time string
std::string mfNowStr(bool ms = false);

// Convert date/time to filename string, optional milliseconds
// i.e. 2020_08_05__12_18_30 or 2020_08_05__12_18_30_120 (ms=true)
std::string mfTimeFileName(const TimeVal& dt, bool ms = false);

// Tick to string
std::string tickToStr(Tick i);

// Sleep for specified time [milliseconds]
void mfSleep(int ms);

// Emulate gettimeofday() under Windows
#ifdef _WIN32
int gettimeofday(struct timeval* tp, struct timezone* tzp);
#endif


// Return time diff in MS
inline int64_t timeValDiffMS(struct timeval& starttime, struct timeval& finishtime)
{
	int64_t msec;
	msec =  (finishtime.tv_sec - starttime.tv_sec) * 1000;
	msec += (finishtime.tv_usec - starttime.tv_usec) / 1000;
	return msec;
}


// Return time diff in seconds
inline double timeValDiffSec(struct timeval& starttime, struct timeval& finishtime)
{
	double sec;
	sec = (finishtime.tv_sec - starttime.tv_sec);
	sec += (double)(finishtime.tv_usec - starttime.tv_usec) / 1000000.0;
	return sec;
}

#endif

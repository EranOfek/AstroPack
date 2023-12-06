#include "mf_time.h"
#include <string.h>

#ifdef _WIN32
    #include <Windows.h>
#else
    #include <unistd.h>
    #include <sys/time.h>
#endif


uint64_t mfGetTimeUsec()
{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return tv.tv_sec*1000000 + tv.tv_usec;
}


Tick mfTick()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    Tick tick = (t.tv_sec * 1000) + (t.tv_usec / 1000);
    return tick;
}


TimeVal mfNow()
{
    TimeVal now;
    gettimeofday(&now.tv, NULL);
    return now;
}


std::string mfTimeStr(const TimeVal& dt, bool ms)
{
	char buf[128], bufms[128];
	time_t t = (time_t)dt.tv.tv_sec;
	struct tm tstruct = *localtime(&t);
    strftime(buf, sizeof(buf), "%d/%m/%Y %H:%M:%S", &tstruct);
	if (ms) {
	    sprintf(bufms, ".%03d", (int)(dt.tv.tv_usec / 1000));
	    strcat(buf, bufms);
	}
	return std::string(buf);
}


std::string mfNowStr(bool ms)
{
    return mfTimeStr(mfNow(), ms);
}


std::string mfTimeFileName(const TimeVal& dt, bool ms)
{
	char buf[128], bufms[128];
	time_t t = (time_t)dt.tv.tv_sec;
	struct tm tstruct = *localtime(&t);
    strftime(buf, sizeof(buf), "%Y_%m_%d__%H_%M_%S", &tstruct);
	if (ms) {
	    sprintf(bufms, "_%03d", (int)(dt.tv.tv_usec / 1000));
	    strcat(buf, bufms);
	}
	return std::string(buf);
}


std::string tickToStr(Tick i)
{
    char s[128];
    sprintf(s, "%llud", (long long unsigned int)i);
    return std::string(s);
}


void mfSleep(int ms)
{
    if (ms > 0) {
        #ifdef _WIN32
    	Sleep(ms);
        #else
	    usleep(1000*ms);
        #endif
    }
}


#ifdef _WIN32
int gettimeofday(struct timeval* tp, struct timezone* tzp)
{
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
    // until 00:00:00 January 1, 1970
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime(&system_time);
    SystemTimeToFileTime(&system_time, &file_time);
    time =  ((uint64_t)file_time.dwLowDateTime)      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return 0;
}
#endif

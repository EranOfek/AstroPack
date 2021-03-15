#include "mf_component.h"
#include "mf_system.h"
#include "mf_thread.h"
#include "mf_logfile.h"
#include <stdarg.h>
#include <iostream>


Component::~Component()
{
    // Terminate thread
    if (thread) {
        thread->terminated = true;
        Thread::sleep(2*threadInterval);
        delete thread;
        delete pCritSect;
    }
}


void Component::log(const std::string& msg, int color)
{
    mfLog(msg, color);
}


void Component::logf(const char* format, ...)
{
    char s[65536];
    va_list paramList;
    va_start(paramList, format);
	vsprintf(s, format, paramList);
    log(std::string(s));
    va_end(paramList);
}


void Component::logEx(Exception& ex, const std::string& msg)
{
    log(msg + " - " + ex.message);
}


bool Component::startThread(int _threadInterval)
{
    // Start thread
    threadInterval = _threadInterval;
    if (!thread) {
        pCritSect = new CritSect();
        thread = new ComponentThread(this);
        thread->start();
    }
    return true;
}


void ComponentThread::run()
{
    // Thread function, runs while the thread is alive, alls component's threadProcess()
    while (!terminated) {
        if (comp) {
            comp->threadProcess();
            Thread::sleep(comp->threadInterval);
        }
        else {
            Thread::sleep(10);
        }
    }
}

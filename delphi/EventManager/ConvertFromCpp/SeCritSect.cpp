//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SeCritSect.h"
#include "SePerfCount.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

//==============================================================================
//                          	TSMCriticalSection
//==============================================================================
TSMCriticalSection::TSMCriticalSection()
{
	InitializeCriticalSection(&FSection);

    Enabled         = true;
	LockTime 	    = 0;
	YieldCount 	    = 0;		//
}
//---------------------------------------------------------------------------
TSMCriticalSection::~TSMCriticalSection()
{
	DeleteCriticalSection(&FSection);
}
//---------------------------------------------------------------------------
void TSMCriticalSection::Enter()
{
    if (Enabled) {
    	EnterCriticalSection(&FSection);
    }
}
//---------------------------------------------------------------------------
void TSMCriticalSection::Leave()
{
    if (Enabled) {
    	LeaveCriticalSection(&FSection);
    }
}
//---------------------------------------------------------------------------
bool TSMCriticalSection::TryEnter()
{
	bool Result = true;
    if (Enabled) {
        Result = TryEnterCriticalSection(&FSection);
    }
	return Result;
}
//---------------------------------------------------------------------------
// Based on TMonitor source code (see SyncObjs.pas) (21/07/14)
int TSMCriticalSection::TryEnterTime(int Timeout)
{
	bool Result;
	int  Count  	= 0;

	// First just try the simple way
	Result = true;

    if (Enabled) {
        Result = TryEnterCriticalSection(&FSection);
    }

	// Aquired
	if (!Result) {

		#ifdef _VDNET_DEBUG_
		TPerfCounterLog PC(String().sprintf(L"TSMCriticalSection::TryEnterTime, Timeout=%d", Timeout), 5);
		#endif

		// Use TSpinWait (06/05/15)
		TSpinWait Spinner;
		Spinner.Reset();

		// Spinner also calls Sleep(1) every 20 calls of SpinCycle()
		while (Timeout >= 0) {
			Spinner.SpinCycle();
			Result = TryEnterCriticalSection(&FSection);
			if (Result)
				break;

			#ifdef never
			// Try to aquire the lock, this happens at least one time, even when YieldCount=0
			for (int y=0;  y <= YieldCount;  y++) {
				Result = TryEnterCriticalSection(&FSection);

				// Aquired
				if (Result)
					break;

				//
				if (y < YieldCount) {

					// (06/05/15)
					// Sleep(0) - Causes the thread to relinquish the remainder of its
					// time slice to any other thread that is ready to run.
					// If there are no other threads ready to run, the function returns
					// immediately, and the thread continues execution.
					// Sleep(0);

					// Causes the calling thread to yield execution to another thread
					// that is ready to run ON THE CURRENT PROCESSOR.
					// The operating system selects the next thread to be executed.
					// SwitchToThread();
				}
			}

			//
			if (Result)
				break;
			#endif

			//
			Timeout--;
			if (Timeout <= 0)
				break;

			// Use Sleep(1) to give other threads the chance to execute, without consuming CPU cycles
			Sleep(1);
			Count++;
		}
	}

	// Locked, set LockTime to the estimated time in ms that the lock operation took
	if (Result)
		LockTime = Count;

	// Lock timeout, set LockTime to -1
	else
		LockTime = -1;

	// Return number of tries on success, or zero on failure
	return Result ? Count+1 : 0;
}
//==============================================================================


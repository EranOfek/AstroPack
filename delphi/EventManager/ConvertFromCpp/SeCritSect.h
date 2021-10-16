//---------------------------------------------------------------------------
#ifndef SeCritSectH
#define SeCritSectH

#include <System.SyncObjs.hpp>

#pragma warn -8027

//===========================================================================
//                          TSMCriticalSection
//===========================================================================
// TCriticalSection in SycObjs.pas is not completed by Borland :)
// TryEnter not present in Borlands version.
class PACKAGE TSMCriticalSection {
public:
	//-----------------------------------------------------------------------
	TSMCriticalSection();
	//-----------------------------------------------------------------------
	~TSMCriticalSection();
	//-----------------------------------------------------------------------
	void Enter();
	//-----------------------------------------------------------------------
	void Leave();
	//-----------------------------------------------------------------------
	bool TryEnter();
	//-----------------------------------------------------------------------
	int TryEnterTime(int Timeout);
	//-----------------------------------------------------------------------
	CRITICAL_SECTION 	FSection;			// Windows object
	int  				LockTime;			//
	int  				YieldCount;			//
    bool                Enabled;            // (11/06/2019)
};

//===========================================================================
//							TSMCriticalSectionLocker
//===========================================================================
// Entire class is inline
// (29//12/11)
class TSMCriticalSectionLocker {
public:
	//-----------------------------------------------------------------------
	TSMCriticalSectionLocker();
	//-----------------------------------------------------------------------
	TSMCriticalSectionLocker(TSMCriticalSection& ACritSec, bool AEnter = true, int Timeout = -1);
	//-----------------------------------------------------------------------
	TSMCriticalSectionLocker(TSMCriticalSection* ACritSec, bool AEnter = true, int Timeout = -1);
	//-----------------------------------------------------------------------
	~TSMCriticalSectionLocker();
	//-----------------------------------------------------------------------
	void Enter();
	//-----------------------------------------------------------------------
	bool TryEnter();
	//-----------------------------------------------------------------------
	int TryEnterTime(int Timeout);
	//-----------------------------------------------------------------------
	void Leave();
	//-----------------------------------------------------------------------
	TSMCriticalSection* CritSec;
	bool Locked;
	int  LockTime;
};

//---------------------------------------------------------------------------
// (19/05/15)
typedef TSMCriticalSection TSeCritSect;
typedef TSMCriticalSectionLocker TSeCritSectLocker;
//===========================================================================
//                               TSMEvent
//===========================================================================
//
enum TSMWaitResult { smwrNone, smwrSignaled, smwrTimeout, smwrAbandoned, smwrError };

// Entire class is inline
//
class TSMEvent {
public:
	TSMEvent();
	TSMEvent(bool ManualReset, bool InitialState, const char* szName);
	TSMEvent(const char* szName);
	//-----------------------------------------------------------------------
	~TSMEvent();
	//-----------------------------------------------------------------------
	bool Init(bool ManualReset, bool InitialState, const char* szName);
	//-----------------------------------------------------------------------
	TSMWaitResult WaitFor(DWORD Timeout);
	//-----------------------------------------------------------------------
	void SetEvent();
	//-----------------------------------------------------------------------
	void ResetEvent();
	//-----------------------------------------------------------------------
	HANDLE  FHandle;
    int     FLastError;
};

//===========================================================================
//						TSMCriticalSectionLocker
//===========================================================================
inline TSMCriticalSectionLocker::TSMCriticalSectionLocker()
{
	CritSec  = NULL;
	Locked   = false;
	LockTime = 0;
}
//---------------------------------------------------------------------------
inline TSMCriticalSectionLocker::TSMCriticalSectionLocker(TSMCriticalSection& ACritSec, bool AEnter, int Timeout)
{
	CritSec  = &ACritSec;
	Locked   = false;
	LockTime = 0;

	if (AEnter) {
		if (Timeout > -1)
			TryEnterTime(Timeout);
		else
			Enter();
	}
}
//---------------------------------------------------------------------------
inline TSMCriticalSectionLocker::TSMCriticalSectionLocker(TSMCriticalSection* ACritSec, bool AEnter, int Timeout)
{
	CritSec  = ACritSec;
	Locked   = false;
	LockTime = 0;

	if (AEnter) {
		if (Timeout > -1)
			TryEnterTime(Timeout);
		else
			Enter();
	}
}
//---------------------------------------------------------------------------
inline TSMCriticalSectionLocker::~TSMCriticalSectionLocker()
{
	if (CritSec && Locked)
		CritSec->Leave();
}
//---------------------------------------------------------------------------
inline void TSMCriticalSectionLocker::Enter()
{
	if (CritSec && !Locked) {
		CritSec->Enter();
		Locked = true;
	}
}
//---------------------------------------------------------------------------
inline bool TSMCriticalSectionLocker::TryEnter()
{
	if (!Locked)
		Locked = CritSec ? CritSec->TryEnter() : true;

	return Locked;
}
//---------------------------------------------------------------------------
inline int TSMCriticalSectionLocker::TryEnterTime(int Timeout)
{
	int Result = 1;
	if (!Locked && CritSec) {
		Result = CritSec->TryEnterTime(Timeout);
		Locked = (Result > 0);
		LockTime = CritSec->LockTime;
	}

	return Result;
}
//---------------------------------------------------------------------------
inline void TSMCriticalSectionLocker::Leave()
{
	if (CritSec && Locked) {
		CritSec->Leave();
		Locked = false;
	}
}

//==============================================================================
//                          		TSMEvent
//==============================================================================
inline TSMEvent::TSMEvent()
{
	FHandle     = NULL;
	FLastError  = 0;
}
//---------------------------------------------------------------------------
inline TSMEvent::TSMEvent(bool ManualReset, bool InitialState, const char* szName)
{
	Init(ManualReset, InitialState, szName);
}
//---------------------------------------------------------------------------
inline TSMEvent::TSMEvent(const char* szName)
{
	FHandle     = ::OpenEventA(EVENT_ALL_ACCESS, false, szName);
	FLastError  = ::GetLastError();
}
//---------------------------------------------------------------------------
inline bool TSMEvent::Init(bool ManualReset, bool InitialState, const char* szName)
{
	FHandle     = ::CreateEventA(NULL, ManualReset, InitialState, szName);
	FLastError  = ::GetLastError();

	return true;
}
//---------------------------------------------------------------------------
inline TSMEvent::~TSMEvent()
{
	if (FHandle)
		::CloseHandle(FHandle);
}
//---------------------------------------------------------------------------
inline TSMWaitResult TSMEvent::WaitFor(DWORD Timeout)
{
	TSMWaitResult Result;

    //
	DWORD Res = ::WaitForSingleObject(FHandle, Timeout);

	//
    if (Res == WAIT_ABANDONED)
		Result = smwrAbandoned;
	else if (Res >= WAIT_OBJECT_0)
		Result = smwrSignaled;
	else if (Res == WAIT_TIMEOUT)
		Result = smwrTimeout;
	else if (Res == WAIT_FAILED) {
		Result     = smwrError;
		FLastError = ::GetLastError();
	}
	else {
		Result = smwrError;
	}

	return Result;
}
//---------------------------------------------------------------------------
inline void TSMEvent::SetEvent()
{
	if (FHandle)
		::SetEvent(FHandle);
}
//---------------------------------------------------------------------------
inline void TSMEvent::ResetEvent()
{
	if (FHandle)
		::ResetEvent(FHandle);
}
//---------------------------------------------------------------------------
#pragma warn -8027

#endif



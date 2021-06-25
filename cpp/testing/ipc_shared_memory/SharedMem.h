#ifndef SesamSharedMemH
#define SesamSharedMemH

//***********************************************************************
// TFileMapping   - Wrapper class for Windows file maping with locking
// TSharedMemory  - Base class for shared data, based on TFileMapping
//
//***********************************************************************

#include <syncobjs.hpp>

// We make TSharedMemory a decendent of TComponent with owner Application,
// so it will be freed on termination, to avoid CodeGuard errors
// This is conditioned because it is not fully tested yet (Chen, 19/02/09)
//
// Note: It seems problematic to have it deleted automatically, since there
//       are many places in the code that deletes it AFTER it is already
//       deleted by TApplication. *** DO NOT USE THIS FEATURE AT THE MOMENT ***
//
// #define _TSHAREDMEMORY_COMPONENT_

// There might be a compiler warning message for this line "Cannot create pre-compiled header".
// Ignore this warning message. Moved to TFileMapping, 13/07/10
// static bool LockFailedMsg = false;

// Define PACKAGE directive for classes
class TFileMapping;
class TSharedMemory;


// There is a conflict with this define after adding MSHTML to the project, this is an ugly workarond(29/04/10)
#define INVALID_HANDLE_VALUE2 ((HANDLE)(int)-1)

#include "SesamSMCritSect.h"


#pragma warn -8118
#pragma warn -8027

//===========================================================================

//==============================================================================
//                                  TFileMapping
//==============================================================================
class TFileMapping {
public:
   //---------------------------------------------------------------------------
   // Create/open mapping
   TFileMapping(const char* _szName, DWORD _dwSize, const char* _szFileName = NULL);
   //---------------------------------------------------------------------------
   // Close
   virtual ~TFileMapping();
   //---------------------------------------------------------------------------
   operator HANDLE() const;
   //---------------------------------------------------------------------------
   HANDLE   hFile;         // File handle
   HANDLE   hMap;          // FileMapping handle
   DWORD    dwMapSize;     // FileMapping size in bytes
   HANDLE   hMutex;        // Mutex handle for synchronization
   DWORD    dwLastError;   //
   DWORD    dwLockTimeout;
   int      nLockCount;
   bool     LockFailedMsg;
   CRITICAL_SECTION LockCritSec;

   // (23/01/15)
   DWORD	dwSize;				//
   char		szName[256];		//
   char		szFileName[256];	//
   char		szMutexName[256];	//

   //===========================================================================
   // Memory locker
   struct TLocker {
      //------------------------------------------------------------------------
      // Lock memory
	  TLocker(TFileMapping* Map, DWORD ATimeout = 0);
	  //------------------------------------------------------------------------
	  // Release lock
	  ~TLocker();
	  //------------------------------------------------------------------------
	  bool Lock(DWORD ATimeout = 0);
	  //------------------------------------------------------------------------
	  void Unlock();
	  //------------------------------------------------------------------------
	  bool IsLocked() { return Locked; }
	  //------------------------------------------------------------------------
	  // Pointer to memory object
	  TFileMapping*     pMap;
	  bool              Locked;
	  bool              ThrowOnFail;
	  DWORD             LastError;
	  DWORD             StartTime;
	  DWORD             Timeout;
	  DWORD 			WaitResult;		// (25/01/15)
   };

   friend class TLocker;
   //===========================================================================
   // Map View class
   struct TView {
      //------------------------------------------------------------------------
	  // Exception
      class TXFail {};
      //------------------------------------------------------------------------
      // Constructor
      TView(TFileMapping* Map, DWORD dwOffset = 0, DWORD dwSize = 0, bool bWrite = true);
      //------------------------------------------------------------------------
      // Destructor
	  virtual ~TView();
      //------------------------------------------------------------------------
      // Get pointer to memory
	  void* GetPtr() const;
      //------------------------------------------------------------------------
      operator LPVOID() const;
	  //------------------------------------------------------------------------
	  LPVOID lpvMem;
	  //------------------------------------------------------------------------
      TFileMapping* pMap;
   };

   friend class TView;
   //===========================================================================
};

//==============================================================================
//                               TSharedMemory
//==============================================================================
class TSharedMemory
#ifdef _TSHAREDMEMORY_COMPONENT_
    : public TComponent
#endif
{
public:
   //---------------------------------------------------------------------------
   // Constructor
   __fastcall TSharedMemory(const char* szName, DWORD dwSize, const char* szFileName = NULL);
   //---------------------------------------------------------------------------
   // Destructor
   virtual __fastcall ~TSharedMemory();
   //---------------------------------------------------------------------------
   // Get pointer to memory
   void* GetPtr() const;
   //---------------------------------------------------------------------------
   // Get pointer to memory as operator
   operator LPVOID() const;
   //---------------------------------------------------------------------------
   // Flush file to disk
   bool FlushFile();
   //---------------------------------------------------------------------------
   static bool Exists(const char* szName);
   //===========================================================================
   // Memory locker
   struct TLocker {
	  //------------------------------------------------------------------------
	  // Lock memory
	  TLocker(TSharedMemory* Memory, DWORD ATimeout = 0) : pMemory(Memory), MLocker(Memory->pMap, ATimeout) {}
	  //------------------------------------------------------------------------
	  // Release lock
	  ~TLocker() {}
	  //------------------------------------------------------------------------
	  bool Lock(DWORD ATimeout = 0) { return MLocker.Lock(ATimeout); }
	  //------------------------------------------------------------------------
	  void Unlock() { MLocker.Unlock(); }
      //------------------------------------------------------------------------
      bool IsLocked() { return MLocker.IsLocked(); }
	  //------------------------------------------------------------------------
      // Pointer to memory object
      TSharedMemory*        pMemory;
	  TFileMapping::TLocker MLocker;
   };
   //===========================================================================
protected:
   TFileMapping*        pMap;    // Pointer to file-mapping object
   TFileMapping::TView* pView;   // Pointer to view object


   friend class TLocker;
};

//==============================================================================

//==============================================================================
//                         TFileMapping Implementation
//==============================================================================
inline TFileMapping::TFileMapping(const char* _szName, DWORD _dwSize, const char* _szFileName) :
   hMutex(0), nLockCount(0)
{
	// (23/01/15)
	strcpy(szName, _szName ? _szName : "");
	strcpy(szFileName, _szFileName ? _szFileName : "");
	strcpy(szMutexName, "");
	dwSize = _dwSize;

	//
   bool fFileExists = false;

   if (*szFileName) {
		// Open/create
		hFile = CreateFileA(szFileName, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
			NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

		fFileExists = true;

		if (hFile == INVALID_HANDLE_VALUE2) {
			hFile = CreateFileA(szFileName, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
				NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);

			fFileExists = false;

            //
            if (hFile) {
                #ifdef never
                BYTE Buf[16384];
				ZeroMemory(Buf, 0, sizeof(Buf));

                int s = (int)dwSize;
                DWORD BytesWritten;
				do {
            	    WriteFile(hFile, Buf, sizeof(Buf), &BytesWritten, NULL);
                    s -= sizeof(Buf);
                } while (s > 0);
				#endif

				SetFilePointer(hFile, dwSize, 0, FILE_BEGIN);
                SetEndOfFile(hFile);
            }
		}

        if (hFile == INVALID_HANDLE_VALUE2) {
			return;
		}
   }
   else hFile = (HANDLE) 0xFFFFFFFF;

   dwMapSize = dwSize;

   dwLastError   = 0;
   dwLockTimeout = 1000;

   // (13/07/10)
   LockFailedMsg = false;

   //
   InitializeCriticalSection(&LockCritSec);

   hMap = CreateFileMappingA(
	   hFile,               // File handle or 0xFFFFFFFF to use paging file
	   NULL,                // no security attr.
       PAGE_READWRITE,      // read/write access
	   0,                   // size: high 32-bits
	   dwSize,              // size: low 32-bits
	   szName);             // name of map object


   if (hMap) {

      // The first process to attach initializes memory
      bool fInit = (GetLastError() != ERROR_ALREADY_EXISTS);

	  // Create mutex
	  wsprintfA(szMutexName, "MapMutex_%s", szName);
	  hMutex = CreateMutexA(NULL, false, szMutexName);

      // Initialize memory if this is the first process
      if (fInit && !fFileExists) {
         TView View(this);
         TLocker Locker(this, 10000);
         memset(View.GetPtr(), 0, dwSize);
      }
   }
}
//------------------------------------------------------------------------------
inline TFileMapping::~TFileMapping()
{
   if (hMap) {
	  CloseHandle(hMap);
	  hMap = 0;
   }

   if (hMutex) {
      CloseHandle(hMutex);
      hMutex = 0;
   }

   if (hFile) {
	  CloseHandle(hFile);
      hFile = 0;
   }

   DeleteCriticalSection(&LockCritSec);
}
//==============================================================================
//                     TFileMapping::TView Implementation
//==============================================================================
inline TFileMapping::TView::TView(TFileMapping* Map, DWORD dwOffset, DWORD dwSize, bool bWrite)
{
   pMap = Map;

   // Get a pointer to the file-mapped shared memory
   lpvMem = MapViewOfFile(
	   pMap->hMap,    // object to map view of
	   bWrite ? FILE_MAP_WRITE : FILE_MAP_READ,
	   0,             // high offset:   map from
	   dwOffset,      // low offset:    beginning
	   dwSize);       // size

   if (!lpvMem) {
	  #if (__BORLANDC__ >= 0x530)
	  throw Exception("TSharedMemory::MapViewOfFile failed, LastError=" + String((int)GetLastError()));
	  #else
	  throw TXFail();
	  #endif
   }
}
//------------------------------------------------------------------------------
inline TFileMapping::TView::~TView()
{
   // Unmap shared memory from the process's address space
   if (lpvMem) {
	  UnmapViewOfFile(lpvMem);
	  lpvMem = 0;
   }
}
//------------------------------------------------------------------------------
inline void* TFileMapping::TView::GetPtr() const
{
   return lpvMem;
}
//------------------------------------------------------------------------------
inline TFileMapping::TView::operator LPVOID() const
{
   return lpvMem;
}

//==============================================================================
inline TFileMapping::operator HANDLE() const
{
   return hMap;
}

//==============================================================================
//                    TSharedMemory::TLocker Implementation
//==============================================================================
inline TFileMapping::TLocker::TLocker(TFileMapping* Map, DWORD ATimeout) :
   pMap(Map), Locked(false), ThrowOnFail(true), LastError(0), Timeout(ATimeout ? ATimeout : 50)
{
	Lock(Timeout);
}
//------------------------------------------------------------------------------
inline TFileMapping::TLocker::~TLocker()
{
	Unlock();
}
//------------------------------------------------------------------------------
inline bool TFileMapping::TLocker::Lock(DWORD ATimeout)
{

	// (25/01/15)
	Timeout = ATimeout;

   //EnterCriticalSection(&LockCritSec);

   // Store current time, to track the lock period
   //StartTime = GetTickCount();

   { //if (true) { //if (nLockCount++ == 0) {

      // Request the mutex, give long timeout because Windows may give control
	  // to other process/thread that loads the CPU
      WaitResult = WaitForSingleObject(pMap->hMutex, 10000);  // Changed from 5000, 08/02/08.  //50 !!! (11/06/07) //dwLockTimeout);  // INFINITE);

      // (11/06/07)
	  StartTime = GetTickCount();

	  // WAIT_OBJECT_0
	  if (WaitResult == WAIT_OBJECT_0) {
		 Locked = true;
		 //pMap->nLockCount++;
	  }

	  // WAIT_ABANDONED (25/01/15)
	  // The specified object is a mutex object that was not released by the
	  // thread that owned the mutex object before the owning thread terminated.
	  // Ownership of the mutex object is granted to the calling thread and the
	  // mutex state is set to nonsignaled.
	  // If the mutex was protecting persistent state information,
	  // you should check it for consistency.
	  else if (WaitResult == WAIT_ABANDONED) {
		//static int ab=0;
		//ab++;
		Locked = true;
	  }

	  // WAIT_TIMEOUT (25/01/15)
	  else if (WaitResult == WAIT_TIMEOUT) {
		Locked = false;
	  }

	  // Other error (should be WAIT_FAILED)
	  else {
		 LastError = GetLastError();

		 #ifdef never
		 // (23/01/15)
		 if (WaitResult == WAIT_FAILED) {
			//static int fail=0;
			//fail++;

		 }
		 else if (WaitResult == WAIT_ABANDONED) {
			//static int ab=0;
			//ab++;
		 }
		 else {
			//static int other=0;
			//other++;
		 }
		 #endif


		#if (__BORLANDC__ >= 0x530)
		if (ThrowOnFail) {
			throw Exception("TSharedMemory::TLocker Lock failed");  //, LastError=" + String((int)LastError));
		}
		else {
			// Allow only one message window
			if (!pMap->LockFailedMsg) {
				pMap->LockFailedMsg = true;
				MessageBoxA(NULL, "TSharedMemory::TLocker Lock failed", "Warning", MB_OK);  //, LastError=" + String((int)LastError));
				pMap->LockFailedMsg = false;
			}
		}
		#endif
	  }
   }

   //LeaveCriticalSection(&LockCritSec);

   return Locked;
}
//------------------------------------------------------------------------------
inline void TFileMapping::TLocker::Unlock()
{
   //if (nLockCount == 0) {
   //     throw Exception("TFileMapping::Lock - Not locked");
   //}

   if (Locked) {
	 //if (nLockCount > 0) {
	  //nLockCount--;

	  { //if (true) { //if (nLockCount == 0) {

		 // Release the mutex
         if (ReleaseMutex(pMap->hMutex)) {
            //Result = true;
            Locked = false;
         }
		 else {
            LastError = GetLastError();
         }
      }
   }

   // Removed time checking, it is used only for debugging,
   // and there is nothing to do anyway with this message (06/02/08).
   #ifdef never

   // Check lock duration
   DWORD Elapsed = GetTickCount() - StartTime;

   #if (__BORLANDC__ >= 0x530)
   if (Elapsed > Timeout) {
      if (ThrowOnFail) {
          throw Exception("TSharedMemory::TLocker too long, time=" + String().sprintf("%d ms", Elapsed));
      }
      else {
            static bool msg=false;
            if (!msg) {
                msg = true;
                MessageBox(NULL, "TSharedMemory::TLocker duration is too long", "Warning", MB_OK);  //, LastError=" + String((int)LastError));
                msg = false;
			}
      }
   }
   #endif

   #endif
}
//==============================================================================
//                       TSharedMemory Implementation
//==============================================================================
inline __fastcall TSharedMemory::TSharedMemory(const char* szName, DWORD dwSize, const char* szFileName)
#ifdef _TSHAREDMEMORY_COMPONENT_
    : TComponent(Application)
#endif
{
   // When compiling with CodeGuard, a memory leak on program termination
   // is normal here, because some objects that uses TSharedMemory are
   // created by static data, and are not freeed.
   pMap  = new TFileMapping(szName, dwSize, szFileName);
   pView = new TFileMapping::TView(pMap);
}
//------------------------------------------------------------------------------
__fastcall TSharedMemory::~TSharedMemory()
{
   delete pView;
   delete pMap;
}
//------------------------------------------------------------------------------
inline void* TSharedMemory::GetPtr() const
{
   return pView->GetPtr();
}
//------------------------------------------------------------------------------
inline TSharedMemory::operator LPVOID() const
{
   return pView->GetPtr();
}
//------------------------------------------------------------------------------
inline bool TSharedMemory::FlushFile()
{
	return FlushViewOfFile(pView->GetPtr(), pMap->dwMapSize);
}
//------------------------------------------------------------------------------
inline bool TSharedMemory::Exists(const char* szName)
{
	// Create mutex
	char szMutexName[256];
	wsprintfA(szMutexName, "MapMutex_%s", szName);

	HANDLE hMutex = OpenMutexA(MUTEX_ALL_ACCESS, false, szMutexName);
	if (hMutex) CloseHandle(hMutex);

	return (hMutex != 0);
}

//==============================================================================
#pragma warn +8118
#pragma warn +8027

#endif



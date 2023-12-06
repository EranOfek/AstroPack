#ifndef SeSharedBufferH
#define SeSharedBufferH

//===========================================================================
// 					 Shared-Memory Buffer with Reallocation
//
// Memory-mapped object cannot be resized by Windows.
// In order to have a shared buffer that can be resized, we need to implement
// a special mechanism.
//
//
// The user of the buffer holds a reference to the shared-memory object.
// When the buffer has to be reallocated, the reference object must be locked
//===========================================================================
// Header

#include "SeSharedMem.h"
#include "SeGetTickCount64.h"

class TSharedBuffer;

// (12/10/2015)
#define SHAREDBUF_HEADER_SIGNATURE		0x20150121		// THeader::Signature
#define SHAREDBUF_BUFHEADER_SIGNATURE	0x20151012		// TBufHeader::Signature

//
class TSharedBuffer {
public:
	//--------------------------------------------------------------------
	struct TInitParams {
		DWORD	BufSize;				//
		DWORD	UserDataSize;			// (20/05/15)
		char	Name[128];				//

		TInitParams() { BufSize = 0;  UserDataSize = 0;  ZeroMemory(Name, sizeof(Name)); }
	};
	//--------------------------------------------------------------------
	// In MemHeader
	struct THeader {
		DWORD		Signature;          //
		DWORD		HeaderSize;			//
		DWORD		MemSize;			// (20/05/15)
		DWORD		Flags;				//
		DWORD		AllocSize;          //
		DWORD		AllocCounter;  		//
		__int64		BufferId;			// Should match BufHeader->BufferId
		CHAR		BufName[128];		//
		CHAR		BufMemName[128];	//
		DWORD		BufMemSize;			//
		DWORD		UserDataSize;		// (20/05/15)
		DWORD		UserDataOffset;		// (20/05/15)
		__int64		AllocTime64;		// (19/05/15)
		__int64		UseTime64;          // (19/05/15)
		BYTE		Reserved[32];		//
	};
	//--------------------------------------------------------------------
	// In MemBuffer (12/10/2015)
	// Actuall data is following
	struct TBufHeader {
		DWORD		Signature;          //
		DWORD		HeaderSize;			//
		DWORD		MemSize;			//
		__int64		BufferId;			//
		DWORD		Reserved[5];		//
    };
	//-----------------------------------------------------------------------
	TSharedBuffer(const TInitParams& Params);
	TSharedBuffer(const char* AName, DWORD ASize);
	//-----------------------------------------------------------------------
	~TSharedBuffer();
	//-----------------------------------------------------------------------
	void Init(const TInitParams& Params);
	//-----------------------------------------------------------------------
	void* GetPointer(DWORD Size = 0, bool AShrink = false, bool AUpdateUseTime = true);
	//-----------------------------------------------------------------------
	void* DoGetPointer(DWORD Size = 0, bool AShrink = false, bool AUpdateUseTime = true);
	//-----------------------------------------------------------------------
	void DoPrepareBufName(DWORD Size);
	//-----------------------------------------------------------------------
	void DoUpdateBuffer();
	//-----------------------------------------------------------------------
	TSharedMemory* GetMemHeader();
	//-----------------------------------------------------------------------
	TSharedMemory* GetMemBuffer();
	//-----------------------------------------------------------------------
	bool IsValid();
	//-----------------------------------------------------------------------
	DWORD GetAllocSize();
	//-----------------------------------------------------------------------
	void* GetUserData();
	//-----------------------------------------------------------------------
	void UpdateUseTime();
	//-----------------------------------------------------------------------
	bool Validate();
	//-----------------------------------------------------------------------
	bool FreeOnUseTimeout(DWORD ATimeout);
	//-----------------------------------------------------------------------
	void Log(String S, TColor AColor = clBlack);
	//-----------------------------------------------------------------------
	void LogEx(Exception& E, String S = "" , TColor AColor = clRed);
	//-----------------------------------------------------------------------
private:
	TSharedMemory*	MemHeader;		// Shared memory that contains THeader
	TSharedMemory*	MemBuffer;		// Shared memory that contains actual data
	THeader*		Header;			// Pointer in MemHeader
	TBufHeader*		BufHeader;		//
	BYTE*			Buffer;         // Pointer in MemBuffer
	DWORD			AllocCounter;	//
	DWORD			AllocSize;		//
	bool			Valid;			// (12/10/2015)
	bool			DebugMode;
};

//===========================================================================
inline TSharedMemory* TSharedBuffer::GetMemHeader()
{
	return MemHeader;
}
//---------------------------------------------------------------------------
inline TSharedMemory* TSharedBuffer::GetMemBuffer()
{
	return MemBuffer;
}
//---------------------------------------------------------------------------
inline bool TSharedBuffer::IsValid()
{
    return Valid;
}
//---------------------------------------------------------------------------
inline DWORD TSharedBuffer::GetAllocSize()
{
	return (MemHeader && Header) ? Header->AllocSize : 0;
}
//---------------------------------------------------------------------------
inline void* TSharedBuffer::GetUserData()
{
	BYTE* p = Header ? (BYTE*)Header + Header->UserDataOffset : NULL;

	return p;
}
//---------------------------------------------------------------------------
inline void TSharedBuffer::UpdateUseTime()
{
	Header->UseTime64 = SeGetTickCount64();
}
//---------------------------------------------------------------------------

#endif



#include <vcl.h>
#pragma hdrstop

#include "SeSharedBuffer.h"
#include "SeLogFile.h"
#include <mem.h>
#include "SeUuid.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

//---------------------------------------------------------------------------
TSharedBuffer::TSharedBuffer(const TInitParams& Params)
{
	Init(Params);
}
//---------------------------------------------------------------------------
TSharedBuffer::TSharedBuffer(const char* AName, DWORD ASize)
{
	TInitParams Params;

	strcpy(Params.Name, AName);
	Params.BufSize = ASize;

	Init(Params);
}
//---------------------------------------------------------------------------
TSharedBuffer::~TSharedBuffer()
{
	if (MemBuffer)
        delete MemBuffer;

	if (MemHeader)
        delete MemHeader;

	MemBuffer = NULL;
	MemHeader = NULL;
	Buffer	  = NULL;
}
//---------------------------------------------------------------------------
void TSharedBuffer::Init(const TInitParams& Params)
{
	//
	DebugMode = IsDebuggerPresent();

	// Create shared-memory object
	char HeaderName[128];
	char DataName[128];

	strcpy(HeaderName, Params.Name);
	strcat(HeaderName, "_ShBufHeader");

	strcpy(DataName, Params.Name);
	strcat(DataName, "_ShBufData");

	//
	DWORD AMemSize = sizeof(THeader) + Params.UserDataSize;
	MemHeader = new TSharedMemory(HeaderName, AMemSize);
	MemBuffer = NULL;

	// (12/10/2015)
	BufHeader = NULL;

	// Get pointer to header and data
	Header = (THeader*)MemHeader->GetPtr();

	// Initialize header
	if (Header->Signature != SHAREDBUF_HEADER_SIGNATURE) {
		ZeroMemory(Header, sizeof(THeader));
		Header->Signature 		= SHAREDBUF_HEADER_SIGNATURE;
		Header->HeaderSize 		= sizeof(THeader);
		Header->MemSize			= AMemSize;
		Header->Flags			= 0;
		Header->UserDataSize	= Params.UserDataSize;
		Header->UserDataOffset	= Header->HeaderSize;
		Header->AllocSize		= 0;
		Header->AllocCounter	= 0;

		//
		strncpy(Header->BufName, Params.Name, sizeof(Header->BufName)-1);

		// Calculate unique Id based on BufName (12/10/2015)
		Header->BufferId		= SesamHashMD5AsInt64(Header->BufName);
	}

	//
	AllocCounter	= 0;
	AllocSize		= 0;
	Buffer			= NULL;
    Valid           = false;     // (13/05/2019)

	//
	Log("Created");

	//
	GetPointer(Params.BufSize);
}
//===========================================================================
void* TSharedBuffer::GetPointer(DWORD Size, bool AShrink, bool AUpdateUseTime)
{
	void* p = NULL;

	TSharedMemory::TLocker Locker(MemHeader, 5000);
	if (Locker.IsLocked()) {
		p = DoGetPointer(Size, AShrink, AUpdateUseTime);
	}

	return p;
}
//---------------------------------------------------------------------------
// MUST be called within locking scope of TSharedMemory::TLocker Locker(MemHeader)
//
void* TSharedBuffer::DoGetPointer(DWORD Size, bool AShrink, bool AUpdateUseTime)
{
	void* Result = NULL;

	if (MemHeader && Header) {

		// Update current values from header, to refelect changes (20/05/15)
		bool ANeedUpdate = false;
		if ((Header->AllocCounter != AllocCounter) || (Header->AllocSize != AllocSize)) {

			Log(String().sprintf(L"DoGetPointer: NeedUpdate: Header->AllocCounter=%d, AllocCounter=%u, Header->AllocSize=%u, AllocSize=%u",
				Header->AllocCounter, AllocCounter, Header->AllocSize, AllocSize));

			AllocCounter = Header->AllocCounter;
			AllocSize    = Header->AllocSize;
			ANeedUpdate  = true;
		}

		// Need to reallocate buffer
		if ((Size > 0) && ((Size > AllocSize) || ((Size < AllocSize) && AShrink))) {
			DoPrepareBufName(Size);
		}

		// Check if AllocCounter has been changed,
		// meaning that we need to access another buffer that was modified by other
		// user of the buffer
		if ((Header->AllocCounter != AllocCounter) || (Header->AllocSize != AllocSize) || ANeedUpdate) {
			DoUpdateBuffer();
		}

		//
		Result = Buffer;

		//
		if (AUpdateUseTime) {
			UpdateUseTime();
        }
	}

	return Result;
}
//---------------------------------------------------------------------------
void TSharedBuffer::DoPrepareBufName(DWORD Size)
{
	// New allocation
	Header->AllocCounter++;
	Header->AllocSize = Size;

	//
	Header->BufMemSize = Size;

	// Prepare new Header->BufMemName as:
	// BufName_AllocCounter_AllocSize
	char sz[32];
	strcpy(Header->BufMemName, Header->BufName);
	strcat(Header->BufMemName, "_");
	itoa(Header->AllocCounter, sz, 10);
	strcat(Header->BufMemName, sz);
	strcat(Header->BufMemName, "_");
	itoa(Header->AllocSize, sz, 10);
	strcat(Header->BufMemName, sz);

	Log("DoPrepareBufName: " + String(Header->BufMemName));
}
//---------------------------------------------------------------------------
void TSharedBuffer::DoUpdateBuffer()
{
	// Update
	AllocCounter = Header->AllocCounter;
	AllocSize    = Header->AllocSize;

	Log(String().sprintf(L"DoUpdateBuffer: AllocCounter=%u, AllocSize=%u", AllocCounter, AllocSize));

	// Delete object, when all instances that access this shared-memory
	// are deleted, Windows frees it from memory
	if (MemBuffer)
        delete MemBuffer;

	// Allocate new
	MemBuffer = new TSharedMemory(Header->BufMemName, Header->BufMemSize + sizeof(TBufHeader) + 16);

	// Initialize header of MemBuffer (12/10/2015)
	BufHeader = (TBufHeader*)MemBuffer->GetPtr();
	if (BufHeader->Signature == 0) {  //SHAREDBUF_BUFHEADER_SIGNATURE) {
		ZeroMemory(BufHeader, sizeof(TBufHeader));
		BufHeader->Signature	= SHAREDBUF_BUFHEADER_SIGNATURE;
		BufHeader->HeaderSize	= sizeof(TBufHeader);
		BufHeader->MemSize		= Header->BufMemSize;
		BufHeader->BufferId		= Header->BufferId;

        // Need to validate also here, otherwise Valid is false and we cannot use buffer (13/05/2019)
		if (!Validate()) {
			Log("DoUpdateBuffer (Sig=0): Validate failed", clRed);
		}
	}
	else {
		if (!Validate()) {
			Log("DoUpdateBuffer: Validate failed", clRed);
		}
	}

	// Buffer data follows header
	Buffer = (BYTE*)BufHeader + BufHeader->HeaderSize;

	//
	Header->AllocTime64 = SeGetTickCount64();
	Header->UseTime64   = Header->AllocTime64;
}
//---------------------------------------------------------------------------
// Validate headers of both shared-memory objects (12/10/2015)
bool TSharedBuffer::Validate()
{
	Valid = false;

	//
	if (Header && BufHeader) {

		//
		if (Header->Signature == SHAREDBUF_HEADER_SIGNATURE) {

			//
			if (BufHeader->Signature == SHAREDBUF_BUFHEADER_SIGNATURE) {

				//
				if (Header->BufferId == BufHeader->BufferId) {
					Valid = true;
				}
				else {
					Log(String().sprintf(L"Validate: Header->BufferId: %Li, BufHeader->BufferId: %Li", Header->BufferId, BufHeader->BufferId), clRed);
				}
			}
			else {
				Log(String().sprintf(L"Validate: Invalid BufHeader->Signature: %08X", BufHeader->Signature), clRed);
			}
		}
		else {
			Log(String().sprintf(L"Validate: Invalid Header->Signature: %08X", Header->Signature), clRed);
		}
	}
	else {
		Log(String().sprintf(L"Validate: Header=%p, BufHeader=%p", Header, BufHeader), clRed);
	}

	return Valid;
}
//---------------------------------------------------------------------------
// This function should be called regulary only by one user of the Shared-Buffer
// for example, between SVdSrvr and DrvRtsp, only SVdSrvr should call it ?????
bool TSharedBuffer::FreeOnUseTimeout(DWORD ATimeout)
{
	if (!Validate()) {
		Log("FreeOnUseTimeout: Invalid", clRed);
		return false;
	}

	// When ATimeout=0, free now (07/10/2015)
	if (!ATimeout && (AllocSize > 64)) {

		Log(String().sprintf(L"FreeOnUseTimeout: Shrinking: Timeout=0"));

		// Shrink to 64 bytes
		GetPointer(64, true, false);
		return true;
	}

	//
	if (Header->UseTime64 && ATimeout && (AllocSize > 64)) {
		__int64 Elapsed64 = SeGetTickCount64() - Header->UseTime64;
		if (Elapsed64 > ATimeout) {
			Log(String().sprintf(L"FreeOnUseTimeout: Shrinking: Timeout=%u, Elapsed=%u", (DWORD)ATimeout, (DWORD)Elapsed64));

			// Shrink to 64 bytes
			GetPointer(64, true, false);
			return true;
		}
	}

	return false;
}
//---------------------------------------------------------------------------
void TSharedBuffer::Log(String S, TColor AColor)
{
	static TLogFile* LogFile = NULL;
	if (!LogFile) {
		LogFile = new TLogFile("$MOD$-SharedBuffer");
	}

	if (MemHeader) {
		S = L"[" + String(Header->BufName) + L"] " + S;
	}

	LogFile->Log(S);
}
//---------------------------------------------------------------------------
void TSharedBuffer::LogEx(Exception& E, String S, TColor AColor)
{
	Log(S + " - " + E.Message, AColor);
}
//---------------------------------------------------------------------------


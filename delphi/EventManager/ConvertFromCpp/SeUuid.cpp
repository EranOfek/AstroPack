//---------------------------------------------------------------------------
#pragma hdrstop

#include "SeUuid.h"

#include <iphlpapi.h>
#include <System.SysUtils.hpp>
#include <IniFiles.hpp>

// Part of HashLib, should be in D:\Bin\BDS2011\HashLib
#include "CryptoAPI.hpp"
#include "SeCRC.hpp"

#pragma link "IPHLPAPI.lib"
//---------------------------------------------------------------------------
#pragma package(smart_init)

HRESULT PACKAGE WinUuidCreateSequential(UUID* guid);
HRESULT PACKAGE SesamUuidCreateSequential(UUID *Uuid);
//===========================================================================
//					Windows UuidCreateSequential Wrapper
//===========================================================================
HRESULT WinUuidCreateSequential(UUID* guid)
{
	typedef __stdcall HRESULT (*fnUuidCreate)(GUID*);

	static 	fnUuidCreate 	UuidCreateFunc 	= NULL;
	static 	HMODULE 		handle 			= NULL;
	static 	bool 			loaded 			= false;

	// Initialize
	if (!loaded) {
		handle = LoadLibrary(L"RPCRT4.DLL");
		if (handle) {
			UuidCreateFunc = (fnUuidCreate)GetProcAddress(handle, "UuidCreateSequential");
			if (!UuidCreateFunc)
				UuidCreateFunc = (fnUuidCreate)GetProcAddress(handle, "UuidCreate");
		}
		loaded = true;
	}

	// Call function
	HRESULT result;
	if (UuidCreateFunc)
		result = UuidCreateFunc(guid);
	else {
		ZeroMemory(guid, sizeof(*guid));
		result = 0xFFFFFFFF;
	}

	return result;
}
//===========================================================================
//
String SesamUuidCreateString()
{
	GUID guid;
	UuidCreate(&guid);
	String S = GUIDToString(guid);

	// Remove { }
	if ((S.Length() > 2) && S[1] == '{') {
		S.Delete(S.Length(), 1);
		S.Delete(1, 1);
	}
	return S;
}
//===========================================================================
//				  		Sources From Wine Project
//===========================================================================
// See: WINE Project, RPCRT4 - UuidCreateSequential()
// http://doxygen.reactos.org/d4/d62/rpcrt4__main_8c_source.html
//
#define TICKS_PER_CLOCK_TICK 1000
#define SECSPERDAY  86400
#define TICKSPERSEC 10000000
/* UUID system time starts at October 15, 1582 */
#define SECS_15_OCT_1582_TO_1601  ((17 + 30 + 31 + 365 * 18 + 5) * SECSPERDAY)
#define TICKS_15_OCT_1582_TO_1601 ((ULONGLONG)SECS_15_OCT_1582_TO_1601 * TICKSPERSEC)
//---------------------------------------------------------------------------
static void RPC_UuidGetSystemTime(ULONGLONG *time)
{
	FILETIME ft;

	GetSystemTimeAsFileTime(&ft);

	*time = ((ULONGLONG)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
	*time += TICKS_15_OCT_1582_TO_1601;
}
//---------------------------------------------------------------------------
/* Assume that a hardware address is at least 6 bytes long */
#define ADDRESS_BYTES_NEEDED 6

static RPC_STATUS RPC_UuidGetNodeAddress(BYTE *address)
{
	// Chen - Need to load resolve RtlGenRandom (=SystemFunction036) in advapi32.dll
	static int 		loaded 		= 0;
	static HMODULE 	hAdvApi32 	= NULL;

	typedef __stdcall BOOLEAN (*fnRtlGenRandom)(PVOID, ULONG);
	static fnRtlGenRandom pRtlGenRandom = 0;

	if (!loaded) {
		loaded = 1;
		hAdvApi32 = LoadLibrary(L"advapi32.dll");
		pRtlGenRandom = (fnRtlGenRandom)GetProcAddress(hAdvApi32, "SystemFunction036");
	}
	if (!pRtlGenRandom)
		return 0;

	DWORD status = RPC_S_OK;

	ULONG buflen = sizeof(IP_ADAPTER_INFO);
	PIP_ADAPTER_INFO adapter = (PIP_ADAPTER_INFO)HeapAlloc(GetProcessHeap(), 0, buflen);

	if (GetAdaptersInfo(adapter, &buflen) == ERROR_BUFFER_OVERFLOW) {
		HeapFree(GetProcessHeap(), 0, adapter);
		adapter = (PIP_ADAPTER_INFO)HeapAlloc(GetProcessHeap(), 0, buflen);
	}

	//
	if (GetAdaptersInfo(adapter, &buflen) == NO_ERROR) {

		// Chen: Choose good MAC address (VMware installs VMnet1/VMnet8 with same MAC for all computers)
		// We look for MAC that its first byte is not zero
		// Examples:
		// VMnet8:   00-50-56-C0-00-08
		// VMnet1:	 00-50-56-C0-00-08
		// Physical: F4-6D-04-64-E6-1D (select this one)
		PIP_ADAPTER_INFO adap = adapter;
		while (adap) {
			if (adap->Address[0] != 0)
				break;
			adap = adap->Next;
		}

		// Not found, use the first one anyway
		if (!adap)
			adap = adapter;

		// Copy
		BYTE mac[ADDRESS_BYTES_NEEDED];
		for (int i = 0;  i < ADDRESS_BYTES_NEEDED;  i++)
			mac[i] = adap->Address[i];

		//Chen: Hide the MAC address by simple XOR
		for (int i = 0;  i < ADDRESS_BYTES_NEEDED;  i++) {
			int X = 13*(i+1);
			address[i] = mac[i] ^ (X & 0xFF);
		}

	}
	/* We can't get a hardware address, just use random numbers.
		Set the multicast bit to prevent conflicts with real cards. */
	else {
		pRtlGenRandom(address, ADDRESS_BYTES_NEEDED);
		address[0] |= 0x01;
		status = RPC_S_UUID_LOCAL_ONLY;
	}

	HeapFree(GetProcessHeap(), 0, adapter);
	return status;
}
//===========================================================================
HRESULT SesamUuidCreateSequential(UUID *Uuid)
{
	static CRITICAL_SECTION uuid_cs;
	static int initialised, count;

	ULONGLONG time;
	static ULONGLONG timelast;
	static WORD sequence;

	static DWORD status;
	static BYTE address[MAX_ADAPTER_ADDRESS_LENGTH];

	// Chen: Initialize
	static bool init_cs = false;
	if (!init_cs) {
		InitializeCriticalSection(&uuid_cs);
		init_cs = true;
	}

	//
	EnterCriticalSection(&uuid_cs);

	if (!initialised) {
		RPC_UuidGetSystemTime(&timelast);
		count = TICKS_PER_CLOCK_TICK;

		sequence = ((rand() & 0xff) << 8) + (rand() & 0xff);
		sequence &= 0x1fff;

		status = RPC_UuidGetNodeAddress(address);
		initialised = 1;
	}

	/* Generate time element of the UUID. Account for going faster
	   than our clock as well as the clock going backwards. */
	while (1) {
		RPC_UuidGetSystemTime(&time);
		if (time > timelast) {
			count = 0;
			break;
		}
		if (time < timelast) {
			sequence = (sequence + 1) & 0x1fff;
			count = 0;
			break;
		}
		if (count < TICKS_PER_CLOCK_TICK) {
			count++;
			break;
		}
	}

	timelast = time;
	time += count;

	/* Pack the information into the UUID structure. */
	Uuid->Data1  = (ULONG)(time & 0xffffffff);
	Uuid->Data2  = (unsigned short)((time >> 32) & 0xffff);
	Uuid->Data3  = (unsigned short)((time >> 48) & 0x0fff);

	/* This is a version 1 UUID */
	Uuid->Data3 |= (1 << 12);

	Uuid->Data4[0]  = sequence & 0xff;
	Uuid->Data4[1]  = (sequence & 0x3f00) >> 8;
	Uuid->Data4[1] |= 0x80;
	memcpy(&Uuid->Data4[2], address, ADDRESS_BYTES_NEEDED);

	LeaveCriticalSection(&uuid_cs);

	//TRACE("%s\n", debugstr_guid(Uuid));

	return status;
}
//---------------------------------------------------------------------------
// Generate Seq UUID like 21EC2020-3AEA-4069-A2DD-08002B30309D
String SesamUuidCreateSequentialString()
{
	GUID guid;
	/*HRESULT result = */SesamUuidCreateSequential(&guid);
	String S = System::Sysutils::GUIDToString(guid);

	// Remove { }
	if ((S.Length() > 2) && S[1] == '{') {
		S.Delete(S.Length(), 1);
		S.Delete(1, 1);
	}
	return S;
}
//---------------------------------------------------------------------------
// Not inside the function below due to compiler warning
static bool __initialised = false;
static String __NodeName;

// Generate Seq UUID like SRV:21EC2020-3AEA-4069-A2DD-08002B30309D
String SesamUuidCreateNodeSequentialString()
{
	if (!__initialised) {
		TIniFile* IniFile = new TIniFile("C:\\Ntsys\\LocalData\\Network.ini");
		__NodeName = IniFile->ReadString("LocalNode", "NodeName", "");
		delete IniFile;
		__initialised = true;
	}

	//
	String S = __NodeName + L":" + SesamUuidCreateSequentialString();
	return S;
}
//---------------------------------------------------------------------------
bool SesamUuidTest(TStrings* Lines)
{
	GUID guid;
	Lines->Add("WinUuidCreateSequential");
	for (int i=0; i < 10; i++) {
		WinUuidCreateSequential(&guid);
		String S = System::Sysutils::GUIDToString(guid);
		Lines->Add(S);
	}

	Lines->Add("SesamUuidCreateSequential");
	for (int i=0; i < 10; i++) {
		SesamUuidCreateSequential(&guid);
		String S = System::Sysutils::GUIDToString(guid);
		Lines->Add(S);
	}

	Lines->Add("UuidCreate");
	for (int i=0; i < 10; i++) {
		UuidCreate(&guid);
		String S = System::Sysutils::GUIDToString(guid);
		Lines->Add(S);
	}

	Lines->Add("CreateGUID");
	for (int i=0; i < 10; i++) {
		TGUID Uid;
		CreateGUID(Uid);
		String S = System::Sysutils::GUIDToString(Uid);
		Lines->Add(S);
	}

	return true;
}
//===========================================================================
DWORD PACKAGE SesamHashCRC32(String Str)
{
	// Convert because UpdateCRC32 works on bytes
	AnsiString AStr = Str;

	// Calculate CRC for this string
	DWORD CRC = UpdateCRC32(0xFFFFFFFF, AStr.c_str(), AStr.Length());

	return CRC;
}
//---------------------------------------------------------------------------
// Calculate MD5, using HashLib (CryptoAPI.cpp)
String SesamHashMD5(String Str)
{
	String Hash;
	Cryptoapi::HashStr(HASH_MD5, Str, Hash);
	return Hash;
}
//---------------------------------------------------------------------------
// Return the most significant 64 bits of an MD5 (16/01/15)
__int64 PACKAGE SesamHashMD5AsInt64(String Str)
{
	String Hash;
	Cryptoapi::HashStr(HASH_MD5, Str, Hash);

	__int64 Result = SesamHashMD5ToInt64(Hash);
	return Result;
}
//---------------------------------------------------------------------------
// Return the most significant 64 bits of hex string (06/10/2015)
__int64 PACKAGE SesamHashMD5ToInt64(String Hash)
{
	__int64 Result = 0;
	String HashMSB = Hash.SubString(1, 16);
	TryStrToInt64("$" + HashMSB, Result);
	return Result;
}
//---------------------------------------------------------------------------


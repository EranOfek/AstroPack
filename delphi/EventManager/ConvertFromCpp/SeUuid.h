//---------------------------------------------------------------------------
#ifndef SeUuidH
#define SeUuidH

#include <Vcl.h>

//---------------------------------------------------------------------------

// Windows wrapper (UuidCreateSequential)
HRESULT PACKAGE WinUuidCreateSequential(UUID* guid);

// Create UUID string, without { } (based on Windows UuidCreate)
String  PACKAGE SesamUuidCreateString();
//---------------------------------------------------------------------------
// Based on WINE source source
HRESULT PACKAGE SesamUuidCreateSequential(UUID *Uuid);
String  PACKAGE SesamUuidCreateSequentialString();

//
String  PACKAGE SesamUuidCreateNodeSequentialString();
//---------------------------------------------------------------------------
String SesamNewUuid()
{
	return SesamUuidCreateSequentialString();
}
//---------------------------------------------------------------------------
String SesamNewSuid()
{
	return SesamUuidCreateNodeSequentialString();
}
//---------------------------------------------------------------------------
bool PACKAGE SesamUuidTest(TStrings* Lines);
//---------------------------------------------------------------------------
DWORD  PACKAGE SesamHashCRC32(String Str);
String PACKAGE SesamHashMD5(String Str);

// Return the most significant 64 bits of an MD5 (16/01/15)
__int64 PACKAGE SesamHashMD5AsInt64(String Str);

// Return the most significant 64 bits of hex string (06/10/2015)
__int64 PACKAGE SesamHashMD5ToInt64(String Hash);

//---------------------------------------------------------------------------
#endif


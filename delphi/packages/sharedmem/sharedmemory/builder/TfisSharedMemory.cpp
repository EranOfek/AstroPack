//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TfisSharedMemory.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TfisSharedMemory *)
{
    new TfisSharedMemory(NULL);
}
//---------------------------------------------------------------------------
__fastcall TfisSharedMemory::TfisSharedMemory(TComponent* Owner)
    : TComponent(Owner)
{
    FShareName = "";
    FTimeout = -1;
    FSize = 0;
    FReadOnly = false;
    FHandle = NULL;
    FMutex = NULL;
}
//---------------------------------------------------------------------------
__fastcall TfisSharedMemory::~TfisSharedMemory(void)
{
    CloseMemory();
}
//---------------------------------------------------------------------------
void __fastcall TfisSharedMemory::SetName(AnsiString aValue)
{
    bool lChange = (ComponentState.Contains(csDesigning) &&
        (Name == FShareName || FShareName.Length() == 0));
    TComponent::SetName(aValue);
    if(lChange)
    {
        FShareName = Name;
    }
}
//---------------------------------------------------------------------------
void *__fastcall TfisSharedMemory::MapMemory(void)
{
    if(FHandle == NULL) return(NULL);
    DWORD lMapping;
    if(FReadOnly)
    {
        lMapping = FILE_MAP_READ;
    }
    else
    {
        lMapping = FILE_MAP_WRITE;
    }
    void *lMapPtr = MapViewOfFile(FHandle, lMapping, 0, 0, FSize);
    if(lMapPtr == NULL)
    {
        ReleaseMutex(FMutex);
    }
    return(lMapPtr);
}
//---------------------------------------------------------------------------
bool __fastcall TfisSharedMemory::UnMapMemory(void *aMapPtr)
{
    if(FHandle != NULL)
    {
        UnmapViewOfFile(aMapPtr);
        return(true);
    }
    return(false);
}
//---------------------------------------------------------------------------
bool __fastcall TfisSharedMemory::CreateMemory(void)
{
    if(FHandle != NULL) return(false);
    FHandle = CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE, 0,
        FSize, FShareName.c_str());
    if(FHandle == NULL || (FHandle != NULL && GetLastError() == ERROR_ALREADY_EXISTS))
    {
        CloseMemory();
        return(false);
    }
    AnsiString lMutexName = FShareName + MUTEX_NAME;
    FMutex = CreateMutex(NULL, false, lMutexName.c_str());
    if(FMutex == NULL)
    {
        CloseMemory();
        return(false);
    }
    return(true);
}
//---------------------------------------------------------------------------
bool __fastcall TfisSharedMemory::OpenMemory(void)
{
    if(FHandle == NULL)
    {
        FHandle = OpenFileMapping(FILE_MAP_ALL_ACCESS, true, FShareName.c_str());
        if(FHandle != NULL)
        {
            AnsiString lMutexName = FShareName + MUTEX_NAME;
            FMutex = OpenMutex(MUTEX_ALL_ACCESS, true, lMutexName.c_str());
            if(FMutex != NULL)
            {
                return(true);
            }
            CloseMemory();
        }
    }
    return(false);
}
//---------------------------------------------------------------------------
bool __fastcall TfisSharedMemory::CloseMemory(void)
{
    if(FHandle != NULL)
    {
        CloseHandle(FHandle);
        FHandle = NULL;
    }
    if(FMutex != NULL)
    {
        CloseHandle(FMutex);
        FMutex = NULL;
    }
    return(true);
}
//---------------------------------------------------------------------------
bool __fastcall TfisSharedMemory::RequestOwnership(void)
{
    if(FHandle != NULL)
    {
        DWORD lTimeout;
        if(FTimeout < 0)
        {
            lTimeout = INFINITE;
        }
        else
        {
            lTimeout = FTimeout;
        }
        return(WaitForSingleObject(FMutex, lTimeout) == WAIT_OBJECT_0);
    }
    return(false);
}
//---------------------------------------------------------------------------
bool __fastcall TfisSharedMemory::ReleaseOwnership(void)
{
    if(FHandle != NULL)
    {
        return(ReleaseMutex(FMutex));
    }
    return(false);
}
//---------------------------------------------------------------------------
namespace Tfissharedmemory
{
    void __fastcall PACKAGE Register()
    {
        TComponentClass classes[1] = {__classid(TfisSharedMemory)};
        RegisterComponents("FISH", classes, 0);
    }
}
//---------------------------------------------------------------------------



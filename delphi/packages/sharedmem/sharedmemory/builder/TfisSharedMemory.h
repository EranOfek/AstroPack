//---------------------------------------------------------------------------
#ifndef TfisSharedMemoryH
#define TfisSharedMemoryH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
#define MUTEX_NAME      "_SMMutex"
//---------------------------------------------------------------------------
class PACKAGE TfisSharedMemory : public TComponent
{
private:
    AnsiString FShareName;
    int FSize;
    HANDLE FHandle, FMutex;
    bool FReadOnly;
    int FTimeout;
protected:
    virtual void __fastcall SetName(AnsiString aValue);
public:
    void *__fastcall MapMemory(void);
    bool __fastcall UnMapMemory(void *aMapPtr);
    bool __fastcall CreateMemory(void);
    bool __fastcall OpenMemory(void);
    bool __fastcall CloseMemory(void);
    bool __fastcall RequestOwnership(void);
    bool __fastcall ReleaseOwnership(void);
    __fastcall TfisSharedMemory(TComponent* Owner);
    __fastcall ~TfisSharedMemory(void);
    __property HANDLE Handle = {read = FHandle};
    __property HANDLE Mutex = {read = FMutex};
__published:
    __property bool ReadOnly = {read = FReadOnly, write = FReadOnly, default = false};
    __property AnsiString ShareName = {read = FShareName, write = FShareName};
    __property int Size = {read = FSize, write = FSize};
    __property int Timeout = {read = FTimeout, write = FTimeout, default = -1};
};
//---------------------------------------------------------------------------
#endif

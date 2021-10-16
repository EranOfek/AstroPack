//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SeEventData.h"
#include "SeCrc.hpp"
#include <Stdio.h>

#define _SMEVENT_

#ifdef _SMEVENT_
	#include "SesamEventSM.h"
#endif

#pragma package(smart_init)
//---------------------------------------------------------------------------
TNotifyEvent OnDeleteEventData = NULL;
TNotifyEvent OnDeleteACLog     = NULL;

//===========================================================================
//
//                               TEventData
//
//===========================================================================
TEventData::TEventData()
{
    Func            = efNone;
    TextColor       = clBlack;
    BkgColor        = clWhite;

    EventRef        = 0;
    UpdateRef       = 0;
    Index           = 0;
    Flags           = 0;
    Priority        = 1;

    DT              = 0;
    ProcessDT       = 0;
    RestoreDT       = 0;
    UserAckDT       = 0;
	ResetDT         = 0;

    EventCode       = 0;

    HistTableRef    = 0;
    HistRecordIndex = 0;


	// All events originated by V10 must have this flag set
	AuxFlags        = EVAFL_V10;
    Serial          = 0;

    // Enterprise server data
    EntHistTableRef     = 0;
    EntHistRecordIndex  = 0;

    // Local data
    LocalFlags          = EVLF_VISIBLE;
    LocalTableId        = 0;
    PageNumber          = 0;
    TableId             = 0;
    AutoResetTimeDT     = 0;
    AutoRestoreTimeDT   = 0;

    VtTree          = NULL;
    VtNode          = NULL;

    ProtFlags       = 0;

    //ObjectEventCode = 0;

    EventTypeFlags  = 0;
    EventTypeExFlags= 0;

    LinkedCamerasList = NULL;

    // Shared memory
    SMTableId       = 0;
    SMRef           = 0;
    SMData          = NULL;

    // (01/12/09)
	ClearTextColor  = (TColor)0;
    ClearBkgColor   = (TColor)0;

	//------------------------------------------------------------------- V10
    Aux2Flags       = 0;
	CmdFlags		= 0;

    // (07/11/2010)
    ChildEventList  = NULL;
    Latitude        = 0;
    Longitude       = 0;
    Altitude        = 0;
    Speed           = 0;
    Course          = 0;
}
//---------------------------------------------------------------------------
TEventData::~TEventData()
{
    // Remove from shared memory
    if (SMRef) {
        DeleteSM();
    }

    if (OnDeleteEventData) {
        try {
            OnDeleteEventData((TObject*)this);
        }
        catch (Exception& E) {
        }
    }

    // Unlink event from display, so InvalidateEvent will not work on deleted
    // object (added 29/09/06)
    VtTree          = NULL;
	VtNode          = NULL;

    if (LinkedCamerasList) {
        delete LinkedCamerasList;
        LinkedCamerasList = NULL;
    }

	//------------------------------------------------------------------- V10
    //
    if (ChildEventList) {
        delete ChildEventList;
        ChildEventList = NULL;
    }
}
//---------------------------------------------------------------------------
#ifdef never
void TEventData::Assign(TEventData& Ev, int Flags)
{
    // VirtualTree pointers are not assigned
    void* _VtTree = VtTree;
    void* _VtNode = VtNode;

    // Don't assign History reference
    // (Enterprise server writes its own history)
    if (Flags & EST_NO_ENT_HIST) {
        DWORD tr = EntHistTableRef;
        DWORD ri = EntHistRecordIndex;
        *this = Ev;
        EntHistTableRef = tr;
        EntHistRecordIndex = ri;
    }
	else {
        *this = Ev;
    }

    VtTree = _VtTree;
    VtNode = _VtNode;
}
#endif
//---------------------------------------------------------------------------
void TEventData::Update(TEventData& Ev, int AssignFlags)
{
    EventRef            = Ev.EventRef;
    Flags               = Ev.Flags;
    Priority            = Ev.Priority;
	EventCode           = Ev.EventCode;
    UpdateRef           = Ev.UpdateRef;
    TextColor           = Ev.TextColor;
    BkgColor            = Ev.BkgColor;

    ObjectName          = Ev.ObjectName;
    Type                = Ev.Type;
    Description         = Ev.Description;
    Map                 = Ev.Map;
    Message             = Ev.Message;
    Instruction         = Ev.Instruction;

    EUnit               = Ev.EUnit;
    Site                = Ev.Site;
    Customer            = Ev.Customer;
    Channel             = Ev.Channel;

    MemoText            = Ev.MemoText;

    DT                  = Ev.DT;
    UserAckDT           = Ev.UserAckDT;
    ProcessDT           = Ev.ProcessDT;
    RestoreDT           = Ev.RestoreDT;
    ResetDT             = Ev.ResetDT;

	AuxFlags            = Ev.AuxFlags;              //????
    LongInstruction     = Ev.LongInstruction;
    InstructionName     = Ev.InstructionName;       // (03/12/07)

    if (Ev.User != "")
        User = Ev.User;

    if (Ev.Serial != 0)
        Serial = Ev.Serial;

    if (Ev.EventNetRef != "")
        EventNetRef = Ev.EventNetRef;

    if (Ev.PanelText != "")
        PanelText = Ev.PanelText;

    if (Ev.SubType != "")
        SubType = Ev.SubType;

    // (07/11/10)
    ParentNetRef    = Ev.ParentNetRef;

	//------------------------------------------------------ Version 10
	//
	if (Ev.ChildEventList) {
		if (!ChildEventList)
			ChildEventList = new TStringList;

		ChildEventList->Assign(Ev.ChildEventList);
	}
	else {
		if (ChildEventList) {
			delete ChildEventList;
			ChildEventList = NULL;
		}
	}

	//
	Aux2Flags	 	= Ev.Aux2Flags;
	CmdFlags		= Ev.CmdFlags;
	UserData        = Ev.UserData;
	FlowId          = Ev.FlowId;
	FlowItemId      = Ev.FlowItemId;
	FlowData        = Ev.FlowData;
	FlowItemData    = Ev.FlowItemData;
	AddrData        = Ev.AddrData;
	GisAddress      = Ev.GisAddress;
	Latitude        = Ev.Latitude;
	Longitude       = Ev.Longitude;
	Altitude        = Ev.Altitude;
	Speed           = Ev.Speed;
	Course          = Ev.Course;
	IconName        = Ev.IconName;

    //
    AddEventText    = Ev.AddEventText;
}
//===========================================================================
//                           TEventData Functions
//===========================================================================
// For compatibility with installed systems:
//
// Any new fields MUST be added at the END !!!
//
// The order of Put() calls must match the order of Get() calles in GetEventData()
//
//

bool TEventData::PutEventData(TSTextMsg& Msg)
{
    TEventData& Event = *this;

	Msg.PutInt  (Event.Func             );
    Msg.PutInt  (Event.EventRef         );
    Msg.PutInt  (Event.Index            );
    Msg.PutDWORD(Event.Flags            );
    Msg.PutDWORD(Event.Priority         );
	Msg.PutDWORD(Event.EventCode        );
    Msg.PutDWORD(Event.HistTableRef     );
    Msg.PutDWORD(Event.HistRecordIndex  );
    Msg.PutDWORD(Event.TextColor        );
    Msg.PutDWORD(Event.BkgColor         );
    Msg.Put     (Event.ObjectName       );
    Msg.Put     (Event.Type             );
    Msg.Put     (Event.Description      );
    Msg.Put     (Event.Map              );
    Msg.Put     (Event.Message          );
	Msg.PutEsc  (Event.Instruction      );		// Esc (01/12/2015)
    Msg.Put     (Event.EUnit            );
    Msg.Put     (Event.Site             );
    Msg.Put     (Event.Customer         );
    Msg.Put     (Event.Channel          );
    Msg.PutLines(Event.MemoText         );

    Msg.PutDateTime(Event.DT            );
    Msg.PutDateTime(Event.UserAckDT     );
    Msg.PutDateTime(Event.ProcessDT     );
    Msg.PutDateTime(Event.RestoreDT     );
    Msg.PutDateTime(Event.ResetDT       );

	Msg.PutDWORD(Event.AuxFlags);
	Msg.Put     (Event.LongInstruction);
	Msg.PutDWORD(Event.Serial);
	Msg.Put     (Event.EventNetRef);
	Msg.Put     (Event.User);
	Msg.PutDWORD(Event.UpdateRef);
	Msg.PutEsc  (Event.PanelText);				// Esc (01/12/2015)
	Msg.Put     (Event.SubType);
	Msg.Put     (Event.InstructionName);

	//------------------------------------------------------ Version 10
	// (07/11/10)
	Msg.PutDWORD(Event.Aux2Flags);
	Msg.PutDWORD(Event.CmdFlags);
	Msg.Put     (Event.ParentNetRef);

	//
	if (Event.ChildEventList)
        Msg.Put(ChildEventList->CommaText);
    else
        Msg.Put("");

    Msg.Put     (Event.UserData);
    Msg.Put     (Event.FlowId);             // (05/01/11)
    Msg.Put     (Event.FlowItemId);         // (05/01/11)
    Msg.Put     (Event.FlowData);
    Msg.Put     (Event.FlowItemData);
    Msg.Put     (Event.AddrData);
    Msg.Put     (Event.GisAddress);
    Msg.PutFloat(Event.Latitude);
    Msg.PutFloat(Event.Longitude);
    Msg.PutFloat(Event.Altitude);
    Msg.PutFloat(Event.Speed);
    Msg.PutFloat(Event.Course);
    Msg.Put     (Event.AddEventText);
    Msg.Put     (Event.IconName);

    return true;
}
//---------------------------------------------------------------------------
// For compatibility with installed systems:
//
// Any new fields MUST be added at the END !!!

bool TEventData::GetEventData(TSTextMsg& Msg)
{
    TEventData& Event = *this;

    try {
        Event.Func            = (TEventFunc) Msg.GetInt();
		Event.EventRef        = Msg.GetInt();
        Event.Index           = Msg.GetInt();
        Event.Flags           = Msg.GetDWORD();
        Event.Priority        = Msg.GetDWORD();
        Event.EventCode       = Msg.GetDWORD() ;
        Event.HistTableRef    = Msg.GetDWORD();
        Event.HistRecordIndex = Msg.GetDWORD();
        Event.TextColor       = (TColor) Msg.GetDWORD();
        Event.BkgColor        = (TColor) Msg.GetDWORD();
        Event.ObjectName      = Msg.Get();
        Event.Type            = Msg.Get();
        Event.Description     = Msg.Get();
        Event.Map             = Msg.Get();
        Event.Message         = Msg.Get();
		Event.Instruction     = Msg.GetEsc();		// Esc (01/12/2015)
        Event.EUnit           = Msg.Get();
        Event.Site            = Msg.Get();
        Event.Customer        = Msg.Get();
		Event.Channel         = Msg.Get();
		Event.MemoText        = Msg.GetLines();
		Event.DT              = Msg.GetDateTime();
		Event.UserAckDT       = Msg.GetDateTime();
		Event.ProcessDT       = Msg.GetDateTime();
		Event.RestoreDT       = Msg.GetDateTime();
		Event.ResetDT         = Msg.GetDateTime();
		Event.AuxFlags        = Msg.GetDWORD();
		Event.LongInstruction = Msg.Get();
		Event.Serial          = Msg.GetDWORD();
		Event.EventNetRef     = Msg.Get();
		Event.User            = Msg.Get();
		Event.UpdateRef       = Msg.GetDWORD();
		Event.PanelText       = Msg.GetEsc();		// Esc (01/12/2015)
		Event.SubType         = Msg.Get();
		Event.InstructionName = Msg.Get();

		//------------------------------------------------------ Version 10
		// (07/11/10)
		Event.Aux2Flags       = Msg.GetDWORD();
		Event.CmdFlags        = Msg.GetDWORD();
		Event.ParentNetRef    = Msg.Get();

		// ChildEventList
		String ChildListText  =  Msg.Get();
		if (ChildListText.Length() > 0) {
			if (!Event.ChildEventList)
				Event.ChildEventList = new TStringList;

			Event.ChildEventList->CommaText = ChildListText;
		}
		else {
			if (Event.ChildEventList) {
				delete Event.ChildEventList;
				Event.ChildEventList = NULL;
			}
		}

        //
        Event.UserData        = Msg.Get();
        Event.FlowId          = Msg.Get();      // (05/01/11)
        Event.FlowItemId      = Msg.Get();      // (05/01/11)
        Event.FlowData        = Msg.Get();
        Event.FlowItemData    = Msg.Get();
        Event.AddrData        = Msg.Get();
        Event.GisAddress      = Msg.Get();
        Event.Latitude        = Msg.GetFloat();
        Event.Longitude       = Msg.GetFloat();
        Event.Altitude        = Msg.GetFloat();
        Event.Speed           = Msg.GetFloat();
        Event.Course          = Msg.GetFloat();
        Event.AddEventText    = Msg.Get();
        Event.IconName        = Msg.Get();

        return true;
    }
    catch (Exception& E) {
		return false;
	}
}
//---------------------------------------------------------------------------
// Xor the two DWORDs that compose a TDateTime (double, 64 bits)
inline DWORD CheckDT(TDateTime& DT)
{
    DWORD* x = (DWORD*)&DT;
    return x[0] ^ x[1];
}
//---------------------------------------------------------------------------
bool TEventData::GetEventChecksum(DWORD& Ref, DWORD& Check)
{
    Ref = EventRef;
    Check = EventCode ^ Flags ^ AuxFlags ^ UpdateRef ^ CheckDT(DT) ^
        CheckDT(UserAckDT) ^ CheckDT(ProcessDT) ^ CheckDT(RestoreDT) ^ CheckDT(ResetDT);

    return true;
}
//---------------------------------------------------------------------------
String TEventData::GetEventKey()
{
	if (EventKey == "") {
		AnsiString AObjectName = ObjectName;
		DWORD CRC = UpdateCRC32(0xFFFFFFFF, AObjectName.c_str(), AObjectName.Length());
		EventKey.sprintf(L"%08X%08X", CRC, EventRef ^ CheckDT(DT));
	}

	return EventKey;
}
//---------------------------------------------------------------------------
bool TEventData::Updated()
{
    UpdateSM();

    return true;
}
//---------------------------------------------------------------------------
void TEventData::AddLinkedCameras(TStrings* AList)
{
    // Create list
    if (!LinkedCamerasList)
        LinkedCamerasList = new TStringList;

    // Add items
    for (int i=0;  i < AList->Count;  i++) {
        if (LinkedCamerasList->IndexOf(AList->Strings[i]) < 0)
            LinkedCamerasList->Add(AList->Strings[i]);
    }
}
//===========================================================================
//                          Shared Memory Support
//===========================================================================
bool TEventData::AddSM()
{
    #ifdef _SMEVENT_
    static bool Check = true;

    // Validate data size
    if (Check) {
        Check = false;

        TSEventData SData;

        #pragma warn -8066
        #pragma warn -8008
        if (sizeof(TSMEventData) > sizeof(SData.dwData)) {
            MessageBoxA(NULL, "Error at EventData.cpp#300, sizeof(TSMEventData) > sizeof(TSEventData.dwData)",
                "Error", MB_OK | MB_SYSTEMMODAL);

            throw Exception("Error at EventData.cpp#300, sizeof(TSMEventData) > sizeof(TSEventData.dwData)");
        }
        #pragma warn +8066
        #pragma warn +8008
    }

    if (!SMRef) {
        SMData = SEventAddEventData(SMTableId, &SMRef);
        CopyToSM();
    }
    #endif

    return true;
}
//---------------------------------------------------------------------------
bool TEventData::DeleteSM()
{
    #ifdef _SMEVENT_
    if (SMRef) {
        SEventDeleteEventData(SMTableId, SMRef);
        SMTableId   = 0;
        SMRef       = 0;
        SMData      = NULL;
    }
    #endif

    return true;
}
//---------------------------------------------------------------------------
bool TEventData::UpdateSM(bool ForceAdd)
{
    #ifdef _SMEVENT_
    if (SMRef) {
        CopyToSM();
    }
    else {
        if (ForceAdd) {
            AddSM();
            if (!SMRef) {
                return false;
            }
        }
        else {
            return false;
        }
    }
    #endif
    return true;
}
//---------------------------------------------------------------------------
bool TEventData::UpdateFromSM()
{
    #ifdef _SMEVENT_

    #endif

    return true;
}
//---------------------------------------------------------------------------
bool TEventData::CopyToSM()
{
    #ifdef _SMEVENT_
    if (!SMRef || !SMData)
        return false;

    // TSMEventData* Data = SEventGetEventData(SMTableId, SMRef);

    TSMEventData* Data = GetSMEventData(SMData);

    if (!Data)
        return false;


    Data->Func                  = Func;
    Data->EventRef              = EventRef;
    Data->Index                 = Index;
    Data->Flags                 = Flags;
    Data->Priority              = Priority;
    Data->EventCode             = EventCode;
    Data->HistTableRef          = HistTableRef;
    Data->HistRecordIndex       = HistRecordIndex;
    Data->UpdateRef             = UpdateRef;
    Data->TextColor             = TextColor;
    Data->BkgColor              = BkgColor;

	// Displayed data
	SEventStringCpy(Data->ObjectName,   AnsiString(ObjectName));
	SEventStringCpy(Data->Type,         AnsiString(Type));
	SEventStringCpy(Data->Description,  AnsiString(Description));
	SEventStringCpy(Data->Map,          AnsiString(Map));
	SEventStringCpy(Data->Message,      AnsiString(Message));
	SEventStringCpy(Data->Instruction,  AnsiString(Instruction));
	SEventStringCpy(Data->EUnit,        AnsiString(EUnit));
	SEventStringCpy(Data->Site,         AnsiString(Site));
	SEventStringCpy(Data->Customer,     AnsiString(Customer));
	SEventStringCpy(Data->Channel,      AnsiString(Channel));
	SEventStringCpy(Data->MemoText,     AnsiString(MemoText));

    // Time
    Data->DT                    = DT;
    Data->UserAckDT             = UserAckDT;
    Data->ProcessDT             = ProcessDT;
    Data->RestoreDT             = RestoreDT;
    Data->ResetDT               = ResetDT;

    // Additional data
    Data->AuxFlags              = AuxFlags;

	SEventStringCpy(Data->LongInstruction,  AnsiString(LongInstruction));
	SEventStringCpy(Data->User,             AnsiString(User));

    Data->Serial                = Serial;
	SEventStringCpy(Data->EventNetRef,      AnsiString(EventNetRef));

    // Used only by Enterprise Server
    Data->EntHistTableRef       = EntHistTableRef;
    Data->EntHistRecordIndex    = EntHistRecordIndex;

    // Local data - not transfered over the network *NO ASSIGN*
    Data->LocalFlags            = LocalFlags;
    Data->LocalTableId          = LocalTableId;
    Data->PageNumber            = PageNumber;
    Data->TableId               = TableId;
    Data->AutoResetTimeDT       = AutoResetTimeDT;
    Data->AutoRestoreTimeDT     = AutoRestoreTimeDT;    

    Data->VtTree                = VtTree;
    Data->VtNode                = VtNode;

    // Network Protocol Data *NO ASSIGN*
    Data->ProtFlags             = ProtFlags;
	SEventStringCpy(Data->ProtSourceNode, AnsiString(ProtSourceNode));
	SEventStringCpy(Data->ProtTargetNode, AnsiString(ProtTargetNode));

    // Additional data
    //DWORD       ObjectEventCode;

    //
	SEventStringCpy(Data->PanelText, AnsiString(PanelText));
    Data->EventTypeFlags        = EventTypeFlags;
    Data->EventTypeExFlags      = EventTypeExFlags;
	SEventStringCpy(Data->EventKey, AnsiString(EventKey));

    // Shared memory support
    Data->SMTableId             = SMTableId;
    Data->SMRef                 = SMRef;

    #endif

    return true;
}
//---------------------------------------------------------------------------
// Not supported yet
bool TEventData::CopyFromSM()
{
    #ifdef _SMEVENT_never

    TSEventData* Data =

    SEventStringCpy

    Data->Func;
    Data->EventRef;
    Data->Index;
    Data->Flags;
    Data->Priority;
    Data->EventCode;
    Data->HistTableRef;
    Data->HistRecordIndex;
    Data->UpdateRef;
    Data->TextColor;
    Data->BkgColor;

    // Displayed data
    Data->ObjectName;
    Data->Type;
    Data->Description;
    Data->Map;
    Data->Message;
    Data->Instruction;

    // Centra data
    Data->EUnit;
    Data->Site;
    Data->Customer;
    Data->Channel;

    // Display data
    Data->MemoText;

    // Time
    Data->DT;
    Data->UserAckDT;
    Data->ProcessDT;
    Data->RestoreDT;
    Data->ResetDT;

    // Additional data
    Data->AuxFlags;
    Data->LongInstruction;
    Data->User;
    Data->Serial;
    Data->EventNetRef;

    // Used only by Enterprise Server
    Data->EntHistTableRef;
    Data->EntHistRecordIndex;

    // Local data - not transfered over the network *NO ASSIGN*
    Data->LocalFlags;
    Data->LocalTableId;
    Data->PageNumber;
    Data->TableId;
    Data->AutoResetTimeDT;
    Data->AutoRestoreTimeDT;


    Data->VtTree;
    Data->VtNode;

    // Network Protocol Data *NO ASSIGN*
    Data->ProtFlags;
    Data->ProtSourceNode;
    Data->ProtTargetNode;

    // Additional data
    //DWORD       ObjectEventCode;

    //
    Data->PanelText;
    Data->EventTypeFlags;
    Data->EventTypeExFlags;
    Data->EventKey;

    // Shared memory support
    Data->SMTableId;
    Data->SMRef;

    #endif

    return true;
}
//===========================================================================
void TEventData::AddChildEvent(String ChildRef)
{
    if (!ChildEventList)
        ChildEventList = new TStringList;

    if (ChildEventList->IndexOf(ChildRef) < 0)
        ChildEventList->Add(ChildRef);
}
//---------------------------------------------------------------------------
void TEventData::RemoveChildEvent(String ChildRef)
{
    if (ChildEventList) {
        int Index = ChildEventList->IndexOf(ChildRef);
        if (Index > -1)
            ChildEventList->Delete(Index);
    }
}
//===========================================================================
//
//===========================================================================
static String DText(String AName, String AValue)
{
	return AName + L": " + AValue + L" - ";
}
//---------------------------------------------------------------------------
static String DPad(String AName, int Len)
{
	if (AName.Length() < Len)
		AName += String::StringOfChar(' ', Len-AName.Length());

    return AName;
}
//---------------------------------------------------------------------------
void TEventData::DumpDebugText(String& S)
{
    TEventData& Event = *this;

	try {
		S =

		DText(L"NetRef",         DPad(Event.EventNetRef, 8)) +
		DText(L"EventRef",       String().sprintf(L"%6d",  Event.EventRef)) +
		DText(L"Flags",          String().sprintf(L"%08X", Event.Flags)) +
		DText(L"Object",         DPad(Event.ObjectName, 24)) +
		DText(L"EventCode",      String().sprintf(L"%4d",  Event.EventCode)) +
		DText(L"Type",           DPad(Event.Type, 16)) +

		DText(L"DT",             DPad(String().sprintf(L"%5.3lf",   Event.DT), 9)) +
		DText(L"Restore",        DPad(String().sprintf(L"%5.3lf",   Event.RestoreDT), 9)) +
		DText(L"Ack",            DPad(String().sprintf(L"%5.3lf",   Event.UserAckDT), 9)) +
		DText(L"Process",        DPad(String().sprintf(L"%5.3lf",   Event.ProcessDT), 9)) +

		DText(L"AuxFlags",       String().sprintf(L"%08X",    Event.AuxFlags)) +
		DText(L"Serial",         String().sprintf(L"%6d",     Event.Serial));
	}
	catch (Exception& E) {
	}
}
//===========================================================================
//                                TEventMemo
//===========================================================================
// (25/06/14) - To be completed - EventNetRef + MemoItemSuid


TEventMemo::TEventMemo(String AText)
{
    List        = new TList;
    EventIndex  = 0;            // (02/09/13)

    if (AText != "")
        LoadFromText(AText);
}
//---------------------------------------------------------------------------
TEventMemo::~TEventMemo()
{
    Clear();
    delete List;
}
//---------------------------------------------------------------------------
void TEventMemo::Clear()
{
    for (int i=0;  i < List->Count;  i++) {
        TEventMemoItem* Item = (TEventMemoItem*)List->Items[i];
        delete Item;
    }

    List->Clear();
}
//---------------------------------------------------------------------------
TEventMemoItem* TEventMemo::AddItem()
{
    TEventMemoItem* Item = new TEventMemoItem;
    Item->Index = List->Count + 1;
    List->Add(Item);
    return Item;
}
//---------------------------------------------------------------------------
void TEventMemo::DeleteItem(TEventMemoItem* Item)
{
    List->Remove(Item);
    delete Item;

    // Update index for all items
    for (int i=0;  i < List->Count;  i++) {
        TEventMemoItem* Item = (TEventMemoItem*)List->Items[i];
        Item->Index = i+1;
    }
}
//---------------------------------------------------------------------------
int TEventMemo::ItemCount()
{
	return List->Count;
}
//---------------------------------------------------------------------------
TEventMemoItem* TEventMemo::GetItem(int Index)
{
    if (Index >= 0 && Index < List->Count)
        return (TEventMemoItem*)List->Items[Index];
    else
        return NULL;
}
//---------------------------------------------------------------------------
bool TEventMemo::LoadFromText(String AText)
{
	Clear();

	// Parse text (this code was taken from EventUtil.cpp, SetGridFromText())
	while (AText.Length() > 0) {
		// Get a line - look for "\n"
		String Line;
		int EndOfLine = AText.Pos("\n");
		if (EndOfLine > 0) {
			Line = AText.SubString(1, EndOfLine-1);
			AText.Delete(1, EndOfLine);
		}
		else {
			Line = AText;
			AText = "";
		}

        if (Line.Trim() == "")
            break;

        TEventMemoItem*Item = AddItem();

        // Parse text - cells are separated by "\t"
        int Col = 0;
        while (Line != "") {
            String  CellText;
            int     EndOfCell = Line.Pos("\t");

            if (EndOfCell > 0) {
                CellText = Line.SubString(1, EndOfCell-1);
                Line.Delete(1, EndOfCell);
            }
            else {
                CellText = Line;
                Line = "";
            }

			// Put text in cell
            Col++;
            if (Col == 1) {
                try {
                    if (CellText == "")
                        Item->DT = (int)Now();
                    else
                        Item->DT = TDateTime(CellText, TDateTime::DateTime);
				}
                catch (Exception& E) {
                    Item->DT = (int)Now();
                }
            }

			if (Col == 2) Item->Text       	  = CellText;
			if (Col == 3) Item->Comment    	  = CellText;

			if (Col == 4) {
				if (CellText.Pos("-") > 0)
					Item->MemoItemSuid  = CellText;
				else
					Item->MemoIndex     = CellText.ToIntDef(0);   // (02/09/13)
			}

			// Changed from 4,5 to 5,6, because we added MemoIndex in v9 (INL)
			if (Col == 5) Item->UserRespRef	= CellText;
			if (Col == 6) Item->UserFields 	= CellText;
        }
    }

    return true;
}
//---------------------------------------------------------------------------
String TEventMemo::SaveToText()
{
    String AText;

    // Update index for all items
    for (int i=0;  i < List->Count;  i++) {
        TEventMemoItem* Item = (TEventMemoItem*)List->Items[i];

        String TimeStr;

        if (DateTimeFormat != "")
            TimeStr = Item->DT.FormatString(DateTimeFormat);
        else
            TimeStr = Item->DT.DateTimeString();

		// Added UserFields (04/06/12)
        // MemoIndex added (02/09/13)		
		// Added Item->ResponseIndex, it was probably missing because above in LoadFromText we write it (18/09/13)
		String Line;

		if (Item->MemoItemSuid.IsEmpty())
			 Line = TimeStr + "\t" + Item->Text + "\t" + Item->Comment + "\t" + String(Item->MemoIndex) + "\t" + Item->UserRespRef + "\t" + Item->UserFields + "\n";
		else
			 Line = TimeStr + "\t" + Item->Text + "\t" + Item->Comment + "\t" + String(Item->MemoItemSuid) + "\t" + Item->UserRespRef + "\t" + Item->UserFields + "\n";

		AText += Line;
	}

	return AText;
}
//---------------------------------------------------------------------------
TACLogEvent::TACLogEvent()
{
	EventRef        = 0;
	Index           = 0;
	Flags           = 0;
	Priority        = 1;
	EventCode       = 0;
	HistTableRef    = 0;
    HistRecordIndex = 0;
    TextColor       = clBlack;
    BkgColor        = clWhite;
    AuxFlags        = 0;
    ChIndex         = 0;
    EntHistTableRef = 0;
    EntHistRecordIndex = 0;
    LocalFlags      = ACLF_VISIBLE;
    VtNode          = NULL;
}
//---------------------------------------------------------------------------
TACLogEvent::~TACLogEvent()
{
    if (VtNode) {
        if (OnDeleteACLog)
            OnDeleteACLog((TObject*)this);

        // Unlink event from display, so InvalidateEvent will not work on deleted
        // object (added 08/06/15)
        VtNode = NULL;
    }
}
//============================================================================
// (30/06/15)
String TEventData::GetTextParam(String& AText, String Param, bool ARemove)
{
    String Result;
    String Start = Param + "=";
    String Stop  = "/" + Param;

    int StartP = AText.Pos(Start);
    if (StartP > 0) {
        int StopP = AText.Pos(Stop);

        if (StopP == 0)
            StopP = AText.Length()+1;

        if (StopP > StartP) {
            int P = StartP + Start.Length();
            Result = AText.SubString(P, StopP - P);

            if (ARemove) {
                AText.Delete(StartP, StopP-StartP+1 + Stop.Length());
            }
        }
    }

    return Result;
}
//---------------------------------------------------------------------------
int TEventData::GetTextParamCommaTextList(String& AText, String Param, TStrings* List)
{
    String Value = TEventData::GetTextParam(AText, Param);
    List->CommaText = Value;
    return List->Count;
}
//---------------------------------------------------------------------------



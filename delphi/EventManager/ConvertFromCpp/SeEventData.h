//---------------------------------------------------------------------------
#ifndef SeEventDataH
#define SeEventDataH

#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Db.hpp>
#include <DBTables.hpp>
#include "VirtualTrees.hpp"

#include "SesamTextMsg.h"
#include "SesamEventSM.h"

//===========================================================================
//
//===========================================================================
struct PACKAGE TEvmMsg;
struct PACKAGE TEventData;
struct PACKAGE TEventMemo;
struct PACKAGE TEventMemoItem;
struct PACKAGE TACLogEvent;

// Defined in SesamEventSM.h
struct TSEventData;
struct PACKAGE TSMEventData;

//===========================================================================
//                             TEventFunc
//===========================================================================
// New functions must be added at end of enum definition
//
// DO NOT CHANGE ORDER of TEventFunc items. Add new items only at the end
//

enum TEventFunc {
	efNone                  = 0,
    efAddEvent              = 1,        // Add event
    efClearEvent            = 2,        // Clear (restore) event
	efRemoveEvent           = 3,        // Remove event from table
    efUpdateEvent           = 4,        // Update events data by owner of the event (by EventRef)
    efUpdateEventByStation  = 5,        // Update event data by station (sent to server)
	efResetEventByStation   = 6,        // Reset single event by station
    efProcessEventByStation = 7,        // Event processed by station
    efAckAllByStation       = 8,        // Ack by station
    efResetAllByStation     = 9,        // Reset all by station
    efResetMultiByStation   = 10,       // Reset multi by station
    efSilenceAll            = 11,       // Sent by server to silence all stations
    efTestModeSetup         = 12,       // Test-Mode Setup, data is in Description field  (added 08/01/04)
    efAckEvent              = 13,       // Ack single event by object name (added 11/01/04)
    efResetEvent            = 14,       // Reset single event (added 11/01/04)
	efForceResetEvent       = 15,       // Force reset single event
    efAckSingleEvent        = 16,       // Ack single event by event ref (added 25/09/05)
    efClickEvent            = 17,       // Event has been clicked (added 29/12/05)
    efSchedTaskSetup        = 18,       // Scheduled tasks setup (added 06/04/06)
    efOpenMap               = 19,       // Used to send OpenMap message from EvmSesamIfc.cpp to SesamMapManager (03/06/09)
    efLast                  = 19        // Always the code of last function
};

// Added (24/08/08)
#include "SesamEventDataSM.h"

//===========================================================================

//===========================================================================
// Event Manager Network Protocol Commands
// These commands are sent between Event Managers over the network
//
// Commands are composed and parsed using TSTextMsg() class that produce
// a text message string.
// It is send to the kernel as the Value field of MSG_SET_OBJPROP_BY_REMOTE message.
//
//      Command
//      Flags
//      SenderNodeName
//      TargetNodeName
//

// ClearTable Node Flags
// Receiver should clear all events of objects of Node
#define MSG_EVT_CLEAR_TABLE_OLD      "ClearTable"            // Clear table

//
#define MSG_EVT_TABLE_DUMP_CLEAR     "TableDumpClear"        // Clear table

// TableDumpStart Node Flags
#define MSG_EVT_TABLE_DUMP_START    "TableDumpStart"        // Start


// TableDumpEnd Node Flags
#define MSG_EVT_TABLE_DUMP_END      "TableDumpEnd"          // End


// TableDumpRequest Node Flags MaxEvents
// Request
#define MSG_EVT_TABLE_DUMP_REQUEST  "TableDumpRequest"      // Request


// TableDumpStop Node Flags
#define MSG_EVT_TABLE_DUMP_STOP     "TableDumpStop"         // Stop sending any more events

// Show Events Table
#define MSG_EVT_TABLE_SHOW          "ShowEventsTable"

// Alive
#define MSG_EVT_ALIVE               "Alive"                 // Alive signal

// EventData ...
#define MSG_EVT_EVENT_DATA          "EventData"             // TEventData

// Alive message Flags bits
#define MSG_EVT_ALIVE_FL_ACTIVE_SERVER      0x00000001      // Active server
#define MSG_EVT_ALIVE_FL_STANDBY_SERVER     0x00000002      // Standby server
#define MSG_EVT_ALIVE_FL_BACKUP_EVENTS      0x00000004      // Backup server has events for the Primary

struct PACKAGE TEvmMsg {
	String      Command;
	String      SourceNodeName;
	String      TargetNodeName;
    DWORD       Flags;
};
//---------------------------------------------------------------------------
// TEventData::AuxFlags - Status Flags
#define EVAFL_BLINK             0x00000001      // Event blinks icon
#define EVAFL_PAGER             0x00000002      // Pager message
#define EVAFL_ROUTINE           0x00000004      // Event should be placed in Routine table
#define EVAFL_NOTABLE           0x00000008      // Event is not inserted to table
#define EVAFL_TESTMODE          0x00000010      // Test-Mode event
#define EVAFL_NETWORK           0x00000020      // Network event
#define EVAFL_BLINK_EVENT_SLOW  0x00000040      // Blink event line in table slow
#define EVAFL_BLINK_EVENT_FAST  0x00000080      // Blink event line in table fast
#define EVAFL_BLINK_EVENT       0x000000C0      // Blink event line in table slow or fast
#define EVAFL_BLINK_EVENT_STATE 0x00000100      // Blink state
#define EVAFL_NEED_ACK_RESTORE  0x00000200      // Event needs ack for its restore (added 05/03/06 INEEL)
#define EVAFL_OBJCAMERAS        0x00000400      // Event has linked cameras (23/12/09)
#define EVAFL_OBJCAMERAS_VIEWED 0x00000800      // Event has linked cameras (23/12/09)

// V10
#define EVAFL_V10				0x00001000		// Event contains V10 extensions (06/09/12)

// Reserved, for common V9/V10 future use
#define EVAFL_RESERVED2         0x00002000
#define EVAFL_RESERVED3			0x00004000
#define EVAFL_RESERVED4			0x00008000

// TEventData::AuxFlags - Command Flags
#define EVAFL_CMD_FORCE_RESET   0x00010000      // Force reset
#define EVAFL_CMD_APPEND        0x00020000      // Append event at end of list (used by DUMP commands)
#define EVAFL_CMD_FORCE_RESET_GHOST	0x00040000  // Force reset ghost event (added 29/09/06)

// Reserved, for common V9/V10 future use...
#define EVAFL_RESERVED5			0x00080000

//------------------------------------------------------------------- V10
// TEventData::Aux2Flags - New V10 Status Flags
#define EVA2FL_MANUAL_EVENT     0x00000001      // Manual event                         (02/01/11)
#define EVA2FL_EVENT_CLOSED     0x00000002      // Event has been closed by user        (02/11/10)
#define EVA2FL_HAS_PARENT       0x00000004      // Event has parent event ParentNetRef  (02/01/11)
#define EVA2FL_HAS_FLOW         0x00000008      // Event has flow data                  (02/01/11)
#define EVA2FL_FLOW_STARTED		0x00000010		// Flow item has been started			(04/09/12)
#define EVA2FL_FLOW_CHECKED     0x00000020      // Event has been checked by user       (02/11/10)

//------------------------------------------------------------------- V10

// TEventData::AuxFlags - Command Flags
// New V10 - Values changed by Chen, 20/02/11
#define EVCMDFL_CLOSE_EVENT		0x00000001      // Close manual/flow event
#define EVCMDFL_FLOWITEM_START	0x00000002      // (04/09/12)
#define EVCMDFL_FLOWITEM_CHECK	0x00000004      // Check flow sub-event
//#define EVCMDFL_FLOW_UNCHECK  	0x00000008      // Uncheck flow sub-event

//---------------------------------------------------------------------------
// Events Table Columns
#define FN_INDEX        "fn#"
#define FN_DATETIME     "fnDateTime"
#define FN_RESTOREDT    "fnRestoreDT"
#define FN_PROCESSDT    "fnProcessDT"
#define FN_ACKDT        "fnAckDT"
#define FN_OBJECT       "fnObject"
#define FN_EVENT        "fnEvent"
#define FN_DESCR        "fnDescription"
#define FN_MESSAGE      "fnMessage"
#define FN_MAP          "fnMap"
#define FN_CUSTOMER     "fnCustomer"
#define FN_SITE         "fnSite"
#define FN_ENDUNIT      "fnEndUnit"
#define FN_NODE_TITLE   "fnNodeTitle"
#define FN_USER         "fnUser"
#define FN_SERIAL       "fnSerial"
#define FN_PRIORITY     "fnPriority"
#define FN_ACCOUNT		"fnAccount"		// (10/01/2016)
//---------------------------------------------------------------------------
// System sound files, played from C:\Ntsys\Audio
#define SOUND_ALARM		"alarm.wav"
#define SOUND_FAULT		"fault.wav"
#define SOUND_DISABLE	"disable.wav"
#define SOUND_ENABLE	"enable.wav"
#define SOUND_DAY		"day.wav"
#define SOUND_NIGHT		"night.wav"

//===========================================================================
//                           ACLog Field Names
//===========================================================================
// Field Names
#define ACFN_SERIAL       "fn#"                 // Event serial number
#define ACFN_DATETIME     "fnDateTime"          // Date & time of event
#define ACFN_OBJECT       "fnObject"            // Reader name
#define ACFN_EVENT        "fnEvent"             // Event
#define ACFN_DESCR        "fnDescription"       // Reader description
#define ACFN_MESSAGE      "fnMessage"           // Reader message
#define ACFN_MAP          "fnMap"               // Reader map
#define ACFN_TAGCODE      "fnTagCode"           // Tag code
#define ACFN_CHNAME       "fnChName"            // Cardholder name
#define ACFN_CHID         "fnChId"              // Cardholder ID
#define ACFN_NODE_TITLE   "fnNodeTitle"         // Node title
#define ACFN_PRIORITY     "fnPriority"          // Priority
#define ACFN_TRACE        "fnTrace"             // Trace
#define ACFN_DETAILS	  "fnDetails"			// Details (09/04/2019)
#define ACFN_PANELTEXT	  "fnPanelText"			// Panel Text (09/04/2019)

//===========================================================================
//                                 TACLogEvent
//===========================================================================

// Flags
#define ACEF_BLINK              0x00010000          // Blink
#define ACEF_TRACE              0x00020000          // Traced cardholder
#define ACEF_TRACE_ALARM        0x00040000          // Alarm on trace
#define ACEF_LPR				0x00080000			// LPR event (09/04/2019)

// Local Flags
#define ACLF_VISIBLE            0x00000001          // Event is visible

//===========================================================================
#define EST_NO_ENT_HIST     0x0010  // No EntHistTableRef, EntHistRecordIndex

#define EST_DEFAULT         EST_NO_ENT_HIST

// Local Flags - Move to slib.h
#define EVLF_VISIBLE        0x00000001      // Visible in events table
#define EVLF_BOOKMARK       0x00000002      // Show event in bookmarks page
#define EVLF_TESTMODE       0x00000004      // Show event in Test-Mode page
#define EVLF_NETWORK        0x00000008      // Show event in Network page

//===========================================================================
//
//                               TEventData
//
//===========================================================================
// Fields marked as *NO ASSIGN* are not assigned by the Assign() fuctions
// because they are local
//
// When adding new fields, make sure to update the Assign() function properly.

struct PACKAGE TEventData {
	TEventFunc  Func;                   // Protocol function to perform
	int         EventRef;               // Internal reference for event
    int         Index;                  // Index
    DWORD       Flags;                  // Event type flags, EVFL_... received in SesamIfc.cpp from Kernel AddEvent message
    DWORD       Priority;               // Priority
    DWORD 		EventCode;              // Event code
    DWORD		HistTableRef;           // History table ref
    DWORD		HistRecordIndex;        // History table record index
    DWORD       UpdateRef;              // Incremented each time data is updated
    TColor      TextColor;              // Text color
    TColor      BkgColor;               // Background color

    // Displayed data
	String      ObjectName;             // Object name
    String      Type;                   // Object/event type string
	String      Description;            // Object description
	String      Map;                    // Map name
    String      Message;                // Object/event message
    String      Instruction;            // Instruction

	// Centra data
    String      EUnit;                  // End unit
	String      Site;                   // Site
	String      Customer;               // Customer
	String      Channel;                // Channel

	// Display data
	String      MemoText;               // Memo text

	// Time
	TDateTime   DT;                     // Start time
	TDateTime   UserAckDT;              // User ack
	TDateTime   ProcessDT;              // User process
	TDateTime   RestoreDT;              // Restore
	TDateTime	ResetDT;                // Reset

	// Additional data
	DWORD       AuxFlags;               // Auxilary flags, EVAFL_ used to manage the event	 (04/09/12)
	String      LongInstruction;        // When starts with '@' - its the file name, otherwise - the text
	String      User;                   // Current user logged in
	DWORD       Serial;                 // Event serial number
	String      EventNetRef;            // Event reference on network

	// Used only by Enterprise Server
	DWORD		EntHistTableRef;        //*NO ASSIGN*
	DWORD		EntHistRecordIndex;     //*NO ASSIGN*

	// Local data - not transfered over the network *NO ASSIGN*
	DWORD       LocalFlags;             // Local flags, EVLF_...
	DWORD       LocalTableId;           // Table identifier when using multiple events table
	DWORD       PageNumber;             // Page number to display the event (in TPageControl object)
	DWORD       TableId;                // Events table number (added 18/01/2006)
    double      AutoResetTimeDT;        // Auto reset time for this event, in TDateTime units
	double      AutoRestoreTimeDT;      // Auto restore time for this event, in TDateTime units (31/03/09)

	void*       VtTree;                 // TVirtualStringTree
    void*       VtNode;                 // TVirtualNode

	// Network Protocol Data *NO ASSIGN*
    DWORD       ProtFlags;              // Flags
    String      ProtSourceNode;         // Source node name
	String      ProtTargetNode;         // Target node name

    // Additional data
    //DWORD       ObjectEventCode;

    //
	String      PanelText;              // Panel text that created the event (for DLAN)
	DWORD       EventTypeFlags;         // Event-type flags
    DWORD       EventTypeExFlags;       // Event-type extened flags
    String      EventKey;               // Unique event key

    // Linked cameras
    TStringList* LinkedCamerasList;     //

    // Shared memory support
	DWORD       SMTableId;              // Shared memory table id
    DWORD       SMRef;                  // Shared memory reference, 0=NULL
    TSEventData* SMData;                // Shared memory data

	// (27/08/07)
    String      SubType;                // Event sub-type, to allow multiple events for the same object

	// (03/12/07)
    String      InstructionName;        // We need the instruction name, to locate it in the SQL database

	// (01/12/09)
	TColor      ClearTextColor;         // Text color
	TColor      ClearBkgColor;          // Background color

	//------------------------------------------------------- Version 10 (starting 01/12/2009)
	DWORD       Aux2Flags;              // Auxilary2 flags, EVA2FL_  used to manage the event (04/09/12)
	DWORD       CmdFlags;               // Command flags,   EVCMDFL_ used to manage the event (04/09/12)

	// GIS
	String       GisAddress;
	double       Latitude;
	double       Longitude;
	double       Altitude;
	double       Speed;
	double       Course;

	// Manual-Event, FlowChart, Sub-Events
	String          ParentNetRef;           // EventNetRef of parent event
	TStringList*    ChildEventList;         // List of EventNetRef of child events
	String          UserData;               // User defined data fields Name=Value as CommaText
	String          FlowId;                 //
	String          FlowItemId;             // Flow ItemId for sub-events that are linked to specific flow item
	String          FlowData;               // As CommaText
	String          FlowItemData;           // As CommaText
	String          AddrData;               // As CommaText
	String          IconName;               //

	// Used to pass extra values from TEvmSesamInterface::PerformActionVer53()
	// (passed by Kernel) to TEventsTableForm::ProcessEventData()
	String          AddEventText;           // Will be parsed as CommaText

	//=======================================================================
	//
	//=======================================================================
	TEventData();
    //-----------------------------------------------------------------------
	~TEventData();
    //-----------------------------------------------------------------------
	void Update(TEventData& Ev, int AssignFlags = EST_DEFAULT);
	//-----------------------------------------------------------------------
	bool IsVisible();
	//-----------------------------------------------------------------------
    bool PutEventData(TSTextMsg& Msg);
    //-----------------------------------------------------------------------
    bool GetEventData(TSTextMsg& Msg);
    //-----------------------------------------------------------------------
    bool GetEventChecksum(DWORD& Ref, DWORD& Check);
    //-----------------------------------------------------------------------
	void DumpDebugText(String& S);
    //-----------------------------------------------------------------------
	String GetEventKey();
    //-----------------------------------------------------------------------
	bool Updated();
    //-----------------------------------------------------------------------
	void AddLinkedCameras(TStrings* AList);
    //-----------------------------------------------------------------------
    bool AddSM();
	//-----------------------------------------------------------------------
    bool DeleteSM();
    //-----------------------------------------------------------------------
	bool UpdateSM(bool ForceAdd = false);
	//-----------------------------------------------------------------------
    bool UpdateFromSM();
    //-----------------------------------------------------------------------
    bool CopyToSM();
    //-----------------------------------------------------------------------
	bool CopyFromSM();
    //-----------------------------------------------------------------------
	static TSMEventData* GetSMEventData(TSEventData* SData);
	//-----------------------------------------------------------------------
	void AddChildEvent(String ChildRef);
	//-----------------------------------------------------------------------
	void RemoveChildEvent(String ChildRef);
	//-----------------------------------------------------------------------
    // (17/07/2019)
    static String GetTextParam(String& AText, String Param, bool ARemove = false);
    static int GetTextParamCommaTextList(String& AText, String Param, TStrings* List);
};
//---------------------------------------------------------------------------
bool TEventData::IsVisible()
{
	return (LocalFlags & EVLF_VISIBLE);
}
//---------------------------------------------------------------------------
TSMEventData* TEventData::GetSMEventData(TSEventData* SData)
{
	return SData ? (TSMEventData*) ((char*)&SData->dwData) : NULL;
}
//===========================================================================
//                     Global Notification Functions
//===========================================================================
extern PACKAGE TNotifyEvent OnDeleteEventData;
extern PACKAGE TNotifyEvent OnDeleteACLog;

//===========================================================================
//                              Event Memo
//===========================================================================
// Event Memo Item
struct TEventMemoItem {
    int         Index;          // Item index
    TDateTime   DT;             // Date/Time when added
    int         Flags;
    int         Param;
    __int64     MemoIndex;      // Should be replacd by MemoItemSuid (02/09/13)
    String      Text;           // Text (action)
	String      Comment;        // User comment (free text)
	String		UserFields;		// As CommaText (04/06/12)
	String		UserRespRef;	// Primary key for CMEVURDET, used in EvmEventsTableForm.cpp (added 20/08/12)
	String		MemoItemSuid;	// SUID field of record in history database (25/06/14)

	TEventMemoItem();
};

//
TEventMemoItem::TEventMemoItem()
{
	Index 	= 0;
	DT 		= Now();
	Flags 	= 0;
	Param 	= 0;
    MemoIndex = 0;    // (02/09/13)	
}
//---------------------------------------------------------------------------
// Event Memo Data
struct TEventMemo {
public:
	TEventMemo(String AText = "");
	//-----------------------------------------------------------------------
    ~TEventMemo();
    //-----------------------------------------------------------------------
    void Clear();
    //-----------------------------------------------------------------------
    TEventMemoItem* AddItem();
    //-----------------------------------------------------------------------
    void DeleteItem(TEventMemoItem* Item);
    //-----------------------------------------------------------------------
	int ItemCount();
    //-----------------------------------------------------------------------
    TEventMemoItem* GetItem(int Index);
	//-----------------------------------------------------------------------
    bool LoadFromText(String AText);
    //-----------------------------------------------------------------------
    String SaveToText();
    //-----------------------------------------------------------------------
    // Data
	String  EventKey;           // Event key
	String  DateTimeFormat;
	TList*  List;               // List of TEventMemoItem
	__int64 EventIndex;         // (02/09/13)
	String	EventNetRef;		// Link to event (instead of EventIndex) (25/06/14)
};

//===========================================================================
//                           Access-Control Events
//===========================================================================
enum TACLogEventFunc { acfNone = 0 };
//---------------------------------------------------------------------------
struct PACKAGE TACLogEvent {
    //-----------------------------------------------------------------------
    TACLogEvent();
    //-----------------------------------------------------------------------
	~TACLogEvent();
    //-----------------------------------------------------------------------
    TACLogEventFunc     Func;
    int                 EventRef;
    int                 Index;
    DWORD               Flags;
	DWORD               Priority;
	DWORD 		        EventCode;
    DWORD		        HistTableRef;
    DWORD		        HistRecordIndex;
	TColor              TextColor;
	TColor              BkgColor;
    String              ObjectName;
    String              Type;
    String              Description;
	String              Map;
	String              Message;
    String              Instruction;
    String              MemoText;
    TDateTime           DT;
    DWORD               AuxFlags;

	String              TagCode;
    int                 ChIndex;
    String              ChLastName;
	String              ChFirstName;
	String              ChName;
    String              ChId;
    String              NodeName;
    String              ChRef;
    String              Department;             // For Avri parking (26/10/09)
    String              Company;                // (26/10/09)

    // Enterprise Server data
	DWORD		        EntHistTableRef;
	DWORD		        EntHistRecordIndex;

    // Local Data (not transfered on network)
    DWORD               LocalFlags;
    int                 VisibleIndex;
    TVirtualNode*       VtNode;

    // Audit data (10/03/10)
    String              AccessLevels;
	String              Readers;
	String              TimeZones;
	String              NetName;
	String              UploadStatus;
	String				EventNetRef;			// (25/06/14)

	// 2019
	String				Details;				// (09/04/2019)
	String				PanelText;				// (09/04/2019)
};
//---------------------------------------------------------------------------
#endif



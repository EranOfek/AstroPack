//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SeZDbUtl.h"
#include <Registry.hpp>
#include <winsock2.h>
#include <FileCtrl.hpp>

#include "SesamProcessReg.h"
#include "SesamConf.h"
#include "SesamAps.h"
#include "SeUuid.h"
#include "SeZDbQuery.h"

#include <SQLMemMain.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ZConnection"
#pragma link "ZAbstractDataset"
#pragma link "ZAbstractRODataset"
#pragma link "ZDataset"
#pragma link "SQLMemMain"
#pragma link "ZAbstractConnection"
#pragma resource "*.dfm"
TZDBUtil *ZDBUtil = NULL;

//===========================================================================
bool __fastcall IsSQLException(Exception& E)
{
    EZSQLException* Ex = dynamic_cast<EZSQLException*>(&E);
    return (Ex != NULL);
}
//---------------------------------------------------------------------------
int __fastcall GetSQLErrorCode(Exception& E)
{
    EZSQLException* Ex = dynamic_cast<EZSQLException*>(&E);
    int ErrorCode = 0;
    if (Ex) {
        ErrorCode = Ex->ErrorCode;
    }
    return ErrorCode;
}
//===========================================================================
//                                TCopySQL
//===========================================================================
__fastcall TCopySQL::TCopySQL(TZQuery* ZQuery)
{
    SQLText = ZQuery->SQL->Text;
    Params = new TParams;
    Params->Assign(ZQuery->Params);
}
//---------------------------------------------------------------------------
__fastcall TCopySQL::TCopySQL(TSQLMemQuery* MemQuery)
{
    SQLText = MemQuery->SQL->Text;
    Params = new TParams;
    Params->Assign(MemQuery->Params);
}

//---------------------------------------------------------------------------
__fastcall TCopySQL::~TCopySQL()
{
    if (Params) delete Params;
}
//---------------------------------------------------------------------------
bool __fastcall TCopySQL::Set(TZQuery* ZQuery)
{
    ZQuery->SQL->Text = SQLText;
    ZQuery->Params->Assign(Params);

    return true;
}
//---------------------------------------------------------------------------
bool __fastcall TCopySQL::Set(TSQLMemQuery* MemQuery)
{
    MemQuery->SQL->Text = SQLText;
    MemQuery->Params->Assign(Params);

    return true;
}
//===========================================================================
//                                  TZDBUtil
//===========================================================================
__fastcall TZDBUtil::TZDBUtil(TComponent* Owner)
    : TForm(Owner)
{
    // (11/01/10)
	LogFile     = new TLogFile(this, "$MOD$-ZDBUtil");
    CFbLogFile  = new TLogFile(this, "$MOD$-ZDBUtil-CheckFB");

    // Force WinSockets to initialize
    WSADATA wsaData;
    WSAStartup(0x0101, &wsaData);

    //
    ReconnectInterval   = 60000;
    DBTemplatePath      = "C:\\Ntsys\\Bin\\";  //ExtractFilePath(ParamStr(0));

    //
    PasswordList    = new TStringList;
    ConnectionList  = new TList;

    //
    Username    = SConfGetFirebirdUser();      //"SYSDBA";
    Password    = SConfGetFirebirdPassword();  //"masterkey";

    for (int i=0;  i < 9;  i++) {
        String APassword = SConfGetFirebirdPassword(i);
        AddPassword(APassword);
    }

    if (Password != "masterkey")
        PasswordList->Add("masterkey");

    //
    MainThreadId = GetCurrentThreadId();

    // (26/01/14)
    FShowMessages = true;
}
//---------------------------------------------------------------------------
__fastcall TZDBUtil::~TZDBUtil()
{
}
//===========================================================================
//                                  Database
//===========================================================================
String __fastcall TZDBUtil::GetField(TDataSet* DataSet, String FieldName, String Default)
{
    String S;

    TField* Field = DataSet->FindField(FieldName);

    if (Field && !Field->IsNull)
        S = Field->AsString;
    else S = Default;

    return S;
}
//---------------------------------------------------------------------------
// Get LargeInt field from DataSet (required because in BCB5 Variant does not support LargeInt)
__int64 __fastcall TZDBUtil::GetFieldInt64(TDataSet* DataSet, String FieldName, __int64 Default)
{
    __int64 Value = Default;

    TLargeintField* F = (TLargeintField*)DataSet->FindField(FieldName);
    if (F && F->DataType == ftLargeint && !F->IsNull)
        Value = F->AsLargeInt;

    // Try Integer (17/09/13)
    else if (F && F->DataType == ftInteger && !F->IsNull)
        Value = F->AsInteger;
        

    return Value;
}
//===========================================================================
//                                  Firebird
//===========================================================================
bool __fastcall TZDBUtil::IsFirebirdRunning()
{
    // Check mutex
	HANDLE hMutex = OpenMutexA(MUTEX_ALL_ACCESS, false, "FIREBIRD_CONNECT_MUTEX");
    if (hMutex) CloseHandle(hMutex);

    //
    bool Result = (hMutex != 0);

    // Mutex not found (of failed to open?)
    if (!Result) {

        // The above test is not working on Vista (why?), so we assume that Firebird is running
        if (IsWindowsVista())
            Result = true;

        // Allow bypass (21/01/09)
		else if (GetPrivateProfileIntA("Database", "FirebirdServiceRunning", 0, SESAM_INIFILE))
            Result = true;
    }

    return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TZDBUtil::IsWindowsVista()
{
    OSVERSIONINFO osvi;
    BOOL bIsWindowsXPorLater;

    ZeroMemory(&osvi, sizeof(OSVERSIONINFO));
    osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

    GetVersionEx(&osvi);

    //bool bIsWindowsXPorLater =
    //   ( (osvi.dwMajorVersion > 5) ||
    //   ( (osvi.dwMajorVersion == 5) && (osvi.dwMinorVersion >= 1) ));

    bool bIsWindowsVista = (osvi.dwMajorVersion >= 6);

    return bIsWindowsVista;
}
//---------------------------------------------------------------------------
// Connect to Firebird database
bool __fastcall TZDBUtil::FBConnectDatabase(TZConnection* ZConnection, TCustomIniFile* IniFile, String Section, String DBFileName, bool ACopy)
{
    // Disconnect
    ZConnection->Connected = false;

    // Load parameters from ini file
    if (IniFile) {
        ZConnection->Database  = IniFile->ReadString(Section,   "Database", ZConnection->Database);
        ZConnection->HostName  = IniFile->ReadString(Section,   "Host",     ZConnection->HostName);
        ZConnection->User      = IniFile->ReadString(Section,   "User",     ZConnection->User);
        ZConnection->Password  = IniFile->ReadString(Section,   "Password", ZConnection->Password);
    }

    // Connect
    try {
        ZConnection->Connected = true;
    }
    catch (Exception& E) {
    }

    // Failed, and database is local
    if (!ZConnection->Connected && IsLocalHost(ZConnection->HostName)) {

        if (DBFileName == "")
            DBFileName = ZConnection->Database;

		String DBTemplateFileName = DBTemplatePath + "_" + ExtractFileName(DBFileName);

		// First, try v10 template (31/07/13)
		String DBTemplateFileNameV10 = ChangeFileExt(DBTemplateFileName, "10.FDB");
		if (FileExists(DBTemplateFileNameV10))
			DBTemplateFileName = DBTemplateFileNameV10;

		try {
			// Database not found, try to copy from Bin directory
			if (!FileExists(DBFileName) && FileExists(DBTemplateFileName)) {
				CopyFileW(DBTemplateFileName.c_str(), DBFileName.c_str(), FALSE);
            }

            // Try to connect again
            ZConnection->Connected = false;
            ZConnection->Connected = true;
        }
        catch (Exception& E) {
        }
    }

    // Failed to connect
    if (!ZConnection->Connected && FShowMessages) {
		String Msg = String(L"Cannot connect to FirebirdSQL database: ") + ZConnection->Database + L", Host: " + ZConnection->HostName;
		String sTitle = L"Database error";
		MessageBoxW(NULL, Msg.c_str(), sTitle.c_str(), MB_OK | MB_SYSTEMMODAL);
	}

    return ZConnection->Connected;
}
//---------------------------------------------------------------------------
bool __fastcall TZDBUtil::FBVerifyDatabaseFile(String DBFileName, String DBTemplateFileName)
{
    bool Result = false;

    if (FileExists(DBFileName)) {
        Result = true;
    }

    else {
		if (DBTemplateFileName == "")
			DBTemplateFileName = DBTemplatePath + "_" + ExtractFileName(DBFileName);

		// First, try v10 template (31/07/13)
		String DBTemplateFileNameV10  = ChangeFileExt(DBTemplateFileName, "10.FDB");
		if (FileExists(DBTemplateFileNameV10))
			DBTemplateFileName = DBTemplateFileNameV10;

		// Database not found, try to copy from Bin directory
		if (!FileExists(DBFileName) && FileExists(DBTemplateFileName)) {
			if (CopyFileW(DBTemplateFileName.c_str(), DBFileName.c_str(), FALSE)) {
                Result = true;
            }
            else if (FShowMessages) {
				String Msg = String(L"Cannot copy database file from template: ") + DBFileName;
				String sTitle = L"Database error";

				MessageBoxW(NULL, Msg.c_str(), sTitle.c_str(), MB_OK | MB_SYSTEMMODAL);
			}
        }
    }

    //
    return Result;
}
//---------------------------------------------------------------------------
// Get new value from Firebird Generator
__int64 __fastcall TZDBUtil::FBAutoInc(TZQuery* ZQuery, String GeneratorName, int Increment)
{
    ZQuery->Close();
    ZQuery->SQL->Text = "SELECT GEN_ID (" + GeneratorName + "," + String(Increment) + ") FROM RDB$DATABASE";
    ZQuery->Open();

    return GetFieldInt64(ZQuery, "GEN_ID");
}
//---------------------------------------------------------------------------
// Set minimum/maximum value for Firebird Generator (19/08/13)
__int64 __fastcall TZDBUtil::FBSetMinMaxAutoInc(TZQuery* ZQuery, String GeneratorName, __int64 MinVal, __int64 MaxVal)
{
	// Get current value
    ZQuery->Close();
    ZQuery->SQL->Text = "SELECT GEN_ID (" + GeneratorName + ",0) FROM RDB$DATABASE";
    ZQuery->Open();

    __int64 CurVal = GetFieldInt64(ZQuery, "GEN_ID");

    // Validate minimum
    if ((MinVal > 0) && (CurVal < MinVal)) {
        Log("Fixing generator " + GeneratorName + " from " + String(CurVal) + " to " + String(MinVal));
        ZQuery->Close();
        ZQuery->SQL->Text = "SET GENERATOR " + GeneratorName + " TO " + String(MinVal);
        ZQuery->ExecSQL();
    }

	// Validate maximum
    if ((MaxVal > 0) && (CurVal >= MaxVal)) {
        Log("Fixing generator " + GeneratorName + " from " + String(CurVal) + " to " + String(MinVal));
        ZQuery->Close();
        ZQuery->SQL->Text = "SET GENERATOR " + GeneratorName + " TO " + String(MinVal);
        ZQuery->ExecSQL();
    }

    // Get current value
    ZQuery->Close();
    ZQuery->SQL->Text = "SELECT GEN_ID (" + GeneratorName + ",0) FROM RDB$DATABASE";
    ZQuery->Open();

    CurVal = GetFieldInt64(ZQuery, "GEN_ID");

    return CurVal;
}
//------------------------------------------------------------------------------
__int64 __fastcall TZDBUtil::FBGetMaxValue(TZQuery* ZQuery, String TableName, String FieldName, __int64 MinValue, __int64 MaxValue)
{
	__int64 Result = -1;

	String AText = "SELECT FIRST(1) " + FieldName + " FROM " + TableName +
			" WHERE (" + FieldName + " >= " + String(MinValue) + ") AND (" +
			FieldName + " <= " + String(MaxValue) + ") ORDER BY " + FieldName + " DESC";

	// (17/09/13)
	try {
		ZQuery->Close();
		ZQuery->SQL->Text = AText;
		ZQuery->Open();

		if (!ZQuery->Eof)
			Result = GetFieldInt64(ZQuery, FieldName);
	}
	catch (Exception& E) {
		LogEx(E, "FirebirdGetMaxValue");
	}

	return Result;
}
//---------------------------------------------------------------------------
void __fastcall TZDBUtil::SetParamByName(TZQuery* Table, TZQuery* Query, String FieldName, String Value)
{
	int MaxLen;
    TField* Field = Table->FindField(FieldName);

	// Added ftWideString (14/07/13)
	if (Field && ((Field->DataType == ftString) || (Field->DataType == ftWideString)) && (Field->Size > 0))
        MaxLen = Field->Size;
    else
        MaxLen = Value.Length();

    try {
        Query->ParamByName(FieldName)->AsString = Value.SubString(1, MaxLen);
    }
	catch (Exception& E) {
		LogEx(E, "SetParamByName: " + FieldName);
    }
}
//---------------------------------------------------------------------------
// The RDB$RELATIONS.RDB$RELATION_TYPE columns is new in FB2.1, right?
// Yes, meaning:
// enum rel_t {
//     rel_persistent = 0,
//     rel_view = 1,
//     rel_external = 2,
//     rel_virtual = 3,
//     rel_global_temp_preserve = 4,
//     rel_global_temp_delete = 5
// };
//
// When gbak restore backup from Firebird 2.0 or older, RDB$RELATIONS.RDB$RELATION_TYPE field gets value NULL instead
// of 0 for all user tables. Is it a bug or intended behavior?
//
//
bool __fastcall TZDBUtil::FBGetTablesList(TZQuery* ZQuery, TStrings* List)
{
	ZQuery->Close();
	ZQuery->SQL->Text = "SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE RDB$SYSTEM_FLAG = 0 AND RDB$VIEW_BLR IS NULL ORDER BY RDB$RELATION_NAME";
	ZQuery->Open();

	while (!ZQuery->Eof) {
		String AName = GetField(ZQuery, "RDB$RELATION_NAME");
		List->Add(AName);

		ZQuery->Next();
	}

	return true;
}
//---------------------------------------------------------------------------
// (04/05/14)
bool __fastcall TZDBUtil::FBTableExists(TZQuery* ZQuery, String TableName)
{
	ZQuery->Close();
	ZQuery->SQL->Text = "SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE (RDB$SYSTEM_FLAG = 0) "
		"AND (RDB$VIEW_BLR IS NULL) AND (RDB$RELATION_NAME = '" + TableName + "') ORDER BY RDB$RELATION_NAME";
	ZQuery->Open();

	return !ZQuery->Eof;
}
//---------------------------------------------------------------------------
String __fastcall TZDBUtil::FBGetRootDir()
{
    String  Root;

    TRegistry *Registry = new TRegistry;

    try {
        Registry->RootKey = HKEY_LOCAL_MACHINE;

        // False because we do not want to create it if it doesn't exist
        Registry->OpenKey("SOFTWARE\\Firebird Project\\Firebird Server\\Instances", false);

        Root = Registry->ReadString("DefaultInstance");

    }
    catch (Exception& E) {
    }

    delete Registry;


    return Root;
}
//---------------------------------------------------------------------------
bool __fastcall TZDBUtil::FBUpdateAlias(String Alias, String& DatabaseName, bool ASet)
{
    String  Root     = FBGetRootDir();
    bool    Result   = false;

    if (Root != "") {
        String FileName = Root + "aliases.conf";

        if (FileExists(FileName)) {
            TStringList* Lines = new TStringList;

            try {
                Lines->LoadFromFile(FileName);

                // Set
                if (ASet) {
                    String Value = Lines->Values[Alias];

                    if (Value != DatabaseName) {
                        Lines->Values[Alias] = DatabaseName;
                        Lines->SaveToFile(FileName);
                        Result = true;
                    }
                }

                // Get
                else {
                    DatabaseName = Lines->Values[Alias];
                    Result = true;
                }
            }
            catch (Exception& E) {
            }

            delete Lines;
        }
    }

    return Result;
}

//===========================================================================
//                              SQL Functions
//===========================================================================
// Set the WHERE part of a simple SQL statement
String __fastcall TZDBUtil::SetWhere(String OriginalSQL, String NewWhere)
{
    String  NewText  = OriginalSQL;
    String  OrderBy;

    // Extract "ORDER BY" (29/09/10)
    int OrderByPos = NewText.Pos("ORDER BY");
    if (OrderByPos > 0) {
        OrderBy = NewText.SubString(OrderByPos, NewText.Length());
        NewText.Delete(OrderByPos, NewText.Length());
    }

    // Find WHERE part of the statement, return if not found
    int WherePos = NewText.Pos("WHERE ");

    // Delete and put the new text
    if (WherePos > 0) {
        NewText.Delete(WherePos, NewText.Length());

        // (29/09/10)
        if (NewWhere.Length() > 0)
            NewText += " WHERE ";
    }
    else {
        if (NewWhere.Length() > 0)
            NewText += " WHERE ";
    }

    // Add condition and ORDER BY (29/09/10)
    NewText += NewWhere + " " + OrderBy;

    return NewText;
}
//---------------------------------------------------------------------------
// Add WHERE part with list of conditions concated with AND/OR
String __fastcall TZDBUtil::SetWhere(String AText, TStrings* CondList, bool AndOr)
{
    // Add WHERE
    if (CondList->Count > 0)
        AText += " WHERE ";

    // Add conditions with AND/OR
    for (int i=0;  i < CondList->Count;  i++) {
        AText += "(" + CondList->Strings[i] + ")";
        if (i < CondList->Count-1)
            AText += AndOr ? " AND " : " OR ";
    }

    return AText;
}
//---------------------------------------------------------------------------
String __fastcall TZDBUtil::SetOrderBy(String OriginalSQL, String FieldName, String Direction)
{
    String NewSQL;
    String Sort;

    // Remove 'order ...' from end of SQL string
    int Position = OriginalSQL.Pos("ORDER BY");
    int Length   = OriginalSQL.Length();

    // Found ORDER BY in current SELECT statement
    if ((Position > 0) && (Direction == "TOGGLE")) {

        String AText = OriginalSQL.UpperCase().Trim();

        if (AText.SubString(AText.Length()-3, 4) == " ASC") {
            Direction = "DESCENDING";
        }
        else if (AText.SubString(AText.Length()-4, 5) == " DESC") {
            Direction = "ASCENDING";
        }

        // Remove "ORDER BY..."
        OriginalSQL.Delete(Position, Length-Position+1);
    }

    //
    if (UpperCase(Direction) == "ASCENDING")
        Sort = " ORDER BY " + FieldName + " ASC";
    else if (UpperCase(Direction) == "DESCENDING")
        Sort = " ORDER BY " + FieldName + " DESC";
    else if (UpperCase(Direction) == "TOGGLE")
        Sort = " ORDER BY " + FieldName + " ASC";

    // Prepare new SQL
    NewSQL = OriginalSQL.Trim() + Sort;

    return NewSQL;
}
//===========================================================================
//                              ZQuery SQL Functions
//===========================================================================
bool __fastcall TZDBUtil::SetNewSQL(TZQuery* Query, String NewSQL)
{
    String OriginalSQL = Query->SQL->Text;

    // Open
    try {
        Query->Close();
        Query->SQL->Text = NewSQL;
        Query->Open();
    }
    catch (Exception& E) {

        // Restore original
        try {
			Query->Close();
            Query->SQL->Text = OriginalSQL;
            Query->Open();
        }
        catch (Exception& E) {
        }
    }

    //
    return Query->Active;
}
//---------------------------------------------------------------------------
// Set the WHERE part of a simple SQL statement
bool __fastcall TZDBUtil::SetWhere(TZQuery* Query, String NewWhere)
{
    String NewSQL = SetWhere(Query->SQL->Text, NewWhere);

    return SetNewSQL(Query, NewSQL);
}
//---------------------------------------------------------------------------
bool __fastcall TZDBUtil::SetOrderBy(TZQuery* Query, String FieldName, String Direction)
{
    String NewSQL = SetOrderBy(Query->SQL->Text, FieldName, Direction);

    return SetNewSQL(Query, NewSQL);
}
//===========================================================================
//                          Sorting functions
//===========================================================================
// Apply sort to selected column, usually called from OnTitleClick event of TDBGrid
void __fastcall TZDBUtil::GridSort(TColumn* Column, String Direction)
{
	if (!Column || !Column->Field)
        return;

    TZQuery* Query = dynamic_cast<TZQuery*>(Column->Grid->DataSource->DataSet);

    if (!Query)
        return;

    if ((Column->Field->DataType != ftMemo) && (Column->Field->DataType != ftBlob) && (Column->Field->DataType != ftGraphic)) {

        TCopySQL SaveSQL(Query);

        String NewSQL = SetOrderBy(Query->SQL->Text, Column->FieldName, Direction);

        // Open
        try {
            Query->Close();
            Query->SQL->Text = NewSQL;
            Query->Open();
        }
        catch (Exception& E) {

            // Restore original
            try {
                Query->Close();
                SaveSQL.Set(Query);
                Query->Open();
            }
			catch (Exception& E) {
            }
		}
    }
}
//---------------------------------------------------------------------------
// Apply sort to selected column, usually called from OnTitleClick event of TXDBGrid
void __fastcall TZDBUtil::GridSort(TXColumn* Column, String Direction)
{
	if (!Column || !Column->Field)
		return;

	// Try TZQuery
    TZQuery* ZQuery = dynamic_cast<TZQuery*>(Column->Grid->DataSource->DataSet);
    if (ZQuery) {
		GridSortZQuery(ZQuery, Column, Direction);
        return;
	}

    // Try TSQLMemQuery
    TSQLMemQuery* MemQuery = dynamic_cast<TSQLMemQuery*>(Column->Grid->DataSource->DataSet);
    if (MemQuery) {
        GridSortMemQuery(MemQuery, Column, Direction);
        return;
	}
}
//---------------------------------------------------------------------------
// Apply sort to selected column, usually called from OnTitleClick event of TXDBGrid
void __fastcall TZDBUtil::GridSortZQuery(TZQuery* Query, TXColumn* Column, String Direction)
{
	if (!Column || !Column->Field)
        return;

    if ((Column->Field->DataType != ftMemo) && (Column->Field->DataType != ftBlob) && (Column->Field->DataType != ftGraphic)) {

        TCopySQL SaveSQL(Query);

        String NewSQL = SetOrderBy(Query->SQL->Text, Column->FieldName, Direction);

        // Open
        try {
			Query->Close();
            Query->SQL->Text = NewSQL;
            Query->Open();
        }
        catch (Exception& E) {

            // Restore original
            try {
                Query->Close();
                SaveSQL.Set(Query);
                Query->Open();
            }
            catch (Exception& E) {
            }
        }
    }
}
//---------------------------------------------------------------------------
// Apply sort to selected column, usually called from OnTitleClick event of TXDBGrid
void __fastcall TZDBUtil::GridSortMemQuery(TSQLMemQuery* Query, TXColumn* Column, String Direction)
{
	if (!Column || !Column->Field)
        return;

    if ((Column->Field->DataType != ftMemo) && (Column->Field->DataType != ftBlob) && (Column->Field->DataType != ftGraphic)) {

        TCopySQL SaveSQL(Query);

        String NewSQL = SetOrderBy(Query->SQL->Text, Column->FieldName, Direction);

        // Open
        try {
            Query->Close();
            Query->SQL->Text = NewSQL;
            Query->Open();
        }
        catch (Exception& E) {

            // Restore original
            try {
                Query->Close();
                SaveSQL.Set(Query);
                Query->Open();
            }
            catch (Exception& E) {
            }
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall TZDBUtil::SelectDistinct(String AText, String FieldName, TStrings* List, bool Clear)
{
    if (Clear)
        List->Clear();

    #ifdef never
    try {
        SelectDistinctQuery->Close();
        SelectDistinctQuery->SQL->Text = AText;
        SelectDistinctQuery->Open();

        while (!SelectDistinctQuery->Eof) {
            String Value = GetField(SelectDistinctQuery, FieldName);

            ComboListAdd(List, Value);
            if (Value != "" && List->IndexOf(Value) < 0)
                List->Add(Value);

            SelectDistinctQuery->Next();
        }
    }
    catch (Exception& E) {
    }
    #endif
}
//---------------------------------------------------------------------------
void __fastcall TZDBUtil::SelectDistinctCombo(TDataSet* DataSet, String FieldName, TElComboBox* Combo)
{
    try {
        String SaveValue;

        if (Combo->ItemIndex > -1)
            SaveValue = Combo->Items->Strings[Combo->ItemIndex];

        Combo->Items->BeginUpdate();
        Combo->Items->Clear();
        Combo->Items->Add(StrAll->Caption);

        while (!DataSet->Eof) {
            String Value = GetField(DataSet, FieldName).Trim();
            if (Value != "")
                Combo->Items->Add(Value);

            DataSet->Next();
        }

        int Index = Combo->Items->IndexOf(SaveValue);
        if (Index < 0)
            Index = 0;

        Combo->ItemIndex = Index;
    }
    catch (Exception& E) {
    }

    Combo->Items->EndUpdate();
}
//---------------------------------------------------------------------------
String __fastcall TZDBUtil::GetComboValue(TElComboBox* Combo)
{
    String Value = Combo->Text.Trim();

	if (Value == StrAll->Caption)
        Value = "";

    return Value;
}
//---------------------------------------------------------------------------
String __fastcall TZDBUtil::GetHostName()
{
    char name[128];
    gethostname(name, sizeof(name));
    return String(name);
}
//---------------------------------------------------------------------------
String __fastcall TZDBUtil::GetIpAddr()
{
    hostent* pHost = gethostbyname(NULL);
    if (pHost)
        return String(inet_ntoa(*(struct in_addr *)*pHost->h_addr_list));
    else
        return "";
}
//---------------------------------------------------------------------------
bool __fastcall TZDBUtil::IsLocalHost(String HostName)
{
	//New!
	return (HostName == "") || (AnsiString(HostName).AnsiCompareIC("localhost") == 0) ||
		(AnsiString(HostName).AnsiCompareIC(GetHostName()) == 0) || (AnsiString(HostName).AnsiCompareIC(GetIpAddr()) == 0);
}
//---------------------------------------------------------------------------
String __fastcall TZDBUtil::CreateSeqUuid()
{
	return SesamUuidCreateSequentialString();
}
//---------------------------------------------------------------------------
String __fastcall TZDBUtil::CreateNodeSeqUuid()
{
	return SesamUuidCreateNodeSequentialString();
}
//===========================================================================

//===========================================================================
// Backup Firebird database file
bool __fastcall TZDBUtil::BackupFDB(String DatabaseName, String BackupFile, bool Compress, bool DeleteFBK, DWORD Timeout, bool StructOnly)
{
    String  GBak        = "gbak";
    String  User        = SConfGetFirebirdUser();      //"SYSDBA";
    String  Password    = SConfGetFirebirdPassword();  //"masterkey";
    bool    Result;

    // Validate source file
    if (!FileExists(DatabaseName)) {
        if (FShowMessages)
            ShowMessage("TZDBUtil::BackupFDB: Database file does not exist: " + DatabaseName);
            
        return false;
    }

    // Backup file not specified
    if (BackupFile == "") {
        BackupFile = ChangeFileExt(DatabaseName, ".FBK");
    }
    else {
        ForceDirectories(ExtractFilePath(BackupFile));
    }

    // Delete current backup file
    DeleteFile(BackupFile);

    // Delete failed, we cannot run gbak if backup file still exists
    if (FileExists(BackupFile)) {
        if (FShowMessages)
            ShowMessage("TZDBUtil::BackupFDB: Cannot delete old backup file: " + BackupFile);
            
        return false;
    }

	// Run GBAK
	AnsiString Cmd = GBak + " -B -USER " + User + " -PASSWORD " + Password;

	// Metadata only (database structure)
	if (StructOnly)
        Cmd += " -M";

    //
    Cmd += " " + DatabaseName + " " + BackupFile;

    // Execute process and wait for termination
    Result = SProcExec(Cmd.c_str(), Timeout);

    if (!FileExists(BackupFile)) {
        if (FShowMessages)
            ShowMessage("TZDBUtil::BackupFDB: Failed to create backup file: " + BackupFile);
            
        return false;
    }

    // Compress FBK
    if (Compress) {
        String CurDir = GetCurrentDir();
        SetCurrentDir(ExtractFilePath(BackupFile));

        String ZipFileName = ChangeFileExt(BackupFile, ".7z");

		//
		AnsiString Cmd = "7za.exe a " + ZipFileName + " " + ExtractFileName(BackupFile) + " -mx=9 -y";
		Result = SProcExec(Cmd.c_str(), Timeout);

        //
        SetCurrentDir(CurDir);

        if (!Result) {
            if (FShowMessages)
                ShowMessage("TZDBUtil::BackupFDB: Failed to compress backup file: " + Cmd);
                
            return false;
        }

        Result = true;

        if (DeleteFBK && FileExists(ZipFileName)) {
            DeleteFile(BackupFile);
        }
    }

    return Result;
}

//===========================================================================
//                          Firebird Database Security
//===========================================================================
bool __fastcall TZDBUtil::AddPassword(String Password)
{
    if ((Password.Length() > 0) && (PasswordList->IndexOf(Password) < 0))
        PasswordList->Add(Password);

    return true;
}
//---------------------------------------------------------------------------
// Set Firebird password for specified user, by running GSEC.EXE
bool __fastcall TZDBUtil::SetFirebirdPassword(String User, String Password, String NewPassword)
{
    bool    Result;

	// Run GSEC
	AnsiString Cmd = "gsec.exe -user " + User + " -password " + Password +
                 " -modify " + User + " -pw " + NewPassword;

    // Execute process and wait for termination
    Result = SProcExec(Cmd.c_str(), 30000);

    return Result;
}
//===========================================================================
// Encrypt string, result length is 3 times the original string length
// Each character is encoded as hex number
AnsiString __fastcall TZDBUtil::EncryptPassword(AnsiString Str)
{
    AnsiString Result;

    for (int i=1;  i <= Str.Length();  i++)
        Result += AnsiString().sprintf("%03X", Str[i] ^ 0xAA);

    return Result;
}
//---------------------------------------------------------------------------
// Decrypt string
AnsiString __fastcall TZDBUtil::DecryptPassword(AnsiString Str)
{
    AnsiString Result;
    int C;

    for (int i=0;  i < Str.Length();  i+=3) {
        if (sscanf(Str.c_str()+i, "%03X", &C) == 1)
			Result += AnsiString().sprintf("%c", C ^ 0xAA);
    }

    return Result;
}
//---------------------------------------------------------------------------
// Connect to database with password from list, try all passwords in the list
// until connection is established.
String __fastcall TZDBUtil::ConnectPasswordList(TZConnection* ZConnection, TStrings* APasswordList, bool AOpen)
{
    // Use default
    if (!APasswordList)
        APasswordList = PasswordList;

    String Password;

    for (int i=0;  i < APasswordList->Count;  i++) {

        // Get password, skip empty strings
        String APassword = APasswordList->Strings[i].Trim();
        if (APassword.Length() == 0)
            continue;

        // Decode it if encrypted
        if (APassword.SubString(1, 3) == "XPW")
            APassword = DecryptPassword(APassword.SubString(4, APassword.Length()-3));

        // Try to connect
        try {
            ZConnection->Connected = false;
            ZConnection->Password  = APassword;

            ZConnection->Connect();

            // Connected, this password is valid
            if (ZConnection->Connected) {
                if (!AOpen)
                    ZConnection->Connected = false;

                Password = APassword;
                break;
            }
        }
        catch (Exception& E) {
        }
    }

    //
    return Password;
}
//---------------------------------------------------------------------------
// Connect to database with password from list, try all passwords in the list
// until connection is established.
String __fastcall TZDBUtil::ConnectPasswordListIBX(TIBDatabase* Connection, TStrings* APasswordList, bool AOpen)
{
	// Use default
	if (!APasswordList)
        APasswordList = PasswordList;

    String Password;

    for (int i=0;  i < APasswordList->Count;  i++) {

        // Get password, skip empty strings
        String APassword = APasswordList->Strings[i].Trim();
        if (APassword.Length() == 0)
            continue;

        // Decode it if encrypted
        if (APassword.SubString(1, 3) == "XPW")
            APassword = DecryptPassword(APassword.SubString(4, APassword.Length()-3));

        // Try to connect
        try {
			Connection->Connected = false;
			Connection->Params->Values["password"] = APassword;

			Connection->Connected = true;

			// Connected, this password is valid
			if (Connection->Connected) {
				if (!AOpen)
					Connection->Connected = false;

				Password = APassword;
				break;
			}
		}
		catch (Exception& E) {
		}
	}

	//
	return Password;
}
//===========================================================================
//                          	SQLMemTable
//===========================================================================
//
bool __fastcall TZDBUtil::ImportMemTableFromQuery(TZQuery* ZQuery, String ZTableName,
    Sqlmemmain::TSQLMemTable* MemTable, Sqlmemmain::TSQLMemQuery* MemQuery, bool CopyData)
{
    bool ZClose = false;

    try {
        // Select one record (or zero if empty) from ZQuery, to have the
        // dataset field names
        if (!ZQuery->Active) {
            ZClose = true;

            if ((ZQuery->SQL->Text == "") && (ZTableName != ""))
                ZQuery->SQL->Text = "SELECT FIRST(0) * FROM " + ZTableName;

            ZQuery->Open();
        }

        // Import table definition from ZQuery to MemTable
        MemTable->Close();        
        MemTable->ImportTable(ZQuery);
        MemTable->Open();

        // Delete all records (required if ZQuery was not empty)
        if (!CopyData) {
            MemTable->First();
            while (!MemTable->Eof)
            	MemTable->Delete();
        }

        // Open query
        if (MemQuery) {
            MemQuery->Close();

            // Assign SQL text
            if (MemQuery->SQL->Text == "")
                MemQuery->SQL->Text = ZQuery->SQL->Text;

            // If no SQL text, set simple SELECT statement
            if (MemQuery->SQL->Text == "")
                MemQuery->SQL->Text = "SELECT * FROM " + MemTable->TableName;

            // Open query
            MemQuery->Open();
        }

        // Close database query if it was not open before
        if (ZClose)
            ZQuery->Close();

        return true;
    }
    catch (Exception& E) {
    }

	return false;
}

//---------------------------------------------------------------------------
#ifdef _LMDEL_
// (20/01/14)
void __fastcall TZDBUtil::MemTableAddIndex(TSQLMemTable* MemTable, TXDBGrid* DBGrid)
{
    try {
        for (int i=0;  i < DBGrid->Columns->Count;  i++) {
            TXColumn* Column = DBGrid->Columns->Items[i];
            MemTable->AddIndex("Ix" + Column->FieldName, Column->FieldName, TIndexOptions() << ixCaseInsensitive);
            MemTable->AddIndex("IxDesc" + Column->FieldName, Column->FieldName, TIndexOptions() << ixCaseInsensitive << ixDescending);
        }
    }
    catch (Exception& E) {
        ShowMessage("MemTableAddIndex: " + E.Message);
    }
}
//---------------------------------------------------------------------------
// (20/01/14)
void __fastcall TZDBUtil::MemTableImport(TSQLMemTable* MemTable, TDataSet* DataSet, TXDBGrid* DBGrid)
{
    try {
        // Import table definition from ZQuery to MemTable
        MemTable->IndexName = "";
        MemTable->IndexDefs->Clear();
        MemTable->Close();
        MemTable->ImportTable(DataSet);

        // (20/01/14)
        MemTableAddIndex(MemTable, DBGrid);
        MemTable->Open();
    }
    catch (Exception& E) {
        ShowMessage("MemTableImport: " + E.Message);
    }
}
//---------------------------------------------------------------------------
// (20/01/14)
void __fastcall TZDBUtil::MemTableDBGridTitleClick(TSQLMemTable* MemTable, TXColumn *Column, String& GridSortFieldName, bool& GridSortAsc)
{
    if (!Column || !Column->Field)
        return;

    if ((Column->Field->DataType != ftMemo) && (Column->Field->DataType != ftBlob) && (Column->Field->DataType != ftGraphic)) {


        if (Column->FieldName == GridSortFieldName) {
            GridSortAsc = !GridSortAsc;
        }
        else {
            GridSortFieldName = Column->FieldName;
            GridSortAsc = true;
        }

        try {
            MemTable->IndexName = GridSortAsc ? String("Ix" + GridSortFieldName) : String("IxDesc" + GridSortFieldName);
        }
        catch (Exception& E) {
            ShowMessage("MemTableDBGridTitleClick: " + E.Message);
        }
    }
}
#endif
//===========================================================================

//===========================================================================
// Derived class to enable cess to FConnection (11/01/10)
// Required to fix a bug in TZConnection.
class TZConFix : public TZConnection {
public:
    void ClearFConnection()
        { FConnection = NULL; }
};
//---------------------------------------------------------------------------
void __fastcall ClearFConnection(TZConnection* ZCon)
{
    TZConFix* Z = (TZConFix*)ZCon;
    Z->ClearFConnection();
}
//---------------------------------------------------------------------------
bool __fastcall TZDBUtil::CheckFBConnection(TZConnection* Connection)
{
    // Call this function only from main thread
    DWORD ThreadId = GetCurrentThreadId();
    if (ThreadId != MainThreadId) {
        CFbLog("CheckFBConnection called from non-main thread, cannot check connection: ThreadId=" + String((int)ThreadId) + ", MainThreadId=" + String((int)MainThreadId), clRed);
		return true;
    }

    //CFbLog("CheckFBConnection: ConnectionName=" + Connection->Name + ", Host=" + Connection->HostName + ", Database=" + Connection->Database, clRed);

    static bool Here = false;
    if (Here) {
        CFbLog("  Alreday inside CheckFBConnection, return true");
        return true;
    }

    Here = true;

    bool AConnected = false;

    //
    try {
        AConnected = DoCheckFBConnection(Connection);
    }
    catch (Exception& E) {
    }

    Here = false;
    return AConnected;
}
//---------------------------------------------------------------------------
bool __fastcall TZDBUtil::DoCheckFBConnection(TZConnection* Connection)
{
	bool AConnected = false;

    // Search ConData
    TZDBUtilConntion* ConData = NULL;
    for (int i=0;  i < ConnectionList->Count;  i++) {
        TZDBUtilConntion* Con = (TZDBUtilConntion*)ConnectionList->Items[i];
        if (Con && (Con->Connection == Connection)) {
            ConData = Con;
            break;
        }
    }

    // Create
    if (!ConData) {
        ConData = new TZDBUtilConntion;
        ConData->Connection = Connection;
        ConnectionList->Add(ConData);

        // Create query (15/02/10)
        ConData->Query = new TZQuery(this);
        ConData->Query->Connection = ConData->Connection;        
    }

    // Check reconnection interval
    DWORD Time = GetTickCount();
    DWORD Elapsed = Time - ConData->LastTime;
	if (Elapsed < ReconnectInterval) {
        return true;
    }

    CFbLog("CheckFBConnection: ConnectionName=" + Connection->Name + ", Host=" + Connection->HostName + ", Database=" + Connection->Database, clRed);
    CFbLog("  Testing connection...");

    //
    ConData->LastTime = Time;

    // Create query
    TZQuery* Query = ConData->Query;

    try {
        //
        Query->Close();

        // Query tables list
        Query->SQL->Text = "SELECT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE "
                    "RDB$SYSTEM_FLAG = 0 AND RDB$VIEW_BLR IS NULL ORDER "
                    "BY RDB$RELATION_NAME";

        Query->Open();
        AConnected = !Query->Eof;
    }
    catch (Exception& E) {
		CFbLog("  CheckFBConnection: " + E.Message, clRed);
        try {
            // Reconnect
            CFbLog("  Reconnecting...");
            try {
                Connection->Connected = false;
            }
            catch (Exception& E) {
                CFbLog("  Connected=false exception: " + E.Message);
            }

            // Required because there is a bug in current version of Zeos, Disconnect()
            ClearFConnection(Connection);

            CFbLog("  Setting Connected=true ...");
            Connection->Connected = true;
            CFbLog("  Setting Connected=true OK");

            // Query again
            try {
                Query->Close();
                Query->Open();

                // Get result
                AConnected = !Query->Eof;
            }
			catch (Exception& E) {
                CFbLog("  Exception: " + E.Message);
            }

            CFbLog("  CheckFBConnection: Reconnected successfully", clGreen);
        }
        catch (Exception& E) {
            CFbLog("  CheckFBConnection: Reconnect failed: " + E.Message, clRed);
        }
    }

    // Update time
    ConData->LastTime = GetTickCount();
    
    // Done
    if (AConnected)
        CFbLog("  Check OK, Database=" + Connection->Database);
    else
        CFbLog("  Check FAILED, Database is not connected: " + Connection->Database);

    //
    return AConnected;
}
//---------------------------------------------------------------------------
bool __fastcall TZDBUtil::CheckFBConnection(TZQuery* Query)
{
	return CheckFBConnection(GetConnection(Query));
}
//---------------------------------------------------------------------------
// XE2 (12/03/12)
TZConnection* __fastcall TZDBUtil::GetConnection(TZQuery* Query)
{
	TZConnection* Connection = dynamic_cast<TZConnection*>(Query->Connection);

	return Connection;
}
//===========================================================================
void __fastcall TZDBUtil::Log(String S, TColor AColor)
{
    if (LogFile)
        LogFile->Log(S);
}
//---------------------------------------------------------------------------
void __fastcall TZDBUtil::LogEx(Exception& E, String S, TColor AColor)
{
	S = S + " - " + E.Message;
	Log(S, AColor);
}
//---------------------------------------------------------------------------
void __fastcall TZDBUtil::CFbLog(String S, TColor AColor)
{
    if (CFbLogFile)
        CFbLogFile->Log(S);
}
//---------------------------------------------------------------------------
// (30/11/14)
void __fastcall TZDBUtil::CloneZConnection(TZConnection* ZSource, TZConnection* ZCon)
{
	ZCon->ControlsCodePage	= ZSource->ControlsCodePage;
	ZCon->ClientCodepage 	= ZSource->ClientCodepage;
	ZCon->Properties->Assign(ZSource->Properties);
	ZCon->TransactIsolationLevel	= ZSource->TransactIsolationLevel;
	ZCon->DesignConnection 			= ZSource->DesignConnection;
	ZCon->HostName		= ZSource->HostName;
	ZCon->Port 			= ZSource->Port;
	ZCon->Database		= ZSource->Database;
	ZCon->User			= ZSource->User;
	ZCon->Password 		= ZSource->Password;
	ZCon->Protocol 		= ZSource->Protocol;
}
//===========================================================================
//									2019
//===========================================================================
// Add to list if doesn't exist or not null
void __fastcall TZDBUtil::ComboListAdd(TStrings* List, String S)
{
	if (S != "" && List->IndexOf(S) < 0)
		List->Add(S);
}
//---------------------------------------------------------------------------
void __fastcall TZDBUtil::LoadComboItems(TStrings* List, String TableName, String FieldName)
{
    List->BeginUpdate();
    List->Clear();
	try {
		TZDbQuery Query("$SESAM:SELECT DISTINCT " + FieldName + " FROM " + TableName + " ORDER BY " + FieldName, true);
		while (!Query.DataSet->Eof) {
			String Value = GetField(Query.DataSet, FieldName);
			ComboListAdd(List, Value);
			Query.DataSet->Next();
		}
	}
	catch (Exception& E) {
	}
	List->EndUpdate();
}
//============================================================================

//============================================================================
#ifdef never
struct PACKAGE TAutoBlobField;

struct TAutoBlobField {

	TAutoBlobField();
	//-----------------------------------------------------------------------
	~TAutoBlobField();
	//-----------------------------------------------------------------------
	bool LoadFromFile(String FileName);
	//-----------------------------------------------------------------------
	bool SaveToFile(String FileName);
	//-----------------------------------------------------------------------
	TMemoryStream* FileStream;
};

//---------------------------------------------------------------------------
TAutoBlobField::TAutoBlobField()
{
	FileStream = new TMemoryStream;
}
//---------------------------------------------------------------------------
TAutoBlobField::~TAutoBlobField()
{
	delete FileStream;

}
//---------------------------------------------------------------------------
bool TAutoBlobField::LoadFromFile(String AFileName)
{
	FileName = AFileName;
	if (FileExists(FileName)) {
		try {
			FileStream->LoadFromFile(FileName);
			FileStream->Position = 0;
			return true;
		}
		catch (Exception& E) {
		}
	}
	return false;
}
//---------------------------------------------------------------------------
bool TAutoBlobField::SaveToFile(String AFileName)
{
	FileName = AFileName;
	try {
		FileStream->Position = 0;
		FileStream->SaveToFile(FileName);
		return true;
	}
	catch (Exception& E) {
	}
	return false;
}
//---------------------------------------------------------------------------
bool TAutoBlobField::LoadFromField(TDataSet* DataSet, String FieldName)
{
	bool Result = false;
	TStream* Stream = DataSet->CreateBlobStream(Field, bmRead);
	try {
		TField* Field = DataSet->FindField(FieldName);
		if (Field) {
			Stream = (TMemoryStream*)DataSet->CreateBlobStream(Field, bmRead);
			Stream->SaveToFile(FileName);
			Result = true;
		}
	}
	catch (Exception& E) {
	}
	delete Stream;
	return Result;
}
//---------------------------------------------------------------------------
bool TAutoBlobField::SaveToField(TDataSet* DataSet, String FieldName)
{
	TZQuery* ZQuery = dynamic_cast<TZQuery*>(DataSet);
	if (!ZQuery)
		return false;

	try {
		ZQuery.GetParams()->ParamByName(FieldName)->LoadFromStream(FileStream, ftBlob);
		return true;	}
	catch (Exception& E) {
	}
	return false;
}



			TField*  	Field  	  = DataSet->FindField("IMAGE");
			TStream* 	Stream 	  = DataSet->CreateBlobStream(Field, bmRead);

			if (Stream) {
				TFileStream* FS = new TFileStream(Data->FileName, fmCreate);
				bool WriteOK = false;
				try {
					FS->CopyFrom(Stream, Stream->Size);
					WriteOK = true;
				}
				catch (Exception& E) {
					LogEx(E, "GetCameraSnapshot: Fetch");
				}
				delete FS;
				if (WriteOK) {
					FileUtil->SetFileTimeDT(Data->FileName, Data->FileDT);
					Load = true;
				}
			}
			delete Stream;

//---------------------------------------------------------------------------
#endif



//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SeZDbQuery.h"
#include <Db.hpp>
#include <DBTables.hpp>
#include "ZConnection.hpp"
#include "ZAbstractDataset.hpp"
#include "ZAbstractRODataset.hpp"
#include "ZDataset.hpp"
#include "SeLogFile.h"
#include "SeLogPanel.h"
#include "SeSesamAps.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ZConnection"
#pragma link "ZAbstractDataset"
#pragma link "ZAbstractRODataset"
#pragma link "ZDataset"
//---------------------------------------------------------------------------
TZConnection* ZDBQueryDefaultConnection = NULL;
TLogFile*     ZDBQueryLogFile           = NULL;
TLogPanel*    ZDBQueryLogPanel          = NULL;
bool          ZDBQueryDebugMode         = true;

TSDatabaseType ZDBQueryDefaultDatabaseType = sdtFirebird;

TStringList*        ZDbDatabaseList = NULL;
//===========================================================================
//                               TZDbDatabase
//===========================================================================
__fastcall TZDbDatabase::TZDbDatabase(TComponent* AOwner, String AName) :
    TComponent(AOwner)
{
    Name            = AName;
    Type            = ZDBQueryDefaultDatabaseType;
    ZConnection     = NULL;
}
//---------------------------------------------------------------------------
__fastcall TZDbDatabase::TZDbDatabase(TComponent* AOwner, String AName, TZConnection* AZConnection, TSDatabaseType SType) :
    TComponent(AOwner)
{
    Name        = AName;
    Type        = (SType == sdtDefault) ? ZDBQueryDefaultDatabaseType : SType;
    ZConnection = AZConnection;
}
//---------------------------------------------------------------------------
__fastcall TZDbDatabase::TZDbDatabase(TComponent* AOwner, String AName, String ABdeAlias) :
    TComponent(AOwner)
{
    Name        = AName;
    Type        = sdtParadox;
    ZConnection = NULL;

    if (ABdeAlias == "")
        BdeAlias = AName;
    else
        BdeAlias = ABdeAlias;
}
//---------------------------------------------------------------------------
__fastcall TZDbDatabase::~TZDbDatabase()
{
    Unregister();
}
//---------------------------------------------------------------------------
bool TZDbDatabase::Register()
{
    if (Name == "")
        return false;

    if (!ZDbDatabaseList)
        ZDbDatabaseList = new TStringList;

    int Index = ZDbDatabaseList->IndexOf(Name);

    if (Index < 0) {
        ZDbDatabaseList->AddObject(Name, (TObject*)this);
        return true;
    }

    return false;
}
//---------------------------------------------------------------------------
bool TZDbDatabase::Unregister()
{
    if (Name != "" && ZDbDatabaseList) {
        int Index = ZDbDatabaseList->IndexOf(Name);

        if (Index > -1) {
            ZDbDatabaseList->Delete(Index);
            return true;
        }
    }

    return false;
}
//---------------------------------------------------------------------------
bool PACKAGE ZDbRegisterDatabase(String AName, String ABdeAlias)
{
    if (ZDbDatabaseList && ZDbDatabaseList->IndexOf(AName) > -1)
        return true;

    TZDbDatabase* Db = new TZDbDatabase(Application, AName, ABdeAlias);
    Db->Register();

    return true;
}
//---------------------------------------------------------------------------
bool PACKAGE ZDbRegisterDatabase(String AName, TZConnection* ZCon, TSDatabaseType SType)
{
    if (ZDbDatabaseList && ZDbDatabaseList->IndexOf(AName) > -1)
        return true;

    TZDbDatabase* Db = new TZDbDatabase(Application, AName, ZCon, SType);
    Db->Register();

    return true;
}
//===========================================================================
//                               TZDbOptions
//===========================================================================
TZDbOptions::TZDbOptions()
{
}
//---------------------------------------------------------------------------
TZDbOptions::~TZDbOptions()
{
}
//===========================================================================
//                               TDbQuery
//===========================================================================

TZDbQuery::TZDbQuery(Zabstractconnection::TZAbstractConnection* _AConnection, String SqlText, bool AOpen)
{
	// XE2 (13/03/12)
	TZConnection* AConnection = dynamic_cast<TZConnection*>(_AConnection);

	Init();

	// Assign connection
	ZConnection  = AConnection;

	// Set database type
    if (ZConnection->Protocol.Pos("firebird") > 0)
        DatabaseType = sdtFirebird;
	else if (ZConnection->Protocol == "ado")
        DatabaseType = sdtMSSQL;
    else {
        DatabaseType = sdtFirebird;
    }

    if (SqlText != "")
        OpenQuery(SqlText, AOpen);

    #ifdef never
    CreateQuery();

    if (ZQuery && SqlText != "") {
        ZQuery->SQL->Text = SqlText;

        if (SqlText.SubString(1, 7).AnsiCompareIC("SELECT ") == 0)
            AOpen = true;

        if (AOpen)
            Open();
        else
            ExecSQL();
    }
    #endif

}
//---------------------------------------------------------------------------
TZDbQuery::TZDbQuery(String SqlText, bool AOpen)
{
    Init();

    if (SqlText != "")
        OpenQuery(SqlText, AOpen);
}
//---------------------------------------------------------------------------
bool TZDbQuery::OpenQuery(String SqlText, bool AOpen)
{
    //
    if (SqlText.SubString(1, 1) == "$") {

        DatabaseName = GetDatabaseName(SqlText);
        SetupDatabase();

        int P = SqlText.Pos(":");
        if (P > 0) {
            SqlText.Delete(1, P);
            if (SqlText.SubString(1, 1) == "@") {
                SqlText.Delete(1, 1);
                SqlText.Insert("SELECT * FROM ", 1);
                AOpen = true;
            }
        }
    }

    //
    if (CreateQuery()) {
        SetSQL(SqlText);

        if (AOpen)
            Open(AOpen);
    }

    return true;
}
//---------------------------------------------------------------------------
TZDbQuery::~TZDbQuery()
{
    // Release memory
    if (ZQuery) delete ZQuery;
    if (PQuery) delete PQuery;
}
//---------------------------------------------------------------------------
void TZDbQuery::Init()
{
    Owner       = NULL;
    Database    = NULL;
    ZConnection = ZDBQueryDefaultConnection;
    LogFile     = ZDBQueryLogFile;
    LogPanel    = ZDBQueryLogPanel;
    DebugMode   = ZDBQueryDebugMode;
    DataSet     = NULL;
    DataSource  = NULL;
    ZQuery      = NULL;
    PQuery      = NULL;
}
//---------------------------------------------------------------------------
bool TZDbQuery::SetupDatabase()
{
    if (DatabaseName == "")
        return true;

    if (!ZDbDatabaseList)
        ZDbDatabaseList = new TStringList;

    int Index = ZDbDatabaseList->IndexOf(DatabaseName);

    if (Index > -1) {
        TZDbDatabase* Db = (TZDbDatabase*)ZDbDatabaseList->Objects[Index];

        Database     = Db;
        DatabaseType = Database->Type;
        return true;
    }

    return false;
}
//---------------------------------------------------------------------------
// Connect to database
bool TZDbQuery::Connect()
{
    // Paradox
    if (DatabaseType == sdtParadox) {
        if (!PQuery)
            PQuery = new TQuery(Owner);
    }

    // Firebird
    else if (DatabaseType == sdtFirebird) {
        if (!ZQuery)
            ZQuery = new TZQuery(Owner);

        ZQuery->Connection = ZConnection;
    }

    // MS SQL
    else if (DatabaseType == sdtMSSQL) {
        if (!ZQuery)
            ZQuery = new TZQuery(Owner);

        ZQuery->Connection = ZConnection;
    }

    // Other ZeosDbo supported
    else {
    }

    return true;
}
//---------------------------------------------------------------------------
// Create new query
bool TZDbQuery::CreateQuery()
{
    // Paradox
    if (DatabaseType == sdtParadox) {
        PQuery                  = new TQuery(Owner);
        PQuery->DatabaseName    = Database->BdeAlias;
		//PQuery->RequestLive     = true;
        PQuery->CachedUpdates   = false;

        DataSet = PQuery;
    }
    else {
        ZQuery                  = new TZQuery(Owner);
        ZQuery->Connection      = Database ? Database->ZConnection : ZConnection;
        //ZQuery->RequestLive     = true;
        ZQuery->CachedUpdates   = false;

        DataSet = ZQuery;
    }

    //
    DataSet = DoGetDataSet();

    return (DataSet != NULL);
}
//---------------------------------------------------------------------------
// Execute query, no result set is returned
void TZDbQuery::ExecSQL()
{
    if (DebugMode)
        Perf.Start();

    if (ZQuery) ZQuery->ExecSQL();
    else if (PQuery) PQuery->ExecSQL();

    if (DebugMode) {
        double Time = Perf.StopSec();
        AnsiString Msg;
        Msg.sprintf("TDbQuery::ExecSQL, %0.4lf sec, SQL: ", Time);
        Msg += GetSQL();
        TColor AColor = (Msg.Pos("SELECT") > 0) ? clGray : clMaroon;
        Log(Msg, AColor);
    }
}
//---------------------------------------------------------------------------
// Execute query and return result set
void TZDbQuery::Open(bool AOpen)
{
    if (DebugMode)
        Perf.Start();

    if (IsZQueryDBConnected(DataSet)) {
        //if (GetSQL().Pos("SELECT
        DataSet->Open();
    }

    if (DebugMode) {
        double Time = Perf.StopSec();
        AnsiString Msg;

        // DO NOT USE DataSet->RecordCount because it may take a long time to process (removed 07/05/06)
        Msg.sprintf("TDbQuery::Open, %0.4lf sec, SQL: ", Time);  //Result: %d records, SQL: ", Time, DataSet->RecordCount);
        Msg += GetSQL();
        TColor AColor = (Msg.Pos("SELECT") > 0) ? clGray : clMaroon;
        Log(Msg, AColor);
    }

    if (DataSource) {
        DataSource->DataSet = DataSet;
    }
}
//---------------------------------------------------------------------------
// Execute query and return result set
void TZDbQuery::Close()
{
    if (DataSource) {
        DataSource->DataSet = NULL;
    }

    DataSet->Close();
}
//---------------------------------------------------------------------------
String TZDbQuery::GetSQL()
{
    String AText;

    if (ZQuery) AText = ZQuery->SQL->Text;
    else if (PQuery) AText = PQuery->SQL->Text;

    return AText;
}
//---------------------------------------------------------------------------
bool TZDbQuery::SetSQL(String AText)
{
    if (ZQuery) ZQuery->SQL->Text = AText;
    else if (PQuery) PQuery->SQL->Text = AText;

    return true;
}
//---------------------------------------------------------------------------
TParams* TZDbQuery::GetParams()
{
    TParams* AParams;

    if (ZQuery) AParams = ZQuery->Params;
    else if (PQuery) AParams = PQuery->Params;
    else AParams = NULL;

    return AParams;
}
//---------------------------------------------------------------------------
// Get current DataSet
TDataSet* TZDbQuery::GetDataSet()
{
	if (!DataSet)
		DataSet = DoGetDataSet();

	return DataSet;
}
//---------------------------------------------------------------------------
// Get current dataset
TDataSet* TZDbQuery::DoGetDataSet()
{
    TDataSet* ADataSet = NULL;

    if (ZQuery)
        ADataSet = ZQuery;
    else if (PQuery)
        ADataSet = PQuery;

    return ADataSet;
}
//---------------------------------------------------------------------------
String TZDbQuery::GetField(String FieldName, String Default)
{
    String S;

    if (DataSet) {
        TField* Field = DataSet->FindField(FieldName);

        if (Field && !Field->IsNull)
            S = Field->AsString;
        else
            S = Default;
    }

    return S;
}
//---------------------------------------------------------------------------
int TZDbQuery::GetFieldInt(String FieldName, int Default)
{
    return GetField(FieldName, Default).ToIntDef(Default);
}
//---------------------------------------------------------------------------
// Get LargeInt field from DataSet (required because in BCB5 Variant does not support LargeInt)
__int64 TZDbQuery::GetFieldInt64(String FieldName, __int64 Default)
{
    __int64 Value = Default;

    if (DataSet) {
        TLargeintField* F = (TLargeintField*)DataSet->FindField(FieldName);
        if (F && F->DataType == ftLargeint && !F->IsNull)
            Value = F->AsLargeInt;

        // Try Integer (17/09/13)
        else if (F && F->DataType == ftInteger && !F->IsNull)
            Value = F->AsInteger;
    }

    return Value;
}
//---------------------------------------------------------------------------
// Get field from dataset
TDateTime TZDbQuery::GetFieldDT(String FieldName, TDateTime Default)
{
    TDateTime Value = Default;

    if (DataSet) {
        TField* Field = DataSet->FindField(FieldName);

        if (Field && !Field->IsNull)
            Value = Field->AsDateTime;
    }

    return Value;
}
//---------------------------------------------------------------------------
// Set TParam as __int64.
// This patch is required because in C++ Builder 5 (Delphi 5),
// TParam (and Variant) does not support ftLargeInt.
// This is the only way to work with ZeosDbo and Firebird with __int64 parameters.
// Chen, 31/03/08.
void TZDbQuery::SetParamInt64(TParam* Param, __int64 Value)
{
    #if (__BORLANDC__ == 0x551)
    Param->AsString = String(Value);

    // Get pointer to TParam::FDataType which is private member of TParam
    TFieldType* pDataType = (TFieldType*) (((BYTE*)Param) + 0x30);

	if (*pDataType == ftString)
        *pDataType = ftLargeint;
    #else
    Param->AsLargeInt = Value;
    #endif
}
//---------------------------------------------------------------------------
// Get new value from Firebird Generator
__int64 TZDbQuery::FBAutoInc(String GeneratorName)
{
    if (DatabaseType == sdtFirebird) {
        ZQuery->Close();
        ZQuery->SQL->Text = "SELECT GEN_ID (" + GeneratorName + ",1) FROM RDB$DATABASE";

        if (IsZQueryDBConnected(ZQuery)) {
            ZQuery->Open();
            return GetFieldInt64("GEN_ID");
        }
    }

    // Other
    return 0;
}
//---------------------------------------------------------------------------
void TZDbQuery::SelectField(String FieldName, TStrings* List, bool Clear)
{
    List->BeginUpdate();

    if (Clear)
        List->Clear();

    try {
        while (!DataSet->Eof) {
            List->Add( GetField(FieldName) );
            DataSet->Next();
        }
    }
    catch (Exception& E) {
    }

    List->EndUpdate();
}
//---------------------------------------------------------------------------
void TZDbQuery::SelectFieldValues(String FieldName, String ValueFieldName, TStrings* List, bool Clear)
{
    List->BeginUpdate();

    if (Clear)
        List->Clear();

    try {
        while (!DataSet->Eof) {
            String AName  = GetField(FieldName);
            String AValue = GetField(ValueFieldName);
            List->Add(AName + "=" + AValue);
            DataSet->Next();
        }
    }
    catch (Exception& E) {
    }

    List->EndUpdate();
}
//---------------------------------------------------------------------------
// Get database name from DATABASE:TABLE string
String TZDbQuery::GetDatabaseName(String Str)
{
    if (Str.SubString(1, 1) == "$")
        Str.Delete(1, 1);

    int P = Str.Pos(":");
    if (P > 0)
        return Str.SubString(1, P-1);
    else
        return Str;
}
//---------------------------------------------------------------------------
// Get table name from DATABASE:TABLE string
String TZDbQuery::GetTableName(String Str)
{
    int P = Str.Pos(":");
    if (P > 0)
        return Str.SubString(P+1, Str.Length()-P);
    else
        return Str;
}
//---------------------------------------------------------------------------
bool TZDbQuery::IsParadox()        { return (DatabaseType == sdtParadox);   }
bool TZDbQuery::IsFirebird()       { return (DatabaseType == sdtFirebird);  }
bool TZDbQuery::IsMSSQL()          { return (DatabaseType == sdtMSSQL);     }
//---------------------------------------------------------------------------
bool TZDbQuery::FlushBuffers()
{
    if (ZQuery) ;
    else if (PQuery) PQuery->FlushBuffers();

    return true;
}
//---------------------------------------------------------------------------
void TZDbQuery::Log(String Msg, TColor AColor, TColor BColor)
{
    if (LogFile) LogFile->Log(Msg);
    if (LogPanel) LogPanel->Add(Msg, AColor, BColor);
}
//---------------------------------------------------------------------------
void TZDbQuery::LogEx(Exception& E, String Msg, TColor AColor, TColor BColor)
{
    Msg = Msg + " - " + E.Message;
    Log(Msg, AColor, BColor);
}
//---------------------------------------------------------------------------
// This will work only when all UPDATE fields are accessed as params
bool TZDbQuery::CheckSQLUpdateModified(TZDbQuery& QSelect)
{
    bool Modified = false;

    // Scan all UPDATE params
    for (int i=0;  i < ZQuery->Params->Count;  i++) {
        String FieldName = ZQuery->Params->Items[i]->Name;

        TField* Field = QSelect.ZQuery->FindField(FieldName);

        if (Field) {
            if (ZQuery->Params->Items[i]->Value.IsNull() != Field->Value.IsNull())
				Modified = true;
			else if ((Field->DataType == ftString) || (Field->DataType == ftWideString)) 	// Added ftWideString (14/07/13)
                Modified = (ZQuery->Params->Items[i]->AsString != Field->AsString);
            else if (ZQuery->Params->Items[i]->Value != Field->Value)
                Modified = true;
        }

        if (Modified) {
            Log("CheckSQLUpdateModified: Modified, Field=" + FieldName);
            break;
        }
    }

    return Modified;
}
//---------------------------------------------------------------------------
bool SaveBlobToFile(TDataSet* DataSet, String FieldName, String FileName)
{
    bool Result = false;

    TMemoryStream* Stream = NULL;
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

    if (Stream) delete Stream;
    return Result;
}
//---------------------------------------------------------------------------
bool LoadBlobFromFile(TDataSet* DataSet, String FieldName, String FileName)
{
	bool Result = false;
	TMemoryStream* Stream = new TMemoryStream;
	try {
		TZQuery* ZQuery = dynamic_cast<TZQuery*>(DataSet);
		if (ZQuery) {
			Stream->LoadFromFile(FileName);
			Stream->Position = 0;
			ZQuery->Params->ParamByName(FieldName)->LoadFromStream(Stream, ftBlob);
			Result = true;
		}
	}
	catch (Exception& E) {
	}
	delete Stream;
	return Result;
}
//---------------------------------------------------------------------------




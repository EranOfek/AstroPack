//---------------------------------------------------------------------------
#ifndef SeZDbUtlH
#define SeZDbUtlH
//---------------------------------------------------------------------------
#include "ZConnection.hpp"
#include "ZAbstractDataset.hpp"
#include "ZAbstractRODataset.hpp"
#include "ZDataset.hpp"

#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <IniFiles.hpp>
#include <Db.hpp>
#include <DBTables.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <stdio.h>
#include <ElCombos.hpp>
#include <XDBGrids.hpp>

#include "SQLMemMain.hpp"
#include "SeLogFile.h"
#include "ZAbstractConnection.hpp"

#include <IBDatabase.hpp>

#ifdef _ZFIBPLUS_
	#include <.hpp>
#endif


//---------------------------------------------------------------------------
// (13/03/08)
namespace Sqlmemmain {
class DELPHICLASS TSQLMemTable;
class DELPHICLASS TSQLMemQuery;
};

//===========================================================================
//                                 TCopySQL
//===========================================================================

class PACKAGE TCopySQL {
public:
    //-----------------------------------------------------------------------
    __fastcall TCopySQL(TZQuery* ZQuery);
    __fastcall TCopySQL(TSQLMemQuery* MemQuery);
    //-----------------------------------------------------------------------
    __fastcall ~TCopySQL();
    //-----------------------------------------------------------------------
    bool __fastcall Set(TZQuery* ZQuery);
    bool __fastcall Set(TSQLMemQuery* MemQuery);
    //-----------------------------------------------------------------------
    String      SQLText;
    TParams*    Params;
};

//===========================================================================

struct TZDBUtilConntion {
    TZConnection*   Connection;
    TZQuery*        Query;
    DWORD           LastTime;

    TZDBUtilConntion()
        { Connection = NULL;  Query = NULL;  LastTime = 0; }
};

//===========================================================================
//                                 TZDBUtil
//===========================================================================
class PACKAGE TZDBUtil : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TLabel *StrAll;
    TZConnection *ZConnection1;
    TZQuery *ZQuery1;
    TQuery *Query1;
    TDBGrid *DBGrid1;
private:	// User declarations
public:		// User declarations
    __fastcall TZDBUtil(TComponent* Owner);
    //-----------------------------------------------------------------------
    __fastcall ~TZDBUtil();
    //=======================================================================
    // Get field value, return default if field does not exist or null
    String __fastcall GetField(TDataSet* DataSet, String FieldName, String Default = "");
    //-----------------------------------------------------------------------
    // Get LargeInt field from DataSet (required because in BCB5 Variant does not support LargeInt)
    __int64 __fastcall GetFieldInt64(TDataSet* DataSet, String FieldName, __int64 Default = 0);
    //=======================================================================
    //                             Firebird
    //=======================================================================
    bool __fastcall IsFirebirdRunning();
    //-----------------------------------------------------------------------
    bool __fastcall IsWindowsVista();
    //-----------------------------------------------------------------------
    bool __fastcall FBConnectDatabase(TZConnection* ZConnection, TCustomIniFile* IniFile = NULL, String Section = "", String DBFileName = "", bool ACopy = false);
    //-----------------------------------------------------------------------
    bool __fastcall FBVerifyDatabaseFile(String DBFileName, String DBTemplateFileName = "");
    //-----------------------------------------------------------------------
    // Get new value from Firebird Generator
	__int64 __fastcall FBAutoInc(TZQuery* ZQuery, String GeneratorName, int Increment = 1);
	//-----------------------------------------------------------------------
	__int64 __fastcall FBSetMinMaxAutoInc(TZQuery* ZQuery, String GeneratorName, __int64 MinVal, __int64 MaxVal);
	//-----------------------------------------------------------------------
	__int64 __fastcall FBGetMaxValue(TZQuery* ZQuery, String TableName, String FieldName, __int64 MinValue, __int64 MaxValue);
	//-----------------------------------------------------------------------
	void __fastcall SetParamByName(TZQuery* Table, TZQuery* Query, String FieldName, String Value);
	//-----------------------------------------------------------------------
	bool __fastcall FBGetTablesList(TZQuery* ZQuery, TStrings* List);
    //-----------------------------------------------------------------------
    bool __fastcall FBTableExists(TZQuery* ZQuery, String TableName);
    //-----------------------------------------------------------------------
    String __fastcall FBGetRootDir();
    //-----------------------------------------------------------------------
    bool __fastcall FBUpdateAlias(String Alias, String& DatabaseName, bool ASet);
    //-----------------------------------------------------------------------
    // Backup FDB file, optionally compress it using 7za.exe
    bool __fastcall BackupFDB(String DatabaseName, String BackupFile, bool Compress, bool DeleteFBK, DWORD Timeout = INFINITE, bool StructOnly = false);
    //-----------------------------------------------------------------------
    // Restore FDB file, optionally compress it using 7za.exe
    //bool __fastcall RestoreFDB(String DatabaseName, String BackupFile, bool Compress, bool DeleteFBK, DWORD Timeout = INFINITE);
    //-----------------------------------------------------------------------
    bool __fastcall AddPassword(String Password);
    //-----------------------------------------------------------------------
    bool __fastcall SetFirebirdPassword(String User, String Password, String NewPassword);
    //-----------------------------------------------------------------------
    AnsiString __fastcall EncryptPassword(AnsiString Str);
    //-----------------------------------------------------------------------
    AnsiString __fastcall DecryptPassword(AnsiString Str);
    //-----------------------------------------------------------------------
	String __fastcall ConnectPasswordList(TZConnection* ZConnection, TStrings* APasswordList = NULL, bool AOpen = false);
	String __fastcall ConnectPasswordListIBX(TIBDatabase* Connection, TStrings* APasswordList = NULL, bool AOpen = false);
    //=======================================================================
    //                        SQL and Sorting
    //=======================================================================
    String __fastcall SetWhere(String OriginalSQL, String NewWhere);
    //-----------------------------------------------------------------------
    String __fastcall SetWhere(String AText, TStrings* CondList, bool AndOr);
    //-----------------------------------------------------------------------
    String __fastcall SetOrderBy(String OriginalSQL, String FieldName, String Direction = "");
    //=======================================================================
    bool __fastcall SetNewSQL(TZQuery* Query, String NewSQL);
    //-----------------------------------------------------------------------
    bool __fastcall SetWhere(TZQuery* Query, String NewWhere);
    //-----------------------------------------------------------------------
    bool __fastcall SetOrderBy(TZQuery* Query, String FieldName, String Direction = "");
    //=======================================================================
    void __fastcall GridSort(TColumn* Sender, String Direction = "TOGGLE");
    //-----------------------------------------------------------------------
	void __fastcall GridSort(TXColumn* Sender, String Direction = "TOGGLE");
	void __fastcall GridSortZQuery(TZQuery* Query, TXColumn* Sender, String Direction = "TOGGLE");
	void __fastcall GridSortMemQuery(TSQLMemQuery* Query, TXColumn* Sender, String Direction = "TOGGLE");
    //=======================================================================
    void __fastcall SelectDistinct(String AText, String FieldName, TStrings* List, bool Clear);
    //-----------------------------------------------------------------------
    void __fastcall SelectDistinctCombo(TDataSet* DataSet, String FieldName, TComboBox* Combo);
    //-----------------------------------------------------------------------
    void __fastcall SelectDistinctCombo(TDataSet* DataSet, String FieldName, TElComboBox* Combo);
    //#endif
    //-----------------------------------------------------------------------
    String __fastcall GetComboValue(TElComboBox* Combo);
    //-----------------------------------------------------------------------
    String __fastcall GetHostName();
    //-----------------------------------------------------------------------
    String __fastcall GetIpAddr();
    //-----------------------------------------------------------------------
	bool __fastcall IsLocalHost(String HostName);
	//-----------------------------------------------------------------------
	String __fastcall CreateSeqUuid();
	String __fastcall CreateNodeSeqUuid();
	//=======================================================================
	//                       	SQLMemTable
    //=======================================================================
    bool __fastcall ImportMemTableFromQuery(TZQuery* ZQuery, String ZTableName,
        Sqlmemmain::TSQLMemTable* MemTable, Sqlmemmain::TSQLMemQuery* MemQuery,
        bool CopyData = true);
    //-----------------------------------------------------------------------
    #ifdef _LMDEL_
    // (20/01/14)
    void __fastcall MemTableAddIndex(TSQLMemTable* MemTable, TXDBGrid* DBGrid);
    //-----------------------------------------------------------------------
    // (20/01/14)
    void __fastcall MemTableImport(TSQLMemTable* MemTable, TDataSet* DataSet, TXDBGrid* DBGrid);
    //-----------------------------------------------------------------------
    // (20/01/14)
    void __fastcall MemTableDBGridTitleClick(TSQLMemTable* MemTable, TXColumn *Column, String& GridSortFieldName, bool& GridSortAsc);
    #endif
    //=======================================================================
    bool __fastcall CheckFBConnection(TZConnection* Connection);
    bool __fastcall DoCheckFBConnection(TZConnection* Connection);    
	bool __fastcall CheckFBConnection(TZQuery* Query);
	TZConnection* __fastcall GetConnection(TZQuery* Query);
    //-----------------------------------------------------------------------
	void __fastcall Log(String S, TColor AColor = clBlack);
    void __fastcall LogEx(Exception& E, String S, TColor AColor = clRed);
    void __fastcall CFbLog(String S, TColor AColor = clBlack);
	//-----------------------------------------------------------------------
	// (30/11/14)
	void __fastcall CloneZConnection(TZConnection* ZSource, TZConnection* ZCon);
	//-----------------------------------------------------------------------
	// (2019)
	void __fastcall ComboListAdd(TStrings* List, String S);
	//-----------------------------------------------------------------------
	void __fastcall LoadComboItems(TStrings* List, String TableName, String FieldName);
	//-----------------------------------------------------------------------

    String          ErrorMsg;
    String          DBTemplatePath;
    String          Username;
    String          Password;
    TStringList*    PasswordList;
    TLogFile*       LogFile;
    TLogFile*       CFbLogFile;
    DWORD           ReconnectInterval;
    TList*          ConnectionList;
    DWORD           MainThreadId;
    bool            FShowMessages;          // (26/01/14)
};
//---------------------------------------------------------------------------
extern PACKAGE TZDBUtil *ZDBUtil;

//---------------------------------------------------------------------------
bool PACKAGE __fastcall IsSQLException(Exception& E);
int  PACKAGE __fastcall GetSQLErrorCode(Exception& E);

void PACKAGE __fastcall ClearFConnection(TZConnection* ZCon);
//---------------------------------------------------------------------------
#endif




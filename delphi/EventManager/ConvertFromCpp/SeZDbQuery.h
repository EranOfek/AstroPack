//---------------------------------------------------------------------------
#ifndef SeZDbQueryH
#define SeZDbQueryH

#include "SePerfCount.h"

#define ZDBQ_BDE
//===========================================================================
//                                 TDbQuery
//===========================================================================

// ZeosDbo components
// We do not include Zeos header files here because there are some problems with it
//class TZConnection;
//class DELPHICLASS TZConnection;
//class TZQuery;

#include "ZConnection.hpp"
#include "ZAbstractDataset.hpp"
#include "ZAbstractRODataset.hpp"
#include "ZDataset.hpp"
#include "SeLogFile.h"
#include "SeLogPanel.h"
#include <dbtables.hpp>


//#define _ZFIBPLUS_

#ifdef _ZFIBPLUS_
    #include <.hpp>
#endif

//---------------------------------------------------------------------------
#pragma warn -8026
#pragma warn -8027

//class TLogFile;
//class TLogPanel;

// Default connection
extern PACKAGE TZConnection*    ZDBQueryDefaultConnection;

// Debug mode
extern PACKAGE bool             ZDBQueryDebugMode;

// Log
extern PACKAGE TLogFile*        ZDBQueryLogFile;
extern PACKAGE TLogPanel*       ZDBQueryLogPanel;


// Database type
enum TSDatabaseType {
    sdtNone     = -1,       //
    sdtDefault  = 0,        //
    sdtParadox  = 1,        // Paradox  - BDE
    sdtFirebird = 2,        // Firebird - Zeos
    sdtMSSQL    = 3,        // MS-SQL   - Zeos
    sdtFIBPlus  = 4 };      // Firebird - FIBPlus


// Default database type
extern PACKAGE TSDatabaseType ZDBQueryDefaultDatabaseType;

//extern PACKAGE TStringList* ZQuery

extern PACKAGE TStringList* ZDbDatabaseList;

//===========================================================================
//                               TZDbDatabase
//===========================================================================
// TZDbDatabase is interface class to different types of database: Zeos, Paradox
class PACKAGE TZDbDatabase : public TComponent {
public:
    // Default constructor
    __fastcall TZDbDatabase(TComponent* AOwner, String AName = "");
    //-----------------------------------------------------------------------
    // Zeos
    __fastcall TZDbDatabase(TComponent* AOwner, String AName, TZConnection* AZConnection, TSDatabaseType SType = sdtDefault);
    //-----------------------------------------------------------------------
    // FIBPlus
    #ifdef _ZFIBPLUS_
    //__fastcall TZDbDatabase(TComponent* AOwner, String AName, TZConnection* AZConnection, TSDatabaseType SType = sdtDefault);
    #endif
    //-----------------------------------------------------------------------
    // BDE
    __fastcall TZDbDatabase(TComponent* AOwner, String AName, String ABdeAlias = "");
    //-----------------------------------------------------------------------
    // Destructor
    __fastcall ~TZDbDatabase();
    //-----------------------------------------------------------------------
    // Register database in list
    bool Register();
    //-----------------------------------------------------------------------
    // Remove from list
    bool Unregister();
    //-----------------------------------------------------------------------
    String              Name;               // Database name
    TSDatabaseType      Type;               // Database type
    TZConnection*       ZConnection;        // ZeosDBO ZConnection
    String              BdeAlias;           // BDE Alias

    #ifdef _ZFIBPLUS_
    TpFIBDatabase*      FibDatabase;
    #endif
};

//===========================================================================
//                               TZDbOptions
//===========================================================================

class PACKAGE TZDbOptions {
public:
    //-----------------------------------------------------------------------
    TZDbOptions();
    //-----------------------------------------------------------------------
    ~TZDbOptions();
    //-----------------------------------------------------------------------
    String          Database;
    TLogFile*       LogFile;
    TLogPanel*      LogPanel;
    bool            DebugMode;
};
//---------------------------------------------------------------------------
bool PACKAGE ZDbRegisterDatabase(String AName, String ABdeAlias = "");
bool PACKAGE ZDbRegisterDatabase(String AName, TZConnection* ZCon, TSDatabaseType SType = sdtDefault);
//===========================================================================
//                               TZDbQuery
//===========================================================================
class PACKAGE TZDbQuery {
public:
    //-----------------------------------------------------------------------
    // Default constructor
    // TZDbQuery();
	//-----------------------------------------------------------------------
    // Connect
    TZDbQuery(String SqlText = "", bool AOpen = false);
    //-----------------------------------------------------------------------
    //
	TZDbQuery(Zabstractconnection::TZAbstractConnection* AConnection, String SqlText = "", bool AOpen = false);
	//-----------------------------------------------------------------------
    #ifdef _ZFIBPLUS_
    #endif
    //-----------------------------------------------------------------------
    // Destructor
    ~TZDbQuery();
    //-----------------------------------------------------------------------
    bool OpenQuery(String SqlText = "", bool AOpen = false);
    //-----------------------------------------------------------------------
    void Init();
    //-----------------------------------------------------------------------
    bool SetupDatabase();
    //-----------------------------------------------------------------------
    // Connect to database
    bool Connect();
    //-----------------------------------------------------------------------
    bool CreateQuery();
    //-----------------------------------------------------------------------
    // ExecSQL
    void ExecSQL();
    //-----------------------------------------------------------------------
    // Open dataset
    void Open(bool AOpen = true);
    //-----------------------------------------------------------------------
    // Close dataset
    void Close();
    //-----------------------------------------------------------------------
    // Get current SQL text
    String GetSQL();
    //-----------------------------------------------------------------------
    // Set SQL text
    bool SetSQL(String AText);
    //-----------------------------------------------------------------------
    // Get pointer to Params
    // Example: QUpdate.GetParams()->ParamByName("DT")->AsDateTime = Now();
    TParams* GetParams();
    //-----------------------------------------------------------------------
    // Get field from dataset
    String GetField(String FieldName, String Default = "");
    //-----------------------------------------------------------------------
    // Get field from dataset
    int GetFieldInt(String FieldName, int Default = 0);
    //-----------------------------------------------------------------------
    // Get field as __int64
    __int64 GetFieldInt64(String FieldName, __int64 Default = 0);
    //-----------------------------------------------------------------------
    // Get field from dataset
    TDateTime GetFieldDT(String FieldName, TDateTime Default = 0);
    //-----------------------------------------------------------------------
    static void SetParamInt64(TParam* Param, __int64 Value);
    //-----------------------------------------------------------------------
    // Get Firebird Generator value as __int64
    __int64 FBAutoInc(String GeneratorName);
    //-----------------------------------------------------------------------
    void SelectField(String FieldName, TStrings* List, bool Clear = true);
    //-----------------------------------------------------------------------
    void SelectFieldValues(String FieldName, String ValueFieldName, TStrings* List, bool Clear = true);
    //-----------------------------------------------------------------------
    // Get current DataSet
	TDataSet* GetDataSet();
    //-----------------------------------------------------------------------
    TDataSet* DoGetDataSet();
    //-----------------------------------------------------------------------
    // Get database name from string
    String GetDatabaseName(String Str);
    //-----------------------------------------------------------------------
    // Get table name from string
    String GetTableName(String Str);
    //-----------------------------------------------------------------------
	bool IsParadox();
	bool IsFirebird();
	bool IsMSSQL();
    //-----------------------------------------------------------------------
    bool FlushBuffers();
    //-----------------------------------------------------------------------
    // Write message to log file
    void Log(String Msg, TColor AColor = clBlack, TColor BColor = clWhite);
    //-----------------------------------------------------------------------
    // Write exception message to log file
    void LogEx(Exception& E, String Msg, TColor AColor = clRed, TColor BColor = clWhite);
    //-----------------------------------------------------------------------
    bool CheckSQLUpdateModified(TZDbQuery& QSelect);
    //-----------------------------------------------------------------------
    TSDatabaseType      DatabaseType;       // Database type
    String              DatabaseName;
    TZDbDatabase*       Database;
    TZConnection*       ZConnection;        // ZeosDBO ZConnection
    TZQuery*            ZQuery;             // ZeosDBO ZQuery
    TQuery*             PQuery;             // BDE TQuery
    TDataSet*           DataSet;            // DataSet
    TDataSource*        DataSource;
    TPerfCounter        Perf;               // Performance

    #ifdef _ZFIBPLUS_
    TpFIBQuery*         FibQuery;           //
    #endif

    TComponent*         Owner;              // Owner
    bool                DebugMode;          // Debug mode
    bool                Throw;              // True to throw exception

    bool                RequestLive;        // RequestLive
    bool                CachedUpdates;      // Cached updates

    TLogFile*           LogFile;            // Log file
    TLogPanel*          LogPanel;           // Log panel
};
//===========================================================================
//                               Inline Functions
//===========================================================================
inline bool IsZQueryDBConnected(TDataSet* DataSet)
{
    bool Result = false;

    TZQuery* ZQuery = dynamic_cast<TZQuery*>(DataSet);
    if (ZQuery) {
        if (ZQuery->Connection && ZQuery->Connection->Connected) {
            Result = true;
        }
    }
    else Result = true;

    #ifdef _ZFIBPLUS_
    TpFIBQuery* FQuery = dynamic_cast<TpFIBQuery*>(DataSet);
    if (FQuery) {
        if (ZQuery->Connection && ZQuery->Connection->Connected) {
            Result = true;
        }
    }
    #endif

    return Result;
}
//---------------------------------------------------------------------------
inline bool OpenZQuery(TDataSet* DataSet, String AText = "")
{
    TZQuery* ZQuery = dynamic_cast<TZQuery*>(DataSet);
    if (ZQuery) {
        ZQuery->Close();
        if (AText != "")
            ZQuery->SQL->Text = AText;

        // Open query only when database connection is ready (14/02/07)
        if (ZQuery->Connection && ZQuery->Connection->Connected) {
            ZQuery->Open();
        }
    }
    else {
        DataSet->Open();
    }

    return DataSet->Active;
}
//---------------------------------------------------------------------------
inline bool OpenZQuery(TDataSource* DataSource, String AText = "")
{
    return OpenZQuery(DataSource->DataSet, AText);
}
//===========================================================================
bool PACKAGE SaveBlobToFile(TDataSet* DataSet, String FieldName, String FileName);
//---------------------------------------------------------------------------
bool PACKAGE LoadBlobFromFile(TDataSet* DataSet, String FieldName, String FileName);
//---------------------------------------------------------------------------
#pragma warn +8026
#pragma warn +8027

#endif



#ifndef SeTableAutoIncH
#define SeTableAutoIncH

#pragma warn -8026
#pragma warn -8027

//
#include <DB.hpp>
#include <dbTables.hpp>


//===========================================================================
//                              TTableAutoInc
//===========================================================================
class PACKAGE TTableAutoInc {
public:
    __fastcall TTableAutoInc();
    //-----------------------------------------------------------------------
    __fastcall ~TTableAutoInc();
    //-----------------------------------------------------------------------
    bool __fastcall SetAutoInc(TDataSet* DataSet, String FieldName, bool Post = true, bool Refresh = false);
    //-----------------------------------------------------------------------
    bool __fastcall RefreshAutoInc(TDataSet* DataSet, String FieldName);
    //-----------------------------------------------------------------------
    int __fastcall GetAutoInc(TDataSet* DataSet, String FieldName);
    //-----------------------------------------------------------------------
    bool __fastcall VerifyTable(TTable* Table, String FieldName);
    //-----------------------------------------------------------------------
    bool __fastcall GetAutoIncIni(TTable* Table, String& IniFileName, String& Section);
    //-----------------------------------------------------------------------
    static String __fastcall GetTableFileName(TTable* Table);
    //-----------------------------------------------------------------------
    TStringList*    TableFieldList;
    TStringList*    MemIniList;
    bool            FWriteIni;
};

//---------------------------------------------------------------------------
inline String __fastcall GetTableFileName(TTable* Table)
{
    return TTableAutoInc::GetTableFileName(Table);
}
//---------------------------------------------------------------------------

#pragma warn +8026
#pragma warn +8027

#endif



#include <vcl.h>
#include <IniFiles.hpp>
#include "SeTableAutoInc.h"
#include "SeAutoBookmark.h"
#include "SeAutoCursor.h"
//---------------------------------------------------------------------------
/*

    TTableAutoInc - Auto Increment Field Manager

    TTableAutoInc is a replacement for the standard Auto-Increment field type
    of Paradox tables. The problem with this field type appears when repairing
    or copying a table. In these cases the auto-increment fields are re-numbered,
    hence cannot allow us to use the field as reference to records.

*/
//---------------------------------------------------------------------------
__fastcall TTableAutoInc::TTableAutoInc()
{
    TableFieldList = new TStringList;
    MemIniList = new TStringList;
    FWriteIni = false;
}
//---------------------------------------------------------------------------
__fastcall TTableAutoInc::~TTableAutoInc()
{
    delete TableFieldList;
    delete MemIniList;
}
//---------------------------------------------------------------------------
bool __fastcall TTableAutoInc::SetAutoInc(TDataSet* DataSet, String FieldName, bool Post, bool Refresh)
{
    try {
        TTable* Table = dynamic_cast<TTable*>(DataSet);

        // Get current field value AS INTEGER
		int Value = Table->FieldValues[FieldName].IsNull() ? 0 : (int)Table->FieldValues[FieldName];

        // Empty, assign new auto-increment value
        if (Value == 0) {
            int Value = GetAutoInc(Table, FieldName);

            if (Value > 0) {
                Table->Edit();
                Table->FieldValues[FieldName] = Value;

                if (Post)
                    Table->Post();
            }
        }

        return true;
    }
    catch (Exception& E) {
    }

    return false;
}
//---------------------------------------------------------------------------
bool __fastcall TTableAutoInc::RefreshAutoInc(TDataSet* DataSet, String FieldName)
{
    try {
        TTable* Table = dynamic_cast<TTable*>(DataSet);

        if (Table)
            return VerifyTable(Table, FieldName);
    }
    catch (Exception& E) {
    }

    return false;
}
//---------------------------------------------------------------------------
//
int __fastcall TTableAutoInc::GetAutoInc(TDataSet* DataSet, String FieldName)
{
    TTable* Table = dynamic_cast<TTable*>(DataSet);
    if (!Table)
        return 0;

    // 
    String Key = GetTableFileName(Table) + "_" + FieldName;

    // Force here! 03/03/2004
    // We must refresh each time because when using replicator or other
    // application that may change the table, the value we have might not be updated.
    // if (TableFieldList->IndexOf(Key) < 0) {

    {
        if (!VerifyTable(Table, FieldName)) {

            //String Msg = "AutoInc: Must verify!  TableKey=" + Key;
            //::MessageBox(NULL, Msg.c_str(), "AutoInc Error", MB_OK | MB_SYSTEMMODAL);

            return 0;
        }
    }

    int Value = 0;
    String IniFileName, Section;

    //
    if (GetAutoIncIni(Table, IniFileName, Section)) {

        // Update disk INI
        if (FWriteIni) {
            TIniFile* IniFile = new TIniFile(IniFileName);
            int AutoInc = IniFile->ReadInteger(Section, FieldName + "_AutoInc", 1);
            Value = AutoInc++;
            IniFile->WriteInteger(Section, FieldName + "_AutoInc", AutoInc);
            delete IniFile;
        }

        // Update memory INI
        else {
            int AutoInc = MemIniList->Values[Section + FieldName + "_AutoInc"].ToIntDef(1);
            Value = AutoInc++;
            MemIniList->Values[Section + FieldName + "_AutoInc"] = AutoInc;
        }
    }

    return Value;
}
//---------------------------------------------------------------------------
bool __fastcall TTableAutoInc::VerifyTable(TTable* ATable, String FieldName)
{
	TAutoCursor ACursor;

    bool Result = false;

    TTable* Table = new TTable(ATable->Owner);
    Table->TableName = GetTableFileName(ATable);

    try {
        Table->Active = true;

        // Scan table - Find highest value
        int Max = 0;
        Table->First();
        while (!Table->Eof) {
			int Value = Table->FieldValues[FieldName].IsNull() ? 0 : (int)Table->FieldValues[FieldName];

            if (Value > Max)
                Max = Value;
            Table->Next();
        }

        // Assign next value
        int AutoInc = Max+1;

        // Verify and fix table table
        TList* List = new TList;
        int FixCount = 0;

        Table->First();
        while (!Table->Eof) {
			//int Value = Table->FieldValues[FieldName].IsNull() ? 0 : String(Table->FieldValues[FieldName]).ToIntDef(0);
			int Value = Table->FieldValues[FieldName].IsNull() ? 0 : (int)Table->FieldValues[FieldName];

            // Add new value
            if (Value > 0 && List->IndexOf((void*)Value) < 0)
                List->Add((void*)Value);

            // Already found in list
            else {
                int NewValue = AutoInc++;

                Table->Edit();
                Table->FieldValues[FieldName] = NewValue;
                Table->Post();

                if (Value > 0)
                    FixCount++;

                List->Add((void*)NewValue);
            }

            Table->Next();
        }

        delete List;

        Table->FlushBuffers();
        Table->Active = false;

        // Display message
        if (FixCount > 0) {
            AnsiString Msg = "Table Auto-Increment field fixed: Found duplicate field values.\n\nTable: " +
				GetTableFileName(Table) + "\nField: " + FieldName + "\nRecords fixed: " +
				String(FixCount) + "\n\nPlease contact support@controlbit.com for more details";



			::MessageBoxA(NULL, Msg.c_str(), "Table fixed", MB_OK | MB_SYSTEMMODAL);
        }

        // Update INI file
        String IniFileName, Section;

        if (GetAutoIncIni(Table, IniFileName, Section)) {

            // Update disk INI
            if (FWriteIni) {
                TIniFile* IniFile = new TIniFile(IniFileName);
                IniFile->WriteInteger(Section, FieldName + "_AutoInc", AutoInc);
                delete IniFile;
            }

            // Update memory INI
            else {
                MemIniList->Values[Section + FieldName + "_AutoInc"] = AutoInc;
            }

            Result = true;

            // Add table to list
            String Key = GetTableFileName(Table) + "_" + FieldName;
            if (TableFieldList->IndexOf(Key) < 0)
                TableFieldList->Add(Key);
        }
    }
    catch (Exception& E) {
    }

    delete Table;

    return Result;
}
//---------------------------------------------------------------------------
// Return INI file name and section
bool __fastcall TTableAutoInc::GetAutoIncIni(TTable* Table, String& IniFileName, String& Section)
{
    String TableName = GetTableFileName(Table);

    if (TableName != "") {

        // Old: Changed for replicator support.
        //IniFileName = ExtractFilePath(TableName) + "AutoInc.ini";

        // Changed 24/09/2003 - Each table has a separate ".ai" file
        // When using the replicator, a global settings file may cause probems
        // with replicated tables.

        IniFileName = ChangeFileExt(TableName, ".ai");

      	Section = ExtractFileName(TableName);
        return true;
    }

    return false;
}
//---------------------------------------------------------------------------
String __fastcall TTableAutoInc::GetTableFileName(TTable* Table)
{
	String FileName = Table->TableName;

    // Get table file name
    if (Table->DatabaseName != "") {
	    try {
        	TDatabase* Database = Table->Database;
        	if (!Database)
				Database = Table->OpenDatabase();

        	String Alias = Database->AliasName;
            if (Alias == "")
            	Alias = Database->DatabaseName;

           	// Get Alias parameters
            if (Alias != "" && Session->IsAlias(Alias)) {
            	TStringList* Params = new TStringList;
	            Session->GetAliasParams(Alias, Params);
	            String Path = Params->Values["PATH"];
                delete Params;

				if (Path.SubString(Path.Length(), 1) != "\\")
                	Path += "\\";
                FileName = Path + ChangeFileExt( ExtractFileName(Table->TableName), ".db" );
            }
        }
        catch (Exception& E) {
        }
    }
    return FileName;
}
//---------------------------------------------------------------------------

